# CUSTOM FUNCTIONS, organized by general area

# data wrangling -----

# function for removing a question from long or wide dfs
remove_cap_fun <- function(df, capacity, cap_var = "question") {
  df_new <- df
  
  if (capacity %in% names(df)) {
    df_new <- df_new %>% select(-!!sym(capacity))
  } else {
    df_new <- df_new %>%
      filter(!!sym(cap_var) != capacity)
  }
  
  return(df_new)
}

# function for cleaning up errors in data entry of targets
clean_char_fun <- function(var) {
  
  var <- tolower(var)
  
  clean_var <- case_when(grepl("rock", var) ~ "rocks",
                         grepl("flow", var) ~ "flowers",
                         grepl("beet", var) ~ "beetles",
                         grepl("crick", var) |
                           grepl("crik", var) ~ "crickets",
                         grepl("chick", var) |
                           grepl("chik", var) ~ "chickens",
                         grepl("mice", var) |
                           grepl("mouse", var) |
                           grepl("^rat", var) ~ "mice",
                         grepl("dog", var) ~ "dogs",
                         grepl("pig", var) ~ "pigs",
                         grepl("child", var) ~ "children",
                         grepl("phone", var) ~ "cellphones",
                         grepl("robot", var) ~ "robots",
                         grepl("alien", var) ~ "aliens",
                         grepl("ghos", var) ~ "ghosts",
                         grepl("god", var) ~ "god",
                         TRUE ~ var)
  
  return(clean_var)
    
}

# function for getting upper triangle of a matrix
# from http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
get_upper_tri_fun <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

# function for making wide-form dataframes
wide_df_fun <- function(df) {
  df_wide <- df %>%
    select(subj_id, question, response) %>%
    spread(question, response) %>%
    column_to_rownames("subj_id")
  
  return(df_wide)
}

# exploratory factor analysis (EFA) -----

# function for checking whether it is ok to extract k factors given p observed variables
factor_check_ok <- function(p_obs_var, k_factors) {
  
  # calculate number of observed datapoints
  observed <- sum(p_obs_var, # observed variances
                  (p_obs_var * (p_obs_var - 1) / 2)) # observed covariances
  
  # calculate number of estimated parameters
  estimated <- sum(p_obs_var * k_factors, # paths between variables and factors
                   k_factors, # estimated variances for each factor
                   -1 * (k_factors * (k_factors - 1) / 2)) # MINUS the constraint on each pair of factors to be orthogonal
  
  # test whether observed datapoints > estimated parameters
  return(ifelse(observed > estimated, TRUE, FALSE))
  
}

# function for determining the maximum number of factors to extract from a dataset with p variables
factor_max_ok <- function(p_obs_var) {
  df_check <- data.frame()
  for (i in 1:p_obs_var) {
    df_check[i, "check"] <- factor_check_ok(p_obs_var, i)
  }
  max <- df_check %>% filter(check) %>% nrow()
  return(max)
}

# general custom efa function
fa_fun <- function(df, n = NULL,
                   chosen_cor = "cor", chosen_rot = "varimax",
                   chosen_fm = "minres", chosen_scores = "tenBerge"){
  
  if (is.null(n)) {
    n <- factor_max_ok(ncol(df))
  }
  
  efa <- fa(df, nfactors = n, missing = T, impute = "median",
            cor = chosen_cor, rotate = chosen_rot,
            fm = chosen_fm, scores = chosen_scores)
  colnames(efa$r.scores) <- paste0("F", 1:n)
  rownames(efa$r.scores) <- paste0("F", 1:n)
  names(efa$R2) <- paste0("F", 1:n)
  colnames(efa$weights) <- paste0("F", 1:n)
  colnames(efa$loadings) <- paste0("F", 1:n)
  colnames(efa$scores) <- paste0("F", 1:n)
  colnames(efa$Vaccounted) <- paste0("F", 1:n)
  return(efa)
}

# function for implementing parallel analysis factor retention criteria
reten_fun_par <- function(df, chosen_cor = "cor"){
  
  pa <- fa.parallel(df, cor = chosen_cor, plot = F)
  retain_k_final <- as.numeric(pa$nfact)
  
  return(retain_k_final)
}

# function for implementing minimizing BIC factor retention criteria
reten_fun_bic <- function(df, chosen_cor = "cor"){
  
  vss <- VSS(df, cor = chosen_cor)
  
  retain_k_final <- vss$vss.stats %>% 
    rownames_to_column("nfact") %>% 
    top_n(-1, BIC) %>% 
    select(nfact) %>% 
    as.numeric()
  
  return(retain_k_final)
}

# function for implementing Weisman et al. factor retention criteria
reten_fun_wdm <- function(df, 
                          chosen_cor = "cor", 
                          chosen_rot = "varimax"){
  
  # figure out max number of factors to retain
  n_var <- ncol(df)
  max_k <- factor_max_ok(n_var)
  
  # run efa with max factors, unrotated
  fa_unrot <- fa(df, nfactors = max_k, cor = chosen_cor, rotate = "none", 
                 scores = "tenBerge", impute = "median")
  eigen <- fa_unrot$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("param") %>%
    gather(factor, value, -param) %>%
    spread(param, value) %>%
    filter(`SS loadings` > 1, `Proportion Explained` > 0.05)
  retain_k <- nrow(eigen)
  
  fa_rot <- fa(df, nfactors = retain_k, cor = chosen_cor, rotate = chosen_rot,
               scores = "tenBerge", impute = "median")
  
  loadings <- fa_rot$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    group_by(capacity) %>%
    top_n(1, abs(loading)) %>%
    ungroup() %>%
    count(factor)
  retain_k_final <- nrow(loadings)
  
  return(retain_k_final)
}


# function for comparing 3 factor retention protocols
reten_fun_compare <- function(df, cor_type = "cor", rot_type = "varimax"){
  nfact_par <- reten_fun_par(df, chosen_cor = cor_type)
  nfact_bic <- reten_fun_bic(df, chosen_cor = cor_type)
  nfact_wdm <- reten_fun_wdm(df, chosen_cor = cor_type, chosen_rot = rot_type)
  
  res <- data.frame(protocol = c("par", "bic", "wdm"),
                    nfact = c(nfact_par, nfact_bic, nfact_wdm))
  
  return(res)
}

# function for extracting factor loadings
loadings_fun <- function(efa, long_wide = "long"){
  loadings_df <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity")
  
  if (long_wide == "long") {
    loadings_df <- loadings_df %>%
      gather(factor, loading, -capacity)
  }
  
  return(loadings_df)
}

# function for grabbing top n mental capacities for which a factor was dominant
top_n_domCap <- function(efa, n, factor, abs_pos = "abs"){
  
  loadings_df <- loadings_fun(efa)
  
  if (abs_pos == "abs") {
    dom_df <- loadings_df %>%
      group_by(capacity) %>%
      top_n(1, abs(loading)) %>%
      ungroup() %>%
      group_by(factor) %>%
      top_n(n, abs(loading)) %>%
      ungroup() %>%
      arrange(desc(abs(loading)))
  } else if (abs_pos == "pos") {
    dom_df <- loadings_df %>%
      group_by(capacity) %>%
      top_n(1, loading) %>%
      ungroup() %>%
      group_by(factor) %>%
      top_n(n, loading) %>%
      ungroup() %>%
      arrange(desc(loading))
  }
  
  wordings <- dom_df$capacity[dom_df$factor == factor] %>% 
    paste(collapse = "_, _")
  
  wordings <- paste0("_", wordings, "_")
  wordings <- stri_replace_last_regex(wordings, ",", ", and")
  wordings <- gsub("sense...far away", 
                   "sense whether something is close by or far away", wordings)
  wordings <- gsub("understand how someone...feeling", 
                   "understand how someone else is feeling", wordings)
  wordings <- gsub("\\.\\.\\.", "", wordings)
  
  return(wordings)
}

# function for getting CIs on factor loadings
cap_ci_fun <- function(efa){
  ctry <- gsub("_.*$", "", colnames(efa$loadings)[1])
  lower_lab <- paste("ci_lower", ctry, "F", sep = "_")
  upper_lab <- paste("ci_upper", ctry, "F", sep = "_")
  
  res <- loadings_fun(efa, "wide") %>%
    rename_at(vars(-capacity), 
              funs(paste0("mean_", .))) %>%
    bind_cols(efa$cis$ci) %>%
    rename_at(vars(starts_with("lower")),
              funs(gsub("lower\\.", lower_lab, .))) %>%
    rename_at(vars(starts_with("upper")),
              funs(gsub("upper\\.", upper_lab, .)))
  
  return(res)
}

# function for getting most congruent factor match 
top_match_fun <- function(cor_df, which_country = c("country_A", "country_B")) {
  
  which_factor = case_when(which_country == "country_A" ~ "factor_B",
                           which_country == "country_B"~ "factor_A",
                           TRUE ~ NA_character_)
  
  other_factor = case_when(which_factor == "factor_A" ~ "factor_B",
                           which_factor == "factor_B" ~ "factor_A",
                           TRUE ~ NA_character_)
  
  df <- cor_df %>%
    # filter(factor_A != factor_B) %>%
    # filter(country_A != country_B) %>%
    group_by(!!sym(which_country), !!sym(which_factor)) %>%
    top_n(1, cong) %>%
    ungroup() %>%
    rename(top_match = !!other_factor) %>%
    select(!!which_factor, !!which_country, top_match) %>%
    distinct()
  
  return(df)
}

# function for getting most congruent factor in each country
top_cong_fun <- function(df_cong, which_factor, filter_ec = T){
  
  if(filter_ec){
    df_cong <- df_cong %>% filter(country_A != "Ecuador", country_B != "Ecuador")
  }
  
  res <- df_cong %>%
    filter(factor_A == which_factor) %>%
    group_by(country_B) %>%
    top_n(1, cong) %>%
    ungroup() 
  
  return(res)
}


# regression -----

# function for writing regression table (fixed effects)
regtab_fun <- function(reg,
                       std_beta = F,
                       cat_var = "super_cat_relig",
                       cat_name = "Category (religious)",
                       country_var1 = "country_gh",
                       country_name1 = "Country (Gh.)",
                       country_var2 = "country_th",
                       country_name2 = "Country (Th.)",
                       country_var3 = "country_ch",
                       country_name3 = "Country (Ch.)",
                       country_var4 = "country_vt",
                       country_name4 = "Country (Vt.)",
                       predictor_var1 = "predictor_a",
                       predictor_name1 = "Predictor (A)",
                       predictor_var2 = "predictor_b",
                       predictor_name2 = "Predictor (B)",
                       predictor_var3 = "predictor_c",
                       predictor_name3 = "Predictor (C)",
                       predictor_var4 = "predictor_d",
                       predictor_name4 = "Predictor (D)"){
  
  var_key <- c(cat_name, country_name1, country_name2, country_name3, country_name4, 
               predictor_name1, predictor_name2, predictor_name3, predictor_name4)
  names(var_key) <- c(cat_var, country_var1, country_var2, country_var3, country_var4,
                      predictor_var1, predictor_var2, predictor_var3, predictor_var4)
  
  reg_class <- class(reg)
  
  if ("lmerModLmerTest" %in% reg_class || reg_class == "lm") {
    regtab <- summary(reg)$coefficients %>%
      data.frame() %>%
      rownames_to_column("Parameter") %>%
      rename(β = Estimate,
              `Std. Err.` = Std..Error,
              t = t.value,
              p = Pr...t..) %>%
      mutate(signif = case_when(p < 0.001 ~ "***",
                                p < 0.01 ~ "**",
                                p < 0.05 ~ "*",
                                TRUE ~ ""),
             p = case_when(p < 0.001 ~ "<0.001",
                           TRUE ~ format(round(p, 3), nsmall = 3))) %>%
      mutate_at(vars(-c(Parameter, p, signif)), 
                funs(format(round(., 2), nsmall = 2))) %>%
      rename(" " = signif)
  }
  
  if (reg_class == "brmsfit") {
    regtab <- fixef(reg) %>%
      data.frame() %>%
      rownames_to_column("Parameter") %>%
      rename(β = Estimate,
              `Std. Err.` = Est.Error) %>%
      mutate(nonzero = case_when((Q2.5 * Q97.5) > 0 ~ "*",
                                 TRUE ~ "")) %>%
      mutate_at(vars(-Parameter, -nonzero), 
                funs(format(round(., 2), nsmall = 2))) %>%
      mutate(`95% CI` = paste0("[", Q2.5, ", ", Q97.5, "]")) %>%
      select(Parameter, β, `Std. Err.`, `95% CI`, nonzero) %>%
      rename(" " = nonzero)
  }
  
  if (std_beta) {
    beta_std <- std_beta(reg, type = "std")
    beta_std2 <- std_beta(reg, type = "std2") %>%
      # correct inconsistencies in naming between std and std2
      mutate(term = gsub("site_rural", "site", term),
             term = gsub("religion_char", "religion", term),
             term = gsub("spirit_scale1", "spirit_scale", term),
             term = gsub("site", "site_rural", term),
             term = gsub("religion", "religion_char", term),
             term = gsub("spirit_scale", "spirit_scale1", term))
    
    beta_df <- beta_std %>% select(term, std.estimate) %>%
      rename("β'" = std.estimate) %>%
      left_join(beta_std2 %>% select(term, std.estimate) %>%
                  rename("β''" = std.estimate)) %>%
      rename(Parameter = term) %>%
      mutate_at(vars(starts_with("β")), 
                funs(format(round(., 2), nsmall = 2)))
    
    regtab <- regtab %>%
      left_join(beta_df) %>%
      select(Parameter, starts_with("β"), everything())
  }
  
  regtab <- regtab %>%
    mutate(Parameter = gsub("\\:", " × ", Parameter),
           Parameter = gsub("\\(Intercept\\)", "Intercept", Parameter),
           Parameter = str_replace_all(string = Parameter, var_key))
  
  return(regtab)
}

# function for writing regression table (random effects, residual variance)
regtab_ran_fun <- function(reg,
                           cat_var = "super_cat_relig",
                           cat_name = "Category (religious)",
                           country_var = "country",
                           country_name = "Country",
                           subj_var = "subject_id",
                           subj_name = "Individual"){
  
  var_key <- c(cat_name, country_name, subj_name)
  names(var_key) <- c(cat_var, country_var, subj_var)
  
  reg_class <- class(reg)
  
  if ("lmerModLmerTest" %in% reg_class) {
    regtab <- summary(reg)$varcor %>%
      data.frame() %>%
      filter(is.na(var2)) %>%
      select(grp, var1, vcov, sdcor) %>%
      mutate(grp = gsub("\\..*$", "", grp))
    
    levels_grp <- c(regtab[(nrow(regtab) - 1):1,"grp"], 
                    regtab[nrow(regtab),"grp"]) %>% unique()
    
    levels_var1 <- c("(Intercept)", cat_var, country_var)
    
    regtab <- regtab %>%
      mutate(grp = factor(grp, levels = levels_grp),
             var1 = factor(var1, levels = levels_var1)) %>%
      arrange(grp, var1) %>%
      mutate_at(vars(grp, var1), funs(as.character)) %>%
      mutate_at(vars(grp, var1), funs(gsub("\\(", "", .))) %>%
      mutate_at(vars(grp, var1), funs(gsub("\\)", "", .))) %>%
      rename(Group = grp, Type = var1, Variance = vcov, `Std. Dev.` = sdcor) %>%
      mutate(Group = gsub("\\:", ", nested within ", Group))
    
  }
  
  if (reg_class == "brmsfit") {
    regsum <- summary(reg)
    
    rantab <- data.frame()
    for (i in 1:length(regsum$group)) {
      temptab <- regsum$random[[regsum$group[i]]] %>%
        data.frame() %>%
        rownames_to_column("Type") %>%
        mutate(grp = regsum$group[[i]])
      rantab <- bind_rows(rantab, temptab)
    }
    
    rantab <- rantab %>%
      filter(!grepl("cor\\(", Type))
    
    resid <- regsum$spec_pars %>%
      data.frame() %>%
      bind_cols("grp" = "Residual", Type = "sd(Intercept)")
    
    regtab <- bind_rows(rantab, resid) %>%
      rename(Group = grp, `Std. Dev.` = Estimate) %>%
      mutate(Variance = `Std. Dev.`^2,
             Type = gsub("sd\\(", "", Type),
             Type = gsub("\\)", "", Type)) %>%
      select(Group, Type, Variance, `Std. Dev.`) %>%
      separate(Group, c("grp1", "grp2", "grp3", "grp4", "grp5"), sep = ":") %>%
      unite(Group, c(grp5, grp4, grp3, grp2, grp1), sep = ", nested within ") %>%
      mutate(Group = gsub("NA, nested within ", "", Group))
    
  }
  
  regtab <- regtab %>%
    mutate_at(vars(Variance, `Std. Dev.`), 
              funs(format(round(., 2), nsmall = 2))) %>%
    mutate_at(vars(Group, Type),
              funs(str_replace_all(string = ., var_key))) %>%
    mutate(Type = case_when(is.na(Type) ~ "", 
                            Type == "Intercept" ~ Type,
                            TRUE ~ paste0("Slope (", Type, ")")))
  
  return(regtab)
}

# function for getting three kinds of regression coefficient estimates
beta_fun <- function(reg, find_name = " ", replace_name = " "){
  require(sjstats)
  
  if ("lmerModLmerTest" %in% class(reg)) {
    res_tab1 <- fixef(reg)
  } else {
    res_tab1 <- coef(reg)
  }
  
  res_tab <- res_tab1 %>%
    data.frame() %>%
    rename(β = ".") %>%
    rownames_to_column("term") %>%
    full_join(std_beta(reg, type = "std") %>%
                select(term, std.estimate) %>%
                rename("β'" = std.estimate)) %>%
    full_join(std_beta(reg, type = "std2") %>% 
                select(term, std.estimate) %>%
                rename("β''" = std.estimate) %>%
                mutate(term = gsub(find_name, replace_name, term))) 
  
  return(res_tab)
}

beta_style_fun <- function(tab){
  res_tab <- tab %>%
    mutate_at(vars(-term), funs(format(round(., 2), nsmall = 2))) %>%
    kable(digits = 2, align = c("l", rep("r", 3))) %>%
    kable_styling()
  
  return(res_tab)
}

# function for styling regtab for easy import to word document
regtab_style_fun <- function(regtab,
                             row_emph = NULL,
                             font_sz = 16,
                             text_col = "black"){
  
  if (" " %in% names(regtab)) {
    align_vec = c(rep("r", ncol(regtab) - 1), "l")
  } else {
    align_vec = "r"
  }
  
  regtab_styled <- regtab %>%
    mutate_at(vars(starts_with("β")), funs(replace_na(., replace = "-"))) %>%
    kable(align = align_vec) %>%
    kable_styling(font_size = font_sz) %>%
    row_spec(1:nrow(regtab), color = text_col)
  
  if (length(row_emph) > 0) {
    regtab_styled <- regtab_styled %>%
      row_spec(row_emph, bold = T)
  }
  
  return(regtab_styled)
}


# reliability -----

# function for calculating Cronbach's alpha
alpha_fun <- function(df, which_vars, which_country, which_keys = NULL,
                      which_use = NULL){
  
  if (which_country != "ALL") {
    df0 <- df %>% filter(country == which_country)
  } else {
    df0 <- df
  }
  
  df0 <- df0 %>% select(!!which_vars)
  
  res <- psych::alpha(df0, keys = which_keys, use = "pairwise")
  res_alpha <- res$total["raw_alpha"] %>% as.numeric()
  
  return(res_alpha)  
}

# function for getting ICC stat
icc_fun <- function(df, var_name = NA, 
                    var1 = "response", var2 = "recoded",
                    which_model = "oneway", which_type = "consistency",
                    which_unit = "single") {
  
  df0 <- df %>%
    filter(question == var_name) %>%
    select_at(c(var1, var2))
  
  res <- irr::icc(df0, model = which_model, type = which_type, unit = which_unit)
  
  icc <- res$value
  
  return(icc)
  
}


# scoring -----

# function for scoring scales after omitting items
score_fun <- function(df, var_omit = NA, 
                      var_group = c("country", "subject_id")){
  
  if (!is.na(var_omit)) {
    df0 <- df %>% select(-!!var_omit)
  } else {
    df0 <- df
  }
  
  df0 <- df0 %>%
    gather(question, response, -!!var_group) %>%
    group_by_at(var_group) %>%
    summarise(score = mean(response, na.rm = T)) %>%
    ungroup()
  
  return(df0)
  
}


# plotting -----

# function for emulating ggplot default colors
# source: https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# function for making histograms by percentage (by country)
demo_plot_fun <- function(df, ss_df, var){
  plot <- df %>%
    left_join(ss_df) %>%
    count(country_n, !!sym(var)) %>%
    group_by(country_n) %>%
    mutate(prop = n/sum(n),
           answered = ifelse(is.na(!!sym(var)), T, F)) %>%
    ungroup() %>%
    ggplot(aes(x = !!sym(var), y = prop, fill = answered)) +
    facet_grid(~ country_n) +
    geom_bar(stat = "identity", alpha = 0.7, color = "black", size = 0.1, 
             show.legend = F) +
    scale_fill_manual(values = c(gg_color_hue(1), "gray"))
  
  return(plot)
}

# function for generating heatmap of factor loadings
heatmap_fun <- function(efa, factor_names = NA){
  
  # get factor names
  if (is.na(factor_names)) {
    factor_names <- paste("Factor", 1:efa$factors)
  }
  
  # put factors in a standard order when applicable
  body_factors <- factor_names[grepl("BODY", factor_names)]
  
  leftovers <- factor_names[!factor_names %in% body_factors]
  heart_factors <- leftovers[grepl("HEART", leftovers)]
  
  leftovers <- leftovers[!leftovers %in% heart_factors]
  mind_factors <- leftovers[grepl("MIND", leftovers)]
  
  other_factors <- leftovers[!leftovers %in% mind_factors]
  
  factor_levels <- c(body_factors, heart_factors, mind_factors, other_factors)
  
  # get factor loadings
  loadings <- efa$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)),
           factor = factor(factor, levels = factor_levels))
  
  # get fa.sort() order
  order <- loadings %>%
    group_by(capacity) %>%
    top_n(1, abs(loading)) %>%
    ungroup() %>%
    arrange(desc(factor), abs(loading)) %>%
    mutate(order = 1:length(levels(factor(loadings$capacity)))) %>%
    select(capacity, order)
  
  # get percent shared variance explained
  shared_var <- efa$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("stat") %>%
    filter(stat == "Proportion Explained") %>%
    select(-stat) %>%
    gather(factor, var) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)),
           factor = factor(factor, levels = factor_levels)) %>%
    mutate(var_shared = paste0(factor, "\n", round(var, 2)*100, "% shared var.,"))
  
  # get percent total variance explained
  total_var <- efa$Vaccounted %>%
    data.frame() %>%
    rownames_to_column("stat") %>%
    filter(stat == "Proportion Var") %>%
    select(-stat) %>%
    gather(factor, var) %>%
    mutate(factor = as.character(factor(factor, labels = factor_names)),
           factor = factor(factor, levels = factor_levels)) %>%
    mutate(var_total = paste0(round(var, 2)*100, "% total var."))
  
  # make plot
  plot <- ggplot(loadings %>% 
                   left_join(order) %>%
                   left_join(shared_var %>% select(-var)) %>%
                   left_join(total_var %>% select(-var)) %>%
                   mutate(capacity = gsub("_", " ", capacity),
                          factor = factor(factor, levels = factor_levels),
                          xlab = paste(var_shared, var_total, sep = "\n")),
                 aes(x = reorder(xlab, as.numeric(factor)), 
                     y = reorder(capacity, order), 
                     fill = loading, 
                     label = format(round(loading, 2), nsmall = 2))) +
    geom_tile(color = "black") +
    geom_text(size = 3) +
    scale_fill_distiller(limits = c(-1, 1), 
                         palette = "RdYlBu",
                         guide = guide_colorbar(barheight = 10)) +
    theme_minimal() +
    scale_x_discrete(position = "top") +
    theme(axis.title = element_blank())
  
  return(plot)
  
}

# function for labeling heatmap with info on solution
heatmap_lab_fun <- function(df_nfact, 
                            which_protocol = c("par", "bic", "wdm", 
                                               "min", "mid", "max")){
  
  if (which_protocol %in% c("par", "bic", "wdm")) {
    nfact <- df_nfact %>%
      filter(protocol == which_protocol) %>%
      select(nfact) %>%
      c() %>%
      as.numeric()
    
    proto <- which_protocol
  } else if (which_protocol == "min") {
    df_new <- df_nfact %>%
      filter(nfact == min(nfact))
    
    nfact <- df_new$nfact
    proto <- df_new$protocol %>% as.character()
    
  } else if (which_protocol == "max") {
    df_new <- df_nfact %>%
      filter(nfact == max(nfact))
    
    nfact <- df_new$nfact
    proto <- df_new$protocol %>% as.character()
    
  } else if (which_protocol == "mid") {
    df_new <- df_nfact %>%
      filter(nfact != min(nfact), nfact != max(nfact))
    
    nfact <- df_new$nfact
    proto <- df_new$protocol %>% as.character()
    
  } else {
    nfact <- "ERROR"
    proto <- "ERROR"
  }
  
  proto_text <- recode(proto,
                       "par" = "parallel analysis",
                       "bic" = "minimizing BIC",
                       "wdm" = "Weisman et al. (2017) criteria")
  
  lab_text <- paste0(nfact, "-factor solution suggested by ", proto_text)
  return(lab_text)
  
}

# function for comparing heatmaps
heatmap_comp_fun <- function(efa_list, shorten = T, padding = F, 
                             cap_order = NA, 
                             facet_order_vars = c("country", "age_group", "fnum"),
                             facet_lab_split = F) {
  
  loadings_all <- data.frame(NULL)
  
  for (i in 1:length(efa_list)) {
    
    f1 <- colnames(efa_list[[i]]$loadings)[1] %>% tolower()
    
    age_gp <- case_when(grepl("adults", f1) ~ "adults",
                        grepl("children", f1) ~ "children")
    
    ctry <- case_when(grepl("^us", f1) ~ "US",
                      grepl("^gh", f1) ~ "Ghana",
                      grepl("^th", f1) ~ "Thailand",
                      grepl("^ch", f1) ~ "China",
                      grepl("^vt", f1) ~ "Vanuatu")
    
    loadings <- loadings_fun(efa_list[[i]]) %>%
      mutate(country = ctry,
             age_group = age_gp)
    
    loadings_all <- bind_rows(loadings_all, loadings)
    
  }
  
  if (padding) {
    
    max_lab_length <- factor_names_adults %>%
      full_join(factor_names_children) %>%
      select(factor_labdescript) %>%
      unlist() %>%
      nchar() %>%
      max()
    
    loadings_all <- loadings_all %>%
      mutate_at(vars(contains("factor_labdescript")),
                funs((str_pad(., width = max_lab_length + 5, side = "left"))))
  }
  
  if (is.na(cap_order)) {
    cap_order <- fa.sort(efa_list[[1]])$loadings[] %>% rownames() %>% rev()
  }
  
  if (shorten) {
    
    loadings_all <- loadings_all %>%
      mutate(capacity = gsub("\\, .*$", " \\[...\\]", capacity))
    
    cap_order <- gsub("\\, .*$", " \\[...\\]", cap_order)
    
  }
  
  loadings_all <- loadings_all %>%
    left_join(full_join(factor_names_adults,
                        factor_names_children)) %>%
    mutate(capacity = factor(capacity, levels = cap_order)) %>%
    mutate(country = factor(country, levels = levels_country),
           age_group = factor(age_group, levels = c("adults", "children")),
           fnum = as.numeric(gsub(".*_F", "", factor))) %>%
    arrange_at(facet_order_vars) %>%
    mutate(order = 1:nrow(.)) %>%
    mutate(sample = paste(country, age_group, 
                          sep = ifelse(facet_lab_split, "\n", " ")))
  
  plot <- loadings_all %>%
    ggplot(aes(x = factor_labdescript, y = capacity, fill = loading)) +
    facet_grid(~ reorder(sample, order), scales = "free", space = "free") +
    geom_tile(color = "black", size = 0.2) +
    geom_text(aes(label = format(round(loading, 2), nsmall = 2)), size = 3) +
    scale_fill_distiller(palette = "RdYlBu", limits = c(-1, 1),
                         guide = guide_colorbar(barheight = 15, barwidth = 0.5)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          panel.spacing.x = unit(0.8, "lines"),
          strip.text.x = element_text(size = 10, face = "bold")) +
    labs(x = NULL, y = NULL, fill = "Factor\nloading")
  
  return(plot)
  
}

# function for making congruence plots
cong_plot_fun <- function(cong_df, which_country, bg_colors = NA) {
  
  if (is.na(bg_colors)) {
    bg_colors <- c("gray20", viridisLite::viridis(2, begin = 0.75/2, end = 0.75))
  }
  
  plot <- cong_df %>%
    filter(country_A == which_country) %>%
    mutate(lab_A = paste(country_A, " ", age_group_A, "\n",
                         factor_labdescript_A, sep = ""),
           lab_B = paste(country_B, age_group_B, sep = "\n")) %>%
    # mutate_at(#vars(contains("labdescript")),
    #   vars(factor_labdescript_B),
    #   funs(gsub(" \\(", "\n\\(", .))) %>%
    # mutate_at(#vars(contains("labdescript")),
    #   vars(factor_labdescript_B),
    #   funs(gsub("\\/", "\\/\n", .))) %>%
    ggplot(aes(x = factor_labdescript_B, y = mean)) +
    facet_grid(lab_A ~ reorder(lab_B, as.numeric(country_B)), 
               space = "free_x", scales = "free_x") +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0.85,
             fill = bg_colors[1], alpha = 0.2) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.85, ymax = 0.95,
             fill = bg_colors[2], alpha = 0.2) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.95, ymax = Inf,
             fill = bg_colors[3], alpha = 0.2) +
    geom_hline(yintercept = 0.85, lty = 2, color = "gray20") +
    geom_hline(yintercept = 0.95, lty = 2, color = "gray20") +
    geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),
                    fatten = 3,
                    show.legend = F) +
    geom_text(aes(label = format(round(mean, 2), nsmall = 2),
                  y = ifelse(ci_lower < 0.3, ci_upper + 0.02, ci_lower - 0.02),
                  vjust = ifelse(ci_lower < 0.2, 0, 1))) +
    scale_y_continuous(breaks = seq(-1, 1, 0.2)) +
    scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
    scale_shape_manual(values = 21:25) +
    labs(x = "Factor", 
         y = expression("Similarity "(italic(r[c]))),
         color = "Country", fill = "Country", shape = "Country") + 
    guides(color = "none", fill = "none") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          legend.position = "right",
          panel.border = element_rect(fill = scales::alpha("white", 0), 
                                      color = "black"),
          strip.text = element_text(size = 10, face = "bold"))
  
  return(plot)
}

# function for comparing congruence plots between children vs. adults
dev_cong_plot_fun <- function(df, which_country, padding = F, 
                              min_cong = min(df$ci_lower),
                              bg_colors = NA) {
  
  max_lab_length <- df$factor_labdescript_A %>% nchar() %>% max()
  
  if (is.na(bg_colors)) {
    bg_colors <- c("gray20", viridisLite::viridis(2, begin = 0.75/2, end = 0.75))
  }
  
  df <- df %>%
    mutate(lab_A = paste(paste(country_A, age_group_A), 
                         factor_labdescript_A, sep = ":\n"),
           lab_B = paste(paste(country_B, age_group_B), 
                         factor_labdescript_B, sep = ":\n"),
           factor_order_A = case_when(grepl("F1", factor_A) ~ 1,
                                      grepl("F2", factor_A) ~ 2,
                                      grepl("F3", factor_A) ~ 3,
                                      grepl("F4", factor_A) ~ 4,
                                      TRUE ~ NA_real_),
           factor_order_B = case_when(grepl("F1", factor_B) ~ 1,
                                      grepl("F2", factor_B) ~ 2,
                                      grepl("F3", factor_B) ~ 3,
                                      grepl("F4", factor_B) ~ 4,
                                      TRUE ~ NA_real_))
  
  if (padding) {
    df <- df %>%
      mutate_at(vars(contains("factor_labdescript")),
                funs((str_pad(., width = max_lab_length + 5, side = "left"))))
  }
  
  plot <- df %>%
    filter(country_A == which_country, country_B == which_country) %>%
    ggplot(aes(x = reorder(factor_labdescript_A, factor_order_A), y = mean)) +
    facet_grid(. ~ reorder(lab_B, factor_order_B), 
               scales = "free_x", space = "free_x") +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0.85,
             fill = bg_colors[1], alpha = 0.2) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.85, ymax = 0.95,
             fill = bg_colors[2], alpha = 0.2) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.95, ymax = Inf,
             fill = bg_colors[3], alpha = 0.2) +
    geom_hline(yintercept = 0.85, lty = 2, color = "gray20") +
    geom_hline(yintercept = 0.95, lty = 2, color = "gray20") +
    geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),
                    fatten = 3,
                    show.legend = F) +
    geom_text(aes(label = format(round(mean, 2), nsmall = 2),
                  y = ifelse(ci_lower < 0.2, ci_upper + 0.05, ci_lower - 0.05),
                  vjust = ifelse(ci_lower < 0.2, 0, 1))) +
    scale_y_continuous(limit = c(min_cong, 1),
                       breaks = seq(-1, 1, 0.2),
                       expand = expansion(add = 0.05)) +
    scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
    scale_shape_manual(values = 21:25) +
    labs(x = paste(which_country, "children", sep = " "), 
         y = expression("Similarity "(italic(r[c])))) + 
    guides(color = "none", fill = "none") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          legend.position = "right",
          panel.border = element_rect(fill = scales::alpha("white", 0), color = "black"),
          strip.text = element_text(size = 10, face = "bold"))
  
  return(plot)
}


# functions for modifying axis text 

face_fun <- function(df_var) {
  
  face <- case_when(grepl("adults", df_var) ~ "bold",
                    TRUE ~ "plain")
  return(face)
}

color_fun <- function(df_var, highlight_us = T, 
                      color_list = c("red3", "blue3", "darkorchid3", "black")) {
  
  df_var <- gsub(",", "", df_var)
  
  if (is.na(color_list)) {
    color_list <- RColorBrewer::brewer.pal(5, "Dark2")
  }
  
  if (highlight_us) {
    color <- case_when(grepl("US adults", df_var) & 
                         grepl("Body", df_var) ~ color_list[1],
                       grepl("US adults", df_var) & 
                         grepl("Mind", df_var) ~ color_list[2],
                       grepl("US adults", df_var) & 
                         grepl("Heart", df_var) ~ color_list[3],
                       TRUE ~ color_list[4])
  } else {
    color <- case_when(grepl("US", df_var) ~ color_list[1],
                       grepl("Ghana", df_var) ~ color_list[2],
                       grepl("Thailand", df_var) ~ color_list[3],
                       grepl("China", df_var) ~ color_list[4],
                       grepl("Vanuatu", df_var) ~ color_list[5],
                       TRUE ~ "black")
  }
  
  return(color)
}

size_fun <- function(df_var, highlight = "US adults", sizes = c(16, 8)) {
  size <- ifelse(grepl(highlight, gsub(",", "", df_var)), sizes[1], sizes[2])
  return(size)
}
