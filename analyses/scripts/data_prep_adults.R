# DATA PREPARATION

# counterbalancing (sequence) info -----
cb <- read_csv("../other docs/beetles cb.csv")


# US -----
files_us <- list.files("../raw data/adults/USA_BG_Adults CSV Files/", 
                       pattern = "*.csv", full.names = T)

d_raw_us <- plyr::ldply(files_us, read_csv) %>%
  rename(target = character) %>%
  # correct one piece of missing data
  mutate(target = case_when(grepl("Robert A", `subject id`) ~ "mice",
                               TRUE ~ target)) %>%
  filter(!is.na(target), !is.na(sequence))

subj_key_us <- d_raw_us %>%
  rename(subj_id_original = `subject id`) %>%
  distinct(subj_id_original, target) %>%
  mutate(target = clean_char_fun(target)) %>%
  mutate(subj_id = paste("us", "adults", 
                         str_pad(1:nrow(.), width = 3, side = "left", pad = "0"),
                         target,
                         sep = "_"))   
  
d_us <- d_raw_us %>%
  rename(subj_id_original = `subject id`,
         dob = `date of birth`,
         experimenter = `experimenter initials`,
         dot = date,
         trial = `#`,
         response = `circle one`) %>%
  # clean up target
  mutate(target = clean_char_fun(target)) %>%
  # switch to anonymized subject ids
  left_join(subj_key_us) %>%
  select(-subj_id_original) %>%
  # calculate age
  mutate_at(vars(dob, dot), 
            funs(parse_date_time(as.character(gsub("xx", "15", .)), 
                                  orders = c("mdY", "mdy")))) %>% # ,
                                  # cutoff_2000 = 18L))) %>%
  mutate(age = year(as.period(interval(start = dob, end = dot))),
         # hacky way to correct for century mis-handling
         age = ifelse(age <= 0, age + 100, age)) %>%
  # recode gender
  mutate(gender = recode_factor(tolower(gender),
                                "m" = "male",
                                "false" = "female",
                                "o" = "other")) %>%
  # make numeric
  mutate_at(vars(sequence, trial), funs(as.numeric)) %>%
  # code types of target 
  mutate(target_cat = case_when(
           target %in% levels_target_univ ~ "universal",
           !target %in% levels_target ~ "site specific"),
         target_orig = target,
         target = factor(target, levels = levels_target)) %>%
  # clean up response
  rename(response_orig = response) %>%
  mutate(response_cat = case_when(response_orig %in% c("no", "kind of", "yes") ~ 
                                    response_orig,
                                  grepl("kind of", response_orig) ~ "kind of",
                                  TRUE ~ NA_character_),
         response_cat = factor(response_cat, levels = c("no", "kind of", "yes")),
         response = recode(response_cat, "no" = 0, "kind of" = 0.5, "yes" = 1)) %>%
  # add cb info
  left_join(cb) %>%
  # deal with types of questions
  mutate(question_cat = case_when(is.na(question) ~ "site specific",
                                  trial > 23 ~ "universal add-on",
                                  !is.na(question) ~ "universal",
                                  TRUE ~ NA_character_)) %>%
  distinct()

# check for duplicates
d_us_dup <- d_us %>%
  filter(trial == "1") %>%
  count(subj_id) %>%
  filter(n > 1)

if (nrow(d_us_dup) > 0) { print("DUPLICATES IN US ADULTS") }

d_us <- d_us %>%
  # drop identifying info
  select(-dob, -dot) %>%
  # add country
  mutate(country = "US") %>%
  # reorder variables
  select(country, subj_id, gender, age,
         experimenter, location,
         target_cat, target_orig, target, sequence, 
         trial, question_cat, question, clarification,
         response_orig, response_cat, response, comments) %>%
  distinct()


# Ghana -----
files_gh <- list.files("../raw data/adults/GHA_BG_Adults CSV files/", 
                       pattern = "*.csv", full.names = T)

d_raw_gh <- plyr::ldply(files_gh, read_csv) %>%
  rename(target = character) %>%
  filter(!is.na(target), !is.na(sequence))

subj_key_gh <- d_raw_gh %>%
  rename(subj_id_original = `subject ID (name)`) %>%
  distinct(subj_id_original, target) %>%
  mutate(target = clean_char_fun(target)) %>%
  mutate(subj_id = paste("gh", "adults", 
                         str_pad(1:nrow(.), width = 3, side = "left", pad = "0"),
                         target,
                         sep = "_"))

d_gh <- d_raw_gh %>%
  rename(subj_id_original = `subject ID (name)`,
         dot = date) %>%
  # clean up target
  mutate(target = clean_char_fun(target)) %>%
  # switch to anonymized subject ids
  left_join(subj_key_gh) %>%
  select(-subj_id_original) %>%
  # get dot
  mutate_at(vars(dob, dot), 
            funs(parse_date_time2(as.character(.), 
                                  orders = "dby",
                                  cutoff_2000 = 18L))) %>%
  # recode gender
  mutate(gender = recode_factor(tolower(gender),
                                "m" = "male",
                                "false" = "female",
                                "o" = "other")) %>%
  # make numeric
  mutate_at(vars(sequence, trial), funs(as.numeric)) %>%
  # code types of target 
  mutate(target_cat = case_when(
           target %in% levels_target_univ ~ "universal",
           !target %in% levels_target ~ "site specific"),
         target_orig = target,
         target = factor(target, levels = levels_target)) %>%
  # clean up and translate response
  rename(response_orig = response) %>%
  mutate(response_cat = case_when(response_orig %in% c("no", "kind of", "yes") ~ 
                                    response_orig,
                                  grepl("kind of", response_orig) ~ "kind of",
                                  TRUE ~ NA_character_),
         response_cat = factor(response_cat, levels = c("no", "kind of", "yes")),
         response = recode(response_cat, "no" = 0, "kind of" = 0.5, "yes" = 1)) %>%
  # add cb info
  left_join(cb) %>%
  # deal with types of questions
  mutate(question_cat = case_when(is.na(question) ~ "site specific",
                                  trial > 23 ~ "universal add-on",
                                  !is.na(question) ~ "universal",
                                  TRUE ~ NA_character_)) %>%
  distinct()

# check for duplicates
d_gh_dup <- d_gh %>%
  filter(trial == "1") %>%
  count(subj_id) %>%
  filter(n > 1)

if (nrow(d_gh_dup) > 0) { print("DUPLICATES IN GHANA ADULTS") }

d_gh <- d_gh %>%
  # drop identifying info
  select(-dob, -dot) %>%
  # add country
  mutate(country = "Ghana") %>%
  # reorder variables
  select(country, subj_id, gender, age,
         experimenter, location,
         target_cat, target_orig, target, sequence, 
         trial, question_cat, question, clarification,
         response_orig, response_cat, response, comments) %>%
  distinct()


# Thailand -----
files_th <- list.files("../raw data/adults/THL_BG_Adults CSV files/", 
                       pattern = "*.csv", full.names = T)

d_raw_th <- plyr::ldply(files_th, read_csv) %>%
  rename(target = character) %>%
  # correct one piece of missing data
  mutate(target = case_when(grepl("Jetniphat", `subject id`) ~ "mice",
                               TRUE ~ target)) %>%
  filter(!is.na(target), !is.na(sequence))

subj_key_th <- d_raw_th %>%
  rename(subj_id_original = `subject id`) %>%
  distinct(subj_id_original, target) %>%
  mutate(target = clean_char_fun(target)) %>%
  mutate(subj_id = paste("th", "adults", 
                         str_pad(1:nrow(.), width = 3, side = "left", pad = "0"),
                         target,
                         sep = "_"))

d_th <- d_raw_th %>%
  rename(subj_id_original = `subject id`,
         dob = `date of birth`,
         experimenter = `experimenter initials`,
         dot = date,
         trial = `#`,
         response = `circle one`) %>%
  # clean up target
  mutate(target = clean_char_fun(target)) %>%
  # switch to anonymized subject ids
  left_join(subj_key_th) %>%
  select(-subj_id_original) %>%
  # calculate age
  mutate(dob = case_when(
    tolower(dob) %in% c("missing data", "not trans") ~ NA_character_,
    TRUE ~ gsub("\\/19", "\\/", dob))) %>%
  mutate(dot = case_when(
    grepl("Mar", dot) ~ paste0("3/", gsub("\\-Mar", "", dot), "/18"),
    grepl("Apr", dot) ~ paste0("4/", gsub("\\-Apr", "", dot), "/18"),
    tolower(dot) %in% c("missing data", "not trans") ~ NA_character_,
    TRUE ~ dot)) %>%
  mutate_at(vars(dob, dot), 
            ~parse_date_time2(as.character(.), 
                              orders = "mdy",
                              cutoff_2000 = 18L)) %>%
  mutate(age = year(as.period(interval(start = dob, end = dot)))) %>%
  # recode gender
  mutate(gender = recode_factor(tolower(gender),
                                "m" = "male",
                                "false" = "female",
                                "o" = "other")) %>%
  # make numeric
  mutate_at(vars(sequence, trial), funs(as.numeric)) %>%
  # code types of target 
  mutate(target_cat = case_when(
           target %in% levels_target_univ ~ "universal",
           !target %in% levels_target ~ "site specific"),
         target_orig = target,
         target = factor(target, levels = levels_target)) %>%
  # clean up and translate response
  mutate(response_orig = tolower(response),
         response_cat = case_when(response_orig %in% c("no", "kind of", "yes") ~ 
                                    response_orig,
                                  grepl("kind of", response_orig) ~ "kind of",
                                  TRUE ~ NA_character_),
         response_cat = factor(response_cat, levels = c("no", "kind of", "yes")),
         response = recode(response_cat, "no" = 0, "kind of" = 0.5, "yes" = 1)) %>%
  # add cb info
  left_join(cb) %>%
  # deal with types of questions
  mutate(question_cat = case_when(is.na(question) ~ "site specific",
                                  trial > 23 ~ "universal add-on",
                                  !is.na(question) ~ "universal",
                                  TRUE ~ NA_character_)) %>%
  distinct()

# check for duplicates
d_th_dup <- d_th %>%
  filter(trial == "1") %>%
  count(subj_id) %>%
  filter(n > 1)

if (nrow(d_th_dup) > 0) { print("DUPLICATES IN THAILAND ADULTS") }

d_th <- d_th %>%
  # drop identifying info
  select(-dob, -dot) %>%
  # add country
  mutate(country = "Thailand") %>%
  # reorder variables
  select(country, subj_id, gender, age,
         experimenter, location,
         target_cat, target_orig, target, sequence, 
         trial, question_cat, question, clarification,
         response_orig, response_cat, response, comments) %>%
  distinct()


# China -----
files_ch <- list.files("../raw data/adults/CHN_BEETLES_CSV Files/", 
                       pattern = "*.csv", full.names = T)

d_raw_ch <- plyr::ldply(files_ch, read_csv) %>%
  rename(target = character) %>%
  filter(!is.na(target), !is.na(sequence))

subj_key_ch <- d_raw_ch %>%
  rename(subj_id_original = `subject id`) %>%
  distinct(subj_id_original, target) %>%
  mutate(target = clean_char_fun(target)) %>%
  mutate(subj_id = paste("ch", "adults", 
                         str_pad(1:nrow(.), width = 3, side = "left", pad = "0"),
                         target,
                         sep = "_"))

d_ch <- d_raw_ch %>%
  rename(subj_id_original = `subject id`,
         age1 = `Date of Birth (or Age in China)`,
         age2 = `date of birth (or age for China)`,
         experimenter = `experimenter initials`,
         dot = date,
         trial = `#`,
         response = `circle one`) %>%
  # clean up target
  mutate(target = clean_char_fun(target)) %>%
  # switch to anonymized subject ids
  left_join(subj_key_ch) %>%
  select(-subj_id_original) %>%
  # deal with dob and age
  mutate(dot = gsub("2019", "19", dot),
         dot = gsub("2018", "18", dot),
         dot = gsub("2017", "17", dot)) %>%
  mutate(dob = age2,
         # hacky solution to year inconsistency
         dob = gsub("\\/19", "\\/", dob),
         dob = gsub("\\/\\/", "\\/19\\/", dob),
         dob = gsub("\\/20", "\\/", dob),
         dob = gsub("\\/\\/", "\\/20\\/", dob),
         dob = gsub("xx", "15", dob),
         dob = case_when(!grepl("\\/", dob) ~ NA_character_,
                         TRUE~ dob)) %>%
  mutate(age3 = case_when(!is.na(age1) ~ as.numeric(age1),
                          !is.na(age2) & !grepl("\\/", age2) ~ as.numeric(age2),
                          TRUE ~ NA_real_)) %>%
  mutate_at(vars(dob, dot),
            funs(parse_date_time2(.,
                                  orders = "mdy",
                                  cutoff_2000 = 19L))) %>%
  mutate(age4 = case_when(
    !is.na(dob) & !is.na(dot) ~ year(as.period(interval(start = dob, end = dot))),
    TRUE ~ NA_real_),
    age = case_when(!is.na(age3) ~ as.numeric(age3),
                    !is.na(age4) ~ as.numeric(age4),
                    TRUE ~ NA_real_),
    age = as.numeric(age)) %>%
  select(-age1, -age2, -age3, -age4) %>%
  # recode gender
  mutate(gender = recode_factor(tolower(gender),
                                "m" = "male",
                                "false" = "female",
                                "o" = "other")) %>%
  # make numeric
  mutate_at(vars(sequence, trial), funs(as.numeric)) %>%
  # code types of target 
  mutate(target_cat = case_when(
           target %in% levels_target_univ ~ "universal",
           !target %in% levels_target ~ "site specific"),
         target_orig = target,
         target = factor(target, levels = levels_target)) %>%
  # clean up and translate response
  rename(response_orig = response) %>%
  mutate(response_cat = case_when(response_orig %in% c("no", "kind of", "yes") ~ 
                                    response_orig,
                                  grepl("kind of", response_orig) ~ "kind of",
                                  TRUE ~ NA_character_),
         response_cat = factor(response_cat, levels = c("no", "kind of", "yes")),
         response = recode(response_cat, "no" = 0, "kind of" = 0.5, "yes" = 1)) %>%
  # add cb info
  left_join(cb) %>%
  # deal with types of questions
  mutate(question = case_when(trial == 27 ~ "have heart-feelings",
                              TRUE ~ question)) %>%
  mutate(question_cat = case_when(trial == 27 | is.na(question) ~ "site specific",
                                  trial > 23 ~ "universal add-on",
                                  !is.na(question) ~ "universal",
                                  TRUE ~ NA_character_)) %>%
  distinct()

# check for duplicates
d_ch_dup <- d_ch %>%
  filter(trial == "1") %>%
  count(subj_id) %>%
  filter(n > 1)

if (nrow(d_ch_dup) > 0) { print("DUPLICATES IN CHINA ADULTS") }

d_ch <- d_ch %>%
  # drop identifying info
  select(-dob, -dot) %>%
  # add country
  mutate(country = "China") %>%
  # reorder variables
  select(country, subj_id, gender, age,
         experimenter, location,
         target_cat, target_orig, target, sequence, 
         trial, question_cat, question, clarification,
         response_orig, response_cat, response, comments) %>%
  distinct()


# Vanuatu -----
files_vt <- list.files("../raw data/adults/VUT_BG_Adults CSV files/", 
                       pattern = "*.csv", full.names = T)

d_raw_vt <- plyr::ldply(files_vt, read_csv) %>%
  rename(target = character) %>%
  filter(!is.na(target), !is.na(sequence))

subj_key_vt <- d_raw_vt %>%
  rename(subj_id_original = `subject id`) %>%
  distinct(subj_id_original, target) %>%
  mutate(target = clean_char_fun(target)) %>%
  mutate(subj_id = paste("vt", "adults", 
                         str_pad(1:nrow(.), width = 3, side = "left", pad = "0"),
                         target,
                         sep = "_"))

d_vt <- d_raw_vt %>%
  # remove weird last column
  select(-ncol(d_raw_vt)) %>%
  rename(subj_id_original = `subject id`,
         dob = `date of birth`,
         experimenter = `experimenter initials`,
         dot = date,
         trial = `#`,
         response = `circle one`) %>%
  # clean up target
  mutate(target = clean_char_fun(target)) %>%
  # switch to anonymized subject ids
  left_join(subj_key_vt) %>%
  select(-subj_id_original) %>%
  # calculate age
  mutate_at(vars(dob, dot), 
            funs(parse_date_time2(str_remove(as.character(.), "^0"), 
                                  orders = "mdY",
                                  cutoff_2000 = 18L))) %>%
  mutate(age = year(as.period(interval(start = dob, end = dot))),
         age = as.numeric(age)) %>%
  # recode gender
  mutate(gender = recode_factor(tolower(gender),
                                "m" = "male",
                                "false" = "female",
                                "o" = "other")) %>%
  # make numeric
  mutate_at(vars(sequence, trial), funs(as.numeric)) %>%
  # code types of target 
  mutate(target_cat = case_when(
           target %in% levels_target_univ ~ "universal",
           !target %in% levels_target ~ "site specific"),
         target_orig = target,
         target = factor(target, levels = levels_target)) %>%
  # clean up and translate response
  rename(response_orig = response) %>%
  mutate(resposne_trans = tolower(response_orig),
         response_trans = gsub("ating", "kind of", resposne_trans),
         response_trans = case_when(resposne_trans %in% c("n", "no") ~ "no",
                                    # grepl("yes", response_trans) ~ "yes",
                                    TRUE ~ response_trans),
         response_trans = gsub("Missing Data", NA_character_, response_trans)) %>%
  mutate(response_cat = case_when(response_trans %in% c("no", "kind of", "yes") ~ 
                                    response_trans,
                                  grepl("kind of", response_trans) ~ "kind of",
                                  TRUE ~ NA_character_),
         response_cat = factor(response_cat, levels = c("no", "kind of", "yes")),
         response = recode(response_cat, "no" = 0, "kind of" = 0.5, "yes" = 1)) %>%
  # add cb info
  left_join(cb) %>%
  # deal with types of questions
  mutate(question_cat = case_when(is.na(question) ~ "site specific",
                                  trial > 23 ~ "universal add-on",
                                  !is.na(question) ~ "universal",
                                  TRUE ~ NA_character_)) %>%
  distinct()

# check for duplicates
d_vt_dup <- d_vt %>%
  filter(trial == "1") %>%
  count(subj_id) %>%
  filter(n > 1)

if (nrow(d_vt_dup) > 0) { print("DUPLICATES IN VANUATU ADULTS") }

d_vt <- d_vt %>%
  # drop identifying info
  select(-dob, -dot) %>%
  # add country
  mutate(country = "Vanuatu") %>%
  # reorder variables
  select(country, subj_id, gender, age,
         experimenter, location,
         target_cat, target_orig, target, sequence, 
         trial, question_cat, question, clarification,
         response_orig, response_cat, response) %>% # no comments column
  distinct()


# Subject keys

subj_key <- bind_rows(subj_key_us, subj_key_gh, subj_key_th, 
                      subj_key_ch, subj_key_vt) %>%
  select(starts_with("subj_"))


# Export CSVs

write_csv(subj_key, "../raw data/adults/subject_key_adults.csv")
write_csv(d_us, "../data/d_us_adults.csv")
write_csv(d_gh, "../data/d_gh_adults.csv")
write_csv(d_th, "../data/d_th_adults.csv")
write_csv(d_ch, "../data/d_ch_adults.csv")
write_csv(d_vt, "../data/d_vt_adults.csv")


