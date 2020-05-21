# levels
levels_country <- c("US", "Ghana", "Thailand", "China", "Vanuatu")
levels_target <- c("rocks", "flowers", 
                   # "beetles*", 
                   "beetles", "crickets",
                   "chickens", "mice", "dogs", "pigs", "children", 
                   "cellphones", "robots", "aliens", "ghosts", "god")

levels_target_univ <- c("rocks", "flowers", 
                        # "beetles*", 
                        "beetles", "crickets",
                        "chickens", "mice", "dogs", "children", 
                        "cellphones", "ghosts", "god")

# contrasts (effect-coding)
contrast_country <- cbind("_gh" = c(-1, 1, 0, 0, 0),
                          "_th" = c(-1, 0, 1, 0, 0),
                          "_ch" = c(-1, 0, 0, 1, 0),
                          "_vt" = c(-1, 0, 0, 0, 1))

contrast_country2 <- cbind("_gh" = c(-1, 1, 0, 0, 0, 0),
                           "_gh_eng" = c(-1, 0, 1, 0, 0, 0),
                           "_th" = c(-1, 0, 0, 1, 0, 0),
                           "_ch" = c(-1, 0, 0, 0, 1, 0),
                           "_vt" = c(-1, 0, 0, 0, 0, 1))
