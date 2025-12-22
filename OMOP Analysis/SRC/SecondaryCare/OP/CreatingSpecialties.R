#  Load specialty mapping and prepare lookups
# ============================================================
library(dplyr)
library(stringr)
library(readxl)
library(stringdist)

### Specialties ----
specialty_mapping_data <- import("check_mainspef_tretspef_outpatient_GF.xlsx",
                                 sheet = "mapping_mainspef_tretspef")

### Create unique lookup tables ----
mainspef_lookup <- specialty_mapping_data %>%
  filter(!is.na(mainspef)) %>%
  distinct(mainspef, mainspef_code)

tretspef_lookup <- specialty_mapping_data %>%
  filter(!is.na(tretspef)) %>%
  distinct(tretspef, tretspef_code)

### Extract only the vectors for fuzzy matching ----
main_specialties  <- mainspef_lookup$mainspef
treat_specialties <- tretspef_lookup$tretspef


### Clean outpatient data ----
table_one_op_omop_appt <- appt_data %>%
  mutate(
    specialty_source_value_modified = specialty_source_value %>%
      str_remove("\\s*\\([^\\)]+\\)") %>%   # remove parentheses
      str_replace_all("&", "and") %>%
      str_to_lower() %>%
      str_trim()
  )


### Fuzzy matching function (Jaro–Winkler similarity) ----

# The function find_best_match() tries to find the closest match to a given
# word or phrase (value) from a list of possible options (lookup_vector).
# “fuzzy matching” function because it doesn’t require exact matches.
# Even if there are small spelling mistakes or differences, it can still find the best match.

# Jaro–Winkler similarity is a way to measure how similar two strings (words) are, 
# based on:
#  1. Matching characters (letters that appear in both words and 
# are close to the same position).
#  2. Number of transpositions (how many letters are swapped).
#  3. Common prefix bonus (if the start of the words is the same, 
# it scores slightly higher).
# It gives a score between:
#   0 = completely different
#   1 = exactly the same

find_best_match <- function(value, lookup_vector, min_similarity = 0.90) {
  # If the input is missing (NA), empty, or there’s nothing to compare to, 
  # it returns NA (nothing found).
  if (is.na(value) || value == "" || length(lookup_vector) == 0) return(NA_character_)
  
  # Calculates how similar the value is to every word in lookup_vector using 
  # the Jaro–Winkler method.
  # stringdist() gives a distance (0 = identical, 1 = very different),
  # so 1 - distance gives a similarity (1 = identical, 0 = completely different).
  sim <- 1 - stringdist(value, lookup_vector, method = "jw")
  
  # Finds the index of the most similar word (best_idx) and 
  # its similarity score (best_sim).
  best_idx <- which.max(sim)
  best_sim <- sim[best_idx]
  
  # If the best match is at least 90% similar (or the threshold you set), 
  # it returns that word.
  # Otherwise, it returns NA, meaning no good enough match was found.
  if (best_sim >= min_similarity) {
    return(lookup_vector[best_idx])
  } else {
    return(NA_character_)
  }
}

### Optimized fuzzy matching on unique specialties ----
unique_specialties <- unique(
  na.omit(
    table_one_op_omop_appt$specialty_source_value_modified
  )
)

### Match only once per unique name ----
main_matches <- tibble(
  specialty_source_value_modified = unique_specialties,
  mainspef = sapply(unique_specialties, find_best_match,
                    lookup_vector = main_specialties,
                    min_similarity = 0.85)
)

treat_matches <- tibble(
  specialty_source_value_modified = unique_specialties,
  tretspef = sapply(unique_specialties, find_best_match,
                    lookup_vector = treat_specialties,
                    min_similarity = 0.85)
)

### Merge back to the full dataset ----
table_one_op_omop_appt <- table_one_op_omop_appt %>%
  left_join(main_matches,  by = "specialty_source_value_modified") %>%
  left_join(treat_matches, by = "specialty_source_value_modified")


###  Add back corresponding codes from the mapping ----
table_one_op_omop_appt <- table_one_op_omop_appt %>%
  left_join(mainspef_lookup, by = "mainspef") %>%
  left_join(tretspef_lookup, by = "tretspef")

##  Checks ----
check <- table_one_op_omop_appt %>%
  select(specialty_source_value_modified, mainspef, tretspef) %>%
  dplyr::count(specialty_source_value_modified, mainspef, tretspef, sort = TRUE)

write.csv(check,
          here::here("OMOP Analysis", "Results", "SecondaryCareAnalysis",
                     "check_mainspef_tretspef_outpatient.csv"),
          row.names = FALSE)

# this file has been manually checked to make sure that the values were correct, and some were modified, some were newly associated too
check_gf <- read_excel("Inputs_HE/check_mainspef_tretspef_outpatient_GF.xlsx")