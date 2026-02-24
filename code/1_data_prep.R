####################################################################
# --- DATA PREPARATION SCRIPT
# --- Reshaping and cleaning raw data, preregistered exclusions
####################################################################

# ------------------------------------------------------------------
# --- PROJECT STRUCTURE
# ------------------------------------------------------------------

# Read in the full project structure from the project file to map experiment names to numbers.

proj <- jsonlite::read_json("data/project_1136_structure.json")

exp_data <- purrr::map_df(proj, \(comp) {
  if (comp$component_type == "exp") {
    data.frame(
      exp_id = comp$id,
      name = comp$name,
      res_name = comp$res_name,
      instructions = comp$instructions,
      question = comp$question,
      exptype = comp$exptype,
      trial_order = comp$trial_order,
      total_stim = comp$total_stim,
      random_stim = comp$random_stim,
      trials = length(comp$trial),
      stim = length(comp$stim)
    )
  } else {
    NULL
  }
}) |>
  mutate(exp = sub("ManyFaces? Pilot Ratings: ", "", res_name) |> trimws())


trial_data <- purrr::map_df(proj, \(comp) {
  if (comp$component_type == "exp") {
    purrr::map_df(comp$trial, \(trial) {
      data.frame(
        exp_id = comp$id,
        n = trial$trial_n,
        name = trial$name,
        img_id = trial$center_img,
        img_path = comp$stimuli[[as.character(trial$center_img)]]
      )
    })
  } else {
    NULL
  }
}) |>
  mutate(name = sub("^(manyfaces|attention_checks)/", "", name))


# ------------------------------------------------------------------
# --- DATA PREP
# ------------------------------------------------------------------

# --- READ AND RESHAPE RAW DATA

exp_raw <- read_csv("data/manyfaces-pilot-exp.csv",
                    show_col_types = FALSE)

ed <- exp_data |>
  select(exp_id, exp, trials) |>
  rename(trials_total = trials)

exp_long_raw <- exp_raw |>
  select(session_id, exp_id, trial_name, dv, rt, dt) |>
  unique() |>
  mutate(trial_name = sub("^(manyfaces|attention_checks)/", "", trial_name)) |>
  left_join(ed, by = "exp_id")


quest_raw <- read_csv("data/manyfaces-pilot-quest.csv",
                      show_col_types = FALSE) |>
  dplyr::select(session_id, q_name, dv, endtime) |>
  unique() |>
  pivot_wider(names_from = q_name, values_from = dv) |>
  mutate(age = as.integer(age))


model_raw <- read_csv("data/manyfaces-pilot-models.csv",
                      show_col_types = FALSE)

# --- PRELIMINARY DATA CLEANING: Model data

ethnicity_recode <- read_csv("data/recode_eth_model.csv", na = "", show_col_types = FALSE)
gender_recode <- read_csv("data/recode_gender_model.csv", na = "", show_col_types = FALSE)

data_models <- model_raw |>
  separate_wider_delim(ID,
                       delim = "_",
                       names = c("lab_id", "model_id"),
                       too_many = "drop") |>
  mutate(across(where(is.character), ~ na_if(.x, "NULL"))) |>
  # recode gender and ethnicity
  mutate(gender = tolower(gender),
         ethnicity = tolower(ethnicity)) |>
  left_join(gender_recode, by = "gender") |>
  left_join(ethnicity_recode, by = "ethnicity") |>
  relocate(gender_rec, .after = gender) |>
  relocate(ethnicity_rec, .after = ethnicity) |>
  # recode height and weight
  # one person reported weight of 37 kg at 163 cm -- recode as NA? Would be a BMI of 13.9
  mutate(height_cm = case_when(height_units == "cm" & as.numeric(height) < 100 ~ as.numeric(height) * 2.54, # likely mislabelled inches
                               height_units == "in" & as.numeric(height) > 100 ~ as.numeric(height), # likely mislabelled cms
                               height_units == "in" ~ as.numeric(height) * 2.54,
                               height_units == "cm" ~ as.numeric(height),
                               .default = NA_real_),
         weight_kg = case_when(as.numeric(weight) == 0 ~ NA_real_,
                               # Note: one participant indicated weight as 37kg and height as 163cm, which would mean BMI of ~14.0 - data entry error or legitimate value?
                               weight_units == "lb" & as.numeric(weight) < 80 ~ as.numeric(weight),            # likely mislabelled kg
                               weight_units == "lb" ~ as.numeric(weight) / 2.20462,
                               weight_units == "kg" ~ as.numeric(weight),
                               as.numeric(weight) == 0 ~ NA,
                               .default = NA_real_)) |>
  relocate(height_cm, .after = height_units) |>
  relocate(c(weight, weight_units, weight_kg), .after = height_cm)


# --- PRELIMINARY DATA CLEANING Questionnaire data
quest_eth_recode <- read_csv("data/recode_eth_quest.csv", na = "", show_col_types = FALSE)

data_quest_pre_exclusions <- quest_raw |>
  # recode ethnicity
  mutate(ethnicity = tolower(ethnicity)) |>
  left_join(quest_eth_recode, by = "ethnicity") |>
  relocate(ethnicity_rec, .after = ethnicity)

# --- PRELIMINARY DATA CLEANING: Experimental data

initial_raters<- exp_long_raw |>
  count(session_id) |>
  count() |>
  pull(n)

initial_completions <- exp_long_raw |>
  count(session_id, exp_id) |>
  count() |>
  pull(n)

# -- Remove duplicate trials
# Some participants completed more than the maximum amount of trials in an experiment. Duplicate trials were removed by only retaining participants' initial ratings of a particular stimulus.

dupl_trials_rm <- exp_long_raw |>
  arrange(session_id, exp_id, trial_name, trials_total, dt) |>
  distinct(session_id, exp_id, trial_name, trials_total, .keep_all = TRUE)

# -- Remove incomplete trials
incomplete <- dupl_trials_rm |>
  count(session_id, exp_id, trials_total) |>
  filter(n < trials_total)

complete <- dupl_trials_rm |>
  filter(!session_id %in% incomplete$session_id)

# -- Remove duplicate completions
# If participants completed more than one experiment, only data from the first was retained.

dupl_exp_ids <- complete |>
  # Mark participants that participated twice and establish which exp they did first
  group_by(session_id, exp_id) |>
  summarise(first_dt = min(dt), .groups = "drop") |>
  group_by(session_id) |>
  mutate(n_exp = n()) |>
  mutate(first_exp = exp_id[which.min(first_dt)]) |>
  ungroup() |>
  filter(n_exp > 1 & exp_id != first_exp)

exp_long <- complete |>
  anti_join(dupl_exp_ids, by = c("session_id", "exp_id" = "first_exp"))

# -- Complete raters prior to preregistered exclusions:
complete_raters <- exp_long |>
  count(session_id) |>
  count() |>
  pull(n)

# ------------------------------------------------------------------
# --- PREREGISTERED EXCLUSIONS
# ------------------------------------------------------------------

# --- OVERLY CONSISTENT RESPONSES
# Participants were excluded based on overly consistent responding, i.e. if they responded to at least 90% of trials identically. Please note that four additional participants with only two unique responses across all trials were identified during reliability analyses and excluded under the original 'overly consistent responding' criterion.
overly_consistent_prereg <- exp_long |>
  filter(!startsWith(trial_name, "check_")) |>
  summarise(
    same_pcnt = max(tabulate(match(dv, unique(dv)))) / n(),
    .by = c(session_id, exp_id)) |>
  dplyr::select(session_id, exp_id, same_pcnt) |>
  filter(same_pcnt >= 0.90)

overly_consistent_additional <- exp_long |>
  filter(!startsWith(trial_name, "check_")) |>
  summarise(
    n_unique = n_distinct(dv, na.rm = TRUE),
    .by = c(session_id, exp_id)) |>
  dplyr::select(session_id, exp_id, n_unique) |>
  filter(n_unique <= 2)

overly_consistent <- overly_consistent_prereg |>
  full_join(overly_consistent_additional, by=c("session_id", "exp_id"))

# --- OVERLY FAST RESPONSES
# Participants were excluded based on overly fast responding, i.e. if their median reaction time fell below the 1st percentile of the overall distribution of median reaction times.

med_rt <- exp_long |>
  summarise(med_rt = median(rt),
            .by = c(session_id, exp_id))

overly_fast <- med_rt |>
  filter(med_rt < quantile(med_rt, probs = 0.01))


# --- SELF-REPORTED HONESTY CHECK
# Participants were excluded based on self-reported honesty check, i.e. if they responded not taking the study seriously vs. taking it authentically.

honesty_check_failed <- data_quest_pre_exclusions |>
  select(session_id, honesty_check = try) |>
  unique() |>
  filter(honesty_check != 2)

# --- ATTENTION CHECKS
# Participants were excluded if they failed one or more attention checks.

attn_checks_failed <- exp_long |>
  select(session_id:dv) |>
  filter(grepl("check", trial_name)) |>
  mutate(check_type = sub("check_[a-z0-9-]+_", "", trial_name),
         check_type = ifelse(exp_id == 1400, substr(check_type, 4, 6), check_type)) |>
  summarise(attn_checks_passed = mean(check_type == dv),
            .by = c("session_id", "exp_id")) |>
  filter(attn_checks_passed < 6/7)

# --- SUMMARY OF EXCLUSIONS

exclusions <- overly_consistent |>
  full_join(overly_fast, by = c("session_id", "exp_id")) |>
  full_join(honesty_check_failed, by = c("session_id")) |>
  full_join(attn_checks_failed, by = c("session_id", "exp_id"))

excluded_raters <- nrow(exclusions)

# --- MAKE EXCLUSIONS
data_exp <- exp_long |>
  anti_join(exclusions, by = "session_id") |>
  filter(!grepl("check_", trial_name))

data_quest <- data_quest_pre_exclusions |>
  filter(session_id %in% data_exp$session_id)

final_raters <- unique(data_exp$session_id) |> length()

# write_csv(data_exp, "data/manyfaces-pilot-exp_cleaned.csv")
# write_csv(data_quest, "data/manyfaces-pilot-quest_cleaned.csv")
