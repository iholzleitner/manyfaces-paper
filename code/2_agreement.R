##############################################################################
# --- INTERRATER AGREEMENT AND POINT(S) OF STABILITY
# --- Code for Results: Agreement Indicators and Results: Points of stability
##############################################################################

# Using parallel cores to speed things up; check how many cores are available
workers <- max(1, parallel::detectCores(logical = FALSE) - 1)

# pull out relevant data
data_trait <- data_exp |>
  filter(exp == "attractive" | exp == "dominant"  | exp == "trustworthy"  | exp == "gender-typical"  | exp == "memorable") |>
  mutate(exp = factor(exp, levels = c("attractive", "dominant", "trustworthy", "gender-typical", "memorable"))) |>
  mutate(dv = as.numeric(dv))

data_trait_unstd <- data_exp |>
  filter(grepl("unstd", exp)) |>
  mutate(dv = as.numeric(dv))

data_emo_int <- data_exp |>
  filter(exp == "anger" | exp == "happiness"  | exp == "disgust"  | exp == "surprise"  | exp == "sadness" | exp == "fear") |>
  mutate(dv = as.numeric(dv))

data_emo_cat <- data_exp |>
  filter(grepl("^em", exp))

# ------------------------------------------------------------------
# --- AGREEMENT INDICATORS FOR THE RATINGS
# ------------------------------------------------------------------

# --- STANDARDISED RATINGS: Corridors of stability
# At what sample size (if at all) do ratings reach acceptable reliability of .75 to .90?

# Set seed for reproducibility
set.seed(123)

# Set a stable parallel plan
future::plan(multisession, workers = workers)

# Get unique experiments
experiments <- unique(data_trait$exp)

# Run the function in parallel across experiments
corridor_icc_list <- furrr::future_map(
  experiments,
  function(exp_name) {
    data_subset <- data_trait |> filter(exp == exp_name)
    run_or_load_icc(
      exp_name = exp_name,
      data = data_subset,
      n_raters_seq = seq(10, 100, 10),
      n_iter = 50
    )
  },
  .options = furrr::furrr_options(seed = TRUE)
)

# Combine results
icc_corridor <- list_rbind(corridor_icc_list)

# Reset plan to sequential
future::plan(sequential)

# Produce summary
icc2k_corridor_summary <- icc_corridor |>
  group_by(experiment, n_raters_sampled) |>
  summarise(
    ICC2_k_median = median(`ICC(2,k)`, na.rm = TRUE),
    ICC2_k_low = quantile(`ICC(2,k)`, 0.025, na.rm = TRUE),
    ICC2_k_high = quantile(`ICC(2,k)`, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate median thresholds
thresholds_median_075 <- icc2k_corridor_summary |>
  group_by(experiment) |>
  summarise(
    N_for_075_median = ifelse(length(n_raters_sampled[ICC2_k_median >= 0.75]) > 0,
                              min(n_raters_sampled[ICC2_k_median >= 0.75]),
                              NA),
    .groups = "drop")

thresholds_median_090 <- icc2k_corridor_summary |>
  group_by(experiment) |>
  summarise(
    N_for_090_median = ifelse(length(n_raters_sampled[ICC2_k_median >= 0.90]) > 0,
                              min(n_raters_sampled[ICC2_k_median >= 0.90]),
                              NA),
    .groups = "drop")

n_075 <- thresholds_median_075 |> deframe()
n_090 <- thresholds_median_090 |> deframe()


# ------------------------------------------------------------------
# --- POINTS OF STABILITY
# ------------------------------------------------------------------

# --- STANDARDISED RATINGS

# To save time, output cached
if (!file.exists("cache/pos_traits_std.rds")) {
  set.seed(123)
  future::plan(multisession, workers = workers)
  stability_stats_traits <- calc_stability_stats(data = data_trait,
                                                 N = 100,
                                                 iterations = 300,
                                                 ci_interval = 0.95, ci_method = "percentile",
                                                 cos_threshold = 0.5,
                                                 save_means = FALSE, # only save if actually needed (huge)
                                                 col_map = list(trait = "exp",
                                                                stim_id = "trial_name",
                                                                rating = "dv"))

  future::plan(sequential)
  saveRDS(stability_stats_traits, "cache/pos_traits_std.rds")
} else {
  stability_stats_traits <- readRDS("cache/pos_traits_std.rds")
}

summary_ci_traits <- stability_stats_traits$cis |>
  group_by(exp, sample_size) |>
  summarise(ul = mean(ul),
            ll = mean(ll),
            .groups = "drop")


# --- UNSTANDARDISED RATINGS
if (!file.exists("cache/pos_traits_unstd.rds")) {
  set.seed(123)
  future::plan(multisession, workers = workers)
  stability_stats_traits_unstd <- calc_stability_stats(data = data_trait_unstd,
                                                       N = 100,
                                                       iterations = 300,
                                                       ci_interval = 0.95, ci_method = "percentile",
                                                       cos_threshold = 0.5,
                                                       save_means = FALSE,
                                                       col_map = list(trait = "exp",
                                                                      stim_id = "trial_name",
                                                                      rating = "dv"))

  future::plan(sequential)
  saveRDS(stability_stats_traits_unstd, "cache/pos_traits_unstd.rds")
} else {
  stability_stats_traits_unstd <- readRDS("cache/pos_traits_unstd.rds")
}

summary_ci_traits_unstd <- stability_stats_traits_unstd$cis |>
  group_by(exp, sample_size) |>
  summarise(ul = mean(ul),
            ll = mean(ll),
            .groups = "drop")


# --- EMOTION INTENSITY RATINGS
if (!file.exists("cache/pos_emo.rds")) {
  set.seed(123)
  future::plan(multisession, workers = workers)
  stability_stats_emo_int <- calc_stability_stats(data = data_emo_int,
                                                  N = 100,
                                                  iterations = 300,
                                                  ci_interval = 0.95, ci_method = "percentile",
                                                  cos_threshold = 0.5,
                                                  save_means = FALSE,
                                                  col_map = list(trait = "exp",
                                                                 stim_id = "trial_name",
                                                                 rating = "dv"))

  future::plan(sequential)
  saveRDS(stability_stats_emo_int, "cache/pos_emo.rds")
} else {
  stability_stats_emo_int <- readRDS("cache/pos_emo.rds")
}

summary_ci_emo_int <- stability_stats_emo_int$cis |>
  group_by(exp, sample_size) |>
  summarise(ul = mean(ul),
            ll = mean(ll),
            .groups = "drop")
