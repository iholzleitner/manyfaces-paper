##############################################################################
# --- INTERRATER AGREEMENT AND POINT(S) OF STABILITY
# --- Code for Results: Agreement Indicators and Results: Points of stability
##############################################################################

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
future::plan(multisession, workers = 3)

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
thresholds_median <- icc2k_corridor_summary |>
  group_by(experiment) |>
  summarise(
    N_for_075_median = ifelse(length(n_raters_sampled[ICC2_k_median >= 0.75]) > 0,
                              min(n_raters_sampled[ICC2_k_median >= 0.75]),
                              NA),
    .groups = "drop")

#print(thresholds_median)


# ------------------------------------------------------------------
# --- POINTS OF STABILITY
# ------------------------------------------------------------------

