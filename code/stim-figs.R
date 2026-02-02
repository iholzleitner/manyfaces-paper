#devtools::install_github("debruine/webmorphR@dev")
library(webmorphR)
library(tidyverse)

# Model data
ct <- cols(
  age = col_integer(),
  height = col_integer(),
  weight = col_integer(),
  ethnicity_rec = col_factor(levels = c("White", "Black", "Asian", "Indigenous", "MENA", "Latine", "Mixed", "Ambiguous label"), include_na = TRUE)
)
data_models <- read_csv("../data/manyfaces-pilot-models_cleaned.csv",
                        col_types = ct)

emo_levels <- c(
  neu = "neutral",
  ang = "anger",
  dis = "disgust",
  fea = "fear",
  hap = "happy",
  sad = "sad",
  sur = "surprised"
)

image_type_n <- data_models |>
  select(model_id:unstd_neu) |>
  pivot_longer(std_ang:unstd_neu) |>
  filter(value == 1) |>
  count(name) |>
  separate(name, c("type", "emotion")) |>
  pivot_wider(names_from = type, values_from = n) |>
  mutate(emotion = factor(emotion, names(emo_levels), emo_levels)) |>
  arrange(emotion)

# fig-img-examples ----

raw_stim <- read_stim("images/raw/", "MF0006_0007_") |>
  resize(1000)

df <- data.frame(id = names(raw_stim)) |>
  separate(id,
           c("lab", "img", "std", "emo", "view"),
           remove = FALSE,
           convert = TRUE) |>
  mutate(emo = factor(emo, names(emo_levels))) |>
  arrange(std, view, emo)

raw_stim <- add_info(raw_stim, df, .by = "id")

std_0 <- df |> filter(std == "std", view == 0) |> pull(id)
unstd_0 <- df |> filter(std == "unstd", view == 0) |> pull(id)
l90 <- df |> filter(view == "l90") |> pull(id)
l45 <- df |> filter(view == "l45") |> pull(id)
r45 <- df |> filter(view == "r45") |> pull(id)
r90 <- df |> filter(view == "r90") |> pull(id)

labels <- blank(length(emo_levels), raw_stim[[1]]$width, raw_stim[[1]]$width /
                  6) |>
  label(
    emo_levels,
    size = 100,
    color = "black",
    weight = 700,
    gravity = "center"
  )

std_labels <- blank(6, raw_stim[[1]]$height, raw_stim[[1]]$height / 3) |>
  label(
    c(
      "Standardised\nFront",
      "Standardised\nLeft 90",
      "Standardised\nLeft 45",
      "Standardised\nRight 45",
      "Standardised\nRight 90",
      "Unstandardised\nFront"
    ),
    size = 90,
    color = "black",
    weight = 700,
    gravity = "center"
  ) |>
  rotate(-90, keep_size = FALSE)

raw_plot <- plot_rows(
  c(blank(), labels),
  c(std_labels[[1]], raw_stim[std_0]),
  c(std_labels[[2]], raw_stim[l90]),
  c(std_labels[[3]], raw_stim[l45]),
  c(std_labels[[4]], raw_stim[r45]),
  c(std_labels[[5]], raw_stim[r90]),
  c(std_labels[[6]], raw_stim[unstd_0])
)

write_stim(raw_plot, "fig", "img-examples", "png")

# fig-img-processing ----

std_neu_0 <- read_stim("images/", "_std_neu_0", recursive = TRUE)

align_tem <- draw_tem(std_neu_0$align)

img_proc <- c(std_neu_0$raw, align_tem, std_neu_0$wb) |>
  resize(height = align_tem[[1]]$height) |>
  label(LETTERS[1:3], gravity = "northwest",
        size = 100, location = "+20+20") |>
  plot_stim()

write_stim(img_proc, "fig", "img-processing", "png")

# fig_angles ----
angle_labels <- c("Left 90", "Left 45", "Front", "Right 45", "Right 90")
stim_angle <- paste0("MF0006_0007_std_neu_", c("l90", "l45", "0", "r45", "r90"))
fig_angle <- raw_stim[stim_angle] |>
  mlabel(angle_labels, "north-west", "+10+10") |>
  plot_stim(nrow = 1)
write_stim(fig_angle, "fig", "angles", "png")

# fig-stim-types ----
wb_stim <- read_stim("images/wb/", "MF0006_0007_") |>
  resize(1000)

n <- c(image_type_n$unstd[[1]], image_type_n$std)
stim_type_labels <- c("unstandardised", emo_levels) |>
  paste0(" (", n, ")")

stim <- sprintf("MF0006_0007_std_%s_0", names(emo_levels)) |>
  c("MF0006_0007_unstd_neu_0", x = _)
fig_stim_types <- wb_stim[stim] |>
  pad(150, 0, 0, 0) |>
  mlabel(stim_type_labels) |> # , "north-west", "+10+10") |>
  plot_stim(nrow = 1)
write_stim(fig_stim_types, "fig", "stim-types", "png")
