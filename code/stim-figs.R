#devtools::install_github("debruine/webmorphR@dev")
library(webmorphR)
library(tidyverse)

emo_levels <- c(
  neu = "neutral",
  ang = "anger",
  dis = "disgust",
  fea = "fear",
  hap = "happy",
  sad = "sad",
  sur = "surprised"
)

# fig-img-examples ----

raw_stim <- read_stim("images/raw/", "std_") |>
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
