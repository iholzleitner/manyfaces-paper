# ------------------------------------------------------------------
# COMPILING RAW DATA (ARCHIVE ONLY -- not to be run)
#
# Notes:
#   - Creates data/manyfaces-pilot-exp.csv and data/manyfaces-pilot-quest.csv files from raw inputs
#   - This workflow requires the data-raw directory, which is not shared on github
# ------------------------------------------------------------------

######################
### --- 1. SQL --- ###
######################

# SQL used to downloading the data from Experimentum.
# Download in chunks of 50000 rows to avoid file download limits on the site (not needed if downloading directly from SQL).

# -- BEGIN SQL
# SELECT
# session.id as session_id, project_id, exp.res_name as exp_name, exp_id,
# session.user_id, user.sex as user_sex, user.status as user_status,
# ROUND(DATEDIFF(ed.dt, REPLACE(birthday, "-00","-01"))/365.25, 1) AS user_age,
# trial.name as trial_name,
# trial_n,
# `order`,
# dv,
# rt,
# ed.side,
# ed.dt
# FROM session
# LEFT JOIN user USING (user_id)
# LEFT JOIN exp_data AS ed ON ed.session_id = session.id
# LEFT JOIN exp ON exp.id = ed.exp_id
# LEFT JOIN trial USING (exp_id, trial_n)
# WHERE session.project_id = 1136
# AND user.status IN ("guest", "registered")
# AND exp_id IN (1384, 1400, 1399, 1398, 1401, 1402, 1403,
#                1404, 1405, 1397, 1390, 1389, 1388, 1387,
#                1386, 1385, 1382, 1381, 1380, 1379, 1377)
# LIMIT 50000
# OFFSET 0
# -- END SQL

####################
### --- 2. R --- ###
####################

# # combine multiple downloads into one file
# exp_raw <- list.files("data-raw/exp", full.names = TRUE) |>
#   read_csv(show_col_types = FALSE) |>
#   unique() |>
#   filter(user_status %in% c("guest", "registered"))
#
# write_csv(exp_raw, paste0("data-raw/ManyFaces-Pilot-Ratings-exps_", Sys.Date(), ".csv"))
#
# # get most recent files
# exp_file <- list.files("data-raw", "ManyFaces-Pilot-Ratings-exps",
#                        full.names = TRUE) |>
#   sort(decreasing = TRUE) |>
#   pluck(1)
#
# exp_raw <- read_csv(exp_file, show_col_types = FALSE) |>
#   filter(user_status %in% c("guest", "registered")) |>
#   unique()
#
# # get most recent files
# quest_file <- list.files("data-raw", "ManyFaces-Pilot-Ratings-quests",
#                          full.names = TRUE) |>
#   sort(decreasing = TRUE) |>
#   pluck(1)
#
# quest_raw <- read_csv(quest_file, show_col_types = FALSE) |>
#   filter(user_status %in% c("guest", "registered")) |>
#   unique()
#
# # write to the data directory
# write_csv(exp_raw, "data/manyfaces-pilot-exp.csv")
# write_csv(quest_raw, "data/manyfaces-pilot-quest.csv")
