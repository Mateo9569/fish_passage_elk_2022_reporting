
# install.packages("weathercan",
#                  repos = c("https://ropensci.r-universe.dev",
#                            "https://cloud.r-project.org"))

# download weatehr can information and make some plots

# sparwood
stations_search(coords = c(49.7309, -114.8862), dist = 50)
#
# # elkford
# stations_search(coords = c(50.0246, -114.9235), dist = 50)

w_raw <- weather_dl(station_ids = 52959, start = "2022-04-01", end = "2022-11-07")

w_prep <- w_raw %>%
  mutate(week = lubridate::isoweek(date),
         day_of_week = lubridate::wday(date, week_start = getOption("lubridate.week.start", 1))
  )


w_week_prep_dates <- w_prep %>%
  filter(day_of_week %in% c(1,7)) %>%
  distinct(date, week, day_of_week) %>%
  group_by(week) %>%
  summarise(day_first = min(date),
            day_last = max(date))

w_week_prep <- w_prep %>%
  reframe(ave_temp_air = ave(temp), .by = week) %>%
  distinct() %>%
  mutate(ave_temp_air = round(ave_temp_air, 1))


w_week <- left_join(

  w_week_prep_dates,
  w_week_prep,

  by = 'week'
)

# put in the sqlite
conn <- rws_connect("data/bcfishpass.sqlite")
rws_write(w_week, exists = F, delete = TRUE,
          conn = conn, x_name = "w_week")
rws_list_tables(conn)
rws_disconnect(conn)
