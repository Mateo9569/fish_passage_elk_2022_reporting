# tidy the temp data and calculate daily, weekly and monthly averages

temp_raw <- readxl::read_excel('data/Weigert_Creek_Temp_Profile_CWF.xlsx',
                               skip = 1,
                               col_types = c('numeric', 'text', 'numeric')) %>%
  purrr::set_names(c('row_id', 'date', 'temp')) %>%
  mutate(date = stringr::str_replace_all(date, '  ', '')) %>%
  tidyr::separate(date,
                  into = c('date', 'time'),
                  sep = c(" "),
                  remove = F) %>%
  # mutate(time = format(strptime(time, "%H:%M:%S %p"), "%H:%M:%S"))
  mutate(date = parse_date_time(date,c('mdY')),
         month = lubridate::month(date),
         week = lubridate::isoweek(date),
         day_of_week = lubridate::wday(date, week_start = getOption("lubridate.week.start", 1))
         # day_of_week = as.numeric(strftime(date, "%w"))
  )

#------------summary by week-------------------
temp_prep <- temp_raw %>%
  reframe(ave_temp_h20 = ave(temp), .by = week) %>%
  distinct() %>%
  mutate(ave_temp_h20 = round(ave_temp_h20, 1))

#make a summary of the weeks days
temp_week_prep <- bind_rows(
  temp_raw %>%
    select(date, week, day_of_week) %>%
    slice(1),

  temp_raw %>%
  filter(day_of_week %in% c(1,7)) %>%
  distinct(date, week, day_of_week)
) %>%
  group_by(week) %>%
  summarise(day_first = min(date),
            day_last = max(date))

temp_week <- left_join(

  temp_week_prep,
  temp_prep,

  by = 'week'
)

#------------celcius degree days--------------------
temp_cdd <- temp_raw %>%
  reframe(ave_dt = ave(temp), .by = date) %>%
  distinct() %>%
  mutate(ave_dt = round(ave_dt, 1)) %>%
  filter(date > '2022-06-26' &
           date < '2022-10-17' )  %>%
  # group_by(day) %>%
  summarize(cdd = sum(ave_dt))


