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


sum_temp_prep <- temp_raw %>%
  reframe(ave_temp = ave(temp), .by = week) %>%
  distinct() %>%
  mutate(ave_temp = round(ave_temp, 1))

#make a summary of the weeks days
sum_weeks <- bind_rows(
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

sum_temp_week <- left_join(

  sum_weeks,
  sum_temp_prep,

  by = 'week'
)
