
library("dplyr")

hofstede <- read.csv(
  "https://geerthofstede.com/wp-content/uploads/2016/08/6-dimensions-for-website-2015-08-16.csv", 
  stringsAsFactors = FALSE,
  sep = ";"
)


idv_df <- select(hofstede,
       country, 
       idv)

idv_numeric_df <- mutate(idv_df,
                         idv = as.numeric(idv))

idv_not_na_df <- filter(idv_numeric_df,
       !is.na(idv))

idv_sorted_df <- arrange(idv_not_na_df,
                         idv)

idv_summary_df <- summarize(idv_sorted_df,
          avg_idv = mean(idv),
          min_idv = min(idv),
          max_idv = max(idv))

# find min and max individualism countries 
max_row <- filter(idv_sorted_df,
       idv == max(idv))

max_country <- pull(max_row, country)

min_row <- filter(idv_sorted_df, 
          idv == min(idv))


# All the above but chaining
# debugging, highlight where you went, then stop before you go to
# the part with the pipe operator 
idv_data_df <- hofstede %>%
  select(country, idv) %>%
  mutate(idv = as.numeric(idv)) %>%
  filter(!is.na(idv)) %>%
  arrange(idv) 

idv_summary2_df <- idv_data_df %>%
  summarize(avg_idv = mean(idv),
            min_idv = min(idv),
            max_idv = max(idv))

# find min and max idv countries 

max_country2 <- idv_data_df %>%
  filter(idv == max(idv)) %>%
  pull(country)
                 

min_country2 <- idv_data_df %>%
  filter(idv == min(idv)) %>% 
  pull(country)



