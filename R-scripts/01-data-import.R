

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)


RFU_files <- c(list.files("data-raw/exp/growth/", full.names = TRUE))

#Renaming files in list
names(RFU_files) <- RFU_files %>% 
  gsub(pattern = ".xlsx$", replacement = "") %>% 
  gsub(pattern = "data-raw/exp/growth/", replacement = "")

#Pulling out RFU reads from files
all_plates <- map_df(RFU_files, read_excel, range = "B30:N38", .id = "file_name") %>% 
  rename(row = `...1`)


all_times <- map_df(RFU_files, read_excel, range = "A6:B8", .id = "file_name") %>% 
  clean_names() %>% 
  spread(key = plate_number, value = plate_1) %>% 
  separate(Time, into = c("crap", "Time"), sep = " ") %>% 
  select(-crap) 



all_plates2 <- dplyr::left_join(all_plates, all_times, by = "file_name")

#Merge plate info and time stamps
all_temp_rfus <- all_plates2 %>% 
  gather(key = column, value = RFU, 3:14) %>%
  unite(row, column, col = "well", remove = FALSE, sep = "") %>% 
  mutate(column = formatC(column, width = 2, flag = 0)) %>%
  mutate(column = str_replace(column, " ", "0")) %>% 
  unite(col = well, row, column, sep = "") %>% 
  unite(col = date_time, Date, Time, sep = " ") %>% 
  mutate(date_time = ymd_hms(date_time)) 



all_temp_rfus %>% 
  ggplot(aes(x = date_time, y = RFU, color = well, group = well)) + geom_point() + 
  geom_line()


well_ids <- read_excel("data-raw/well-numbers.xlsx") %>% 
  mutate(treatment = case_when(grepl("01", number) ~ "no-dilution-25",
                               grepl("05", number) ~ "no-dilution-36",
                               grepl("03", number) ~ "dilution-25",
                               grepl("07", number) ~ "dilution-36",
                               grepl("08", number) ~ "no-dilution-25-culture",
                               grepl("09", number) ~ "dilution-25-culture",
                               grepl("10", number) ~ "combo",
                               grepl("11", number) ~ "no-dilution-36-culture",
                               grepl("12", number) ~ "dilution-36-culture",
        TRUE ~ "other")) %>% 
  mutate(well = paste(well, number, sep = ""))



all_rfus2 <- left_join(all_temp_rfus, well_ids, by = "well")

all_rfus2 %>% 
  filter(grepl("no-dilution", treatment)) %>% 
  
  ggplot(aes(x = date_time, y = RFU, color = treatment, group = well)) + geom_point() + 
  geom_line()
