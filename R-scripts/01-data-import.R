

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(cowplot)
theme_set(theme_cowplot())


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




growth_rfus <- left_join(all_temp_rfus, well_ids, by = "well")

all_rfus2 %>% 
  # filter(grepl("dilution", treatment)) %>% 
  filter(treatment == "dilution-25" | treatment == "dilution-36") %>% 
  filter(!grepl("culture", treatment)) %>% 
  
  ggplot(aes(x = date_time, y = RFU, color = treatment, group = well)) + geom_point() + 
  geom_line()
ggsave("figures/growth-diluted.png", width = 8, height=  6)
ggsave("figures/growth-diluted.pdf", width = 8, height=  6)





# lipids ------------------------------------------------------------------

RFU_files <- c(list.files("data-raw/exp/lipids/", full.names = TRUE))

#Renaming files in list
names(RFU_files) <- RFU_files %>% 
  gsub(pattern = ".xlsx$", replacement = "") %>% 
  gsub(pattern = "data-raw/exp/lipids/", replacement = "")

#Pulling out RFU reads from files
all_plates <- map_df(RFU_files, read_excel, range = "B31:N39", .id = "file_name") %>% 
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

nile_red_rfus <- left_join(all_temp_rfus, well_ids, by = "well")

# all_rfus2 <- left_join(all_temp_rfus, well_ids, by = "well")

all_rfus2 %>% 
  # filter(grepl("dilution", treatment)) %>% 
  # filter(treatment == "dilution-25" | treatment == "dilution-36") %>% 
  filter(!grepl("culture", treatment)) %>% 
  filter(grepl("no-dilution", treatment)) %>% 
  
  ggplot(aes(x = date_time, y = RFU, color = treatment, group = well)) + geom_point() + 
  geom_line()
ggsave("figures/lipids-diluted.png", width = 8, height=  6)
ggsave("figures/lipids-diluted.pdf", width = 8, height=  6)


# nile_red_rfus <- all_rfus2

nile_red_rfus2 <- nile_red_rfus %>% 
  mutate(time_point = case_when(grepl("time0", file_name) ~ "time 0h",
                                grepl("time 0", file_name) ~ "time 0h",
                                grepl("time 3", file_name) ~ "time 3h",
                                
                                grepl("time 6", file_name) ~ "time 6h",
                                grepl("time 18h lipids", file_name) ~ "time 18h",
                                grepl("time 1", file_name) ~ "time 1h",
                                grepl("time 24", file_name) ~ "time 24h",
                                TRUE ~ NA)) %>% 
  rename(rfu_nile = RFU) %>% 
  # select(well, rfu_nile, time_point) %>% 
  filter(time_point != "time 1h") %>% 
  select(well, rfu_nile, time_point, treatment)


growth_rfus2 <- growth_rfus %>% 
  mutate(time_point = case_when(grepl("time0", file_name) ~ "time 0h",
                                grepl("time 0", file_name) ~ "time 0h",
                                grepl("time 3", file_name) ~ "time 3h",
                                
                                grepl("time 6", file_name) ~ "time 6h",
                                grepl("time 18h growth", file_name) ~ "time 18h",
                                grepl("time 1", file_name) ~ "time 1h",
                                grepl("time 24", file_name) ~ "time 24h",
                                TRUE ~ NA)) %>% 
  rename(rfu_growth = RFU) %>%
  select(well, rfu_growth, time_point, treatment)


all_data <- left_join(growth_rfus2, nile_red_rfus2) %>% 
  filter(time_point != "time 1h")


all_data2 <- all_data %>% 
  mutate(nile_per_cell = rfu_nile / rfu_growth)


all_data2 %>% 
  filter(treatment != "combo") %>% 
  filter(treatment == "no-dilution-25" | treatment == "no-dilution-36") %>% 
  ggplot(aes(x = time_point, y = nile_per_cell, color = treatment)) + geom_point()

library(plotrix)

all_data2 %>% 
  filter(treatment != "combo") %>% 
  filter(treatment == "no-dilution-25" | treatment == "no-dilution-36") %>% 
  group_by(treatment, time_point) %>% 
  summarise(mean_rfu = mean(nile_per_cell),
            se_rfu = std.error(nile_per_cell)) %>% 
  ggplot(aes(x = time_point, y = mean_rfu, color = treatment)) + geom_point() +
  geom_errorbar(aes(x = time_point, ymin = mean_rfu - se_rfu, ymax = mean_rfu + se_rfu))


all_data3 <- all_data2 %>% 
  filter(treatment != "combo") %>% 
  filter(treatment == "no-dilution-25" | treatment == "no-dilution-36") %>% 
  mutate(time_point = fct_relevel(time_point,c("time 0h","time 3h","time 6h", "time 18h", "time 24h")))


all_data3 %>% 
  filter(treatment != "combo") %>% 
  filter(treatment == "no-dilution-25" | treatment == "no-dilution-36") %>% 
  group_by(treatment, time_point) %>% 
  summarise(mean_rfu = mean(nile_per_cell),
            se_rfu = std.error(nile_per_cell)) %>% 
  ggplot(aes(x = time_point, y = mean_rfu, color = treatment)) + geom_point(size = 3) +
  geom_errorbar(aes(x = time_point, ymin = mean_rfu - se_rfu, ymax = mean_rfu + se_rfu, width = 0.3)) + ylab("Relative fluorescence units (RFU)") + xlab("Time point")
ggsave("figures/rfu-lipids-time.png", width = 8, height = 6)

