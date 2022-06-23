
##################################### data download and format check
library(tidyverse)
library(lubridate)

# ensure that all data frames from all years have the same columns and collate data frames
# the data frame budg contains budget information for each 319 grant from the GRTS database
# the data frame summ contains additional project summary information for each 319 grant
file_crawl_budget <- list.files("data/grts_project-budget-report")
file_crawl_summary <- list.files("data/grts_project-summary-report")

check_b <- NULL
budg <- NULL
for(i in 1:length(file_crawl_budget)){
  temp <- read.csv(paste0("data/grts_project-budget-report/", file_crawl_budget[i]))
  temp_head <- as.data.frame(t(names(temp)))
  check_b <- rbind(temp_head, check_b)
  budg <- rbind(budg, temp)
}

check_s <- NULL
summ <- NULL
for(i in 1:length(file_crawl_summary)){
  temp <- read.csv(paste0("data/grts_project-summary-report/", file_crawl_summary[i]))
  temp_head <- as.data.frame(t(names(temp)))
  check_s <- rbind(temp_head, check_s)
  summ <- rbind(summ, temp)
}

check_b %>% dplyr::summarize(across(V1:V19, unique)) %>% nrow()
check_s %>% dplyr::summarize(across(V1:V15, unique)) %>% nrow()

# fix date entry errors in GRTS data and join project budget and summary information
date_fix <- function(dt){
  return(str_replace(dt, "00[:digit:][:digit:]", 
                     if_else(str_sub(dt, 3,4) %>% as.numeric()<51, 
                             paste0("20", str_sub(dt, 3, 4)),
                             paste0("19", str_sub(dt, 3, 4)))))
}

data <- budg %>% dplyr::rename("Total.319h.Funds.Budget"="Total.319h.Funds") %>% 
  full_join(summ %>% dplyr::rename("Total.319h.Funds.Summary"="Total.319h.Funds"), 
            by = c("State","Award.Fiscal.Year","Grant.No","Grantee","Project.Number","Title")) %>% 
  mutate(Start.Date=str_extract(Project.Start, "\\d\\d/\\d\\d/\\d\\d\\d\\d") %>% mdy(), 
         End.Date=str_extract(Project.End, "\\d\\d/\\d\\d/\\d\\d\\d\\d") %>% mdy(), 
         Fed.Money = readr::parse_number(Total.319h.Funds.Budget), 
         Total.Money = readr::parse_number(Total.Budget)) %>% 
  mutate(Start.Date.Replace=if_else(str_sub(Start.Date, 1,4)=="0199", paste0("1999", str_sub(Start.Date, 5, 10)), 
                                    if_else(str_sub(Start.Date, 1,4)=="0100", paste0("2001", str_sub(Start.Date, 5, 10)), 
                                            if_else(str_sub(Start.Date, 1,4)=="0219", paste0("2019", str_sub(Start.Date, 5,10)), date_fix(Start.Date))
                                    ))) %>% 
  mutate(End.Date.Replace=if_else(str_sub(End.Date, 1,4)=="0199", paste0("1999", str_sub(End.Date, 5, 10)), 
                                  if_else(str_sub(End.Date, 1,4)=="0100", paste0("2001", str_sub(End.Date, 5, 10)), 
                                          if_else(str_sub(End.Date, 1,4)=="0219", paste0("2019", str_sub(End.Date, 5,10)), date_fix(End.Date))
                                  ))) %>% 
  mutate(Start.Year = year(ymd(Start.Date.Replace)), 
         End.Year = year(ymd(End.Date.Replace)))

################################################# summarize spending

# summarized 319 spending from 2007-2019 by state
summarized_file <- data %>% filter(is.na(End.Year)==F) %>% 
  group_by(State, End.Year) %>% 
  dplyr::summarize(Total.Money.Spent = sum(Total.Money, na.rm=T)) %>% 
  filter(End.Year>=2007, End.Year<=2019) %>% 
  ungroup() %>% 
  group_by(State) %>% 
  dplyr::summarize(Total.State.Money=sum(Total.Money.Spent, na.rm=T)) %>% 
  arrange(desc(Total.State.Money))

# summarized 319 spending from 2007-2019 by state and year
Award.Fiscal.Year <- rep(2007:2019, 50)
State <- rep(state.abb, each=13)
template <- cbind(State, Award.Fiscal.Year) %>% as.data.frame() %>% mutate(Award.Fiscal.Year=as.integer(Award.Fiscal.Year))

year_summary <- data %>% filter(is.na(Award.Fiscal.Year)==F) %>%
  filter(!(State %in% c("GU", "VI", "AS", "PR", "DC"))) %>% 
  filter(Award.Fiscal.Year>=2007, Award.Fiscal.Year<=2019) %>% 
  group_by(State, Award.Fiscal.Year) %>% 
  dplyr::summarize(Total.Money.Spent = sum(Total.Money, na.rm=T)) %>% 
  full_join(template, by=c("State","Award.Fiscal.Year")) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  arrange(Award.Fiscal.Year) %>% 
  arrange(State)

write.csv(summarized_file, "data/clean_data/summarized_319.csv", row.names = F)
write.csv(year_summary,"data/clean_data/summarized_319_by-year.csv", row.names = F)

rm(list = ls())
