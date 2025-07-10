
library(lubridate)
library(dplyr)

variable_data = data.frame(date = seq(ymd("2018-01-01"),ymd("2018-12-31"),by="days"),
                           temperature = round(rnorm(365, 20, 10), 1),
                           PM = round(rnorm(365, 40, 10), 1))

case_data = data.frame(date = sample(seq(ymd("2018-01-01"),ymd("2018-12-31"),by="days"), 100, T),
                       status = 1,
                       age = round(rnorm(100, 40, 10)))

case_crossover_data = case_data %>%
  left_join(variable_data, by = "date")

for(i in 1:nrow(case_crossover_data)){
  
  control_days = variable_data %>%
    filter(date %in% c(case_crossover_data$date[i]-14,
                       case_crossover_data$date[i]-7,
                       case_crossover_data$date[i]+7,
                       case_crossover_data$date[i]+14)) %>%
    mutate(status = 0) %>%
    mutate(age = case_crossover_data$age[i]) %>%
    select(date, status, age, temperature, PM)
  
  case_crossover_data = rbind(case_crossover_data, control_days)
  
}  

model = glm(data = case_crossover_data, formula = status ~ temperature + PM, family = binomial(link = "logit"))
summary(model)

