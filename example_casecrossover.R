
library(lubridate)
library(dplyr)

# variable dataset will have temperature, pollution etc... data for each day of the year
variable_data = data.frame(date = seq(ymd("2018-01-01"),ymd("2018-12-31"),by="days"),
                           temperature = round(rnorm(365, 20, 10), 1),
                           PM = round(rnorm(365, 40, 10), 1))

# case dataset will have case date and other individual variables (eg age)
# the "status" column is used to denote between cases (1) and control (0) for the regression
case_data = data.frame(date = sample(seq(ymd("2018-01-01"),ymd("2018-12-31"),by="days"), 100, T),
                       status = 1,
                       age = round(rnorm(100, 40, 10)))

# create the case crossover dataset by taking the case dataset and adding variable
#   info for each case day
case_crossover_data = case_data %>%
  left_join(variable_data, by = "date")

# expand the case crosssover dataset by adding controls for each case
for(i in 1:nrow(case_crossover_data)){
  
  # for each case, we select up to 4 control days before/after the case day
  # we can adjust the sampling strategy to pick controls
  control_days = variable_data %>%
    filter(date %in% c(case_crossover_data$date[i]-14,
                       case_crossover_data$date[i]-7,
                       case_crossover_data$date[i]+7,
                       case_crossover_data$date[i]+14)) %>%
    mutate(status = 0) %>%
    mutate(age = case_crossover_data$age[i]) %>%
    select(date, status, age, temperature, PM)
  
  # add the controls to the case crossover dataset
  case_crossover_data = rbind(case_crossover_data, control_days)
  
}  

# here, because everything is random, nothing should be significant (except the intercept)
model = glm(data = case_crossover_data, formula = status ~ age + temperature + PM, family = binomial(link = "logit"))
summary(model)

