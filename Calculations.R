library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(tibble)
library(writexl)
library(openxlsx)

data <- read.csv("2024-srcsc-superlife-inforce-dataset.csv", skip = 3)

data <- data %>%
  mutate(age_at_death = Issue.age + Year.of.Death - Issue.year)

data$Lapse.Indicator <- gsub("Y", 1, data$Lapse.Indicator)

T20 <- subset(data, Policy.type == 'T20')
SPWL <- subset(data, Policy.type == 'SPWL')
female <- subset(data, Sex == 'F')
male <- subset(data, Sex == 'M')
smoker <- subset(data, Smoker.Status == 'S')
non_smoker <- subset(data, Smoker.Status == 'NS')
T20_M_S <- subset(T20, Sex == 'M' & Smoker.Status == 'S')
T20_M_NS <- subset(T20, Sex == 'M' & Smoker.Status == 'NS')
T20_F_S <- subset(T20, Sex == 'F' & Smoker.Status == 'S')
T20_F_NS <- subset(T20, Sex == 'F' & Smoker.Status == 'NS')
SPWL_M_S <- subset(SPWL, Sex == 'M' & Smoker.Status == 'S')
SPWL_M_NS <- subset(SPWL, Sex == 'M' & Smoker.Status == 'NS')
SPWL_F_S <- subset(SPWL, Sex == 'F' & Smoker.Status == 'S')
SPWL_F_NS <- subset(SPWL, Sex == 'F' & Smoker.Status == 'NS')

calc_mort_savings <- function(data) {
  min_age <- min(data$Issue.age)
  max_age <- max(data$Issue.age + (2023 - data$Issue.year))
  ages <- seq(min_age, max_age)
  mortality_rates <- data.frame(age = ages, mort_rate = numeric(length(ages))
                                , pop = numeric(length(ages)), deaths_next_year = numeric(length(ages)))
  
  for (age in ages) {
    pop_at_age <- subset(data, age >= Issue.age & (Issue.year + age - Issue.age) <= 2023 & (age < age_at_death | is.na(age_at_death)))
    deaths_next_year <- sum(!is.na(pop_at_age$age_at_death) & pop_at_age$age_at_death == (age + 1))
    mortality_rates$pop[age - min(ages) + 1] <- nrow(pop_at_age)
    mortality_rates$deaths_next_year[age - min(ages) + 1] <- deaths_next_year
  }
  
  intervention <- 0.06
  
  mortality_rates <- mortality_rates %>%
    mutate(mort_rate = deaths_next_year / pop,
           new_mort_rate = mort_rate * (1 - intervention))
  
  mortality_savings <- mortality_rates %>%
    mutate(new_deaths_next_year = new_mort_rate * pop,
           mortality_savings = deaths_next_year - new_deaths_next_year)
  
  sum(mortality_savings$mortality_savings)
  
  return(mortality_savings)
}

full_mort <- calc_mort_savings(data)

T20_mort <- calc_mort_savings(T20)
SPWL_mort <- calc_mort_savings(SPWL)
male_mort <- calc_mort_savings(male)
female_mort <- calc_mort_savings(female)
smoker_mort <- calc_mort_savings(smoker)
non_smoker_mort <- calc_mort_savings(non_smoker)

T20_M_S_mort <- calc_mort_savings(T20_M_S)
T20_M_NS_mort <- calc_mort_savings(T20_M_NS)
T20_F_S_mort <- calc_mort_savings(T20_F_S)
T20_F_NS_mort <- calc_mort_savings(T20_F_NS)

SPWL_M_S_mort <- calc_mort_savings(SPWL_M_S)
SPWL_M_NS_mort <- calc_mort_savings(SPWL_M_NS)
SPWL_F_S_mort <- calc_mort_savings(SPWL_F_S)
SPWL_F_NS_mort <- calc_mort_savings(SPWL_F_NS)

subsets <- list(
  full_mort = full_mort,
  T20_mort = T20_mort,
  SPWL_mort = SPWL_mort,
  male_mort = male_mort,
  female_mort = female_mort,
  smoker_mort = smoker_mort,
  non_smoker_mort = non_smoker_mort,
  T20_M_S_mort = T20_M_S_mort,
  T20_M_NS_mort = T20_M_NS_mort,
  T20_F_S_mort = T20_F_S_mort,
  T20_F_NS_mort = T20_F_NS_mort,
  SPWL_M_S_mort = SPWL_M_S_mort,
  SPWL_M_NS_mort = SPWL_M_NS_mort,
  SPWL_F_S_mort = SPWL_F_S_mort,
  SPWL_F_NS_mort = SPWL_F_NS_mort
)

combined_mortality_rates <- data.frame(age = numeric(0), stringsAsFactors = FALSE)

for (class_name in names(subsets)) {
  mortality_rates <- subsets[[class_name]][, c("age", "mort_rate", "new_mort_rate")]
  col_name <- paste(class_name, "mortality_rate", sep = "_")
  mortality_rates <- setNames(mortality_rates, c("age", col_name))
  combined_mortality_rates <- merge(combined_mortality_rates, mortality_rates, by = "age", all = TRUE)
}

wb <- createWorkbook()
addWorksheet(wb, "Combined_Mortality_Rates")
writeData(wb, "Combined_Mortality_Rates", colnames(combined_mortality_rates), startRow = 1, startCol = 1)
writeData(wb, "Combined_Mortality_Rates", combined_mortality_rates, startRow = 2, startCol = 1)
saveWorkbook(wb, "combined_mortality_rates6.xlsx", overwrite = TRUE)


wb <- createWorkbook()
for (name in names(subsets)) {
  addWorksheet(wb, name)
  writeData(wb, name, mortality_data[[name]])
}
saveWorkbook(wb, "mortality_tables1.xlsx", overwrite = TRUE)

T20_M_NS_savings <- T20_M_NS_mort %>% select(c(age, mortality_savings))
T20_M_S_savings <- T20_M_S_mort %>% select(c(age, mortality_savings))
T20_F_NS_savings <- T20_F_NS_mort %>% select(c(age, mortality_savings))
T20_F_S_savings <- T20_F_S_mort %>% select(c(age, mortality_savings))
total_savings <- Reduce(function(x, y) merge(x, y, by = "age", all = TRUE), list(T20_M_NS_savings,
                                                                                 T20_M_S_savings,
                                                                                 T20_F_NS_savings,
                                                                                 T20_F_S_savings))
total_savings <- head(total_savings, nrow(total_savings) - 4)
colnames(total_savings) <- c('age', 'T20 M NS', 'T20 M S', 'T20 F NS', 'T20 F S')
total_savings <- bind_rows(total_savings, as_tibble(t(colSums(total_savings))))


create_mortality_plot <- function(data, title, filename) {
  data <- head(data, nrow(data) - 7)
  p <- ggplot(data, aes(x = age)) +
      geom_line(aes(y = mort_rate, colour = "Current Mortality Rate")) +
      geom_line(aes(y = new_mort_rate, colour = "Mortality Rate With Intervention")) +
      labs(title = title, x = "Age", y = "Mortality Rate") +
      scale_colour_manual(values = c("Current Mortality Rate" = "blue", "Mortality Rate With Intervention" = "red"))
  
  ggsave(filename, plot = p, width = 8, height = 6, dpi = 300)
  return(filename)
}

T20_M_NS_rates <- create_mortality_plot(T20_M_NS_mort, "T20 M NS Mortality Rates", "T20_M_NS_plot.png")
T20_M_S_rates <- create_mortality_plot(T20_M_S_mort, "T20 M S Mortality Rates", "T20_M_S_plot.png")
T20_F_NS_rates <- create_mortality_plot(T20_F_NS_mort, "T20 F NS Mortality Rates", "T20_F_NS_plot.png")
T20_F_S_rates <- create_mortality_plot(T20_F_S_mort, "T20 F S Mortality Rates", "T20_F_S_plot.png")
T20_rates <- create_mortality_plot(T20_mort, "T20 Mortality Rates", "T20_plot.png")

create_death_plot <- function(data, title, filename) {
  data <- head(data, nrow(data) - 7)
  p <- ggplot(data, aes(x = age)) +
      geom_line(aes(y = deaths_next_year, colour = "Current Deaths")) +
      geom_line(aes(y = new_deaths_next_year, colour = "Deaths With Intervention")) +
      labs(title = title, x = "Age", y = "Deaths") +
      scale_colour_manual(values = c("Current Deaths" = "blue", "Deaths With Intervention" = "red"))
  ggsave(filename, plot = p, width = 8, height = 6, dpi = 300)
  return(filename)
}

T20_M_NS_deaths <- create_death_plot(T20_M_NS_mort, "T20 M NS Deaths", "T20_M_NS_plot2.png")
T20_M_S_deaths <- create_death_plot(T20_M_S_mort, "T20 M S Deaths", "T20_M_S_plot2.png")
T20_F_NS_deaths <- create_death_plot(T20_F_NS_mort, "T20 F NS Deaths", "T20_F_NS_plot2.png")
T20_F_S_deaths <- create_death_plot(T20_F_S_mort, "T20 F S Deaths", "T20_F_S_plot2.png")
T20_deaths <- create_death_plot(T20_mort, "T20 Deaths", "T20_plot2.png")

##### LAPSE RATE

calc_lapse_rate <- function(data) {
  data <- data %>% mutate(Duration = Year.of.Lapse - Issue.year + 1)
  duration_summary <- as.data.frame(table(data$Duration, useNA = "always"))
  colnames(duration_summary) <- c("Duration", "Count")
  duration_summary$Lapse_Rate <- (duration_summary$Count / sum(duration_summary$Count))
  
  group_intervals <- c(0.5, 1.5, 5.5, 10.5, 15.5, 19.5)
  data$Duration_group <- cut(data$Duration, breaks = group_intervals, labels = FALSE, include.lowest = TRUE)
  duration_group <- data.frame(table(data$Duration_group))
  colnames(duration_group) <- c("Duration", "Count")
  duration_group$Duration <- c("1", "2-5", "6-10", "11-15", "16+")
  duration_group$Raw_Lapse_Rate <- (duration_group$Count / sum(duration_summary$Count))
  duration_group$Years <- c(1, 4, 5, 5, 4)
  duration_group <- duration_group %>% mutate(Weighted_Lapse_Rate = Raw_Lapse_Rate / Years)
  return(duration_group)
}

T20_lapse <- calc_lapse_rate(T20)
T20_M_S_lapse <- calc_lapse_rate(T20_M_S)
T20_M_NS_lapse <- calc_lapse_rate(T20_M_NS)
T20_F_S_lapse <- calc_lapse_rate(T20_F_S)
T20_F_NS_lapse <- calc_lapse_rate(T20_F_NS)

lapse_subsets <- list(
  T20_lapse = T20_lapse,
  T20_M_S_lapse = T20_M_S_lapse,
  T20_M_NS_lapse = T20_M_NS_lapse,
  T20_F_S_lapse = T20_F_S_lapse,
  T20_F_NS_lapse = T20_F_NS_lapse
)

wb <- createWorkbook()
for (name in names(lapse_subsets)) {
  addWorksheet(wb, name)
  writeData(wb, name, lapse_subsets[[name]])
}
saveWorkbook(wb, "lapse_rates.xlsx", overwrite = TRUE)



#### CURRENT MAKE UP OF POLICYHOLDERS

create_subsets <- function(face_amount) {
  subset_data <- subset(SPWL, Face.amount == face_amount)
  
  M_NS <- subset(subset_data, Sex == "M" & Smoker.Status == "NS")
  M_S <- subset(subset_data, Sex == "M" & Smoker.Status == "S")
  F_NS <- subset(subset_data, Sex == "F" & Smoker.Status == "NS")
  F_S <- subset(subset_data, Sex == "F" & Smoker.Status == "S")
  
  class_pops <- list(
    M_NS_pop = nrow(M_NS),
    M_S_pop = nrow(M_S),
    F_NS_pop = nrow(F_NS),
    F_S_pop = nrow(F_S)
  )
  
  age_pops <- list(
    M_NS_age = table(M_NS$Issue.age),
    M_S_age = table(M_S$Issue.age),
    F_NS_age = table(F_NS$Issue.age),
    F_S_age = table(F_S$Issue.age)
  )
  
  return(list(class_pops = class_pops, age_pops = age_pops))
}

SPWL_100000_pops <- create_subsets(100000)
SPWL_50000_pops <- create_subsets(50000)
SPWL_2000000_pops <- create_subsets(2000000)
SPWL_250000_pops <- create_subsets(250000)
SPWL_1000000_pops <- create_subsets(1000000)
SPWL_500000_pops <- create_subsets(500000)

write_populations_to_excel <- function(pop_list, file_name) {
  class_pops_df <- data.frame(
    M_NS_pop = pop_list$class_pops$M_NS_pop,
    M_S_pop = pop_list$class_pops$M_S_pop,
    F_NS_pop = pop_list$class_pops$F_NS_pop,
    F_S_pop = pop_list$class_pops$F_S_pop
  )
  
  write_xlsx(list(pop_list = class_pops_df), paste0(file_name, ".xlsx"))
}


write_populations_to_excel(SPWL_100000_pops, "SPWL_100000_pops")
write_populations_to_excel(SPWL_50000_pops, "SPWL_50000_pops")
write_populations_to_excel(SPWL_2000000_pops, "SPWL_2000000_pops")
write_populations_to_excel(SPWL_250000_pops, "SPWL_250000_pops")
write_populations_to_excel(SPWL_1000000_pops, "SPWL_1000000_pops")
write_populations_to_excel(SPWL_500000_pops, "SPWL_500000_pops")

write_age_populations_to_excel <- function(age_pop_list, file_name) {
  max_rows <- max(sapply(age_pop_list, length))
  
  filled_age_pop_list <- lapply(age_pop_list, function(x) {
    length_diff <- max_rows - length(x)
    c(x, rep(NA, length_diff))
  })
    age_pops_df <- data.frame(
    M_NS_age = unname(filled_age_pop_list$M_NS_age),
    M_S_age = unname(filled_age_pop_list$M_S_age),
    F_NS_age = unname(filled_age_pop_list$F_NS_age),
    F_S_age = unname(filled_age_pop_list$F_S_age)
  )

  write_xlsx(list(age_pop_list = age_pops_df), paste0(file_name, ".xlsx"))
}

write_age_populations_to_excel(SPWL_100000_pops$age_pops, "SPWL_100000_age_pops")
write_age_populations_to_excel(SPWL_50000_pops$age_pops, "SPWL_50000_age_pops")
write_age_populations_to_excel(SPWL_2000000_pops$age_pops, "SPWL_2000000_age_pops")
write_age_populations_to_excel(SPWL_250000_pops$age_pops, "SPWL_250000_age_pops")
write_age_populations_to_excel(SPWL_1000000_pops$age_pops, "SPWL_1000000_age_pops")
write_age_populations_to_excel(SPWL_500000_pops$age_pops, "SPWL_500000_age_pops")


table(SPWL$Face.amount)


#### CURRENT MAKE UP OF POLICYHOLDERS
in_force <- subset(data, is.na(Death.indicator) == TRUE & is.na(Lapse.Indicator) == TRUE) 
in_force <- in_force %>% 
  mutate(Actual_age = Issue.age + 2024 - Issue.year,
         Duration = 2024 - Issue.year,
         Pop_weight = (20 - Duration) / 20)

T20_IF <- subset(in_force, Policy.type == "T20")
SPWL_IF <- subset(in_force, Policy.type == "SPWL")

T20_M_NS_IF <- subset(T20_IF, Sex == "M" & Smoker.Status == "NS")
T20_M_S_IF <- subset(T20_IF, Sex == "M" & Smoker.Status == "S")
T20_F_NS_IF <- subset(T20_IF, Sex == "F" & Smoker.Status == "NS")
T20_F_S_IF <- subset(T20_IF, Sex == "F" & Smoker.Status == "S")

SPWL_M_NS_IF <- subset(SPWL_IF, Sex == "M" & Smoker.Status == "NS")
SPWL_M_S_IF <- subset(SPWL_IF, Sex == "M" & Smoker.Status == "S")
SPWL_F_NS_IF <- subset(SPWL_IF, Sex == "F" & Smoker.Status == "NS")
SPWL_F_S_IF <- subset(SPWL_IF, Sex == "F" & Smoker.Status == "S")

calc_pop <- function(data) {
  data <- data %>%
    group_by(Actual_age) %>%
    summarise(pop = sum(Pop_weight))
  return(data)
}

populations <- list(
  T20_M_NS_
)
calc_pop(T20_M_NS_IF)

populations <- list(
  T20_M_NS = nrow(T20_M_NS_IF),
  T20_M_S = nrow(T20_M_S_IF),
  T20_F_NS = nrow(T20_F_NS_IF),
  T20_F_S = nrow(T20_F_S_IF),
  SPWL_M_NS = nrow(SPWL_M_NS_IF),
  SPWL_M_S = nrow(SPWL_M_S_IF),
  SPWL_F_NS = nrow(SPWL_F_NS_IF),
  SPWL_F_S = nrow(SPWL_F_S_IF)
)

populations_df <- data.frame(Class = names(populations), Population = populations)


write.xlsx(populations_df, getwd())



inforce <- subset(data, is.na(Death.indicator) == TRUE & is.na(Lapse.Indicator) == TRUE) 
years <- unique(inforce$Issue.year)
num_policies <- vector(mode = "numeric", length = length(years))
for (i in 1:length(years)) {
  num_policies[i] <- nrow(inforce[inforce$Issue.year == years[i],])
}

print(num_policies)

pct_increase <- vector(mode = "numeric", length = length(years) - 1)
for (i in 2:length(years)) {
  pct_increase[i - 1] <- (num_policies[i] - num_policies[i - 1]) / num_policies[i - 1] * 100
}

print(pct_increase)
print(years)

print(mean(pct_increase))


inforce <- subset(data, is.na(Death.indicator) == TRUE & is.na(Lapse.Indicator) == TRUE & Policy.type == "SPWL") 
ages <- unique(inforce$Issue.age)
num_policies_df <- data.frame(Year = numeric(), Age = numeric(), Policies = numeric())


for (i in 1:length(years)) {
  for (j in 1:length(ages)) {
    num_policies_df <- rbind(num_policies_df, data.frame(Year = years[i], Age = ages[j], 
                                                         Policies = nrow(inforce[inforce$Issue.year == years[i] & 
                                                                                   inforce$Issue.age == ages[j],])))
  }
}


print(num_policies_df)


year_age_combinations <- expand.grid(Year = unique(inforce$Issue.year), Age = unique(inforce$Issue.age))


num_policies <- numeric()


for (i in 1:nrow(year_age_combinations)) {
  year <- year_age_combinations$Year[i]
  age <- year_age_combinations$Age[i]
  
  num_policies[i] <- nrow(subset(inforce, Issue.year == year & Issue.age == age))
}

year_age_combinations$Policies <- num_policies

print(year_age_combinations)

year_age_combinations <- year_age_combinations[order(year_age_combinations$Age, year_age_combinations$Year),]

percentage_change_list <- list()

for (age in unique(year_age_combinations$Age)) {

  subset_df <- subset(year_age_combinations, Age == age)
  
  percentage_change <- c(NA, diff(subset_df$Policies) / subset_df$Policies[-nrow(subset_df)] * 100)

  percentage_change_list[[as.character(age)]] <- percentage_change
}

percentage_change_df <- as.data.frame(percentage_change_list)

percentage_change_df$Age <- as.numeric(rownames(percentage_change_df))

percentage_change_df <- percentage_change_df[, -1]

print(percentage_change_df)


average_percentage_df <- data.frame(Age = numeric(), Average_Percentage_Change = numeric())

for (age in names(percentage_change_list)) {
  mean_value <- mean(percentage_change_list[[age]][!is.infinite(percentage_change_list[[age]])], na.rm = TRUE)
  
  average_percentage_df <- rbind(average_percentage_df, data.frame(Age = age, Average_Percentage_Change = mean_value))
}

print(average_percentage_df)

write.xlsx(average_percentage_df, "average_percentage_change.xlsx", Row.names = FALSE)




