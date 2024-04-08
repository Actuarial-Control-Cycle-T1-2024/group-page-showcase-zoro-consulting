library(tidyverse)
library(readxl)
library(RColorBrewer)

superlife_inforce <- read.csv("2024-srcsc-superlife-inforce-dataset.csv", skip = 3)

#Check duplicate policy number
length(superlife_inforce$Policy.number) - length(unique(superlife_inforce$Policy.number))

unique(superlife_inforce$Cause.of.Death)

#Check Policy.Type
sum(is.na(superlife_inforce$Policy.type))
unique(superlife_inforce$Policy.type)

#check Issue Year
sum(is.na(superlife_inforce$Issue.year))
unique(superlife_inforce$Issue.year)

#Check issue age
sum(is.na(superlife_inforce$Issue.age))
sort(unique(superlife_inforce$Issue.age))
  ggplot(superlife_inforce) +
    aes(x = Issue.age) +
    geom_histogram(bins = length(unique(superlife_inforce$Issue.age)))

cause_of_death <- read_xls("CorrespondenceTable_Mortality_PYLL_ICDCodes_OECDHealthStatistics2023.xls", 
                             skip = 4)

colnames(cause_of_death)[1] <- "Cause_of_Death_Description"
colnames(cause_of_death)[2] <- "ICD_10_code"

cause_of_death <- cause_of_death %>%
  select(c(Cause_of_Death_Description,ICD_10_code))

superlife_inforce <- left_join(superlife_inforce, cause_of_death, by = join_by(Cause.of.Death == ICD_10_code))



superlife_inforce <- superlife_inforce %>%
  mutate( leave.indicator = case_when(Death.indicator == 1 | Lapse.Indicator == 1 ~ 1),
          Current_age = (2023-Issue.year+Issue.age)*is.na(leave.indicator),
          leave_year = case_when(is.na(Year.of.Death) == F | is.na(Year.of.Lapse) == F ~ pmax(Year.of.Death, Year.of.Lapse, na.rm = T)),
          years_in_force_leave = leave_year - Issue.year,
          years_in_force_current = 2023 - Issue.year,
          leave_age = Issue.age + years_in_force_leave)

ggplot(data = superlife_inforce) +
  aes(x = Face.amount) +
  geom_histogram()

sort(unique(superlife_inforce$Face.amount)) # amounts for face amount 50000, 100000, 250000, 500000, 1000000, 2000000

superlife_inforce$Current_age[superlife_inforce$Current_age == 0] <- NA   

#F01-F99 Mental, Behavioral and Neurodevelopmental disorders not in cause of death 
superlife_inforce$Cause.of.Death[is.na(superlife_inforce$Cause.of.Death)] <- 0
superlife_inforce$Cause_of_Death_Description[superlife_inforce$Cause.of.Death == "F01-F99"] <- "Mental, Behavioral and Neurodevelopmental disorders"


superlife_inforce_T20 <- superlife_inforce %>% 
  filter(Policy.type == "T20")

sort(unique(superlife_inforce_T20$Face.amount)) #50000  100000  250000  500000 1000000 2000000


superlife_inforce_SPWL <- superlife_inforce %>%
  filter(Policy.type == "SPWL")

sort(unique(superlife_inforce_SPWL$Face.amount)) #100000  250000  500000 1000000 2000000


#analysis on policy holders who die
unique(superlife_inforce$Death.indicator)
superlife_inforce_death <- superlife_inforce[is.na(superlife_inforce$Death.indicator) == F,]

# EDA ---------------------------------------------------------------------


#EDA superlife - death
ggplot(data = superlife_inforce_death) +
  aes(x = leave_age, fill = Sex, colour = I("black")) +
  geom_histogram(position = "identity", alpha = 0.3, bins = length(unique(superlife_inforce_death$leave_age))) 

ggplot(data = superlife_inforce_death) +
  aes(x = leave_age, fill = Sex, colour = I("black"), y = after_stat(density)) +
  geom_histogram(position = "identity", alpha = 0.3, bins = length(unique(superlife_inforce_death$leave_age))) 

ggplot(data = superlife_inforce_death) +
  aes(x = years_in_force, fill = Sex, colour = I("black")) +
  geom_histogram(position = "identity", alpha = 0.3, bins = length(unique(superlife_inforce_death$years_in_force)))

ggplot(data = superlife_inforce_death) +
  aes(x = years_in_force, fill = Sex, colour = I("black"), y = after_stat(density)) +
  geom_histogram(position = "identity", alpha = 0.3, bins = length(unique(superlife_inforce_death$years_in_force)))

ggplot(data = superlife_inforce_death) +
  aes(x = years_in_force, fill = Cause.of.Death, colour = I("black")) +
  geom_histogram(alpha = 1, bins = length(unique(superlife_inforce_death$years_in_force)))

ggplot(data = superlife_inforce_death) +
  aes(x = leave_age, fill = Cause.of.Death, colour = I("black")) +
  geom_histogram(alpha = 1)

ggplot(data = superlife_inforce_death) +
  aes(x = leave_age, fill = Smoker.Status, colour = I("black"), y = after_stat(density)) +
  geom_histogram(position = "identity", alpha = 0.3, bins = length(unique(superlife_inforce_death$leave_age)))

ggplot(data = superlife_inforce_death) +
  aes(x = years_in_force, fill = Smoker.Status, colour = I("black"), y = after_stat(density)) +
  geom_histogram(position = "identity", alpha = 0.3, bins = length(unique(superlife_inforce_death$years_in_force)))

ggplot(data = superlife_inforce_death) +
  aes(x = leave_age, fill = Underwriting.Class, colour = I("black"), y = after_stat(density)) +
  geom_histogram(position = "identity", alpha = 0.3, bins = length(unique(superlife_inforce_death$leave_age)))

ggplot(data = superlife_inforce_death) +
  aes(x = years_in_force, fill = Underwriting.Class, colour = I("black"), y = after_stat(density)) +
  geom_histogram(position = "identity", alpha = 0.3, bins = length(unique(superlife_inforce_death$years_in_force)))

ggplot(data = superlife_inforce_death) +
  aes(x = leave_age, fill = Urban.vs.Rural, colour = I("black"), y = after_stat(density)) +
  geom_histogram(position = "identity", alpha = 0.3, bins = length(unique(superlife_inforce_death$leave_age)))

ggplot(data = superlife_inforce_death) +
  aes(x = years_in_force, fill = Urban.vs.Rural, colour = I("black"), y = after_stat(density)) +
  geom_histogram(position = "identity", alpha = 0.3, bins = length(unique(superlife_inforce_death$years_in_force)))


# EDA T20 -----------------------------------------------------------------

ggplot(data = superlife_inforce_T20) +
  aes(x = years_in_force, colour = I("black")) +
  geom_histogram(position = "identity", bins = length(unique(superlife_inforce_T20$years_in_force)))

length(unique(superlife_inforce_T20$years_in_force))

sum(is.na(superlife_inforce_T20$years_in_force))
sum(is.na(superlife_inforce$years_in_force))

superlife_inforce_T20

sum(superlife_inforce$.indicator, na.rm = T)/nrow(superlife_inforce)



# Smoker ------------------------------------------------------------------

superlife_inforce_fe <- superlife_inforce %>%
  filter(Sex == "F")

superlife_inforce_ma <- superlife_inforce %>%
  filter(Sex == "M")

sum(superlife_inforce_fe$Smoker.Status == "S")/nrow(superlife_inforce_fe)
sum(superlife_inforce_ma$Smoker.Status == "S")/nrow(superlife_inforce_ma)
