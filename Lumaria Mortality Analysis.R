library(tidyverse)
library(readxl)
library(RColorBrewer)
library(survival)
library("MortalityTables")
library(MASS)
library(glmnet)
library("survminer")
library(markovchain)

superlife_inforce <- read.csv("2024-srcsc-superlife-inforce-dataset.csv", skip = 3)

unique(superlife_inforce$Lapse.Indicator)

superlife_inforce[superlife_inforce$Lapse.Indicator == "Y",]


superlife_inforce[superlife_inforce$Lapse.Indicator == 1,]

sum(is.na(superlife_inforce$Cause.of.Death))

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

unique(superlife_inforce$Lapse.Indicator)

cause_of_death <- read_xls("CorrespondenceTable_Mortality_PYLL_ICDCodes_OECDHealthStatistics2023.xls", 
                           skip = 4)

colnames(cause_of_death)[1] <- "Cause_of_Death_Description"
colnames(cause_of_death)[2] <- "ICD_10_code"

cause_of_death <- cause_of_death %>%
  dplyr::select(c(Cause_of_Death_Description,ICD_10_code))

superlife_inforce <- left_join(superlife_inforce, cause_of_death, by = join_by(Cause.of.Death == ICD_10_code))



superlife_inforce <- superlife_inforce %>%
  mutate( leave.indicator = case_when(Death.indicator == 1 | Lapse.Indicator == 1 ~ 1),
          Current_age = (2024-Issue.year+Issue.age)*is.na(leave.indicator),
          leave_year = case_when(is.na(Year.of.Death) == F | is.na(Year.of.Lapse) == F ~ pmax(Year.of.Death, Year.of.Lapse, na.rm = T)),
          years_in_force_leave = leave_year - Issue.year,
          years_in_force_current = case_when(is.na(years_in_force_leave) == T ~ (2024 - Issue.year), .default = NA),
          leave_age = Issue.age + years_in_force_leave,
          cohort = Issue.year - Issue.age)

superlife_inforce$Lapse.Indicator <- as.integer(superlife_inforce$Lapse.Indicator)

superlife_inforce_T20 <- superlife_inforce %>% 
  filter(Policy.type == "T20")

superlife_inforce_SPWL <- superlife_inforce %>%
  filter(Policy.type == "SPWL")

sum(is.na(superlife_inforce$years_in_force_current))

# Cox Regression ----------------------------------------------------------
sum(is.na(superlife_inforce$Current_age))

superlife_inforce_cox <- superlife_inforce %>%
  dplyr::select(Policy.type,Issue.age, Sex, Face.amount, Smoker.Status, Underwriting.Class, Urban.vs.Rural, Region, Distribution.Channel, years_in_force_leave, years_in_force_current, Death.indicator)

superlife_inforce_cox$Death.indicator[is.na(superlife_inforce_cox$Death.indicator)] <- 0

superlife_inforce_cox <- superlife_inforce_cox %>%
  mutate(years_in_force = pmax(years_in_force_leave, years_in_force_current, na.rm = T), .keep = "unused")

superlife_inforce_cox$Region <- as.factor(superlife_inforce_cox$Region)

superlife_inforce$Policy.type <- as.factor(superlife_inforce_cox$Policy.type)



cox_full <- coxph(Surv(years_in_force, Death.indicator) ~ .,
                  data = superlife_inforce_cox)

summary(cox_full)

# step.cox.backwards <- stepAIC(cox_full, direction = "backward")
# 
# cox_age <- coxph(Surv(years_in_force, Death.indicator) ~ Issue.age,
#                               data = superlife_inforce_cox)
# 
# step.cox.forward <- stepAIC(cox_full, direction = "both")
# 
# step.cox.backwards$coefficients

cox.model <- coxph(Surv(years_in_force, Death.indicator) ~ Policy.type + Issue.age + Sex + Face.amount + Smoker.Status + Underwriting.Class + Distribution.Channel,
                   data = superlife_inforce_cox)

cox.simp <- coxph(Surv(years_in_force, Death.indicator) ~ Policy.type + Issue.age + Sex  + Smoker.Status,
                  data = superlife_inforce_cox)

summary(cox.model)
summary(cox.simp)

ggsurvplot(survfit(cox.simp, data = superlife_inforce_cox), palette = "#2E9FDF",
           ggtheme = theme_minimal())

sum(superlife_inforce_cox$Death.indicator)/length(superlife_inforce_cox$Death.indicator)





pred <- predict(cox.simp, superlife_inforce_cox, type = "expected")

superlife_inforce_cox$Prediction_hazard <- pred

superlife_inforce_cox$survival_prob <- exp(-pred)

mort_table <- superlife_inforce_cox %>%
  group_by(Issue.age) %>%
  summarise(arith_mean = mean(survival_prob),
            geo_mean = exp(mean(log(survival_prob))))

mort_table <- mort_table %>%
  mutate(mort_arith = 1 - arith_mean^(1/22),
         mort_geo = 1- geo_mean^(1/22))

mort_table_inforce <- superlife_inforce_cox %>%
  group_by(years_in_force) %>%
  summarise(arith_mean = mean(survival_prob),
            geo_mean = exp(mean(log(survival_prob))))

mort_table_inforce <- mort_table_inforce %>%
  mutate(mort_arith = 1 - arith_mean^(1/22),
         mort_geo = 1- geo_mean^(1/22))

mort_table_inforce_plot <- pivot_longer(mort_table_inforce, cols = c(starts_with("mort")))



lumaria_mort_table <- read_excel("srcsc-2024-lumaria-mortality-table.xlsx", skip = 13)

colnames(lumaria_mort_table)[2] <- "Mortality_Rate"
ggplot(data = lumaria_mort_table) +
  aes(x = Age, y = Mortality_Rate ) +
  geom_line()

lumaria_mort_table_filt <- lumaria_mort_table %>%
  filter(Age %in% mort_table$Issue.age)

mort_table$mort_lumaria <- lumaria_mort_table_filt$Mortality_Rate

mort_table_plot <- pivot_longer(mort_table, cols = c(starts_with("mort")))

ggplot(mort_table_plot) +
  aes(x = Issue.age, y = log(value), colour = name) +
  geom_line()

ggplot(mort_table_inforce_plot) +
  aes(x = years_in_force, y = log(value), colour = name) +
  geom_line()

SexM_coef <- cox.simp$coefficients["SexM"]
Smoke_coef <- cox.simp$coefficients["Smoker.StatusS"]
Age_coef <- cox.simp$coefficients["Issue.age"]

# Smoking -----------------------------------------------------------------
sum(superlife_inforce$Smoker.Status == "S")/length(superlife_inforce$Smoker.Status)

superlife_inforce_smoke <- superlife_inforce %>%
  dplyr::filter(Smoker.Status == "S")

prop_smoker <- 0.18

prop_fe_smoke <- prop_smoker * sum(superlife_inforce_smoke$Sex == "F")/length(superlife_inforce_smoke$Sex)
prop_fe_smoke_f <- 2*prop_fe_smoke
prop_ma_smoke <- prop_smoker * (1 - sum(superlife_inforce_smoke$Sex == "F")/length(superlife_inforce_smoke$Sex))
prop_ma_smoke_m <- prop_ma_smoke*2

prop_fe_smoke_superlife <- sum(superlife_inforce$Smoker.Status == "S")/length(superlife_inforce$Smoker.Status)
# backing out mortality ---------------------------------------------------
# lumaria_mort_table_sex <- lumaria_mort_table %>% #Assume 50% F/M
#   mutate(Female = Mortality_Rate/(0.5*1+0.5*exp(SexM_coef)),
#           Male = Female*exp(SexM_coef))
# 
# lumaria_mort_table_sex_smoke <- lumaria_mort_table_sex %>%
#  mutate(Female_NS = Female/((1-prop_fe_smoke_f)*1+prop_fe_smoke_f*exp(Smoke_coef)),
#          Female_S = Female_NS*exp(Smoke_coef),
#          Male_NS = Male/((1-prop_ma_smoke_m)*1+prop_ma_smoke_m*exp(Smoke_coef)),
#          Male_S = Male_NS*exp(Smoke_coef))
# 
# lumaria_mort_split <- pivot_longer(lumaria_mort_table_sex_smoke, cols = -1)
# 
# ggplot(lumaria_mort_split) +
#  aes(x = Age, y = log(value), col = name ) +
#  geom_line() +
#  scale_colour_manual(values = brewer.pal(7, "Accent"))



# Proportion of groupings (F_NS, F_S, M_NS, M_S) --------------------------
#Prelim info 18% smokers 

#distribution other countries 
Smoke_coef <- 1 #https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-015-0281-z#:~:text=In%20this%20large%2Dscale%2C%20population,smokers%2C%20over%20the%20ages%20examined.
#https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/484996
exp(Smoke_coef)
exp(SexM_coef)
# Prop_Fe_S <- prop_fe_smoke
# Prop_Fe_NS <- 0.5 - Prop_Fe_S
# Prop_Ma_S <- prop_ma_smoke
# Prop_Ma_NS <- 0.5 - Prop_Ma_S

# sum(Prop_Fe_S, Prop_Fe_NS, Prop_Ma_S, Prop_Ma_NS)

smoking <-
  c(
    rep(0, 18),
    rep(18 / 100 * 0.7, 7),
    rep(18 / 100, 10),
    rep(18 / 100 * 1.1, 10),
    rep(18 / 100 * 1.2, 10),
    rep(18 / 100 * 1.4, 10),
    rep(18 / 100 * 0.8, 10),
    rep(18 / 100 * 0.3, 45)
  )  #https://www.abs.gov.au/articles/insights-australian-smokers-2021-22

Uzbekistan <- data.frame(c(5.19, 5.58), c(11.5, 11.5) , c(1.07, 0.758613)) #https://genderdata.worldbank.org/indicators/sp-pop/?age=65%2B&geos=UZB&view=bar

Fe_zero_fourteen <- Uzbekistan[1,1]/sum(Uzbekistan[,1])
Fe_fifteen_sixtyfour <- Uzbekistan[1,2]/sum(Uzbekistan[,2])
Fe_sixtyfive_plus <- Uzbekistan[1,3]/sum(Uzbekistan[,3])

gendersplit <- c(rep(Fe_zero_fourteen,14), rep(Fe_fifteen_sixtyfour,50), rep(Fe_sixtyfive_plus,56))

lumaria_mort_table[,3] <- smoking
lumaria_mort_table[,4] <- gendersplit

colnames(lumaria_mort_table)[3:4] <- c("smoking_rate", "female")

lumaria_mort_table_split <- lumaria_mort_table %>%
  mutate(Fe_S = smoking_rate * female/1.5, 
         Fe_NS = female - Fe_S,
         Ma_S = smoking_rate * (1-female)*1.5,
         Ma_NS = (1-female) - Ma_S)

  

lumaria_mort_table_split <- lumaria_mort_table_split %>% 
  mutate(Hazard_rate = -log(1 - Mortality_Rate)) %>%
  mutate(
    Female_NS_Haz = Hazard_rate / (
      Fe_NS * 1 + Fe_S * exp(Smoke_coef) + Ma_NS * exp(SexM_coef) +
        Ma_S * exp(Smoke_coef + SexM_coef)
    ),
    Female_S_Haz = Female_NS_Haz * exp(Smoke_coef),
    Male_NS_Haz = Female_NS_Haz * exp(SexM_coef),
    Male_S_Haz = Female_NS_Haz * exp(SexM_coef + Smoke_coef)
  ) %>%
  mutate(
    Female_NS_Mort = 1 - exp(-Female_NS_Haz),
    Female_S_Mort = 1 - exp(-Female_S_Haz),
    Male_NS_Mort = 1 - exp(-Male_NS_Haz),
    Male_S_Mort = 1 - exp(-Male_S_Haz)
  )

lumaria_mort_table_split_plot <- lumaria_mort_table_split %>% 
  dplyr::select(c("Age", "Mortality_Rate",ends_with("Mort")))

colnames(lumaria_mort_table_split_plot) <- c("Age", "Overall Mortality", "Female Non-Smoker", "Female Smoker", "Male Non-Smoker", "Male Smoker")

lumaria_mort_table_split_plot <- pivot_longer(lumaria_mort_table_split_plot, 2:5)

ggplot(lumaria_mort_table_split_plot) +
  aes(x = Age, y = value, colour = name) +
  geom_line() +
  labs(title = "Mortality by Classes" , x = "Age", y = "Mortality", colour = "Class" ) +
  scale_colour_manual(values = brewer.pal(n = 4, "Dark2"))
  
display.brewer.all()

lumaria_mort_table_split_1 <- lumaria_mort_table_split %>% 
  dplyr::select(c("Age", "Mortality_Rate",ends_with("Mort")))

write.csv(lumaria_mort_table_split_1, "Lumaria-Mortality-Table_Split.csv")

display.brewer.all()
# SPWL --------------------------------------------------------------------




# cohort ------------------------------------------------------------------
superlife_inforce_cohort <- superlife_inforce %>%
  group_by(cohort) %>%
  summarise(mortality_prob = sum(Death.indicator, na.rm = T)/n()) %>%
  mutate(age = 2024 - cohort)

cohort_plot <- left_join(superlife_inforce_cohort, lumaria_mort_table, by = join_by(age == Age))

col_order <- c("age","mortality_prob","Mortality_Rate")

cohort_plot <- cohort_plot[, col_order]

colnames(cohort_plot) <- c("Age", "Mort_cohort", "Mort_lumaria")

cohort_plot <- pivot_longer(cohort_plot, cols = starts_with("Mort"))

ggplot(cohort_plot) +
  aes(x = Age, y = value, colour = name) +
  geom_line()


# Period ------------------------------------------------------------------

superlife_inforce_period <- superlife_inforce %>%
  mutate(Age = 2023 - cohort) %>%
  group_by(Age) %>%
  summarise(mort_prob = sum(Death.indicator, na.rm = T)/n())

Period_plot <- left_join(superlife_inforce_period, lumaria_mort_table[,1:2], by = join_by(Age))

colnames(Period_plot) <- c("Age", "Mort_Period", "Mort_lumaria")

Period_plot <- pivot_longer(Period_plot, cols = starts_with("Mort"))

ggplot(Period_plot) +
  aes(x = Age, y = value, colour = name) +
  geom_line()
# Country comp ------------------------------------------------------------


ggplot(lumaria_mort_table) +
  aes(x = Age, y = log(Mortality_Rate)) +
  geom_line()

Australia_mort <- read.delim("bltper_1x1.txt", skip = 2, sep = "")
Austria_mort <- read.delim("bltper_1x1_Austria.txt", skip = 2, sep = "")
Belarus_mort <- read.delim("bltper_1x1_Belarus.txt", skip = 2, sep = "")
Canada_mort <- read.delim("bltper_1x1_Canada.txt", skip = 2, sep = "")
Belgium_mort <- read.delim("bltper_1x1_Belgium.txt", skip = 2, sep = "")
Bulgaria_mort <- read.delim("bltper_1x1_Bulgaria.txt", skip = 2, sep = "")
Chile_mort <- read.delim("bltper_1x1_Chile.txt", skip = 2, sep = "")
Croatia_mort <- read.delim("bltper_1x1_Croatia.txt", skip = 2, sep = "")


Jp_mort <- read.delim("bltper_1x1_JP.txt", skip = 2, sep = "")



add_Country <- function(Comp, Country_mort) {
  Country_mort <- Country_mort %>%
    filter(Year == max(Year)) %>%
    dplyr::select(Age, qx)
  
  Country_mort$Age[Country_mort$Age == "110+"] <- as.integer(110)
  
  Country_mort$Age <- as.numeric(Country_mort$Age)
  
  Comp <- left_join(Comp, Country_mort, by = join_by(Age))
  return(Comp)
}



Country_comp <- lumaria_mort_table[,1:2]

Country_comp <-
  add_Country(Comp = Country_comp, Country_mort =  Australia_mort) %>%
  add_Country(Country_mort = Austria_mort) %>%
  add_Country(Country_mort = Belarus_mort) %>%
  add_Country(Country_mort = Belgium_mort) %>%
  add_Country(Country_mort = Bulgaria_mort) %>%
  add_Country(Country_mort = Canada_mort) %>%
  add_Country(Country_mort = Chile_mort) %>%
  add_Country(Country_mort = Croatia_mort)

colnames(Country_comp) <-
  c(
    "Age",
    "Lumaria",
    "Australia",
    "Austria",
    "Belarus",
    "Belgium",
    "Bulgaria",
    "Canada",
    "Chile",
    "Croatia"
  )

Country_comp <- Country_comp %>%
  filter(Age <= 110)

Country_comp <- sapply(Country_comp, as.numeric)

Country_comp <- as.data.frame(Country_comp)

country_comp_plot <- pivot_longer(Country_comp, cols = -1)

country_comp_plot <- mutate(country_comp_plot,
                           lumaria = case_when(name == "Lumaria" ~ "Lumaria", .default = "Other"))



ggplot(country_comp_plot) +
  aes(x = Age, y = log(value), colour = name, size = lumaria) +
  geom_line() +
  scale_size_manual(values = c(2,0.5)) +
  scale_color_manual(values = brewer.pal(n = 9, name = "Set1")) +
  labs(y = "Mortality Rate (Log-Scale)", x = "Age", title = "Country Comparison of Mortality", size = "", colour = "Country") 

world_age_dist <- read_xlsx("World age distribution.xlsx", skip = 2)

world_age_dist[nrow(world_age_dist) + 1,] <- list("Lumaria", "20%", "76%", "4%")


world_age_dist <- world_age_dist[c(nrow(world_age_dist), 1:(nrow(world_age_dist)-1)), ]


world_age_dist[, 2:4] <- sapply(world_age_dist[, 2:4], function(x) as.numeric(sub("%", "", x))/100)


world_age_dist_mat <- world_age_dist[, 2:4]

dist_matrix <- dist(world_age_dist_mat)

dist_matrix <- as.matrix(dist_matrix)

min(dist_matrix[1,-1])

row.names(dist_matrix)[dist_matrix[1, ] == min(dist_matrix[1,-1])]

as.numeric(names(sort(dist_matrix[1,])[1:20]))

world_age_dist$Country[as.numeric(names(sort(dist_matrix[1,])[1:20]))]




# Economic Data -----------------------------------------------------------


economic_data <- read_xlsx("srcsc-2024-lumaria-economic-data.xlsx", skip = 11)




colnames(economic_data) <- make.names(colnames(economic_data))

mean(economic_data$Government_of_Lumaria_Overnight_Rate)

economic_data_plot <- pivot_longer(economic_data, cols = 2:5 )

ggplot(economic_data_plot) +
  aes(x = Year, y = value, colour = name) +
  geom_line() +
  scale_colour_manual(values = brewer.pal(4, "Dark2"))

display.brewer.all()

mean(tail(economic_data$X10.yr.Risk.Free.Annual.Spot.Rate, 20))
mean(tail(economic_data$X1.yr.Risk.Free.Annual.Spot.Rate, 20))  


# SPWL --------------------------------------------------------------------


superlife_inforce_SPWL$Current_age[superlife_inforce_SPWL$Current_age == 0] <- NA

min(superlife_inforce_SPWL$Issue.age)
max(superlife_inforce_SPWL$Issue.age)

sum(superlife_inforce_SPWL$Distribution.Channel == "Agent")
sum(superlife_inforce$Distribution.Channel == "Agent")

min(superlife_inforce_T20$Issue.age)
max(superlife_inforce_T20$Issue.age)

superlife_inforce_SPWL_group <- superlife_inforce_SPWL

sum(superlife_inforce_SPWL$Distribution.Channel == "Agent")/length(superlife_inforce_SPWL$Distribution.Channel)
#Alive

superlife_inforce_alive <- superlife_inforce %>%
  filter(is.na(leave.indicator) == T)


min(superlife_inforce$Issue.age)
max(superlife_inforce_T20$Issue.age)

superlife_inforce_in <- left_join(superlife_inforce_alive, lumaria_mort_table[,1:2], by = join_by(Issue.age == Age))
