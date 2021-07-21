# _______________________########
# PACKAGES AND LIBRARIES ########
# _______________________########

#__reader #####
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
library(readr)

#__tidyverse #####
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)

#__caret #####
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)

#__igraph ####
if(!require(igraph)) install.packages("igraph", repos = "http://cran.us.r-project.org")
library(igraph)

#__rattle ####
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
library(rattle)

#__randomForest ####
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
library(randomForest)

#__fastAdaboost ####
if(!require(fastAdaboost)) install.packages("fastAdaboost", repos = "http://cran.us.r-project.org")
library(fastAdaboost)

#__ROSE ####
if(!require(ROSE)) install.packages("ROSE", repos = "http://cran.us.r-project.org")
library(ROSE)

#__mda ####
if(!require(mda)) install.packages("mda", repos = "http://cran.us.r-project.org")
library(mda)

#__klaR ####
if(!require(klaR)) install.packages("klaR", repos = "http://cran.us.r-project.org")
library(klaR)

#__nnet ####
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
library(nnet)

#__kernlab ####
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
library(kernlab)

#__e1071 ####
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
library(e1071)

#__viridis ####
if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
library(viridis)

#__patchwork####
if(!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
library(patchwork)

#__hrbrthemes####
if(!require(hrbrthemes)) install.packages("hrbrthemes", repos = "http://cran.us.r-project.org")
library(hrbrthemes)

#__ggraph####
if(!require(ggraph)) install.packages("ggraph", repos = "http://cran.us.r-project.org")
library(ggraph)

#__readxl####
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
library(readxl)

#__knitr####
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
library(knitr)

#__tidyr####
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
library(tidyr)

#__dplyr####
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

#__plotly####
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
library(plotly)

#__magrittr####
if(!require(magrittr)) install.packages("plotly", repos = "http://cran.us.r-project.org")
library(magrittr)

#__lubridate####
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(lubridate)

#__rvest####
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
library(rvest)

#__rpart####
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
library(rpart)

#__rpart.plot####
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
library(rpart.plot)

#__pROC####
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
library(pROC)

#__nnet####
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
library(nnet)

#__earth####
if(!require(earth)) install.packages("earth", repos = "http://cran.us.r-project.org")
library(earth)

#__LiblineaR####
if(!require(LiblineaR)) install.packages("LiblineaR", repos = "http://cran.us.r-project.org")
library(LiblineaR)

#__MLeval####
if(!require(MLeval)) install.packages("MLeval", repos = "http://cran.us.r-project.org")

#__Naives Bayes####
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")
library(naivebayes) #cambio

#__Recipes####
if(!require(recipes)) install.packages("recipes", repos = "http://cran.us.r-project.org")
library(recipes)

#__Rattle####
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
library(rattle)

# _______________________########
# DATA DOWNLOAD ########
# _______________________########

# stroke_data from github #####

stroke_data <- 
  read.csv("https://raw.github.com/felmaggilab/edx_data_science_capstone_strokes/master/data/healthcare-dataset-stroke-data.csv")

str(stroke_data)

# _______________________########
# DATA WRANGLING ########
# _______________________########

# Categorical and binary data as.factors #####

stroke_data <- stroke_data %>% 
  filter(!bmi == "N/A")  %>% # filtering bmi = N/A
  mutate(gender = as.factor(gender),
         hypertension = as.factor(ifelse(hypertension == 0, "No", "Yes")),
         heart_disease = as.factor(ifelse(heart_disease == 0, "No", "Yes")),
         ever_married = as.factor(ever_married),
         work_type = as.factor(work_type),
         Residence_type = as.factor(Residence_type),
         bmi = as.numeric(bmi),
         smoking_status = as.factor(smoking_status),
         stroke = as.factor(ifelse(stroke == 1, "stroke", "no_stroke"))) %>% 
  select(!id) # Removing "id" variable

# Relevel "stroke" "no_stroke" factors: positive class: "stroke" #### 
# It makes "stroke" the positive class, in order to facilitate interpretations

stroke_data$stroke <- relevel(stroke_data$stroke, ref = "stroke")

head(stroke_data)
str(stroke_data)

# _______________________########
# EXPLORATORY DATA ANALYSIS ########
# https://www.cienciadedatos.net/documentos/41_machine_learning_con_r_y_caret

# _______________________########

#Number of observations ######
n <- nrow(stroke_data)
n
# 4909

# Number of strokes in data set ####
n_strokes <-  sum(stroke_data$stroke == "stroke") 
# 209

# Strokes proportion #####
table(stroke_data$stroke) %>% 
  kable()

prop.table(table(stroke_data$stroke)) %>% 
  kable()

# Number of gender = Male in data set ####
n_males <- sum(stroke_data$gender == "Male") 
# 2011

# Number of gender = Female in data set ####
n_females <- sum(stroke_data$gender == "Female") 
# 2897

# Percent of strokes = 1 in data set ####
mean(stroke_data$stroke == "stroke") 
# 0.04257486

# Percent of no_strokes in data set ####
mean(stroke_data$stroke == "no_stroke") 
# 0.9574251

# Percent of gender = Male in data set ####
mean(stroke_data$gender == "Male") 
# 0.4096557

# Percent of gender = Female in data set ####
mean(stroke_data$gender == "Female") 
# 0.5901406

#Percent of strokes: gender Male ####
stroke_data %>% 
  filter(gender == "Male") %>%
  summarise(mean(stroke == "stroke")) 
# 0.0443

#Percent of strokes: gender Female ####
stroke_data %>% 
  filter(gender == "Female") %>%
  summarise(mean(stroke == "stroke"))
# 0.0414

# NUMERICAL VARIABLES____ ######

# AGE (continuous)######

# __Statistical Age Data  ####
stroke_data %>% 
  group_by(stroke) %>% 
  summarise(avg_age = round(mean(age),1),
            median_age = round(median(age)),
            min_age = min(age),
            max_age = max(age)) %>% 
  kable()

# __Density Plot: Age per Class ####
stroke_data %>% ggplot(aes(age, fill = stroke)) +
  geom_density(alpha = 0.2, bw = 1) +
  labs(title = "Age density plot") +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

stroke_data %>% ggplot(aes(age, y = ..count.., fill = stroke)) +
  geom_density(alpha = 0.2, bw = 1) 

# __Box Plot: Age per Class ####
stroke_data %>% ggplot(aes(stroke, age, color = stroke)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, width = 0.15) +
  labs(title = "Age box plot") +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Summary table by age #####
# age, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(age) %>%
  summarise(total = n(), percent = round(total/n, 3), 
            strokes = sum(stroke == "stroke"), 
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Summary table by age (filtering only positive strokes cases) #####
# age, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(age) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  filter(!strokes == 0) %>% 
  unique() %>%
  knitr::kable()

# __Distribution of observations by age #####
stroke_data %>% 
  group_by(age) %>%
  summarise(age = age, total = n(), strokes = sum(stroke == "stroke"),
            stroke_ratio = mean(stroke == "stroke")) %>% 
  # filter(!stroke_ratio == 0) %>% 
  unique() %>%
  ggplot(aes(x=age, y=total)) +
  geom_segment(aes(x=age, xend=age, y=0, yend=total), color="skyblue") +
  geom_point( color="blue", size=2, alpha=0.6) +
  theme_light() +
  #coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Number of Observations by Age") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Number of strokes by age  ######
stroke_data %>% 
  group_by(age) %>%
  summarise(age = age, total = n(), strokes = sum(stroke == "stroke"),
            stroke_ratio = mean(stroke == "stroke")) %>% 
  filter(!stroke_ratio == 0) %>% 
  unique() %>%
  ggplot(aes(x=age, y=strokes)) +
  geom_segment(aes(x=age, xend=age, y=0, yend=strokes), color="skyblue") +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  # coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Number of Strokes by Age") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Percent of strokes by age ####
stroke_data %>% 
  group_by(age) %>%
  summarise(age = age, total = n(), strokes = sum(stroke == "stroke"),
            stroke_percent = mean(stroke == "stroke")) %>% 
  filter(!stroke_percent == 0) %>% 
  unique() %>%
  ggplot(aes(x=age, y=stroke_percent)) +
  geom_segment(aes(x=age, xend=age, y=0, yend=stroke_percent), color="skyblue") +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  # coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Percent of Strokes by Age") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Summary table by age (rounded nearest 10) #####
# age, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(round(age, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke =="stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Distribution of observations by rounded age  #####
stroke_data %>% 
  group_by(age = round(age, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  ggplot(aes(x=age, y=total)) +
  geom_segment(aes(x=age, xend=age, y=0, yend=total), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  # coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Number of Observations by Age (Rounded)") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Number of strokes by rounded age  #####
stroke_data %>% 
  group_by(age = round(age, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  ggplot(aes(x=age, y=strokes)) +
  geom_segment(aes(x=age, xend=age, y=0, yend=strokes), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  #coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Number of Strokes by Age (Rounded)") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Percent of strokes by rounded age  ####  
stroke_data %>% 
  group_by(age = round(age, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  ggplot(aes(x=age, y=stroke_percent)) +
  geom_segment(aes(x=age, xend=age, y=0, yend=stroke_percent), color="skyblue") +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  # coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Percent of Strokes by Age") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# AVG GLUCOSE LEVEL (continuous) ######

# __Statistical Glucose Data  ####
stroke_data %>% 
  group_by(stroke) %>% 
  summarise(avg_glucose = round(mean(avg_glucose_level),1),
            median_glucose = round(median(avg_glucose_level)),
            min_glucose = min(avg_glucose_level),
            max_glucose = max(avg_glucose_level)) %>% 
  kable()

# __Density Plot: Avg Level per Class ####
stroke_data %>% ggplot(aes(avg_glucose_level, fill = stroke)) +
  geom_density(alpha = 0.2, bw = 5) +
  labs(title = "Avg Glucose Level density plot") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))
  

stroke_data %>% ggplot(aes(avg_glucose_level, y = ..count.., fill = stroke)) +
  geom_density(alpha = 0.2, bw = 1) +
  labs(title = "Avg Glucose Level density plot") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Box Plot: Avg Level per Class ####
stroke_data %>% ggplot(aes(stroke, avg_glucose_level, color = stroke)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, width = 0.15) +
  labs(title = "Avg Glucose Level box plot") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Summary table by avg_glucose_level (round to nearest ten) #####
# avg_glucose_level, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(round_avg_glucose_level = round(avg_glucose_level, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Distribution of observations by rounded avg_glucose_level  #####
stroke_data %>% 
  group_by(avg_glucose_level = round(avg_glucose_level, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  ggplot(aes(x=avg_glucose_level, y=total)) +
  geom_segment(aes(x=avg_glucose_level, xend=avg_glucose_level, y=0, yend=total), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  # coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Number of Observations by avg_glucose_level (Rounded)") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Number of strokes by rounded avg_glucose_level  #####
stroke_data %>% 
  group_by(avg_glucose_level = round(avg_glucose_level, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  ggplot(aes(x=avg_glucose_level, y=strokes)) +
  geom_segment(aes(x=avg_glucose_level, xend=avg_glucose_level, y=0, yend=strokes), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  # coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Number of Strokes by avg_glucose_level (Rounded)") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Percent of strokes by rounded avg_glucose_level ####  
stroke_data %>% 
  group_by(avg_glucose_level = round(avg_glucose_level, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  ggplot(aes(x=avg_glucose_level, y=stroke_percent)) +
  geom_segment(aes(x=avg_glucose_level, xend=avg_glucose_level, y=0, yend=stroke_percent), color="skyblue") +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  # coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Percent of Strokes by avg_glucose_level (Rounded)") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# BMI (continuous) ######

# __Statistical BMI Data  ####
stroke_data %>% 
  group_by(stroke) %>% 
  summarise(avg_bmi = round(mean(bmi),1),
            median_bmi = round(median(bmi)),
            min_bmi = min(bmi),
            max_bmi = max(bmi)) %>% 
  kable()

# __Density Plot: BMI per Class ####
stroke_data %>% ggplot(aes(bmi, fill = stroke)) +
  geom_density(alpha = 0.2, bw = 1) +
  labs(title = "BMI density plot") +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

stroke_data %>% ggplot(aes(bmi, y = ..count.., fill = stroke)) +
  geom_density(alpha = 0.2, bw = 1)

# __Box Plot: BMI per Class ####
stroke_data %>% ggplot(aes(stroke, bmi, color = stroke)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, width = 0.15)  +
  labs(title = "BMI box plot") +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Summary table by bmi (round to nearest ten) #####
# bmi, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  mutate(bmi = as.numeric(bmi)) %>% 
  group_by(round_bmi = round(bmi,-1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Distribution of observations by rounded bmi  #####
stroke_data %>% 
  mutate(bmi = as.numeric(bmi)) %>% 
  group_by(bmi = round(bmi, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  ggplot(aes(x=bmi, y=total)) +
  geom_segment(aes(x=bmi, xend=bmi, y=0, yend=total), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  #coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Number of Observations by bmi (Rounded)") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Number of strokes by rounded bmi  #####
stroke_data %>% 
  mutate(bmi = as.numeric(bmi)) %>% 
  group_by(bmi = round(bmi, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  ggplot(aes(x=bmi, y=strokes)) +
  geom_segment(aes(x=bmi, xend=bmi, y=0, yend=strokes), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  # coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Number of Strokes by bmi (Rounded)") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Percent of strokes by rounded bmi ####  
stroke_data %>% 
  mutate(bmi = as.numeric(bmi)) %>% 
  group_by(bmi = round(bmi, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  ggplot(aes(x=bmi, y=stroke_percent)) +
  geom_segment(aes(x=bmi, xend=bmi, y=0, yend=stroke_percent), color="skyblue") +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  # coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Percent of bmi (Rounded)") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# CATEGORICAL VARIABLES######

# GENDER (binary)_____ ######

# __Summary table by gender #####
# gender, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(gender) %>%
  summarise(total = n(), percent = round(total/n, 3), 
            strokes = sum(stroke == "stroke"), 
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

stroke_data <- stroke_data %>% filter(!gender == "Other") # Filtering gender "Other" for training purposes

# __Gender distribution #####
stroke_data %>% 
  ggplot(aes(x = gender, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Gender distribution") +
  theme_bw() +
  theme(legend.position = "bottom")

# HYPERTENSION (binary) ######

# __Summary table by hypertension #####
# hypertension, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(hypertension) %>%
  summarise( total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
             stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Hypertension distribution #####
stroke_data %>% 
  ggplot(aes(x = hypertension, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Hypertension distribution") +
  theme_bw() +
  theme(legend.position = "bottom")

# HEART DESEASE (binary) ######

# __Summary table by heart_disease #####
# heart_disease, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(heart_disease) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke =="stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Heart_disease distribution #####
stroke_data %>% 
  ggplot(aes(x = heart_disease, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Heart disease distribution") +
  theme_bw() +
  theme(legend.position = "bottom")

# __EVER MARRIED (binary) ######

# __Summary table by ever_married #####
# ever_married, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(ever_married) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Ever_married distribution #####
stroke_data %>% 
  ggplot(aes(x = ever_married, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Ever married distribution") +
  theme_bw() +
  theme(legend.position = "bottom")

# WORK TYPE (nominal) ######

# __Summary table by work_type #####
# work_type, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(work_type) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Work_type distribution #####
stroke_data %>% 
  ggplot(aes(x = work_type, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Work type distribution") +
  theme_bw() +
  theme(legend.position = "bottom")

# __Summary table by work_type #####
# work_type, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(work_type) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# RESIDENCE TYPE (binary)######

# Summary table by Residence_type #####
# age, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(Residence_type) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Residence_type distribution #####
stroke_data %>% 
  ggplot(aes(x = Residence_type, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Residence type distribution") +
  theme_bw() +
  theme(legend.position = "bottom")


# SMOKING STATUS (nominal) ######

# __Summary table by smoking_status  #####
# smoking_status, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(smoking_status) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"), 
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Smoking_status distribution #####
stroke_data %>% 
  ggplot(aes(x = smoking_status, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Smoking status distribution") +
  theme_bw() +
  theme(legend.position = "bottom")

# _______________________########
# VARIABLE IMPORTANCE: Looking for variables I ########
# _______________________########

# COR. OF NUMERICAL VARIABLES #####

# __Age vs Avg Glucose Level #####
stroke_data %>% 
  ggplot(aes(age, avg_glucose_level)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Age vs Avg Glucose Level") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

cor(stroke_data$age, stroke_data$avg_glucose_level)

# __Age vs Avg Glucose Level correlation test
cor.test(stroke_data$age, stroke_data$avg_glucose_level, method = "pearson")
# 0.2359996

# __Age vs BMI #####
stroke_data %>% 
  ggplot(aes(age, bmi)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Age vs BMI") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

cor(stroke_data$age, stroke_data$bmi)

# __Age vs BMI correlation test
cor.test(stroke_data$age, stroke_data$bmi, method = "pearson")
# 0.3333142

# __Avg Glucose Level vs BMI #####
stroke_data %>% 
  ggplot(aes(avg_glucose_level, bmi)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Avg Glucose Level vs BMI") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

cor(stroke_data$avg_glucose_level, stroke_data$bmi)

# __Avg Glucose Level vs BMI correlation test
cor.test(stroke_data$avg_glucose_level, stroke_data$bmi, method = "pearson")
# 0.1756717


# There are some correlation between numerical variables, 
# but it is not really strong


# CATEGORICAL VARIABLES #####
# __Contrast of proportions #####

# Continuous and qualitative variables that do not group patients are excluded.
stroke_categorical <- stroke_data %>% 
  select(gender, hypertension, heart_disease, ever_married, work_type,
         Residence_type, smoking_status, stroke)

stroke_categorical_tidy <- data.frame(stroke_categorical %>%
                                        gather(key = "variable", value = "group",-stroke))

# An identifier consisting of the name of the variable and the group is added
stroke_categorical_tidy <- stroke_categorical_tidy %>%
  mutate(group_variable = paste(variable, group, sep = "_"))

# Function that calculates the proportions test for the column "Stroke" of a df
proportion_test <- function(df){
  n_strokes <- sum(df$stroke == "stroke") 
  n_no_stroke     <- sum(df$stroke == "no_stroke")
  n_total <- n_strokes + n_no_stroke
  test <- prop.test(x = n_strokes, n = n_total, p = 0.04257486)
  prop_strokes <- n_strokes / n_total
  return(data.frame(p_value = test$p.value, prop_strokes))
}

# The data is grouped by "group_variable" and the test_proportion () function 
# is applied to each group.
prop_analisis <- stroke_categorical_tidy %>%
  group_by(group_variable) %>%
  nest() %>%
  arrange(group_variable) %>%
  mutate(prop_test = map(.x = data, .f = proportion_test)) %>%
  unnest(prop_test) %>%
  arrange(p_value) %>% 
  select(group_variable,p_value, prop_strokes) %>% 
  head(10) %>% 
  kable()
prop_analisis

# NUMERICAL AND CATEGORICAL VARIABLES #####

# __Random Forest Method #####
variables_rf <- stroke_data


randforest_model <- randomForest(formula = stroke ~ . ,
                                 data = variables_rf,
                                 mtry = 5,
                                 importance = TRUE, 
                                 ntree = 1000) 

importance <- as.data.frame(randforest_model$importance)
importance <- rownames_to_column(importance,var = "variable")

importance1 <- ggplot(data = importance, aes(x = reorder(variable, MeanDecreaseAccuracy),
                                             y = MeanDecreaseAccuracy,
                                             fill = MeanDecreaseAccuracy)) +
  labs(x = "variable", title = "Accuracy reduction") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
importance1

importance2 <- ggplot(data = importance, aes(x = reorder(variable, MeanDecreaseGini),
                                             y = MeanDecreaseGini,
                                             fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Gini Reduction") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
importance2

# Why is Heart Disease so underweight?
# Because it is closely related to age, which is the main variable

# __Summary table by age and Heart Desease
stroke_data %>% 
  group_by(age) %>%
  summarise(total = n(), percent = round(total/n, 3), heart_disease = sum(heart_disease == "Yes"),
            heart_desease_percent = heart_disease/total) %>% 
  unique() %>%
  knitr::kable()

# __Summary table by age (rounded nearest 10) and Heart Desease
# age, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(round_age = round(age, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), heart_disease = sum(heart_disease == "Yes"),
            heart_desease_percent = round(heart_disease/total,3)) %>% 
  unique() %>%
  knitr::kable()

# Why is Hypertension so underweight?
# As Heart Desease, it is closely related to age, which is the main variable

# __Summary table by age (rounded nearest 10) and Hypertension
# age, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(round_age = round(age, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), hypertension = sum(hypertension == "Yes"),
            hypertension_percent = round(hypertension/total,3)) %>% 
  unique() %>%
  knitr::kable()


# __Near Zero Variance Analysis #####
stroke_data %>% 
  select(-stroke) %>% 
  nearZeroVar(saveMetrics = TRUE)

# _______________________########
# TREES (rpart): Loking for variables II ########
# _______________________########

# Rpart tunning parameters ######
minsplit_tune <- tune.rpart(stroke ~ .,
                            data = stroke_data, 
                            minsplit = seq(1,100,5))
plot(minsplit_tune, main = "tune minsplit")

cp_tune <- tune.rpart(stroke ~ .,
                      data = stroke_data, 
                      cp = seq(0.000, 0.02, len = 50))
plot(cp_tune, main = "tune cp")

cp_tune <- train(stroke ~ ., 
                 method = "rpart",
                 tuneGrid = data.frame(cp = seq(0.000, 0.02, len = 50)),
                 data = stroke_data)
plot(cp_tune)

maxdepth_tune <- tune.rpart(stroke ~ .,
                            data = stroke_data, 
                            maxdepth = 1:30)
plot(maxdepth_tune, main = "tune maxdepth")

# All Data #####

# __Gini Tree, CP = 0.01, minsplit = 20, minbucket round 20/3, maxdepht = 30 ####
rpart.plot(rpart(stroke ~ ., 
                 data = stroke_data)) # Default rpart tree

# __Gini Tree, CP = 0.001, minsplit = 20, minbucket round 20/3, maxdepht = 30 #####
rpart.plot(rpart(stroke ~., 
                 data = stroke_data, 
                 parms=list(split=c("gini")),
                 cp = 0.001))

# __Gini Tree, CP = 0.0024, minslit = 20, minbucket round 20/3, maxdepht = 30 ######
rpart.plot(rpart(stroke ~., 
                 data = stroke_data, 
                 parms=list(split=c("gini")),
                 cp = 0.0024))

# __Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 6 #####
rpart.plot(rpart(stroke ~., 
                 data = stroke_data, 
                 parms=list(split=c("gini")),
                 cp = 0.001,
                 maxdepth = 6))

# __Information Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
rpart.plot(rpart(stroke ~., 
                 data = stroke_data, 
                 parms=list(split=c("information"))))

# __Information Tree, CP = 0.001. minslit = 20, minbucket round 20/3, maxdepth = 30 #####
rpart.plot(rpart(stroke ~., 
                 data = stroke_data, 
                 parms=list(split=c("information")),
                 cp = 0.001))

# __Information Tree, CP = 0.0023. minslit = 20, minbucket round 20/3, maxdepht = 30 #####
rpart.plot(rpart(stroke ~., 
                 data = stroke_data, 
                 parms=list(split=c("information")),
                 cp = 0.0023))

# __Information Tree, CP = 0.0023. minslit = 20, minbucket round 20/3, maxdepht = 6 #####
rpart.plot(rpart(stroke ~., 
                 data = stroke_data, 
                 parms=list(split=c("information")),
                 cp = 0.0023,
                 maxdepth = 6))

inf_tree_cp0.001_max6 <- rpart(stroke ~., 
                               data = train_stroke_t, 
                               parms=list(split=c("information")),
                               cp = 0.0023,
                               maxdepth = 6)

#Categorical Data and Binary Data #####
stroke_categorical <- stroke_data %>% 
  select(gender, hypertension, heart_disease, ever_married, work_type,
         Residence_type, smoking_status, stroke)

# __Default Tree: Gini, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
default_tree_cat <- rpart(stroke ~ ., 
                          data = stroke_categorical)
rpart.plot(default_tree_cat)

# __Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
gini_tree_cp0.001_cat <- rpart(stroke ~., 
                               data = stroke_categorical, 
                               parms=list(split=c("gini")),
                               cp = 0.001)
rpart.plot(gini_tree_cp0.001_cat)

# __Information Tree, CP = 0.001. minslit = 20, minbucket round 20/3, maxdepht = 30 #####
inf_tree_cp0.001_cat <- rpart(stroke ~., 
                              data = stroke_categorical, 
                              parms=list(split=c("information")),
                              cp = 0.001)
rpart.plot(inf_tree_cp0.001_cat)

#Numerical Data #####
stroke_numerical <- stroke_data %>% 
  select(age, avg_glucose_level, bmi, stroke)

# __Default Tree: Gini, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
default_tree_num <- rpart(stroke ~ ., 
                          data = stroke_numerical)
rpart.plot(default_tree_num)

# __Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
gini_tree_cp0.001_num <- rpart(stroke ~., 
                               data = stroke_numerical, 
                               parms=list(split=c("gini")),
                               cp = 0.001)
rpart.plot(gini_tree_cp0.001_num)

# __Information Tree, CP = 0.001. minslit = 20, minbucket round 20/3, maxdepht = 30 #####
inf_tree_cp0.001_num <- rpart(stroke ~., 
                              data = stroke_numerical, 
                              parms=list(split=c("information")),
                              cp = 0.001)
rpart.plot(inf_tree_cp0.001_num)

# _______________________########
# BALANCED DATA: Loking for variables III ########
# _______________________########

# _______________________########
# Over Sampling ######
# _______________________########
table(stroke_data$stroke)
prop.table(table(stroke_data$stroke))

n_over = sum(stroke_data == "no_stroke")

set.seed(1969, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1969)`
# Every time we run the code, we get a different ovun.sample
# Visualization tree, Accuracy, Sensibility, Balanced accuracy and F.meas strongly depends on this ramdom process.

stroke_data_over <- ovun.sample(stroke ~ ., data = stroke_data, method = "over", N = n_over*2)$data
table(stroke_data_over$stroke)

# Relevel "stroke" "no_stroke" factors: positive class: "stroke"
stroke_data_over$stroke <- relevel(stroke_data_over$stroke, ref = "stroke")

table(stroke_data_over$stroke) %>% 
  kable()

prop.table(table(stroke_data_over$stroke))  %>% 
  kable()

# As expected, the process of balancing (over, both or better method) doesn't change the distributions of 
# variables. We will see ths next:

# __Statistical Age Data  #####

stroke_data_over %>% 
  group_by(stroke) %>% 
  summarise(avg_age = round(mean(age),1),
            median_age = round(median(age)),
            min_age = min(age),
            max_age = max(age)) %>% 
  kable()

# __Density Plot: Age per Class ####

stroke_data_over %>% ggplot(aes(age, fill = stroke)) +
  geom_density(alpha = 0.2, bw = 1)

stroke_data_over %>% ggplot(aes(age, y = ..count.., fill = stroke)) +
  geom_density(alpha = 0.2, bw = 1)

# __Box Plot: Age per Class ####

stroke_data_over %>% ggplot(aes(stroke, age, color = stroke)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, width = 0.15)

# __Statistical Glucose Data  ####

stroke_data_over %>% 
  group_by(stroke) %>% 
  summarise(avg_glucose = round(mean(avg_glucose_level),1),
            median_glucose = round(median(avg_glucose_level)),
            min_glucose = min(avg_glucose_level),
            max_glucose = max(avg_glucose_level)) %>% 
  kable()

# __Density Plot: Avg Level per Class ####

stroke_data_over %>% ggplot(aes(avg_glucose_level, fill = stroke)) +
  geom_density(alpha = 0.2, bw = 5)

stroke_data_over %>% ggplot(aes(avg_glucose_level, y = ..count.., fill = stroke)) +
  geom_density(alpha = 0.2, bw = 5)

# __Box Plot: Avg Level per Class ####

stroke_data_over %>% ggplot(aes(stroke, avg_glucose_level, color = stroke)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, width = 0.15)

# __Statistical BMI Data  ####

stroke_data_over %>% 
  group_by(stroke) %>% 
  summarise(avg_glucose = round(mean(bmi),1),
            median_glucose = round(median(bmi)),
            min_glucose = min(bmi),
            max_glucose = max(bmi)) %>% 
  kable()

# __Density Plot: Avg Glucose Level per Class ####

stroke_data_over %>% ggplot(aes(bmi, fill = stroke)) +
  geom_density(alpha = 0.2, bw = 1)

stroke_data_over %>% ggplot(aes(bmi, y = ..count.., fill = stroke)) +
  geom_density(alpha = 0.2, bw = 1)

# __Box Plot: Avg Level Glucose  per Class ####

stroke_data_over %>% ggplot(aes(stroke, bmi, color = stroke)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, width = 0.15)

# __Gender distribution #####
stroke_data_over %>% 
  ggplot(aes(x = gender, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Gender") +
  theme_bw() +
  theme(legend.position = "bottom")

# __Hypertension distribution #####
stroke_data_over %>% 
  ggplot(aes(x = hypertension, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Hypertension") +
  theme_bw() +
  theme(legend.position = "bottom")

# __Heart_disease distribution #####
stroke_data_over %>% 
  ggplot(aes(x = heart_disease, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Heart_disease") +
  theme_bw() +
  theme(legend.position = "bottom")


# __Ever_married distribution #####
stroke_data_over %>% 
  ggplot(aes(x = ever_married, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Ever_married") +
  theme_bw() +
  theme(legend.position = "bottom")

# __Work_type distribution #####
stroke_data_over %>% 
  ggplot(aes(x = work_type, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Work_type") +
  theme_bw() +
  theme(legend.position = "bottom")

# __Residence_type distribution #####
stroke_data_over %>% 
  ggplot(aes(x = Residence_type, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Residence_type") +
  theme_bw() +
  theme(legend.position = "bottom")

# __Summary table by smoking_status  #####
# smoking_status, total of observations, number of strokes, percent of strokes

n_stroke_over <-  nrow(stroke_data_over)
  
stroke_data_over %>% 
  group_by(smoking_status) %>%
  summarise(total = n(), percent = round(total/n_stroke_over, 3), strokes = sum(stroke == "stroke"), 
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Smoking_status distribution #####
stroke_data_over %>% 
  ggplot(aes(x = smoking_status, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Smoking_status") +
  theme_bw() +
  theme(legend.position = "bottom")

# Gini Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
rpart.plot(rpart(stroke ~ ., 
                 data = stroke_data_over)) # Default rpart tree

caret_tree_print_over <- train(stroke ~ ., method = "rpart", 
                               data = stroke_data_over)

fancyRpartPlot(caret_tree_print_over$finalModel)

# COR. OF NUMERICAL VARIABLES #####

# __Age vs Avg Glucose Level #####
stroke_data_over %>% 
  ggplot(aes(age, avg_glucose_level)) +
  geom_point() +
  geom_smooth()

cor(stroke_data_over$age, stroke_data_over$avg_glucose_level)
cor.test(stroke_data_over$age, stroke_data_over$avg_glucose_level, method = "pearson")

# __BMI vs Avg Glucose Level #####
stroke_data_over %>% 
  ggplot(aes(bmi, avg_glucose_level)) +
  geom_point() +
  geom_smooth()

cor(stroke_data_over$bmi, stroke_data_over$avg_glucose_level)
cor.test(stroke_data_over$bmi, stroke_data_over$avg_glucose_level, method = "pearson")

# __Age vs BMI #####
stroke_data_over %>% 
  ggplot(aes(age, bmi)) +
  geom_point() +
  geom_smooth()

cor(stroke_data_over$age, stroke_data_over$bmi)
cor.test(stroke_data_over$age, stroke_data_over$avg_glucose_level, method = "pearson")

# NUMERICAL AND CATEGORICAL VARIABLES #####

# __Contrast of proportions #####

# Continuous and qualitative variables that do not group patients are excluded.
stroke_categorical_over <- stroke_data_over %>% 
  filter(!gender == "Other") %>% 
  select(gender, hypertension, heart_disease, ever_married, work_type,
         Residence_type, smoking_status, stroke)

stroke_categorical_tidy_over <- data.frame(stroke_categorical_over %>%
                                             gather(key = "variable", value = "group",-stroke))

# An identifier consisting of the name of the variable and the group is addedo 
stroke_categorical_tidy_over <- stroke_categorical_tidy_over %>%
  mutate(group_variable = paste(variable, group, sep = "_"))

# Function that calculates the proportions test for the column "Stroke" of a df
proportion_test_over <- function(df){
  n_strokes <- sum(df$stroke == "stroke") 
  n_no_stroke     <- sum(df$stroke == "no_stroke")
  n_total <- n_strokes + n_no_stroke
  test <- prop.test(x = n_strokes, n = n_total, p = 0.5)
  prop_strokes <- n_strokes / n_total
  return(data.frame(p_value = test$p.value, prop_strokes))
}

# The data is grouped by "group_variable" and the test_proportion () function 
# is applied to each group.
prop_analisis_over <- stroke_categorical_tidy_over %>%
  group_by(group_variable) %>%
  nest() %>%
  arrange(group_variable) %>%
  mutate(prop_test = map(.x = data, .f = proportion_test_over)) %>%
  unnest(prop_test) %>%
  arrange(p_value) %>% 
  select(group_variable,p_value, prop_strokes) %>% 
  head(20) %>% 
  kable()
prop_analisis_over

# __Random Forest Method #####
variables_rf_over <- stroke_data_over


randforest_model_over <- randomForest(formula = stroke ~ . ,
                                      data = variables_rf_over,
                                      mtry = 5,
                                      importance = TRUE, 
                                      ntree = 1000) 

importance_over <- as.data.frame(randforest_model_over$importance)
importance_over <- rownames_to_column(importance_over,var = "variable")

importance1_over <- ggplot(data = importance_over, aes(x = reorder(variable, MeanDecreaseAccuracy),
                                                       y = MeanDecreaseAccuracy,
                                                       fill = MeanDecreaseAccuracy)) +
  labs(x = "variable", title = "Accuracy reduction - Over") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
importance1_over

importance2_over <- ggplot(data = importance_over, aes(x = reorder(variable, MeanDecreaseGini),
                                                       y = MeanDecreaseGini,
                                                       fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Gini Reduction - Over") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
importance2_over

# __Near Zero Variance Analysis #####
stroke_data_over %>% 
  select(-stroke) %>% 
  nearZeroVar(saveMetrics = TRUE)

# _______________________########
# Over_Under Sampling: Both ######
# _______________________########
n_both = sum(stroke_data$stroke == "stroke") + sum(stroke_data$stroke == "no_stroke")

set.seed(1969, sample.kind="Rounding") # Every time we run the code, we get a different ovun.sample
# Accuracy and balanced accuracy strongly depends on this ramdom process.

stroke_data_both <- ovun.sample(stroke ~ ., data = stroke_data, method = "both", p = 0.5, N = n_both)$data
table(stroke_data_both$stroke)

# Relevel "stroke" "no_stroke" factors: positive class: "stroke"
stroke_data_both$stroke <- relevel(stroke_data_both$stroke, ref = "stroke")

table(stroke_data_over$stroke)
prop.table(table(stroke_data_over$stroke))

# __Summary table by smoking_status  #####
# smoking_status, total of observations, number of strokes, percent of strokes

n_stroke_both <-  nrow(stroke_data_both)

stroke_data_both %>% 
  group_by(smoking_status) %>%
  summarise(total = n(), percent = round(total/n_stroke_both, 3), strokes = sum(stroke == "stroke"), 
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Smoking_status distribution #####
stroke_data_over %>% 
  ggplot(aes(x = smoking_status, y = ..count.., fill = stroke)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Smoking_status") +
  theme_bw() +
  theme(legend.position = "bottom")

# Gini Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
rpart.plot(rpart(stroke ~ ., 
                 data = stroke_data_both)) # Default rpart tree

caret_tree_print_both <- train(stroke ~ ., method = "rpart", 
                               data = stroke_data_both)

fancyRpartPlot(caret_tree_print_both$finalModel)

table(stroke_data$stroke)
prop.table(table(stroke_data$stroke))

# COR. OF NUMERICAL VARIABLES #####

#__ Age vs Avg Glucose Level #####
stroke_data_both %>% 
  ggplot(aes(age, avg_glucose_level)) +
  geom_point() +
  geom_smooth()

cor(stroke_data_both$age, stroke_data_both$avg_glucose_level)
cor.test(stroke_data_both$age, stroke_data_both$avg_glucose_level, method = "pearson")

#__ BMI vs Avg Glucose Level #####
stroke_data_both %>% 
  ggplot(aes(bmi, avg_glucose_level)) +
  geom_point() +
  geom_smooth()

cor(stroke_data_both$bmi, stroke_data_both$avg_glucose_level)
cor.test(stroke_data_both$bmi, stroke_data_both$avg_glucose_level, method = "pearson")

#__ Age vs BMI #####
stroke_data_both %>% 
  ggplot(aes(age, bmi)) +
  geom_point() +
  geom_smooth()

cor(stroke_data_both$age, stroke_data_both$bmi)
cor.test(stroke_data_both$age, stroke_data_both$avg_glucose_level, method = "pearson")

# NUMERICAL AND CATEGORICAL VARIABLES #####

#__Contrast of proportions #####

# Continuous and qualitative variables that do not group patients are excluded.
stroke_categorical_both <- stroke_data_both %>% 
  filter(!gender == "Other") %>% 
  select(gender, hypertension, heart_disease, ever_married, work_type,
         Residence_type, smoking_status, stroke)

stroke_categorical_tidy_both <- data.frame(stroke_categorical_both %>%
                                             gather(key = "variable", value = "group",-stroke))

# An identifier consisting of the name of the variable and the group is added
stroke_categorical_tidy_both <- stroke_categorical_tidy_both %>%
  mutate(group_variable = paste(variable, group, sep = "_"))

# Function that calculates the proportions test for the column "Stroke" of a df
proportion_test_both <- function(df){
  n_strokes <- sum(df$stroke == "stroke") 
  n_no_stroke     <- sum(df$stroke == "no_stroke")
  n_total <- n_strokes + n_no_stroke
  test <- prop.test(x = n_strokes, n = n_total, p = 0.5)
  prop_strokes <- n_strokes / n_total
  return(data.frame(p_value = test$p.value, prop_strokes))
}

# The data is grouped by "group_variable" and the test_proportion () function 
# is applied to each group.
prop_analisis_both <- stroke_categorical_tidy_both %>%
  group_by(group_variable) %>%
  nest() %>%
  arrange(group_variable) %>%
  mutate(prop_test = map(.x = data, .f = proportion_test_both)) %>%
  unnest(prop_test) %>%
  arrange(p_value) %>% 
  select(group_variable,p_value, prop_strokes) %>% 
  head(20)
prop_analisis_both

#__Random Forest Method #####
variables_rf_both <- stroke_data_both


randforest_model_both <- randomForest(formula = stroke ~ . ,
                                      data = variables_rf_both,
                                      mtry = 5,
                                      importance = TRUE, 
                                      ntree = 1000) 

importance_both <- as.data.frame(randforest_model_both$importance)
importance_both <- rownames_to_column(importance_both,var = "variable")

importance1_both <- ggplot(data = importance_both, aes(x = reorder(variable, MeanDecreaseAccuracy),
                                                       y = MeanDecreaseAccuracy,
                                                       fill = MeanDecreaseAccuracy)) +
  labs(x = "variable", title = "Accuracy reduction - Both") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
importance1_both

importance2_both <- ggplot(data = importance_both, aes(x = reorder(variable, MeanDecreaseGini),
                                                       y = MeanDecreaseGini,
                                                       fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Gini Reduction - Both") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
importance2_both

# __Near Zero Variance Analysis #####
stroke_data_both %>% 
  select(-stroke) %>% 
  nearZeroVar(saveMetrics = TRUE)

# _______________________########
# Better Estimates #####
# _______________________########

table(stroke_data$stroke)
prop.table(table(stroke_data$stroke))

set.seed(1969, sample.kind="Rounding") # Every time we run the code, we get a different ovun.sample
# Accuracy and balanced accuracy strongly depends on this ramdom process.

stroke_data_better <- ROSE(stroke ~ ., data = stroke_data)$data
table(stroke_data_better$stroke)

# Relevel "stroke" "no_stroke" factors: positive class: "stroke"
stroke_data_better$stroke <- relevel(stroke_data_better$stroke, ref = "stroke")

table(stroke_data_better$stroke)
prop.table(table(stroke_data_better$stroke))

# Gini Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
rpart.plot(rpart(stroke ~ ., 
                 data = stroke_data_better)) # Default rpart tree

caret_tree_print_better <- train(stroke ~ ., method = "rpart", 
                                 data = stroke_data_better)

fancyRpartPlot(caret_tree_print_better$finalModel)

# COR. OF NUMERICAL VARIABLES #####

#__Age vs Avg Glucose Level #####
stroke_data_better %>% 
  ggplot(aes(age, avg_glucose_level)) +
  geom_point() +
  geom_smooth()

cor(stroke_data_better$age, stroke_data_better$avg_glucose_level)
cor.test(stroke_data_better$age, stroke_data_better$avg_glucose_level, method = "pearson")

#__BMI vs Avg Glucose Level #####
stroke_data_better %>% 
  ggplot(aes(bmi, avg_glucose_level)) +
  geom_point() +
  geom_smooth()

cor(stroke_data_better$bmi, stroke_data_better$avg_glucose_level)
cor.test(stroke_data_better$bmi, stroke_data_better$avg_glucose_level, method = "pearson")

#__Age vs BMI #####
stroke_data_better %>% 
  ggplot(aes(age, bmi)) +
  geom_point() +
  geom_smooth()

cor(stroke_data_better$age, stroke_data_better$bmi)
cor.test(stroke_data_better$age, stroke_data_better$avg_glucose_level, method = "pearson")

# NUMERICAL AND CATEGORICAL VARIABLES #####

#__Contrast of proportions #####

# Continuous and qualitative variables that do not group patients are excluded.
stroke_categorical_better <- stroke_data_better %>% 
  filter(!gender == "Other") %>% 
  select(gender, hypertension, heart_disease, ever_married, work_type,
         Residence_type, smoking_status, stroke)

stroke_categorical_tidy_better <- data.frame(stroke_categorical_better %>%
                                               gather(key = "variable", value = "group",-stroke))

# An identifier consisting of the name of the variable and the group is added
stroke_categorical_tidy_better <- stroke_categorical_tidy_better %>%
  mutate(group_variable = paste(variable, group, sep = "_"))

# Function that calculates the proportions test for the column "Stroke" of a df
proportion_test_better <- function(df){
  n_strokes <- sum(df$stroke == "stroke") 
  n_no_stroke     <- sum(df$stroke == "no_stroke")
  n_total <- n_strokes + n_no_stroke
  test <- prop.test(x = n_strokes, n = n_total, p = 0.5)
  prop_strokes <- n_strokes / n_total
  return(data.frame(p_value = test$p.value, prop_strokes))
}

# The data is grouped by "group_variable" and the test_proportion () function 
# is applied to each group.
prop_analisis_better <- stroke_categorical_tidy_better %>%
  group_by(group_variable) %>%
  nest() %>%
  arrange(group_variable) %>%
  mutate(prop_test = map(.x = data, .f = proportion_test_better)) %>%
  unnest(prop_test) %>%
  arrange(p_value) %>% 
  select(group_variable,p_value, prop_strokes) %>% 
  head(20)
prop_analisis_better

#__Random Forest Method #####
variables_rf_better <- stroke_data_better


randforest_model_better <- randomForest(formula = stroke ~ . ,
                                        data = variables_rf_better,
                                        mtry = 5,
                                        importance = TRUE, 
                                        ntree = 1000) 

importance_better <- as.data.frame(randforest_model_better$importance)
importance_better<- rownames_to_column(importance_better,var = "variable")

importance1_better <- ggplot(data = importance_better, aes(x = reorder(variable, MeanDecreaseAccuracy),
                                                           y = MeanDecreaseAccuracy,
                                                           fill = MeanDecreaseAccuracy)) +
  labs(x = "variable", title = "Accuracy reduction - Better") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
importance1_better

importance2_better <- ggplot(data = importance_better, aes(x = reorder(variable, MeanDecreaseGini),
                                                           y = MeanDecreaseGini,
                                                           fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Gini Reduction - better") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
importance2_better

# __Near Zero Variance Analysis #####
stroke_data_better %>% 
  select(-stroke) %>% 
  nearZeroVar(saveMetrics = TRUE)

# _______________________########
# TRAIN and TEST SET ########
# _______________________########

# Creating train and test set #####

# We will use 80% of data to train, and 20% of data to test.
set.seed(1970, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1970)`
test_index <- createDataPartition(y = stroke_data$stroke, times = 1, p = 0.2,
                                  list = FALSE)

# __train_stroke ####
train_stroke <- stroke_data[-test_index,]
str(train_stroke)

# __test_stroke ####
test_stroke <- stroke_data[test_index,]
str(test_stroke)

# Review #####
# Comparing test and train set to be sure that proportions are similar

# __Percent of strokes = 1 in train set ####
mean(train_stroke$stroke == "stroke") 
# 0.04253693

# __Percent of strokes = 1 in test set ####
mean(test_stroke$stroke == "stroke") 
# 0.04257486

test_stroke %>% filter(gender == "Other")

# __Percent of males in train set ####
mean(train_stroke$gender == "Male")
# 0.4146714

# __Percent of females in train set ####
mean(train_stroke$gender == "Female")
# 0.5853286

# __Percent of males in test set ####
mean(test_stroke$gender == "Male")
# 0.3900204

# __Percent of females in test set ####
mean(test_stroke$gender == "Female")
# 0.6099796

# __Distribution of observations by age train #####
train_stroke %>% 
  group_by(age) %>%
  summarise(age = age, total = n(), strokes = sum(stroke == "stroke"),
            stroke_ratio = mean(stroke == "stroke")) %>% 
  #filter(!stroke_ratio == 0) %>% 
  unique() %>%
  ggplot(aes(x=age, y=total)) +
  geom_segment(aes(x=age, xend=age, y=0, yend=total), color="skyblue") +
  geom_point( color="blue", size=2, alpha=0.6) +
  theme_light() +
  #coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Number of Observations by Age") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Distribution of observations by age test #####
test_stroke %>% 
  group_by(age) %>%
  summarise(age = age, total = n(), strokes = sum(stroke == "stroke"),
            stroke_ratio = mean(stroke == "stroke")) %>% 
  #filter(!stroke_ratio == 0) %>% 
  unique() %>%
  ggplot(aes(x=age, y=total)) +
  geom_segment(aes(x=age, xend=age, y=0, yend=total), color="skyblue") +
  geom_point( color="blue", size=2, alpha=0.6) +
  theme_light() +
  #coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Number of Observations by Age") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Percent of strokes by rounded age train ####  
train_stroke %>% 
  group_by(age = round(age, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  ggplot(aes(x=age, y=stroke_percent)) +
  geom_segment(aes(x=age, xend=age, y=0, yend=stroke_percent), color="skyblue") +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  # coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Percent of Strokes by Age") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Percent of strokes by rounded age test ####  
test_stroke %>% 
  group_by(age = round(age, -1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  ggplot(aes(x=age, y=stroke_percent)) +
  geom_segment(aes(x=age, xend=age, y=0, yend=stroke_percent), color="skyblue") +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  # coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Percent of Strokes by Age") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Summary table by age (rounded nearest 10) train #####
# age, total of observations, number of strokes, percent of strokes
n_train <- nrow(train_stroke)

train_stroke %>% 
  group_by(round(age, -1)) %>%
  summarise(total = n(), percent = round(total/n_train, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Summary table by age (rounded nearest 10) test #####
# age, total of observations, number of strokes, percent of strokes
n_test <- nrow(test_stroke)

test_stroke %>% 
  group_by(round(age, -1)) %>%
  summarise(total = n(), percent = round(total/n_test, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Summary table by heart_disease train #####
# heart_disease, total of observations, number of strokes, percent of strokes
train_stroke %>% 
  group_by(heart_disease) %>%
  summarise(total = n(), percent = round(total/n_train, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Summary table by heart_disease train test #####
# heart_disease, total of observations, number of strokes, percent of strokes
test_stroke %>% 
  group_by(heart_disease) %>%
  summarise(total = n(), percent = round(total/n_test, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Summary table by avg_glucose_level (round to nearest ten) train #####
# avg_glucose_level, total of observations, number of strokes, percent of strokes
train_stroke %>% 
  group_by(round(avg_glucose_level, -1)) %>%
  summarise(total = n(), percent = round(total/n_train, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Summary table by avg_glucose_level (round to nearest ten) test #####
# avg_glucose_level, total of observations, number of strokes, percent of strokes
test_stroke %>% 
  group_by(round(avg_glucose_level, -1)) %>%
  summarise(total = n(), percent = round(total/n_test, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Percent of strokes by rounded avg_glucose_level train ####  
train_stroke %>% 
  group_by(avg_glucose_level = round(avg_glucose_level, -1)) %>%
  summarise(total = n(), percent = round(total/n_train, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  ggplot(aes(x=avg_glucose_level, y=stroke_percent)) +
  geom_segment(aes(x=avg_glucose_level, xend=avg_glucose_level, y=0, yend=stroke_percent), color="skyblue") +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  # coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Percent of Strokes by avg_glucose_level (Rounded)") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# __Percent of strokes by rounded avg_glucose_level test ** ####  
test_stroke %>% 
  group_by(avg_glucose_level = round(avg_glucose_level, -1)) %>%
  summarise(total = n(), percent = round(total/n_test, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  ggplot(aes(x=avg_glucose_level, y=stroke_percent)) +
  geom_segment(aes(x=avg_glucose_level, xend=avg_glucose_level, y=0, yend=stroke_percent), color="skyblue") +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  # coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Percent of Strokes by avg_glucose_level (Rounded)") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(plot.margin = unit(c(1,0,1,0), "cm"))

# There are several changes in percentages by avg_glucose level!

# PreProcessing: Centering and Scaling numerical variables ######
#(t stands for transformed)#####

preProcValues <- preProcess(train_stroke, method = c("center", "scale"))

# __train_stroke_t ####
train_stroke_t <- predict(preProcValues, train_stroke)

# __test_stroke_t ####
test_stroke_t <- predict(preProcValues, test_stroke)


# _______________________########
# TRAIN CONTOL  ########
# For Caret Trains ########
# _______________________########

ctrl <- trainControl(method="repeatedcv", 
                     number = 10,
                     repeats = 5, summaryFunction=twoClassSummary, classProbs=T,
                     savePredictions = T) # Parameters ClassProbs and SavePred: needed to plot ROC Curves.


# _______________________########
# ORIGINAL DATA ########
# Imbalanced ########
# _______________________########

# RPART native#######
# Recursive Partitioning and Regression Trees ######

# Gini, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

gini_tree_cp0.01 <- rpart(stroke ~ ., 
                          data = train_stroke_t) # Default rpart tree

plotcp(gini_tree_cp0.01)
print(gini_tree_cp0.01)
summary(gini_tree_cp0.01)

y_hat_gini_tree_cp0.01 <- predict(gini_tree_cp0.01, test_stroke_t, type = "class")

# Model Evaluation :::::::

# Confusion Matrix
confusionMatrix(y_hat_gini_tree_cp0.01,
                test_stroke$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.01,
                test_stroke$stroke)$overall

confusionMatrix(y_hat_gini_tree_cp0.01,
                test_stroke$stroke)$table

cm_gini_tree_cp0.01 <- confusionMatrix(y_hat_gini_tree_cp0.01, test_stroke$stroke)
cm_gini_tree_cp0.01

# F_meas
F_meas(confusionMatrix(y_hat_gini_tree_cp0.01,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_gini_tree_cp0.01 <- predict(gini_tree_cp0.01,
                                test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_roc_gini_tree_cp0.01 <- plot(roc(response = test_stroke_t$stroke, 
                                                predictor = roc_gini_tree_cp0.01$stroke, levels = c("no_stroke", "stroke")), 
                                            print.auc = TRUE,
                                            col = "#377eb8", 
                                            lwd = 2,
                                            xlim = c(1,0),
                                            ylim = c(0,1),
                                            main = "Gini, CP : 0.01")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_roc_gini_tree_cp0.01)

# __Accuracy : 0.9572 ####
# __Sensitivity : 0.0000 ####
# __Specificity : 1.0000 ####
# __Balanced Accuracy : 0.5000 ####
# __F_meas, beta = 1 : NA  ####
# __AUC Sens vs Spec : 0.5 #####

# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
gini_tree_cp0.001 <- rpart(stroke ~., 
                           data = train_stroke_t, 
                           parms=list(split=c("gini")),
                           cp = 0.001)

plotcp(gini_tree_cp0.001)
print(gini_tree_cp0.001)
summary(gini_tree_cp0.001)

y_hat_gini_tree_cp0.001 <- predict(gini_tree_cp0.001, test_stroke_t, type = "class")

# Model Evaluation :::::::

# Confusion Matrix
confusionMatrix(y_hat_gini_tree_cp0.001,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.001,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_gini_tree_cp0.001,
                test_stroke_t$stroke)$table

cm_y_hat_gini_tree_cp0.001 <- confusionMatrix(y_hat_gini_tree_cp0.001, 
                                              test_stroke_t$stroke)
cm_y_hat_gini_tree_cp0.001

# F_Meas
F_meas(confusionMatrix(y_hat_gini_tree_cp0.001,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_gini_tree_cp0.001 <- predict(gini_tree_cp0.001,
                                 test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_gini_tree_cp0.001 <- plot(roc(response = test_stroke_t$stroke, 
                                             predictor = roc_gini_tree_cp0.001$stroke, levels = c("no_stroke", "stroke")), 
                                         print.auc = TRUE,
                                         col = "#377eb8", 
                                         lwd = 2,
                                         xlim = c(1,0),
                                         ylim = c(0,1),
                                         main = "Gini Tree, CP: 0.001")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_gini_tree_cp0.001)

# __Accuracy : 0.9501  ####
# __Sensitivity : 0.023810 ####
# __Specificity : 0.991489 ####
# __Balanced Accuracy : 0.507649 ####
# __F_meas, beta = 1 : 0.03921569 ####
# __AUC Sens vs Spec : 0.7385 #####

# Gini Tree, CP = 0.0024, minslit = 20, minbucket round 20/3, maxdepht = 30 ######
# Same results Gini CP 0.001
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
gini_tree_cp0.0024 <- rpart(stroke ~., 
                            data = train_stroke_t, 
                            parms=list(split=c("gini")),
                            cp = 0.0024)

plotcp(gini_tree_cp0.0024)
print(gini_tree_cp0.0024)
summary(gini_tree_cp0.0024)

y_hat_gini_tree_cp0.0024 <- predict(gini_tree_cp0.0024, test_stroke_t, type = "class")

# Model Evaluation ::::::::

# Confusion Matrix
confusionMatrix(y_hat_gini_tree_cp0.0024,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.0024,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_gini_tree_cp0.0024,
                test_stroke_t$stroke)$table

cm_y_hat_gini_tree_cp0.0024 <- confusionMatrix(y_hat_gini_tree_cp0.0024, test_stroke_t$stroke)
cm_y_hat_gini_tree_cp0.0024

# F_meas
F_meas(confusionMatrix(y_hat_gini_tree_cp0.0024,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_gini_tree_cp0.0024 <- predict(gini_tree_cp0.0024,
                                  test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_gini_tree_cp0.0024 <- plot(roc(response = test_stroke_t$stroke, 
                                              predictor = roc_gini_tree_cp0.0024$stroke, levels = c("no_stroke", "stroke")), 
                                          print.auc = TRUE,
                                          col = "#377eb8", 
                                          lwd = 2,
                                          xlim = c(1,0),
                                          ylim = c(0,1),
                                          main = "Gini Tree, CP: 0.0024")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_gini_tree_cp0.0024)

# __Accuracy : 0.9501 ######
# __Sensitivity : 0.023810  ######
# __Specificity : 0.991489  ######
# __Balanced Accuracy : 0.507649  ######
# __F_meas, beta = 1 : 0.03921569 #######
# __AUC Sens vs Spec : 0.7385 #####

# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 5 ######
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
gini_tree_cp0.001_max5 <- rpart(stroke ~., 
                                data = train_stroke_t, 
                                parms=list(split=c("gini")),
                                cp = 0.001,
                                maxdepth = 5)

plotcp(gini_tree_cp0.001_max5)
print(gini_tree_cp0.001_max5)
summary(gini_tree_cp0.001_max5)

y_hat_gini_tree_cp0.001_max5 <- predict(gini_tree_cp0.001_max5, test_stroke_t, type = "class")

# Model Evaluation :::::::

confusionMatrix(y_hat_gini_tree_cp0.001_max5,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.001_max5,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_gini_tree_cp0.001_max5,
                test_stroke_t$stroke)$table

cm_y_hat_gini_tree_cp0.001_max5 <- confusionMatrix(y_hat_gini_tree_cp0.001_max5, 
                                                   test_stroke_t$stroke)
cm_y_hat_gini_tree_cp0.001_max5

# F_meas
F_meas(confusionMatrix(y_hat_gini_tree_cp0.001_max5,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_gini_tree_cp0.001_max5 <- predict(gini_tree_cp0.001_max5,
                                      test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_gini_tree_cp0.001_max5 <- plot(roc(response = test_stroke_t$stroke, 
                                                  predictor = roc_gini_tree_cp0.001_max5$stroke, levels = c("no_stroke", "stroke")), 
                                              print.auc = TRUE,
                                              col = "#377eb8", 
                                              lwd = 2,
                                              xlim = c(1,0),
                                              ylim = c(0,1),
                                              main = "Gini Tree, CP: 0.001 max5")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_gini_tree_cp0.001_max5)

# __Accuracy : 0.9552   ######
# __Sensitivity : 0.000000 ######
# __Specificity : 0.997872 ######
# __Balanced Accuracy : 0.498936 ######
# __F_meas, beta = 1 : NaN #####
# __AUC Sens vs Spec :  0.7365??????? #####

# Information Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
inf_tree <- (rpart(stroke ~., 
                   data = train_stroke_t, 
                   parms=list(split=c("information"))))

plotcp(inf_tree)
print(inf_tree)
summary(inf_tree)

y_hat_inf_tree <- predict(inf_tree, test_stroke_t, type = "class")

# Model Evaluation ::::::::

confusionMatrix(y_hat_inf_tree,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_inf_tree,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_inf_tree,
                test_stroke_t$stroke)$table

cm_y_hat_inf_tree <- confusionMatrix(y_hat_inf_tree, test_stroke_t$stroke)
cm_y_hat_inf_tree

# F_Meas

F_meas(confusionMatrix(y_hat_inf_tree,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_inf_tree <- predict(inf_tree,
                        test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_inf_tree <- plot(roc(response = test_stroke_t$stroke, 
                                    predictor = roc_inf_tree$stroke, levels = c("no_stroke", "stroke")), 
                                print.auc = TRUE,
                                col = "#377eb8", 
                                lwd = 2,
                                xlim = c(1,0),
                                ylim = c(0,1),
                                main = "Information Tree, CP: 0.01")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_inf_tree)

# __Accuracy : 0.9572  ######
# __Sensitivity : 0.0000  ######
# __Specificity : 1.0000 ######
# __Balanced Accuracy : 0.5000 ######
# __F_meas, beta = 1 : NA #####
# __AUC Sens vs Spec : 0.5 #####

# Information Tree, CP = 0.001. minslit = 20, minbucket round 20/3, maxdepht = 30 #####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
inf_tree_cp0.001 <- rpart(stroke ~., 
                          data = train_stroke_t, 
                          parms=list(split=c("information")),
                          cp = 0.001)

plotcp(inf_tree_cp0.001)
print(inf_tree_cp0.001)
summary(inf_tree_cp0.001)

y_hat_inf_tree_cp0.001 <- predict(inf_tree_cp0.001, test_stroke_t, type = "class")

# Model Evaluation ::::::::

confusionMatrix(y_hat_inf_tree_cp0.001,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_inf_tree_cp0.001,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_inf_tree_cp0.001,
                test_stroke_t$stroke)$table

cm_y_hat_inf_tree_cp0.001 <- confusionMatrix(y_hat_inf_tree_cp0.001, test_stroke_t$stroke)
cm_y_hat_inf_tree_cp0.001

# F_Meas 

F_meas(confusionMatrix(y_hat_inf_tree_cp0.001,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_inf_tree_cp0.001 <- predict(inf_tree_cp0.001,
                                test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_inf_tree_cp0.001 <- plot(roc(response = test_stroke_t$stroke, 
                                            predictor = roc_inf_tree_cp0.001$stroke, levels = c("no_stroke", "stroke")), 
                                        print.auc = TRUE,
                                        col = "#377eb8", 
                                        lwd = 2,
                                        xlim = c(1,0),
                                        ylim = c(0,1),
                                        main = "Information Tree, CP: 0.001")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_inf_tree_cp0.001)

# __Accuracy : 0.9481  #####
# __Sensitivity : 0.023810   #####
# __Specificity : 0.989362   #####
# __Balanced Accuracy : 0.506586  #####
# __F_meas, beta = 1 : 0.03773585 #####
# __AUC Sens vs Spec :  0.7733 #####

# Information Tree, CP = 0.0023. minslit = 20, minbucket round 20/3, maxdepht = 30 #####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
inf_tree_cp0.0023 <- rpart(stroke ~., 
                           data = train_stroke_t, 
                           parms=list(split=c("information")),
                           cp = 0.0023)

plotcp(inf_tree_cp0.0023)
print(inf_tree_cp0.0023)
summary(inf_tree_cp0.0023)

y_hat_inf_tree_cp0.0023 <- predict(inf_tree_cp0.0023, test_stroke_t, type = "class")

# Model Evaluation ::::::::

confusionMatrix(y_hat_inf_tree_cp0.0023,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_inf_tree_cp0.0023,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_inf_tree_cp0.0023,
                test_stroke_t$stroke)$table

cm_y_hat_inf_tree_cp0.0023 <- confusionMatrix(y_hat_inf_tree_cp0.0023, test_stroke_t$stroke)
cm_y_hat_inf_tree_cp0.0023

# F_Meas

F_meas(confusionMatrix(y_hat_inf_tree_cp0.0023,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_inf_tree_cp0.0023 <- predict(inf_tree_cp0.0023,
                                 test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_inf_tree_cp0.0023 <- plot(roc(response = test_stroke_t$stroke, 
                                             predictor = roc_inf_tree_cp0.0023$stroke, levels = c("no_stroke", "stroke")), 
                                         print.auc = TRUE,
                                         col = "#377eb8", 
                                         lwd = 2,
                                         xlim = c(1,0),
                                         ylim = c(0,1),
                                         main = "Information Tree, CP: 0.001")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_inf_tree_cp0.0023)

# __Accuracy : 0.9501  #####
# __Sensitivity : 0.023810   #####        
# __Specificity : 0.991489 #####
# __Balanced Accuracy : 0.507649  #####
# __F_meas, beta = 1 : 0.03921569 #####
# __AUC Sens vs Spec : 0.7865 #####


# RPART caret ######
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_caret_tree <- train(stroke ~ ., method = "rpart", 
                          data = train_stroke_t, 
                          trControl = ctrl, 
                          metric = "ROC")

plot(train_caret_tree)
print(train_caret_tree)
summary(train_caret_tree)

y_hat_caret_tree <- predict(train_caret_tree, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_caret_tree, test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_caret_tree, test_stroke_t$stroke)$table

cm_caret_tree <- confusionMatrix(y_hat_caret_tree, test_stroke_t$stroke)
cm_caret_tree

# F_Meas

F_meas(confusionMatrix(y_hat_caret_tree,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_caret_tree <- predict(train_caret_tree,
                          test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_caret_tree <- plot(roc(response = test_stroke_t$stroke, 
                                      predictor = roc_caret_tree$stroke, levels = c("no_stroke", "stroke")), 
                                  print.auc = TRUE,
                                  col = "#377eb8", 
                                  lwd = 2,
                                  xlim = c(1,0),
                                  ylim = c(0,1),
                                  main = "CARET Tree")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_caret_tree)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_caret_tree <- evalm(list(train_caret_tree), 
                         positive = "stroke",
                         gnames=c('CARET Tree'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_caret_tree <- evalm(list(train_caret_tree), 
                       positive = "stroke",
                       plots = "cc",
                       title = "Calibration Curve",
                       gnames=c('CARET Tree'))

# Precision - Recall Gain
prg_caret_tree <- evalm(list(train_caret_tree), 
                        positive = "stroke",
                        plots = "prg",
                        title = "Precision - Recall Gain",
                        gnames=c('CARET Tree'))

# Precision - Recall Curve
pr_caret_tree <- evalm(list(train_caret_tree), 
                       positive = "stroke",
                       plots = "pr",
                       title = "Precision - Recall Curve",
                       gnames=c('CARET Tree'))

# True positive vs false positive ROC
roc_tp_fp_caret_tree <- evalm(list(train_caret_tree), 
                              positive = "stroke",
                              plots = "r",
                              title = "True Positive - False Positive rate ROC",
                              gnames=c('CARET Tree'))

# __Accuracy : 0.9521   ######
# __Sensitivity : 0.023810  #####         
# __Specificity : 0.993617 #####
# __Balanced Accuracy : 0.508713  #####
# __F_meas, beta : 0.04081633  #####
# __AUC Sens vs Spec :  0.7394 #####

# KNN ########
# K-Nearest-Neighbor #######
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_knn <- train(stroke ~ ., method = "knn", 
                   data = train_stroke_t, 
                   trControl = ctrl, 
                   metric = "ROC")

plot(train_knn)
print(train_knn)
summary(train_knn)

y_hat_knn <- predict(train_knn, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_knn,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_knn,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_knn,
                test_stroke_t$stroke)$table

cm_knn <- confusionMatrix(y_hat_knn, test_stroke_t$stroke)
cm_knn

# F_Meas

F_meas(confusionMatrix(y_hat_knn,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_knn <- predict(train_knn,
                   test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_knn <- plot(roc(response = test_stroke_t$stroke, 
                               predictor = roc_knn$stroke, levels = c("no_stroke", "stroke")), 
                           print.auc = TRUE,
                           col = "#377eb8", 
                           lwd = 2,
                           xlim = c(1,0),
                           ylim = c(0,1),
                           main = "KNN")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_knn)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_knn <- evalm(list(train_knn), 
                  positive = "stroke",
                  gnames=c('KNN'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_knn <- evalm(list(train_knn), 
                positive = "stroke",
                plots = "cc",
                title = "Calibration Curve",
                gnames=c('KNN'))

# Precision - Recall Gain
prg_knn <- evalm(list(train_knn), 
                 positive = "stroke",
                 plots = "prg",
                 title = "Precision - Recall Gain",
                 gnames=c('KNN'))

# Precision - Recall Curve
pr_knn <- evalm(list(train_knn), 
                positive = "stroke",
                plots = "pr",
                title = "Precision - Recall Curve",
                gnames=c('KNN'))

# True positive vs false positive ROC
roc_knn <- evalm(list(train_knn), 
                 positive = "stroke",
                 plots = "r",
                 title = "True Positive - False Positive rate ROC",
                 gnames=c('KNN'))


# __Accuracy : 0.9572  ####
# __Sensitivity :  0.000000  ####        
# __Specificity : 1.00000 ####
# __Balanced Accuracy : 0.50000  ####
# __F_meas, beta = 1 : NA #####
# __AUC Sens vs Spec : 0.6206 #####


# RANDOM FOREST ########
# it takes time! ######
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_rf <- train(stroke ~ ., method = "rf", 
                  data = train_stroke_t, 
                  trControl = ctrl, 
                  metric = "ROC")

plot(train_rf)
print(train_rf)
summary(train_rf)

y_hat_rf <- predict(train_rf, test_stroke_t, type = "raw")

# Model Evaluation :::::::: 

confusionMatrix(y_hat_rf, test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_rf, test_stroke_t$stroke)$table

cm_rf <- confusionMatrix(y_hat_rf, test_stroke_t$stroke)
cm_rf

# F_Meas

F_meas(confusionMatrix(y_hat_rf,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_rf <- predict(train_rf,
                  test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_rf <- plot(roc(response = test_stroke_t$stroke, 
                              predictor = roc_rf$stroke, levels = c("no_stroke", "stroke")), 
                          print.auc = TRUE,
                          col = "#377eb8", 
                          lwd = 2,
                          xlim = c(1,0),
                          ylim = c(0,1),
                          main = "Random Forest")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_rf)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_rf <- evalm(list(train_rf), 
                 positive = "stroke",
                 gnames=c('Random Forest'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_rfr <- evalm(list(train_rf), 
                positive = "stroke",
                plots = "cc",
                title = "Calibration Curve",
                gnames=c('Random Forest'))

# Precision - Recall Gain
prg_train_rf <- evalm(list(train_rf), 
                      positive = "stroke",
                      plots = "prg",
                      title = "Precision - Recall Gain",
                      gnames=c('Random Forest'))

# Precision - Recall Curve
pr_rf <- evalm(list(train_rf), 
               positive = "stroke",
               plots = "pr",
               title = "Precision - Recall Curve",
               gnames=c('Random Forest'))

# True positive vs false positive ROC
roc_tp_fp_rf <- evalm(list(train_rf), 
                      positive = "stroke",
                      plots = "r",
                      title = "True Positive - False Positive rate ROC",
                      gnames=c('Random Forest'))



# __Accuracy : 0.9582   ####
# __Sensitivity : 0.023810      ####     
# __Specificity : 1.000000   ####
# __Balanced Accuracy : 0.511905  ####
# __F_meas, beta = 1 : 0.04651163 #####
# __AUC Sens vs Spec :  0.7987 ???? #####


# FDA #####
# Flexible Discriminant Analysis #####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_fda <- train(stroke ~ ., method = "fda", 
                   data = train_stroke_t, 
                   trControl = ctrl, 
                   metric = "ROC")

plot(train_fda)
print(train_fda)
summary(train_fda)

y_hat_fda <- predict(train_fda, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_fda,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_fda,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_fda,
                test_stroke_t$stroke)$table

cm_fda <- confusionMatrix(y_hat_fda, test_stroke_t$stroke)
cm_fda

# F_Meas

F_meas(confusionMatrix(y_hat_fda,
                       test_stroke_t$stroke)$table, beta = 1)


# Calc. Probs for every class
roc_fda <- predict(train_fda,
                   test_stroke_t, type = "prob")

# Curve graph
sens_espec_roc_fda <- plot(roc(response = test_stroke_t$stroke, 
                               predictor = roc_fda$stroke, levels = c("no_stroke", "stroke")), 
                           print.auc = TRUE,
                           xlim = c(1,0),
                           ylim = c(0,1),
                           xlab = "Specificity", 
                           ylab ="Sensibility", 
                           main = "FDA")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_fda)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_fda <- evalm(list(train_fda), 
                  positive = "stroke",
                  gnames=c('FDA'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_fda <- evalm(list(train_fda), 
                positive = "stroke",
                plots = "cc",
                title = "Calibration Curve",
                gnames=c('FDA'))

# Precision - Recall Gain
prg_fda <- evalm(list(train_fda), 
                 positive = "stroke",
                 plots = "prg",
                 title = "Precision - Recall Gain",
                 gnames=c('FDA'))

# Precision - Recall Curve
pr_fda <- evalm(list(train_fda), 
                positive = "stroke",
                plots = "pr",
                title = "Precision - Recall Curve",
                gnames=c('FDA'))

# True positive vs false positive ROC
roc_tp_fp_fda <- evalm(list(train_fda), 
                       positive = "stroke",
                       plots = "r",
                       title = "Precision - Recall Curve",
                       gnames=c('FDA'))

# __Accuracy : 0.9338  ####
# __Sensitivity : 0.14286 ####        
# __Specificity : 0.96915   ####  
# __Balanced Accuracy : 0.55600    ####
# __F_meas, beta = 1 : 0.1558442 #####
# # __AUC Sens vs Spec : 0.857 ###### 

# _______________________########
# BALANCED DATA ########
# _______________________######## 

# _______________________######## 
# Oversampling #####
# _______________________######## 

n_over = sum(train_stroke_t$stroke == "no_stroke")

set.seed(1969, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1969)`
# Every time we run the code, we get a different ovun.sample
# Visualization tree, Accuracy, Sensibility, Balanced accuracy and F.meas strongly depends on this ramdom process.

train_stroke_over <- ovun.sample(stroke ~ ., data = train_stroke_t, method = "over", N = n_over*2)$data

str(train_stroke_over)

# Relevel "stroke" "no_stroke" factors: positive class: "stroke" #### 
train_stroke_over$stroke <- relevel(train_stroke_over$stroke, ref = "stroke")

# Strokes proportion #####
table(train_stroke_over$stroke) %>% 
  kable()


# RPART native #######
# Recursive Partitioning and Regression Trees ######

# Gini Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
gini_tree_cp0.01_over <- rpart(stroke ~ ., 
                               data = train_stroke_over)


plotcp(gini_tree_cp0.01_over)
print(gini_tree_cp0.01_over)
summary(gini_tree_cp0.01_over)

y_hat_gini_tree_cp0.01_over <- predict(gini_tree_cp0.01_over, test_stroke_t, 
                                       type = "class")
# Model Evaluation ::::::::

confusionMatrix(y_hat_gini_tree_cp0.01_over,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.01_over,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_gini_tree_cp0.01_over,
                test_stroke_t$stroke)$table

cm_gini_tree_cp0.01_over <- confusionMatrix(y_hat_gini_tree_cp0.01_over, 
                                            test_stroke_t$stroke)
cm_gini_tree_cp0.01_over

# F_Meas   

F_meas(confusionMatrix(y_hat_gini_tree_cp0.01_over,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_gini_tree_cp0.01_over <- predict(gini_tree_cp0.01_over,
                                     test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_roc_gini_tree_cp0.01_over <- plot(roc(response = test_stroke_t$stroke, 
                                           predictor = roc_gini_tree_cp0.01_over$stroke, levels = c("stroke", "no_stroke")), 
                                       print.auc = TRUE,
                                       xlim = c(1,0),
                                       ylim = c(0,1),
                                       xlab = "Specificity", 
                                       ylab ="Sensibility", 
                                       main = "Gini Tree, CP: 0.01 - Over")

# Sensitivity vs Specificity AUC
auc(sens_roc_gini_tree_cp0.01_over)

# __Accuracy : 0.7566  ####
# __Sensitivity : 0.73810   ####         
# __Specificity : 0.75745  ####
# __Balanced Accuracy : 0.74777  ####
# __F_meas, beta = 1 : 0.2059801   ####
# __AUC Sens vs Spec : 0.7372 ######

# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
gini_tree_cp0.001_over <- rpart(stroke ~., 
                                data = train_stroke_over, 
                                parms=list(split=c("gini")),
                                cp = 0.001)

plotcp(gini_tree_cp0.001_over)
print(gini_tree_cp0.001_over)
summary(gini_tree_cp0.001_over)

y_hat_gini_tree_cp0.001_over <- predict(gini_tree_cp0.001_over, test_stroke_t, 
                                        type = "class")

# Model Evaluation ::::::::

confusionMatrix(y_hat_gini_tree_cp0.001_over,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.001_over,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_gini_tree_cp0.001_over,
                test_stroke_t$stroke)$table

cm_gini_tree_cp0.001_over <- confusionMatrix(y_hat_gini_tree_cp0.001_over, 
                                             test_stroke_t$stroke)
cm_gini_tree_cp0.001_over

# F_Meas

F_meas(confusionMatrix(y_hat_gini_tree_cp0.001_over,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_gini_tree_cp0.001_over <- predict(gini_tree_cp0.001_over,
                                      test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_gini_tree_cp0.001_over <- plot(roc(response = test_stroke_t$stroke, 
                                                  predictor = roc_gini_tree_cp0.001_over$stroke, levels = c("stroke", "no_stroke")), 
                                              print.auc = TRUE,
                                              xlim = c(1,0),
                                              ylim = c(0,1),
                                              xlab = "Specificity", 
                                              ylab ="Sensibility", 
                                              main = "Gini Tree, CP:001 - Over")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_gini_tree_cp0.001_over)

# __Accuracy : 0.8697  ####
# __Sensitivity : 0.47619 ####       
# __Specificity : 0.88723 ####
# __Balanced Accuracy : 0.68171 ####
# __F_meas, beta = 1 : 0.2380952 ####
# __AUC Sens vs Spec : 0.3212 ######

# Default Gini Tree & Cost Matrix 3 to 1 #####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
cost_matrix_tree_over <- rpart(stroke ~ ., 
                               data = train_stroke_over,
                               parms=list(
                                 loss=matrix(c(0,3,1,0), # A false negative is 3 times worse than a false positive
                                             byrow=TRUE,
                                             nrow=2)))

plotcp(cost_matrix_tree_over)
print(cost_matrix_tree_over)
summary(cost_matrix_tree_over)

y_hat_cost_matrix_tree_over <- predict(cost_matrix_tree_over, test_stroke_t, 
                                       type = "class")

# Model Evaluation ::::::::

confusionMatrix(y_hat_cost_matrix_tree_over,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_cost_matrix_tree_over,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_cost_matrix_tree_over,
                test_stroke_t$stroke)$table

cm_cost_matrix_tree_over <- confusionMatrix(y_hat_cost_matrix_tree_over, 
                                            test_stroke_t$stroke)
cm_cost_matrix_tree_over

# F_Meas

F_meas(confusionMatrix(y_hat_cost_matrix_tree_over,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_cost_matrix_tree_over <- predict(cost_matrix_tree_over,
                                     test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_cost_matrix_tree_over <- plot(roc(response = test_stroke_t$stroke, 
                                                 predictor = roc_cost_matrix_tree_over$stroke, levels = c("stroke", "no_stroke")), 
                                             print.auc = TRUE,
                                             xlim = c(1,0),
                                             ylim = c(0,1),
                                             xlab = "Specificity", 
                                             ylab ="Sensibility", 
                                             main = "Default Gini Tree & Cost Matrix 3 to 1 - Over")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_cost_matrix_tree_over)

# __Accuracy : 0.6426   ##### 
# __Sensitivity : 0.90476!!!! #####         
# __Specificity : 0.63085 #####  
# __Balanced Accuracy : 0.76781 #####
# __F_meas, beta = 1 : 0.1779859 #####
# __AUC Sens vs Spec : 0.7525 ######


# RPART caret ####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_caret_tree_over <- train(stroke ~ ., method = "rpart",
                               data = train_stroke_over,
                               trControl = ctrl,
                               metric="ROC")

plot(train_caret_tree_over)
print(train_caret_tree_over)
summary(train_caret_tree_over)

y_hat_caret_tree_over <- predict(train_caret_tree_over, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_caret_tree_over, 
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_caret_tree_over, 
                test_stroke_t$stroke)$table

cm_caret_tree_over <- confusionMatrix(y_hat_caret_tree_over, test_stroke_t$stroke)
cm_caret_tree_over

# F_Meas

F_meas(confusionMatrix(y_hat_caret_tree_over, 
                       test_stroke_t$stroke)$table, beta = 1, estimator = "binary")

# Calc. Probs for every class
roc_caret_tree_over <- predict(train_caret_tree_over,
                               test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_caret_tree_over <- plot(roc(response = test_stroke_t$stroke, 
                                           predictor = roc_caret_tree_over$stroke, levels = c("stroke", "no_stroke")), 
                                       print.auc = TRUE,
                                       xlim = c(1,0),
                                       ylim = c(0,1),
                                       xlab = "Specificity", 
                                       ylab ="Sensibility", 
                                       main = "RPART caret - Over")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_caret_tree_over)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_caret_tree_over <- evalm(list(train_caret_tree_over), 
                              positive = "stroke",
                              gnames=c('CARET Tree - Over'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_caret_tree_over <- evalm(list(train_caret_tree_over), 
                            positive = "stroke",
                            plots = "cc",
                            title = "Calibration Curve",
                            gnames=c('CARET Tree - Over'))

# Precision - Recall Gain
prg_caret_tree_over <- evalm(list(train_caret_tree_over), 
                             positive = "stroke",
                             plots = "prg",
                             title = "Precision - Recall Gain",
                             gnames=c('CARET Tree - Over'))

# Precision - Recall Curve
pr_caret_tree_over <- evalm(list(train_caret_tree_over), 
                            positive = "stroke",
                            plots = "pr",
                            title = "Precision - Recall Curve",
                            gnames=c('CARET Tree - Over'))

# True positive vs false positive ROC
roc_tp_fp_caret_tree_over <- evalm(list(train_caret_tree_over), 
                                   positive = "stroke",
                                   plots = "r",
                                   title = "True Positive - False Positive rate ROC",
                                   gnames=c('CARET Tree - Over'))

# __Accuracy : 0.7739   ####
# __Sensitivity : 0.73810      ####        
# __Specificity : 0.77553  ####
# __Balanced Accuracy : 0.75681   ####
# __F_meas, beta = 1 : 0.2183099  #####
# __AUC Sens vs Spec : 0.8042 ######


#KNN caret #######
# K-Nearest-Neighbor #######
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_knn_over <- train(stroke ~ ., method = "knn", 
                        data = train_stroke_over, 
                        trControl = ctrl,
                        metric="ROC")

plot(train_knn_over)
print(train_knn_over)
summary(train_knn_over)

y_hat_knn_over <- predict(train_knn_over, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_knn_over,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_knn_over,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_knn_over,
                test_stroke_t$stroke)$table

cm_knn_over <- confusionMatrix(y_hat_knn_over, test_stroke_t$stroke)
cm_knn_over

# F_Meas

F_meas(confusionMatrix(y_hat_knn_over,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_knn_over <- predict(train_knn_over,
                        test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_knn_over <- plot(roc(response = test_stroke_t$stroke, 
                                    predictor = roc_knn_over$stroke, levels = c("stroke", "no_stroke")), 
                                print.auc = TRUE,
                                xlim = c(1,0),
                                ylim = c(0,1),
                                xlab = "Specificity", 
                                ylab ="Sensibility", 
                                main = "KNN - Over")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_knn_over)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate ?????? Very strange results!
eval_knn_over <- evalm(list(train_knn_over), 
                       positive = "stroke",
                       gnames=c('KNN - Over'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_knn_over <- evalm(list(train_knn_over), 
                     positive = "stroke",
                     plots = "cc",
                     title = "Calibration Curve",
                     gnames=c('KNN - Over'))

# Precision - Recall Gain
prg_knn_over <- evalm(list(train_knn_over), 
                      positive = "stroke",
                      plots = "prg",
                      title = "Precision - Recall Gain",
                      gnames=c('KNN - Over'))

# Precision - Recall Curve
pr_knn_over <- evalm(list(train_knn_over), 
                     positive = "stroke",
                     plots = "pr",
                     title = "Precision - Recall Curve",
                     gnames=c('KNN - Over'))

# True positive vs false positive ROC
roc_tp_fp_knn_over <- evalm(list(train_knn_over), 
                            positive = "stroke",
                            plots = "r",
                            title = "True Positive - False Positive rate ROC",
                            gnames=c('KNN - Over'))

# __Accuracy : 0.7607   ##### 
# __Sensitivity : 0.42857  #####         
# __Specificity : 0.77553  ##### 
# __Balanced Accuracy : 0.60205   #####
# __F_meas, beta = 1 : 0.1328413 #####
# __AUC Sens vs Spec : 0.4015 ######

# RANDOM FOREST ########
# it takes time! ######
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_rf_over <- train(stroke ~ ., method = "rf", 
                       data = train_stroke_over, 
                       trControl = ctrl,
                       metric="ROC")

plot(train_rf_over)
print(train_rf_over)
summary(train_rf_over)

ggplot(train_rf_over, highlight = TRUE) 

y_hat_rf_over <- predict(train_rf_over, test_stroke_t, type = "raw")

# Model Evaluation ::::::::

confusionMatrix(y_hat_rf_over, test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_rf_over, test_stroke_t$stroke)$table

cm_rf_over <- confusionMatrix(y_hat_rf_over, test_stroke_t$stroke)
cm_rf_over

# F_Meas 

F_meas(confusionMatrix(y_hat_rf_over, test_stroke_t$stroke)$table, 
       reference = Reference, beta = 1)


# Calc. Probs for every class
roc_rf_over <- predict(train_rf_over,
                       test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_rf_over <- plot(roc(response = test_stroke_t$stroke, 
                                   predictor = roc_rf_over$stroke, levels = c("stroke", "no_stroke")), 
                               print.auc = TRUE,
                               xlim = c(1,0),
                               ylim = c(0,1),
                               xlab = "Specificity", 
                               ylab ="Sensibility", 
                               main = "Random Forest - Over")

# Sensitivity vs Specificity ROC CURVE
auc(sens_espec_roc_rf_over)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate ?????? Very strange results!
eval_rf_over <- evalm(list(train_rf_over), 
                      positive = "stroke",
                      gnames=c('Random Forest - Over'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_rf_over <- evalm(list(train_rf_over), 
                    positive = "stroke",
                    plots = "cc",
                    title = "Calibration Curve",
                    gnames=c('Random Forest - Over'))

# Precision - Recall Gain
prg_rf_over <- evalm(list(train_rf_over), 
                     positive = "stroke",
                     plots = "prg",
                     title = "Precision - Recall Gain",
                     gnames=c('Random Forest - Over'))

# Precision - Recall Curve
pr_rf_over <- evalm(list(train_rf_over), 
                    positive = "stroke",
                    plots = "pr",
                    title = "Precision - Recall Curve",
                    gnames=c('Random Forest - Over'))

# True positive vs false positive ROC
roc_tp_fp_rf_over <- evalm(list(train_rf_over), 
                           positive = "stroke",
                           plots = "r",
                           title = "True Positive - False Positive rate ROC",
                           gnames=c('Random Forest - Over'))

# __Accuracy : 0.947   ##### 
# __Sensitivity : 0.071429  #####         
# __Specificity : 0.986170    ##### 
# __Balanced Accuracy : 0.528799  ##### 
# __F_meas, beta = 1 : 0.1034483 #####
# # __AUC Sens vs Spec : 0.7827 ??????? ###### 

# MDA: TRAIN ERROR ######
# Mixture Discriminant Analysis ####
# set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
# Results are random variables

# train_mda_over <- train(stroke ~ ., method = "mda", 
# data = train_stroke_over,
# trControl = ctrl,
# metric="ROC") # Several Warnings

# plot(train_mda_over)
# print(train_mda_over)
# summary(train_mda_over)

# y_hat_mda_over <- predict(train_mda_over, test_stroke_t)

# Model Evaluation ::::::::

# confusionMatrix(y_hat_mda_over,
# test_stroke_t$stroke)$overall["Accuracy"]

# confusionMatrix(y_hat_mda_over,
# test_stroke$stroke)$overall

# confusionMatrix(y_hat_mda_over,
# test_stroke_t$stroke)$table

# cm_mda_over <- confusionMatrix(y_hat_mda_over, test_stroke_t$stroke, 
# positive = "stroke")
# cm_mda_over

# F_Meas

# F_meas(confusionMatrix(y_hat_mda_over,
# test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
# roc_mda_over <- predict(train_mda_over,
# test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
# sens_espec_roc_mda_over <-plot(roc(response = test_stroke_t$stroke, 
# predictor = roc_mda_over $stroke, levels = c("stroke", "no_stroke")), 
# print.auc = TRUE,
#  xlim = c(1,0),
#  ylim = c(0,1),
#  xlab = "Specificity", 
#  ylab ="Sensibility", 
#  main = "MDA - Over")

# Sensitivity vs Specificity AUC
# auc(sens_espec_roc_mda_over)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
# eval_mda_over <- evalm(list(train_mda_over), 
# positive = "stroke",
#  gnames=c('MDA - Over')) 

# ERROR
# Error in names(x) <- value : 
# el atributo 'names' [1] debe tener la misma longitud que el vector [0]
# Además: Warning messages:
# 1: Removed 7520 row(s) containing missing values (geom_path). 
# 2: Removed 1 rows containing missing values (geom_segment). 
# 3: Removed 7520 row(s) containing missing values (geom_path). 
# 4: Removed 7520 row(s) containing missing values (geom_path). 


# Calibration Curve (True prob. vs Predicted prob.)
# cc_mda_over <- evalm(list(train_mda_over), 
# positive = "stroke",
# plots = "cc",
# title = "Calibration Curve",
# gnames=c('MDA - Over'))

# Precision - Recall Gain
# prg_mda_over <- evalm(list(train_mda_over), 
# positive = "stroke",
# plots = "prg",
# title = "Precision - Recall Gain",
# gnames=c('MDA - Over'))

# Precision - Recall Curve
# pr_mda_over <- evalm(list(train_mda_over), 
# positive = "stroke",
# plots = "pr",
# title = "Precision - Recall Curve",
# gnames=c('MDA - Over'))

# True positive vs false positive ROC
# roc_tp_fp_mda_over <- evalm(list(train_mda_over), 
# positive = "stroke",
# plots = "r",
# title = "True Positive - False Positive rate ROC",
# gnames=c('MDA - Over'))

# NNET: great variability !!!#####
# Neural Network ####
# It takes some time!

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_nnet_over <- train(stroke ~ ., method = "nnet",
                         data = train_stroke_over,
                         trControl = ctrl,
                         trace = FALSE,
                         metric="ROC")

plot(train_nnet_over)
print(train_nnet_over)
summary(train_nnet_over)

y_hat_nnet_over <- predict(train_nnet_over, test_stroke_t, type = "raw")

# Model Evaluation ::::::::

confusionMatrix(as.factor(y_hat_nnet_over),
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(as.factor(y_hat_nnet_over),
                test_stroke_t$stroke)$overall

confusionMatrix(as.factor(y_hat_nnet_over),
                test_stroke_t$stroke)$table

cm_nnet_over <- confusionMatrix(as.factor(y_hat_nnet_over), test_stroke_t$stroke)
cm_nnet_over

# F_Meas

F_meas(confusionMatrix(as.factor(y_hat_nnet_over),
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_nnet_over <- predict(train_nnet_over,
                         test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_nnet_over <- plot(roc(response = test_stroke_t$stroke, 
                                     predictor = roc_nnet_over$stroke, levels = c("no_stroke", "stroke")), 
                                 print.auc = TRUE,
                                 xlim = c(1,0),
                                 ylim = c(0,1),
                                 xlab = "Specificity", 
                                 ylab ="Sensibility", 
                                 main = "NNET - Over")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_nnet_over)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_nnet_over <- evalm(list(train_nnet_over), 
                        positive = "stroke",
                        gnames=c('NNET - Over'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_nnet_over <- evalm(list(train_nnet_over), 
                      positive = "stroke",
                      plots = "cc",
                      title = "Calibration Curve",
                      gnames=c('NNET - Over'))

# Precision - Recall Gain
prg_nnet_over <- evalm(list(train_nnet_over), 
                       positive = "stroke",
                       plots = "prg",
                       title = "Precision - Recall Gain",
                       gnames=c('NNET - Over'))

# Precision - Recall Curve
pr_nnet_over <- evalm(list(train_nnet_over), 
                      positive = "stroke",
                      plots = "pr",
                      title = "Precision - Recall Curve",
                      gnames=c('NNET - Over'))

# True positive vs false positive ROC
roc_tp_fp_nnet_over <- evalm(list(train_nnet_over), 
                             positive = "stroke",
                             plots = "r",
                             title = "True Positive - False Positive rate ROC",
                             gnames=c('NNET - Over'))

# __Accuracy : 0.777  ####
# __Sensitivity : 0.76190 ####        
# __Specificity : 0.77766  ####
# __Balanced Accuracy : 0.76978   ####
# __F_meas, beta = 1 : 0.2261484 #####
# __AUC Sens vs Spec 0.8095 ###### 

# FDA #####
# Flexible Discriminant Analysis #####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_fda_over <- train(stroke ~ ., method = "fda", 
                        data = train_stroke_over, 
                        trControl = ctrl,
                        metric="ROC")

plot(train_fda_over)
print(train_fda_over)
summary(train_fda_over)

y_hat_fda_over <- predict(train_fda_over, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_fda_over,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_fda_over,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_fda_over,
                test_stroke_t$stroke)$table

cm_fda_over <- confusionMatrix(y_hat_fda_over, test_stroke_t$stroke)
cm_fda_over

# F_Meas

F_meas(confusionMatrix(y_hat_fda_over,
                       test_stroke_t$stroke)$table, beta = 1)


# Calc. Probs for every class
roc_fda_over <- predict(train_fda_over,
                        test_stroke_t, type = "prob")

# Curve graph
sens_espec_roc_fda_over <- plot(roc(response = test_stroke_t$stroke, 
                                    predictor = roc_fda_over$stroke, levels = c("no_stroke", "stroke")), 
                                print.auc = TRUE,
                                xlim = c(1,0),
                                ylim = c(0,1),
                                xlab = "Specificity", 
                                ylab ="Sensibility", 
                                main = "Flexible Discriminant Analysis - Over")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_fda_over)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_fda_over <- evalm(list(train_fda_over), 
                       positive = "stroke",
                       gnames=c('FDA - OVER'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_fda_over <- evalm(list(train_fda_over), 
                     positive = "stroke",
                     plots = "cc",
                     title = "Calibration Curve",
                     gnames=c('FDA - OVER'))

# Precision - Recall Gain
prg_fda_over <- evalm(list(train_fda_over), 
                      positive = "stroke",
                      plots = "prg",
                      title = "Precision - Recall Gain",
                      gnames=c('FDA - OVER'))

# Precision - Recall Curve
pr_fda_over <- evalm(list(train_fda_over), 
                     positive = "stroke",
                     plots = "pr",
                     title = "Precision - Recall Curve",
                     gnames=c('FDA - OVER'))

# True positive vs false positive ROC
roc_tp_fp_fda_over <- evalm(list(train_fda_over), 
                            positive = "stroke",
                            plots = "r",
                            title = "True Positive - False Positive rate ROC",
                            gnames=c('FDA - OVER'))

# __Accuracy : 0.7454 ####
# __Sensitivity : 0.73810 ####        
# __Specificity : 0.74574 ####  
# __Balanced Accuracy : 0.74192 ####
# __F_meas, beta = 1 : 0.1987179 #####
# __AUC Sens vs Spec : 0.8163 ###### 


# NAIVE BAYES ######
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_naiveBayes_over <- train(stroke ~ ., method = "naive_bayes", 
                               data = train_stroke_over, 
                               trControl = ctrl,
                               metric="ROC")

plot(train_naiveBayes_over)
print(train_naiveBayes_over)
summary(train_naiveBayes_over)

y_hat_naiveBayes_over <- predict(train_naiveBayes_over, test_stroke_t)


# Model Evaluation ::::::::

confusionMatrix(y_hat_naiveBayes_over,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_naiveBayes_over,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_naiveBayes_over,
                test_stroke_t$stroke)$table



cm_naiveBayes_over <- confusionMatrix(y_hat_naiveBayes_over, test_stroke_t$stroke)
cm_naiveBayes_over

# F_Meas

F_meas(confusionMatrix(y_hat_naiveBayes_over,
                       test_stroke_t$stroke)$table,beta = 1)


# Calc. Probs for every class
roc_naiveBayes_over <- predict(train_naiveBayes_over,
                               test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_naiveBayes_over <- plot(roc(response = test_stroke_t$stroke, 
                                           predictor = roc_naiveBayes_over$stroke, levels = c("no_stroke", "stroke")), 
                                       print.auc = TRUE,
                                       xlim = c(1,0),
                                       ylim = c(0,1),
                                       xlab = "Specificity", 
                                       ylab ="Sensibility", 
                                       main = "Naive Bayes - Over")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_naiveBayes_over)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_naiveBayes_over <- evalm(list(train_naiveBayes_over), 
                              positive = "stroke",
                              gnames=c('Naive Bayes - Over'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_naiveBayes_over <- evalm(list(train_naiveBayes_over), 
                            positive = "stroke",
                            plots = "cc",
                            title = "Calibration Curve",
                            gnames=c('Naive Bayes - Over'))

# Precision - Recall Gain
prg_naiveBayes_over <- evalm(list(train_naiveBayes_over), 
                             positive = "stroke",
                             plots = "prg",
                             title = "Precision - Recall Gain",
                             gnames=c('Naive Bayes - Over'))

# Precision - Recall Curve
pr_naiveBayes_over <- evalm(list(train_naiveBayes_over), 
                            positive = "stroke",
                            plots = "pr",
                            title = "Precision - Recall Curve",
                            gnames=c('Naive Bayes - Over'))

# True positive vs false positive ROC
roc_tp_fp_naiveBayes_over <- evalm(list(train_naiveBayes_over), 
                                   positive = "stroke",
                                   plots = "r",
                                   title = "True Positive - False Positive rate ROC",
                                   gnames=c('Naive Bayes - Over'))


# __Accuracy : 0.9348  ##### 
# __Sensitivity : 0.26190   #####         
# __Specificity : 0.96489 ##### 
# __Balanced Accuracy : 0.61340   ##### 
# __F_meas, beta = 1 : 0.255814 #####
# __AUC : 0.8489 ???? ######

# _______________________######## 
# Oversamplig and Undersampling: both #####
# _______________________######## 

table(train_stroke$stroke)
prop.table(table(train_stroke$stroke))

n_both = sum(train_stroke$stroke == "stroke") + sum(train_stroke$stroke == "no_stroke")

set.seed(1969, sample.kind="Rounding") # Every time we run the code, we get a different ovun.sample
# Accuracy and balanced accuracy strongly depends on this ramdom process.

train_stroke_both <- ovun.sample(stroke ~ ., data = train_stroke_t, method = "both", p = 0.5, N = n_both)$data
table(train_stroke_both$stroke)

str(train_stroke_both)

# Relevel "stroke" "no_stroke" factors: positive class: "stroke" #### 
train_stroke_both$stroke <- relevel(train_stroke_both$stroke, ref = "stroke")


# RPART native #######
# Recursive Partitioning and Regression Trees ######

# Gini Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
gini_tree_cp0.01_both <- rpart(stroke ~ ., 
                               data = train_stroke_both)

plotcp(gini_tree_cp0.01_both)
print(gini_tree_cp0.01_both)
summary(gini_tree_cp0.01_both)

y_hat_gini_tree_cp0.01_both <- predict(gini_tree_cp0.01_both, test_stroke_t, 
                                       type = "class")

# Model Evaluation ::::::::

confusionMatrix(y_hat_gini_tree_cp0.01_both,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.01_both,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_gini_tree_cp0.01_both,
                test_stroke_t$stroke)$table

cm_gini_tree_cp0.01_both <- confusionMatrix(y_hat_gini_tree_cp0.01_both, 
                                            test_stroke_t$stroke)
cm_gini_tree_cp0.01_both

# F_Meas   

F_meas(confusionMatrix(y_hat_gini_tree_cp0.01_both,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_gini_tree_cp0.01_both <- predict(gini_tree_cp0.01_both,
                                     test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_roc_gini_tree_cp0.01_both <- plot(roc(response = test_stroke_t$stroke, 
                                           predictor = roc_gini_tree_cp0.01_both$stroke, 
                                           levels = c("stroke", "no_stroke")), 
                                       print.auc = TRUE,
                                       xlim = c(1,0),
                                       ylim = c(0,1),
                                       xlab = "Specificity", 
                                       ylab ="Sensibility", 
                                       main = "Gini Tree, CP: 0.01 - both")

# Sensitivity vs Specificity AUC
auc(sens_roc_gini_tree_cp0.01_both)

# __Accuracy :  0.7953  ####
# __Sensitivity :   0.71429  ####         
# __Specificity : 0.79894 ####
# __Balanced Accuracy : 0.75661   ####
# __F_meas, beta = 1 :  0.2298851  ####
# __AUC Sens vs Spec : 0.7352 ######

# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
gini_tree_cp0.001_both <- rpart(stroke ~., 
                                data = train_stroke_both, 
                                parms=list(split=c("gini")),
                                cp = 0.001)

plotcp(gini_tree_cp0.001_both)
print(gini_tree_cp0.001_both)
summary(gini_tree_cp0.001_both)

y_hat_gini_tree_cp0.001_both <- predict(gini_tree_cp0.001_both, test_stroke_t, 
                                        type = "class")

# Model Evaluation ::::::::

confusionMatrix(y_hat_gini_tree_cp0.001_both,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.001_both,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_gini_tree_cp0.001_both,
                test_stroke_t$stroke)$table

cm_gini_tree_cp0.001_both <- confusionMatrix(y_hat_gini_tree_cp0.001_both, 
                                             test_stroke_t$stroke)
cm_gini_tree_cp0.001_both

# F_Meas

F_meas(confusionMatrix(y_hat_gini_tree_cp0.001_both,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_gini_tree_cp0.001_both <- predict(gini_tree_cp0.001_both,
                                      test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_gini_tree_cp0.001_both <- plot(roc(response = test_stroke_t$stroke, 
                                                  predictor = roc_gini_tree_cp0.001_both$stroke, 
                                                  levels = c("stroke", "no_stroke")), 
                                              print.auc = TRUE,
                                              xlim = c(1,0),
                                              ylim = c(0,1),
                                              xlab = "Specificity", 
                                              ylab ="Sensibility", 
                                              main = "Gini Tree, CP:001 - both")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_gini_tree_cp0.001_both)

# __Accuracy : 0.8269   ####
# __Sensitivity : 0.38095 ####       
# __Specificity : 0.84681 ####
# __Balanced Accuracy : 0.61388  ####
# __F_meas, beta = 1 : 0.1584158   ####
# __AUC Sens vs Spec : 0.3724 ######

# Default Gini Tree & Cost Matrix 3 to 1 #####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
cost_matrix_tree_both <- rpart(stroke ~ ., 
                               data = train_stroke_both,
                               parms=list(
                                 loss=matrix(c(0,3,1,0), # A false negative is 3 times worse than a false positive
                                             byrow=TRUE,
                                             nrow=2)))

plotcp(cost_matrix_tree_both)
print(cost_matrix_tree_both)
summary(cost_matrix_tree_both)

y_hat_cost_matrix_tree_both <- predict(cost_matrix_tree_both, test_stroke_t, 
                                       type = "class")

# Model Evaluation ::::::::

confusionMatrix(y_hat_cost_matrix_tree_both,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_cost_matrix_tree_both,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_cost_matrix_tree_both,
                test_stroke_t$stroke)$table

cm_cost_matrix_tree_both <- confusionMatrix(y_hat_cost_matrix_tree_both, 
                                            test_stroke_t$stroke)
cm_cost_matrix_tree_both

# F_Meas

F_meas(confusionMatrix(y_hat_cost_matrix_tree_both,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_cost_matrix_tree_both <- predict(cost_matrix_tree_both,
                                     test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_cost_matrix_tree_both <- plot(roc(response = test_stroke_t$stroke, 
                                                 predictor = roc_cost_matrix_tree_both$stroke, 
                                                 levels = c("stroke", "no_stroke")), 
                                             print.auc = TRUE,
                                             xlim = c(1,0),
                                             ylim = c(0,1),
                                             xlab = "Specificity", 
                                             ylab ="Sensibility", 
                                             main = "Default Gini Tree & Cost Matrix 3 to 1 - both")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_cost_matrix_tree_both)

# __Accuracy : 0.6069   ##### 
# __Sensitivity : 0.85714  #####         
# __Specificity : 0.59574  #####  
# __Balanced Accuracy : 0.72644  #####
# __F_meas, beta = 1 : 0.15720522  #####
# __AUC Sens vs Spec : 0.7368 ######


# RPART caret ####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_caret_tree_both <- train(stroke ~ ., method = "rpart",
                               data = train_stroke_both, 
                               trControl = ctrl,
                               metric="ROC")

plot(train_caret_tree_both)
print(train_caret_tree_both)
summary(train_caret_tree_both)

y_hat_caret_tree_both <- predict(train_caret_tree_both, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_caret_tree_over, 
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_caret_tree_both, 
                test_stroke_t$stroke)$table

cm_caret_tree_both <- confusionMatrix(y_hat_caret_tree_both, test_stroke_t$stroke)
cm_caret_tree_both

# F_Meas

F_meas(confusionMatrix(y_hat_caret_tree_both, 
                       test_stroke_t$stroke)$table, beta = 1, estimator = "binary")

# Calc. Probs for every class
roc_caret_tree_both <- predict(train_caret_tree_both,
                               test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_caret_tree_both <- plot(roc(response = test_stroke_t$stroke, 
                                           predictor = roc_caret_tree_both$stroke, 
                                           levels = c("stroke", "no_stroke")), 
                                       print.auc = TRUE,
                                       xlim = c(1,0),
                                       ylim = c(0,1),
                                       xlab = "Specificity", 
                                       ylab ="Sensibility", 
                                       main = "RPART caret - both")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_caret_tree_both)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_caret_tree_both <- evalm(list(train_caret_tree_both), 
                              positive = "stroke",
                              gnames=c('CARET Tree - both'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_caret_tree_both <- evalm(list(train_caret_tree_both), 
                            positive = "stroke",
                            plots = "cc",
                            title = "Calibration Curve",
                            gnames=c('CARET Tree - both'))

# Precision - Recall Gain
prg_caret_tree_both <- evalm(list(train_caret_tree_both), 
                             positive = "stroke",
                             plots = "prg",
                             title = "Precision - Recall Gain",
                             gnames=c('CARET Tree - both'))

# Precision - Recall Curve
pr_caret_tree_both <- evalm(list(train_caret_tree_both), 
                            positive = "stroke",
                            plots = "pr",
                            title = "Precision - Recall Curve",
                            gnames=c('CARET Tree - both'))

# True positive vs false positive ROC
roc_tp_fp_caret_tree_both <- evalm(list(train_caret_tree_both), 
                                   positive = "stroke",
                                   plots = "r",
                                   title = "True Positive - False Positive rate ROC",
                                   gnames=c('CARET Tree - both'))

# __Accuracy :  0.7953   ####
# __Sensitivity : 0.71429      ####        
# __Specificity : 0.79894  ####
# __Balanced Accuracy : 0.75661   ####
# __F_meas, beta = 1 : 0.2298851  #####
# __AUC Sens vs Spec : 0.7352 ######


#KNN caret #######
# K-Nearest-Neighbor #######
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_knn_both <- train(stroke ~ ., method = "knn", 
                        data = train_stroke_both, 
                        trControl = ctrl,
                        metric="ROC")

plot(train_knn_both)
print(train_knn_both)
summary(train_knn_both)

y_hat_knn_both <- predict(train_knn_both, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_knn_both,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_knn_both,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_knn_both,
                test_stroke_t$stroke)$table

cm_knn_both <- confusionMatrix(y_hat_knn_both, test_stroke_t$stroke)
cm_knn_both

# F_Meas

F_meas(confusionMatrix(y_hat_knn_both,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_knn_both <- predict(train_knn_both,
                        test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_knn_both <- plot(roc(response = test_stroke_t$stroke, 
                                    predictor = roc_knn_both$stroke, levels = c("stroke", "no_stroke")), 
                                print.auc = TRUE,
                                xlim = c(1,0),
                                ylim = c(0,1),
                                xlab = "Specificity", 
                                ylab ="Sensibility", 
                                main = "KNN - both")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_knn_both)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate ?????? Very strange results!
eval_knn_both <- evalm(list(train_knn_both), 
                       positive = "stroke",
                       gnames=c('KNN - both'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_knn_both <- evalm(list(train_knn_both), 
                     positive = "stroke",
                     plots = "cc",
                     title = "Calibration Curve",
                     gnames=c('KNN - both'))

# Precision - Recall Gain
prg_knn_both <- evalm(list(train_knn_both), 
                      positive = "stroke",
                      plots = "prg",
                      title = "Precision - Recall Gain",
                      gnames=c('KNN - both'))

# Precision - Recall Curve
pr_knn_both <- evalm(list(train_knn_both), 
                     positive = "stroke",
                     plots = "pr",
                     title = "Precision - Recall Curve",
                     gnames=c('KNN - both'))

# True positive vs false positive ROC
roc_tp_fp_knn_both <- evalm(list(train_knn_both), 
                            positive = "stroke",
                            plots = "r",
                            title = "True Positive - False Positive rate ROC",
                            gnames=c('KNN - both'))

# __Accuracy : 0.7648 ##### 
# __Sensitivity : 0.57143  #####         
# __Specificity : 0.77340  ##### 
# __Balanced Accuracy : 0.67242  #####
# __F_meas, beta = 1 : 0.172043 #####
# __AUC Sens vs Spec : 0.6639 ######

# RANDOM FOREST ########
# it takes time! ######
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_rf_both <- train(stroke ~ ., method = "rf", 
                       data = train_stroke_both, 
                       trControl = ctrl,
                       metric="ROC")

plot(train_rf_both)
print(train_rf_both)
summary(train_rf_both)

y_hat_rf_both <- predict(train_rf_both, test_stroke_t, type = "raw")

# Model Evaluation ::::::::

confusionMatrix(y_hat_rf_both, test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_rf_both, test_stroke_t$stroke)$table

cm_rf_both <- confusionMatrix(y_hat_rf_both, test_stroke_t$stroke)
cm_rf_both

# F_Meas 

F_meas(confusionMatrix(y_hat_rf_both, test_stroke_t$stroke)$table, 
       reference = Reference, beta = 1)

# Calc. Probs for every class
roc_rf_both <- predict(train_rf_both,
                       test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_rf_both <- plot(roc(response = test_stroke_t$stroke, 
                                   predictor = roc_rf_both$stroke, levels = c("stroke", "no_stroke")), 
                               print.auc = TRUE,
                               xlim = c(1,0),
                               ylim = c(0,1),
                               xlab = "Specificity", 
                               ylab ="Sensibility", 
                               main = "Random Forest - both")

# Sensitivity vs Specificity ROC CURVE
auc(sens_espec_roc_rf_both)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate ?????? Very strange results!
eval_rf_both <- evalm(list(train_rf_both), 
                      positive = "stroke",
                      gnames=c('Random Forest - both'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_rf_both <- evalm(list(train_rf_both), 
                    positive = "stroke",
                    plots = "cc",
                    title = "Calibration Curve",
                    gnames=c('Random Forest - both'))

# Precision - Recall Gain
prg_rf_both <- evalm(list(train_rf_both), 
                     positive = "stroke",
                     plots = "prg",
                     title = "Precision - Recall Gain",
                     gnames=c('Random Forest - both'))

# Precision - Recall Curve
pr_rf_both <- evalm(list(train_rf_both), 
                    positive = "stroke",
                    plots = "pr",
                    title = "Precision - Recall Curve",
                    gnames=c('Random Forest - both'))

# True positive vs false positive ROC
roc_tp_fp_rf_both <- evalm(list(train_rf_both), 
                           positive = "stroke",
                           plots = "r",
                           title = "True Positive - False Positive rate ROC",
                           gnames=c('Random Forest - both'))

# __Accuracy : 0.9277    ##### 
# __Sensitivity : 0.119048  #####         
# __Specificity : 0.963830   ##### 
# __Balanced Accuracy : 0.541439  ##### 
# __F_meas, beta = 1 : 0.1234568 #####
# # __AUC Sens vs Spec : 0.805  ###### 

# MDA: TRAIN ERROR ######
# Mixture Discriminant Analysis ####
# set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
# Results are random variables

# train_mda_both <- train(stroke ~ ., method = "mda", 
# data = train_stroke_both,
# trControl = ctrl,
# metric="ROC") # Several Warnings

# Error: Stopping
# Además: There were 50 or more warnings (use warnings() to see the first 50)

# plot(train_mda_both)
# print(train_mda_both)
# summary(train_mda_both)

# y_hat_mda_both <- predict(train_mda_both, test_stroke_t)

# Model Evaluation ::::::::

# confusionMatrix(y_hat_mda_both,
# test_stroke_t$stroke)$overall["Accuracy"]

# confusionMatrix(y_hat_mda_both,
# test_stroke$stroke)$overall

# confusionMatrix(y_hat_mda_both,
# test_stroke_t$stroke)$table

# cm_mda_both<- confusionMatrix(y_hat_mda_both, test_stroke_t$stroke, 
# positive = "stroke")
# cm_mda_both

# F_Meas

# F_meas(confusionMatrix(y_hat_mda_both,
# test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
# roc_mda_both <- predict(train_mda_both,
# test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
# sens_espec_roc_mdaboth <-plot(roc(response = test_stroke_t$stroke, 
# predictor = roc_mda_both$stroke, levels = c("stroke", "no_stroke")), 
# print.auc = TRUE,
# xlim = c(1,0),
# ylim = c(0,1),
# xlab = "Specificity", 
# ylab ="Sensibility", 
# main = "MDA - Over")

# Sensitivity vs Specificity AUC
# auc(sens_espec_roc_mda_both)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
# eval_mda_both <- evalm(list(train_mda_both), 
# positive = "stroke",
# gnames=c('MDA - both')) 

# Calibration Curve (True prob. vs Predicted prob.)
# cc_mda_both <- evalm(list(train_mda_both), 
# positive = "stroke",
# plots = "cc",
# title = "Calibration Curve",
# gnames=c('MDA - both'))

# Precision - Recall Gain
# prg_mda_both <- evalm(list(train_mda_both), 
# positive = "stroke",
# plots = "prg",
# title = "Precision - Recall Gain",
# gnames=c('MDA - both'))

# Precision - Recall Curve
# pr_mda_both <- evalm(list(train_mda_both), 
# positive = "stroke",
# plots = "pr",
# title = "Precision - Recall Curve",
# gnames=c('MDA - both'))

# True positive vs false positive ROC
# roc_tp_fp_mda_both <- evalm(list(train_mda_both), 
# positive = "stroke",
# plots = "r",
# title = "True Positive - False Positive rate ROC",
# gnames=c('MDA - both'))

# NNET: great variability !!!#####
# Neural Network ####
# It takes some time!
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
# Results are random variables: great variability!!!

train_nnet_both <- train(stroke ~ ., method = "nnet",
                         data = train_stroke_both,
                         trControl = ctrl,
                         trace = FALSE,
                         metric="ROC")

plot(train_nnet_both)
print(train_nnet_both)
summary(train_nnet_both)

y_hat_nnet_both <- predict(train_nnet_both, test_stroke_t, type = "raw")

# Model Evaluation ::::::::

confusionMatrix(as.factor(y_hat_nnet_both),
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(as.factor(y_hat_nnet_both),
                test_stroke_t$stroke)$overall

confusionMatrix(as.factor(y_hat_nnet_both),
                test_stroke_t$stroke)$table

cm_nnet_both <- confusionMatrix(as.factor(y_hat_nnet_both), test_stroke_t$stroke)
cm_nnet_both

# F_Meas

F_meas(confusionMatrix(as.factor(y_hat_nnet_both),
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_nnet_both <- predict(train_nnet_both,
                         test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_nnet_both <- plot(roc(response = test_stroke_t$stroke, 
                                     predictor = roc_nnet_both$stroke, 
                                     levels = c("no_stroke", "stroke")), 
                                 print.auc = TRUE,
                                 xlim = c(1,0),
                                 ylim = c(0,1),
                                 xlab = "Specificity", 
                                 ylab ="Sensibility", 
                                 main = "NNET - both")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_nnet_both)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_nnet_both <- evalm(list(train_nnet_both), 
                        positive = "stroke",
                        gnames=c('NNET - both'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_nnet_both <- evalm(list(train_nnet_both), 
                      positive = "stroke",
                      plots = "cc",
                      title = "Calibration Curve",
                      gnames=c('NNET - both'))

# Precision - Recall Gain
prg_nnet_both <- evalm(list(train_nnet_both), 
                       positive = "stroke",
                       plots = "prg",
                       title = "Precision - Recall Gain",
                       gnames=c('NNET - both'))

# Precision - Recall Curve
pr_nnet_both <- evalm(list(train_nnet_both), 
                      positive = "stroke",
                      plots = "pr",
                      title = "Precision - Recall Curve",
                      gnames=c('NNET - both'))

# True positive vs false positive ROC
roc_tp_fp_nnet_both <- evalm(list(train_nnet_both), 
                             positive = "stroke",
                             plots = "r",
                             title = "True Positive - False Positive rate ROC",
                             gnames=c('NNET - both'))

# __Accuracy : 0.78 ####
# __Sensitivity : 0.76190 ####        
# __Specificity : 0.78085   ####
# __Balanced Accuracy : 0.77138    ####
# __F_meas, beta = 1 : 0.2285714 #####
# __AUC Sens vs Spec : 0.8138  ###### 

# FDA #####
# Flexible Discriminant Analysis #####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_fda_both <- train(stroke ~ ., method = "fda", 
                        data = train_stroke_both, 
                        trControl = ctrl,
                        metric="ROC")

plot(train_fda_both)
print(train_fda_both)
summary(train_fda_both)

y_hat_fda_both <- predict(train_fda_both, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_fda_both,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_fda_both,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_fda_both,
                test_stroke_t$stroke)$table

cm_fda_both <- confusionMatrix(y_hat_fda_both, test_stroke_t$stroke)
cm_fda_both

# F_Meas

F_meas(confusionMatrix(y_hat_fda_both,
                       test_stroke_t$stroke)$table, beta = 1)


# Calc. Probs for every class
roc_fda_both <- predict(train_fda_both,
                        test_stroke_t, type = "prob")

# Curve graph
sens_espec_roc_fda_both <- plot(roc(response = test_stroke_t$stroke, 
                                    predictor = roc_fda_both$stroke, 
                                    levels = c("no_stroke", "stroke")), 
                                print.auc = TRUE,
                                xlim = c(1,0),
                                ylim = c(0,1),
                                xlab = "Specificity", 
                                ylab ="Sensibility", 
                                main = "Flexible Discriminant Analysis - both")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_fda_both)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_fda_both <- evalm(list(train_fda_both), 
                       positive = "stroke",
                       gnames=c('FDA - both'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_fda_both <- evalm(list(train_fda_both), 
                     positive = "stroke",
                     plots = "cc",
                     title = "Calibration Curve",
                     gnames=c('FDA - both'))

# Precision - Recall Gain
prg_fda_both <- evalm(list(train_fda_both), 
                      positive = "stroke",
                      plots = "prg",
                      title = "Precision - Recall Gain",
                      gnames=c('FDA - both'))

# Precision - Recall Curve
pr_fda_both <- evalm(list(train_fda_both), 
                     positive = "stroke",
                     plots = "pr",
                     title = "Precision - Recall Curve",
                     gnames=c('FDA - both'))

# True positive vs false positive ROC
roc_tp_fp_fda_both <- evalm(list(train_fda_both), 
                            positive = "stroke",
                            plots = "r",
                            title = "True Positive - False Positive rate ROC",
                            gnames=c('FDA - both'))

# __Accuracy : 0.7413    ####
# __Sensitivity : 0.73810  ####        
# __Specificity : 0.74149 ####  
# __Balanced Accuracy :  0.73979   ####
# __F_meas, beta = 1 : 0.1962025 #####
# __AUC Sens vs Spec : 0.8346 ###### 


# NAIVE BAYES ######
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_naiveBayes_both <- train(stroke ~ ., method = "naive_bayes", 
                               data = train_stroke_both, 
                               trControl = ctrl,
                               metric="ROC")

plot(train_naiveBayes_both)
print(train_naiveBayes_both)
summary(train_naiveBayes_both)

y_hat_naiveBayes_both <- predict(train_naiveBayes_both, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_naiveBayes_both,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_naiveBayes_both,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_naiveBayes_both,
                test_stroke_t$stroke)$table

cm_naiveBayes_both <- confusionMatrix(y_hat_naiveBayes_both, test_stroke_t$stroke)
cm_naiveBayes_both

# F_Meas

F_meas(confusionMatrix(y_hat_naiveBayes_both,
                       test_stroke_t$stroke)$table,beta = 1)


# Calc. Probs for every class
roc_naiveBayes_both <- predict(train_naiveBayes_both,
                               test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_naiveBayes_both<- plot(roc(response = test_stroke_t$stroke, 
                                          predictor = roc_naiveBayes_both$stroke, 
                                          levels = c("no_stroke", "stroke")), 
                                      print.auc = TRUE,
                                      xlim = c(1,0),
                                      ylim = c(0,1),
                                      xlab = "Specificity", 
                                      ylab ="Sensibility", 
                                      main = "Naive Bayes - both")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_naiveBayes_both)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_naiveBayes_both <- evalm(list(train_naiveBayes_both), 
                              positive = "stroke",
                              gnames=c('Naive Bayes - both'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_naiveBayes_both <- evalm(list(train_naiveBayes_both), 
                            positive = "stroke",
                            plots = "cc",
                            title = "Calibration Curve",
                            gnames=c('Naive Bayes - both'))

# Precision - Recall Gain
prg_naiveBayes_both <- evalm(list(train_naiveBayes_both), 
                             positive = "stroke",
                             plots = "prg",
                             title = "Precision - Recall Gain",
                             gnames=c('Naive Bayes - both'))

# Precision - Recall Curve
pr_naiveBayes_both <- evalm(list(train_naiveBayes_both), 
                            positive = "stroke",
                            plots = "pr",
                            title = "Precision - Recall Curve",
                            gnames=c('Naive Bayes - both'))

# True positive vs false positive ROC
roc_tp_fp_naiveBayes_both <- evalm(list(train_naiveBayes_both), 
                                   positive = "stroke",
                                   plots = "r",
                                   title = "True Positive - False Positive rate ROC",
                                   gnames=c('Naive Bayes - both'))

# __Accuracy : 0.9358  ##### 
# __Sensitivity : 0.26190  #####         
# __Specificity : 0.96596 ##### 
# __Balanced Accuracy : 0.61393  ##### 
# __F_meas, beta = 1 : 0.2588235 #####
# __AUC Sens vs Spec : 0.8505 ######

# _______________________######## 
# Better estimates ######
# _______________________######## 

set.seed(1969, sample.kind="Rounding") # Every time we run the code, we get a different ovun.sample
# Accuracy and balanced accuracy strongly depends on this ramdom process.

train_stroke_better <- ROSE(stroke ~ ., data = train_stroke_t)$data
table(train_stroke_better$stroke)

str(train_stroke_better)

# Relevel "stroke" "no_stroke" factors: positive class: "stroke"
train_stroke_better$stroke <- relevel(train_stroke_better$stroke, ref = "stroke")

# RPART native #######
# Recursive Partitioning and Regression Trees ######

# Gini Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
gini_tree_cp0.01_better <- rpart(stroke ~ ., 
                                 data = train_stroke_better)

plotcp(gini_tree_cp0.01_better)
print(gini_tree_cp0.01_better)
summary(gini_tree_cp0.01_better)

rpart.plot(gini_tree_cp0.01_better)

y_hat_gini_tree_cp0.01_better <- predict(gini_tree_cp0.01_better, test_stroke_t, 
                                         type = "class")
# Model Evaluation ::::::::

confusionMatrix(y_hat_gini_tree_cp0.01_better,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.01_better,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_gini_tree_cp0.01_better,
                test_stroke_t$stroke)$table

cm_gini_tree_cp0.01_better <- confusionMatrix(y_hat_gini_tree_cp0.01_better, 
                                              test_stroke_t$stroke)
cm_gini_tree_cp0.01_better

# F_Meas   

F_meas(confusionMatrix(y_hat_gini_tree_cp0.01_better,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_gini_tree_cp0.01_better <- predict(gini_tree_cp0.01_better,
                                       test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_roc_gini_tree_cp0.01_better <- plot(roc(response = test_stroke_t$stroke, 
                                             predictor = roc_gini_tree_cp0.01_better$stroke, 
                                             levels = c("stroke", "no_stroke")), 
                                         print.auc = TRUE,
                                         xlim = c(1,0),
                                         ylim = c(0,1),
                                         xlab = "Specificity", 
                                         ylab ="Sensibility", 
                                         main = "Gini Tree, CP: 0.01 - better")

# Sensitivity vs Specificity AUC
auc(sens_roc_gini_tree_cp0.01_better)

# __Accuracy : 0.8096  ####
# __Sensitivity : 0.73810  ####         
# __Specificity : 0.81277  ####
# __Balanced Accuracy : 0.77543  ####
# __F_meas, beta = 1 :  0.248996  ####
# __AUC Sens vs Spec : 0.8338 ######

# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
gini_tree_cp0.001_better <- rpart(stroke ~., 
                                  data = train_stroke_better, 
                                  parms=list(split=c("gini")),
                                  cp = 0.001)

plotcp(gini_tree_cp0.001_better)
print(gini_tree_cp0.001_better)
summary(gini_tree_cp0.001_better)

y_hat_gini_tree_cp0.001_better <- predict(gini_tree_cp0.001_both, test_stroke_t, 
                                          type = "class")

# Model Evaluation ::::::::

confusionMatrix(y_hat_gini_tree_cp0.001_better,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.001_better,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_gini_tree_cp0.001_better,
                test_stroke_t$stroke)$table

cm_gini_tree_cp0.001_better <- confusionMatrix(y_hat_gini_tree_cp0.001_better, 
                                               test_stroke_t$stroke)
cm_gini_tree_cp0.001_better

# F_Meas

F_meas(confusionMatrix(y_hat_gini_tree_cp0.001_better,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_gini_tree_cp0.001_better <- predict(gini_tree_cp0.001_better,
                                        test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_gini_tree_cp0.001_better <- plot(roc(response = test_stroke_t$stroke, 
                                                    predictor = roc_gini_tree_cp0.001_better$stroke, 
                                                    levels = c("stroke", "no_stroke")), 
                                                print.auc = TRUE,
                                                xlim = c(1,0),
                                                ylim = c(0,1),
                                                xlab = "Specificity", 
                                                ylab ="Sensibility", 
                                                main = "Gini Tree, CP:001 - both")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_gini_tree_cp0.001_better)

# __Accuracy : 0.8269   ####
# __Sensitivity : 0.38095 ####       
# __Specificity : 0.84681  ####
# __Balanced Accuracy : 0.61388   ####
# __F_meas, beta = 1 : 0.1584158  ####
# __AUC Sens vs Spec : 0.8052 ######

# Default Gini Tree & Cost Matrix 3 to 1 #####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
cost_matrix_tree_better <- rpart(stroke ~ ., 
                                 data = train_stroke_better,
                                 parms=list(
                                   loss=matrix(c(0,3,1,0), # A false negative is 3 times worse than a false positive
                                               byrow=TRUE,
                                               nrow=2)))

plotcp(cost_matrix_tree_better)
print(cost_matrix_tree_better)
summary(cost_matrix_tree_better)

y_hat_cost_matrix_tree_better <- predict(cost_matrix_tree_both, test_stroke_t, 
                                         type = "class")

# Model Evaluation ::::::::

confusionMatrix(y_hat_cost_matrix_tree_better,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_cost_matrix_tree_better,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_cost_matrix_tree_better,
                test_stroke_t$stroke)$table

cm_cost_matrix_tree_better <- confusionMatrix(y_hat_cost_matrix_tree_better, 
                                              test_stroke_t$stroke)
cm_cost_matrix_tree_better

# F_Meas

F_meas(confusionMatrix(y_hat_cost_matrix_tree_better,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_cost_matrix_tree_better <- predict(cost_matrix_tree_better,
                                       test_stroke_t, type = "prob") %>% 
  data.frame()

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_cost_matrix_tree_better <- plot(roc(response = test_stroke_t$stroke, 
                                                   predictor = roc_cost_matrix_tree_better$stroke, 
                                                   levels = c("stroke", "no_stroke")), 
                                               print.auc = TRUE,
                                               xlim = c(1,0),
                                               ylim = c(0,1),
                                               xlab = "Specificity", 
                                               ylab ="Sensibility", 
                                               main = "Default Gini Tree & Cost Matrix 3 to 1 - both")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_cost_matrix_tree_better)

# __Accuracy :  0.6069 ##### 
# __Sensitivity : 0.85714 #####         
# __Specificity : 0.59574 #####  
# __Balanced Accuracy : 0.72644  #####
# __F_meas, beta = 1 : 0.1572052 #####
# __AUC Sens vs Spec :  0.2991


# RPART caret ####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_caret_tree_better <- train(stroke ~ ., method = "rpart",
                                 data = train_stroke_better, 
                                 trControl = ctrl,
                                 metric="ROC")

plot(train_caret_tree_better)
print(train_caret_tree_better)
summary(train_caret_tree_better)

y_hat_caret_tree_better <- predict(train_caret_tree_better, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_caret_tree_better, 
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_caret_tree_better, 
                test_stroke_t$stroke)$table

cm_caret_tree_better <- confusionMatrix(y_hat_caret_tree_better, test_stroke_t$stroke)
cm_caret_tree_better

# F_Meas

F_meas(confusionMatrix(y_hat_caret_tree_better, 
                       test_stroke_t$stroke)$table, beta = 1, estimator = "binary")

# Calc. Probs for every class
roc_caret_tree_better <- predict(train_caret_tree_better,
                                 test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_caret_tree_better <- plot(roc(response = test_stroke_t$stroke, 
                                             predictor = roc_caret_tree_better$stroke, 
                                             levels = c("stroke", "no_stroke")), 
                                         print.auc = TRUE,
                                         xlim = c(1,0),
                                         ylim = c(0,1),
                                         xlab = "Specificity", 
                                         ylab ="Sensibility", 
                                         main = "RPART caret - better")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_caret_tree_better)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_caret_tree_better <- evalm(list(train_caret_tree_better), 
                                positive = "stroke",
                                gnames=c('CARET Tree - better'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_caret_tree_better <- evalm(list(train_caret_tree_better), 
                              positive = "stroke",
                              plots = "cc",
                              title = "Calibration Curve",
                              gnames=c('CARET Tree - better'))

# Precision - Recall Gain
prg_caret_tree_better <- evalm(list(train_caret_tree_better), 
                               positive = "stroke",
                               plots = "prg",
                               title = "Precision - Recall Gain",
                               gnames=c('CARET Tree - better'))

# Precision - Recall Curve
pr_caret_tree_better <- evalm(list(train_caret_tree_better), 
                              positive = "stroke",
                              plots = "pr",
                              title = "Precision - Recall Curve",
                              gnames=c('CARET Tree - better'))

# True positive vs false positive ROC
roc_tp_fp_caret_tree_better <- evalm(list(train_caret_tree_better), 
                                     positive = "stroke",
                                     plots = "r",
                                     title = "True Positive - False Positive rate ROC",
                                     gnames=c('CARET Tree - better'))

# __Accuracy : 0.8096    ####
# __Sensitivity :  0.73810     ####        
# __Specificity : 0.81277  ####
# __Balanced Accuracy : 0.77543   ####
# __F_meas, beta = 1 : 0.248996 #####
# __AUC Sens vs Spec : 0.8338 ######


#KNN caret #######
# K-Nearest-Neighbor #######
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_knn_better <- train(stroke ~ ., method = "knn", 
                          data = train_stroke_better, 
                          trControl = ctrl,
                          metric="ROC")

plot(train_knn_better)
print(train_knn_better)
summary(train_knn_better)

y_hat_knn_better <- predict(train_knn_better, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_knn_better,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_knn_better,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_knn_better,
                test_stroke_t$stroke)$table

cm_knn_better <- confusionMatrix(y_hat_knn_better, test_stroke_t$stroke)
cm_knn_better

# F_Meas

F_meas(confusionMatrix(y_hat_knn_better,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_knn_better <- predict(train_knn_better,
                          test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_knn_better <- plot(roc(response = test_stroke_t$stroke, 
                                      predictor = roc_knn_better$stroke, levels = c("stroke", "no_stroke")), 
                                  print.auc = TRUE,
                                  xlim = c(1,0),
                                  ylim = c(0,1),
                                  xlab = "Specificity", 
                                  ylab ="Sensibility", 
                                  main = "KNN - better")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_knn_better)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate ?????? Very strange results!
eval_knn_better <- evalm(list(train_knn_better), 
                         positive = "stroke",
                         gnames=c('KNN - better'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_knn_better <- evalm(list(train_knn_better), 
                       positive = "stroke",
                       plots = "cc",
                       title = "Calibration Curve",
                       gnames=c('KNN - better'))

# Precision - Recall Gain
prg_knn_better <- evalm(list(train_knn_better), 
                        positive = "stroke",
                        plots = "prg",
                        title = "Precision - Recall Gain",
                        gnames=c('KNN - better'))

# Precision - Recall Curve
pr_knn_better <- evalm(list(train_knn_better), 
                       positive = "stroke",
                       plots = "pr",
                       title = "Precision - Recall Curve",
                       gnames=c('KNN - better'))

# True positive vs false positive ROC
roc_tp_fp_knn_better <- evalm(list(train_knn_better), 
                              positive = "stroke",
                              plots = "r",
                              title = "True Positive - False Positive rate ROC",
                              gnames=c('KNN - better'))

# __Accuracy : 0.7637   ##### 
# __Sensitivity : 0.61905   #####         
# __Specificity : 0.77021   ##### 
# __Balanced Accuracy : 0.69463   #####
# __F_meas, beta = 1 : 0.1830986 #####
# __AUC Sens vs Spec : 0.7685 ######

# RANDOM FOREST ########
# it takes time! ######
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_rf_better <- train(stroke ~ ., method = "rf", 
                         data = train_stroke_better, 
                         trControl = ctrl,
                         metric="ROC")

plot(train_rf_better)
print(train_rf_better)
summary(train_rf_better)

y_hat_rf_better <- predict(train_rf_better, test_stroke_t, type = "raw")

# Model Evaluation ::::::::

confusionMatrix(y_hat_rf_better, test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_rf_better, test_stroke_t$stroke)$table

cm_rf_better <- confusionMatrix(y_hat_rf_better, test_stroke_t$stroke)
cm_rf_better

# F_Meas 

F_meas(confusionMatrix(y_hat_rf_better, test_stroke_t$stroke)$table, 
       reference = Reference, beta = 1)


# Calc. Probs for every class
roc_rf_better <- predict(train_rf_better,
                         test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_rf_better <- plot(roc(response = test_stroke_t$stroke, 
                                     predictor = roc_rf_better$stroke, 
                                     levels = c("stroke", "no_stroke")), 
                                 print.auc = TRUE,
                                 xlim = c(1,0),
                                 ylim = c(0,1),
                                 xlab = "Specificity", 
                                 ylab ="Sensibility", 
                                 main = "Random Forest - better")

# Sensitivity vs Specificity ROC CURVE
auc(sens_espec_roc_rf_better)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate ?????? Very strange results!
eval_rf_better <- evalm(list(train_rf_better), 
                        positive = "stroke",
                        gnames=c('Random Forest - better'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_rf_better <- evalm(list(train_rf_better), 
                      positive = "stroke",
                      plots = "cc",
                      title = "Calibration Curve",
                      gnames=c('Random Forest - better'))

# Precision - Recall Gain
prg_rf_better <- evalm(list(train_rf_better), 
                       positive = "stroke",
                       plots = "prg",
                       title = "Precision - Recall Gain",
                       gnames=c('Random Forest - better'))

# Precision - Recall Curve
pr_rf_better <- evalm(list(train_rf_better), 
                      positive = "stroke",
                      plots = "pr",
                      title = "Precision - Recall Curve",
                      gnames=c('Random Forest - better'))

# True positive vs false positive ROC
roc_tp_fp_rf_better <- evalm(list(train_rf_better), 
                             positive = "stroke",
                             plots = "r",
                             title = "True Positive - False Positive rate ROC",
                             gnames=c('Random Forest - better'))

# __Accuracy : 0.7862    ##### 
# __Sensitivity : 0.59524  #####         
# __Specificity : 0.79468   ##### 
# __Balanced Accuracy : 0.69496 ##### 
# __F_meas, beta = 1 : 0.1923077 #####
# # __AUC Sens vs Spec : 0.8196  ###### 

# MDA: TRAIN ERROR ######
# Mixture Discriminant Analysis ####
# set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
# Results are random variables

# train_mda_better <- train(stroke ~ ., method = "mda", 
# data = train_stroke_better,
# trControl = ctrl,
# metric="ROC") # Several Warnings

# Error: Stopping
# Además: There were 50 or more warnings (use warnings() to see the first 50)

# plot(train_mda_better)
# print(train_mda_better)
# summary(train_mda_better)

# y_hat_mda_both <- predict(train_mda_both, test_stroke_t)

# Model Evaluation ::::::::

# confusionMatrix(y_hat_mda_better,
# test_stroke_t$stroke)$overall["Accuracy"]

# confusionMatrix(y_hat_mda_better,
# test_stroke$stroke)$overall

# confusionMatrix(y_hat_mda_better,
# test_stroke_t$stroke)$table

# cm_mda_better <- confusionMatrix(y_hat_mda_both, test_stroke_t$stroke, 
# positive = "stroke")
# cm_mda_better

# F_Meas

# F_meas(confusionMatrix(y_hat_mda_better,
# test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
# roc_mda_both <- predict(train_mda_better,
# test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
# sens_espec_roc_mda_better <-plot(roc(response = test_stroke_t$stroke, 
# predictor = roc_mda_better$stroke, 
# levels = c("stroke", "no_stroke")), 
# print.auc = TRUE,
# xlim = c(1,0),
# ylim = c(0,1),
# xlab = "Specificity", 
# ylab ="Sensibility", 
# main = "MDA - Over")

# Sensitivity vs Specificity AUC
# auc(sens_espec_roc_mda_better)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
# eval_mda_better <- evalm(list(train_mda_better), 
# positive = "stroke",
# gnames=c('MDA - better')) 

# Calibration Curve (True prob. vs Predicted prob.)
# cc_mda_better <- evalm(list(train_mda_better), 
# positive = "stroke",
# plots = "cc",
# title = "Calibration Curve",
# gnames=c('MDA - better'))

# Precision - Recall Gain
# prg_mda_better <- evalm(list(train_mda_better), 
# positive = "stroke",
# plots = "prg",
# title = "Precision - Recall Gain",
# gnames=c('MDA - better'))

# Precision - Recall Curve
# pr_mda_better <- evalm(list(train_mda_better), 
# positive = "stroke",
# plots = "pr",
# title = "Precision - Recall Curve",
# gnames=c('MDA - better'))

# True positive vs false positive ROC
# roc_tp_fp_mda_better <- evalm(list(train_mda_better), 
# positive = "stroke",
# plots = "r",
# title = "True Positive - False Positive rate ROC",
# gnames=c('MDA - better'))

# NNET: great variability !!!#####
# Neural Network ####
# It takes some time!
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

train_nnet_better <- train(stroke ~ ., method = "nnet",
                           data = train_stroke_better,
                           trControl = ctrl,
                           trace = FALSE,
                           metric="ROC")

plot(train_nnet_better)
print(train_nnet_better)
summary(train_nnet_better)

y_hat_nnet_better <- predict(train_nnet_better, test_stroke_t, type = "raw")

# Model Evaluation ::::::::

confusionMatrix(as.factor(y_hat_nnet_better),
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(as.factor(y_hat_nnet_better),
                test_stroke_t$stroke)$overall

confusionMatrix(as.factor(y_hat_nnet_better),
                test_stroke_t$stroke)$table

cm_nnet_better <- confusionMatrix(as.factor(y_hat_nnet_better), test_stroke_t$stroke)
cm_nnet_better

# F_Meas

F_meas(confusionMatrix(as.factor(y_hat_nnet_better),
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_nnet_better <- predict(train_nnet_better,
                           test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_nnet_better <- plot(roc(response = test_stroke_t$stroke, 
                                       predictor = roc_nnet_better$stroke, 
                                       levels = c("no_stroke", "stroke")), 
                                   print.auc = TRUE,
                                   xlim = c(1,0),
                                   ylim = c(0,1),
                                   xlab = "Specificity", 
                                   ylab ="Sensibility", 
                                   main = "NNET - Over")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_nnet_better)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_nnet_better <- evalm(list(train_nnet_better), 
                          positive = "stroke",
                          gnames=c('NNET - better'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_nnet_better <- evalm(list(train_nnet_better), 
                        positive = "stroke",
                        plots = "cc",
                        title = "Calibration Curve",
                        gnames=c('NNET - better'))

# Precision - Recall Gain
prg_nnet_better <- evalm(list(train_nnet_better), 
                         positive = "stroke",
                         plots = "prg",
                         title = "Precision - Recall Gain",
                         gnames=c('NNET - better'))

# Precision - Recall Curve
pr_nnet_better <- evalm(list(train_nnet_better), 
                        positive = "stroke",
                        plots = "pr",
                        title = "Precision - Recall Curve",
                        gnames=c('NNET - better'))

# True positive vs false positive ROC
roc_tp_fp_nnet_better <- evalm(list(train_nnet_better), 
                               positive = "stroke",
                               plots = "r",
                               title = "True Positive - False Positive rate ROC",
                               gnames=c('NNET - better'))

# __Accuracy :  0.7587 ####
# __Sensitivity : 0.71429 ####        
# __Specificity : 0.76064  ####
# __Balanced Accuracy : 0.73746   ####
# __F_meas, beta = 1 : 0.2020202 #####
# __AUC Sens vs Spec : 0.8 ###### 

# FDA #####
# Flexible Discriminant Analysis #####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_fda_better <- train(stroke ~ ., method = "fda", 
                          data = train_stroke_better, 
                          trControl = ctrl,
                          metric="ROC")

plot(train_fda_better)
print(train_fda_better)
summary(train_fda_better)

y_hat_fda_better <- predict(train_fda_better, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_fda_better,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_fda_better,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_fda_better,
                test_stroke_t$stroke)$table

cm_fda_better <- confusionMatrix(y_hat_fda_better, test_stroke_t$stroke)
cm_fda_better

# F_Meas

F_meas(confusionMatrix(y_hat_fda_better,
                       test_stroke_t$stroke)$table, beta = 1)


# Calc. Probs for every class
roc_fda_better <- predict(train_fda_better,
                          test_stroke_t, type = "prob")

# Curve graph
sens_espec_roc_fda_better <- plot(roc(response = test_stroke_t$stroke, 
                                      predictor = roc_fda_better$stroke, 
                                      levels = c("no_stroke", "stroke")), 
                                  print.auc = TRUE,
                                  xlim = c(1,0),
                                  ylim = c(0,1),
                                  xlab = "Specificity", 
                                  ylab ="Sensibility", 
                                  main = "Flexible Discriminant Analysis - Over")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_fda_better)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_fda_better <- evalm(list(train_fda_better), 
                         positive = "stroke",
                         gnames=c('FDA - better'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_fda_better <- evalm(list(train_fda_better), 
                       positive = "stroke",
                       plots = "cc",
                       title = "Calibration Curve",
                       gnames=c('FDA - better'))

# Precision - Recall Gain
prg_fda_better <- evalm(list(train_fda_better), 
                        positive = "stroke",
                        plots = "prg",
                        title = "Precision - Recall Gain",
                        gnames=c('FDA - better'))

# Precision - Recall Curve
pr_fda_better <- evalm(list(train_fda_better), 
                       positive = "stroke",
                       plots = "pr",
                       title = "Precision - Recall Curve",
                       gnames=c('FDA - better'))

# True positive vs false positive ROC
roc_tp_fp_fda_better <- evalm(list(train_fda_better), 
                              positive = "stroke",
                              plots = "r",
                              title = "True Positive - False Positive rate ROC",
                              gnames=c('FDA - better'))

# __Accuracy : 0.7587   ####
# __Sensitivity : 0.85714  ####        
# __Specificity : 0.75426 ####  
# __Balanced Accuracy : 0.80570   ####
# __F_meas, beta = 1 :  0.2330097 ####
#__AUC Sens vs Spec : 0.8588 ###### 


# NAIVE BAYES ######
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_naiveBayes_better <- train(stroke ~ ., method = "naive_bayes", 
                                 data = train_stroke_better, 
                                 trControl = ctrl,
                                 metric="ROC")

plot(train_naiveBayes_better)
print(train_naiveBayes_better)
summary(train_naiveBayes_better)

y_hat_naiveBayes_better <- predict(train_naiveBayes_better, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_naiveBayes_better,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_naiveBayes_better,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_naiveBayes_better,
                test_stroke_t$stroke)$table

cm_naiveBayes_better <- confusionMatrix(y_hat_naiveBayes_better, test_stroke_t$stroke)
cm_naiveBayes_better

# F_Meas

F_meas(confusionMatrix(y_hat_naiveBayes_better,
                       test_stroke_t$stroke)$table,beta = 1)


# Calc. Probs for every class
roc_naiveBayes_better <- predict(train_naiveBayes_better,
                                 test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_naiveBayes_better<- plot(roc(response = test_stroke_t$stroke, 
                                            predictor = roc_naiveBayes_better$stroke, 
                                            levels = c("no_stroke", "stroke")), 
                                        print.auc = TRUE,
                                        xlim = c(1,0),
                                        ylim = c(0,1),
                                        xlab = "Specificity", 
                                        ylab ="Sensibility", 
                                        main = "Naive Bayes - both")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_naiveBayes_better)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_naiveBayes_better <- evalm(list(train_naiveBayes_better), 
                                positive = "stroke",
                                gnames=c('Naive Bayes - better'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_naiveBayes_better <- evalm(list(train_naiveBayes_better), 
                              positive = "stroke",
                              plots = "cc",
                              title = "Calibration Curve",
                              gnames=c('Naive Bayes - better'))

# Precision - Recall Gain
prg_naiveBayes_better <- evalm(list(train_naiveBayes_better), 
                               positive = "stroke",
                               plots = "prg",
                               title = " - Recall Gain",
                               gnames=c('Naive Bayes - better'))

# Precision - Recall Curve
pr_naiveBayes_better <- evalm(list(train_naiveBayes_better), 
                              positive = "stroke",
                              plots = "pr",
                              title = "Precision - Recall Curve",
                              gnames=c('Naive Bayes - better'))

# True positive vs false positive ROC
roc_tp_fp_naiveBayes_better <- evalm(list(train_naiveBayes_better), 
                                     positive = "stroke",
                                     plots = "r",
                                     title = "True Positive - False Positive rate ROC",
                                     gnames=c('Naive Bayes - both'))



# __Accuracy : 0.9379  ##### 
# __Sensitivity : 0.14286    #####         
# __Specificity : 0.97340  ##### 
# __Balanced Accuracy : 0.55813  ##### 
# __F_meas, beta = 1 : 0.1643836 #####
#__AUC Sens vs Spec : 0.8503 ######

# _______________________########
# RESULTS SUMMARY ########
# _______________________########

models <- c("gini_tree_cp0.001_over",
            "gini_tree_cp0.001_over",
            "cost_matrix_tree_over",
            "caret_tree_over",
            "knn_over",
            "nnet_over",
            "fda_over",
            "gini_tree_cp0.01_both",
            "gini_tree_cp0.001_both",
            "cost_matrix_tree_both",
            "caret_tree_both",
            "knn_both",
            "nnet_both",
            "fda_both",
            "gini_tree_cp0.01_better",
            "gini_tree_cp0.001_better",
            "cost_matrix_tree_better",
            "caret_tree_better",
            "knn_better",
            "rf_better",
            "nnet_better",
            "fda_better")

sensitivity <- c(
  0.73810,
  0.47619,
  0.90476,
  0.73810,
  0.42857,
  0.76190,
  0.73810,
  0.71429,
  0.38095,
  0.85714,
  0.71429,
  0.57143,
  0.76190,
  0.73810,
  0.73810,
  0.38095,
  0.85714,
  0.73810,
  0.61905,
  0.59524,
  0.71429,
  0.85714)

specificity <- c(
  0.75745,
  0.88723,
  0.63085,
  0.77553,
  0.77553,
  0.77766,
  0.74574,
  0.79894,
  0.84681,
  0.59574,
  0.79894,
  0.77340,
  0.78085,
  0.74149,
  0.81277,
  0.84681,
  0.59574,
  0.81277,
  0.77021,
  0.79468,
  0.76064,
  0.75426)

balanced_accuracy <- c(
  0.74777,
  0.68171,
  0.76781,
  0.75681,
  0.60205,
  0.76978,
  0.74192,
  0.75661,
  0.61388,
  0.72644,
  0.75661,
  0.67242,
  0.77138,
  0.73979,
  0.77543,
  0.61388,
  0.72644,
  0.77543,
  0.69463,
  0.69496,
  0.73746,
  0.80570)

AUC <- c(
  0.7372,
  0.3212,
  0.7525,
  0.8042,
  0.4015,
  0.8095,
  0.8163,
  0.7352,
  0.3724,
  0.7368,
  0.7352,
  0.6639,
  0.8138,
  0.8346,
  0.8338,
  0.8052,
  0.2991,
  0.8338,
  0.7685,
  0.8196,
  0.8000,
  0.8588)

results_summary <- data.frame(models, 
                              sensitivity, specificity, balanced_accuracy, AUC) %>% 
  arrange(desc(AUC)) %>% 
  kable()

# FDA BETTER: Best model#####

# |Accuracy|Sensitivity|Specificity|Balanced Accuracy|      AUC |
# |-------:|----------:|----------:|----------------:|---------:|
# |  0.7587|    0.85714|    0.75426|          0.80570|    0.8588|


# Model comp. Precision-Recall Curve ####
# As an example, let's compare here the FDA-better model with three others: 
# Caret RPART-better, NNET-both and RF-better 

# __ROC True Prositve - False Positive ####

comp_roc <- evalm(list(train_caret_tree_better, train_nnet_both,
                       train_rf_better, train_fda_better), 
                  positive = "stroke",
                  plots = "r",
                  title = "ROC True Positive - False Positive",
                  gnames=c('caret_tree_better', 'nnet_both',
                           'train_rf_better', "train_fda_better"))

comp_prg <- evalm(list(train_caret_tree_better, train_nnet_both,
                       train_rf_better, train_fda_better), 
                  positive = "stroke",
                  plots = "prg",
                  title = "Precision - Recall Gain",
                  gnames=c('caret_tree_better', 'nnet_both',
                           'train_rf_better', "train_fda_better"))

comp_pr <- evalm(list(train_caret_tree_better, train_nnet_both,
                      train_rf_better, train_fda_better), 
                 positive = "stroke",
                 plots = "pr",
                 title = "Precision - Recall Curve",
                 gnames=c('caret_tree_better', 'nnet_both',
                          'train_rf_better', "train_fda_better"))




# _______________________########
# SELECTED MODELS A ########
# AUC Sens vs Spec >= 0.8  ########
# _______________________########

# OVER
# 1.caret_tree_over ####
y_hat_caret_tree_over

# 2.nnet_over ####
y_hat_nnet_over

# 3.fda_over ####
y_hat_fda_over

# 4.naiveBayes_over  ####
y_hat_naiveBayes_over

# 5.rf_both  ####
y_hat_rf_both

# 6.fda_both   ####
y_hat_fda_both

# 7.nnet_both   ####
y_hat_nnet_both 

# 8.naiveBayes_both   ####
y_hat_naiveBayes_both

#  9.gini_tree_cp0.001_better
y_hat_gini_tree_cp0.001_better

#  10.caret_tree_better   ####
y_hat_caret_tree_better

# 11.rf_better   ####
y_hat_rf_better

#  12.nnet_better   ####
y_hat_nnet_better

#  13.fda_better   ####
y_hat_fda_better

#  14.naiveBayes_better   ####
y_hat_naiveBayes_better

# _______________________########
# ENSAMBLE Models A ########
# _______________________########

ensamble_1_a <- data.frame(caret_tree_over = y_hat_caret_tree_over,
                           nnet_over = y_hat_nnet_over,
                           fda_over = y_hat_fda_over,
                           naiveBayes_over = y_hat_naiveBayes_over,
                           rf_both = y_hat_rf_both,
                           fda_both = y_hat_fda_both,
                           nnet_both = y_hat_nnet_both,
                           naiveBayes_both = y_hat_naiveBayes_both,
                           gini_tree_cp0.001_better = y_hat_gini_tree_cp0.001_better,
                           caret_tree_better = y_hat_caret_tree_better,
                           rf_better = y_hat_rf_better,
                           nnet_better = y_hat_nnet_better,
                           fda_better = y_hat_fda_better,
                           naiveBayes_better = y_hat_naiveBayes_better) 

ensamble_2_a <- ensamble_1_a %>% 
  mutate(caret_tree_over = ifelse(caret_tree_over == "stroke", 1,0),
         nnet_over = ifelse(nnet_over == "stroke", 1,0),
         fda_over = ifelse(fda_over == "stroke", 1,0),
         naiveBayes_over = ifelse(naiveBayes_over == "stroke", 1,0),
         rf_both = ifelse(rf_both == "stroke", 1,0),
         fda_both = ifelse(fda_both == "stroke", 1,0),
         nnet_both = ifelse(nnet_both == "stroke", 1,0),
         naiveBayes_both = ifelse(naiveBayes_both == "stroke", 1,0),
         gini_tree_cp0.001_better = ifelse(gini_tree_cp0.001_better == "stroke", 1,0),
         caret_tree_better = ifelse(caret_tree_better == "stroke", 1,0),
         rf_better = ifelse(rf_better == "stroke", 1,0),
         nnet_better = ifelse(nnet_better == "stroke", 1,0),
         fda_better = ifelse(fda_better == "stroke", 1,0),
         naiveBayes_better = ifelse(naiveBayes_better == "stroke", 1,0))

# Searching Best Cut-Off

cut_off_a <- seq(1:14)

ensamble_acc_a <- map_dbl(cut_off_a, function(cut_off_a){
  y_hat_ensamble <- data.frame(total = rowSums(ensamble_2_a)) %>% 
    mutate(total = as.factor(ifelse(total >= cut_off_a, "stroke", "no_stroke"))) %>% 
    pull(total) %>% relevel(ref = "stroke")
  confusionMatrix(y_hat_ensamble,
                  test_stroke_t$stroke)$overall["Accuracy"]
})

cut_of_acc_a <- data.frame(cut_off_a, ensamble_acc_a)
plot(cut_off_a, ensamble_acc_a, main = "Cut_Off vs Accuracy")

ensamble_sens_a <- map_dbl(cut_off_a, function(cut_off_a){
  y_hat_ensamble <- data.frame(total = rowSums(ensamble_2_a)) %>% 
    mutate(total = as.factor(ifelse(total >= cut_off_a, "stroke", "no_stroke"))) %>% 
    pull(total) %>% relevel(ref = "stroke")
  confmat <- confusionMatrix(y_hat_ensamble, test_stroke_t$stroke)
  confmat$byClass["Sensitivity"]
})

cut_of_sens_a <- data.frame(cut_off_a, ensamble_sens_a)
plot(cut_off_a, ensamble_sens_a, main = "Cut_Off vs Sensibility")

ensamble_spe_a <- map_dbl(cut_off_a, function(cut_off_a){
  y_hat_ensamble <- data.frame(total = rowSums(ensamble_2_a)) %>% 
    mutate(total = as.factor(ifelse(total >= cut_off_a, "stroke", "no_stroke"))) %>% 
    pull(total) %>% relevel(ref = "stroke")
  confmat <- confusionMatrix(y_hat_ensamble, test_stroke_t$stroke)
  confmat$byClass["Specificity"]
})

cut_of_spe_a <- data.frame(cut_off_a, ensamble_spe_a)
plot(cut_off_a, ensamble_spe_a, main = "Cut_Off vs Specificity")

ensamble_bal_a <- map_dbl(cut_off_a, function(cut_off_a){
  y_hat_ensamble <- data.frame(total = rowSums(ensamble_2_a)) %>% 
    mutate(total = as.factor(ifelse(total >= cut_off_a, "stroke", "no_stroke"))) %>% 
    pull(total) %>% relevel(ref = "stroke")
  confmat <- confusionMatrix(y_hat_ensamble, test_stroke_t$stroke)
  confmat$byClass["Balanced Accuracy"]
})

cut_of_bal_a <- data.frame(cut_off_a, ensamble_bal_a)
plot(cut_off_a, ensamble_bal_a, main = "Cut_Off vs Balanced Accuracy")

ensamble_F_Meas_a <- map_dbl(cut_off_a, function(cut_off_a){
  y_hat_ensamble <- data.frame(total = rowSums(ensamble_2_a)) %>% 
    mutate(total = as.factor(ifelse(total >= cut_off_a, "stroke", "no_stroke"))) %>% 
    pull(total) %>% relevel(ref = "stroke")
  F_meas(confusionMatrix(y_hat_ensamble,
                         test_stroke_t$stroke)$table,beta = 1)
})

cut_of_fmeas_a <- data.frame(cut_off_a, ensamble_F_Meas_a)
plot(cut_off_a, ensamble_F_Meas_a, main = "Cut_Off vs F_Meas")

y_hat_ensamble_a <- data.frame(total = rowSums(ensamble_2_a)) %>% 
  mutate(total = as.factor(ifelse(total >=4, "stroke", "no_stroke"))) %>% 
  pull(total) %>% relevel(ref = "stroke")

# Model Evaluation ::::::::

confusionMatrix(y_hat_ensamble_a,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_ensamble_a,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_ensamble_a,
                test_stroke_t$stroke)$table

cm_ensamble_a <- confusionMatrix(y_hat_ensamble_a, test_stroke_t$stroke)
cm_ensamble_a

F_meas(confusionMatrix(y_hat_ensamble_a,
                       test_stroke_t$stroke)$table,beta = 1)

# __Accuracy : 0.7525   ##### 
# __Sensitivity : 0.88095    #####         
# __Specificity : 0.74681  ##### 
# __Balanced Accuracy : 0.81388  ##### 
# __F_meas, beta = 1 : 0.2334385 #####


# _______________________########
# SELECTED MODELS B ########
# Balanced Accuracy >= 0.75  ########
# _______________________########

# 1.gini_tree_cp0.01_over ######
# 2.cost_matrix_tree_over ######
# 3.caret_tree_over ######
# 4.nnet_over ######
# 5.gini_tree_cp0.01_both ######
# 6.caret_tree_both  ###### 
# 7.nnet_both ######
# 8.fda_both ######
# 9.gini_tree_cp0.01_better ######
# 10.caret_tree_better  ######
# 11.fda_better ######

# _______________________########
# ENSAMBLE Models B ########
# _______________________########

ensamble_1_b <- data.frame(gini_tree_cp0.01_over = y_hat_gini_tree_cp0.01_over,
                           cost_matrix_tree_over = y_hat_cost_matrix_tree_over,
                           caret_tree_over = y_hat_caret_tree_over,
                           nnet_over = y_hat_nnet_over,
                           gini_tree_cp0.01_both = y_hat_gini_tree_cp0.01_both,
                           caret_tree_both = y_hat_caret_tree_both,
                           nnet_both = y_hat_nnet_both,
                           fda_both = y_hat_fda_both,
                           gini_tree_cp0.01_better = y_hat_gini_tree_cp0.01_better,
                           caret_tree_better = y_hat_caret_tree_better,
                           fda_better = y_hat_fda_better) 

ensamble_2_b <- ensamble_1_b %>% 
  mutate(gini_tree_cp0.01_over = ifelse(gini_tree_cp0.01_over == "stroke", 1,0),
         cost_matrix_tree_over = ifelse(cost_matrix_tree_over == "stroke", 1,0),
         caret_tree_over = ifelse(caret_tree_over == "stroke", 1,0),
         nnet_over = ifelse(nnet_over == "stroke", 1,0),
         gini_tree_cp0.01_both = ifelse(gini_tree_cp0.01_both == "stroke", 1,0),
         caret_tree_both = ifelse(caret_tree_both == "stroke", 1,0),
         nnet_both = ifelse(nnet_both == "stroke", 1,0),
         fda_both = ifelse(fda_both == "stroke", 1,0),
         gini_tree_cp0.01_better = ifelse(gini_tree_cp0.01_better == "stroke", 1,0),
         caret_tree_better = ifelse(caret_tree_better == "stroke", 1,0),
         fda_better = ifelse(fda_better == "stroke", 1,0))


# Searching Best Cut-Off

cut_off_b <- seq(1:11)

ensamble_acc_b <- map_dbl(cut_off_b, function(cut_off_b){
  y_hat_ensamble <- data.frame(total = rowSums(ensamble_2_b)) %>% 
    mutate(total = as.factor(ifelse(total >= cut_off_b, "stroke", "no_stroke"))) %>% 
    pull(total) %>% relevel(ref = "stroke")
  confusionMatrix(y_hat_ensamble,
                  test_stroke_t$stroke)$overall["Accuracy"]
})

cut_of_acc_b <- data.frame(cut_off_b, ensamble_acc_b)
plot(cut_off_b, ensamble_acc_b, main = "Cut_Off vs Accuracy")

ensamble_sens_b <- map_dbl(cut_off_b, function(cut_off_b){
  y_hat_ensamble <- data.frame(total = rowSums(ensamble_2_b)) %>% 
    mutate(total = as.factor(ifelse(total >= cut_off_b, "stroke", "no_stroke"))) %>% 
    pull(total) %>% relevel(ref = "stroke")
  confmat <- confusionMatrix(y_hat_ensamble, test_stroke_t$stroke)
  confmat$byClass["Sensitivity"]
})

cut_of_sens_b <- data.frame(cut_off_b, ensamble_sens_b)
plot(cut_off_b, ensamble_sens_b, main = "Cut_Off vs Sensitivity")

ensamble_spe_b <- map_dbl(cut_off_b, function(cut_off_b){
  y_hat_ensamble <- data.frame(total = rowSums(ensamble_2_b)) %>% 
    mutate(total = as.factor(ifelse(total >= cut_off_b, "stroke", "no_stroke"))) %>% 
    pull(total) %>% relevel(ref = "stroke")
  confmat <- confusionMatrix(y_hat_ensamble, test_stroke_t$stroke)
  confmat$byClass["Specificity"]
})

cut_of_spe_b <- data.frame(cut_off_b, ensamble_spe_b)
plot(cut_off_b, ensamble_spe_b, main = "Cut_Off vs Specificity")

ensamble_bal_b <- map_dbl(cut_off_b, function(cut_off_b){
  y_hat_ensamble <- data.frame(total = rowSums(ensamble_2_b)) %>% 
    mutate(total = as.factor(ifelse(total >= cut_off_b, "stroke", "no_stroke"))) %>% 
    pull(total) %>% relevel(ref = "stroke")
  confmat <- confusionMatrix(y_hat_ensamble, test_stroke_t$stroke)
  confmat$byClass["Balanced Accuracy"]
})

cut_of_bal_b <- data.frame(cut_off_b, ensamble_bal_b)
plot(cut_off_b, ensamble_bal_b, main = "Cut_Off vs Balanced Accuracy")

ensamble_F_Meas_b <- map_dbl(cut_off_b, function(cut_off_b){
  y_hat_ensamble <- data.frame(total = rowSums(ensamble_2_b)) %>% 
    mutate(total = as.factor(ifelse(total >= cut_off_b, "stroke", "no_stroke"))) %>% 
    pull(total) %>% relevel(ref = "stroke")
  F_meas(confusionMatrix(y_hat_ensamble,
                         test_stroke_t$stroke)$table,beta = 1)
})

cut_of_fmeas_b <- data.frame(cut_off_b, ensamble_F_Meas_b)
plot(cut_off_b, ensamble_F_Meas_b, main = "Cut_Off vs F_Meas")

y_hat_ensamble_b <- data.frame(total = rowSums(ensamble_2_b)) %>% 
  mutate(total = as.factor(ifelse(total >=3, "stroke", "no_stroke"))) %>% 
  pull(total) %>% relevel(ref = "stroke")

# Model Evaluation ::::::::

confusionMatrix(y_hat_ensamble_b,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_ensamble_b,
                test_stroke_t$stroke)$overall

confusionMatrix(y_hat_ensamble_b,
                test_stroke_t$stroke)$table

cm_ensamble_b <- confusionMatrix(y_hat_ensamble_b, test_stroke_t$stroke)
cm_ensamble_b

F_meas(confusionMatrix(y_hat_ensamble_b,
                       test_stroke_t$stroke)$table,beta = 1)

# __Accuracy : 0.7149    ##### 
# __Sensitivity : 0.90476    #####         
# __Specificity : 0.70638  ##### 
# __Balanced Accuracy : 0.80557  ##### 
# __F_meas, beta = 1 : 0.2134831 #####

# _______________________########
# Resources ########
# _______________________########

# http://topepo.github.io/caret/model-training-and-tuning.html 
# https://bookdown.org/content/2274/portada.html
# https://www.cienciadedatos.net/documentos/41_machine_learning_con_r_y_caret#Redes_neuronales_(NNET)
# https://rpubs.com/rdelgado/397838
# http://www.scielo.org.mx/scielo.php?script=sci_arttext&pid=S1405-77432017000400457&lng=es&nrm=iso
# https://www.socr.umich.edu/people/dinov/courses/DSPA_notes/14_ImprovingModelPerformance.html
# https://csantill.github.io/RTuningModelParameters/
# http://datamining.togaware.com/survivor/Tuning_rpart.html
# https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
# https://machinelearningmastery.com/cost-sensitive-learning-for-imbalanced-classification/
# https://bmcbioinformatics.biomedcentral.com/track/pdf/10.1186/1471-2105-10-S1-S22.pdf
# https://mlr.mlr-org.com/articles/tutorial/cost_sensitive_classif.html
# https://www.rdocumentation.org/packages/DMwR/versions/0.4.1/topics/SMOTE# https://cran.r-project.org/web/packages/costsensitive/costsensitive.pdf
# https://intobioinformatics.wordpress.com/2019/11/26/how-to-easily-make-a-roc-curve-in-r/
# https://www.learnbymarketing.com/tutorials/rpart-decision-trees-in-r/
# https://www.learnbymarketing.com/methods/classification-and-regression-decision-trees-explained/
# https://www.learnbymarketing.com/481/decision-tree-flavors-gini-info-gain/
# https://machinelearningmastery.com/non-linear-classification-in-r/
# http://www.personal.psu.edu/jol2/course/stat597e/notes2/mda.pdf
# https://www.r-bloggers.com/2013/07/a-brief-look-at-mixture-discriminant-analysis/
# https://discuss.datasciencedojo.com/t/how-can-i-perform-cross-validation-using-rpart-package-on-titanic-dataset/138
# http://www.personal.psu.edu/jol2/course/stat597e/notes2/mda.pdf
# https://stats.stackexchange.com/questions/275652/rpart-cross-validation
# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
# https://www.iartificial.net/precision-recall-f1-accuracy-en-clasificacion/
# https://developers.google.com/machine-learning/crash-course/classification/roc-and-auc?hl=en
# https://www.cienciadedatos.net/documentos/41_machine_learning_con_r_y_caret
# https://www.who.int/news-room/fact-sheets/detail/the-top-10-causes-of-death
# https://www.r-graph-gallery.com/301-custom-lollipop-chart.html
# https://topepo.github.io/caret/train-models-by-tag.html#Two_Class_Only
# http://ww.web.stanford.edu/~hastie/Papers/fda.pdf

 













