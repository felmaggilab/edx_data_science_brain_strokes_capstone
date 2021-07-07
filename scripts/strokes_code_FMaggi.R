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
library(MLeval)

# _______________________########
# DATA DOWNLOAD ########
# _______________________########

# stroke_data from github #####
stroke_data <- 
  read_csv("https://raw.github.com/felmaggilab/edx_data_science_capstone_strokes/master/data/healthcare-dataset-stroke-data.csv")

str(stroke_data)
head(stroke_data)

# Categorical and binary data as.factors #####
# BMI as.numeric (NAs introduced by coercion)

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
stroke_data$stroke <- relevel(stroke_data$stroke, ref = "stroke")

head(stroke_data)
str(stroke_data)


# _______________________########
# EXPLORATORY DATA ANALYSIS ########
# _______________________########

#Number of observations
n <- nrow(stroke_data)
# 4909

# Number of strokes = 1 in data set ####
sum(stroke_data$stroke == "stroke") 
# 209

# Number of strokes = 0 in data set ####
sum(stroke_data$stroke == "no_stroke") 
# 4700

# Number of gender = Male in data set ####
n_males <- sum(stroke_data$gender == "Male") 
# 2011

# Number of gender = Female in data set ####
n_females <- sum(stroke_data$gender == "Female") 
# 2897

# Number of gender = Other in data set ####
sum(stroke_data$gender == "Other") 
# 1

# Percent of strokes = 1 in data set ####
mean(stroke_data$stroke == "stroke") 
# 0.04257486

# Percent of strokes = 0 in data set ####
mean(stroke_data$stroke == "no_stroke") 
# 0.9574251

# Percent of gender = Male in data set ####
mean(stroke_data$gender == "Male") 
# 0.4096557

# Percent of gender = Female in data set ####
mean(stroke_data$gender == "Female") 
# 0.5901406

# Percent of gender = Other in data set ####
mean(stroke_data$gender == "Other") 
# 0.0002037075

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

# Summary table by gender #####
# gender, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(gender) %>%
  summarise(total = n(), percent = round(total/n, 3), 
            strokes = sum(stroke == "stroke"), 
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Summary table by age #####
# age, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(age) %>%
  summarise(total = n(), percent = round(total/n, 3), 
            strokes = sum(stroke == "stroke"), 
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Summary table by age (filtering only positive strokes cases) #####
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

# Summary table by age (rounded nearest 10) #####
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

# Summary table by hypertension #####
# hypertension, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(hypertension) %>%
  summarise( total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
             stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Summary table by heart_disease #####
# heart_disease, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(heart_disease) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke =="stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Summary table by ever_married #####
# ever_married, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(ever_married) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Summary table by work_type #####
# work_type, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(work_type) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Summary table by Residence_type #####
# age, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(Residence_type) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Summary table by avg_glucose_level (round to nearest ten) #####
# avg_glucose_level, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(round(avg_glucose_level, -1)) %>%
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

# Summary table by bmi (round to nearest ten) #####
# bmi, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  filter(!bmi == "N/A") %>% 
  mutate(bmi = as.numeric(bmi)) %>% 
  group_by(round(bmi,-1)) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# __Distribution of observations by rounded bmi  #####
stroke_data %>% 
  filter(!bmi == "N/A") %>% 
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
  filter(!bmi == "N/A") %>% 
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
  filter(!bmi == "N/A") %>% 
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

# Summary table by smoking_status  #####
# smoking_status, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  group_by(smoking_status) %>%
  summarise(total = n(), percent = round(total/n, 3), strokes = sum(stroke == "stroke"), 
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Summary table by age - Males (rounded) #####
# age, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  filter(gender == "Male") %>% 
  group_by(round(age, -1)) %>%
  summarise(total = n(), percent = round(total/n_males, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Summary table by age - Females (rounded) #####
# age, total of observations, number of strokes, percent of strokes
stroke_data %>% 
  filter(gender == "Female") %>% 
  group_by(round(age, -1)) %>%
  summarise(total = n(), percent = round(total/n_females, 2), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

stroke_data %>% 
  mutate(stroke = as.factor(stroke)) %>% 
  ggplot(aes(age, avg_glucose_level, color = stroke)) +
  geom_point()

stroke_data %>% 
  mutate(stroke = as.factor(stroke), bmi = as.numeric(bmi)) %>% 
  ggplot(aes(bmi, avg_glucose_level, color = stroke)) +
  geom_point()

stroke_data %>% 
  mutate(stroke = as.factor(stroke), bmi = as.numeric(bmi)) %>% 
  ggplot(aes(age, bmi, color = stroke)) +
  geom_point()

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

# PreProcessing: Centering and Scaling numerical variables ######
#(t stands for transformed)#####

preProcValues <- preProcess(train_stroke, method = c("center", "scale"))

# __train_stroke_t ####
train_stroke_t <- predict(preProcValues, train_stroke)

# __test_stroke_t ####
test_stroke_t <- predict(preProcValues, test_stroke)

# Review #####

# __Percent of strokes = 1 in train set ####
mean(train_stroke_t$stroke == "stroke") 
# 0.0425261

# __Percent of strokes = 1 in test set ####
mean(test_stroke_t$stroke == "stroke") 
# 0.04257486

# __Percent of males in train set ####
mean(train_stroke$gender == "Male")

# __Percent of females in train set ####
mean(train_stroke$gender == "Female")

# __Percent of males in test set ####
mean(test_stroke$gender == "Male")

# __Percent of females in test set ####
mean(test_stroke$gender == "Female")

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

# _______________________########
# TREES (rpart): Loking for variables  ########
# _______________________########

# Rpart tunning parameters ######
minsplit_tune <- tune.rpart(stroke ~ .,
                            data = train_stroke_t, 
                            minsplit = seq(1,100,5))
plot(minsplit_tune, main = "tune minsplit")

cp_tune <- tune.rpart(stroke ~ .,
                      data = train_stroke_t, 
                      cp = seq(0.000, 0.02, len = 50))
plot(cp_tune, main = "tune cp")

cp_tune <- train(stroke ~ ., 
                 method = "rpart",
                 tuneGrid = data.frame(cp = seq(0.000, 0.02, len = 50)),
                 data = train_stroke_t)
plot(cp_tune)

maxdepth_tune <- tune.rpart(stroke ~ .,
                            data = train_stroke_t, 
                            maxdepth = 1:30)
plot(maxdepth_tune, main = "tune maxdepth")

# All Data #####

# __Gini Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
rpart.plot(rpart(stroke ~ ., 
                 data = train_stroke)) # Default rpart tree


# __Gini Tree, CP = 0.001, minsplit = 20, minbucket round 20/3, maxdepht = 30 #####
rpart.plot(rpart(stroke ~., 
                 data = train_stroke, 
                 parms=list(split=c("gini")),
                 cp = 0.001))

# __Gini Tree, CP = 0.0024, minslit = 20, minbucket round 20/3, maxdepht = 30 ######
rpart.plot(rpart(stroke ~., 
                 data = train_stroke, 
                 parms=list(split=c("gini")),
                 cp = 0.0024))

# __Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 5 #####
rpart.plot(rpart(stroke ~., 
                 data = train_stroke, 
                 parms=list(split=c("gini")),
                 cp = 0.001,
                 maxdepth = 5))

# __Information Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
rpart.plot(rpart(stroke ~., 
                 data = train_stroke, 
                 parms=list(split=c("information"))))

# __Information Tree, CP = 0.001. minslit = 20, minbucket round 20/3, maxdepth = 30 #####
rpart.plot(rpart(stroke ~., 
                 data = train_stroke, 
                 parms=list(split=c("information")),
                 cp = 0.001))

# __Information Tree, CP = 0.0023. minslit = 20, minbucket round 20/3, maxdepht = 30 #####
rpart.plot(rpart(stroke ~., 
                 data = train_stroke, 
                 parms=list(split=c("information")),
                 cp = 0.0023))

# __Information Tree, CP = 0.0023. minslit = 20, minbucket round 20/3, maxdepht = 6 #####
rpart.plot(rpart(stroke ~., 
                 data = train_stroke, 
                 parms=list(split=c("information")),
                 cp = 0.0023,
                 maxdepth = 6))

inf_tree_cp0.001_max6 <- rpart(stroke ~., 
                           data = train_stroke_t, 
                           parms=list(split=c("information")),
                           cp = 0.0023,
                           maxdepth = 6)


#Categorical Data and Binary Data #####
train_stroke_categorical <- train_stroke %>% 
  select(gender, hypertension, heart_disease, ever_married, work_type,
         Residence_type, smoking_status, stroke)

# __Default Tree: Gini, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
default_tree_cat <- rpart(stroke ~ ., 
                      data = train_stroke_categorical)
rpart.plot(default_tree_cat)

# __Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
gini_tree_cp0.001_cat <- rpart(stroke ~., 
                           data = train_stroke_categorical, 
                           parms=list(split=c("gini")),
                           cp = 0.001)
rpart.plot(gini_tree_cp0.001_cat)
# __Information Tree, CP = 0.001. minslit = 20, minbucket round 20/3, maxdepht = 30 #####

inf_tree_cp0.001_cat <- rpart(stroke ~., 
                          data = train_stroke_categorical, 
                          parms=list(split=c("information")),
                          cp = 0.001)
rpart.plot(inf_tree_cp0.001_cat)


#Numerical Data #####
train_stroke_numerical <- stroke_data %>% 
  select(age, avg_glucose_level, bmi, stroke)

# __Default Tree: Gini, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
default_tree_num <- rpart(stroke ~ ., 
                          data = train_stroke_numerical)
rpart.plot(default_tree_num)

# __Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
gini_tree_cp0.001_num <- rpart(stroke ~., 
                               data = train_stroke_numerical, 
                               parms=list(split=c("gini")),
                               cp = 0.001)
rpart.plot(gini_tree_cp0.001_num)

# __Information Tree, CP = 0.001. minslit = 20, minbucket round 20/3, maxdepht = 30 #####
inf_tree_cp0.001_num <- rpart(stroke ~., 
                              data = train_stroke_numerical, 
                              parms=list(split=c("information")),
                              cp = 0.001)
rpart.plot(inf_tree_cp0.001_num)


# _______________________########
# TRAIN CONTOL  ########
# For Caret Trains ########
# _______________________########

ctrl <- trainControl(method="repeatedcv", 
                     number = 10,
                     repeats = 5, summaryFunction=twoClassSummary, classProbs=T,
                     savePredictions = T)


# _______________________########
# ORIGINAL DATA ########
# Imbalanced ########
# _______________________########

# RPART native#######
# Recursive Partitioning and Regression Trees ######

# Gini, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
gini_tree_cp0.01 <- rpart(stroke ~ ., 
                      data = train_stroke_t) # Default rpart tree

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
gini_tree_cp0.001 <- rpart(stroke ~., 
                           data = train_stroke_t, 
                           parms=list(split=c("gini")),
                           cp = 0.001)

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

# Review F_meas function
recall <- 3/(3 + 11)
precision <- 3/(3+39)
beta <- 1

(1+beta^2) * precision * recall/((beta^2 * precision) + recall) # OK

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

# __Accuracy : 0.9491 ####
# __Sensitivity : 0.071429  ####
# __Specificity : 0.988298  ####
# __Balanced Accuracy : 0.529863 ####
# __F_meas, beta = 1 : 0.1071429 ####
# __AUC Sens vs Spec : 0.7301 #####

# Gini Tree, CP = 0.0024, minslit = 20, minbucket round 20/3, maxdepht = 30 ######
# Same results Gini CP 0.001
gini_tree_cp0.0024 <- rpart(stroke ~., 
                            data = train_stroke_t, 
                            parms=list(split=c("gini")),
                            cp = 0.0024)

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

# __Accuracy : 0.9491 ######
# __Sensitivity : 0.071429 ######
# __Specificity : 0.988298  ######
# __Balanced Accuracy : 0.529863 ######
# __F_meas, beta = 1 : 0.1071429 #######
# __AUC Sens vs Spec : 0.7301 #####

# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 5 ######
gini_tree_cp0.001_max5 <- rpart(stroke ~., 
                                data = train_stroke_t, 
                                parms=list(split=c("gini")),
                                cp = 0.001,
                                maxdepth = 5)

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
# __AUC Sens vs Spec :  0.7385 ??????? #####

# Information Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
inf_tree <- (rpart(stroke ~., 
                   data = train_stroke_t, 
                   parms=list(split=c("information"))))

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
inf_tree_cp0.001 <- rpart(stroke ~., 
                          data = train_stroke_t, 
                          parms=list(split=c("information")),
                          cp = 0.001)

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

# __Accuracy : 0.945  #####
# __Sensitivity : 0.071429   #####
# __Specificity : 0.984043  #####
# __Balanced Accuracy : 0.527736  #####
# __F_meas, beta = 1 : 0.1 #####
# __AUC Sens vs Spec :  0.7719 #####

# Information Tree, CP = 0.0023. minslit = 20, minbucket round 20/3, maxdepht = 30 #####
inf_tree_cp0.0023 <- rpart(stroke ~., 
                           data = train_stroke_t, 
                           parms=list(split=c("information")),
                           cp = 0.0023)

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

# __Accuracy : 0.9481  #####
# __Sensitivity : 0.071429   #####        
# __Specificity : 0.987234 #####
# __Balanced Accuracy : 0.529331  #####
# __F_meas, beta = 1 : 0.1052632 #####
# __AUC Sens vs Spec : 0.7867 #####


# RPART caret ######
train_caret_tree <- train(stroke ~ ., method = "rpart", data = train_stroke_t, 
                          trControl = ctrl)

y_hat_caret_tree <- predict(train_caret_tree, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_caret_tree, test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_caret_tree, test_stroke_t$stroke)$table

cm_caret_tree <- confusionMatrix(y_hat_train_caret_tree, test_stroke_t$stroke)
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
roc_tp_fp_caret_tree <- evalm(list(train_knn_under), 
                                   positive = "stroke",
                                   plots = "r",
                                   title = "True Positive - False Positive rate ROC",
                                   gnames=c('CARET Tree'))

# __Accuracy : 0.9501  ######
# __Sensitivity : 0.095238 #####         
# __Specificity : 0.988298 #####
# __Balanced Accuracy : 0.541768  #####
# __F_meas, beta = 0.1403509  #####
# __AUC Sens vs Spec :  0.7403 #####

# KNN ########
# K-Nearest-Neighbor #######
train_knn <- train(stroke ~ ., method = "knn", 
                   data = train_stroke_t, 
                   trControl = ctrl)

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
# __AUC Sens vs Spec : 0.6503 #####


# RANDOM FOREST ########
# it takes time! ######
train_rf <- train(stroke ~ ., method = "rf", 
                  data = train_stroke_t, 
                  trControl = ctrl)

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



# __Accuracy : 0.9572  ####
# __Sensitivity : 0.0000     ####     
# __Specificity : 1.0000   ####
# __Balanced Accuracy : 0.5000  ####
# __F_meas, beta = 1 : NA #####
# __AUC Sens vs Spec :  0.78 ???? #####


# FDA #####
# Flexible Discriminant Analysis #####
train_fda <- train(stroke ~ ., method = "fda", 
                        data = train_stroke_t, trControl = ctrl)

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

# __Accuracy : 0.9379  ####
# __Sensitivity : 0.14286 ####        
# __Specificity : 0.97340 ####  
# __Balanced Accuracy : 0.55813   ####
# __F_meas, beta = 1 : 0.1643836 #####
# # __AUC Sens vs Spec : 0.8589 ###### 

# _______________________########
# BALANCED DATA ########
# _______________________######## 

# _______________________######## 
# Oversampling #####
# _______________________######## 

table(train_stroke_t$stroke)
prop.table(table(train_stroke_t$stroke))

n_over = sum(train_stroke_t$stroke == "no_stroke")

set.seed(1969, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1969)`
# Every time we run the code, we get a different ovun.sample
# Visualization tree, Accuracy, Sensibility, Balanced accuracy and F.meas strongly depends on this ramdom process.

train_stroke_over <- ovun.sample(stroke ~ ., data = train_stroke_t, method = "over", N = n_over*2)$data
table(train_stroke_over$stroke)

str(train_stroke_over)

# Relevel "stroke" "no_stroke" factors: positive class: "stroke" #### 
train_stroke_over$stroke <- relevel(train_stroke_over$stroke, ref = "stroke")


# RPART native #######
# Recursive Partitioning and Regression Trees ######

# Gini Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
gini_tree_cp0.01_over <- rpart(stroke ~ ., 
                      data = train_stroke_over)

plotcp(gini_tree_cp0.01_over)
summary(gini_tree_cp0.01_over)

rpart.plot(gini_tree_cp0.01_over)

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

# __Accuracy : 0.7515  ####
# __Sensitivity : 0.73810   ####         
# __Specificity : 0.75213 ####
# __Balanced Accuracy : 0.74511  ####
# __F_meas, beta = 1 : 0.2026144   ####
# __AUC Sens vs Spec : 0.7384 ######

# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
gini_tree_cp0.001_over <- rpart(stroke ~., 
                           data = train_stroke_over, 
                           parms=list(split=c("gini")),
                           cp = 0.001)

plotcp(gini_tree_cp0.001_over)
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

# __Accuracy : 0.8982  ####
# __Sensitivity : 0.42857 ####       
# __Specificity : 0.91915 ####
# __Balanced Accuracy : 0.67386 ####
# __F_meas, beta = 1 : 0.1960784  ####
# __AUC Sens vs Spec : 0.3733 ######

# Default Gini Tree & Cost Matrix 3 to 1 #####
cost_matrix_tree_over <- rpart(stroke ~ ., 
                               data = train_stroke_over,
                               parms=list(
                                 loss=matrix(c(0,3,1,0), # A false negative is 3 times worse than a false positive
                                             byrow=TRUE,
                                             nrow=2)))

plotcp(cost_matrix_tree_over)
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

# __Accuracy : 0.6202   ##### 
# __Sensitivity : 0.92857 !!!! #####         
# __Specificity : 0.60638#####  
# __Balanced Accuracy : 0.76748 #####
# __F_meas, beta = 1 : 0.172949 #####
# __AUC Sens vs Spec : 0.7584 ######


# RPART caret ####
train_caret_tree_over <- train(stroke ~ ., method = "rpart",
                               data = train_stroke_over, 
                               trControl = ctrl)

plot(train_caret_tree_over)
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

# __Accuracy : 0.8045   ####
# __Sensitivity : 0.71429      ####        
# __Specificity : 0.80851  ####
# __Balanced Accuracy : 0.76140   ####
# __F_meas, beta = 1 : 0.2380952  #####
# __AUC Sens vs Spec : 0.8067 ######


#KNN caret #######
# K-Nearest-Neighbor #######
train_knn_over <- train(stroke ~ ., method = "knn", 
                   data = train_stroke_over, 
                   trControl = ctrl)

plot(train_knn_over)
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

# __Accuracy : 0.8035   ##### 
# __Sensitivity : 0.38095  #####         
# __Specificity : 0.82234  ##### 
# __Balanced Accuracy : 0.60165   #####
# __F_meas, beta = 1 : 0.1422222 #####
# __AUC Sens vs Spec : 0.4003 ######

# RANDOM FOREST ########
# it takes time! ######

train_rf_over <- train(stroke ~ ., method = "rf", 
                  data = train_stroke_over, 
                  trControl = ctrl)

plot(train_rf_over)
summary(train_knn_over)

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

# __Accuracy : 0.9501   ##### 
# __Sensitivity : 0.071429  #####         
# __Specificity : 0.989362   ##### 
# __Balanced Accuracy : 0.530395  ##### 
# __F_meas, beta = 1 : 0.1090909 #####
# # __AUC Sens vs Spec : 0.7912 ??????? ###### 

# MDA: EVAL ERROR ######
# Mixture Discriminant Analysis ####
set.seed(1970, sample.kind="Rounding") 
# Results are random variables

train_mda_over <- train(stroke ~ ., method = "mda", 
                        data = train_stroke_over,
                        trControl = ctrl) # Several Warnings

plot(train_mda_over)
print(train_mda_over)
summary(train_mda_over)

y_hat_mda_over <- predict(train_mda_over, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_mda_over,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_mda_over,
                test_stroke$stroke)$overall

confusionMatrix(y_hat_mda_over,
                test_stroke_t$stroke)$table

cm_mda_over <- confusionMatrix(y_hat_mda_over, test_stroke_t$stroke, 
                                     positive = "stroke")
cm_mda_over

# F_Meas

F_meas(confusionMatrix(y_hat_mda_over,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_mda_over <- predict(train_mda_over,
                             test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_mda_over <-plot(roc(response = test_stroke_t$stroke, 
         predictor = roc_mda_over $stroke, levels = c("stroke", "no_stroke")), 
     print.auc = TRUE,
     xlim = c(1,0),
     ylim = c(0,1),
     xlab = "Specificity", 
     ylab ="Sensibility", 
     main = "MDA - Over")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_mda_over)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_mda_over <- evalm(list(train_mda_over), 
                              positive = "stroke",
                              gnames=c('MDA - Over')) 

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

# __Accuracy : 0.7454   ##### 
# __Sensitivity : 0.78571    #####        
# __Specificity : 0.74362    ##### 
# __Balanced Accuracy : 0.76467     ##### 
# __F_meas, beta = 1 : 0.2088608  #####
# __AUC Sens vs Spec : 0.8466 ###### 

# NNET: great variability !!!#####
# Neural Network ####
# It takes some time!

set.seed(2, sample.kind="Rounding") 
# Results are random variables: great variability!!!

train_nnet_over <- train(stroke ~ ., method = "nnet",
                         data = train_stroke_over,
                         trControl = ctrl)

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

# __Accuracy : 0.7892 ####
# __Sensitivity : 0.61905 ####        
# __Specificity : 0.79681  ####
# __Balanced Accuracy : 0.70793   ####
# __F_meas, beta = 1 : 0.2007722 #####
# __AUC Sens vs Spec 0.7989 ###### 

# FDA #####
# Flexible Discriminant Analysis #####
train_fda_over <- train(stroke ~ ., method = "fda", 
                        data = train_stroke_over, trControl = ctrl)

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

# __Accuracy : 0.7342   ####
# __Sensitivity : 0.73810  ####        
# __Specificity : 0.73404 ####  
# __Balanced Accuracy : 0.73607   ####
# __F_meas, beta = 1 : 0.1919505 #####
# # __AUC Sens vs Spec : 0.8122 ###### 


# NAIVE BAYES ######
train_naiveBayes_over <- train(stroke ~ ., method = "naive_bayes", 
                               data = train_stroke_over, 
                               trControl = ctrl)

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


# __Accuracy : 0.9582  ##### 
# __Sensitivity : 0.047619  #####         
# __Specificity : 0.998936 ##### 
# __Balanced Accuracy : 0.523278  ##### 
# __F_meas, beta = 1 : 0.08888889 #####
# __AUC : 0.8485 ???? ######

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
gini_tree_cp0.01_both <- rpart(stroke ~ ., 
                               data = train_stroke_both)

plotcp(gini_tree_cp0.01_both)
summary(gini_tree_cp0.01_both)

rpart.plot(gini_tree_cp0.01_both)

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

# __Accuracy :   ####
# __Sensitivity :    ####         
# __Specificity :  ####
# __Balanced Accuracy :   ####
# __F_meas, beta = 1 :    ####
# __AUC Sens vs Spec :  ######

# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
gini_tree_cp0.001_both <- rpart(stroke ~., 
                                data = train_stroke_both, 
                                parms=list(split=c("gini")),
                                cp = 0.001)

plotcp(gini_tree_cp0.001_both)
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

# __Accuracy :   ####
# __Sensitivity :  ####       
# __Specificity :  ####
# __Balanced Accuracy :  ####
# __F_meas, beta = 1 :   ####
# __AUC Sens vs Spec :  ######

# Default Gini Tree & Cost Matrix 3 to 1 #####
cost_matrix_tree_both <- rpart(stroke ~ ., 
                               data = train_stroke_both,
                               parms=list(
                                 loss=matrix(c(0,3,1,0), # A false negative is 3 times worse than a false positive
                                             byrow=TRUE,
                                             nrow=2)))

plotcp(cost_matrix_tree_both)
summary(cost_matrix_tree_both)

y_hat_cost_matrix_tree_over <- predict(cost_matrix_tree_both, test_stroke_t, 
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
roc_cost_matrix_tree_over <- predict(cost_matrix_tree_both,
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

# __Accuracy :    ##### 
# __Sensitivity :  #####         
# __Specificity : #####  
# __Balanced Accuracy :  #####
# __F_meas, beta = 1 :  #####
# __AUC Sens vs Spec :  ######


# RPART caret ####
train_caret_tree_both <- train(stroke ~ ., method = "rpart",
                               data = train_stroke_both, 
                               trControl = ctrl)

plot(train_caret_tree_both)
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

# __Accuracy :    ####
# __Sensitivity :       ####        
# __Specificity :   ####
# __Balanced Accuracy :    ####
# __F_meas, beta = 1 :   #####
# __AUC Sens vs Spec :  ######


#KNN caret #######
# K-Nearest-Neighbor #######
train_knn_both <- train(stroke ~ ., method = "knn", 
                        data = train_stroke_both, 
                        trControl = ctrl)

plot(train_knn_both)
summary(train_knn_both)

y_hat_knn_over <- predict(train_knn_both, test_stroke_t)

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

# __Accuracy :    ##### 
# __Sensitivity :   #####         
# __Specificity :   ##### 
# __Balanced Accuracy :    #####
# __F_meas, beta = 1 :  #####
# __AUC Sens vs Spec :  ######

# RANDOM FOREST ########
# it takes time! ######

train_rf_both <- train(stroke ~ ., method = "rf", 
                       data = train_stroke_both, 
                       trControl = ctrl)

plot(train_rf_both)
summary(train_knn_both)

y_hat_rf_over <- predict(train_rf_both, test_stroke_t, type = "raw")

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

# __Accuracy :    ##### 
# __Sensitivity :   #####         
# __Specificity :    ##### 
# __Balanced Accuracy :   ##### 
# __F_meas, beta = 1 :  #####
# # __AUC Sens vs Spec :   ###### 

# MDA: EVAL ERROR ######
# Mixture Discriminant Analysis ####
set.seed(1970, sample.kind="Rounding") 
# Results are random variables

train_mda_both <- train(stroke ~ ., method = "mda", 
                        data = train_stroke_both,
                        trControl = ctrl) # Several Warnings

plot(train_mda_both)
print(train_mda_both)
summary(train_mda_both)

y_hat_mda_both <- predict(train_mda_both, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_mda_both,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_mda_both,
                test_stroke$stroke)$overall

confusionMatrix(y_hat_mda_both,
                test_stroke_t$stroke)$table

cm_mda_both<- confusionMatrix(y_hat_mda_both, test_stroke_t$stroke, 
                               positive = "stroke")
cm_mda_both

# F_Meas

F_meas(confusionMatrix(y_hat_mda_both,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_mda_both <- predict(train_mda_both,
                        test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_mdaboth <-plot(roc(response = test_stroke_t$stroke, 
                                   predictor = roc_mda_both$stroke, levels = c("stroke", "no_stroke")), 
                               print.auc = TRUE,
                               xlim = c(1,0),
                               ylim = c(0,1),
                               xlab = "Specificity", 
                               ylab ="Sensibility", 
                               main = "MDA - Over")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_mda_both)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_mda_both <- evalm(list(train_mda_both), 
                       positive = "stroke",
                       gnames=c('MDA - both')) 

# ERROR
# Error in names(x) <- value : 
# el atributo 'names' [1] debe tener la misma longitud que el vector [0]
# Además: Warning messages:
# 1: Removed 7520 row(s) containing missing values (geom_path). 
# 2: Removed 1 rows containing missing values (geom_segment). 
# 3: Removed 7520 row(s) containing missing values (geom_path). 
# 4: Removed 7520 row(s) containing missing values (geom_path). 


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

# __Accuracy :    ##### 
# __Sensitivity :     #####        
# __Specificity :    ##### 
# __Balanced Accuracy :      ##### 
# __F_meas, beta = 1 :   #####
# __AUC Sens vs Spec :  ###### 

# NNET: great variability !!!#####
# Neural Network ####
# It takes some time!

set.seed(2, sample.kind="Rounding") 
# Results are random variables: great variability!!!

train_nnet_both <- train(stroke ~ ., method = "nnet",
                         data = train_stroke_both,
                         trControl = ctrl)

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
roc_nnet_over <- predict(train_nnet_both,
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
                                 main = "NNET - Over")

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

# __Accuracy :  ####
# __Sensitivity :  ####        
# __Specificity :   ####
# __Balanced Accuracy :    ####
# __F_meas, beta = 1 :  #####
# __AUC Sens vs Spec  ###### 

# FDA #####
# Flexible Discriminant Analysis #####
train_fda_both <- train(stroke ~ ., method = "fda", 
                        data = train_stroke_both, trControl = ctrl)

y_hat_fda_over <- predict(train_fda_both, test_stroke_t)

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
                                main = "Flexible Discriminant Analysis - Over")

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

# __Accuracy :    ####
# __Sensitivity :   ####        
# __Specificity :  ####  
# __Balanced Accuracy :    ####
# __F_meas, beta = 1 :  #####
# # __AUC Sens vs Spec :  ###### 


# NAIVE BAYES ######
train_naiveBayes_both <- train(stroke ~ ., method = "naive_bayes", 
                               data = train_stroke_both, 
                               trControl = ctrl)

y_hat_naiveBayes_over <- predict(train_naiveBayes_both, test_stroke_t)

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

# __Accuracy :   ##### 
# __Sensitivity :   #####         
# __Specificity :  ##### 
# __Balanced Accuracy :   ##### 
# __F_meas, beta = 1 :  #####
# __AUC :  ######

# _______________________######## 
# Better estimates ######
# _______________________######## 

set.seed(1969, sample.kind="Rounding") # Every time we run the code, we get a different ovun.sample
# Accuracy and balanced accuracy strongly depends on this ramdom process.

train_stroke_better <- ROSE(stroke ~ ., data = train_stroke_t)$data
table(train_stroke_better$stroke)

str(train_stroke_better)

# Relevel "stroke" "no_stroke" factors: positive class: "stroke" #### 
train_stroke_better$stroke <- relevel(train_stroke_better$stroke, ref = "stroke")

# RPART native #######
# Recursive Partitioning and Regression Trees ######

# Gini Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
gini_tree_cp0.01_better <- rpart(stroke ~ ., 
                               data = train_stroke_better)

plotcp(gini_tree_cp0.01_better)
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

# __Accuracy :   ####
# __Sensitivity :    ####         
# __Specificity :  ####
# __Balanced Accuracy :   ####
# __F_meas, beta = 1 :    ####
# __AUC Sens vs Spec :  ######

# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
gini_tree_cp0.001_better <- rpart(stroke ~., 
                                data = train_stroke_better, 
                                parms=list(split=c("gini")),
                                cp = 0.001)

plotcp(gini_tree_cp0.001_better)
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

# __Accuracy :   ####
# __Sensitivity :  ####       
# __Specificity :  ####
# __Balanced Accuracy :  ####
# __F_meas, beta = 1 :   ####
# __AUC Sens vs Spec :  ######

# Default Gini Tree & Cost Matrix 3 to 1 #####
cost_matrix_tree_better <- rpart(stroke ~ ., 
                               data = train_stroke_better,
                               parms=list(
                                 loss=matrix(c(0,3,1,0), # A false negative is 3 times worse than a false positive
                                             byrow=TRUE,
                                             nrow=2)))

plotcp(cost_matrix_tree_better)
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

# __Accuracy :    ##### 
# __Sensitivity :  #####         
# __Specificity : #####  
# __Balanced Accuracy :  #####
# __F_meas, beta = 1 :  #####
# __AUC Sens vs Spec :  ######


# RPART caret ####
train_caret_tree_better<- train(stroke ~ ., method = "rpart",
                               data = train_stroke_better, 
                               trControl = ctrl)

plot(train_caret_tree_better)
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

# __Accuracy :    ####
# __Sensitivity :       ####        
# __Specificity :   ####
# __Balanced Accuracy :    ####
# __F_meas, beta = 1 :   #####
# __AUC Sens vs Spec :  ######


#KNN caret #######
# K-Nearest-Neighbor #######
train_knn_better <- train(stroke ~ ., method = "knn", 
                        data = train_stroke_better, 
                        trControl = ctrl)

plot(train_knn_better)
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

# __Accuracy :    ##### 
# __Sensitivity :   #####         
# __Specificity :   ##### 
# __Balanced Accuracy :    #####
# __F_meas, beta = 1 :  #####
# __AUC Sens vs Spec :  ######

# RANDOM FOREST ########
# it takes time! ######

train_rf_better <- train(stroke ~ ., method = "rf", 
                       data = train_stroke_better, 
                       trControl = ctrl)

plot(train_rf_better)
summary(train_knn_better)

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

# __Accuracy :    ##### 
# __Sensitivity :   #####         
# __Specificity :    ##### 
# __Balanced Accuracy :   ##### 
# __F_meas, beta = 1 :  #####
# # __AUC Sens vs Spec :   ###### 

# MDA: EVAL ERROR ######
# Mixture Discriminant Analysis ####
set.seed(1970, sample.kind="Rounding") 
# Results are random variables

train_mda_better <- train(stroke ~ ., method = "mda", 
                        data = train_stroke_better,
                        trControl = ctrl) # Several Warnings

plot(train_mda_better)
print(train_mda_better)
summary(train_mda_better)

y_hat_mda_both <- predict(train_mda_both, test_stroke_t)

# Model Evaluation ::::::::

confusionMatrix(y_hat_mda_better,
                test_stroke_t$stroke)$overall["Accuracy"]

confusionMatrix(y_hat_mda_better,
                test_stroke$stroke)$overall

confusionMatrix(y_hat_mda_better,
                test_stroke_t$stroke)$table

cm_mda_better <- confusionMatrix(y_hat_mda_both, test_stroke_t$stroke, 
                              positive = "stroke")
cm_mda_better

# F_Meas

F_meas(confusionMatrix(y_hat_mda_better,
                       test_stroke_t$stroke)$table, beta = 1)

# Calc. Probs for every class
roc_mda_both <- predict(train_mda_better,
                        test_stroke_t, type = "prob")

# Sensitivity vs Specificity ROC CURVE
sens_espec_roc_mda_better <-plot(roc(response = test_stroke_t$stroke, 
                                  predictor = roc_mda_better$stroke, 
                                  levels = c("stroke", "no_stroke")), 
                              print.auc = TRUE,
                              xlim = c(1,0),
                              ylim = c(0,1),
                              xlab = "Specificity", 
                              ylab ="Sensibility", 
                              main = "MDA - Over")

# Sensitivity vs Specificity AUC
auc(sens_espec_roc_mda_better)

# Calibration curve, Precision Recall, Precision vs Recall gain,
# True positive rate vs false positive rate
eval_mda_better <- evalm(list(train_mda_better), 
                       positive = "stroke",
                       gnames=c('MDA - better')) 

# ERROR
# Error in names(x) <- value : 
# el atributo 'names' [1] debe tener la misma longitud que el vector [0]
# Además: Warning messages:
# 1: Removed 7520 row(s) containing missing values (geom_path). 
# 2: Removed 1 rows containing missing values (geom_segment). 
# 3: Removed 7520 row(s) containing missing values (geom_path). 
# 4: Removed 7520 row(s) containing missing values (geom_path). 


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

# __Accuracy :    ##### 
# __Sensitivity :     #####        
# __Specificity :    ##### 
# __Balanced Accuracy :      ##### 
# __F_meas, beta = 1 :   #####
# __AUC Sens vs Spec :  ###### 

# NNET: great variability !!!#####
# Neural Network ####
# It takes some time!

set.seed(2, sample.kind="Rounding") 
# Results are random variables: great variability!!!

train_nnet_better <- train(stroke ~ ., method = "nnet",
                         data = train_stroke_better,
                         trControl = ctrl)

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

# __Accuracy :  ####
# __Sensitivity :  ####        
# __Specificity :   ####
# __Balanced Accuracy :    ####
# __F_meas, beta = 1 :  #####
# __AUC Sens vs Spec  ###### 

# FDA #####
# Flexible Discriminant Analysis #####
train_fda_better <- train(stroke ~ ., method = "fda", 
                        data = train_stroke_better, trControl = ctrl)

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
roc_fda_both <- predict(train_fda_better,
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
                       gnames=c('FDA - both'))


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

# __Accuracy :    ####
# __Sensitivity :   ####        
# __Specificity :  ####  
# __Balanced Accuracy :    ####
# __F_meas, beta = 1 :  #####
# # __AUC Sens vs Spec :  ###### 


# NAIVE BAYES ######
train_naiveBayes_better <- train(stroke ~ ., method = "naive_bayes", 
                               data = train_stroke_better, 
                               trControl = ctrl)

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
eval_naiveBayes_better <- evalm(list(train_naiveBayes_both), 
                              positive = "stroke",
                              gnames=c('Naive Bayes - better'))


# Calibration Curve (True prob. vs Predicted prob.)
cc_naiveBayes_better <- evalm(list(train_naiveBayes_both), 
                            positive = "stroke",
                            plots = "cc",
                            title = "Calibration Curve",
                            gnames=c('Naive Bayes - better'))

# Precision - Recall Gain
prg_naiveBayes_better <- evalm(list(train_naiveBayes_both), 
                             positive = "stroke",
                             plots = "prg",
                             title = "Precision - Recall Gain",
                             gnames=c('Naive Bayes - better'))

# Precision - Recall Curve
pr_naiveBayes_better <- evalm(list(train_naiveBayes_both), 
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

# __Accuracy :   ##### 
# __Sensitivity :   #####         
# __Specificity :  ##### 
# __Balanced Accuracy :   ##### 
# __F_meas, beta = 1 :  #####
# __AUC :  ######


# _______________________########
# SELECTED MODELS ########
# F_meas, beta = 1 >= 20  ########
# _______________________########







# _______________________########
# Resources ########
# _______________________########

# http://topepo.github.io/caret/model-training-and-tuning.html 
# https://bookdown.org/content/2274/portada.html
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


# Ver comentarios

# https://topepo.github.io/caret/train-models-by-tag.html#Two_Class_Only
# Cost-Sensitive CART

# method = 'rpartCost'
# Type: Classification

# Tuning parameters:
  
# cp (Complexity Parameter)
# Cost (Cost)
# Required packages: rpart, plyr

 













