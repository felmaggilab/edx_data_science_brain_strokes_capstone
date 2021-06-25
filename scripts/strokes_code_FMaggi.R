# _______________________########
# PACKAGES AND LIBRARIES ########
# _______________________########

#__reader #####
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
library(readr)

#__tidyverse #####
library(tidyverse)

#__caret #####
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)

#__ igraph ####
if(!require(igraph)) install.packages("igraph", repos = "http://cran.us.r-project.org")
library(igraph)

#__ igraph ####
if(!require(networkD3)) install.packages("networkD3", repos = "http://cran.us.r-project.org")
library(networkD3)

#__ rattle ####
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
library(rattle)

if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
library(randomForest)

if(!require(fastAdaboost)) install.packages("fastAdaboost", repos = "http://cran.us.r-project.org")
library(fastAdaboost)

if(!require(ROSE)) install.packages("ROSE", repos = "http://cran.us.r-project.org")
library(ROSE)

if(!require(mda)) install.packages("mda", repos = "http://cran.us.r-project.org")
library(mda)

if(!require(klaR)) install.packages("klaR", repos = "http://cran.us.r-project.org")
library(klaR)

if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
library(nnet)

if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
library(kernlab)

if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
library(e1071)

library(viridis)
library(patchwork)
library(hrbrthemes)
library(ggraph)
library(igraph)
library(networkD3)
library(tidyverse)    # includes readr
library(readxl)
library(knitr)
library(tidyr)
library(dplyr)
library(plotly)
library(magrittr)
library(lubridate)
library(rvest)
library(igraph)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)

# _______________________########
# DATA DOWNLOAD ########
# _______________________########

# stroke_data from github #####
stroke_data <- 
  read_csv("https://raw.github.com/felmaggilab/edx_data_science_capstone_strokes/master/data/healthcare-dataset-stroke-data.csv")

str(stroke_data)
head(stroke_data)

# Categorical and binari data as.factors #####
# BMI as.numeric (NAs intro by coercion)

stroke_data <- stroke_data %>% 
  filter(!bmi == "N/A")  %>% 
  mutate(gender = as.factor(gender), 
         ever_married = as.factor(ever_married),
         work_type = as.factor(work_type),
         Residence_type = as.factor(Residence_type),
         bmi = as.numeric(bmi),
         smoking_status = as.factor(smoking_status),
         stroke = as.factor(ifelse(stroke == 1, "stroke", "no_stroke"))) %>% 
  select(!id)

head(stroke_data)
str(stroke_data)


# _______________________########
# EXPLORATORY DATA ANALYSIS ########
# _______________________########

#Number of observations
n <- nrow(stroke_data)
# 4909

# Number of strokes = 1 in data set ####
sum(stroke_data$stroke == 1) 
# 209

4909 + 209

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
  summarise(total = n(), percent = round(total/n_females, 2), strokes = sum(stroke == 1),
            stroke_percent = round(mean(stroke == 1), 3)) %>% 
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

# We will use 80% of data to train, and 20% of data to test.
set.seed(1970, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1970)`
test_index <- createDataPartition(y = stroke_data$stroke, times = 1, p = 0.2,
                                  list = FALSE)

train_stroke <- stroke_data[-test_index,]

test_stroke <- stroke_data[test_index,]


# Percent of strokes = 1 in train set ####
mean(train_stroke$stroke == "stroke") 
# 0.0425261

# Percent of strokes = 1 in test set ####
mean(test_stroke$stroke == "stroke") 
# 0.04257486

# Percent of males in train set ####
mean(train_stroke$gender == "Male")

# Percent of females in train set ####
mean(train_stroke$gender == "Female")

# Percent of males in test set ####
mean(test_stroke$gender == "Male")

# Percent of females in test set ####
mean(test_stroke$gender == "Female")

# Distribution of observations by age train #####
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

# Distribution of observations by age test #####
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

# Percent of strokes by rounded age train ####  

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

# Percent of strokes by rounded age test ####  

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

# Summary table by age (rounded nearest 10) train #####
# age, total of observations, number of strokes, percent of strokes

n_train <- nrow(train_stroke)

train_stroke %>% 
  group_by(round(age, -1)) %>%
  summarise(total = n(), percent = round(total/n_train, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Summary table by age (rounded nearest 10) test #####
# age, total of observations, number of strokes, percent of strokes

n_test <- nrow(test_stroke)

test_stroke %>% 
  group_by(round(age, -1)) %>%
  summarise(total = n(), percent = round(total/n_test, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Summary table by heart_disease train #####
# heart_disease, total of observations, number of strokes, percent of strokes
train_stroke %>% 
  group_by(heart_disease) %>%
  summarise(total = n(), percent = round(total/n_train, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Summary table by heart_disease train test #####
# heart_disease, total of observations, number of strokes, percent of strokes
test_stroke %>% 
  group_by(heart_disease) %>%
  summarise(total = n(), percent = round(total/n_test, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Summary table by avg_glucose_level (round to nearest ten) train #####
# avg_glucose_level, total of observations, number of strokes, percent of strokes
train_stroke %>% 
  group_by(round(avg_glucose_level, -1)) %>%
  summarise(total = n(), percent = round(total/n_train, 3), strokes = sum(stroke == "stroke"),
            stroke_percent = round(mean(stroke == "stroke"), 3)) %>% 
  unique() %>%
  knitr::kable()

# Summary table by avg_glucose_level (round to nearest ten) test #####
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

# __Percent of strokes by rounded avg_glucose_level test ####  

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
                            data = train_stroke, 
                            minsplit = seq(1,100,5))
plot(minsplit_tune, main = "tune minsplit")

cp_tune <- tune.rpart(stroke ~ .,
                      data = train_stroke, 
                      cp = seq(0.000, 0.02, len = 50))
plot(cp_tune, main = "tune cp")

cp_tune <- train(stroke ~ ., 
                 method = "rpart",
                 tuneGrid = data.frame(cp = seq(0.000, 0.02, len = 50)),
                 data = train_stroke)
plot(cp_tune)

maxdepth_tune <- tune.rpart(stroke ~ .,
                            data = train_stroke, 
                            maxdepth = 1:30)
plot(maxdepth_tune, main = "tune maxdepth")

# All Data #####

# __Default Tree: Gini, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
default_tree <- rpart(stroke ~ ., 
                      data = train_stroke)
rpart.plot(default_tree)

# __Gini Tree, CP = 0.001, minsplit = 20, minbucket round 20/3, maxdepht = 30 #####
gini_tree_cp0.001 <- rpart(stroke ~., 
                   data = train_stroke, 
                   parms=list(split=c("gini")),
                   cp = 0.001)
rpart.plot(gini_tree_cp0.001)

# __Gini Tree, CP = 0.0024, minslit = 20, minbucket round 20/3, maxdepht = 30 ######
gini_tree_cp0.0024 <- rpart(stroke ~., 
                           data = train_stroke, 
                           parms=list(split=c("gini")),
                           cp = 0.0024)
rpart.plot(gini_tree_cp0.0024)

# __Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 5 #####
gini_tree_cp0.001_max5 <- rpart(stroke ~., 
                            data = train_stroke, 
                            parms=list(split=c("gini")),
                            cp = 0.001,
                            maxdepth = 5)
rpart.plot(gini_tree_cp0.001_max5)

# __Information Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 #####
inf_tree <- rpart(stroke ~., 
                  data = train_stroke, 
                  parms=list(split=c("information")))
rpart.plot(inf_tree)

# __Information Tree, CP = 0.001. minslit = 20, minbucket round 20/3, maxdepth = 30 #####
inf_tree_cp0.001 <- rpart(stroke ~., 
                  data = train_stroke, 
                  parms=list(split=c("information")),
                  cp = 0.001)
rpart.plot(inf_tree_cp0.001)


# __Information Tree, CP = 0.0023. minslit = 20, minbucket round 20/3, maxdepht = 30 #####
inf_tree_cp0.0023 <- rpart(stroke ~., 
                          data = train_stroke, 
                          parms=list(split=c("information")),
                          cp = 0.0023)
rpart.plot(inf_tree_cp0.0023)

# __Information Tree, CP = 0.0023. minslit = 20, minbucket round 20/3, maxdepht = 6 #####
inf_tree_cp0.001_max6 <- rpart(stroke ~., 
                           data = train_stroke, 
                           parms=list(split=c("information")),
                           cp = 0.001,
                           maxdepth = 6)
rpart.plot(inf_tree_cp0.001_max6)

#Categorical Data #####
train_stroke_categorical <- train_stroke %>% 
  select(gender, ever_married, work_type,
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
  select(age, hypertension, heart_disease, avg_glucose_level, stroke)

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
# ORIGINAL DATA ########
# Imbalanced ########
# _______________________########

# RPART native#######
# Recursive Partitioning and Regression Trees ######
# Default_tree #####
y_hat_default_tree <- predict(default_tree, test_stroke, type = "class")

confusionMatrix(y_hat_default_tree,
                test_stroke$stroke, positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_default_tree,
                test_stroke$stroke, positive = "stroke")$overall

confusionMatrix(y_hat_default_tree,
                test_stroke$stroke, positive = "stroke")$table

cm_default_tree <- confusionMatrix(y_hat_default_tree, test_stroke$stroke, positive = "stroke")
cm_default_tree

F_meas(y_hat_default_tree, test_stroke$stroke)

# __Accuracy : 0.9572 ####
# __Sensitivity : 0.0000 ####
# __Specificity : 1.0000 ####
# __Balanced Accuracy : 0.5000 ####


# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####

y_hat_gini_tree_cp0.001 <- predict(gini_tree_cp0.001, test_stroke, type = "class")

confusionMatrix(y_hat_gini_tree_cp0.001,
                test_stroke$stroke, positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.001,
                test_stroke$stroke, positive = "stroke")$overall

confusionMatrix(y_hat_gini_tree_cp0.001,
                test_stroke$stroke, positive = "stroke")$table

cm_y_hat_gini_tree_cp0.001 <- confusionMatrix(y_hat_gini_tree_cp0.001, test_stroke$stroke, 
                                              positive = "stroke")
cm_y_hat_gini_tree_cp0.001

# __Accuracy : 0.9521 ####
# __Sensitivity : 0.07143  ####
# __Specificity : 0.99149 ####
# __Balanced Accuracy : 0.53146 ####


# Gini Tree, CP = 0.0024, minslit = 20, minbucket round 20/3, maxdepht = 30 ######

y_hat_gini_tree_cp0.0024 <- predict(gini_tree_cp0.0024, test_stroke, type = "class")

confusionMatrix(y_hat_gini_tree_cp0.0024,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.0024,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_gini_tree_cp0.0024,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_y_hat_gini_tree_cp0.0024 <- confusionMatrix(y_hat_gini_tree_cp0.0024, test_stroke$stroke, 
                                               positive = "stroke")
cm_y_hat_gini_tree_cp0.0024

# __Accuracy : 0.9532 ######
# __Sensitivity : 0.04762 ######
# __Specificity : 0.99362 ######
# __Balanced Accuracy : 0.52062 ######


# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 5 ######

y_hat_gini_tree_cp0.001_max5 <- predict(gini_tree_cp0.001_max5, test_stroke, type = "class")

confusionMatrix(y_hat_gini_tree_cp0.001_max5,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.001_max5,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_gini_tree_cp0.001_max5,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_y_hat_gini_tree_cp0.001_max5 <- confusionMatrix(y_hat_gini_tree_cp0.001_max5, 
                                                   test_stroke$stroke, 
                                                   positive = "stroke")
cm_y_hat_gini_tree_cp0.001_max5

F_meas(y_hat_gini_tree_cp0.001_max5, test_stroke$stroke)

# __Accuracy : 0.9542  ######
# __Sensitivity : 0.02381 ######
# __Specificity : 0.99574 ######
# __Balanced Accuracy : 0.50978 ######


# Information Tree, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 #####

y_hat_inf_tree <- predict(inf_tree, test_stroke, type = "class")

confusionMatrix(y_hat_inf_tree,
                test_stroke$stroke, 
                positive = "stroke", 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_inf_tree,
                test_stroke$stroke, 
                positive = "stroke", 
                positive = "stroke")$overall

confusionMatrix(y_hat_inf_tree,
                test_stroke$stroke, 
                positive = "stroke", 
                positive = "stroke")$table

cm_y_hat_inf_tree <- confusionMatrix(y_hat_inf_tree, test_stroke$stroke, 
                                     positive = "stroke")
cm_y_hat_inf_tree

# __Accuracy : 0.9572  ######
# __Sensitivity : 0.0000  ######
# __Specificity : 1.0000 ######
# __Balanced Accuracy : 0.5000 ######


# Information Tree, CP = 0.001. minslit = 20, minbucket round 20/3, maxdepht = 30 #####

y_hat_inf_tree_cp0.001 <- predict(inf_tree_cp0.001, test_stroke, type = "class")

confusionMatrix(y_hat_inf_tree_cp0.001,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_inf_tree_cp0.001,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_inf_tree_cp0.001,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_y_hat_inf_tree_cp0.001 <- confusionMatrix(y_hat_inf_tree_cp0.001, test_stroke$stroke, 
                                             positive = "stroke")
cm_y_hat_inf_tree_cp0.001

# __Accuracy : 0.9532  #####
# __Sensitivity : 0.04762  #####
# __Specificity : 0.993617  #####
# __Balanced Accuracy : 0.52062  #####


# Information Tree, CP = 0.0023. minslit = 20, minbucket round 20/3, maxdepht = 30 #####

y_hat_inf_tree_cp0.0023 <- predict(inf_tree_cp0.0023, test_stroke, type = "class")

confusionMatrix(y_hat_inf_tree_cp0.0023,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_inf_tree_cp0.0023,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_inf_tree_cp0.0023,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_y_hat_inf_tree_cp0.0023 <- confusionMatrix(y_hat_inf_tree_cp0.0023, test_stroke$stroke, 
                                              positive = "stroke")
cm_y_hat_inf_tree_cp0.0023

# __Accuracy : 0.9542  #####
# __Sensitivity : 0.02381  #####        
# __Specificity : 0.99574 #####
# __Balanced Accuracy : 0.50978  #####


# RPART caret ######

train_caret_tree <- train(stroke ~ ., method = "rpart", data = train_stroke)

y_hat_caret_tree <- predict(train_caret_tree, test_stroke)

confusionMatrix(y_hat_caret_tree, test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_caret_tree, test_stroke$stroke, 
                positive = "stroke")$table

cm_caret_tree <- confusionMatrix(y_hat_caret_tree, test_stroke$stroke, 
                                 positive = "stroke")
cm_caret_tree

# __Accuracy : 0.9572  ######
# __Sensitivity : 0.0000 #####         
# __Specificity : 1.0000 #####
# __Balanced Accuracy : 0.5000  #####


# LDA ########
# Linear Discriminant Analisis ########
# it generates several warnings! ####


train_lda <- train(stroke ~ ., method = "lda", data = train_stroke)

y_hat_lda <- predict(train_lda, test_stroke)

confusionMatrix(y_hat_lda, test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_lda, test_stroke$stroke, 
                positive = "stroke")$table

cm_lda <- confusionMatrix(y_hat_lda, test_stroke$stroke, 
                          positive = "stroke")
cm_lda

# Accuracy : 0.9481 ####
# Sensitivity : 0.1429  ####        
# Specificity : 0.9840  ####
# Balanced Accuracy : 0.5634 ####

# KNN ########
# K-Nearest-Neighbor #######

train_knn <- train(stroke ~ ., method = "knn", 
                   data = train_stroke)

y_hat_knn <- predict(train_knn, test_stroke)

confusionMatrix(y_hat_knn,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_knn,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_knn,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_knn <- confusionMatrix(y_hat_knn, test_stroke$stroke, 
                          positive = "stroke")
cm_knn

# Accuracy : 0.9542  ####
# Sensitivity :  0.0000  ####        
# Specificity : 0.9968 ####
# Balanced Accuracy : 0.4984  ####

# RANDOM FOREST ########
# it takes time! ######

train_rf <- train(stroke ~ ., method = "rf", 
                  data = train_stroke)

y_hat_rf <- predict(train_rf, test_stroke, type = "raw")

confusionMatrix(y_hat_rf, test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_rf, test_stroke$stroke, 
                positive = "stroke")$table

cm_rf <- confusionMatrix(y_hat_rf, test_stroke$stroke, 
                         positive = "stroke")
cm_rf

# Accuracy : 0.9572  ####
# Sensitivity : 0.0000     ####     
# Specificity : 1.0000   ####
# Balanced Accuracy : 0.5000  ####

# _______________________########
# BALANCED DATA ########
# _______________________######## 
# _______________________######## 
# Oversampling #####
# _______________________######## 

table(train_stroke$stroke)
prop.table(table(train_stroke$stroke))

n_over = sum(train_stroke$stroke == "no_stroke")

set.seed(1969, sample.kind="Rounding") # Every time we run the code, we get a different ovun.sample
# Accuracy and balanced accuracy strongly depends on this ramdom process.

train_stroke_over <- ovun.sample(stroke ~ ., data = train_stroke, method = "over", N = n_over*2)$data
table(train_stroke_over$stroke)

# RPART native #######
# Recursive Partitioning and Regression Trees ######

# Default Tree: Gini, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
default_tree_over <- rpart(stroke ~ ., 
                      data = train_stroke_over)
rpart.plot(default_tree_over)

y_hat_default_tree_over <- predict(default_tree_over, test_stroke, type = "class")

confusionMatrix(y_hat_default_tree_over,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_default_tree_over,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_default_tree_over,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_default_tree_over <- confusionMatrix(y_hat_default_tree_over, test_stroke$stroke, 
                                        positive = "stroke")
cm_default_tree_over

F_meas(confusionMatrix(y_hat_default_tree_over,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7342  ####
# __Sensitivity : 0.7619   ####         
# __Specificity : 0.7330 ####
# __Balanced Accuracy : 0.747  ####
# __F_meas, beta = 3 : 0.484115   ####

# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####

gini_tree_cp0.001_over <- rpart(stroke ~., 
                           data = train_stroke_over, 
                           parms=list(split=c("gini")),
                           cp = 0.001)

y_hat_gini_tree_cp0.001_over <- predict(gini_tree_cp0.001_over, test_stroke, type = "class")

confusionMatrix(y_hat_gini_tree_cp0.001_over,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.001_over,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_gini_tree_cp0.001_over,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_gini_tree_cp0.001_over <- confusionMatrix(y_hat_gini_tree_cp0.001_over, test_stroke$stroke, 
                                                   positive = "stroke")
cm_gini_tree_cp0.001_over

F_meas(confusionMatrix(y_hat_gini_tree_cp0.001_over,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.8982  ####
# __Sensitivity : 0.42857 ####       
# __Specificity : 0.91915 ####
# __Balanced Accuracy : 0.67386 ####
# __F_meas, beta = 3 : 0.3813559  ####

# Default Gini Tree & Cost Matrix 3 to 1 #####

cost_matrix_tree_over <- rpart(stroke ~ ., 
                               data = train_stroke_over,
                               parms=list(
                                 loss=matrix(c(0,1,3,0), # A false negative is 5 times worse than a false positive
                                             byrow=TRUE,
                                             nrow=2)))

rpart.plot(cost_matrix_tree_over)

y_hat_cost_matrix_tree_over <- predict(cost_matrix_tree_over, test_stroke, type = "class")

confusionMatrix(y_hat_cost_matrix_tree_over,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_cost_matrix_tree_over,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_cost_matrix_tree_over,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_cost_matrix_tree_over <- confusionMatrix(y_hat_cost_matrix_tree_over, test_stroke$stroke, 
                                            positive = "stroke")
cm_cost_matrix_tree_over

F_meas(confusionMatrix(y_hat_cost_matrix_tree_over,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.5642  ##### 
# __Sensitivity : 0.88095 !!!! #####         
# __Specificity : 0.55000 #####  
# __Balanced Accuracy : 0.71548 #####
# __F_meas, beta = 3 : 0.3813559 - its has sense?? #####
# We are appliying a cost matrix

# RPART caret ####

train_caret_tree_over <- train(stroke ~ ., method = "rpart", data = train_stroke_over)

y_hat_caret_tree_over <- predict(train_caret_tree_over, test_stroke)

confusionMatrix(y_hat_caret_tree_over, 
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_caret_tree_over, 
                test_stroke$stroke, 
                positive = "stroke")$table

cm_caret_tree_over <- confusionMatrix(y_hat_caret_tree_over, test_stroke$stroke, 
                                      positive = "stroke")
cm_caret_tree_over

F_meas(confusionMatrix(y_hat_caret_tree_over, 
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7179   ####
# __Sensitivity : 0.7619      ####        
# __Specificity : 0.7160   ####
# __Balanced Accuracy : 0.7389    ####
# __F_meas, beta = 3 : 0.4726736 #####

#KNN caret #######
# K-Nearest-Neighbor #######

train_knn_over <- train(stroke ~ ., method = "knn", 
                   data = train_stroke_over)

y_hat_knn_over <- predict(train_knn_over, test_stroke)

confusionMatrix(y_hat_knn_over,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_knn_over,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_knn_over,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_knn_over <- confusionMatrix(y_hat_knn_over, test_stroke$stroke, 
                               positive = "stroke")
cm_knn_over

F_meas(confusionMatrix(y_hat_knn_over,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.8228  ##### 
# __Sensitivity : 0.47619  #####         
# __Specificity : 0.83830  ##### 
# __Balanced Accuracy : 0.65724  #####
# __F_meas, beta = 3 : 0.3636364 #####

# KNN3 native: cutoff >= 0.5 #######

train_knn3_over <- knn3(stroke ~ ., 
                        data = train_stroke_over)

knn3_over <- predict(train_knn3_over, test_stroke)

y_hat_knn3_over <- as.data.frame(knn3_over) %>% 
  mutate(stroke_1 = ifelse(stroke >= 0.5, "stroke", "no_stroke" )) %>% 
  pull(stroke_1) %>% 
  as_factor() %>% 
  relevel(ref = "no_stroke")

confusionMatrix(y_hat_knn3_over,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_knn3_over,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_knn3_over,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_knn3_over <- confusionMatrix(y_hat_knn3_over, test_stroke$stroke, 
                               positive = "stroke")
cm_knn3_over

F_meas(confusionMatrix(y_hat_knn3_over,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.8228   ##### 
# __Sensitivity : 0.47619   ##### 
# __Specificity : 0.83830   ##### 
# __Balanced Accuracy : 0.65724  ##### 
# __F_meas, beta = 3 : 0.3636364 #####

# RANDOM FOREST ########
# it takes time! ######

train_rf_over <- train(stroke ~ ., method = "rf", 
                  data = train_stroke_over)

y_hat_rf_over <- predict(train_rf_over, test_stroke, type = "raw")

confusionMatrix(y_hat_rf_over, test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_rf_over, test_stroke$stroke, 
                positive = "stroke")$table

cm_rf_over <- confusionMatrix(y_hat_rf_over, test_stroke$stroke, 
                         positive = "stroke")
cm_rf_over

F_meas(confusionMatrix(y_hat_rf_over, test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# # __Accuracy : 0.946   ##### 
# # __Sensitivity : 0.00000  #####         
# # __Specificity : 0.98830   ##### 
# # __Balanced Accuracy : 0.49415  ##### 
# __F_meas, beta = 3 : NaN #####

# MDA ######
# Mixture Discriminant Analysis ####

set.seed(1970, sample.kind="Rounding") 
# Results are random variables
         
train_mda_over <- mda(stroke ~., data = train_stroke_over)

y_hat_mda_over <- predict(train_mda_over, test_stroke)

confusionMatrix(y_hat_mda_over,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_mda_over,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_mda_over,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_mda_over <- confusionMatrix(y_hat_mda_over, test_stroke$stroke, 
                               positive = "stroke")
cm_mda_over

F_meas(confusionMatrix(y_hat_mda_over,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7424   ##### 
# __Sensitivity : 0.78571    #####        
# __Specificity : 0.74043    ##### 
# __Balanced Accuracy : 0.76307    ##### 
# __F_meas, beta = 3 : 0.5038168 #####

# RDA #####
# Regularized Discriminant Analysis  #####

set.seed(1970, sample.kind="Rounding") 
train_rda_over <- rda(stroke ~., data = train_stroke_over)

y_hat_rda_over <- predict(train_rda_over, test_stroke)

confusionMatrix(y_hat_rda_over$class,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_rda_over$class,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_rda_over$class,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_rda_over <- confusionMatrix(y_hat_rda_over$class, test_stroke$stroke, 
                               positive = "stroke")
cm_rda_over

F_meas(confusionMatrix(y_hat_rda_over$class,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7067  ##### 
# __Sensitivity : 0.80952 #####        
# __Specificity : 0.70213 #####
# __Balanced Accuracy : 0.75583 #####
# __F_meas, beta = 3 : 0.4913295 #####

# NNET: great variability !!!#####
# Neural Network ####

if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
library(nnet)

set.seed(2, sample.kind="Rounding") 
# Results are random variables# Results are random variables: great variability!!!

train_nnet_over <- nnet(stroke ~., data = train_stroke_over, size=4, 
                        decay=0.0001, maxit=500)

y_hat_nnet_over <- predict(train_nnet_over, test_stroke, type = "class")

head(y_hat_nnet_over)

confusionMatrix(as.factor(y_hat_nnet_over),
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(as.factor(y_hat_nnet_over),
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(as.factor(y_hat_nnet_over),
                test_stroke$stroke, 
                positive = "stroke")$table

cm_nnet_over <- confusionMatrix(as.factor(y_hat_nnet_over), test_stroke$stroke, 
                                positive = "stroke")
cm_nnet_over

F_meas(confusionMatrix(as.factor(y_hat_nnet_over),
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7525 ####
# __Sensitivity : 0.76190 ####        
# __Specificity : 0.75213 ####
# __Balanced Accuracy : 0.75702 ####
# __F_meas, beta = 3 : 0.4976672 #####

# FDA #####
# Flexible Discriminant Analysis #####

train_fda_over <- fda(stroke ~., data = train_stroke_over)

y_hat_fda_over <- predict(train_fda_over, test_stroke)

confusionMatrix(y_hat_fda_over,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_fda_over,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_fda_over,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_fda_over <- confusionMatrix(y_hat_fda_over, test_stroke$stroke, 
                               positive = "stroke")
cm_fda_over

F_meas(confusionMatrix(y_hat_fda_over,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7261  ####
# __Sensitivity : 0.80952  ####        
# __Specificity : 0.72234  ####  
# __Balanced Accuracy : 0.76593  ####
# __F_meas, beta = 3 : 0.5052006 #####

# KSVM ######
# Support Vector Machine #####

set.seed(2, sample.kind="Rounding") 
# Results are random variables 

train_ksvm_over <- ksvm(stroke ~., data = train_stroke_over)

y_hat_ksvm_over <- predict(train_ksvm_over, test_stroke)

confusionMatrix(y_hat_ksvm_over,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_ksvm_over,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_ksvm_over,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_ksvm_over <- confusionMatrix(y_hat_ksvm_over, test_stroke$stroke, 
                                positive = "stroke")
cm_ksvm_over

F_meas(confusionMatrix(y_hat_ksvm_over,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7923 ##### 
# __Sensitivity : 0.64286    #####      
# __Specificity : 0.79894  #####  
# __Balanced Accuracy : 0.72090  #####
# __F_meas, beta = 3 : 0.4545455 #####

# NAIVE BAYES ######

train_naiveBayes_over <- naiveBayes(stroke ~., data = train_stroke_over)

y_hat_naiveBayes_over <- predict(train_naiveBayes_over, test_stroke)

confusionMatrix(y_hat_naiveBayes_over,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_naiveBayes_over,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_naiveBayes_over,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_naiveBayes_over <- confusionMatrix(y_hat_naiveBayes_over, test_stroke$stroke, 
                                positive = "stroke")
cm_naiveBayes_over

F_meas(confusionMatrix(y_hat_naiveBayes_over,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7566  ##### 
# __Sensitivity : 0.73810  #####         
# __Specificity : 0.75745  ##### 
# __Balanced Accuracy : 0.74777  ##### 
# __F_meas, beta = 3 : 0.4866562 #####

# _______________________######## 
# Undersampling #####
# _______________________######## 

table(train_stroke$stroke)
prop.table(table(train_stroke$stroke))

n_under = sum(train_stroke$stroke == "stroke")

set.seed(500, sample.kind="Rounding") # Every time we run the code, we get a different ovun.sample
# Accuracy and balanced accuracy strongly depends on this ramdom process.

train_stroke_under <- ovun.sample(stroke ~ ., data = train_stroke, method = "under", N = n_under*2)$data
table(train_stroke_under$stroke)

# RPART native #######
# Recursive Partitioning and Regression Trees ######

# Default Tree: Gini, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
default_tree_under <- rpart(stroke ~ ., 
                           data = train_stroke_under)
rpart.plot(default_tree_under)

y_hat_default_tree_under <- predict(default_tree_under, test_stroke, type = "class")

confusionMatrix(y_hat_default_tree_under,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_default_tree_under,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_default_tree_under,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_default_tree_under <- confusionMatrix(y_hat_default_tree_under, test_stroke$stroke, 
                                        positive = "stroke")
cm_default_tree_under

F_meas(confusionMatrix(y_hat_default_tree_under,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7189  ####
# __Sensitivity : 0.83333 !!!! ####         
# __Specificity : 0.71383 ####
# __Balanced Accuracy : 0.77358  ####
# __F_meas, beta = 3 : 0.5131965 #####

# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####

gini_tree_cp0.001_under <- rpart(stroke ~., 
                                data = train_stroke_under, 
                                parms=list(split=c("gini")),
                                cp = 0.001)

y_hat_gini_tree_cp0.001_under <- predict(gini_tree_cp0.001_under, test_stroke, type = "class")

confusionMatrix(y_hat_gini_tree_cp0.001_under,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.001_under,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_gini_tree_cp0.001_under,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_gini_tree_cp0.001_under <- confusionMatrix(y_hat_gini_tree_cp0.001_under, test_stroke$stroke, 
                                                   positive = "stroke")
cm_gini_tree_cp0.001_under

F_meas(confusionMatrix(y_hat_gini_tree_cp0.001_under,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7413 ####
# __Sensitivity : 0.80952 ####       
# __Specificity : 0.73830  ####
# __Balanced Accuracy : 0.77391 ####
# __F_meas, beta = 3 : 0.5167173 #####


# Default Gini Tree & Cost Matrix 3 to 1 #####

cost_matrix_tree_under <- rpart(stroke ~ ., 
                               data = train_stroke_under,
                               parms=list(
                                 loss=matrix(c(0,1,3,0), # A false negative is 5 times worse than a false positive
                                             byrow=TRUE,
                                             nrow=2)))

rpart.plot(cost_matrix_tree_under)

y_hat_cost_matrix_tree_under  <- predict(cost_matrix_tree_under, test_stroke, type = "class")

confusionMatrix(y_hat_cost_matrix_tree_under,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_cost_matrix_tree_under,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_cost_matrix_tree_under,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_cost_matrix_tree_under <- confusionMatrix(y_hat_cost_matrix_tree_under, test_stroke$stroke, 
                                            positive = "stroke")
cm_cost_matrix_tree_under

F_meas(confusionMatrix(y_hat_cost_matrix_tree_under,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.5519  ##### 
# __Sensitivity : 0.90476 !!!! #####         
# __Specificity : 0.53617 #####  
# __Balanced Accuracy : 0.72047 ##### 
# __F_meas, beta = 3 : 0.4460094 #####


# RPART caret ####

train_caret_tree_under <- train(stroke ~ ., method = "rpart", data = train_stroke_under)

y_hat_caret_tree_under <- predict(train_caret_tree_under, test_stroke)

confusionMatrix(y_hat_caret_tree_under, test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_caret_tree_under, test_stroke$stroke, 
                positive = "stroke")$table

cm_caret_tree_under <- confusionMatrix(y_hat_caret_tree_under, test_stroke$stroke, 
                                      positive = "stroke")
cm_caret_tree_under

F_meas(confusionMatrix(y_hat_caret_tree_under, test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.6202 ####
# __Sensitivity : 0.85714 ####        
# __Specificity : 0.60957 ####
# __Balanced Accuracy : 0.73336 ####
# __F_meas, beta = 3 : 0.4609475 #####

#KNN caret #######
# K-Nearest-Neighbor #######

train_knn_under <- train(stroke ~ ., method = "knn", 
                        data = train_stroke_under)

y_hat_knn_under <- predict(train_knn_under, test_stroke)

confusionMatrix(y_hat_knn_under,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_knn_under,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_knn_under,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_knn_under <- confusionMatrix(y_hat_knn_under, test_stroke$stroke, 
                               positive = "stroke")
cm_knn_under

F_meas(confusionMatrix(y_hat_knn_under,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.6548 ##### 
# __Sensitivity : 0.78571 #####         
# __Specificity : 0.64894 ##### 
# __Balanced Accuracy : 0.71733 #####
# __F_meas, beta = 3 : 0.4453441 #####

# KNN3 native: cutoff >= 0.5 #######
# K-Nearest-Neighbor ####### 

train_knn3_under <- knn3(stroke ~ ., 
                        data = train_stroke_under)

knn3_under <- predict(train_knn3_under, test_stroke)

y_hat_knn3_under <- as.data.frame(knn3_under) %>% 
  mutate(stroke_1 = ifelse(stroke >= 0.5, "stroke", "no_stroke" )) %>% # we can play with cutoff
  pull(stroke_1) %>% 
  as_factor() %>% 
  relevel(ref = "no_stroke")

confusionMatrix(y_hat_knn3_under,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_knn3_under,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_knn3_under,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_knn3_under <- confusionMatrix(y_hat_knn3_under, test_stroke$stroke, 
                                positive = "stroke")
cm_knn3_under

F_meas(confusionMatrix(y_hat_knn3_under,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.6589 ##### 
# __Sensitivity : 0.78571 #####          
# __Specificity : 0.65319 ##### 
# __Balanced Accuracy : 0.71945 ##### 
# __F_meas, beta = 3 : 0.4477612 #####

# RANDOM FOREST ########
# it takes time! ######

train_rf_under <- train(stroke ~ ., method = "rf", 
                       data = train_stroke_under)

y_hat_rf_under <- predict(train_rf_over, test_stroke, type = "raw")

confusionMatrix(y_hat_rf_under, test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_rf_under, test_stroke$stroke, 
                positive = "stroke")$table

cm_rf_under <- confusionMatrix(y_hat_rf_under, test_stroke$stroke, 
                              positive = "stroke")
cm_rf_under

F_meas(confusionMatrix(y_hat_rf_under, test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.947 ##### 
# __Sensitivity : 0.00000  #####         
# __Specificity : 0.98936  ##### 
# __Balanced Accuracy : 0.49468 ##### 
# __F_meas, beta = 3 : NaN #####

# MDA: error ######
# Mixture Discriminant Analysis ####

set.seed(1970, sample.kind="Rounding") 
# Results are random variables

train_mda_under <- mda(stroke ~., data = train_stroke_under)

y_hat_mda_under <- predict(train_mda_under, test_stroke)

# Error in maxdist[l] <- x[l, i] : 
  # NAs no son permitidos en asignaciones subscritas

# RDA #####
# Regularized Discriminant Analysis  #####

set.seed(1970, sample.kind="Rounding") 
train_rda_under <- rda(stroke ~., data = train_stroke_under)

y_hat_rda_under <- predict(train_rda_under, test_stroke)

confusionMatrix(y_hat_rda_under$class,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_rda_under$class,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_rda_under$class,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_rda_under <- confusionMatrix(y_hat_rda_under$class, test_stroke$stroke, 
                               positive = "stroke")
cm_rda_under

F_meas(confusionMatrix(y_hat_rda_under$class,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.6823  ##### 
# __Sensitivity : 0.80952  #####        
# __Specificity : 0.67660 #####
# __Balanced Accuracy : 0.74306 #####
# __F_meas, beta = 3 : 0.4748603 #####

# NNET: great variability !!!#####
# Neural Network ####

set.seed(2, sample.kind="Rounding") 
# Results are random variables# Results are random variables: great variability!!!

train_nnet_under <- nnet(stroke ~., data = train_stroke_under, size=4, 
                        decay=0.0001, maxit=500)

y_hat_nnet_under <- predict(train_nnet_under, test_stroke, type = "class")
  
confusionMatrix(as.factor(y_hat_nnet_under),
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(as.factor(y_hat_nnet_under),
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(as.factor(y_hat_nnet_under),
                test_stroke$stroke, 
                positive = "stroke")$table

cm_nnet_under <- confusionMatrix(as.factor(y_hat_nnet_under), test_stroke$stroke, 
                                positive = "stroke")
cm_nnet_under

F_meas(confusionMatrix(as.factor(y_hat_nnet_under),
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.9572  ####
# __Sensitivity : 0.00000  ####        
# __Specificity : 1.00000 ####
# __Balanced Accuracy : 0.50000####
# __F_meas, beta = 3 : NaN #####

# FDA: error #####
# Flexible Discriminant Analysis #####

train_fda_under <- fda(stroke ~., data = train_stroke_under)

y_hat_fda_under <- predict(train_fda_under, test_stroke)

# Error in mindist[l] <- ndist[l] : 
  # NAs no son permitidos en asignaciones subscritas


# KSVM ######
# Support Vector Machine #####

set.seed(2, sample.kind="Rounding") 
# Results are random variables 

train_ksvm_under <- ksvm(stroke ~., data = train_stroke_under)

y_hat_ksvm_under <- predict(train_ksvm_under, test_stroke)

confusionMatrix(y_hat_ksvm_under,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_ksvm_under,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_ksvm_under,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_ksvm_under <- confusionMatrix(y_hat_ksvm_under, test_stroke$stroke, 
                                positive = "stroke")
cm_ksvm_under

F_meas(confusionMatrix(y_hat_ksvm_under,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.6823 ##### 
# __Sensitivity : 0.78571 #####      
# __Specificity : 0.67766 #####  
# __Balanced Accuracy : 0.73169 #####
# __F_meas, beta = 3 : 0.4621849 #####

# NAIVE BAYES ######

train_naiveBayes_under <- naiveBayes(stroke ~., data = train_stroke_under)

y_hat_naiveBayes_under <- predict(train_naiveBayes_under, test_stroke)

confusionMatrix(y_hat_naiveBayes_under,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_naiveBayes_under,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_naiveBayes_under,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_naiveBayes_under <- confusionMatrix(y_hat_naiveBayes_under, test_stroke$stroke, 
                                      positive = "stroke")
cm_naiveBayes_under

F_meas(confusionMatrix(y_hat_naiveBayes_under,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7892  ##### 
# __Sensitivity : 0.69048  #####         
# __Specificity : 0.79362   ##### 
# __Balanced Accuracy : 0.74205  #####
# __F_meas, beta = 3 : 0.4825291 #####

# _______________________######## 
# Oversamplig and Undersampling: both #####
# _______________________######## 

table(train_stroke$stroke)
prop.table(table(train_stroke$stroke))

n_both = sum(train_stroke$stroke == "stroke") + sum(train_stroke$stroke == "no_stroke")

set.seed(1969, sample.kind="Rounding") # Every time we run the code, we get a different ovun.sample
# Accuracy and balanced accuracy strongly depends on this ramdom process.

train_stroke_both <- ovun.sample(stroke ~ ., data = train_stroke, method = "both", p = 0.5, N = n_both)$data
table(train_stroke_both$stroke)

# RPART native #######
# Recursive Partitioning and Regression Trees ######

# Default Tree: Gini, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
default_tree_both <- rpart(stroke ~ ., 
                            data = train_stroke_both)
rpart.plot(default_tree_both)

y_hat_default_tree_both <- predict(default_tree_both, test_stroke, type = "class")

confusionMatrix(y_hat_default_tree_both,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_default_tree_both,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_default_tree_both,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_default_tree_both <- confusionMatrix(y_hat_default_tree_both, test_stroke$stroke, 
                                         positive = "stroke")
cm_default_tree_both

F_meas(confusionMatrix(y_hat_default_tree_both,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.779   ####
# __Sensitivity : 0.73810 ####         
# __Specificity : 0.78085 ####
# __Balanced Accuracy : 0.75947 ####
# __F_meas, beta = 3 : 0.504065 #####

# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####

gini_tree_cp0.001_both <- rpart(stroke ~., 
                                 data = train_stroke_both, 
                                 parms=list(split=c("gini")),
                                 cp = 0.001)

y_hat_gini_tree_cp0.001_both <- predict(gini_tree_cp0.001_both, test_stroke, type = "class")

confusionMatrix(y_hat_gini_tree_cp0.001_both,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.001_both,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_gini_tree_cp0.001_both,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_gini_tree_cp0.001_both <- confusionMatrix(y_hat_gini_tree_cp0.001_both, test_stroke$stroke, 
                                              positive = "stroke")
cm_gini_tree_cp0.001_both

F_meas(confusionMatrix(y_hat_gini_tree_cp0.001_both,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.8157 ####
# __Sensitivity : 0.54762 ####       
# __Specificity : 0.82766  ####
# __Balanced Accuracy : 0.68764 ####
# __F_meas, beta = 3 : 0.4085258 #####

# Default Gini Tree & Cost Matrix 3 to 1 #####

cost_matrix_tree_both <- rpart(stroke ~ ., 
                                data = train_stroke_both,
                                parms=list(
                                  loss=matrix(c(0,1,3,0), # A false negative is 5 times worse than a false positive
                                              byrow=TRUE,
                                              nrow=2)))

rpart.plot(cost_matrix_tree_both)

y_hat_cost_matrix_tree_both  <- predict(cost_matrix_tree_both, test_stroke, type = "class")

confusionMatrix(y_hat_cost_matrix_tree_both,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_cost_matrix_tree_both,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_cost_matrix_tree_both,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_cost_matrix_tree_both <- confusionMatrix(y_hat_cost_matrix_tree_both, test_stroke$stroke, 
                                             positive = "stroke")
cm_cost_matrix_tree_both

F_meas(confusionMatrix(y_hat_cost_matrix_tree_both,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.6049 ##### 
# __Sensitivity : 0.88095 #####         
# __Specificity : 0.59255 #####  
# __Balanced Accuracy : 0.73675#####
# __F_meas, beta = 3 : 0.4636591 #####

# RPART caret ####

train_caret_tree_both <- train(stroke ~ ., method = "rpart", data = train_stroke_both)

y_hat_caret_tree_both <- predict(train_caret_tree_both, test_stroke)

confusionMatrix(y_hat_caret_tree_both, test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_caret_tree_both, test_stroke$stroke, 
                positive = "stroke")$table

cm_caret_tree_both <- confusionMatrix(y_hat_caret_tree_both, test_stroke$stroke, 
                                       positive = "stroke")
cm_caret_tree_both

F_meas(confusionMatrix(y_hat_caret_tree_both, test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7699 ####
# __Sensitivity : 0.76190  ####        
# __Specificity : 0.77021 ####
# __Balanced Accuracy : 0.76606 ####
# __F_meas, beta = 3 : 0.5111821 #####

#KNN caret #######
# K-Nearest-Neighbor #######

train_knn_both <- train(stroke ~ ., method = "knn", 
                         data = train_stroke_both)

y_hat_knn_both <- predict(train_knn_both, test_stroke)

confusionMatrix(y_hat_knn_both,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_knn_both,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_knn_both,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_knn_both <- confusionMatrix(y_hat_knn_both, test_stroke$stroke, 
                                positive = "stroke")
cm_knn_both

F_meas(confusionMatrix(y_hat_knn_both,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7566 ##### 
# __Sensitivity : 0.64286 #####         
# __Specificity : 0.76170 ##### 
# __Balanced Accuracy : 0.70228 #####
# __F_meas, beta = 3 : 0.4292528 #####

# KNN3 native: cutoff >= 0.5 #######
# K-Nearest-Neighbor ####### 

train_knn3_both <- knn3(stroke ~ ., 
                         data = train_stroke_both)

knn3_both <- predict(train_knn3_both, test_stroke)

y_hat_knn3_both <- as.data.frame(knn3_both) %>% 
  mutate(stroke_1 = ifelse(stroke >= 0.5, "stroke", "no_stroke" )) %>% # we can play with cutoff
  pull(stroke_1) %>% 
  as_factor() %>% 
  relevel(ref = "no_stroke")

confusionMatrix(y_hat_knn3_both,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_knn3_both,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_knn3_both,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_knn3_both <- confusionMatrix(y_hat_knn3_both, test_stroke$stroke, 
                                 positive = "stroke")
cm_knn3_both

F_meas(confusionMatrix(y_hat_knn3_both,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7566 ##### 
# __Sensitivity : 0.64286 #####          
# __Specificity : 0.76170 ##### 
# __Balanced Accuracy : 0.70228 #####
# __F_meas, beta = 3 : 0.4292528 #####

# RANDOM FOREST ########
# it takes time! ######

train_rf_both <- train(stroke ~ ., method = "rf", 
                        data = train_stroke_both)

y_hat_rf_both <- predict(train_rf_both, test_stroke, type = "raw")

confusionMatrix(y_hat_rf_both, test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_rf_both, test_stroke$stroke, 
                positive = "stroke")$table

cm_rf_both <- confusionMatrix(y_hat_rf_both, test_stroke$stroke, 
                               positive = "stroke")
cm_rf_both

F_meas(confusionMatrix(y_hat_rf_both, test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.9358 ##### 
# __Sensitivity : 0.28571 #####         
# __Specificity : 0.96489  ##### 
# __Balanced Accuracy : 0.62530 #####
# __F_meas, beta = 3 : 0.2836879 #####

# MDA  ######
# Mixture Discriminant Analysis ####

set.seed(1970, sample.kind="Rounding") 
# Results are random variables

train_mda_both <- mda(stroke ~., data = train_stroke_both)

y_hat_mda_both <- predict(train_mda_both, test_stroke)

confusionMatrix(y_hat_mda_both,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_mda_both,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_mda_both,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_mda_both <- confusionMatrix(y_hat_mda_both, test_stroke$stroke, 
                               positive = "stroke")
cm_mda_both

F_meas(confusionMatrix(y_hat_mda_both,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7098 ##### 
# __Sensitivity : 0.69048 #####        
# __Specificity : 0.71064 ##### 
# __Balanced Accuracy : 0.70056 #####
# __F_meas, beta = 3 : 0.4270987 #####

# RDA #####
# Regularized Discriminant Analysis  #####

set.seed(1970, sample.kind="Rounding") 

train_rda_both <- rda(stroke ~., data = train_stroke_both)

y_hat_rda_both <- predict(train_rda_both, test_stroke)

confusionMatrix(y_hat_rda_both$class,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_rda_both$class,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_rda_both$class,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_rda_both <- confusionMatrix(y_hat_rda_both$class, test_stroke$stroke, 
                                positive = "stroke")
cm_rda_both

F_meas(confusionMatrix(y_hat_rda_both$class,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.722 ##### 
# __Sensitivity : 0.80952 #####        
# __Specificity : 0.71809 #####
# __Balanced Accuracy : 0.76380 #####
# __F_meas, beta = 3 : 0.5022157 #####

# NNET: great variability !!!#####
# Neural Network ####

set.seed(2, sample.kind="Rounding") 
# Results are random variables# Results are random variables: great variability!!!

train_nnet_both <- nnet(stroke ~., data = train_stroke_both, size=4, 
                         decay=0.0001, maxit=500)

y_hat_nnet_both <- predict(train_nnet_both, test_stroke, type = "class")

confusionMatrix(as.factor(y_hat_nnet_both),
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(as.factor(y_hat_nnet_both),
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(as.factor(y_hat_nnet_both),
                test_stroke$stroke, 
                positive = "stroke")$table

cm_nnet_both <- confusionMatrix(as.factor(y_hat_nnet_both), test_stroke$stroke, 
                                 positive = "stroke")
cm_nnet_both

F_meas(confusionMatrix(as.factor(y_hat_nnet_both),
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7882  ####
# __Sensitivity : 0.71429  ####        
# __Specificity : 0.79149 ####
# __Balanced Accuracy : 0.75289 ####
# __F_meas, beta = 3 : 0.4966887 #####

# FDA #####
# Flexible Discriminant Analysis #####

train_fda_both <- fda(stroke ~., data = train_stroke_both)

y_hat_fda_both <- predict(train_fda_both, test_stroke)

confusionMatrix(y_hat_fda_both,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_fda_both,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_fda_both,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_fda_both <- confusionMatrix(y_hat_fda_both, test_stroke$stroke, 
                               positive = "stroke")
cm_fda_both

F_meas(confusionMatrix(y_hat_fda_both,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.721 #####
# __Sensitivity : 0.76190    #####      
# __Specificity : 0.71915  ##### 
# __Balanced Accuracy : 0.74053 #####
# __F_meas, beta = 3 : 0.4747774 #####


# KSVM ######
# Support Vector Machine #####

set.seed(2, sample.kind="Rounding") 
# Results are random variables 

train_ksvm_both <- ksvm(stroke ~., data = train_stroke_both)

y_hat_ksvm_both <- predict(train_ksvm_both, test_stroke)

confusionMatrix(y_hat_ksvm_both,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_ksvm_both,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_ksvm_both,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_ksvm_both <- confusionMatrix(y_hat_ksvm_both, test_stroke$stroke, 
                                 positive = "stroke")
cm_ksvm_both

F_meas(confusionMatrix(y_hat_ksvm_both,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7729 ##### 
# __Sensitivity : 0.69048 #####      
# __Specificity : 0.77660  #####  
# __Balanced Accuracy : 0.73354 #####
# __F_meas, beta = 3 : 0.4700162 #####

# NAIVE BAYES ######

train_naiveBayes_both <- naiveBayes(stroke ~., data = train_stroke_both)

y_hat_naiveBayes_both <- predict(train_naiveBayes_both, test_stroke)

confusionMatrix(y_hat_naiveBayes_both,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_naiveBayes_both,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_naiveBayes_both,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_naiveBayes_both <- confusionMatrix(y_hat_naiveBayes_both, test_stroke$stroke, 
                                       positive = "stroke")
cm_naiveBayes_both

F_meas(confusionMatrix(y_hat_naiveBayes_both,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7525  ##### 
# __Sensitivity : 0.73810  #####         
# __Specificity : 0.75319   ##### 
# __Balanced Accuracy : 0.74564  #####
# __F_meas, beta = 3 : 0.4836193 #####

# _______________________######## 
# Beter estimates ######
# _______________________######## 

set.seed(1969, sample.kind="Rounding") # Every time we run the code, we get a different ovun.sample
# Accuracy and balanced accuracy strongly depends on this ramdom process.

train_stroke_better <- ROSE(stroke ~ ., data = train_stroke)$data
table(train_stroke_better_est$stroke)

# RPART native #######
# Recursive Partitioning and Regression Trees ######

# Default Tree: Gini, CP = 0.01, minslit = 20, minbucket round 20/3, maxdepht = 30 ####
default_tree_better <- rpart(stroke ~ ., 
                           data = train_stroke_better)

rpart.plot(default_tree_better)

y_hat_default_tree_better <- predict(default_tree_better, test_stroke, type = "class")

confusionMatrix(y_hat_default_tree_better,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_default_tree_better,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_default_tree_better,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_default_tree_better <- confusionMatrix(y_hat_default_tree_better, test_stroke$stroke, 
                                        positive = "stroke")
cm_default_tree_better

F_meas(confusionMatrix(y_hat_default_tree_better,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.8248 ####
# __Sensitivity : 0.64286 ####         
# __Specificity : 0.83298 ####
# __Balanced Accuracy : 0.73792 ####
# __F_meas, beta = 3 : 0.480427 #####

# Gini Tree, CP = 0.001, minslit = 20, minbucket round 20/3, maxdepht = 30 #####

gini_tree_cp0.001_better <- rpart(stroke ~., 
                                data = train_stroke_better, 
                                parms=list(split=c("gini")),
                                cp = 0.001)

y_hat_gini_tree_cp0.001_better <- predict(gini_tree_cp0.001_better, test_stroke, type = "class")

confusionMatrix(y_hat_gini_tree_cp0.001_better,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_gini_tree_cp0.001_better,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_gini_tree_cp0.001_better,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_gini_tree_cp0.001_better <- confusionMatrix(y_hat_gini_tree_cp0.001_better, 
                                               test_stroke$stroke, 
                                               positive = "stroke")
cm_gini_tree_cp0.001_better

F_meas(confusionMatrix(y_hat_gini_tree_cp0.001_better,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.8839 ####
# __Sensitivity : 0.47619 ####       
# __Specificity : 0.90213  ####
# __Balanced Accuracy : 0.68916  ####
# __F_meas, beta = 3 : 0.4081633 #####

# Default Gini Tree & Cost Matrix 3 to 1 #####

cost_matrix_tree_better <- rpart(stroke ~ ., 
                               data = train_stroke_better,
                               parms=list(
                                 loss=matrix(c(0,1,3,0), # A false negative is 5 times worse than a false positive
                                             byrow=TRUE,
                                             nrow=2)))

rpart.plot(cost_matrix_tree_better)

y_hat_cost_matrix_tree_better  <- predict(cost_matrix_tree_better, test_stroke, type = "class")

confusionMatrix(y_hat_cost_matrix_tree_better,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_cost_matrix_tree_better,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_cost_matrix_tree_better,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_cost_matrix_tree_better <- confusionMatrix(y_hat_cost_matrix_tree_better, test_stroke$stroke, 
                                            positive = "stroke")
cm_cost_matrix_tree_better

F_meas(confusionMatrix(y_hat_cost_matrix_tree_better,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.612 ##### 
# __Sensitivity : 0.83333 #####         
# __Specificity : 0.60213 #####  
# __Balanced Accuracy : 0.71773 #####  
# __F_meas, beta = 3 : 0.4447268 #####

# RPART caret ####

train_caret_tree_better<- train(stroke ~ ., method = "rpart", data = train_stroke_better)

y_hat_caret_tree_better <- predict(train_caret_tree_better, test_stroke)

confusionMatrix(y_hat_caret_tree_better, test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_caret_tree_better, test_stroke$stroke, 
                positive = "stroke")$table

cm_caret_tree_better <- confusionMatrix(y_hat_caret_tree_better, test_stroke$stroke, 
                                      positive = "stroke")
cm_caret_tree_better

F_meas(confusionMatrix(y_hat_caret_tree_better, test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

## __Accuracy : 0.8778  ####
## __Sensitivity : 0.40476  ####        
## __Specificity : 0.89894 ####
## __Balanced Accuracy : 0.65185 ####
# __F_meas, beta = 3 : 0.3469388 #####

#KNN caret #######
# K-Nearest-Neighbor #######

train_knn_better <- train(stroke ~ ., method = "knn", 
                        data = train_stroke_better)

y_hat_knn_better <- predict(train_knn_better, test_stroke)

confusionMatrix(y_hat_knn_better,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_knn_better,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_knn_better,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_knn_better <- confusionMatrix(y_hat_knn_better, test_stroke$stroke, 
                               positive = "stroke")
cm_knn_better

F_meas(confusionMatrix(y_hat_knn_better,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7128 ##### 
# __Sensitivity : 0.80952  #####         
# __Specificity : 0.70851 ##### 
# __Balanced Accuracy : 0.75902 ##### 
# __F_meas, beta = 3 : 0.4956268 #####

# KNN3 native: cutoff >= 0.5 #######
# K-Nearest-Neighbor ####### 

train_knn3_better <- knn3(stroke ~ ., 
                        data = train_stroke_better)

knn3_better <- predict(train_knn3_better, test_stroke)

y_hat_knn3_better <- as.data.frame(knn3_better) %>% 
  mutate(stroke_1 = ifelse(stroke >= 0.5, "stroke", "no_stroke" )) %>% # we can play with cutoff
  pull(stroke_1) %>% 
  as_factor() %>% 
  relevel(ref = "no_stroke")

confusionMatrix(y_hat_knn3_better,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_knn3_better,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_knn3_better,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_knn3_better <- confusionMatrix(y_hat_knn3_better, test_stroke$stroke, 
                                positive = "stroke")
cm_knn3_better

F_meas(confusionMatrix(y_hat_knn3_better,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7301 ##### 
# __Sensitivity : 0.78571  #####          
# __Specificity : 0.72766 ##### 
# __Balanced Accuracy : 0.75669 ##### 
# __F_meas, beta = 3 : 0.4947526 #####


# RANDOM FOREST ########
# it takes time! ######

train_rf_better <- train(stroke ~ ., method = "rf", 
                       data = train_stroke_better)

y_hat_rf_better <- predict(train_rf_better, test_stroke, type = "raw")

confusionMatrix(y_hat_rf_better, test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_rf_better, test_stroke$stroke, 
                positive = "stroke")$table

cm_rf_better <- confusionMatrix(y_hat_rf_better, test_stroke$stroke, 
                              positive = "stroke")
cm_rf_better

F_meas(confusionMatrix(y_hat_rf_better, test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.9236  ##### 
# __Sensitivity : 0.35714  #####         
# __Specificity : 0.94894  ##### 
# __Balanced Accuracy : 0.65304 #####
# __F_meas, beta = 3 : 0.3401361 #####

# MDA: error ######
# Mixture Discriminant Analysis ####

set.seed(1970, sample.kind="Rounding") 
# Results are random variables

train_mda_better <- mda(stroke ~., data = train_stroke_better)

y_hat_mda_better <- predict(train_mda_better, test_stroke)

# Error in maxdist[l] <- x[l, i] : 
  # NAs no son permitidos en asignaciones subscritas

# RDA #####
# Regularized Discriminant Analysis  #####

set.seed(1970, sample.kind="Rounding") 

train_rda_better <- rda(stroke ~., data = train_stroke_better)

y_hat_rda_better <- predict(train_rda_better, test_stroke)

confusionMatrix(y_hat_rda_better$class,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_rda_better$class,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_rda_better$class,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_rda_better<- confusionMatrix(y_hat_rda_better$class, test_stroke$stroke, 
                               positive = "stroke")
cm_rda_better

F_meas(confusionMatrix(y_hat_rda_better$class,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.6853 ##### 
# __Sensitivity : 0.80952 #####        
# __Specificity : 0.67979 #####
# __Balanced Accuracy : 0.74466 #####
# __F_meas, beta = 3 : 0.4768583 #####

# NNET: great variability !!!#####
# Neural Network ####

set.seed(2, sample.kind="Rounding") 
# Results are random variables# Results are random variables: great variability!!!

train_nnet_better <- nnet(stroke ~., data = train_stroke_better, size=4, 
                        decay=0.0001, maxit=500)

y_hat_nnet_better <- predict(train_nnet_better, test_stroke, type = "class")

confusionMatrix(as.factor(y_hat_nnet_better),
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"] 

confusionMatrix(as.factor(y_hat_nnet_better),
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(as.factor(y_hat_nnet_better),
                test_stroke$stroke, 
                positive = "stroke")$table

cm_nnet_better <- confusionMatrix(as.factor(y_hat_nnet_better), test_stroke$stroke, 
                                positive = "stroke")
cm_nnet_better

F_meas(confusionMatrix(as.factor(y_hat_nnet_better),
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7179  ####
# __Sensitivity : 0.80952  ####        
# __Specificity : 0.71383 ####
# __Balanced Accuracy : 0.76168 ####
# __F_meas, beta = 3 : 0.4992658 #####

# FDA: Error #####
# Flexible Discriminant Analysis #####

train_fda_better <- fda(stroke ~., data = train_stroke_better)

y_hat_fda_better <- predict(train_fda_better, test_stroke)

# Error in mindist[l] <- ndist[l] : 
  # NAs no son permitidos en asignaciones subscritas

# KSVM ######
# Support Vector Machine #####

set.seed(2, sample.kind="Rounding") 
# Results are random variables 

train_ksvm_better <- ksvm(stroke ~., data = train_stroke_better)

y_hat_ksvm_better <- predict(train_ksvm_better, test_stroke)

confusionMatrix(y_hat_ksvm_better,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_ksvm_better,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_ksvm_better,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_ksvm_better <- confusionMatrix(y_hat_ksvm_better, test_stroke$stroke, 
                                positive = "stroke")
cm_ksvm_better

F_meas(confusionMatrix(y_hat_ksvm_better,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.7464  ##### 
# __Sensitivity : 0.71429 #####      
# __Specificity : 0.74787  #####  
# __Balanced Accuracy : 0.73108 #####
# __F_meas, beta = 3 : 0.4651163 #####

# NAIVE BAYES ######

train_naiveBayes_better <- naiveBayes(stroke ~., data = train_stroke_better)

y_hat_naiveBayes_better <- predict(train_naiveBayes_better, test_stroke)

confusionMatrix(y_hat_naiveBayes_better,
                test_stroke$stroke, 
                positive = "stroke")$overall["Accuracy"]

confusionMatrix(y_hat_naiveBayes_better,
                test_stroke$stroke, 
                positive = "stroke")$overall

confusionMatrix(y_hat_naiveBayes_better,
                test_stroke$stroke, 
                positive = "stroke")$table

cm_naiveBayes_better <- confusionMatrix(y_hat_naiveBayes_better, test_stroke$stroke, 
                                      positive = "stroke")
cm_naiveBayes_better

F_meas(confusionMatrix(y_hat_naiveBayes_better,
                       test_stroke$stroke, 
                       positive = "stroke")$table, 
       reference = Reference,
       relevant = "stroke",
       beta = 3)

# __Accuracy : 0.777  ##### 
# __Sensitivity : 0.73810  #####         
# __Specificity : 0.77872  ##### 
# __Balanced Accuracy : 0.75841 ##### 
# __F_meas, beta = 3 : 0.5024311 #####

# _______________________########
# TRAIN CONTROL CROSS VAL ########
# _______________________########

control <- trainControl(method="repeatedcv", number=10, repeats=3)































 
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


# Ver comentarios

# https://topepo.github.io/caret/train-models-by-tag.html#Two_Class_Only
# Cost-Sensitive CART

# method = 'rpartCost'
# Type: Classification

# Tuning parameters:
  
# cp (Complexity Parameter)
# Cost (Cost)
# Required packages: rpart, plyr

 













