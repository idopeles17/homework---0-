library(dslabs)
data(murders)
a <-murders$state
class(a)
a <-2
b <--1
c <--4
(-b - sqrt(b^2 - 4*a*c))/(2*a)
(-b + sqrt(b^2 - 4*a*c))/(2*a)
log(1024,4)
data(movielens)
library(dslabs)
data("movielens")
class(movielens$title)
class(movielens$genres)
nlevels(movielens$genres)
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time <- time/60
speed <- distance /time
time
speed
library(dplyr)
library(dslabs)
data(heights)
options(digits = 3)
heights
Avg <- mean(heights$height)
Avg
ind <- filter(heights,height > Avg, sex == "Female")
nrow(ind)
options(digits = 3)
heights$sex <-"Female" == TRUE
mean(heights$sex)
ind <- which ( heights$sex == "Female" )
sum(ind)
which("Female")
heights <- which ( sex == "Female")  
ind <- c(TRUE,FALSE,TRUE)
mean(ind)
ind<- which.min(heights$height)
sum(1032)
ind[heights$height]
index <- which.max(heights$height)
heights$height[index]
heights[index]
ind <- match()
heights$sex[1032]
50.0 =< x =< 82.7
ind <- match(x,heights$height)
heights2 <-mutate(heights,ht_cm = height * 2.54)
mean(heights2$ht_cm)
ind <- filter(heights2,sex == "Female")
mean (ind$ht_cm)
data(olive)
head(olive)
boxplot(palmitic~palmitoleic , data=olive)
hist(olive$eicosenoic)
boxplot(palmitic~region , data=olive)
class(heights$sex)
heights$sex 
data(data)
library(data_Michelle_1_)
library(data_Michelle_1_)
hist(Staff_answers_1_2_$Delta_Suds)
library(titanic)
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
class(titanic_train$Survived)
titanic_train  %>%
  group_by(Sex) %>%
  ggplot(aes(Age,y=..count..,fill=Sex)) +
  geom_density(alpha = 0.2, bw = 2.50, position = "stack")  
  params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
  titanic_train %>%
  ggplot(aes(sample=Age)) +
  geom_qq(dparams = params)+
  geom_abline()
  titanic_train  %>%
    ggplot(aes(Survived,fill=Sex)) +
    geom_bar(position = position_dodge())
  bw = 2.50, position = "stack"
  titanic  %>%
  ggplot(aes(Age,y=..count..,fill= Survived)) +
    geom_density(alpha = 0.2,position = "stack") +
    facet_grid( .~ Survived)
titanic %>%
  filter(!is.na(Fare)) %>%
  ggplot(aes(Survived,Fare)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  geom_jitter(alpha=0.2)
# barplot of passenger class filled by survival
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar() +
  ylab("Count")
# barplot of passenger class filled by survival with position_fill
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion")
# Barplot of survival filled by passenger class with position_fill
titanic %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion")
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(position = "stack") +
  facet_grid(Sex ~ Pclass)
library(data_Michelle_1_)
  1 <- data_Michelle_1_$age_di == "0" & data_Michelle_1_$Clownpressent=="0"  %>%
  2 <- data_Michelle_1_$age_di == "0" & data_Michelle_1_$Clownpressent=="1"  %>%
  3 <- data_Michelle_1_$age_di == "1" & data_Michelle_1_$Clownpressent=="0"  %>%
  4 <- data_Michelle_1_$age_di == "1" & data_Michelle_1_$Clownpressent=="1"  %>%

    data_Michelle_1_ %>% 
mutate(groups = reorder(data_Michelle_1_$groups,data_Michelle_1_$age_di,FUN = mode)) %>%
ggplot(aes(groups,TIMEGAP,fill=groups)) +
  geom_boxplot()+
    geom_point(alpha=0.3)+
    scale_y_continuous(trans = "log2") +
  ggtitle("Time Gap VS Clown pressent by Age") +
   xlab("")+ ylab("Time Gap")
  help(hjust)
  data(Staff_answeres_1_2_)
  
  Staff_answers_1_2_ %>%
    filter(!is.na(SUDS) ) %>%
    group_by(Clown) %>%
    mutate(location = ifelse(Time.of == "before", 1, 2),
           location = ifelse(Time.of== "before" & Profession == 2 , location-0.12 ,location),
           location = ifelse(Time.of== "before" & Profession == 3 , location-0.22 ,location),
           location = ifelse(Time.of== "after" & Profession == 2 , location+0.12 ,location),
           location = ifelse(Time.of== "after" & Profession == 3 , location+0.22 ,location),
           hjust = ifelse(Time.of == "before", 1, 0)) %>% 
           ggplot(aes(Time.of, SUDS, group = profession.name)) +
    scale_y_continuous()
    geom_line(aes(color = profession.name), show.legend = FALSE) +
    geom_text(aes(x = location, label = profession.name, hjust = hjust),
              show.legend = FALSE,size=2.5,) +
    facet_wrap(date ~.)

  help("scale_y_continuous")

  
  
  dat %>%.
    mutate(location = ifelse(year == 2010, 1, 2), 
           location = ifelse(year == 2015 & 
                               country %in% c("United Kingdom", "Portugal"),
                             location+0.22, location),
           hjust = ifelse(year == 2010, 1, 0)) %>%
    mutate(year = as.factor(year)) %>%
    ggplot(aes(year, life_expectancy, group = country)) +
    geom_line(aes(color = country), show.legend = FALSE) +
    geom_text(aes(x = location, label = country, hjust = hjust,size=0.5), 
              show.legend = FALSE) +
    xlab("") + ylab("Life Expectancy")
 
  
  
 
  

    


 