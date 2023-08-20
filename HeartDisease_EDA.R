##Packages downloaded (if necessary)
install.packages("corrplot")
install.packages("ggplot2")
install.packages("dplyr")

##Packages loaded
library(corrplot)
library(ggplot2)
library(dplyr)

##Data set downloaded
heart <- read.csv("heart.csv")
View(heart)

##Brief overview of correlation of variables (all correlated but 4)
corrplot(cor(heart), type="upper")

##Identify variables in data set
ls()
ls(heart)

##Categorical variables changed from numeric values to string values
heart$sex[heart$sex == 0] = "female"
heart$sex[heart$sex == 1] = "male"
heart$cp[heart$cp == 0] = "typical angina"
heart$cp[heart$cp == 1] = "atypical angina"
heart$cp[heart$cp == 2] = "non-anginal pain"
heart$cp[heart$cp == 3] = "asymptomatic"
##trestbps does not need to be changed as it shows resting blood pressure (unique)
##chol does not need to be changed as it shows cholesterol level (unique)
heart$fbs[heart$fbs == 0] = "blood sugar = or < 120mg/dl"
heart$fbs[heart$fbs == 1] = "blood sugar > 120mg/dl"
heart$restecg[heart$restecg == 0] = "normal ECG"
heart$restecg[heart$restecg == 1] = "ST-T wave abnormality"
heart$restecg[heart$restecg == 2] = "left ventricular hypertrophy"
##thalach does not need to be changed as it shows maximum heart rate (unique)
heart$exang[heart$exang == 0] = "no"
heart$exang[heart$exang == 1] = "yes"
##oldpeak does not need to be changed as it shows ST depression (unique)
heart$slope[heart$slope == 0] = "upsloping"
heart$slope[heart$slope == 1] = "flat"
heart$slope[heart$slope == 2] = "downsloping"
##ca does not need to be changed as it shows number of major vessels (numeric)
heart$thal[heart$thal == 1] = "normal"
heart$thal[heart$thal == 2] = "fixed defect"
heart$thal[heart$thal == 3] = "reversable defect"

##Target1 created to keep the numeric value of target and create a categorical 
##value of target1
heart$target1[heart$target == 0] = "no"
heart$target1[heart$target == 1] = "yes"

##Q: What is proportion of sample population with heart disease?

##Proportion table
prop.table(table(heart$target1))

##Proportion table with result rounded to two decimal places
round(prop.table(table(heart$target1)), 2)

##Q: What age subpopulation is heart disease most prevalent in? 

##Distribution of sample
ggplot(heart, aes(x=age)) + 
  geom_histogram() + 
  ggtitle("Distribution of Age") + 
  xlab("Age") + 
  ylab("Count")

##Min and max age ranges
min(heart$age)
max(heart$age)

##Ages range from 29-77. Age groups separated into 4-year sections.
heart$age_group = cut(heart$age, breaks = seq(29,77,4), include.lowest = TRUE)

##Proportion table by age groups
round(prop.table(table(heart$target, heart$age_group)), 2)

##Calculate number of people with heart disease for each age group. 
HD_by_age <- heart %>%
  group_by(age_group) %>%
  summarise(heart_disease = sum(target))

HD_by_age

##Visualisation
HD_by_age %>%
  ggplot(aes(x = age_group, y = heart_disease)) + 
  geom_bar(stat = "identity", fill = "red", alpha = .5, width = .4) + 
  xlab("Age Groups") + 
  ylab("# People with HD") + 
  ggtitle("# HD by Age Group") + 
  theme_bw()

##Visualisation swith relative rate of HD
prop_in_age = heart %>%
  group_by(age_group) %>%
  summarise(HD_prop = round(sum(target)/n(), 3)*100)

prop_in_age

##Visualisation
prop_in_age %>%
  ggplot(aes(x = age_group, y = HD_prop)) + 
  geom_bar(stat = "identity", fill = "blue", alpha = .5, width = .4) + 
  xlab("Age Groups") + 
  ylab("Prop People with HD") + 
  ggtitle("Prop HD by Age Group") + 
  theme_bw()

##Q: What is proportion of sample population with HD by sex?
round(prop.table(table(heart$sex)), 2)

##What is the proportion of males vs females with HD?
round(prop.table(table(heart$sex, heart$target1)), 2)

##Q: Is there a correlation between HR and ST slope of peak exercise?

##Visualisation
ggplot(heart, aes(x = slope, fill = target1)) + 
  geom_bar(position = "dodge") + 
  xlab("Slope") + 
  ylab("Count") + 
  ggtitle("HD Incidence by ST Slope") + 
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

##Proportion of ST slop by sex. Now, let's look at the above proportions by sex.
male_data = heart[heart$sex=="male",]
female_data = heart[heart$sex=="female",]

##Visualisation (male)
ggplot(male_data, aes(x = slope, fill = target1)) + 
  geom_bar(position = "dodge") + 
  xlab("Slope") + 
  ylab("Count") + 
  ggtitle("HD Incidence of ST Slope - Males") + 
  scale_fill_discrete(name = "HD", labels = c("No", "Yes"))

##Visualiation (female)
ggplot(female_data, aes(x = slope, fill = target1)) + 
  geom_bar(position = "dodge") + 
  xlab("Slope") + 
  ylab("Count") + 
  ggtitle("HD Incidence of ST Slope - Females") + 
  scale_fill_discrete(name = "HD", labels = c("No", "Yes"))

##Visualisation
ggplot(heart, aes(x = slope, fill = target1)) + 
  geom_bar(position = "dodge") + 
  facet_wrap( ~ sex) +
  xlab("Slope") + 
  ylab("Count") + 
  ggtitle("HD Incidence by ST Slope") + 
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

##Q: Is there a correlation between HD and number of major vessels?

##Visualisation of differences in HD by number of major vessels (mosaic plot)
mosaicplot(table(heart$target1, heart$ca), col = c("red", "yellow", "blue", 
                                                   "black", "pink"), 
           main = "HD for Major Vessels")

##Visualisation by sex 
mosaicplot(table(male_data$target1, male_data$ca), col = c("red", "yellow",
                                                           "blue", "black", 
                                                           "pink"), 
           main = "HD for Major Vessels - Male")

mosaicplot(table(female_data$target1, female_data$ca), col = c("red", "yellow", 
                                                               "blue", "black", 
                                                               "pink"), 
           main = "HD for Major Vessels - Female")

##Q: Is there a correlation between HD and ST Depression by exercise?

##Visualisation
ggplot(heart, aes(x = target1, y = oldpeak)) + 
  geom_boxplot()

##Visualisation
ggplot(heart, aes(x = age, y = oldpeak, color = target1)) + 
  geom_point(alpha = 0.3) + 
  labs(color = "Heart Disease State") + 
  xlab("Age") + 
  ylab("ST Depression") + 
  ggtitle("Age vs ST Depression by Heart Condition") +
  guides(size = FALSE)

##Q: Is there a correlation between HD and resting blood pressure?
ggplot(heart, aes(x = target1, y = trestbps)) + 
  geom_boxplot() + 
  xlab("Heart Disease State") + 
  ylab("Resting BP") + 
  ggtitle("Boxplots of Resting BP by Heart Condition")

##Visualisation, adding ST depression 
ggplot(heart, aes(x = age, y = trestbps, color = target1, size = factor(oldpeak))) + 
  geom_point(alpha = 0.3) + 
  xlab("Heart Disease State") + 
  ylab("Resting BP") + 
  ggtitle("Boxplots of Resting BP by Heart Condition") + 
  guides(size = FALSE)