library(lmerTest)
library(EMAtools)
library(readxl)
library(effectsize)
library(tidyverse)
library(dplyr)

setwd("~/.../study2")

#data import
s2 <- read.csv("data/study2.csv") %>%
  filter(qual.text.check == 1) %>% 
  filter(slider.attn_10 == 100) %>%
  mutate(pol.id_1 = as.numeric(pol.id_1),
         climate.concern_1 = as.numeric(climate.concern_1),
         condition = as.factor(c),
         age = as.numeric(age),
         climateLink1 = as.numeric(climateLink1),
         climateLink2 = as.numeric(climateLink2),
         climateLink3 = as.numeric(climateLink3),
         climateLink4 = as.numeric(climateLink4)) %>% 
  dplyr::select(ResponseId,condition,open.1:pol.id_1,qual.text.check,climateLink1:climateLink4,race,age,gender,income,zipcode) %>% 
  as_tibble()
#write.csv(s2,"data/study2clean.csv")

#condition relabel
s2$condition <- recode(s2$condition, `1` = "Low",  `2` = "High")

# link click conversion
for (i in 1:nrow(s2)) {
  s2$links[i] <- sum(s2$climateLink1[i],s2$climateLink2[i],s2$climateLink3[i],s2$climateLink4[i])
}
s2$links <- as.numeric(s2$links >= 1)
table(s2$links)

#descriptives
table(s2$gender)
mean(s2$age)
sd(s2$age)
table(s2$race)
 
# analyses

# LIWC 22
textData <- s2 %>% select(ResponseId,condition,open.1,open.2)
#write.csv(textData,"data/textData.csv")
textData <- read.csv("data/textDataLIWC.csv")

#affect
aff.M <- glm(Affect ~ condition, data = textData)
summary(aff.M)

aff.M.WC <- glm(Affect ~ condition + WC, data = textData)
summary(aff.M.WC)

cohens_d(Affect ~ condition, data = textData)
# d = 0.53

#emotion
emo.M <- glm(emotion ~ condition, data = textData)
summary(emo.M)

emo.M.WC <- glm(emotion ~ condition + WC, data = textData)
summary(emo.M.WC)

cohens_d(emotion ~ condition, data = textData)
# d = 0.32

hi <- s2 %>% filter(condition == "High")
lo <- s2 %>% filter(condition == "Low")

s2Policy <- s2 %>%
  select(ResponseId,condition,pol.id_1,fuel_10:food_10) %>% 
  gather(policy, rating, fuel_10:food_10, factor_key=TRUE) %>% 
  mutate(rating = as.numeric(rating))

# write_csv(s2Policy,"df_policy.csv")

s2Behavior <- s2 %>%
  select(ResponseId,condition,pol.id_1,beh.e.vehicle_1:beh.no.car_1) %>% 
  gather(behavior, rating, beh.e.vehicle_1:beh.no.car_1, factor_key=TRUE) %>% 
  mutate(rating = as.numeric(rating))

# write_csv(s2Behavior,"df_behavior.csv")

#climate concern
percepM <- glm(climate.concern_1 ~ condition, data = s2)
summary(percepM)
cohens_d(climate.concern_1 ~ condition, data = s2)
# d = 0.17

#policy endorsement
polM <- lmer(rating ~ condition +  (1|policy) + (1|ResponseId), data = s2Policy)
summary(polM)
lme.dscore(polM,s2Policy,"lme4")
# d = 0.21

#behavior intention
behM <- lmer(rating ~ condition + (1|behavior) + (1|ResponseId), data = s2Behavior)
summary(behM)
lme.dscore(behM,s2Behavior,"lme4")
# d = 0.20

#link clicks
clickM <- glm(links ~ condition, data = s2, family = binomial)
summary(clickM)
model_coef <- coef(clickM)
odds_ratios <- exp(model_coef)
oddsratio_to_d(1.41)
# d = 0.18

# exploratory ideology analyses

percepM.ID <- glm(climate.concern_1 ~ condition + pol.id_1 + condition*pol.id_1, data = s2)
summary(percepM.ID)

polM.ID <- lmer(rating ~ condition + pol.id_1 + condition*pol.id_1 +
                  (1|policy) + (1|ResponseId), data = s2Policy)
summary(polM.ID)

behM.ID <- lmer(rating ~ condition + pol.id_1 + condition*pol.id_1 +
                  (1|behavior) + (1|ResponseId), data = s2Behavior)
summary(behM.ID)

clickM.ID <- glm(links ~ condition + pol.id_1 + condition*pol.id_1, data = s2, family = binomial)
summary(clickM.ID)




