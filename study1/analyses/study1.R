library(lmerTest)
library(EMAtools)
library(readxl)
library(effectsize)
library(bruceR)
library(mediation)
library(tidyverse)
library(dplyr)

setwd("~/Documents/research/coco/climateImages/NatCC-submission/study1")

# to skip all munging, just load data with following commented line and then jump to analyses
# s1 <- read.csv("data/study1.csv")

#data import
study1.all <- read.csv("data/study1RAW.csv") %>%
  filter(attn.check == 0)

data <- study1.all %>%
  dplyr::select(ArgCC_emotion_1:attn.check) %>% 
  lapply(function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

data$ID <- seq.int(nrow(data))
data <- data %>%
  dplyr::select(ID, everything()) %>% 
  dplyr::select(-attn.check)

data_long <- gather(data, stim, rating, ArgCC_emotion_1:VnmGW_action_1, factor_key=TRUE)

data_long <- data_long %>% 
  mutate(term = case_when(substr(data_long$stim,4,6) == "CC_" ~ "change",
                          substr(data_long$stim,4,6) == "CCr" ~ "crisis",
                          substr(data_long$stim,4,5) == "CE" ~ "emergency",
                          substr(data_long$stim,4,5) == "GW" ~ "warming",
                          TRUE ~ "NA" ),
         country = case_when(substr(data_long$stim,1,3) == "xxx" ~ "NA",
                             TRUE ~ substr(data_long$stim,1,3)),
         country = toupper(country),
         itemType = case_when(str_sub(data_long$stim, -8, -3) == "motion" ~ "emoRating",
                              str_sub(data_long$stim, -6, -3) == "tive" ~ "negRating",
                              str_sub(data_long$stim, -6, -3) == "cted" ~ "affRating",
                              TRUE ~ "actRating"))

# back to wide form
data_wide <- spread(data_long, itemType, rating) %>%
  dplyr::select(-stim) %>% 
  distinct() %>% 
  group_by(ID, country, term) %>% 
  summarise(actRating = mean(actRating, na.rm = TRUE),
            affRating = mean(affRating, na.rm = TRUE),
            emoRating = mean(emoRating, na.rm = TRUE),
            negRating = mean(negRating, na.rm = TRUE)) %>%
  mutate_if(is.numeric, list(~na_if(., -Inf)))

data_wide <- data_wide[!(is.na(data_wide$emoRating) &
                           is.na(data_wide$negRating) &
                           is.na(data_wide$affRating) &
                           is.na(data_wide$emoRating)),]

data_wide$term <- factor(data_wide$term)
data_wide$country <- factor(data_wide$country)

#EPI, CRI, and GDP data
externalData <- read_excel("data/external_climate_indices.xlsx")

s1.full <- left_join(data_wide,externalData,by = "country")

s1.scaled <- s1.full %>% 
  ungroup() %>% 
  dplyr::select(actRating:GDP) %>% 
  scale() %>% 
  as_tibble()

s1.scaled$ID <- s1.full$ID
s1.scaled$country <- s1.full$country 
s1.scaled$term <- s1.full$term

s1 <- s1.scaled %>%
  dplyr::select(ID, country, term, everything())

# write.csv(s1,"data/study1.csv")
# s1 <- read.csv("data/study1.csv")

M <- lmer(emoRating ~ concern + (1 | country) + (1 | term) + (1 | ID), data = s1)
summary(M)
lme.dscore(M,s1,"lme4")

M <- lmer(emoRating ~ concern + EPI + CRI + GDP + (1|country) + (1|term) + (1|ID), data = s1)
summary(M)
lme.dscore(M,s1,"lme4")

M <- lmer(actRating ~ concern + (1 | country) + (1 | term) + (1 | ID), data = s1)
summary(M)
lme.dscore(M,s1,"lme4")

M <- lmer(actRating ~ concern + EPI + CRI + GDP + (1|country) + (1|term) + (1|ID), data = s1)
summary(M)
lme.dscore(M,s1,"lme4")

M <- lmer(actRating ~ emoRating + concern + EPI + CRI + GDP + (1 | country) + (1 | term) + (1 | ID), data = s1)
summary(M)
lme.dscore(M,s1,"lme4")

# mediation
medData <- s1 %>%
  group_by(country) %>% 
  summarise(actRating = mean(actRating, na.rm = TRUE),
            affRating = mean(affRating, na.rm = TRUE),
            emoRating = mean(emoRating, na.rm = TRUE),
            negRating = mean(negRating, na.rm = TRUE),
            concern = mean(concern, na.rm = TRUE)) %>% 
  drop_na() 

model.M <- lm(emoRating ~ concern, medData)
model.Y <- lm(actRating ~ emoRating + concern, medData)
results <- mediate(model.M, model.Y, treat="concern", mediator = "emoRating",
                   boot=TRUE, sims = 10000)
summary(model.M)
summary(model.Y)
summary(results)

PROCESS(medData, y = "actRating", x = "concern", meds = "emoRating", nsim = 10000)

#potential alternative mediators
PROCESS(medData, y = "actRating", x = "concern", meds = "negRating", nsim = 10000)
PROCESS(medData, y = "actRating", x = "concern", meds = "affRating", nsim = 10000)

# demos

demos <- study1.all %>%
  dplyr::select(ResponseId,age,gender,race) %>%
  as_tibble()

mean(as.numeric(demos$age),na.rm=TRUE)
sd(as.numeric(demos$age),na.rm=TRUE)
table(demos$gender)
table(demos$race)

