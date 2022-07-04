
################################################################################

# DOM Networks -- Wrangling and Analysis

################################################################################

# Load and set up --------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lme4)

raw <- read_xlsx("DOM-networks_data-raw.xlsx")

# Preliminary cleaning ---------------------------------------------------------

raw <- raw %>% 
  slice(-1, -2) %>% 
  type_convert()

raw <- raw %>% 
  filter(Status == 0 & Finished == 1)

raw <- raw %>% 
  mutate(
    exclusion = case_when(
      mem.check.1.points == 0  | mem.check.2.points == 0  ~ 1,
      mem.check.1.points == 25 & mem.check.2.points == 25 ~ 0
    )
  ) %>% 
  filter(exclusion == 0)

# Reshaping --------------------------------------------------------------------

info_disc <- raw %>% 
  select(sub = ResponseId,
         group,
         starts_with("gu"), 
         starts_with("hs"), 
         starts_with("ug"), 
         starts_with("ls"))

info_disc_long <- info_disc %>% 
  pivot_longer(
    cols = c(starts_with("gu"), starts_with("hs"), starts_with("ug"), starts_with("ls")),
    names_to = "information",
    values_to = "disclosure"
  ) %>% 
  mutate(
    disclosure = case_when(
      !is.na(disclosure) ~ 1,
      is.na(disclosure)  ~ 0
    )
  )

info_disc_long <- info_disc_long %>% 
  extract(
    col   = "information",
    into  = c("info_type", "item_number"),
    regex = "(..)\\.po\\.(.*)",
    remove = FALSE
  ) %>% 
  mutate(
    info_type = as.factor(info_type),
    risk      = case_when(
      info_type == "gu" | info_type == "hs" ~ 1,
      info_type == "ug" | info_type == "ls" ~ 0
    ),
    benefit   = case_when(
      info_type == "ug" | info_type == "hs" ~ 1,
      info_type == "gu" | info_type == "ls" ~ 0
    ),
    item_number = as.numeric(item_number),
    topic = case_when(
      item_number < 5                    ~ 1,
      item_number >= 5 & item_number < 9 ~ 2,
      item_number >= 9                   ~ 3
    )
  )


# Main analysis ----------------------------------------------------------------

## Model fitting

model_1 <- glmer(disclosure ~ risk + benefit + (1|sub) + (1|topic) + (1|information), 
                 family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_2 <- glmer(disclosure ~ risk * benefit + (1|sub) + (1|topic) + (1|information), 
                 family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_3 <- glmer(disclosure ~ risk * benefit + (1|group:sub) + (1|group) + (1|topic) + (1|information), 
                 family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_4 <- glmer(disclosure ~ risk * benefit + (1 + risk + benefit|group:sub) + (1|group) + (1|topic) + (1|information), 
                 family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_5 <- glmer(disclosure ~ risk * benefit + (1 + risk + benefit|group:sub) + (1+ risk + benefit|group) + (1|topic) + (1|information), 
                 family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

## Likelihood ratio tests

lrt <- anova(model_1, model_2, model_3, model_4, model_5)

