
################################################################################

# DOM Networks - Exploratory Analyses

################################################################################

# This script assumes you have already run DOM-networks_primary-analysis.R

# Wrangle ----------------------------------------------------------------------

info_disc_summary <- info_disc_long %>% 
  group_by(sub, info_type) %>% 
  summarise(
    sum = sum(disclosure)
  ) 

info_disc_wide <- info_disc_summary %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = "sub",
    names_from = "info_type",
    values_from = "sum"
  )

topic_disc_summary <- info_disc_long %>% 
  group_by(sub, topic, info_type) %>% 
  summarise(
    sum = sum(disclosure) / 4
  ) 

topic_disc_wide <- topic_disc_summary %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = c("sub", "topic"),
    names_from = "info_type",
    values_from = "sum"
  )

# Plot -------------------------------------------------------------------------

info_labs        <- c("Guarded", "High-Stakes", "Low-Stakes", "Unguarded")
names(info_labs) <- c("gu", "hs", "ls", "ug") 

sub_disclosure_hist <- 
ggplot(info_disc_summary,
       aes(
         x = sum
       )) +
  facet_wrap(
    ~ info_type,
    labeller = labeller(info_type = info_labs)
  ) +
  geom_histogram(
    binwidth = 1,
    color = "grey"
  ) +
  labs(
    y = "Frequency",
    x = "Total Items Disclosed"
  ) +
  theme_classic()

topic_disclosure_hist <- 
  ggplot(topic_disc_summary,
         aes(
           x = sum
         )) +
  facet_wrap(~ info_type) +
  geom_histogram(
    binwidth = .25
  ) +
  theme_classic()

cowplot::save_plot("sub_disclosure_hist.tiff", sub_disclosure_hist, base_height = 6, base_width = 8)

# Models including the first affiliation measure -------------------------------

## Wrangling

info_disc_long <- info_disc_long %>% 
  left_join(select(raw, sub = ResponseId, IOS.pre_1), by = "sub") %>% 
  mutate(
    IOS.pre_1 = scale(IOS.pre_1, scale = FALSE)
  )

## Model fitting

model_1_IOS <- glmer(disclosure ~ risk + benefit + IOS.pre_1 + (1|sub) + (1|topic) + (1|information), 
                 family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_2_IOS <- glmer(disclosure ~ (risk + benefit + IOS.pre_1)^2 + (1|sub) + (1|topic) + (1|information), 
                 family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_3_IOS <- glmer(disclosure ~ (risk + benefit + IOS.pre_1)^2 + (1|group:sub) + (1|group) + (1|topic) + (1|information), 
                 family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_4_IOS <- glmer(disclosure ~ (risk + benefit + IOS.pre_1)^2 + (1 + risk + benefit|group:sub) + (1|group) + (1|topic) + (1|information), 
                 family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_5_IOS <- glmer(disclosure ~ (risk + benefit + IOS.pre_1)^2 + (1 + risk + benefit|group:sub) + (1+ risk + benefit|group) + (1|topic) + (1|information), 
                 family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_6_IOS <- glmer(disclosure ~ (risk + benefit + IOS.pre_1)^3 + (1 + risk + benefit|group:sub) + (1+ risk + benefit|group) + (1|topic) + (1|information), 
                     family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

## Likelihood ratio tests

lrt_IOS <- anova(model_1_IOS, model_2_IOS, model_3_IOS, model_4_IOS, model_5_IOS, model_6_IOS)

# Models with no interaction term ----------------------------------------------

## Model fitting

model_1_noint <- glmer(disclosure ~ risk + benefit + (1|sub) + (1|topic) + (1|information), 
                 family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_2_noint <- glmer(disclosure ~ risk + benefit + (1|group:sub) + (1|group) + (1|topic) + (1|information), 
                 family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_3_noint <- glmer(disclosure ~ risk + benefit + (1 + risk + benefit|group:sub) + (1|group) + (1|topic) + (1|information), 
                 family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_4_noint <- glmer(disclosure ~ risk + benefit + (1 + risk + benefit|group:sub) + (1+ risk + benefit|group) + (1|topic) + (1|information), 
                 family = binomial(), data = info_disc_long, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

## Likelihood ratio tests

lrt_noint <- anova(model_1_noint, model_2_noint, model_3_noint, model_4_noint)
