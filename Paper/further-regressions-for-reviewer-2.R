
# Table 2 but with per-individual random effects (i.e. random intercepts)
# 
# Replaces the `table` R section in the Rnw file, i.e. run `setup` first
 
library(lme4)
library(broom)

dfr_targets <- dfr %>% ungroup %>% filter(indcondition == condition) %>% mutate(
  indcondition = recode(indcondition, Group  = 'Group Reciprocity', Direct = 'Direct Reciprocity'), 
  roletrust = relevel(factor(roletrust), 2)
)
regs <- dfr_targets %>% group_by(roletrust) %>% do(
  give_mod  = lmer(give ~ 0 + indcondition + (1 | sid), .),
  disc_mod  = lmer(discrimination ~ 0 + indcondition + (1 | sid), .),
  recip_mod = lmer(give ~ indcondition + indcondition:kindness + (1 | sid), .),
  give_mod2 = lmer(give ~ indcondition + (1 | sid), .),
  disc_mod2 = lmer(discrimination ~ indcondition + (1 | sid), .)
) 

mean_coefs <- regs %>% do(tidy(.$give_mod, effects = "fixed"))
disc_coefs <- regs %>% do(tidy(.$disc_mod, effects = "fixed"))
suppressWarnings({
  recip_coefs_orig <- regs %>% do(tidy(.$recip_mod, effects = "fixed"))
  recip_coefs <- recip_coefs_orig %>% filter(grepl('kindness', term))
  mean_sigs   <- regs %>% do(tidy(.$give_mod2, effects = "fixed"))
})
recip_coefs$estimate[c(1,5)] <- 0 # to avoid formatting probs later
disc_sigs <- regs %>% do(tidy(.$disc_mod2, effects = "fixed"))


tbl <- data_frame(
  Condition = sub('indcondition', '', mean_coefs$term),
  Allocation = paste0(myfmt(mean_coefs$estimate), ' (', myfmt(round(mean_coefs$std.error, 2)), ') '),
  Discrimination = paste0(myfmt(disc_coefs$estimate), ' (', myfmt(round(disc_coefs$std.error, 2)), ') '),
  Reciprocity = paste0(myfmt(round(recip_coefs$estimate, 2)), ' (', myfmt(round(recip_coefs$std.error, 2)), ')')
)
tbl$Reciprocity[c(1,5)] <- '---'
tbl$Allocation[c(1,5)]  <- '35.00 (---)'

# Per-individual random reciprocity slopes

summary(lmer(give ~ indcondition + indcondition:kindness + (0 + indcondition | sid), 
      data = dfr_targets, subset = roletrust == "Sender"))
summary(lmer(give ~ indcondition + indcondition:kindness + (0 + indcondition | sid), 
  data = dfr_targets, subset = roletrust == "Responder"))

# Controlling for initial amount sent

dfr_targets$sent_demeaned <- dfr_targets$sent - mean(dfr_targets$sent)
m1 <- lm(give ~ kindness + sent_demeaned, dfr_targets, subset = roletrust == "Sender" & indcondition == 
    "Group Reciprocity")
m2 <- lm(give ~ kindness * sent_demeaned, dfr_targets, subset = roletrust == "Sender" & indcondition == 
    "Group Reciprocity")
coeftest(m1, cluster.vcov(m1, ~ session))
coeftest(m2, cluster.vcov(m2, ~ session))




