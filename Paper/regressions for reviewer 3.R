
# random, nested intercepts
# 
# reviewer 3 insisted on these :-/
# 
# Replaces the `table` R section in the Rnw file, i.e. run `setup` first



dfr_targets <- dfr %>% ungroup %>% 
  filter(indcondition == condition, (condition != "Baseline") | (recipient == 1)) %>% mutate(
    indcondition = recode(indcondition, Group  = 'Group Reciprocity', Direct = 'Direct Reciprocity'), 
    roletrust = relevel(factor(roletrust), 2)
  )

library(plm)
myplm <- purrr::partial(plm, model = "random", effect = "individual", index = c("sid", "period"), random.method="walhus")
regs_plm <- dfr_targets %>% group_by(roletrust) %>% do(
  give_mod  = myplm(give ~ 0 + indcondition + factor(sessionid), .),
  disc_mod  = myplm(discrimination ~ 0 + indcondition + factor(sessionid), .),
  recip_mod = myplm(give ~ indcondition + indcondition:kindness + factor(sessionid), .),
  give_mod2 = myplm(give ~ indcondition + factor(sessionid), .),
  disc_mod2 = myplm(discrimination ~ indcondition + factor(sessionid), .)
) 

regs <- regs_plm
mycoeftest <- function (x) dplyr::filter(tidy(coeftest(x, vcovDC)), ! grepl("factor", term))
mean_coefs <- regs_plm %>% do(mycoeftest(.$give_mod))
disc_coefs <- regs_plm %>% do(mycoeftest(.$disc_mod))
suppressWarnings({
  recip_coefs_orig <- regs_plm %>% do(mycoeftest(.$recip_mod))
  recip_coefs <- recip_coefs_orig %>% filter(grepl('kindness', term))
  mean_sigs   <- regs_plm %>% do(mycoeftest(.$give_mod2))
})
recip_coefs$estimate[c(1,5)] <- 0 # to avoid formatting probs later
disc_sigs <- regs_plm %>% do(mycoeftest(.$disc_mod2))


tbl <- data_frame(
  Condition = sub('indcondition', '', mean_coefs$term),
  Allocation = paste0(myfmt(mean_coefs$estimate), ' (', myfmt(round(mean_coefs$std.error, 2)), ') '),
  Discrimination = paste0(myfmt(disc_coefs$estimate), ' (', myfmt(round(disc_coefs$std.error, 2)), ') '),
  Reciprocity = paste0(myfmt(round(recip_coefs$estimate, 2)), ' (', myfmt(round(recip_coefs$std.error, 2)), ')')
)
tbl$Reciprocity[c(1,5)] <- '---'
tbl$Allocation[c(1,5)]  <- '35.00 (---)'

