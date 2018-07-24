
#
dfr %<>% 
      group_by(sid, otherteam, othernum) %>% 
      mutate(
        n_times_met = seq(0, length.out = n()),
        n_times_met_total = n()
      ) %>% 
      ungroup()


dfr_targets <- dfr %>% 
  ungroup %>% 
  filter(indcondition == condition, condition != "Baseline" | recipient == 1) %>% 
  mutate(
    indcondition = recode(indcondition, Group  = 'Group Reciprocity', Direct = 'Direct Reciprocity'), 
    roletrust = relevel(factor(roletrust), 2)
  )
myplm <- purrr::partial(plm, model = "random", effect = "individual", index = c("sid", "period"), 
  random.method = "walhus")

# include dummy for "previously met"
regs <- dfr_targets %>% group_by(roletrust) %>% do(
  give_mod  = myplm(give ~ 0 + indcondition + factor(sessionid) + n_times_met, .),
  disc_mod  = myplm(discrimination ~ 0 + indcondition + factor(sessionid) + n_times_met, .),
  recip_mod = myplm(give ~ indcondition + indcondition:kindness + factor(sessionid) + n_times_met, .),
  give_mod2 = myplm(give ~ indcondition + factor(sessionid) + n_times_met, .),
  disc_mod2 = myplm(discrimination ~ indcondition + factor(sessionid) + n_times_met, .)
) 


# include only un-met participants
# myplm2 <- purrr::partial(plm, subset = n_times_met == 0, model = "random", effect = "individual", index = c("sid", "period"), 
#   random.method = "walhus")
# 
# regs <- dfr_targets %>% group_by(roletrust) %>% do(
#   give_mod  = myplm2(give ~ 0 + indcondition + factor(sessionid), .),
#   disc_mod  = myplm2(discrimination ~ 0 + indcondition + factor(sessionid), .),
#   recip_mod = myplm2(give ~ indcondition + indcondition:kindness + factor(sessionid), .),
#   give_mod2 = myplm2(give ~ indcondition + factor(sessionid), .),
#   disc_mod2 = myplm2(discrimination ~ indcondition + factor(sessionid), .)
# ) 

mycoeftest <- function (x) filter(tidy(coeftest(x, plm::vcovDC)), ! grepl("factor", term), term != 'n_times_met')
mean_coefs <- regs %>% do(mycoeftest(.$give_mod))
disc_coefs <- regs %>% do(mycoeftest(.$disc_mod))
suppressWarnings({
  recip_coefs_orig <- regs %>% do(mycoeftest(.$recip_mod))
  recip_coefs <- recip_coefs_orig %>% filter(grepl('kindness', term))
  mean_sigs   <- regs %>% do(mycoeftest(.$give_mod2))
})
recip_coefs$estimate[c(1,5)] <- 0 # to avoid formatting probs later
disc_sigs <- regs %>% do(mycoeftest(.$disc_mod2))

mean_stars  <- mystar(mean_sigs$p.value)
disc_stars  <- mystar(disc_sigs$p.value)
recip_stars <- mystar(recip_coefs$p.value)
mean_stars[c(1,5)]  <- ''
disc_stars[c(1,5)]  <- ''
recip_stars[c(1,5)] <- ''

tbl <- data_frame(
  Condition = sub('indcondition', '', mean_coefs$term),
  Allocation = paste0(myfmt(mean_coefs$estimate), ' (', myfmt(round(mean_coefs$std.error, 2)), ') ', mean_stars),
  Discrimination = paste0(myfmt(disc_coefs$estimate), ' (', myfmt(round(disc_coefs$std.error, 2)), ') ', disc_stars),
  Reciprocity = paste0(myfmt(recip_coefs$estimate), ' (', myfmt(round(recip_coefs$std.error, 2)), ')', recip_stars)
)
tbl$Reciprocity[c(1,5)] <- '---'
tbl$Allocation[c(1,5)]  <- '35.00 (---)'

tbl
latex(tbl[, -1], file = '', rgroup = c('Senders', 'Responders'),
  caption = 'Allocations and Discrimination', rowname = tbl$Condition, center = 'centerline', rowlabel = '',
  insert.bottom = '\\justify Mean allocation, mean discrimination, and reciprocity (marginal effect of TG partner\'s kindness on allocation) by condition. Robust standard errors clustered on sessions. Significance of comparison to Baseline is marked.$^{*}$, $^{**}$, and $^{***}$ indicate p $<$ 0.05, p $<$ 0.01, and p $<$ 0.001, respectively.')

# 1 is reciprocity to recipients; 2 is reciprocity to senders
for (r in c("give_mod", "disc_mod", "recip_mod", "give_mod2")) {
  cat(r, "\n")
  print(filter(tidy(coeftest(regs[[r]][[1]], plm::vcovDC)), term=="n_times_met"))
  cat("")
  print(filter(tidy(coeftest(regs[[r]][[2]], plm::vcovDC)), term=="n_times_met"))
  cat("\n\n")
}

