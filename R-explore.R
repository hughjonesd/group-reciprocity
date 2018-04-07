
library(magrittr)
library(dplyr)
library(multiwayvcov)
library(lmtest)
library(effects)
library(car)
filter <- dplyr::filter
setwd("~/Dropbox/Dave_Ro'i/group and indiv reciprocity")
d <- read.csv("Data after reshape corrected.csv", stringsAsFactors = FALSE)
d %<>% filter(give >= 0)
d.orig <- d
d$treatment <- with(d, factor(direct + 2 * greciprocity + 3 * in_group + 4 * (otherdirect + 2 * othergreciprocity + 3 *
      otherin_group), labels = c("nn", "dn", "gn", "in", "nd", "ng", "ni")))
# include only the direct/group/ingroup player, or one out of the two players in the 'nn' treatment
d %<>% filter(treatment %in% c("dn", "gn", "in") ) # %>% filter(treatment != "nn" | recipient==1)
d$treatment %<>% droplevels

m1 <- lm(give ~ treatment + treatment:kindness, data = d, subset = roletrust == 1)
trustorvcov <- . %>% cluster.vcov(d$sid[d$roletrust==1])
coeftest(m1, trustorvcov(m1))
m2 <- lm(give ~ treatment + treatment:sent, data = d, subset = roletrust == 2)
trusteevcov <- .  %>% cluster.vcov(d$sid[d$roletrust==2])
coeftest(m2, trusteevcov(m2))

e1 <- effect(c("treatment:kindness"), m1, vcov. = trustorvcov)
e2 <- effect(c("treatment:sent"), m2, vcov. = trusteevcov)
plot(e1, multiline = TRUE, ci.style="bands", band.transparency = 0.1)
plot(e2, multiline = TRUE, ci.style="bands", band.transparency = 0.1)

# a piecewise version
d$k3 <- cut(d$kindness, quantile(d$kindness,c(0,0.33,0.66,1), na.rm=T))
m.pw <- lm(give ~ treatment + treatment:k3, data = d, subset = roletrust == 1, x = T, y = T)
coeftest(m.pw, trustorvcov(m.pw))
e.pw <- Effect(c("k3", "treatment"), m.pw, vcov. = trustorvcov)
plot(e.pw)

# with svo
d$SVO <- factor(d$svo_angle > median(d$svo_angle), labels=c("low", "high"))
m1.svo <- lm(give ~ 0 + treatment:SVO + treatment:kindness:SVO, data = d, subset = roletrust == 1)
coeftest(m1.svo, trustorvcov(m1.svo))
e.svo <- effect("treatment:SVO:kindness", m1.svo, vcov. = trustorvcov)
plot(e.svo, multiline = TRUE, ci.style = "bands", band.transparency = 0.1)
# not strong evidence:
linearHypothesis(m1.svo, "treatmentgn:SVOhigh:kindness - treatmentgn:SVOlow:kindness = 0", 
      vcov. = trustorvcov)
linearHypothesis(m1.svo, "treatmentdn:SVOhigh:kindness - treatmentdn:SVOlow:kindness = 0", 
  vcov. = trustorvcov)
# but if we don't cut the data, it's a bit stronger, so I tend to believe this:
m1.svoa <- lm(give ~ 0 + treatment + treatment:kindness + treatment:svo_angle + treatment:kindness:svo_angle,
      data = d, subset = roletrust == 1)
coeftest(m1.svoa, trustorvcov(m1.svoa))
# is this just because SVO people give 35, 35? no seems not


# senders with svo. 
m2.svo <- lm(give ~ 0 + (treatment + treatment:sent)*SVO, data = d, subset = roletrust == 2)
coeftest(m2.svo, trusteevcov(m2.svo))
e2.svo <- effect("treatment:sent:SVO", m2.svo, vcov. = trusteevcov)
plot(e2.svo, multiline = TRUE, ci.style = "bands", band.transparency = 0.1)

# robustness: throw out very high kindness
d.trim <- d %>% filter(kindness <= 1.5)
m1trim <- update(m1, data=d.trim)
coeftest(m1trim, trustorvcov(m1trim))
m2trim <- update(m2, data=d.trim)
coeftest(m2trim, trusteevcov(m2trim))

# robustness: period and session FEs
m1.c <- update(m1, . ~ . + factor(period) + factor(session))
coeftest(m1.c, trustorvcov(m1.c))
# robustness: two-level clustering by individual and session
coeftest(m1, cluster.vcov(m1, d %>% filter(roletrust == 1) %>% select(sid, session)))
