

# Setup -----------------------------------------------------------------------------------------------------------

library(magrittr)
library(dplyr)
library(multiwayvcov)
library(lmtest)
library(ggplot2)
filter <- dplyr::filter

setwd("~/Dropbox/Dave_Ro'i/group and indiv reciprocity")
dfr <- read.csv("data/clean.csv", stringsAsFactors = FALSE)
dfr <- dfr %>% filter(give >= 0)

dfr$kind_split <- dfr$kindness >= median(dfr$kindness[dfr$roletrust == "Sender"], na.rm = T)
dfr$kind_split_send <- dfr$sent >= median(dfr$sent[dfr$roletrust == "Responder"])

dfr %<>% group_by(sid, indcondition) %>% mutate(n_choices = n())
save_pics <- FALSE

# Histograms ------------------------------------------------------------------------------------------------------



with(dfr %>% filter(period == 1, recipient == 1, roletrust == "Sender"), 
  hist(sent/150, 30, col = "darkgreen", main = "", xlab = "Senders' Kindness", ylim = c(0, 35)))
abline(v = 1/2, col = "red", lty = 2, lwd = 2)
text(1/2, 25, "median", col = "red", srt = 90, adj = c(0, 1))
if (save_pics) quartz.save("sender-kindness.png")

with(dfr %>% filter(period == 1, recipient == 1, roletrust == "Responder"),
  hist(kindness, 30, col = "wheat4", main = "", xlab = "Responders' Kindness", ylim = c(0, 30)))
abline(v = 1/3, col = "red", lty = 2, lwd = 2)
text(1/3, 20, "median", col = "red", srt = 90, adj = c(0, 1))
if (save_pics) quartz.save("responder-kindness.png")

layout(matrix(1:4, 2))
par(mar = c(4.1,4,2.5,0))
with(dfr %>% filter(condition=="Baseline"), hist(give, 36, col = "grey", border = NA, xlab = "", main = "Baseline"))
with(dfr %>% filter(direct==1), hist(give, 36, col = "orange", border = NA, xlab = "Amount given", main = "Direct"))
with(dfr %>% filter(ingroup==1), hist(give, 36, col = "navy", border = NA, xlab = "", main = "Ingroup", ylab = ''))
with(dfr %>% filter(greciprocity==1), hist(give, 36, col = "red", border = NA, xlab = "Amount given", main = "GR", ylab = ''))

if (save_pics) quartz.save("hist-give.png", width = 7, height = )

# Giving plot ----------------------------------------------------------------------------------------------


library(plotrix)
library(scales)
mycol <- alpha("darkblue", 0.75)

dfrl <- dfr %>% select(sid, give, indcondition, kindness, sent, roletrust)
dfrl <- dfrl %>% filter(indcondition == "Direct") %>% 
  left_join(dfrl %>% filter(indcondition == "Group"), by = c('sid', 'kindness', 'sent', 'roletrust'))
sizeplot(dfrl$give.x, dfrl$give.y, pch = 19, col = mycol, xlab = "Give (DR)", ylab = "Give (GR)")
m1 <- lm(give.y ~ give.x, dfrl)
abline(m1, col = "red")
if (save_pics) quartz.save("giving-direct-by-group.png", width = 5, height = 5)
cor.test(~ give.x + give.y, dfrl)
# does the correlation survive if we control for kindness?
# NB forcing effect of sent/kindness to zero when role is the wrong one:
summary(lm(give.y ~ give.x + roletrust + I(sent*(roletrust=="Responder")) + I(kindness*(roletrust=="Sender")), dfrl))

# Barplots --------------------------------------------------------------------------------------------------------


library(ggplot2)
library(gridExtra)

p1 <- ggplot(dfr %>% filter(direct == 1, roletrust == "Sender"), aes(x = kind_split, y = give)) + 
  stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal) + 
  labs(x = "Kindness >= median", title = "Senders")
p2 <- ggplot(dfr %>% filter(direct == 1, roletrust == "Responder"), aes(x = kind_split_send, y = give)) +
  stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal) + 
  labs(x = "Kindness >= median", title = "Responders")
grid.arrange(p1, p2, top = "DR Treatment", ncol = 2, nrow = 1)
if (save_pics) quartz.save("barplot-dr.png", width = 6, height = 4, bg = "white")

p3 <- ggplot(dfr %>% filter(greciprocity == 1, roletrust == "Sender"), aes(x = kind_split, y = give)) + 
  stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal) +
  labs(x = "Kindness >= median", title = "Senders")
p4 <- ggplot(dfr %>% filter(greciprocity == 1, roletrust == "Responder"), aes(x = kind_split_send, y = give)) + 
  stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal) + 
  labs(x = "Kindness >= median", title = "Responders")
grid.arrange(p3, p4, top = "GR Treatment", ncol = 2, nrow = 1)
if (save_pics) quartz.save("barplot-gr.png", width = 6, height = 4, bg = "white")

wilcox.test(give ~ kind_split_send, dfr, subset = greciprocity == 1 & roletrust == "Responder")
wilcox.test(give ~ kind_split, dfr, subset = greciprocity == 1 & roletrust == "Sender")

wilcox.test(give ~ kind_split_send, dfr, subset = direct == 1 & roletrust == "Responder")
wilcox.test(give ~ kind_split, dfr, subset = direct == 1 & roletrust == "Sender")

# SVO/collectivism barplots ----------------------------------------------------------------------------------------------------


dfr$SVO <- factor(dfr$svo_angle >= median(dfr$svo_angle), labels = c("Low SVO", "High SVO"))
# NB: *low* scores on the collectivism measure indicated high collectivism
dfr$col_split <- factor(dfr$collectivism >= median(dfr$collectivism, na.rm = TRUE), labels = c("High coll.", "Low coll."))
dfr$col_split <- factor(dfr$col_split, levels = c("Low coll.", "High coll."))

ggplot(dfr %>% filter(greciprocity == 1, roletrust == "Sender"), aes(x = kind_split, y = give)) + 
  stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal) + 
  labs(x = "Kindness >= median", title = "Senders") + facet_grid(~ SVO)
if (save_pics) quartz.save("barplot-svo-gr.png", bg = "white", height = 4)

ggplot(dfr %>% filter(direct == 1, roletrust == "Sender"), aes(x = kind_split, y = give)) + 
  stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal) + 
  labs(x = "Kindness >= median", title = "Senders") + facet_grid(~ SVO)
if (save_pics) quartz.save("barplot-svo-dr.png", bg = "white", height = 4)

t.test(give ~ kind_split, dfr %>% filter(direct == 1, roletrust == "Sender"), subset = SVO == "Low SVO")
t.test(give ~ kind_split, dfr %>% filter(greciprocity == 1, roletrust == "Sender"), subset = SVO == "Low SVO")

ggplot(dfr %>% filter(greciprocity == 1, roletrust == "Sender", ! is.na(col_split)), aes(x = kind_split, y = give)) + 
  stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal) + 
  labs(x = "Kindness >= median", title = "Senders") + facet_grid(~ col_split)
if (save_pics) quartz.save("barplot-collectivism-gr.png", bg = "white", height = 4)

t.test(give ~ kind_split, dfr %>% filter(greciprocity == 1, roletrust == "Sender"), subset = col_split == "High coll.")

m1 <- lm(give ~ kind_split*SVO, dfr, subset = greciprocity==1 & roletrust=="Sender")
m2 <- lm(give ~ kind_split*col_split, dfr, subset = greciprocity==1 & roletrust=="Sender")
library(multiwayvcov)
set.seed(27101975)
# wild bootstrap avoids NAs in values for interaction - see Cameron Gehlbach and Miller 2008
coeftest(m1, cluster.boot(m1, ~ sid, parallel = TRUE, boot_type="wild", R = 999))
coeftest(m2, cluster.boot(m2, ~ sid, parallel = TRUE, boot_type="wild", R = 999)) 
