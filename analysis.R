

# Setup -----------------------------------------------------------------------------------------------------------

library(magrittr)
library(dplyr)
library(lme4)

setwd("~/Dropbox/Dave_Ro'i/group and indiv reciprocity")
dfr <- read.csv("data/clean.csv", stringsAsFactors = FALSE)
dfr %<>% filter(give >= 0)
dfr$SVO <- dfr$svo_angle >= median(dfr$svo_angle, na.rm = TRUE)
dfr$hicoll <- dfr$collectivism <= median(dfr$collectivism, na.rm = TRUE)
boot_clust <- function(data, statistic, R, cluster) {
  replicate(R, {
    data <- as.data.frame(data)
    clid <- unique(data[,cluster])
    clids <- sample(clid, replace = TRUE)
    rowids <- unlist(lapply(clids, function(x) which(data[,cluster] == x)))
    resamp <- data[rowids,]
    statistic(resamp)
  })  
}


# Mixed models with cluster bootstrap -----------------------------------------------------------------------------

my_coefs <- function(mod) if (inherits(mod, 'lmerMod')) fixef(mod) else coef(mod)

do_boot_model <- function(mod, R = 500) {
  data <- get_all_vars(mod, model.frame(mod))
  boot_clust(data, . %>% {my_coefs(update(mod, data = .))}, R = R, cluster = 'sid')
}

ci <- function(boot_res, ivl = 0.95) t(apply(boot_res, 1, . %>% quantile(c((1-ivl)/2, 1 - (1-ivl)/2))))
se <- function(boot_res) apply(boot_res, 1, sd)

print.boot_model <- function(bm, ...) {
  attr(bm, 'boot') <- NULL
  attr(bm, 'model') <- NULL
  print.default(unclass(bm), ...)
}

plot.boot_model <- function(bm, ...) {
  xlim <- range(c(bm, 0)) 
  xlim <- xlim + diff(xlim) * c(-.1, .1)
  nr <- nrow(bm)
  plot(bm[,1], nr:1, xlim = xlim, yaxt = "n", xaxt = "s", pch = 19, ylab = "", xlab = "", ...)
  mtext(rownames(bm), 2, at = nr:1, adj = 1, las = 2)
  segments(y0 = nr:1, x0 = bm[,2], x1 = bm[,3])
  abline(v = 0, lty = 2)
}

boot_model <- function(formula, data, ivl = 0.95, ...) {
  mod <- lmer(formula, data, ...)
  boot <- do_boot_model(mod)
  res <- cbind(my_coefs(mod), ci(boot, ivl))
  attr(res, 'model') <- mod
  attr(res, 'boot') <- boot
  class(res) <- 'boot_model'
  res
}


m1 <- boot_model(give ~ kindness + (1|sid), data = dfr %>% filter(greciprocity == 1, roletrust=="Sender"))
m2 <- boot_model(give ~ kindness + (1|sid), data = dfr %>% filter(greciprocity == 1, roletrust=="Responder"))
m3 <- boot_model(give ~ kindness*SVO + (1|sid), data = dfr %>% filter(greciprocity == 1, roletrust=="Sender"))
m4 <- boot_model(give ~ kindness + sent + (1|sid), data = dfr %>% filter(greciprocity == 1, roletrust=="Sender"))
m5 <- boot_model(give ~ kindness*hicoll + (1|sid), data = dfr %>% filter(greciprocity == 1, roletrust=="Sender"))


# Reversed models -------------------------------------------------------------------------------------------------

dfr$lowSVO <- ! dfr$SVO
dfr$locoll <- ! dfr$hicoll
m3r <- boot_model(give ~ kindness*lowSVO + (1|sid), data = dfr %>% filter(greciprocity == 1, roletrust=="Sender"))
m5r <- boot_model(give ~ kindness*locoll + (1|sid), data = dfr %>% filter(greciprocity == 1, roletrust=="Sender"))

# Direct reciprocity models ---------------------------------------------------------------------------------------

m1d <- lm(give ~ kindness, data = dfr %>% filter(direct == 1, roletrust=="Sender"))
m2d <- lm(give ~ kindness, data = dfr %>% filter(direct == 1, roletrust=="Responder"))
m3d <- lm(give ~ kindness*SVO, data = dfr %>% filter(direct == 1, roletrust=="Sender"))
m4d <- lm(give ~ kindness + sent, data = dfr %>% filter(direct == 1, roletrust=="Sender"))
m5d <- lm(give ~ kindness*hicoll, data = dfr %>% filter(direct == 1, roletrust=="Sender"))
m3dr <- lm(give ~ kindness*lowSVO, data = dfr %>% filter(direct == 1, roletrust=="Sender"))
m5dr <- lm(give ~ kindness*locoll, data = dfr %>% filter(direct == 1, roletrust=="Sender"))

# Beautiful regression plots --------------------------------------------------------------------------------------


library(scales)
library(plotrix)

par(mfrow=c(1,2))
b <- attr(m1, "boot")
fe <- fixef(attr(m1, "model"))
with(dfr %>% filter(greciprocity == 1, roletrust=="Sender"), 
  sizeplot(kindness, give, pch = 19, col = alpha("darkgreen", .85), xlab = "Responder kindness", ylab = "Give", main = "Senders")) 
apply(b[1:2,], 2, abline, col = alpha("lightgrey", 0.03), lwd = 3)
abline(fe[1:2], col = "green", lwd = 3)

b <- attr(m2, "boot")
fe <- fixef(attr(m2, "model"))
with(dfr %>% filter(greciprocity == 1, roletrust=="Responder"), 
  sizeplot(kindness, give, pch = 19, col = alpha("navy", .65), xlab = "Sender kindness", ylab = "Give", main = "Responders"))
apply(b[1:2,], 2, abline, col = alpha("lightgrey", 0.03), lwd = 3)
abline(fe[1:2], col = "blue", lwd = 2)


m1d <- lm(give ~ kindness, data = dfr %>% filter(direct == 1, roletrust=="Sender"))
m2d <- lm(give ~ kindness, data = dfr %>% filter(direct == 1, roletrust=="Responder"))

par(mfrow=c(1,2))
with(dfr %>% filter(direct == 1, roletrust=="Sender"), 
  sizeplot(kindness, give, pch = 19, col = alpha("darkgreen", .85), xlab = "Responder kindness", ylab = "Give", main = "Senders")) 
abline(m1d, col = "green", lwd = 3)

# adding confidence intervals
# tmp <- model.frame(m1d)
# s <- sd(tmp$give)/sqrt(nrow(tmp))
# sumxix <- sum((tmp$kindness - mean(tmp$kindness))^2)
# se_pred <- s*sqrt(1/n + (tmp$kindness - mean(tmp$kindness))^2/sumxix)
# lines(tmp$kindness, predict(m1d) + 1.96*se_pred)
# lines(tmp$kindness, predict(m1d) - 1.96*se_pred)

with(dfr %>% filter(direct == 1, roletrust=="Responder"), 
  sizeplot(kindness, give, pch = 19, col = alpha("navy", .65), xlab = "Sender kindness", ylab = "Give", main = "Responders"))
abline(m2d, col = "blue", lwd = 2)


par(mfrow=c(1,2))
b <- attr(m3, "boot")
fe <- fixef(attr(m3, "model"))
with(dfr %>% filter(greciprocity == 1, roletrust=="Sender", ! SVO), 
  sizeplot(kindness, give, pch = 19, col = alpha("black", .65), xlab = "Kindness", ylab = "Give", main = "Low SVO")) 
apply(b[1:2,], 2, abline, col = alpha("orange", 0.03), lwd = 3)
abline(fe[1:2], col = "red", lwd = 2)
with(dfr %>% filter(greciprocity == 1, roletrust=="Sender", SVO), 
  sizeplot(kindness, give, pch = 19, col = alpha("black", .65), xlab = "Kindness", ylab = "Give", main = "High SVO"))
apply(b[1:2,] + b[3:4,], 2, abline, col = alpha("lightblue", 0.03), lwd = 3)
abline(fe[1:2] + fe[3:4], col = "blue", lwd = 2)

m3d <- lm(give ~ kindness*SVO, data = dfr %>% filter(direct == 1, roletrust=="Sender"))
par(mfrow=c(1,2))
with(dfr %>% filter(direct == 1, roletrust=="Sender", ! SVO), 
  sizeplot(kindness, give, pch = 19, col = alpha("black", .65), xlab = "Kindness", ylab = "Give", main = "Low SVO")) 
abline(coef(m3d)[1:2], col = "red", lwd = 2)
with(dfr %>% filter(direct == 1, roletrust=="Sender", SVO), 
  sizeplot(kindness, give, pch = 19, col = alpha("black", .65), xlab = "Kindness", ylab = "Give", main = "High SVO"))
abline(coef(m3d)[1:2] + coef(m3d)[3:4], col = "blue", lwd = 2)



par(mfrow=c(1,2))
b <- attr(m5, "boot")
fe <- fixef(attr(m5, "model"))
with(dfr %>% filter(greciprocity == 1, roletrust=="Sender", ! hicoll), 
  sizeplot(kindness, give, pch = 19, col = alpha("black", .65), xlab = "Kindness", ylab = "Give", main = "Low collectivism")) 
apply(b[1:2,], 2, abline, col = alpha("orange", 0.03), lwd = 3)
abline(fe[1:2], col = "red", lwd = 2)
with(dfr %>% filter(greciprocity == 1, roletrust=="Sender", hicoll), 
  sizeplot(kindness, give, pch = 19, col = alpha("black", .65), xlab = "Kindness", ylab = "Give", main = "High collectivism"))
apply(b[1:2,] + b[3:4,], 2, abline, col = alpha("lightblue", 0.03), lwd = 3)
abline(fe[1:2] + fe[3:4], col = "blue", lwd = 2)

# to find significance of interactions, generate locoll and noSVO and rerun boot_models