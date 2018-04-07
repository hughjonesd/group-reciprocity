
library(rms)
library(lme4)
library(reshape2)
library(ggplot2)

setwd("~/Dropbox/Dave_Ro'i/group and indiv reciprocity")
source("zTree.R")


files <- list.files(pattern="[0-9]{6}_[0-9]{4}\\.xls")
Sys.setlocale('LC_ALL','C')
tmp <- zTreeTables(files, tables="subjects")
Sys.setlocale('LC_ALL','en_US.UTF-8')
ds <- tmp$subjects

# from pilot. Should probably be joined on to above
# ds <- read.csv("Pilot.csv")
# ds <- ds[! is.na(ds$Period),]

ds <- ds[! is.na(ds$TrustGame),]
ds$sessid <- as.numeric(factor(ds$Date))
ds$uid <- paste(ds$sessid, ds$Subject, sep=".")
ds$upair  <- paste(ds$sessid, ds$Pair, sep=".")
ds$ugroup  <- paste(ds$sessid, ds$Group, sep=".")
ds$sent <- ave(ds$TrustGame, ds$uid, FUN=max)  
ds$returned <- ave(ds$TrustGameResponder, ds$uid, FUN=max) 
ds$kindness <- ds$returned/ds$sent
ds$partner_team <- ave(ds$Group, ds$upair, ds$Period, FUN=sum)
ds$partner_team <- ds$partner_team - ds$Group
ds$partner_id <- ave(ds$InGroup, ds$upair, ds$Period, FUN=sum)
ds$partner_id <- ds$partner_id - ds$InGroup
ds$give_1 <- ds$Allocation
ds$give_2 <- ds$AllocationDummy
ds$otherteam_1 <- ds$Other1Team
ds$otherteam_2 <- ds$Other2Team
ds$othernum_1 <- ds$Other1Num
ds$othernum_2 <- ds$Other2Num

vars <- c(outer(c("give_", "otherteam_", "othernum_"), 1:2, FUN=paste0))
dsl <- reshape(ds, 
      varying=vars, timevar="Recipient", idvar=c("uid", "Period"), 
      sep="_", direction="long", new.row.names=1:(nrow(ds)*2))

dsl$direct <- with(dsl, otherteam==partner_team & othernum==partner_id)
dsl$greciprocity <- with(dsl, otherteam==partner_team & othernum!=partner_id)
dsl$target <- factor(dsl$direct+2*dsl$greciprocity, labels=c("Neutral", "Direct",
  "Group rec."))

# pictures
# raw data
ggplot(dsl, aes(x=factor(Recipient), y=give, fill=target)) + geom_bar(stat="identity") +   
      ylim(-2,70) + facet_grid(Period~Subject)
# subject averages
ggplot(dsl, aes(x=target, y=give, fill=target)) + 
      stat_summary(fun.y=mean, geom="bar") + facet_wrap(~Subject)

# basic scatterplots + reg. lines
ggplot(dsl[dsl$RoleTrust==1,], aes(x=kindness, y=give, colour=target)) + 
      geom_point(alpha=.5) + stat_sum(aes(size=..n..)) +
      stat_smooth(method="lm", se=FALSE) + scale_size(range=c(3,10)) + 
      ggtitle("Trustors")
ggplot(dsl[dsl$RoleTrust==2,], aes(x=sent, y=give, colour=target)) + 
      geom_point(alpha=.7) + stat_sum(aes(size=..n..)) +
      stat_smooth(method="lm", se=FALSE) + scale_size(range=c(3,10)) + 
      ggtitle("Trustees")

# statistics


# subject/treatment averages. 
dss <- melt(dsl[,c("uid", "direct", "greciprocity", "give", "sent", "kindness",
      "RoleTrust")], id.vars=c("uid", "direct", "greciprocity"))
dss <- dcast(dss, uid + direct + greciprocity ~ variable, fun.aggregate=mean)

# can't think of non-parametric way to test interactions, so:
cor.test( ~ give + kindness, data=dss, subset=RoleTrust==1 & direct, method="spearman")
cor.test( ~ give + kindness, data=dss, subset=RoleTrust==1 & greciprocity, method="spearman")
cor.test( ~ give + kindness, data=dss, subset=RoleTrust==1 & ! greciprocity & ! direct, method="spearman")

cor.test( ~ give + sent, data=dss, subset=RoleTrust==2 & direct, method="spearman")
cor.test( ~ give + sent, data=dss, subset=RoleTrust==2 & greciprocity, method="spearman")
cor.test( ~ give + sent, data=dss, subset=RoleTrust==2 & ! greciprocity & ! direct, method="spearman")

# models using means only
mm1 <- ols(give ~ kindness*(direct+greciprocity), data=dss, subset=RoleTrust==1, x=T, y=T)
mm1 <- robcov(mm1, dss$uid[dss$RoleTrust==1])
mm2 <- ols(give ~ sent*(direct+greciprocity), data=dss, subset=RoleTrust==2, x=T, y=T)
mm2 <- robcov(mm2, dss$uid[dss$RoleTrust==2])

# using returned makes little sense, it is bounded above by 0
# m1 <- ols(give ~ sent + returned*(direct+greciprocity), data=dsl, 
#       subset=RoleTrust==1, x=T, y=T)
# m1 <- robcov(m1, dsl$Subject[dsl$RoleTrust==1])
m2 <- ols(give ~ kindness + sent*(direct+greciprocity), data=dsl,
      subset=RoleTrust==2, x=T, y=T)
m2 <- robcov(m2, dsl$uid[dsl$RoleTrust==2])

# throws out one subject who sent 0
m1k <- ols(give ~ sent + kindness*(direct+greciprocity), data=dsl, 
  subset=RoleTrust==1, x=T, y=T)
m1k <- robcov(m1k, dsl$uid[dsl$RoleTrust==1])

# RE models
m1re <- lmer(give ~ sent + kindness*(direct+greciprocity) + (1|Subject), data=dsl, 
  subset=RoleTrust==1)
m2re <- lmer(give ~ sent*(direct+greciprocity) + (1|Subject), data=dsl, 
  subset=RoleTrust==2)

# thoughts:
# roles aren't randomized across teams -- teams 1-3 are always trustors
# this adds a tiny confound
# seems easy to fix

# am I right in thinking subjects never played against their own group member?

# some subjects see more direct/greciprocity partners than others... 
# it makes sense to balance across subjects
table(dsl$Subject, dsl$greciprocity)
table(dsl$Subject, dsl$direct)
# more seriously: some PERIODS have more direct/greciprocity partners than others.. 
table(dsl$Period, dsl$greciprocity)
table(dsl$Period, dsl$direct)
