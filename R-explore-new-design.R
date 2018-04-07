

# Import data -----------------------------------------------------------------------------------------------------


library(dplyr)
library(magrittr)
Sys.setlocale('LC_ALL','C')
setwd("~/Dropbox/Dave_Ro'i/group and indiv reciprocity/")
source("zTree.r")
#d <- zTreeTables("150615_1224.xls")
d <- zTreeTables(list.files("data", pattern = "*.xls", full.names = TRUE))

# Clean data ------------------------------------------------------------------------------------------------------

ss <- d$subjects %>% dplyr::filter(Treatment==2)
ss %<>% 
  select(
    - starts_with("input"),
    - starts_with("chosen"),
    - starts_with("item"),
    - starts_with("GroupCorrect"),
    - starts_with("random_"),
    - starts_with("input"),
    - starts_with("Time"), 
    - matches('\\[')
  ) %>%  
  mutate(
    RoleTrust = factor(RoleTrust, labels=c("Trustor", "Trustee")), 
    kindness = TrustGameResponder / TrustGame,
    OtherNum1 = Other1Num,
    OtherNum2 = Other2Num,
    OtherTeam1 = Other1Team,
    OtherTeam2 = Other2Team,
    upair = paste(Date, Pair, sep=".")
  )

ss %<>% group_by(Pair) %>% mutate(inpair = row_number(), oth_inpair = 3 - inpair) %>% ungroup()

table(ss$RoleTrust, ss$upair) -> problems
problems <- colnames(problems)[which(problems[1,] != 1)] # 2 trustors/trustees in a pair!
ss %<>% dplyr::filter(! upair %in% problems) 

ss %<>% left_join(
     ss %>% select(upair, oth_inpair, p_group = Group, p_num = InGroup), 
     by=c("upair"="upair", "inpair"="oth_inpair")
   ) 

# remove dodgy data

# Explore ---------------------------------------------------------------------------------------------------------


ss %>% mutate(
    d1 = p_group == OtherTeam1 & p_num == OtherNum1, 
    g1 = p_group == OtherTeam1 & p_num != OtherNum1, 
    i1 = Group == OtherTeam1,
    d2 = p_group == OtherTeam2 & p_num == OtherNum2, 
    g2 = p_group == OtherTeam2 & p_num != OtherNum2, 
    i2 = Group == OtherTeam2,
    tr1 = factor(i1 + 2 * d1 + 3 * g1, labels = c("Neut", "Ind", "Group", "Ingroup")),
    tr2 = factor(i2 + 2 * d2 + 3 * g2, labels = c("Neut", "Ind", "Group", "Ingroup")),
    tr = paste(tr1, tr2, sep="-"),
    all1 = Allocation,
    all2 = AllocationDummy
  ) %>% 
  select(d1, d2, g1, g2, i1, i2, OtherTeam1, OtherNum1, OtherTeam2, OtherNum2, p_group, p_num, all1, all2, tr1, tr2, tr
  ) -> tmp


