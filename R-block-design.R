
subj <- 1:24
nums <- rep(1:4, 6)

splits <- list(1:2, c(1,3), c(1,4), 3:4, c(2,4), c(2,3))
tms <- matrix(NA, ncol=6, nrow=24)
groups <- matrix(NA, ncol=6, nrow=24)
for (p in 1:6) {
  hex1 <- subj[ nums %in% splits[[p]] ]
  hex2 <- subj[ nums %in% setdiff(1:4, splits[[p]]) ]
  hex2 <- hex2[12:1]  # rotate (add 2 = 60)
  period.rot <- c(5:12,1:4)   # must be a multiple of 60 degrees (i.e. 2) to keep 
                              # "opposite groups" together
  for (rot in 1:p) {hex1 <- hex1[period.rot]; hex2 <- hex2[period.rot]}
  for (g in 1:2) {
    h <- list(hex1, hex2)[[g]]
    tms[h[c(1,2,5)], p]   <- c("IN", "IN", "NN") # IIN
    groups[h[c(1,2,5)], p] <- paste0("IIN", g)
    tms[h[c(3,10,11)], p] <- c("GN", "GN", "NN") # GGN
    groups[h[c(3,10,11)], p] <- paste0("GGN", g*2-1)
    tms[h[c(4,9,8)], p]   <- c("GN", "GN", "NN") # GGN
    groups[h[c(4,9,8)], p] <- paste0("GGN", g*2)
    tms[h[c(6,12,7)], p]   <- c("DN", "DN", "NN") # DDN
    groups[h[c(6,12,7)], p] <- paste0("DDN", g)
  }
}

indivs <- cbind(DN=rowSums(tms=="DN"), IN=rowSums(tms=="IN"), 
      GN=rowSums(tms=="GN"), NN=rowSums(tms=="NN"))
sum(indivs==0)
periods <- cbind(DN=colSums(tms=="DN"), IN=colSums(tms=="IN"), 
  GN=colSums(tms=="GN"), NN=colSums(tms=="NN"))

groups <- as.data.frame(groups)
colnames(groups) <- paste0("period.", 1:6)
groups$colour <- rep(1:6, each=4)
groups$number <- nums
write.csv(groups, file="newmatching.csv")
# L1, L2, TR1 = an IIN group (count L1=1, clockwise)
# TL1, BR2, BL1 = a GGN group
# TL2, BR1, R2 = a GGN group
# TR2, BL2, R1 = a DDN group
#     2  1
#    1    2
# 2          1
# 1          2
#    2    1
#     1  2
