# this file provides two functions: zTreeTables and zTreeSbj
# author: Oliver Kirchkamp http://www.kirchkamp.de/
# date: 2009-12-14
#----------------------------------------
# zTreeTables is an R function to read zTree .xls files
# The return value is a list of dataframes, one for each table
#
# Example:
#
# source("http://www.kirchkamp.de/lab/zTree.R")
# zTT <- zTreeTables ( "090602_1746.xls" , "contracts" )
# zTT <- zTreeTables ( c("090602_1746.xls","090602_1912.xls"), c("globals","subjects", "contracts" ))
# plot(Profit ~ Period,data=zTT$subjects)
#

zTreeTables <- function(filelist,tables=c("globals","subjects")) {
  splittable <- function(filename,tables=c("globals","subjects")) {
    getTable <- function(start, stop) {
      if (!is.na(stop) && !is.na(start)) {
        names<-aa2[[start]][-3]
        names[1]<-"Date"
        names[2]<-"Treatment"
        tab<-as.data.frame(matrix(nrow=stop-start-1,ncol=length(names)))
        colnames(tab)<-names
        for( i in 1:(stop-start-1)) {
          tab[i,] <- aa2[[start+i]][-3]
        }
        for (n in colnames(tab)) {
          if (is.character(tab[[n]])) {
            tab[[n]][tab[[n]]=="-"] <- NA
            mm<-mean(as.numeric(tab[[n]]),na.rm=TRUE)
            if (!is.na(mm)) {
              tab[[n]]<-as.numeric(tab[[n]])
            }
          }
        }
        tab
      }
    }
    
    getTables <- function(name) {
      tab<-NULL
      for (i in which ((splitname==name))) {
        new<-getTable(splitpoints[i],splitpoints[i+1])
        if (length(new)>0) {
          if (is.null(tab)) {
            tab<-new
          } else {
            tab <- merge(tab,new,all=TRUE)
          }
        }
      }
      tab
    }
    cat("reading ",filename,"...\n")
    Tfile<-file(filename,"r")
    aa<-readLines(Tfile)
    close(Tfile)
    aa2<-strsplit(aa,"\t")
    splitpoints<-array()
    splitname<-array()
    splittreat<-array()
    table(splitname)
    splitcols<-array()
    last<-0
    for (i in 1:length(aa2)) {
      if (last==0 || (aa2[[i]][3] != aa2[[i-1]][3])) {
        last<-last+1
        splitpoints[last]<-i
        splitname[last]<-aa2[[i]][3]
        splittreat[last]<-aa2[[i]][2]
        splitcols[last]<-length(aa2[[i]])
      }
      splitpoints[last+1]<-i+1
    }
    # cat(splitpoints)
    result<-list()
    do <- intersect(splitname,tables)
    miss <- setdiff(splitname,tables)
                                        #if (length(miss)>0)
    cat ("Skipping:",miss,"\n")
    for (name in do) {
      cat ("Doing:",name,"\n")
      aTable<-getTables(name)
      if (!is.null(aTable)) result[[name]]<-aTable
    }
    result
  }
  
  
  z<-splittable(filelist[1],tables)
  for (name in filelist[-1]) {
    cat (sprintf("*** %s is file %d / %d ***\n",name,which(name==filelist),length(filelist)))
    a=splittable(name,tables)
    for(t in tables) {
      z[[t]]=merge(z[[t]],a[[t]],all=TRUE)
    }
  }
  z
}
#
# zTreeSbj takes a vector of .sbj-files and returns a matrix
# Example:
# files <- list.files(pattern = "*.sbj$",recursive=TRUE)
# fname <- sub(".*/","",files)
# sbj <- zTreeSbj(aggregate(files,list(fname),function(x) x[1])$x)
#
zTreeSbj <- function(files) {
  sbj<-NULL
  for (filename in files) {
    cat("reading ",filename,"...\n")
    Tfile<-file(filename,"r")
    aa<-readLines(Tfile)
    close(Tfile)
    aa2<-strsplit(aa,"\t")
    N <- length(aa2[[2]])-1
    aa3<-as.data.frame(list(Date=rep(sub(".sbj$","",sub(".*/","",filename)),N)))
    lapply(aa2,function(x) if (length(x)==N+1) aa3[[x[1]]]<<- x[-1]) 
    if (! is.null(sbj)) sbj<-merge(sbj,aa3,all=TRUE) else sbj <- aa3
  }
  sbj
}
#
toLongDate <- function (shortDate) {
  sapply(as.character(shortDate),function(zz) {
    pre <- ifelse(substr(zz,1,2)<"80","20","19")
    if (nchar(zz)==8) {
#      hour  <- which(LETTERS==substr(zz,7,7))-1
      minute<- 60*which(LETTERS==substr(zz,7,7)) + (which(c(as.character(0:9),LETTERS)==substr(zz,8,8)))*2 - 21
      sprintf("%s%s-%02d:%02d",pre,substr(zz,1,6),minute%/%60,minute%%60)
    }
    else if (nchar(zz)==11) sprintf("%s%s-%s:%s",pre,substr(zz,1,6),substr(zz,8,9),substr(zz,10,11))
    else zz
  })
}
