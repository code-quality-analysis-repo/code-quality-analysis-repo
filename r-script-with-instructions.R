#Load raw-data.csv to replicate the results when considering "all files"
#Load raw-data-only-modified-files.csv to replicate the results when considering "modified files"
data<-read.table("raw-data.csv",sep=",",header=TRUE)
#data<-read.table("raw-data-only-modified-files.csv",sep=",",header=TRUE)

#subsets by measure
cohesion <- data[which(data["measure"] == 'cohesion'),]
coupling <- data[which(data["measure"] == 'coupling'),]
complexity <- data[which(data["measure"] == 'complexity'),]
readability <- data[which(data["measure"] == 'readability'),]

#subsets by metric
lcom <- cohesion[which(cohesion["metric_name"] == 'LCOM'),]
c3 <- cohesion[which(cohesion["metric_name"] == 'C3'),]
wmc <- complexity[which(complexity["metric_name"] == 'WMC'),]
cbo <- coupling[which(coupling["metric_name"] == 'CBO'),]
rfc <- coupling[which(coupling["metric_name"] == 'RFC'),]
buse <- readability[which(readability["metric_name"] == 'readability-bw'),]
scalabrino <- readability[which(readability["metric_name"] == 'readability-s'),]

#subsets before vs after
lcom_before <- lcom[which(lcom["before_or_after_commit"] == 'before'),]
lcom_after <- lcom[which(lcom["before_or_after_commit"] == 'after'),]
c3_before <- c3[which(c3["before_or_after_commit"] == 'before'),]
c3_after <- c3[which(c3["before_or_after_commit"] == 'after'),]
wmc_before <- wmc[which(wmc["before_or_after_commit"] == 'before'),]
wmc_after <- wmc[which(wmc["before_or_after_commit"] == 'after'),]
cbo_before <- cbo[which(cbo["before_or_after_commit"] == 'before'),]
cbo_after <- cbo[which(cbo["before_or_after_commit"] == 'after'),]
rfc_before <- rfc[which(rfc["before_or_after_commit"] == 'before'),]
rfc_after <- rfc[which(rfc["before_or_after_commit"] == 'after'),]
buse_before <- buse[which(buse["before_or_after_commit"] == 'before'),]
buse_after <- buse[which(buse["before_or_after_commit"] == 'after'),]
scalabrino_before <- scalabrino[which(scalabrino["before_or_after_commit"] == 'before'),]
scalabrino_after <- scalabrino[which(scalabrino["before_or_after_commit"] == 'after'),]

################################################################################################################
#*******Box Plots*******

pdf("before-after-lcom.pdf")
boxplot(lcom_before$value, lcom_after$value, col="gray", boxwex = 0.25, at = 1:2 - 0.0, names = c("LCOM\nbefore","LCOM\nafter"))
points(1,mean(lcom_before$value), col="red", pch=16)
points(2,mean(lcom_after$value), col="red", pch=16)
dev.off() 

pdf("before-after-c3.pdf")
boxplot(c3_before$value, c3_after$value, col="gray", boxwex = 0.25, at = 1:2 - 0.0, names = c("C3\nbefore","C3\nafter"))
points(1,mean(c3_before$value), col="red", pch=16)
points(2,mean(c3_after$value), col="red", pch=16)
dev.off() 

pdf("before-after-wmc.pdf")
boxplot(wmc_before$value, wmc_after$value, col="gray", boxwex = 0.25, at = 1:2 - 0.0, names = c("WMC\nbefore","WMC\nafter"), ylim=c(0,150))
points(1,mean(wmc_before$value), col="red", pch=16)
points(2,mean(wmc_after$value), col="red", pch=16)
dev.off() 

pdf("before-after-cbo.pdf")
boxplot(cbo_before$value, cbo_after$value, col="gray", boxwex = 0.25, at = 1:2 - 0.0, names = c("CBO\nbefore","CBO\nafter"), ylim=c(0,50))
points(1,mean(cbo_before$value), col="red", pch=16)
points(2,mean(cbo_after$value), col="red", pch=16)
dev.off() 

pdf("before-after-rfc.pdf")
boxplot(rfc_before$value, rfc_after$value, col="gray", boxwex = 0.25, at = 1:2 - 0.0, names = c("RFC\nbefore","RFC\nafter"), ylim=c(0,130))
points(1,mean(rfc_before$value), col="red", pch=16)
points(2,mean(rfc_after$value), col="red", pch=16)
dev.off() 


pdf("before-after-readability.pdf")
boxplot(buse_before$value, buse_after$value, scalabrino_before$value, scalabrino_after$value, col="gray", boxwex = 0.25, at = 1:4 - 0.0, names = c("BUSE\nbefore","BUSE\nafter", "SCALABRINO\nbefore","SCALABRINO\nafter"))
points(1,mean(buse_before$value), col="red", pch=16)
points(2,mean(buse_after$value), col="red", pch=16)

points(3,mean(scalabrino_before$value), col="red", pch=16)
points(4,mean(scalabrino_after$value), col="red", pch=16)
dev.off() 


#STATISTICAL TESTS (orddom library is needed)
#Statistical analysis
#Needed to compute the cliffdelta
if (!require(orddom))
{
  install.packages("orddom")
  library(orddom)
}

################Function to run statistical analysis################
#!!!!IMPORTANT!!!! - Use paired=TRUE when working on modified files only, paired=FALSE when considering all files
runStatAnalysis <- function(dist1, dist2){
result<-c()
result[1]<-wilcox.test(dist1,dist2,alternative="two.side",paired=FALSE)$p.value
#result[1]<-wilcox.test(dist1,dist2,alternative="two.side",paired=TRUE)$p.value

t1=as.matrix(dist1)
colnames(t1)<-c("t1")
t2=as.matrix(dist2)
colnames(t2)<-c("t2")
o<-orddom(t1,t2)
result[2]<-o[13,1]
return(result)
}
################END Function to run statistical analysis################
names<-c()
pval<-c()
esize<-c()

names<-c(names,"LCOM")
result<-runStatAnalysis(lcom_before$value,lcom_after$value)
pval<-c(pval,result[1])
esize<-c(esize,result[2])

names<-c(names,"C3")
result<-runStatAnalysis(c3_before$value,c3_after$value)
pval<-c(pval,result[1])
esize<-c(esize,result[2])

names<-c(names,"CBO")
result<-runStatAnalysis(cbo_before$value,cbo_after$value)
pval<-c(pval,result[1])
esize<-c(esize,result[2])

names<-c(names,"RFC")
result<-runStatAnalysis(rfc_before$value,rfc_after$value)
pval<-c(pval,result[1])
esize<-c(esize,result[2])

names<-c(names,"WMC")
result<-runStatAnalysis(wmc_before$value,wmc_after$value)
pval<-c(pval,result[1])
esize<-c(esize,result[2])

names<-c(names,"BUSE")
result<-runStatAnalysis(buse_before$value,buse_after$value)
pval<-c(pval,result[1])
esize<-c(esize,result[2])

names<-c(names,"SCALABRINO")
result<-runStatAnalysis(scalabrino_before$value, scalabrino_after$value)
pval<-c(pval,result[1])
esize<-c(esize,result[2])

adj<-p.adjust(pval,method="holm")

for(i in 1:(length(adj))){
	message(names[i], ": p-value=", adj[i], " | d=", esize[i])
}

