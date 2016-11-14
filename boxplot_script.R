### Get PiratePlot
#install.packages("yarrr")
library(yarrr)
#install.packages("scales")
library(scales)
bootstrap.mean <- function(x, Nboot=5000) {
  means.boot <- numeric(Nboot)
  for (i in 1:Nboot) {
    x.boot <- sample(x, replace=T)
    means.boot[i] <- mean(x.boot)
  }
  result <- quantile(means.boot, c(0.025, 0.975))
  return(result)
}




dmist.2norm <- function(x, mu1, mu2, sig1, sig2, a, log=FALSE){
  z <- a*dnorm(x, mean=mu1, sd=sig1) + 
  (1-a)*dnorm(x, mean=mu2, sd=sig2)
  if(log) log(z)
  else z
}

rmist.2norm <- function(n, mean1, mean2, sd1, sd2, prob1) {
  dat.sim <- numeric(n)
  dat.which <- sample(c(1,2),size=n,replace=T,prob=c(prob1, 1-prob1))
  dat.sim[dat.which==1] <- rnorm(sum(dat.which==1),mean=mean1,sd=sd1)
  dat.sim[dat.which==2] <- rnorm(sum(dat.which==2),mean=mean2,sd=sd2)
  return(dat.sim)
}

Nunicorn = 75

set.seed(42)

dat.unicorn.1 <- rnorm(Nunicorn,25,5)
dat.unicorn.2 <- rmist.2norm(Nunicorn,15,35,5,5,0.5)
dat.unicorn.3 <- rgamma(Nunicorn,shape=255, rate=10)


mean(dat.unicorn.1)
mean(dat.unicorn.2)
mean(dat.unicorn.3)


dat.all <- matrix(c(dat.unicorn.1, dat.unicorn.2, dat.unicorn.3))
dat.all <- as.data.frame(dat.all)
names(dat.all) <- "HornLength"
dat.all$Species <- as.factor(c(rep("Deserto", Nunicorn), rep("Floresta", Nunicorn), rep("Montanha", Nunicorn)))
str(dat.all)


setwd("D:/anotherecoblog")

### Density plots
png(filename="boxplot_PDF.png", res=300, unit="cm", height=20, width=15)
par(mfrow=c(3,1), mar=c(3,3,2,2),oma=c(3,3,1,1))
curve(dnorm(x,25,5), from=0, to=50, main="Deserto",xlab="",ylab="")
curve(dmist.2norm(x, mu1=15, mu2=35, sig1=5, sig2=5, a=0.5), from=0, to=50, main="Floresta",xlab="",ylab="")
curve(dgamma(x,shape=250,rate=10), from=0, to=50, main="Montanha",xlab="",ylab="")
mtext(side=1,text="Comprimento de chifre (cm)", outer=T)
mtext(side=2,text="Probabilidade", outer=T)
dev.off()

### Histogram
png(filename="boxplot_hist.png", res=300, unit="cm", height=20, width=15)
par(mfrow=c(3,1), mar=c(3,3,2,2), oma=c(3,3,1,1))
hist(dat.unicorn.1, breaks=25, main="Deserto", xlim=range(dat.all[,1]),xlab="",ylab="")
hist(dat.unicorn.2, breaks=25, main="Floresta", xlim=range(dat.all[,1]),xlab="",ylab="")
hist(dat.unicorn.3, breaks=25, main="Montanha", xlim=range(dat.all[,1]),xlab="",ylab="")
mtext(side=1,text="Comprimento de chifre (cm)", outer=T)
mtext(side=2,text="Frequência", outer=T)
dev.off()



### Bar plot
dat.mean <- aggregate(HornLength ~ Species, data=dat.all, FUN=mean)
dat.sd <- aggregate(HornLength ~ Species, data=dat.all, FUN=sd)

dat.mean$sd <- dat.sd$HornLength
rm(dat.sd)

dat.CIboot <- aggregate(HornLength ~ Species, data=dat.all, FUN=bootstrap.mean)
dat.CIboot.m <- as.matrix(dat.CIboot$HornLength)


names(dat.mean) <- c("Species", "Mean", "SD")

png(filename="boxplot_bar1.png", res=300, unit="cm", height=15, width=15)
coords <- barplot(dat.mean$Mean, ylim=c(0,30), ylab="Comprimento de chifre (cm)")
axis(side=1, at=coords, labels=c("Deserto", "Floresta", "Montanha"), lwd=0, tick=T)
mtext(side=1, text="População", outer=T, line=-2)
dev.off()


png(filename="boxplot_bar2.png", res=300, unit="cm", height=15, width=20)
par(mfrow=c(1,2), mar=c(3,4,2,0), oma=c(2,2,2,2))
coords <- barplot(dat.mean$Mean, ylim=c(0,30), ylab="Comprimento de chifre (cm)", main="IC paramétrico")
axis(side=1, at=coords, labels=c("Deserto", "Floresta", "Montanha"), lwd=0, tick=T)
arrows(x0=coords, y0=dat.mean$Mean-(dat.mean$SD/sqrt(Nunicorn)), y1=dat.mean$Mean+(dat.mean$SD/sqrt(Nunicorn)), code=3, angle=90, lwd=2)

coords <- barplot(dat.mean$Mean, ylim=c(0,30), main="IC por bootstrap")
axis(side=1, at=coords, labels=c("Deserto", "Floresta", "Montanha"), lwd=0, tick=T)
arrows(x0=coords, y0=dat.CIboot.m[,1], y1=dat.CIboot.m[,2], code=3, angle=90, lwd=2)
mtext(side=1, text="População", outer=T, line=0)

dev.off()

### Box plot
png(filename="boxplot_box.png", res=300, unit="cm", height=15, width=15)
boxplot(HornLength ~ Species, data=dat.all, ylab="Comprimento de chifre (cm)", xlab="")
mtext(side=1, text="População", outer=T, line=-2)
dev.off()




### Jitter plot
jitter.species <- jitter(as.numeric(dat.all$Species))
png(filename="boxplot_jitter.png", res=300, unit="cm", height=15, width=15)
plot(HornLength ~ jitter.species, xaxt="n", data=dat.all, xlim=c(0.5,3.5), xlab="", ylab="Comprimento de chifre (cm)", cex=0.8)
axis(side=1, at=c(1,2,3), labels=c("Deserto", "Floresta", "Montanha"))
mtext(side=1, text="População", outer=T, line=-2)
dev.off()


### Jitter plot e boxplot
png(filename="boxplot_boxJitter.png", res=300, unit="cm", height=15, width=15)
plot(HornLength ~ jitter.species, xaxt="n", data=dat.all, xlim=c(0.5,3.5), xlab="", ylab="Comprimento de chifre (cm)", cex=0.8, col="gray",pch=18)
boxplot(HornLength ~ Species, data=dat.all, ylab="Comprimento de chifre (cm)", xlab="", add=T, col=alpha("white",0))
mtext(side=1, text="População", outer=T, line=-2)
dev.off()

### Jitter plot e média

png(filename="boxplot_jitterMean.png", res=300, unit="cm", height=15, width=15)
plot(HornLength ~ jitter.species, xaxt="n", data=dat.all, xlim=c(0.5,3.5), xlab="", ylab="Comprimento de chifre (cm)", cex=0.8, pch=18, col="gray")
segments(y0=dat.mean$Mean, x0=c(0.7, 1.7, 2.7), x1=c(1.4, 2.4, 3.4), col="black", lwd=2)
arrows(x0=c(1,2,3), y0=dat.CIboot.m[,1], y1=dat.CIboot.m[,2], code=3, angle=90, lwd=2, col="black", length=0.1)
axis(side=1, at=c(1,2,3), labels=c("Deserto", "Floresta", "Montanha"))
mtext(side=1, text="População", outer=T, line=-2)
dev.off()


###Pirate plot
png(filename="boxplot_pirate.png", res=300, unit="cm", height=15, width=15)
pirateplot(HornLength~Species, data=dat.all, pal="gray", jitter.val=0.05, inf.f.o=0, inf.b.o=0, avg.line.lwd=1, bar.f.o=0,bean.b.0=1, bean.f.o=0.8, bean.f.col="gray90", bean.b.col="black", bean.b.lwd=1, point.col="black", gl.lwd=0, point.o=0.6,gl.lty=5, gl.col="white", ylab="Comprimento de chifre (cm)", xlab="População")
dev.off()
