### A test of the tradeoffs between sampling size (number of plots) and sampling efforts (number of individuals per plot), in a landscape ecology context.
### The code would have been much more elegant if I used lists to organize the datasets. I was feelings kinda lazy.
### No rights reserved. Feel free to modify and use it however you wish.
### Author: Pavel Dodonov - pdodonov@gmail.com (State University of Santa Cruz - Ilhéus, BA, Brazil) - 26 jan 2017

test.signif <- function(x, alpha=0.05) { #a function to assess whether something was significant.
  foo <- sum(x < alpha) / length(x)
  return(foo)
}

### Define the relationship - assuming Y is affected by X.
setwd("/home/pavel/Profissional/Extensao/Blogs/AnotherEcoBlog/tradeoffsNvsEffort")


sd.real <- 5
slope.real <- 0.05
Nsim <- 5000
Ntot <- 1000
sample.sizes <- c(10, 30, 60, 100, 500)

samples.10 <- round(seq(1,40,length.out=10))
samples.15 <- round(seq(1,40,length.out=15))
samples.20 <- round(seq(1,40,length.out=20))
samples.40 <- 1:40

errors <- array(dim=c(5,4,Nsim), dimnames=list(samples=c("s10","s30","s60","s100","s500"),fragments=c("n10","n15","n20","n40"),sims=NULL))
signifs <- array(dim=c(5,4,Nsim), dimnames=list(samples=c("s10","s30","s60","s100","s500"),fragments=c("n10","n15","n20","n40"),sims=NULL))

### The simulation proceed by simulating X as forest cover (%) and Y as unicorn height (dimensionless units)

set.seed(42)
for (i in 1:Nsim) {
  cover <- sort(runif(40,0,100)) #simulating habitat amount
  heights <- matrix(ncol=40,nrow=Ntot)
  for(j in 1:40) {
    heights[,j] <- rnorm(Ntot, mean=cover[j]*slope.real, sd=sd.real)
  }
  # Calculating mean values to remove intra-fragment dependency, as would be done in a real setting
  # (This or use mixed effects models. I'm feeling lazy.)
  heights.s10.n10 <- apply(heights[1:10, samples.10],2,mean)
  heights.s10.n15 <- apply(heights[1:10, samples.15],2,mean)
  heights.s10.n20 <- apply(heights[1:10, samples.20],2,mean)
  heights.s10.n40 <- apply(heights[1:10, samples.40],2,mean)

  heights.s30.n10 <- apply(heights[1:30, samples.10],2,mean)
  heights.s30.n15 <- apply(heights[1:30, samples.15],2,mean)
  heights.s30.n20 <- apply(heights[1:30, samples.20],2,mean)
  heights.s30.n40 <- apply(heights[1:30, samples.40],2,mean)

  heights.s60.n10 <- apply(heights[1:60, samples.10],2,mean)
  heights.s60.n15 <- apply(heights[1:60, samples.15],2,mean)
  heights.s60.n20 <- apply(heights[1:60, samples.20],2,mean)
  heights.s60.n40 <- apply(heights[1:60, samples.40],2,mean)

  heights.s100.n10 <- apply(heights[1:100, samples.10],2,mean)
  heights.s100.n15 <- apply(heights[1:100, samples.15],2,mean)
  heights.s100.n20 <- apply(heights[1:100, samples.20],2,mean)
  heights.s100.n40 <- apply(heights[1:100, samples.40],2,mean)

  heights.s500.n10 <- apply(heights[1:500, samples.10],2,mean)
  heights.s500.n15 <- apply(heights[1:500, samples.15],2,mean)
  heights.s500.n20 <- apply(heights[1:500, samples.20],2,mean)
  heights.s500.n40 <- apply(heights[1:500, samples.40],2,mean)

  errors[1,1,i] <- (lm(heights.s10.n10~cover[samples.10])$coefficients[2]-slope.real)^2
  errors[2,1,i] <- (lm(heights.s30.n10~cover[samples.10])$coefficients[2]-slope.real)^2
  errors[3,1,i] <- (lm(heights.s60.n10~cover[samples.10])$coefficients[2]-slope.real)^2
  errors[4,1,i] <- (lm(heights.s100.n10~cover[samples.10])$coefficients[2]-slope.real)^2
  errors[5,1,i] <- (lm(heights.s500.n10~cover[samples.10])$coefficients[2]-slope.real)^2

  errors[1,2,i] <- (lm(heights.s10.n15~cover[samples.15])$coefficients[2]-slope.real)^2
  errors[2,2,i] <- (lm(heights.s30.n15~cover[samples.15])$coefficients[2]-slope.real)^2
  errors[3,2,i] <- (lm(heights.s60.n15~cover[samples.15])$coefficients[2]-slope.real)^2
  errors[4,2,i] <- (lm(heights.s100.n15~cover[samples.15])$coefficients[2]-slope.real)^2
  errors[5,2,i] <- (lm(heights.s500.n15~cover[samples.15])$coefficients[2]-slope.real)^2

  errors[1,3,i] <- (lm(heights.s10.n20~cover[samples.20])$coefficients[2]-slope.real)^2
  errors[2,3,i] <- (lm(heights.s30.n20~cover[samples.20])$coefficients[2]-slope.real)^2
  errors[3,3,i] <- (lm(heights.s60.n20~cover[samples.20])$coefficients[2]-slope.real)^2
  errors[4,3,i] <- (lm(heights.s100.n20~cover[samples.20])$coefficients[2]-slope.real)^2
  errors[5,3,i] <- (lm(heights.s500.n20~cover[samples.20])$coefficients[2]-slope.real)^2

  errors[1,4,i] <- (lm(heights.s10.n40~cover[samples.40])$coefficients[2]-slope.real)^2
  errors[2,4,i] <- (lm(heights.s30.n40~cover[samples.40])$coefficients[2]-slope.real)^2
  errors[3,4,i] <- (lm(heights.s60.n40~cover[samples.40])$coefficients[2]-slope.real)^2
  errors[4,4,i] <- (lm(heights.s100.n40~cover[samples.40])$coefficients[2]-slope.real)^2
  errors[5,4,i] <- (lm(heights.s500.n40~cover[samples.40])$coefficients[2]-slope.real)^2

  signifs[1,1,i] <- summary(lm(heights.s10.n10~cover[samples.10]))$coefficients[2,4]
  signifs[2,1,i] <- summary(lm(heights.s30.n10~cover[samples.10]))$coefficients[2,4]
  signifs[3,1,i] <- summary(lm(heights.s60.n10~cover[samples.10]))$coefficients[2,4]
  signifs[4,1,i] <- summary(lm(heights.s100.n10~cover[samples.10]))$coefficients[2,4]
  signifs[5,1,i] <- summary(lm(heights.s500.n10~cover[samples.10]))$coefficients[2,4]
  signifs[1,2,i] <- summary(lm(heights.s10.n15~cover[samples.15]))$coefficients[2,4]
  signifs[2,2,i] <- summary(lm(heights.s30.n15~cover[samples.15]))$coefficients[2,4]
  signifs[3,2,i] <- summary(lm(heights.s60.n15~cover[samples.15]))$coefficients[2,4]
  signifs[4,2,i] <- summary(lm(heights.s100.n15~cover[samples.15]))$coefficients[2,4]
  signifs[5,2,i] <- summary(lm(heights.s500.n15~cover[samples.15]))$coefficients[2,4]
  signifs[1,3,i] <- summary(lm(heights.s10.n20~cover[samples.20]))$coefficients[2,4]
  signifs[2,3,i] <- summary(lm(heights.s30.n20~cover[samples.20]))$coefficients[2,4]
  signifs[3,3,i] <- summary(lm(heights.s60.n20~cover[samples.20]))$coefficients[2,4]
  signifs[4,3,i] <- summary(lm(heights.s100.n20~cover[samples.20]))$coefficients[2,4]
  signifs[5,3,i] <- summary(lm(heights.s500.n20~cover[samples.20]))$coefficients[2,4]
  signifs[1,4,i] <- summary(lm(heights.s10.n40~cover[samples.40]))$coefficients[2,4]
  signifs[2,4,i] <- summary(lm(heights.s30.n40~cover[samples.40]))$coefficients[2,4]
  signifs[3,4,i] <- summary(lm(heights.s60.n40~cover[samples.40]))$coefficients[2,4]
  signifs[4,4,i] <- summary(lm(heights.s100.n40~cover[samples.40]))$coefficients[2,4]
  signifs[5,4,i] <- summary(lm(heights.s500.n40~cover[samples.40]))$coefficients[2,4]


  print(i)
}


mean.error <- apply(errors, c(1,2), mean)
mean.signif <- apply(signifs,c(1,2),mean)
max.signif <- apply(signifs,c(1,2),max)
n.signif <- apply(signifs, c(1,2), test.signif)




png(filename="TradeoffsNvsEffort.png",height=25,width=20, unit="cm",res=300)
par(mfrow=c(5,4), mar=c(2,2,2,2), oma=c(2,2,2,2))
plot(heights.s10.n10 ~ cover[samples.10], ylab="",xlab="",main="s10.n10")
plot(heights.s10.n15 ~ cover[samples.15], ylab="",xlab="",main="s10.n15")
plot(heights.s10.n20 ~ cover[samples.20], ylab="",xlab="",main="s10.n20")
plot(heights.s10.n40 ~ cover[samples.40], ylab="",xlab="",main="s10.n40")
plot(heights.s30.n10 ~ cover[samples.10], ylab="",xlab="",main="s30.n10")
plot(heights.s30.n15 ~ cover[samples.15], ylab="",xlab="",main="s30.n15")
plot(heights.s30.n20 ~ cover[samples.20], ylab="",xlab="",main="s30.n20")
plot(heights.s30.n40 ~ cover[samples.40], ylab="",xlab="",main="s30.n40")
plot(heights.s60.n10 ~ cover[samples.10], ylab="",xlab="",main="s60.n10")
plot(heights.s60.n15 ~ cover[samples.15], ylab="",xlab="",main="s60.n15")
plot(heights.s60.n20 ~ cover[samples.20], ylab="",xlab="",main="s60.n20")
plot(heights.s60.n40 ~ cover[samples.40], ylab="",xlab="",main="s60.n40")
plot(heights.s100.n10 ~ cover[samples.10], ylab="",xlab="",main="s100.n10")
plot(heights.s100.n15 ~ cover[samples.15], ylab="",xlab="",main="s100.n15")
plot(heights.s100.n20 ~ cover[samples.20], ylab="",xlab="",main="s100.n20")
plot(heights.s100.n40 ~ cover[samples.40], ylab="",xlab="",main="s100.n40")
plot(heights.s500.n10 ~ cover[samples.10], ylab="",xlab="",main="s500.n10")
plot(heights.s500.n15 ~ cover[samples.15], ylab="",xlab="",main="s500.n15")
plot(heights.s500.n20 ~ cover[samples.20], ylab="",xlab="",main="s500.n20")
plot(heights.s500.n40 ~ cover[samples.40], ylab="",xlab="",main="s500.n40")
dev.off()

cover.M <- matrix(cover, ncol=40, nrow=Ntot, byrow=T)
png(filename="TradeoffsNvsEffort_full.png", height=20,width=20,unit="cm",res=300)
plot(as.numeric(heights)~as.numeric(cover.M), xlab="Cobertura florestal (%)", ylab="Altura de ents")
abline(coef=c(0,slope.real), lwd=2, col="red")
dev.off()



### Now for the null model

### Define the relationship.

sd.real <- 5
slope.real.null <- 0
Nsim <- 5000
Ntot <- 1000
sample.sizes <- c(10, 30, 60, 100, 500)

samples.10 <- round(seq(1,40,length.out=10))
samples.15 <- round(seq(1,40,length.out=15))
samples.20 <- round(seq(1,40,length.out=20))
samples.40 <- 1:40

errors.null <- array(dim=c(5,4,Nsim), dimnames=list(samples=c("s10","s30","s60","s100","s500"),fragments=c("n10","n15","n20","n40"),sims=NULL))
signifs.null <- array(dim=c(5,4,Nsim), dimnames=list(samples=c("s10","s30","s60","s100","s500"),fragments=c("n10","n15","n20","n40"),sims=NULL))

set.seed(42)
for (i in 1:Nsim) {
  cover <- sort(runif(40,0,100)) #simulating habitat amount
  heights <- matrix(ncol=40,nrow=Ntot)
  for(j in 1:40) {
    heights[,j] <- rnorm(Ntot, mean=cover[j]*slope.real.null, sd=sd.real)
  }
  
  heights.s10.n10 <- apply(heights[1:10, samples.10],2,mean)
  heights.s10.n15 <- apply(heights[1:10, samples.15],2,mean)
  heights.s10.n20 <- apply(heights[1:10, samples.20],2,mean)
  heights.s10.n40 <- apply(heights[1:10, samples.40],2,mean)

  heights.s30.n10 <- apply(heights[1:30, samples.10],2,mean)
  heights.s30.n15 <- apply(heights[1:30, samples.15],2,mean)
  heights.s30.n20 <- apply(heights[1:30, samples.20],2,mean)
  heights.s30.n40 <- apply(heights[1:30, samples.40],2,mean)

  heights.s60.n10 <- apply(heights[1:60, samples.10],2,mean)
  heights.s60.n15 <- apply(heights[1:60, samples.15],2,mean)
  heights.s60.n20 <- apply(heights[1:60, samples.20],2,mean)
  heights.s60.n40 <- apply(heights[1:60, samples.40],2,mean)

  heights.s100.n10 <- apply(heights[1:100, samples.10],2,mean)
  heights.s100.n15 <- apply(heights[1:100, samples.15],2,mean)
  heights.s100.n20 <- apply(heights[1:100, samples.20],2,mean)
  heights.s100.n40 <- apply(heights[1:100, samples.40],2,mean)

  heights.s500.n10 <- apply(heights[1:500, samples.10],2,mean)
  heights.s500.n15 <- apply(heights[1:500, samples.15],2,mean)
  heights.s500.n20 <- apply(heights[1:500, samples.20],2,mean)
  heights.s500.n40 <- apply(heights[1:500, samples.40],2,mean)

  errors.null[1,1,i] <- (lm(heights.s10.n10~cover[samples.10])$coefficients[2]-slope.real.null)^2
  errors.null[2,1,i] <- (lm(heights.s30.n10~cover[samples.10])$coefficients[2]-slope.real.null)^2
  errors.null[3,1,i] <- (lm(heights.s60.n10~cover[samples.10])$coefficients[2]-slope.real.null)^2
  errors.null[4,1,i] <- (lm(heights.s100.n10~cover[samples.10])$coefficients[2]-slope.real.null)^2
  errors.null[5,1,i] <- (lm(heights.s500.n10~cover[samples.10])$coefficients[2]-slope.real.null)^2

  errors.null[1,2,i] <- (lm(heights.s10.n15~cover[samples.15])$coefficients[2]-slope.real.null)^2
  errors.null[2,2,i] <- (lm(heights.s30.n15~cover[samples.15])$coefficients[2]-slope.real.null)^2
  errors.null[3,2,i] <- (lm(heights.s60.n15~cover[samples.15])$coefficients[2]-slope.real.null)^2
  errors.null[4,2,i] <- (lm(heights.s100.n15~cover[samples.15])$coefficients[2]-slope.real.null)^2
  errors.null[5,2,i] <- (lm(heights.s500.n15~cover[samples.15])$coefficients[2]-slope.real.null)^2

  errors.null[1,3,i] <- (lm(heights.s10.n20~cover[samples.20])$coefficients[2]-slope.real.null)^2
  errors.null[2,3,i] <- (lm(heights.s30.n20~cover[samples.20])$coefficients[2]-slope.real.null)^2
  errors.null[3,3,i] <- (lm(heights.s60.n20~cover[samples.20])$coefficients[2]-slope.real.null)^2
  errors.null[4,3,i] <- (lm(heights.s100.n20~cover[samples.20])$coefficients[2]-slope.real.null)^2
  errors.null[5,3,i] <- (lm(heights.s500.n20~cover[samples.20])$coefficients[2]-slope.real.null)^2

  errors.null[1,4,i] <- (lm(heights.s10.n40~cover[samples.40])$coefficients[2]-slope.real.null)^2
  errors.null[2,4,i] <- (lm(heights.s30.n40~cover[samples.40])$coefficients[2]-slope.real.null)^2
  errors.null[3,4,i] <- (lm(heights.s60.n40~cover[samples.40])$coefficients[2]-slope.real.null)^2
  errors.null[4,4,i] <- (lm(heights.s100.n40~cover[samples.40])$coefficients[2]-slope.real.null)^2
  errors.null[5,4,i] <- (lm(heights.s500.n40~cover[samples.40])$coefficients[2]-slope.real.null)^2

  signifs.null[1,1,i] <- summary(lm(heights.s10.n10~cover[samples.10]))$coefficients[2,4]
  signifs.null[2,1,i] <- summary(lm(heights.s30.n10~cover[samples.10]))$coefficients[2,4]
  signifs.null[3,1,i] <- summary(lm(heights.s60.n10~cover[samples.10]))$coefficients[2,4]
  signifs.null[4,1,i] <- summary(lm(heights.s100.n10~cover[samples.10]))$coefficients[2,4]
  signifs.null[5,1,i] <- summary(lm(heights.s500.n10~cover[samples.10]))$coefficients[2,4]
  signifs.null[1,2,i] <- summary(lm(heights.s10.n15~cover[samples.15]))$coefficients[2,4]
  signifs.null[2,2,i] <- summary(lm(heights.s30.n15~cover[samples.15]))$coefficients[2,4]
  signifs.null[3,2,i] <- summary(lm(heights.s60.n15~cover[samples.15]))$coefficients[2,4]
  signifs.null[4,2,i] <- summary(lm(heights.s100.n15~cover[samples.15]))$coefficients[2,4]
  signifs.null[5,2,i] <- summary(lm(heights.s500.n15~cover[samples.15]))$coefficients[2,4]
  signifs.null[1,3,i] <- summary(lm(heights.s10.n20~cover[samples.20]))$coefficients[2,4]
  signifs.null[2,3,i] <- summary(lm(heights.s30.n20~cover[samples.20]))$coefficients[2,4]
  signifs.null[3,3,i] <- summary(lm(heights.s60.n20~cover[samples.20]))$coefficients[2,4]
  signifs.null[4,3,i] <- summary(lm(heights.s100.n20~cover[samples.20]))$coefficients[2,4]
  signifs.null[5,3,i] <- summary(lm(heights.s500.n20~cover[samples.20]))$coefficients[2,4]
  signifs.null[1,4,i] <- summary(lm(heights.s10.n40~cover[samples.40]))$coefficients[2,4]
  signifs.null[2,4,i] <- summary(lm(heights.s30.n40~cover[samples.40]))$coefficients[2,4]
  signifs.null[3,4,i] <- summary(lm(heights.s60.n40~cover[samples.40]))$coefficients[2,4]
  signifs.null[4,4,i] <- summary(lm(heights.s100.n40~cover[samples.40]))$coefficients[2,4]
  signifs.null[5,4,i] <- summary(lm(heights.s500.n40~cover[samples.40]))$coefficients[2,4]


  print(i)
}


mean.error.null <- apply(errors.null, c(1,2), mean)
mean.signif.null <- apply(signifs.null,c(1,2),mean)
max.signif.null <- apply(signifs.null,c(1,2),max)
n.signif.null <- apply(signifs.null, c(1,2), test.signif)




png(filename="TradeoffsNvsEffort_null.png",height=25,width=20, unit="cm",res=300)
par(mfrow=c(5,4), mar=c(2,2,2,2), oma=c(2,2,2,2))
plot(heights.s10.n10 ~ cover[samples.10], ylab="",xlab="",main="s10.n10")
plot(heights.s10.n15 ~ cover[samples.15], ylab="",xlab="",main="s10.n15")
plot(heights.s10.n20 ~ cover[samples.20], ylab="",xlab="",main="s10.n20")
plot(heights.s10.n40 ~ cover[samples.40], ylab="",xlab="",main="s10.n40")
plot(heights.s30.n10 ~ cover[samples.10], ylab="",xlab="",main="s30.n10")
plot(heights.s30.n15 ~ cover[samples.15], ylab="",xlab="",main="s30.n15")
plot(heights.s30.n20 ~ cover[samples.20], ylab="",xlab="",main="s30.n20")
plot(heights.s30.n40 ~ cover[samples.40], ylab="",xlab="",main="s30.n40")
plot(heights.s60.n10 ~ cover[samples.10], ylab="",xlab="",main="s60.n10")
plot(heights.s60.n15 ~ cover[samples.15], ylab="",xlab="",main="s60.n15")
plot(heights.s60.n20 ~ cover[samples.20], ylab="",xlab="",main="s60.n20")
plot(heights.s60.n40 ~ cover[samples.40], ylab="",xlab="",main="s60.n40")
plot(heights.s100.n10 ~ cover[samples.10], ylab="",xlab="",main="s100.n10")
plot(heights.s100.n15 ~ cover[samples.15], ylab="",xlab="",main="s100.n15")
plot(heights.s100.n20 ~ cover[samples.20], ylab="",xlab="",main="s100.n20")
plot(heights.s100.n40 ~ cover[samples.40], ylab="",xlab="",main="s100.n40")
plot(heights.s500.n10 ~ cover[samples.10], ylab="",xlab="",main="s500.n10")
plot(heights.s500.n15 ~ cover[samples.15], ylab="",xlab="",main="s500.n15")
plot(heights.s500.n20 ~ cover[samples.20], ylab="",xlab="",main="s500.n20")
plot(heights.s500.n40 ~ cover[samples.40], ylab="",xlab="",main="s500.n40")
dev.off()


cover.M <- matrix(cover, ncol=40, nrow=Ntot, byrow=T)
png(filename="TradeoffsNvsEffort_null_full.png", height=20,width=20,unit="cm",res=300)
plot(as.numeric(heights)~as.numeric(cover.M), , xlab="Cobertura florestal (%)", ylab="Altura de ents")
abline(coef=c(0,slope.real.null), lwd=2, col="red")
dev.off()

### And now with an effect and a random intercept!


sd.real <- 5
slope.real <- 0.05
Nsim <- 5000
Ntot <- 1000
sample.sizes <- c(10, 30, 60, 100, 500)
sd.inter <- 1

samples.10 <- round(seq(1,40,length.out=10))
samples.15 <- round(seq(1,40,length.out=15))
samples.20 <- round(seq(1,40,length.out=20))
samples.40 <- 1:40

errors.randIntercept <- array(dim=c(5,4,Nsim), dimnames=list(samples=c("s10","s30","s60","s100","s500"),fragments=c("n10","n15","n20","n40"),sims=NULL))
signifs.randIntercept <- array(dim=c(5,4,Nsim), dimnames=list(samples=c("s10","s30","s60","s100","s500"),fragments=c("n10","n15","n20","n40"),sims=NULL))

### The simulation proceed by simulating X as forest cover (%) and Y as unicorn height (dimensionless units)

set.seed(42)
for (i in 1:Nsim) {
  cover <- sort(runif(40,0,100)) #simulating habitat amount
  heights <- matrix(ncol=40,nrow=Ntot)
  for(j in 1:40) {
    inter <- rnorm(1,0,sd.inter)
    heights[,j] <- rnorm(Ntot, mean=cover[j]*slope.real, sd=sd.real)+inter
  }
  # Calculating mean values to remove intra-fragment dependency, as would be done in a real setting
  # (This or use mixed effects models. I'm feeling lazy.)
  heights.s10.n10 <- apply(heights[1:10, samples.10],2,mean)
  heights.s10.n15 <- apply(heights[1:10, samples.15],2,mean)
  heights.s10.n20 <- apply(heights[1:10, samples.20],2,mean)
  heights.s10.n40 <- apply(heights[1:10, samples.40],2,mean)

  heights.s30.n10 <- apply(heights[1:30, samples.10],2,mean)
  heights.s30.n15 <- apply(heights[1:30, samples.15],2,mean)
  heights.s30.n20 <- apply(heights[1:30, samples.20],2,mean)
  heights.s30.n40 <- apply(heights[1:30, samples.40],2,mean)

  heights.s60.n10 <- apply(heights[1:60, samples.10],2,mean)
  heights.s60.n15 <- apply(heights[1:60, samples.15],2,mean)
  heights.s60.n20 <- apply(heights[1:60, samples.20],2,mean)
  heights.s60.n40 <- apply(heights[1:60, samples.40],2,mean)

  heights.s100.n10 <- apply(heights[1:100, samples.10],2,mean)
  heights.s100.n15 <- apply(heights[1:100, samples.15],2,mean)
  heights.s100.n20 <- apply(heights[1:100, samples.20],2,mean)
  heights.s100.n40 <- apply(heights[1:100, samples.40],2,mean)

  heights.s500.n10 <- apply(heights[1:500, samples.10],2,mean)
  heights.s500.n15 <- apply(heights[1:500, samples.15],2,mean)
  heights.s500.n20 <- apply(heights[1:500, samples.20],2,mean)
  heights.s500.n40 <- apply(heights[1:500, samples.40],2,mean)

  errors.randIntercept[1,1,i] <- (lm(heights.s10.n10~cover[samples.10])$coefficients[2]-slope.real)^2
  errors.randIntercept[2,1,i] <- (lm(heights.s30.n10~cover[samples.10])$coefficients[2]-slope.real)^2
  errors.randIntercept[3,1,i] <- (lm(heights.s60.n10~cover[samples.10])$coefficients[2]-slope.real)^2
  errors.randIntercept[4,1,i] <- (lm(heights.s100.n10~cover[samples.10])$coefficients[2]-slope.real)^2
  errors.randIntercept[5,1,i] <- (lm(heights.s500.n10~cover[samples.10])$coefficients[2]-slope.real)^2

  errors.randIntercept[1,2,i] <- (lm(heights.s10.n15~cover[samples.15])$coefficients[2]-slope.real)^2
  errors.randIntercept[2,2,i] <- (lm(heights.s30.n15~cover[samples.15])$coefficients[2]-slope.real)^2
  errors.randIntercept[3,2,i] <- (lm(heights.s60.n15~cover[samples.15])$coefficients[2]-slope.real)^2
  errors.randIntercept[4,2,i] <- (lm(heights.s100.n15~cover[samples.15])$coefficients[2]-slope.real)^2
  errors.randIntercept[5,2,i] <- (lm(heights.s500.n15~cover[samples.15])$coefficients[2]-slope.real)^2

  errors.randIntercept[1,3,i] <- (lm(heights.s10.n20~cover[samples.20])$coefficients[2]-slope.real)^2
  errors.randIntercept[2,3,i] <- (lm(heights.s30.n20~cover[samples.20])$coefficients[2]-slope.real)^2
  errors.randIntercept[3,3,i] <- (lm(heights.s60.n20~cover[samples.20])$coefficients[2]-slope.real)^2
  errors.randIntercept[4,3,i] <- (lm(heights.s100.n20~cover[samples.20])$coefficients[2]-slope.real)^2
  errors.randIntercept[5,3,i] <- (lm(heights.s500.n20~cover[samples.20])$coefficients[2]-slope.real)^2

  errors.randIntercept[1,4,i] <- (lm(heights.s10.n40~cover[samples.40])$coefficients[2]-slope.real)^2
  errors.randIntercept[2,4,i] <- (lm(heights.s30.n40~cover[samples.40])$coefficients[2]-slope.real)^2
  errors.randIntercept[3,4,i] <- (lm(heights.s60.n40~cover[samples.40])$coefficients[2]-slope.real)^2
  errors.randIntercept[4,4,i] <- (lm(heights.s100.n40~cover[samples.40])$coefficients[2]-slope.real)^2
  errors.randIntercept[5,4,i] <- (lm(heights.s500.n40~cover[samples.40])$coefficients[2]-slope.real)^2

  signifs.randIntercept[1,1,i] <- summary(lm(heights.s10.n10~cover[samples.10]))$coefficients[2,4]
  signifs.randIntercept[2,1,i] <- summary(lm(heights.s30.n10~cover[samples.10]))$coefficients[2,4]
  signifs.randIntercept[3,1,i] <- summary(lm(heights.s60.n10~cover[samples.10]))$coefficients[2,4]
  signifs.randIntercept[4,1,i] <- summary(lm(heights.s100.n10~cover[samples.10]))$coefficients[2,4]
  signifs.randIntercept[5,1,i] <- summary(lm(heights.s500.n10~cover[samples.10]))$coefficients[2,4]
  signifs.randIntercept[1,2,i] <- summary(lm(heights.s10.n15~cover[samples.15]))$coefficients[2,4]
  signifs.randIntercept[2,2,i] <- summary(lm(heights.s30.n15~cover[samples.15]))$coefficients[2,4]
  signifs.randIntercept[3,2,i] <- summary(lm(heights.s60.n15~cover[samples.15]))$coefficients[2,4]
  signifs.randIntercept[4,2,i] <- summary(lm(heights.s100.n15~cover[samples.15]))$coefficients[2,4]
  signifs.randIntercept[5,2,i] <- summary(lm(heights.s500.n15~cover[samples.15]))$coefficients[2,4]
  signifs.randIntercept[1,3,i] <- summary(lm(heights.s10.n20~cover[samples.20]))$coefficients[2,4]
  signifs.randIntercept[2,3,i] <- summary(lm(heights.s30.n20~cover[samples.20]))$coefficients[2,4]
  signifs.randIntercept[3,3,i] <- summary(lm(heights.s60.n20~cover[samples.20]))$coefficients[2,4]
  signifs.randIntercept[4,3,i] <- summary(lm(heights.s100.n20~cover[samples.20]))$coefficients[2,4]
  signifs.randIntercept[5,3,i] <- summary(lm(heights.s500.n20~cover[samples.20]))$coefficients[2,4]
  signifs.randIntercept[1,4,i] <- summary(lm(heights.s10.n40~cover[samples.40]))$coefficients[2,4]
  signifs.randIntercept[2,4,i] <- summary(lm(heights.s30.n40~cover[samples.40]))$coefficients[2,4]
  signifs.randIntercept[3,4,i] <- summary(lm(heights.s60.n40~cover[samples.40]))$coefficients[2,4]
  signifs.randIntercept[4,4,i] <- summary(lm(heights.s100.n40~cover[samples.40]))$coefficients[2,4]
  signifs.randIntercept[5,4,i] <- summary(lm(heights.s500.n40~cover[samples.40]))$coefficients[2,4]


  print(i)
}


mean.error.randIntercept <- apply(errors.randIntercept, c(1,2), mean)
mean.signif.randIntercept <- apply(signifs.randIntercept,c(1,2),mean)
max.signif.randIntercept <- apply(signifs.randIntercept,c(1,2),max)
n.signif.randIntercept <- apply(signifs.randIntercept, c(1,2), test.signif)




png(filename="TradeoffsNvsEffort_randIntercept.png",height=25,width=20, unit="cm",res=300)
par(mfrow=c(5,4), mar=c(2,2,2,2), oma=c(2,2,2,2))
plot(heights.s10.n10 ~ cover[samples.10], ylab="",xlab="",main="s10.n10")
plot(heights.s10.n15 ~ cover[samples.15], ylab="",xlab="",main="s10.n15")
plot(heights.s10.n20 ~ cover[samples.20], ylab="",xlab="",main="s10.n20")
plot(heights.s10.n40 ~ cover[samples.40], ylab="",xlab="",main="s10.n40")
plot(heights.s30.n10 ~ cover[samples.10], ylab="",xlab="",main="s30.n10")
plot(heights.s30.n15 ~ cover[samples.15], ylab="",xlab="",main="s30.n15")
plot(heights.s30.n20 ~ cover[samples.20], ylab="",xlab="",main="s30.n20")
plot(heights.s30.n40 ~ cover[samples.40], ylab="",xlab="",main="s30.n40")
plot(heights.s60.n10 ~ cover[samples.10], ylab="",xlab="",main="s60.n10")
plot(heights.s60.n15 ~ cover[samples.15], ylab="",xlab="",main="s60.n15")
plot(heights.s60.n20 ~ cover[samples.20], ylab="",xlab="",main="s60.n20")
plot(heights.s60.n40 ~ cover[samples.40], ylab="",xlab="",main="s60.n40")
plot(heights.s100.n10 ~ cover[samples.10], ylab="",xlab="",main="s100.n10")
plot(heights.s100.n15 ~ cover[samples.15], ylab="",xlab="",main="s100.n15")
plot(heights.s100.n20 ~ cover[samples.20], ylab="",xlab="",main="s100.n20")
plot(heights.s100.n40 ~ cover[samples.40], ylab="",xlab="",main="s100.n40")
plot(heights.s500.n10 ~ cover[samples.10], ylab="",xlab="",main="s500.n10")
plot(heights.s500.n15 ~ cover[samples.15], ylab="",xlab="",main="s500.n15")
plot(heights.s500.n20 ~ cover[samples.20], ylab="",xlab="",main="s500.n20")
plot(heights.s500.n40 ~ cover[samples.40], ylab="",xlab="",main="s500.n40")
dev.off()


cover.M <- matrix(cover, ncol=40, nrow=Ntot, byrow=T)
png(filename="TradeoffsNvsEffort_randIntercept_full.png", height=20,width=20,unit="cm",res=300)
plot(as.numeric(heights)~as.numeric(cover.M), , xlab="Cobertura florestal (%)", ylab="Altura de ents")
abline(coef=c(0,slope.real), lwd=2, col="red")
dev.off()



### finally - null model with random intercept


sd.real <- 5
slope.real.null <- 0.0
Nsim <- 5000
Ntot <- 1000
sample.sizes <- c(10, 30, 60, 100, 500)

samples.10 <- round(seq(1,40,length.out=10))
samples.15 <- round(seq(1,40,length.out=15))
samples.20 <- round(seq(1,40,length.out=20))
samples.40 <- 1:40

errors.randIntercept.null <- array(dim=c(5,4,Nsim), dimnames=list(samples=c("s10","s30","s60","s100","s500"),fragments=c("n10","n15","n20","n40"),sims=NULL))
signifs.randIntercept.null <- array(dim=c(5,4,Nsim), dimnames=list(samples=c("s10","s30","s60","s100","s500"),fragments=c("n10","n15","n20","n40"),sims=NULL))

### The simulation proceed by simulating X as forest cover (%) and Y as unicorn height (dimensionless units)

set.seed(42)
for (i in 1:Nsim) {
  cover <- sort(runif(40,0,100)) #simulating habitat amount
  heights <- matrix(ncol=40,nrow=Ntot)
  for(j in 1:40) {
    heights[,j] <- rnorm(Ntot, mean=cover[j]*slope.real.null, sd=sd.real)
  }
  # Calculating mean values to remove intra-fragment dependency, as would be done in a real setting
  # (This or use mixed effects models. I'm feeling lazy.)
  heights.s10.n10 <- apply(heights[1:10, samples.10],2,mean)
  heights.s10.n15 <- apply(heights[1:10, samples.15],2,mean)
  heights.s10.n20 <- apply(heights[1:10, samples.20],2,mean)
  heights.s10.n40 <- apply(heights[1:10, samples.40],2,mean)

  heights.s30.n10 <- apply(heights[1:30, samples.10],2,mean)
  heights.s30.n15 <- apply(heights[1:30, samples.15],2,mean)
  heights.s30.n20 <- apply(heights[1:30, samples.20],2,mean)
  heights.s30.n40 <- apply(heights[1:30, samples.40],2,mean)

  heights.s60.n10 <- apply(heights[1:60, samples.10],2,mean)
  heights.s60.n15 <- apply(heights[1:60, samples.15],2,mean)
  heights.s60.n20 <- apply(heights[1:60, samples.20],2,mean)
  heights.s60.n40 <- apply(heights[1:60, samples.40],2,mean)

  heights.s100.n10 <- apply(heights[1:100, samples.10],2,mean)
  heights.s100.n15 <- apply(heights[1:100, samples.15],2,mean)
  heights.s100.n20 <- apply(heights[1:100, samples.20],2,mean)
  heights.s100.n40 <- apply(heights[1:100, samples.40],2,mean)

  heights.s500.n10 <- apply(heights[1:500, samples.10],2,mean)
  heights.s500.n15 <- apply(heights[1:500, samples.15],2,mean)
  heights.s500.n20 <- apply(heights[1:500, samples.20],2,mean)
  heights.s500.n40 <- apply(heights[1:500, samples.40],2,mean)

  errors.randIntercept.null[1,1,i] <- (lm(heights.s10.n10~cover[samples.10])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[2,1,i] <- (lm(heights.s30.n10~cover[samples.10])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[3,1,i] <- (lm(heights.s60.n10~cover[samples.10])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[4,1,i] <- (lm(heights.s100.n10~cover[samples.10])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[5,1,i] <- (lm(heights.s500.n10~cover[samples.10])$coefficients[2]-slope.real.null)^2

  errors.randIntercept.null[1,2,i] <- (lm(heights.s10.n15~cover[samples.15])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[2,2,i] <- (lm(heights.s30.n15~cover[samples.15])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[3,2,i] <- (lm(heights.s60.n15~cover[samples.15])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[4,2,i] <- (lm(heights.s100.n15~cover[samples.15])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[5,2,i] <- (lm(heights.s500.n15~cover[samples.15])$coefficients[2]-slope.real.null)^2

  errors.randIntercept.null[1,3,i] <- (lm(heights.s10.n20~cover[samples.20])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[2,3,i] <- (lm(heights.s30.n20~cover[samples.20])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[3,3,i] <- (lm(heights.s60.n20~cover[samples.20])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[4,3,i] <- (lm(heights.s100.n20~cover[samples.20])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[5,3,i] <- (lm(heights.s500.n20~cover[samples.20])$coefficients[2]-slope.real.null)^2

  errors.randIntercept.null[1,4,i] <- (lm(heights.s10.n40~cover[samples.40])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[2,4,i] <- (lm(heights.s30.n40~cover[samples.40])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[3,4,i] <- (lm(heights.s60.n40~cover[samples.40])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[4,4,i] <- (lm(heights.s100.n40~cover[samples.40])$coefficients[2]-slope.real.null)^2
  errors.randIntercept.null[5,4,i] <- (lm(heights.s500.n40~cover[samples.40])$coefficients[2]-slope.real.null)^2

  signifs.randIntercept.null[1,1,i] <- summary(lm(heights.s10.n10~cover[samples.10]))$coefficients[2,4]
  signifs.randIntercept.null[2,1,i] <- summary(lm(heights.s30.n10~cover[samples.10]))$coefficients[2,4]
  signifs.randIntercept.null[3,1,i] <- summary(lm(heights.s60.n10~cover[samples.10]))$coefficients[2,4]
  signifs.randIntercept.null[4,1,i] <- summary(lm(heights.s100.n10~cover[samples.10]))$coefficients[2,4]
  signifs.randIntercept.null[5,1,i] <- summary(lm(heights.s500.n10~cover[samples.10]))$coefficients[2,4]
  signifs.randIntercept.null[1,2,i] <- summary(lm(heights.s10.n15~cover[samples.15]))$coefficients[2,4]
  signifs.randIntercept.null[2,2,i] <- summary(lm(heights.s30.n15~cover[samples.15]))$coefficients[2,4]
  signifs.randIntercept.null[3,2,i] <- summary(lm(heights.s60.n15~cover[samples.15]))$coefficients[2,4]
  signifs.randIntercept.null[4,2,i] <- summary(lm(heights.s100.n15~cover[samples.15]))$coefficients[2,4]
  signifs.randIntercept.null[5,2,i] <- summary(lm(heights.s500.n15~cover[samples.15]))$coefficients[2,4]
  signifs.randIntercept.null[1,3,i] <- summary(lm(heights.s10.n20~cover[samples.20]))$coefficients[2,4]
  signifs.randIntercept.null[2,3,i] <- summary(lm(heights.s30.n20~cover[samples.20]))$coefficients[2,4]
  signifs.randIntercept.null[3,3,i] <- summary(lm(heights.s60.n20~cover[samples.20]))$coefficients[2,4]
  signifs.randIntercept.null[4,3,i] <- summary(lm(heights.s100.n20~cover[samples.20]))$coefficients[2,4]
  signifs.randIntercept.null[5,3,i] <- summary(lm(heights.s500.n20~cover[samples.20]))$coefficients[2,4]
  signifs.randIntercept.null[1,4,i] <- summary(lm(heights.s10.n40~cover[samples.40]))$coefficients[2,4]
  signifs.randIntercept.null[2,4,i] <- summary(lm(heights.s30.n40~cover[samples.40]))$coefficients[2,4]
  signifs.randIntercept.null[3,4,i] <- summary(lm(heights.s60.n40~cover[samples.40]))$coefficients[2,4]
  signifs.randIntercept.null[4,4,i] <- summary(lm(heights.s100.n40~cover[samples.40]))$coefficients[2,4]
  signifs.randIntercept.null[5,4,i] <- summary(lm(heights.s500.n40~cover[samples.40]))$coefficients[2,4]


  print(i)
}


mean.error.randIntercept.null <- apply(errors.randIntercept.null, c(1,2), mean)
mean.signifrandIntercept.null <- apply(signifs.randIntercept.null,c(1,2),mean)
max.signifrandIntercept.null <- apply(signifs.randIntercept.null,c(1,2),max)
n.signif.randIntercept.null <- apply(signifs.randIntercept.null, c(1,2), test.signif)




png(filename="TradeoffsNvsEffort_randIntercept_null.png",height=25,width=20, unit="cm",res=300)
par(mfrow=c(5,4), mar=c(2,2,2,2), oma=c(2,2,2,2))
plot(heights.s10.n10 ~ cover[samples.10], ylab="",xlab="",main="s10.n10")
plot(heights.s10.n15 ~ cover[samples.15], ylab="",xlab="",main="s10.n15")
plot(heights.s10.n20 ~ cover[samples.20], ylab="",xlab="",main="s10.n20")
plot(heights.s10.n40 ~ cover[samples.40], ylab="",xlab="",main="s10.n40")
plot(heights.s30.n10 ~ cover[samples.10], ylab="",xlab="",main="s30.n10")
plot(heights.s30.n15 ~ cover[samples.15], ylab="",xlab="",main="s30.n15")
plot(heights.s30.n20 ~ cover[samples.20], ylab="",xlab="",main="s30.n20")
plot(heights.s30.n40 ~ cover[samples.40], ylab="",xlab="",main="s30.n40")
plot(heights.s60.n10 ~ cover[samples.10], ylab="",xlab="",main="s60.n10")
plot(heights.s60.n15 ~ cover[samples.15], ylab="",xlab="",main="s60.n15")
plot(heights.s60.n20 ~ cover[samples.20], ylab="",xlab="",main="s60.n20")
plot(heights.s60.n40 ~ cover[samples.40], ylab="",xlab="",main="s60.n40")
plot(heights.s100.n10 ~ cover[samples.10], ylab="",xlab="",main="s100.n10")
plot(heights.s100.n15 ~ cover[samples.15], ylab="",xlab="",main="s100.n15")
plot(heights.s100.n20 ~ cover[samples.20], ylab="",xlab="",main="s100.n20")
plot(heights.s100.n40 ~ cover[samples.40], ylab="",xlab="",main="s100.n40")
plot(heights.s500.n10 ~ cover[samples.10], ylab="",xlab="",main="s500.n10")
plot(heights.s500.n15 ~ cover[samples.15], ylab="",xlab="",main="s500.n15")
plot(heights.s500.n20 ~ cover[samples.20], ylab="",xlab="",main="s500.n20")
plot(heights.s500.n40 ~ cover[samples.40], ylab="",xlab="",main="s500.n40")
dev.off()

cover.M <- matrix(cover, ncol=40, nrow=Ntot, byrow=T)
png(filename="TradeoffsNvsEffort_randIntercept_null_full.png", height=20,width=20,unit="cm",res=300)
plot(as.numeric(heights)~as.numeric(cover.M), , xlab="Cobertura florestal (%)", ylab="Altura de ents")
abline(coef=c(0,slope.real.null), lwd=2, col="red")
dev.off()




sample.size <- 2:500
sds <- numeric(499)
for(i in 1:499) {
	foo <- rnorm(sample.size[i],0,5)
	sds[i] <- sd(foo)
}

png(filename="sds.png",height=15,widht=20, unit="cm",res=300)
plot(sds~sample.size, xlab="Tamanho amostral", ylab="Desvio-padrão")

