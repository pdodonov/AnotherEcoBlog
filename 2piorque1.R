# Abrimos os pacotes necessários pra seleção de modelos:
library(bbmle)
library(mgcv)
library(segmented)

setwd("/home/pavel/Profissional/Extensao/Blogs/AnotherEcoBlog/doispiorqueum")

# Definimos um seed pra rodar as mesmas simulações mais de uma vez

set.seed(978)

###### Simulamos uma série de tamanhos de rio:

rios1 <- sort(runif(15, 20, 140))
rios2 <- sort(runif(15, 110, 230))
regiao <- c(rep("Tear",times=15), rep("Mayene", times=15))
regiao <- as.factor(regiao)

rios <- c(rios1, rios2)

# Situations: 1) no relation with river length; 2) different between regions; 3) linearly increases with river length; 4) piecewise, threshold at 100 km)

slope <- 0.23
slope1 <- 0.32
slope2 <- 1
lambdas <- ifelse(rios<100, rios*slope1, 40)

abund1 <- rpois(30, 30)
abund2 <- c(rpois(15,20), rpois(15,40))
abund3 <- rpois(30, slope*rios)
abund4 <- rpois(30, lambdas)

abund.max <- max(c(abund1, abund2, abund3, abund4))


### Ajustando os modelos certos...
modcor1 <- glm(abund1 ~ 1, family=poisson)
modcor2 <- glm(abund2 ~ regiao, family=poisson)
modcor3 <- glm(abund3 ~ rios, family=poisson)
modcor4 <- glm(abund4 ~ rios, family=poisson)
modcor4 <- segmented(modcor4, seg.Z = ~rios, psi=110)

rios.range <- range(rios)
rios.new <- seq(from=rios.range[1], to=rios.range[2], length.out=1000)
rios.range.tear <- range(rios[regiao=="Tear"])
rios.range.mayene <- range(rios[regiao=="Mayene"])

pred1 <- predict(modcor1, type="response")[1]
pred2 <- predict(modcor2, type="response")[c(1,30)]
pred3 <- predict(modcor3, type="response", newdata=list(rios=rios.new))
pred4 <- predict(modcor4, type="response", newdata=data.frame(rios=rios.new))

png(filename="sereias.png", width=20,height=20,res=300,unit="cm")
par(mfrow=c(2,2), mar=c(3,3,2,2), oma=c(2,2,1,1))
plot(abund1~rios, pch=15+as.numeric(regiao), ylim=c(0,abund.max))
segments(x0=rios.range[1], x1=rios.range[2], y0=pred1, y1=pred1, lwd=2, col="grey30")
plot(abund2~rios, pch=15+as.numeric(regiao), ylim=c(0,abund.max))
segments(x0=rios.range.tear[1], x1=rios.range.tear[2], y0=pred2[1], y1=pred2[1], lwd=2, col="grey30")
segments(x0=rios.range.mayene[1], x1=rios.range.mayene[2], y0=pred2[2], y1=pred2[2], lwd=2, col="grey30")
plot(abund3~rios, pch=15+as.numeric(regiao), ylim=c(0,abund.max))
lines(pred3 ~ rios.new, lwd=2, col="grey30")
plot(abund4~rios, pch=15+as.numeric(regiao), ylim=c(0,abund.max))
lines(pred4 ~ rios.new, lwd=2, col="grey30")
mtext(side=1, outer=T, text="Comprimento do rio (km)")
mtext(side=2, outer=T, text="Abundância de sereias de manguezal")
dev.off()


### Seleção de modelos - completo
# 5 modelos: nulo, região, linear, piecewise, aditivo

mod1.null <- glm(abund1 ~ 1, family=poisson)
mod1.reg <- glm(abund1 ~ regiao, family=poisson)
mod1.lin <- glm(abund1 ~ rios, family=poisson)
mod1.seg <- segmented(mod1.lin, seg.Z = ~rios, psi=110)
mod1.gam <- gam(abund1 ~ s(rios, fx=F, k=-1), family=poisson)

AICctab(mod1.null, mod1.reg, mod1.lin, mod1.seg, mod1.gam)


mod2.null <- glm(abund2 ~ 1, family=poisson)
mod2.reg <- glm(abund2 ~ regiao, family=poisson)
mod2.lin <- glm(abund2 ~ rios, family=poisson)
mod2.seg <- segmented(mod2.lin, seg.Z = ~rios, psi=110)
mod2.gam <- gam(abund2 ~ s(rios, fx=F, k=-1), family=poisson)

AICctab(mod2.null, mod2.reg, mod2.lin, mod2.seg, mod2.gam)
AICctab(mod2.null, mod2.lin, mod2.seg, mod2.gam)



mod3.null <- glm(abund3 ~ 1, family=poisson)
mod3.reg <- glm(abund3 ~ regiao, family=poisson)
mod3.lin <- glm(abund3 ~ rios, family=poisson)
mod3.seg <- segmented(mod3.lin, seg.Z = ~rios, psi=110)
mod3.gam <- gam(abund3 ~ s(rios, fx=F, k=-1), family=poisson)

AICctab(mod3.null, mod3.reg, mod3.lin, mod3.seg, mod3.gam)






mod4.null <- glm(abund4 ~ 1, family=poisson)
mod4.reg <- glm(abund4 ~ regiao, family=poisson)
mod4.lin <- glm(abund4 ~ rios, family=poisson)
mod4.seg <- segmented(mod4.lin, seg.Z = ~rios, psi=110)
mod4.gam <- gam(abund4 ~ s(rios, fx=F, k=-1), family=poisson)

AICctab(mod4.null, mod4.reg, mod4.lin, mod4.seg, mod4.gam)


pred4.1 <- predict(mod4.null, type="response")[1]
pred4.2 <- predict(mod4.reg, type="response")[c(1,30)]
pred4.3 <- predict(mod4.lin, type="response", newdata=list(rios=rios.new))
pred4.4 <- predict(mod4.seg, type="response", newdata=data.frame(rios=rios.new))
pred4.5 <- predict(mod4.gam, type="response", newdata=data.frame(rios=rios.new))


png(filename="sereias2.png", width=20, height=20, unit="cm", res=300)
plot(abund4 ~ rios, xlab="Comprimento do rio (km)", ylab="Número de sereias", pch=15+as.numeric(regiao))
segments(x0=rios.range[1], x1=rios.range[2], y0=pred4.1, y1=pred4.1, lwd=2, col="grey50")
segments(x0=rios.range.tear[1], x1=rios.range.tear[2], y0=pred4.2[1], y1=pred4.2[1], lwd=2, col="grey50", lty=2)
segments(x0=rios.range.mayene[1], x1=rios.range.mayene[2], y0=pred4.2[2], y1=pred4.2[2], lwd=2, col="grey50", lty=2)
lines(pred4.3 ~ rios.new, lwd=2, col="black", lty=1)
lines(pred4.4 ~ rios.new, lwd=2, col="black", lty=2)
lines(pred4.5 ~ rios.new, lwd=2, col="black", lty=3)
dev.off()


pred2.1 <- predict(mod2.null, type="response")[1]
pred2.2 <- predict(mod2.reg, type="response")[c(1,30)]
pred2.3 <- predict(mod2.lin, type="response", newdata=list(rios=rios.new))
pred2.4 <- predict(mod2.seg, type="response", newdata=data.frame(rios=rios.new))
pred2.5 <- predict(mod2.gam, type="response", newdata=data.frame(rios=rios.new))



png(filename="sereias3.png", width=20, height=20, unit="cm", res=300)
plot(abund2 ~ rios, xlab="Comprimento do rio (km)", ylab="Número de sereias", pch=15+as.numeric(regiao))
segments(x0=rios.range[1], x1=rios.range[2], y0=pred2.1, y1=pred2.1, lwd=2, col="grey50")
segments(x0=rios.range.tear[1], x1=rios.range.tear[2], y0=pred2.2[1], y1=pred2.2[1], lwd=2, col="grey50", lty=2)
segments(x0=rios.range.mayene[1], x1=rios.range.mayene[2], y0=pred2.2[2], y1=pred2.2[2], lwd=2, col="grey50", lty=2)
lines(pred2.3 ~ rios.new, lwd=2, col="black", lty=1)
lines(pred2.4 ~ rios.new, lwd=2, col="black", lty=2)
lines(pred2.5 ~ rios.new, lwd=2, col="black", lty=3)
dev.off()


png(filename="sereias4.png", width=20, height=20, unit="cm", res=300)
plot(abund2 ~ rios, xlab="Comprimento do rio (km)", ylab="Número de sereias", pch=15+as.numeric(regiao))
segments(x0=rios.range[1], x1=rios.range[2], y0=pred2.1, y1=pred2.1, lwd=2, col="grey50")
lines(pred2.3 ~ rios.new, lwd=2, col="black", lty=1)
lines(pred2.4 ~ rios.new, lwd=2, col="black", lty=2)
lines(pred2.5 ~ rios.new, lwd=2, col="black", lty=3)
dev.off()



pred3.1 <- predict(mod3.null, type="response")[1]
pred3.2 <- predict(mod3.reg, type="response")[c(1,30)]
pred3.3 <- predict(mod3.lin, type="response", newdata=list(rios=rios.new))
pred3.4 <- predict(mod3.seg, type="response", newdata=data.frame(rios=rios.new))
pred3.5 <- predict(mod3.gam, type="response", newdata=data.frame(rios=rios.new))


png(filename="sereias5.png", width=20, height=20, unit="cm", res=300)
plot(abund3 ~ rios, xlab="Comprimento do rio (km)", ylab="Número de sereias", pch=15+as.numeric(regiao))
lines(pred3.4 ~ rios.new, lwd=2, col="black", lty=2)
lines(pred3.5 ~ rios.new, lwd=2, col="black", lty=3)
dev.off()


### Agora 1000 simulações...

# Regras:
# dAIC < 2
# Escolher o modelo com o menor número de graus de liberdade

modsel1 <- character(1000)
modsel2 <- character(1000)
modsel3 <- character(1000)
modsel4 <- character(1000)

# Usando sem o modelo região

modsel1.b <- character(1000)
modsel2.b <- character(1000)
modsel3.b <- character(1000)
modsel4.b <- character(1000)

set.seed(978)

for (i in 1:1000) {
	rios1 <- sort(runif(15, 20, 140))
	rios2 <- sort(runif(15, 110, 230))
	regiao <- c(rep("Tear",times=15), rep("Mayene", times=15))
	regiao <- as.factor(regiao)
	
	rios <- c(rios1, rios2)
	
	# Situations: 1) no relation with river length; 2) different between regions; 3) linearly increases with river length; 4) piecewise, threshold at 100 km)
	
	slope <- 0.23
	slope1 <- 0.32
	slope2 <- 1
	lambdas <- ifelse(rios<100, rios*slope1, 40)
	
	abund1 <- rpois(30, 30)
	abund2 <- c(rpois(15,20), rpois(15,40))
	abund3 <- rpois(30, slope*rios)
	abund4 <- rpois(30, lambdas)
	
	mod1.null <- glm(abund1 ~ 1, family=poisson)
	mod1.reg <- glm(abund1 ~ regiao, family=poisson)
	mod1.lin <- glm(abund1 ~ rios, family=poisson)
	mod1.seg <- try(segmented(mod1.lin, seg.Z = ~rios, psi=110), silent=T)
	mod1.gam <- gam(abund1 ~ s(rios, fx=F, k=-1), family=poisson)
	if(class(mod1.seg)[1] == "try-error") {
		AICc1 <- AICctab(mod1.null, mod1.reg, mod1.lin, mod1.gam)
		} else {
			AICc1 <- AICctab(mod1.null, mod1.reg, mod1.lin, mod1.seg, mod1.gam)
		}
	AICc1.names <- attr(AICc1, "row.names")[AICc1$dAICc <= 2]
	AICc1.df <- AICc1$df[AICc1$dAICc <= 2]
	foo <- AICc1.names[AICc1.df == min(AICc1.df)]
	if (length(foo) > 1) foo <- paste(foo, collapse="_")
	modsel1[i] <- foo
	if(class(mod1.seg)[1] == "try-error") {
		AICc1 <- AICctab(mod1.null, mod1.lin, mod1.gam)
		} else {
			AICc1 <- AICctab(mod1.null, mod1.lin, mod1.seg, mod1.gam)
		}
	AICc1.names <- attr(AICc1, "row.names")[AICc1$dAICc <= 2]
	AICc1.df <- AICc1$df[AICc1$dAICc <= 2]
	foo <- AICc1.names[AICc1.df == min(AICc1.df)]
	if (length(foo) > 1) foo <- paste(foo, collapse="_")
	modsel1.b[i] <- foo

	mod2.null <- glm(abund2 ~ 1, family=poisson)
	mod2.reg <- glm(abund2 ~ regiao, family=poisson)
	mod2.lin <- glm(abund2 ~ rios, family=poisson)
	mod2.seg <- try(segmented(mod2.lin, seg.Z = ~rios, psi=110), silent=T)
	mod2.gam <- gam(abund2 ~ s(rios, fx=F, k=-1), family=poisson)
	if(class(mod2.seg)[1] == "try-error") {
		AICc2 <- AICctab(mod2.null, mod2.reg, mod2.lin, mod2.gam)
		} else {
			AICc2 <- AICctab(mod2.null, mod2.reg, mod2.lin, mod2.seg, mod2.gam)
		}
	AICc2.names <- attr(AICc2, "row.names")[AICc2$dAICc <= 2]
	AICc2.df <- AICc2$df[AICc2$dAICc <= 2]
	foo <- AICc2.names[AICc2.df == min(AICc2.df)]
	if (length(foo) > 1) foo <- paste(foo, collapse="_")
	modsel2[i] <- foo
	if(class(mod2.seg)[1] == "try-error") {
		AICc2 <- AICctab(mod2.null, mod2.lin, mod2.gam)
		} else {
			AICc2 <- AICctab(mod2.null, mod2.lin, mod2.seg, mod2.gam)
		}
	AICc2.names <- attr(AICc2, "row.names")[AICc2$dAICc <= 2]
	AICc2.df <- AICc2$df[AICc2$dAICc <= 2]
	foo <- AICc2.names[AICc2.df == min(AICc2.df)]
	if (length(foo) > 1) foo <- paste(foo, collapse="_")
	modsel2.b[i] <- foo

	mod3.null <- glm(abund3 ~ 1, family=poisson)
	mod3.reg <- glm(abund3 ~ regiao, family=poisson)
	mod3.lin <- glm(abund3 ~ rios, family=poisson)
	mod3.seg <- try(segmented(mod3.lin, seg.Z = ~rios, psi=110), silent=T)
	mod3.gam <- gam(abund3 ~ s(rios, fx=F, k=-1), family=poisson)
	if(class(mod3.seg)[1] == "try-error") {
		AICc3 <- AICctab(mod3.null, mod3.reg, mod3.lin, mod3.gam)
		} else {
			AICc3 <- AICctab(mod3.null, mod3.reg, mod3.lin, mod3.seg, mod3.gam)
		}
	AICc3.names <- attr(AICc3, "row.names")[AICc3$dAICc <= 2]
	AICc3.df <- AICc3$df[AICc3$dAICc <= 2]
	foo <- AICc3.names[AICc3.df == min(AICc3.df)]
	if (length(foo) > 1) foo <- paste(foo, collapse="_")
	modsel3[i] <- foo
	if(class(mod3.seg)[1] == "try-error") {
		AICc3 <- AICctab(mod3.null, mod3.lin, mod3.gam)
		} else {
			AICc3 <- AICctab(mod3.null, mod3.lin, mod3.seg, mod3.gam)
		}
	AICc3.names <- attr(AICc3, "row.names")[AICc3$dAICc <= 2]
	AICc3.df <- AICc3$df[AICc3$dAICc <= 2]
	foo <- AICc3.names[AICc3.df == min(AICc3.df)]
	if (length(foo) > 1) foo <- paste(foo, collapse="_")
	modsel3.b[i] <- foo



	mod4.null <- glm(abund4 ~ 1, family=poisson)
	mod4.reg <- glm(abund4 ~ regiao, family=poisson)
	mod4.lin <- glm(abund4 ~ rios, family=poisson)
	mod4.seg <- try(segmented(mod4.lin, seg.Z = ~rios, psi=110), silent=T)
	mod4.gam <- gam(abund4 ~ s(rios, fx=F, k=-1), family=poisson)
	if(class(mod4.seg)[1] == "try-error") {
		AICc4 <- AICctab(mod4.null, mod4.reg, mod4.lin, mod4.gam)
		} else {
			AICc4 <- AICctab(mod4.null, mod4.reg, mod4.lin, mod4.seg, mod4.gam)
		}
	AICc4.names <- attr(AICc4, "row.names")[AICc4$dAICc <= 2]
	AICc4.df <- AICc4$df[AICc4$dAICc <= 2]
	foo <- AICc4.names[AICc4.df == min(AICc4.df)]
	if (length(foo) > 1) foo <- paste(foo, collapse="_")
	modsel4[i] <- foo
	if(class(mod4.seg)[1] == "try-error") {
		AICc4 <- AICctab(mod4.null, mod4.lin, mod4.gam)
		} else {
			AICc4 <- AICctab(mod4.null, mod4.lin, mod4.seg, mod4.gam)
		}
	AICc4.names <- attr(AICc4, "row.names")[AICc4$dAICc <= 2]
	AICc4.df <- AICc4$df[AICc4$dAICc <= 2]
	foo <- AICc4.names[AICc4.df == min(AICc4.df)]
	if (length(foo) > 1) foo <- paste(foo, collapse="_")
	modsel4.b[i] <- foo

	print(i)
}



table(modsel1)/1000
table(modsel1.b)/1000

table(modsel2)/1000
table(modsel2.b)/1000

table(modsel3)/1000
table(modsel3.b)/1000