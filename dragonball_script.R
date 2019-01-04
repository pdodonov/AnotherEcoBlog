setwd("/home/pavel/Profissional/Extensao/Blogs/AnotherEcoBlog/dragonball")

dados <- read.table("dragonball_dados.txt", header=T, sep="\t", stringsAsFactors=F)
cores <- read.table("dragonball_cores.txt", header=T, sep="\t", stringsAsFactors=F)

str(dados)

png(filename="dragonball_fig01.png", height=20, width=20, res=300, unit="cm")
plot(Poder ~ Saga, data=subset(dados, Personagem=="Goku"), xlab="Tempo (segmento da série)", ylab="Poder de luta", main="Poder de luta de Goku", type="o", cex=1.3)
dev.off()

png(filename="dragonball_fig02.png", height=20, width=20, res=300, unit="cm")
plot(Poder ~ Saga, data=subset(dados, Personagem=="Goku"), xlab="Tempo (segmento da série)", ylab="Poder de luta", main="Poderes de luta", type="o", cex=1.3, ylim=range(dados$Poder, na.rm=T), pch=21, bg="purple")
points(Poder ~ Saga, data=subset(dados, Personagem=="Krilin"), type="o", cex=1.3, pch=22, bg="red")
points(Poder ~ Saga, data=subset(dados, Personagem=="Yamcha"), type="o", cex=1.3, pch=23, bg="orange")
points(Poder ~ Saga, data=subset(dados, Personagem=="Piccolo Daimaku"), type="o", cex=1.3, pch=24, bg="green")
legend(x=1, y=350, legend=c("Goku", "Krilin", "Yamcha", "Piccolo"), pch=c(21,22,23,24), pt.bg=c("purple", "red", "orange", "green"), cex=1.3)
dev.off()


poder.maximo <- aggregate(Poder ~ Saga, data=subset(dados, Personagem != "Goku"), FUN=max, na.rm=T)

png(filename="dragonball_fig03.png", height=20, width=20, res=300, unit="cm")
plot(Poder ~ Saga, data=subset(dados, Personagem=="Goku"), xlab="Tempo (segmento da série)", ylab="Poder de luta", main="Poderes de luta - Goku VS o mais forte", type="o", cex=1.3, ylim=range(dados$Poder, na.rm=T), pch=21, bg="purple")
points(Poder ~ Saga, data=poder.maximo, type="o", cex=1.3, pch=22, bg="red")
legend(x=1, y=350, legend=c("Goku", "Outros"), pch=c(21,22), pt.bg=c("purple", "red"), cex=1.3)
dev.off()

plot(Ordem ~ Saga, pch=21, bg=cores$Cor, cex=sqrt(Poder)/4, data=dados, ylim=c(41,1))


png(filename="dragonball_fig04.png", height=40, width=20, unit="cm", res=300)

par(mar=c(4,10,2,2))

plot(Ordem ~ Saga, type="n", pch=21, bg="pink", cex=Poder/100, data=dados, yaxt="n", ylab="", ylim=c(41,1))
axis(side=2, at=1:41, labels=dados$Personagem[1:41], las=1)

abline(h=1:41,col="gray",lty=2, lwd=1.6)
points(Ordem ~ Saga, type="p", pch=21, bg="pink", cex=Poder/100, data=dados)

dev.off()



png(filename="dragonball_fig05.png", height=40, width=20, unit="cm", res=300)

par(mar=c(4,10,2,6), xpd=T)

plot(Ordem ~ Saga, type="n", pch=21, bg="pink", cex=Poder/100, data=dados, yaxt="n", ylab="", ylim=c(41,1))
axis(side=2, at=1:41, labels=dados$Personagem[1:41], las=1)

segments(x0=0.8, x1=9.2, y0=1:41, y1=1:41, col="gray",lty=2, lwd=1.6)
points(Ordem ~ Saga, type="p", pch=21, bg="pink", cex=Poder/100, data=dados)

legend(x=9.5,y=1, legend=c(10,50,100, 200, 300, 400), pch=21, pt.bg="pink", pt.cex=c(10,50,100, 200, 300, 400)/100, y.intersp=2, title="Poder", cex=1.3)


dev.off()


png(filename="dragonball_figura.png", height=40, width=20, unit="cm", res=300)

par(mar=c(4,10,2,14), xpd=T)

plot(Ordem ~ Saga, type="n", pch=21, bg=cores$Cor, cex=Poder/100, data=dados, yaxt="n", ylab="", ylim=c(41,1))
axis(side=2, at=1:41, labels=dados$Personagem[1:41], las=1)

segments(x0=0.8, x1=9.2, y0=1:41, y1=1:41, col="gray",lty=2, lwd=1.6)
points(Ordem ~ Saga, type="p", pch=21, bg=cores$Cor, cex=Poder/100, data=dados)

legend(x=9.5,y=1, legend=c(10,50,100, 200, 300, 400), pch=21, pt.bg="pink", pt.cex=c(10,50,100, 200, 300, 400)/100, y.intersp=2, title="Poder", cex=1.3)

legend(x=9.5,y=14, legend=c("Amigas/os de \n Goku", "Competidoras/es", "Red Ribbon", "Mundo das \n trevas", "Namekuseijins"), pch=21, pt.bg=c("blue", "purple", "red", "orange", "gray", "green"), y.intersp=2, pt.cex=2.5, title="Grupo", cex=1.3)

dev.off()



png(filename="dragonball_figura2.png", height=40, width=20, unit="cm", res=300)

par(mar=c(4,10,2,14), xpd=T)

plot(Ordem ~ Saga, type="n", pch=21, bg=cores$Cor, cex=sqrt(Poder)/4, data=dados, yaxt="n", ylab="", ylim=c(41,1))
axis(side=2, at=1:41, labels=dados$Personagem[1:41], las=1)

segments(x0=0.8, x1=9.2, y0=1:41, y1=1:41, col="gray",lty=2, lwd=1.6)
points(Ordem ~ Saga, type="p", pch=21, bg=cores$Cor, cex=sqrt(Poder)/4, data=dados)

legend(x=9.5,y=1, legend=c(10,50,100, 200, 300, 400), pch=21, pt.bg="pink", pt.cex=sqrt(c(10,50,100, 200, 300, 400))/4, y.intersp=2, title="Poder", cex=1.3)

legend(x=9.5,y=14, legend=c("Amigas/os de \n Goku", "Competidoras/es", "Red Ribbon", "Mundo das \n trevas", "Namekuseijins"), pch=21, pt.bg=c("blue", "purple", "red", "orange", "gray", "green"), y.intersp=2, pt.cex=2.5, title="Grupo", cex=1.3)

dev.off()







