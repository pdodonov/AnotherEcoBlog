### Códigos usados para escrever o post
### "Blog Zero em gráficos coloridos"
### por Pavel Dodonov

# Carregando os pacotes
library(xlsx) # Para salvar arquivo de Excel
library(wordcloud) # Para fazer uma nuvem de palavras
library(lubridate) # Para trabalhar com datas
library(scales) # Para adicionar transparência
library(circular) # Para estatística circular

# Preparando a primeira nuvem de palavras

setwd("e:/Pavel/Profissional/Extensao/")
dados <- read.table("BlogZeroEmGraficosColoridos_textos_palavras.txt",
	header=FALSE, sep="\t")
dados <- unlist(dados)
palavras <- unlist(strsplit(dados, " "))
palavras <- gsub("'", "", palavras)
palavras <- gsub('"', "", palavras)
palavras <- gsub(",", "", palavras)
palavras <- gsub("\\.", "", palavras)
palavras <- gsub("\\(", "", palavras)
palavras <- gsub("\\)", "", palavras)
palavras <- gsub("!", "", palavras)
palavras <- gsub("\\?", "", palavras)
palavras <- gsub("\\;", "", palavras)
palavras <- gsub("\\:", "", palavras)
palavras <- gsub("‘", "", palavras)
palavras <- gsub("’", "", palavras)
palavras <- gsub("“", "", palavras)
palavras <- gsub("”", "", palavras)
palavras <- gsub("…", "", palavras)
palavras <- noquote(palavras)
palavras.minusc <- tolower(palavras)
tab.freq <- as.data.frame.table(table(palavras.minusc))
tab.freq <- tab.freq[order(tab.freq$Freq, decreasing=TRUE),]
head(tab.freq, 15)
nrow(tab.freq)

write.xlsx(tab.freq[order(tab.freq$palavras.minusc),], 
	"BlogZeroEmGraficosColoridos_palavras.xlsx", row.names=FALSE)

# Primeira nuvem de palavras

cores <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#b2abd2", 
	"#8073ac", "#542788", "#2d004b")

dados.freq <- read.table("BlogZeroEmGraficosColoridos_textos_freq.txt", 
	header=TRUE, sep="\t")



png(filename="BlogZeroEmGraficosColoridos.png", height=25, width=25,
	unit="cm", res=600)
wordcloud(words=dados.freq$Palavra, freq=dados.freq$Freq,
	min.freq=2, colors=cores[8:6], scale=c(4, .01))
dev.off()


# Segunda nuvem de palavras
dados.tit <- read.table("clipboard", header=TRUE, sep="\t")
titulos <- dados.tit[,2]
tit.palavras <- unlist(strsplit(titulos, " "))
tit.palavras <- gsub("'", "", tit.palavras)
tit.palavras <- gsub('"', "", tit.palavras)
tit.palavras <- gsub(",", "", tit.palavras)
tit.palavras <- gsub("\\.", "", tit.palavras)
tit.palavras <- gsub("\\(", "", tit.palavras)
tit.palavras <- gsub("\\)", "", tit.palavras)
tit.palavras <- gsub("!", "", tit.palavras)
tit.palavras <- gsub("\\?", "", tit.palavras)
tit.palavras <- gsub("\\;", "", tit.palavras)
tit.palavras <- gsub("\\:", "", tit.palavras)
tit.palavras <- gsub("‘", "", tit.palavras)
tit.palavras <- gsub("’", "", tit.palavras)
tit.palavras <- gsub("“", "", tit.palavras)
tit.palavras <- gsub("”", "", tit.palavras)
tit.palavras <- gsub("…", "", tit.palavras)
tit.palavras <- noquote(tit.palavras)
tit.palavras.minusc <- tolower(tit.palavras)
tit.tab.freq <- as.data.frame.table(table(tit.palavras.minusc))

write.xlsx(tit.tab.freq, 
	"BlogZeroEmGraficosColoridos_palavras_titulos.xlsx", row.names=FALSE)

cores <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#b2abd2", 
	"#8073ac", "#542788", "#2d004b")

dados.freq.tit <- read.table("clipboard", 
	header=TRUE, sep="\t")

png(filename="BlogZeroEmGraficosColoridos_fig03.png", height=25, width=25,
	unit="cm", res=600)
wordcloud(words=dados.freq.tit$Palavra, freq=dados.freq$Freq,
	min.freq=1, colors=cores[8:6], scale=c(4, .01))

dev.off()

# Gráfico de barras - por ano

dados.tit$Data2 <- as_date(dados.tit$Data)
dados.tit$Ano <- year(dados.tit$Data2)

postsPorAno <- table(dados.tit$Ano)

png(filename="BlogZeroEmGraficosColoridos_fig04.png",
	height=20, width=20, unit="cm", res=300)
par(mar=c(4,4,2,2))
barplot(postsPorAno, col=alpha("#2d004b", 0.5), 
	xlab="Ano", ylab="Número de postagens", ylim=c(0,50))
dev.off()

# Análises circulares 

dados.tit$Dia <- yday(dados.tit$Data2)
dados.tit$Angulo <- with(dados.tit, (Dia/ifelse(leap_year(Data2),366,365))*360)
dados.tit$Radian <- rad(dados.tit$Angulo)

radianos.posts <- circular(dados.tit$Radian, type="angles", units="radians",
	template="geographics")

meses <- yday(as.Date(c("2023-01-01","2023-02-01", "2023-03-01", "2023-04-01", 
	"2023-05-01", "2023-06-01", "2023-07-01", "2023-08-01", "2023-09-01", 
	"2023-10-01", "2023-11-01",	"2023-12-01")))

cores <- c("#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
cores2 <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33")

meses.grau <- circular((meses/366)*360, template="geographics",
	type="angles", units="degrees")
meses.nome <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set",
	"Out","Nov","Dez")

png(filename="BlogZeroEmGraficosColoridos_fig05.png", height=20, width=20, 
	unit="cm", res=300)
plot.circular(radianos.posts, stack=FALSE, pch=21, cex=1.5, 
	bg=alpha(cores2[dados.tit$Ano-min(dados.tit$Ano)+1],0.5), 
	template="geographics", axes=FALSE,
	main="", shrink=1)
axis.circular(at=meses.grau, labels=meses.nome)
legend(x=0,y=0, fill=cores2, legend=2019:2024, bty="n")
dev.off()

graus.posts <- circular(dados.tit$Angulo, type="angles", units="degrees",
	template="geographics")
mean.circular(graus.posts)
rho.circular(graus.posts)
rayleigh.test(graus.posts)


png(filename="BlogZeroEmGraficosColoridos_fig06.png", height=20, width=20, 
	unit="cm", res=300)
plot.circular(radianos.posts, stack=FALSE, pch=21, cex=1.5, 
	bg=alpha(cores2[dados.tit$Ano-min(dados.tit$Ano)+1],0.5), 
	template="geographics", axes=FALSE,
	main="", shrink=1)
axis.circular(at=meses.grau, labels=meses.nome)
arrows.circular(mean.circular(radianos.posts), length=0.1, lwd=2,
	shrink=rho.circular(radianos.posts))
legend(x=-0.5,y=0, fill=cores2, legend=2019:2024, bty="n")
dev.off()
