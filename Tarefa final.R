library(readr)
proteinas_alimentacao <- read_table2("POS GRADUAÇÃO, CIENCIA DE DADOS/Análise Multivariada de Dados/Proteinas_alimentacao.txt")
proteinas_alimentacao<- data.frame(proteinas_alimentacao, row.names = 1)

## Box plot
boxplot(proteinas_alimentacao)

##Escalonando as variaveis
proteinas_escal <- scale(proteinas_alimentacao)

## matriz de distancia

dist(proteinas_escal)

## Executando o dendograma
## Diferenca entre os tipos de teste

#average ""(m edida de similaridade entre dois clusters, definida pela média das distâncias de todos os pontos do 1 º
#cluster em relação aos pontos do 2 º cluster)
pais.hc.average <- hclust(dist(proteinas_escal), method="average")

#complete :calcula se todas as distâncias entre pontos de clusters distintos e fica se com a MAIOR destas distâncias
pais.hc.complete <- hclust(dist(proteinas_escal), method="complete")           

#single"(mesmo raciocínio do " retendo a MENOR das distâncias)
pais.hc.single <- hclust(dist(proteinas_escal), method="single")

par(mfrow=c(1,3))
abline(h=6, lty=2, lwd=2, col = "#E31A1C")
plot(pais.hc.complete, main="Complete Linkage", xlab="", sub="",
     cex=.9, hang = -1)
plot(pais.hc.average, main="Average Linkage", xlab="", sub="",
     cex=.9, hang = -1)
plot(pais.hc.single, main="Single Linkage", xlab="", sub="",
     cex=.9, hang = -1)


# Tracando uma linha com as divisioes das linha
## Ecolhe o metodo complete para manter um padrao
par(mfrow=c(1,3))
plot(pais.hc.complete, main="HC - Corte em 2 Clusters", xlab="", sub="",
     cex=.9, hang=-1)
abline(h=6, lty=2, lwd=2, col = "#E31A1C")
plot(pais.hc.complete, main="HC - Corte em 3 Clusters", xlab="", sub="",
     cex=.9, hang=-1)
abline(h=5.5, lty=2, lwd=2, col = "#E31A1C")
plot(pais.hc.complete, main="HC - Corte em 4 Clusters", xlab="", sub="",
     cex=.9, hang=-1)
abline(h=4, lty=2, lwd=2, col = "#E31A1C")

## Foi escolhido como melhor divisao o numero de quatro clusters,
## Os grupos ficarao assim:
# cluster 1 = AleOc,Tchec,Polon,Hungr,USSR (5)
# cluster 2 = Finla,Norue,Dinam	,Sueci,Suica,AleOr,Austr,PaisB,Franca,ReinU,Irlan,Belgi(12)
# cluster 3 = Portugal,  Espanha(2)
# cluster 4 = Greci,Itali,Alban,Bula,Romen,Iugos(6)