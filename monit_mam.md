# INICIAR
## Música -> distribuição dos dados
#upgride de pacotes 'update.packages(repos='http://cran.rstudio.com/ ', ask=FALSE, checkBuilt=TRUE)'

## Pastas 
getwd()
setwd("/home/user/Área de Trabalho/Serviços/MG - Caldas/2021_06_07 - Red/R") #Mudança de diretório para pasta do projeto

#p_opendir(path.expand("~")) #abrir pasta
#p_opendir(pacman:::p_basepath()) #abir pasta pacotes

### Pacotes e entradas de arquivos
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
#pacman::p_load(readODS, openxlsx) 
#setRepositories(ind=c(1:9))

##### Pacote de leitor de ods (reserva)
#caminho.do.arquivo <- "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_06_grancol_rbananal.ods"
#planilhatotal <- read_ods(caminho.do.arquivo,sheet = 1,col_names = T,na = "")
#p_opendir(path.expand("~")) #abrir pasta
#p_opendir(pacman:::p_basepath()) #abir pasta pacotes

### Pacotes e entradas de arquivos
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
#pacman::p_load(readODS, openxlsx) 
#setRepositories(ind=c(1:9))

##### Pacote de leitor de ods (reserva)
#caminho.do.arquivo <- "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_06_grancol_rbananal.ods"
#Xplanilhatotal <- read_ods(caminho.do.arquivo,sheet = 1,col_names = T,na = "")

## Pacote de análise e leitura de dados, gráficos
pacman::p_load(magrittr,dplyr) #magrittr para operações de pipe/dplyr para manipulador de dados
##### Operador Pipe - usar o valor resultante da expressão do lado esquerdo como primeiro argumento da função do lado direito
##### As medidas de dispersão são estatísticas descritivas, que quantificam de algum modo a variabilidade dos dados, geralmente utilizando como referência uma medida de posição.

## Pacotes gŕaficos
pacman::p_load(ggplot2, devtools, ggrepel, graphics) 
#scripta reservas
#installed.packages('ggplot2')
#install.packages("devtools")
#fazer gráficos e extensões para salvar e inserir imagem
#require(remotes)
#remotes::install_version
#install_version("ggplot2", version = "2.0.0", repos = "http://cran.us.r-project.org")
#Library(ggplot2)

## Pacote ecologia
pacman::p_load(vegan)  #vegan para estatística ecológica/graphics para os gráficos

# Caregar Planilha
pacman::p_load(openxlsx) 
caminho.do.arquivo <- "/home/user/Área de Trabalho/Serviços/MG - Caldas/2021_06_07 - Red/2021_06_07_caldas_red.xlsx"
planilhatotal <- read.xlsx(caminho.do.arquivo, #local do arquivo
                         sheet = 1, # em qual planilha estão os dados
                         colNames = T, # as colunas dos dados possuem nomes?
                         na.strings = "NA") # como estão identificados os dados omissos?

head(planilhatotal)

### Corrigir planilha
##### Retirar os NA 
planilhatotal <- subset(planilhatotal, !is.na(Dia)) #tirar n/a da altitude
planilhatotal <- subset(planilhatotal, !is.na(Abundancia)) #tirar n/a da Abundancia

### Teste de plots
ts.plot(planilhatotal$Abundancia)

boxplot(planilhatotal$Abundancia)

# Diversidade
# Pacote - Criar tabelas para análise (vai ter mensagem, mas funciona)
pacman::p_load(reshape2)

### Selecionar dados
p2 <- subset(planilhatotal, Grupo == "Mastofauna") 
p2 <- subset(p2, Dados == "Primários") 
p2 <- subset(p2, Origem == "Nativo") 

## Acumulação
#### Acumulação de Discos
##### Riqueza
acum<-reshape2::dcast(p2, Dia ~ Espécie) #, value.var = "Abundancia", fun.aggregate = sum)
acum=data.frame(acum, row.names=1)
spAbund<-rowSums(acum) #abunância por espécie
spAbund

##### Curva
rarecurve(acum, col="blue",cex=1,xlab="Tamanho amostral",ylab="Dias",main="Curva de abundância de registros") 

#abundância espécie
acumt=t(acum)
acumplot<-specaccum(acum) #dados de acumulação
plot(acumplot) #curva simples
plot(acumplot,ci.type="poly",col="black",lwd=2,ci.lty=0,ci.col="lightgrey",ylab="Riqueza",
     xlab="Dias",main="Curva de acumulação de registros",las=1,font=1.5,font.lab=1.5,cex.lab=1,cex.axis=1) #curva clássica

### Acumulação Discos
##### Cálculos de rarefação
sp1<-specaccum(acum,method="rarefaction")

sp2<-specaccum(acum,method="exact")

sp3<-specaccum(acum,method="random")

sp4<-specaccum(acum,method="collector")

##### Rarefação Gráficos
###### Gráficos juntos
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Serviços/MG - Caldas/2021_06_07 - Red/1.Acumul.png",width=800,height=600) #local e tmamanho
par(mfrow=c(2,2)) 
plot(sp1, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Amostras",ylab="Rarefação")
plot(sp2, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgrey",xlab="Amostras",ylab="Riqueza Esperada")
plot(sp3, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="yellow",xlab="Amostras",ylab="Sítios Aleatórios")
plot(sp4, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Amostras",ylab="Curva do Coletor")
par(mfrow=c(1,1)) #compilado de curvas
#dev.off()

###### Gráficos de coletor
sp4<-specaccum(acum,method="collector")
acum<-reshape2::dcast(p2, Dia ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
#names(acum)[grep('Animalia', names(altitude))] <- 'Abundance'
shannon<-diversity(acum)
a<-data.frame(sp4$sites,sp4$richness,acum,shannon,spAbund)
names(a)[grep('shannon', names(a))] <- 'Diversidade de mamíferos'

ggplot(a, aes(x = Dia, y = sp4.richness)) + 
  geom_line(size=6, alpha=0.6, color="Gray") + #geom_line(aes(group = sp4.sites))
  geom_point(aes(size=spAbund, colour=`Diversidade de mamíferos`), alpha=0.3) +
    #geom_label_repel(aes(label = sp4$richness), size=4, alpha=0.8, #funciona no zoom
                   #box.padding   = 0.35, 
                   #point.padding = 0.75,
                   #segment.color = 'grey50') +
  scale_size(range = c(.1, 24), name="Abundância de registros") +
  #geom_text(aes(label = a$sp4.richness),col = 'black',size = 5) +
  ggtitle("Curva do coletor") +
  xlab("Dias") +
  ylab("Riqueza") + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + theme_classic() 
#ggsave("2.Acumul_álbum_count.png",width = 14, height = 6, dpi = 300)

### Estimativa de riqueza
faixa<-reshape2::dcast(p2, Dia ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
faixa=data.frame(faixa,row.names=1)
pool1<-specpool(faixa)
pool1


## Influência
##### Tabela
local<-reshape2::dcast(p2, Impacto ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Impacto ~ Espécie, value.var = "Abundancia", fun.aggregate = sum) 
local<-data.frame(local, H, simp, S, J, abund)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Impacto), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Abundância de registros") +
  geom_label_repel(aes(label = Impacto), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade da mastofauna nas áreas de influência") +
  xlab("classificação de impacto") +
  ylab("Diversidade de mamíferos") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("2.Diver_impacto.png",width = 15, height = 8, dpi = 600)

### Cluster
pacman::p_load("ade4")
local<-reshape2::dcast(planilhatotal, Impacto ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram


## Uso e ocupação do solo
##### Tabela
local<-reshape2::dcast(p2, Vegetação ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Vegetação ~ Espécie, value.var = "Abundancia", fun.aggregate = sum) 
local<-data.frame(local, H, simp, S, J, abund)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Vegetação), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Abundância de registros") +
  geom_label_repel(aes(label = Vegetação), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade da mastofauna para o uso e ocupação do solo") +
  xlab("Riqueza") +
  ylab("Diversidade de mamíferos") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("2.Diver_uso.png",width = 15, height = 8, dpi = 600)

### Cluster
pacman::p_load("ade4")
local<-reshape2::dcast(planilhatotal, Impacto ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram


## UC
##### Tabela
local<-reshape2::dcast(p2, Localidade ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Localidade ~ Espécie, value.var = "Abundancia", fun.aggregate = sum) 
local<-data.frame(local, H, simp, S, J, abund)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Localidade), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Abundância de registros") +
  geom_label_repel(aes(label = Localidade), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade da mastofauna para as localidades") +
  xlab("Localidades") +
  ylab("Riqueza") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("2.Diver_localidade.png",width = 15, height = 8, dpi = 600)

### Cluster
pacman::p_load("ade4")
local<-reshape2::dcast(p2, Localidade ~ Espécie, value.var = "Abundancia", fun.aggregate = sum) 
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) 
#method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram


# PCA
## Impacto
### Pacotes
pacman::p_load(psych) library("devtools") library("ggbiplot") #install_github("vqv/ggbiplot")

### Riqueza
#### Tabelas
local<-reshape2::dcast(p2, Impacto ~ Ordem) #sem abudância para ser a riqueza/diversidade
names(local)[grep('Impacto', names(local))] <- 'Fator'
impacto <- data.frame(local)
local<-reshape2::dcast(p2, Vegetação ~ Ordem) #sem abudância para ser a riqueza/diversidade
names(local)[grep('Vegetação', names(local))] <- 'Fator'
uso <- data.frame(local)
local<-reshape2::dcast(p2, Localidade ~ Ordem) #sem abudância para ser a riqueza/diversidade
names(local)[grep('Localidade', names(local))] <- 'Fator'
local <- data.frame(local)
localt<-rbind(impacto, uso, local)
grupo<-localt
#grupo<-reshape2::dcast(p2, Impacto ~ Ordem, value.var = "Abundancia", fun.aggregate = sum)
#grupo2<-reshape2::dcast(planilhatotal, País ~ Gênero, value.var = "Pontos", fun.aggregate = sum)

#### Gráficos
wine.pca <- prcomp(localt, scale. = TRUE)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         #groups = grupo$Fator, 
         ellipse = TRUE, circle = TRUE) +
  geom_label_repel(aes(label = grupo$Fator), size=4, alpha= 1, 
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  theme_classic()
#ggsave("3.PCA_riqueza.png",width = 6, height = 5, dpi = 600)
summary(prcomp(localt, scale = TRUE))
biplot(prcomp(localt, scale = TRUE))

### Abundancia
#### Tabelas
local<-reshape2::dcast(p2, Impacto ~ Ordem, value.var = "Abundancia", fun.aggregate = sum)
names(local)[grep('Impacto', names(local))] <- 'Fator'
impacto <- data.frame(local)
local<-reshape2::dcast(p2, Vegetação ~ Ordem, value.var = "Abundancia", fun.aggregate = sum) 
names(local)[grep('Vegetação', names(local))] <- 'Fator'
uso <- data.frame(local)
local<-reshape2::dcast(p2, Localidade ~ Ordem, value.var = "Abundancia", fun.aggregate = sum) 
names(local)[grep('Localidade', names(local))] <- 'Fator'
local <- data.frame(local)

localt<-rbind(impacto, uso, local)
grupo<-localt
#grupo<-reshape2::dcast(p2, Impacto ~ Ordem, value.var = "Abundancia", fun.aggregate = sum)
#grupo2<-reshape2::dcast(planilhatotal, País ~ Gênero, value.var = "Pontos", fun.aggregate = sum)

#### Gráficos
wine.pca <- prcomp(localt, scale. = TRUE)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         #groups = grupo$Fator, 
         ellipse = TRUE, circle = TRUE) +
  geom_label_repel(aes(label = grupo$Fator), size=4, alpha= 1, 
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  theme_classic()
#ggsave("3.PCA_abundancia.png",width = 6, height = 5, dpi = 600)
summary(prcomp(localt, scale = TRUE))
biplot(prcomp(localt, scale = TRUE))




#correlaçao família/vegetação
pacman::p_load(psych)
local<-reshape2::dcast(planilhatotal, Família ~ Vegetação, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)

ind <- sample(2, nrow(local),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- local[ind==1,]
testing <- local[ind==2,]

pairs.panels(training[,-5],
             gap = 0,
             bg = c("red", "yellow", "blue"),
             pch=21)

##relação entre dois grupos
#Vegetação/Dia
model <- lm(Abundancia ~ factor(Vegetação), data = planilhatotal)
result <- tidy(model) #Afloramento rochoso é o intercept
#plot(result)

pacman::p_load(dabestr, ggbeeswarm)
library(dabestr)

plot_difference <- ggplot() +
  geom_pointrange(aes(x = term, y = estimate,
                      ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error),
                  data = result) +
  ylim(-5, 5) +
  ylab("Value") +
  xlab("Coefficient") +
  coord_flip() +
  theme_bw()

bootstrap <- dabest(planilhatotal, #demora
                    Grupo,
                    Abundancia,
                    idx = c( "Avifauna", "Herpetofauna", "Mastofauna"),
                    paired = FALSE)

bootstrap_diff <- mean_diff(bootstrap)

plot(bootstrap_diff) #Afloramento rochoso é o intercept

#abundancia/grupo -> há outras forma spara ver isso com o ggviolin
ymin <- min(planilhatotal$Abundancia)
ymax <- max(planilhatotal$Abundancia)

plot_points_ga <- ggplot() +
  geom_quasirandom(aes(x = factor(Grupo), y = Abundancia),
                   data = planilhatotal) +
  xlab("Grupo") +
  ylab("Abundância") +
  theme_bw() +
  scale_y_continuous(limits = c(ymin, ymax))

plot_points_ga
#ggsave("gene_plot-point.png",width = 15, height = 8, dpi = 600)

#ride
pacman::p_load(ggridges, forcat)

gg_tx_ridge <- planilhatotal %>%
  ggplot(aes(x = Abundancia, y = Vegetação)) +
  geom_density_ridges(
    color="gray20",
    fill="gray10",
    alpha=0.55,
    size=1) +
  theme_minimal() +
  labs(y = "Uso e ocupação do solo", x = "Abundância", title = "Distribuição dos registros")

gg_tx_ridge
#ggsave("ridge_gen_ano.png",width = 15, height = 8, dpi = 600)

## Dia

#
local<-reshape2::dcast(planilhatotal, Dia ~ Classe, value.var = "Abundancia", fun.aggregate = sum)
alt #Abundancia por dia
#Distribuição dos registros na altitude

#
faixa<-reshape2::dcast(planilhatotal, Dia ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
faixa=data.frame(faixa,row.names=1)
faixa[is.na(faixa)] <- 0
#faixa #Diversidade espécie por faixa
#Abundancia por Faixa
abund<-rowSums(faixa) #abunância por faixa
abund
ts.plot(abund)
#Riqueza
specnumber<-specnumber(faixa)
specnumber #Riqueza por Faixa
ts.plot(specnumber)
barplot(specnumber)
#Gráfico de Acumulação por Faixa
acumplot<-specaccum(faixa) #dados de acumulação
acumplot
plot(acumplot) #curva simples
plot(acumplot,ci.type="poly",col="black",lwd=2,ci.lty=0,ci.col="lightgrey",ylab="Riqueza",
     xlab="Dias",main="Curva de Acumulação de Espécies",las=1,font=1.5,font.lab=1.5,cex.lab=1,cex.axis=1) #curva clássica
#Rarefacao faixas
sp1<-specaccum(faixa,method="rarefaction")
sp2<-specaccum(faixa, method="exact")
sp3<-specaccum(faixa,method="random")
sp4<-specaccum(faixa,method="collector")
#Rarefacao Graficos
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/acum.png",width=800,height=600) #local e tmamanho
par(mfrow=c(2,2)) 
plot(sp1, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Dias",ylab="Rarefação")
plot(sp2, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgrey",xlab="Dias",ylab="Riqueza Esperada")
plot(sp3, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="yellow",xlab="Dias",ylab="Sítios Aleatórios")
plot(sp4, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Dias",ylab="Curva do Coletor")
#par(mfrow=c(1,1)) #compilado de curvas
dev.off()
faixa<-reshape2::dcast(planilhatotal, Dia ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
faixa=data.frame(faixa,row.names=1)
sp4<-specaccum(faixa,method="collector")
#names(acum)[grep('Animalia', names(altitude))] <- 'Abundance'
shannon<-diversity(faixa)
faixa<-reshape2::dcast(planilhatotal, Dia ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
a<-data.frame(sp4$sites,sp4$richness,faixa,shannon, abund, faixa)
names(a)[grep('shannon', names(a))] <- 'Diversidade de Shannon'

ggplot(a, aes(x = Dia, y = sp4.richness)) + 
  geom_point(aes(size=abund, colour=`Diversidade de Shannon`), alpha=0.3) +
  geom_line(size=6, alpha=0.6, color="Gray") + #geom_line(aes(group = sp4.sites))
  #geom_label_repel(aes(label = sp4$richness), size=4, alpha=0.8, #funciona no zoom
  #box.padding   = 0.35, 
  #point.padding = 0.75,
  #segment.color = 'grey50') +
  scale_size(range = c(.1, 24), name="Abundância") +
  #geom_text(aes(label = a$sp4.richness),col = 'black',size = 5) +
  #ggtitle(" ") +
  xlab("Dia") +
  ylab("Riqueza") + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + theme_classic() 

#ggsave("acum.png",width = 14, height = 6, dpi = 300)


#Riqueza Estimada
#total
faixa<-reshape2::dcast(planilhatotal, Dia ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
faixa=data.frame(faixa,row.names=1)
pool1<-specpool(faixa)
pool1
pool1t<-t(pool1)
boxplot(pool1t,xlab="Total")


######Classe#####

#Herpeto
c <- subset(planilhatotal, Grupo == "Herpetofauna")
#b <- subset (planilhatotal, Classe == "Reptilia") 
#pacman::p_load(dplyr, plyr, data.table)
#c<-rbindlist(list(a,b), fill = TRUE)

#Masto
c <- subset(planilhatotal, Classe == "Mammalia")
faixa<-reshape2::dcast(c, Dia ~ Espécie, value.var = "Abundancia")
faixa=data.frame(faixa,row.names=1)
faixa[is.na(faixa)] <- 0

#Aves
c <- subset(planilhatotal, Classe == "Aves")
faixa<-reshape2::dcast(c, Dia ~ Espécie,value.var = "Abundancia")
faixa=data.frame(faixa,row.names=1)
faixa[is.na(faixa)] <- 0

# Diversidade ##
#criar tabelas para análise (vai ter mensagem, mas funciona)
library(reshape2) #ir em packages
pacman::p_load(reshape2) #sudo apt-get install r-cran-reshape2

## Abundancia total Total
abund<-reshape2::dcast(c, Espécie ~ Classe, value.var= "Abundancia", fun.aggregate = sum)
#abund
abundt=t(abund)

##Transepto## 

#Uso e ocupação do solo
local<-reshape2::dcast(c, Espécie ~ Transepto, value.var = "Abundancia", fun.aggregate = sum)

#local #Diveraidade espécie por UC
local[is.na(local)] <- 0
#Riqueza
localt=t(local)
specnumber<-specnumber(localt)
specnumber #Riqueza por UC
barplot(specnumber)
local<-reshape2::dcast(c, Transepto ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)
abund<-rowSums(local) #abunância por faixa
abund

#Cuva de Abundancia de UCs
local<-reshape2::dcast(c, Transepto ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)
rarecurve(local,col="blue",cex=1,xlab="Abundancia",ylab="Riqueza",main="Curva de Abundancia") #Abundancia localidade

#Diversidade  Local 
H <- diversity(local)
H #shannon
#Dominancia (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
#inverso do simpson
invsimp <- diversity(local, "inv")
invsimp #pielou
## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
## Fisher alpha
alpha <- fisher.alpha(local)
alpha
## Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
## Species richness (S) and Pielou's evenness (J):
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
J <- H/log(S)
J

#gráfico
local<-reshape2::dcast(c, Transepto ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local<-data.frame(local, H, simp, unbias.simp, alpha, S, J, abund)

ggplot(local, aes(x = H, y = S)) + 
  geom_point(aes(size=abund, colour = Transepto))+ 
  scale_size(range = c(.1, 18), name = "Transepto") +
  geom_label_repel(aes(label = S), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  #geom_boxplot() +
  ggtitle("Índices de Diversidade") +
  xlab("Diversidade de Shannon") +
  ylab("Riqueza") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()

#ggsave("transpetoaves.png",width = 9, height = 5, dpi = 600)

#cluster
pacman::p_load("ade4")
local<-reshape2::dcast(c, Transepto ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram


###vegetação

#Uso e ocupação do solo
local<-reshape2::dcast(c, Espécie ~ Vegetação, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)
#local #Diveraidade espécie por UC
local[is.na(local)] <- 0
localt=t(local)
#Riqueza
specnumber<-specnumber(localt)
specnumber #Riqueza por UC
barplot(specnumber)
abund<-rowSums(localt) #abunância por faixa
abund

#Cuva de Abundancia de UCs
rarecurve(localt,col="blue",cex=1,xlab="Abundancia",ylab="Riquesa",main="Curva de Abundancia") #Abundancia localidade

#Diversidade  Local 
local<-reshape2::dcast(c, Vegetação ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)
H <- diversity(local)
H #shannon
#Dominancia (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
#inverso do simpson
invsimp <- diversity(local, "inv")
invsimp #pielou
## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
## Fisher alpha
alpha <- fisher.alpha(local)
alpha
## Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
## Species richness (S) and Pielou's evenness (J):
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
J <- H/log(S)
J

#gráfico
local<-reshape2::dcast(c, Vegetação ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local<-data.frame(local, H, simp, unbias.simp, alpha, S, J, abund)

ggplot(local, aes(x = H, y = S)) + 
  geom_point(aes(size=abund, colour = Vegetação), alpha = 0.7)+ 
  scale_size(range = c(.1, 18), name = "Vegetação") +
  geom_label_repel(aes(label = S), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  #geom_boxplot() +
  ggtitle("Índices de Diversidade") +
  xlab("Diversidade de Shannon") +
  ylab("Riqueza") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()

#ggsave("vegaves.png",width = 9, height = 5, dpi = 600)


#cluster
pacman::p_load("ade4")
local<-reshape2::dcast(c, Vegetação ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram


###Registro

#Registro
local<-reshape2::dcast(c, Espécie ~ Registro, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)
#local #Diveraidade espécie por UC
local[is.na(local)] <- 0
localt=t(local)
#Riqueza
specnumber<-specnumber(localt)
specnumber #Riqueza por UC
barplot(specnumber)
abund<-rowSums(localt) #abunância por faixa
abund

#Cuva de Abundancia de UCs
rarecurve(localt,col="blue",cex=1,xlab="Abundancia",ylab="Riquesa",main="Curva de Abundancia") #Abundancia localidade

#Diversidade  Local 
local<-reshape2::dcast(c, Registro ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)
H <- diversity(local)
H #shannon
#Dominancia (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
#inverso do simpson
invsimp <- diversity(local, "inv")
invsimp #pielou
## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
## Fisher alpha
alpha <- fisher.alpha(local)
alpha
## Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
## Species richness (S) and Pielou's evenness (J):
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
J <- H/log(S)
J

#gráfico
local<-reshape2::dcast(c, Registro ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local<-data.frame(local, H, simp, unbias.simp, alpha, S, J, abund)

ggplot(local, aes(x = H, y = S)) + 
  geom_point(aes(size=abund, colour = Registro))+ 
  scale_size(range = c(.1, 18), name = "Registro") +
  geom_label_repel(aes(label = S), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  #geom_boxplot() +
  ggtitle("Índices de Diversidade") +
  xlab("Diversidade de Shannon") +
  ylab("Riqueza") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()

#ggsave("regismamm.png",width = 9, height = 5, dpi = 600)


#cluster
pacman::p_load("ade4")
local<-reshape2::dcast(c, Registro ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram

## Dia

local<-reshape2::dcast(c, Dia ~ Classe, value.var = "Abundancia", fun.aggregate = sum)
alt #Abundancia por dia
#Distribuição dos registros na altitude

#
faixa<-reshape2::dcast(c, Dia ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
faixa=data.frame(faixa,row.names=1)
faixa[is.na(faixa)] <- 0
#faixa #Diversidade espécie por faixa
#Abundancia por Faixa
abund<-rowSums(faixa) #abunância por faixa
abund
ts.plot(abund)
#Riqueza
specnumber<-specnumber(faixa)
specnumber #Riqueza por Faixa
ts.plot(specnumber)
barplot(specnumber)
#Gráfico de Acumulação por Faixa
acumplot<-specaccum(faixa) #dados de acumulação
acumplot
plot(acumplot) #curva simples
plot(acumplot,ci.type="poly",col="black",lwd=2,ci.lty=0,ci.col="lightgrey",ylab="Riqueza",
     xlab="Dias",main="Curva de Acumulação de Espécies",las=1,font=1.5,font.lab=1.5,cex.lab=1,cex.axis=1) #curva clássica
#Rarefacao faixas
sp1<-specaccum(faixa,method="rarefaction")
sp2<-specaccum(faixa, method="exact")
sp3<-specaccum(faixa,method="random")
sp4<-specaccum(faixa,method="collector")
#Rarefacao Graficos
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/acuherpv.png",width=800,height=600) #local e tmamanho
par(mfrow=c(2,2)) 
plot(sp1, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Dias",ylab="Rarefação")
plot(sp2, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgrey",xlab="Dias",ylab="Riqueza Esperada")
plot(sp3, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="yellow",xlab="Dias",ylab="Sítios Aleatórios")
plot(sp4, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Dias",ylab="Curva do Coletor")
par(mfrow=c(1,1)) #compilado de curvas
#dev.off()


## Família

#Acumulação/ano
acum<-reshape2::dcast(c, Família ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
acum=data.frame(acum, row.names=1)
#acum #Diversidade espécie por ano
acum[is.na(acum)] <- 0 #transformar N/A em 0
acumt=t(acum) #transpose dos dados
#Abundãncia por ano
abund<-sort(colSums(acumt),decr=TRUE) #abunância por ano
abund
#Curva de acumulação por ano
rarecurve(acum,col="blue",cex=1,xlab="Abundancia",ylab="Riqueza",main="Curva de Abundancia") #Abundancia espécie

#Group
#Faixas altitudinais
grupo<-reshape2::dcast(c, Família ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
grupo=data.frame(grupo,row.names=1)
#grupo #Diversidade espécie por faixa
#Abundancia por Faixa
abund<-rowSums(grupo) #abunância por faixa
abund

#Diversidade  Local 
H <- diversity(grupo)
H #shannon
#Dominancia (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(grupo, "simpson")
simp #simpson
#inverso do simpson
invsimp <- diversity(grupo, "inv")
invsimp #pielou
## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(grupo, 2) - 1
unbias.simp
## Fisher alpha
alpha <- fisher.alpha(grupo)
alpha
## Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
## Species richness (S) and Pielou's evenness (J):
S <- specnumber(grupo) ## rowSums(BCI > 0) does the same...
S
J <- H/log(S)
J
#gráfico
acum<-reshape2::dcast(c, Família + Classe ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
acum<-data.frame(acum, H, simp, unbias.simp, alpha, S, J, abund)

ggplot(acum, aes(x = S, y = Família)) + 
  geom_point(aes(size=abund, colour = Família))+ 
  scale_size(range = c(.1, 18), name = "Abundância") +
  geom_label_repel(aes(label = S), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  #geom_boxplot() +
  ggtitle("Índices de Diversidade") +
  xlab("Riqueza") +
  ylab("Família") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()

#ggsave("Famíliaave.png",width = 9, height = 7, dpi = 600)

#Riqueza Estimada
#total
faixa<-reshape2::dcast(c, Dia ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
faixa<-data.frame(faixa, row.names=1)
pool1<-specpool(faixa)
pool1
pool1t<-t(pool1)
boxplot(pool1t,xlab="Total")
