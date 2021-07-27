## Apresentação
Repositório para trelatório técnicos de estudo de fauna para uso pessoal. O intuito são análises de diversidade. Será dividido da seguinte forma:

- Início;
- Acumulação;
- Riqueza estimada;
- Diveridade;
- PCA.

## Início
Primeiro, vamos indicar as pastas corretas.
- Prestar a atenção no diretório.
```
getwd()
setwd("/home/user/Área de Trabalho/Serviços/ES - Água Doce do Norte/2018_02_12_dj_granitos/2021_07_06_dj_granitos/R") 
```
Agora os principais pacotes utilizados:
```
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
pacman::p_load(magrittr,dplyr,reshape2) #magrittr para operações de pipe/dplyr para manipulador de dados
pacman::p_load(ggplot2, devtools, ggrepel, graphics, lubridate) 
pacman::p_load(vegan)  #vegan para estatística ecológica/graphics para os gráficos
```
Agora vamos adicionar a planilha. Algumas coisas devem ser notadas:
- O caminho do arquivo para a tabela de dados brutos;
```
pacman::p_load(openxlsx) 
caminho.do.arquivo <- "/home/user/Área de Trabalho/Serviços/ES - Água Doce do Norte/2014_agua_doce_do_norte.xlsx"
planilhatotal <- read.xlsx(caminho.do.arquivo, #local do arquivo
                         sheet = 1, # em qual planilha estão os dados
                         colNames = T, # as colunas dos dados possuem nomes?
                         na.strings = "NA") # como estão identificados os dados omissos?
head(planilhatotal)
```
Agora vamos filtrar a tabela. Primeiro tirar os dias não amostrados
```
planilhatotal <- subset(planilhatotal, !is.na(Dia))
planilhatotal <- subset(planilhatotal, !is.na(Mês))
planilhatotal <- subset(planilhatotal, !is.na(Ano))
planilhatotal <- subset(planilhatotal, !is.na(Abundancia)) #tirar n/a da Abundancia
```
Agora escolher o que analisar e atribuir uma tabela chamada p2 parar as análises:
`p2 <- subset(planilhatotal, Empresa == "Itaguaçu Granitos")`

E ainda vamos atribuir as datas:
```
p3 <- p2 %>% 
  select(Ano,Mês,Dia) %>% 
  mutate(Data = make_date(Ano,Mês,Dia))
Data <- data.frame(p3,p2)
```
Vamos ver:
`ts.plot(Data$Data)`

## Acumulação
Agora vamos aos cálculos de diversidade. Vamos primeiro selecionar a tabela.
```
acum<-reshape2::dcast(Data, Espécie ~ Data)
acum=data.frame(acum, row.names=1)
```
E vamos gerar a curva:
```
acumplot<-specaccum(acum) #dados de acumulação
plot(acumplot) #curva simples
plot(acumplot,ci.type="poly",col="black",lwd=2,ci.lty=0,ci.col="lightgrey",ylab="Riqueza",
     xlab="Dias",main="Curva de acumulação de registros",las=1,font=1.5,font.lab=1.5,cex.lab=1,cex.axis=1) #curva clássica
```
Podemos fazer alguns tipos de gráficos de acumulação, veremos a seguir em três etapas, primeiro selecionando a tabela.
```
acum<-reshape2::dcast(Data, Data ~ Espécie, value.var = "Abundancia", fun = length)
acum=data.frame(acum, row.names=1)
```
Segundo os cálculos
```
sp1<-specaccum(acum,method="rarefaction")
sp2<-specaccum(acum,method="exact")
sp3<-specaccum(acum,method="random")
sp4<-specaccum(acum,method="collector")
```
Por fim os gráficos:
```
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Serviços/ES - Baixo Guandu/2021_06_30_granitos_itaguacu/R/1.Acumul.png",width=800,height=600) #local e tmamanho
par(mfrow=c(2,2)) 
plot(sp1, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Amostras",ylab="Rarefação")
plot(sp2, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgrey",xlab="Amostras",ylab="Riqueza Esperada")
plot(sp3, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="yellow",xlab="Amostras",ylab="Sítios Aleatórios")
plot(sp4, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Amostras",ylab="Curva do Coletor")
par(mfrow=c(1,1)) #compilado de curvas
#dev.off()
```
Podemos focar também a curva do coletor e adicionar a abundância por dia. Primeiro vamos aos cálculos de diversidade e abundância.
```
sp4<-specaccum(acum,method="collector")
shannon<-diversity(acum)
spAbund<-rowSums(acum)
```
Agora preparamosa tabela:
```
acum<-reshape2::dcast(Data, Data ~ Espécie, value.var = "Abundancia", fun = length)
a<-data.frame(shannon,spAbund,sp4$sites,sp4$richness,acum)
```
E plotamos:
```
ggplot(a, aes(x = Data, y = sp4.richness)) + 
  geom_line(size=6, alpha=0.6, color="Gray") + #geom_line(aes(group = sp4.sites))
  geom_point(aes(size=spAbund, colour=shannon), alpha=0.3) +
    geom_label_repel(aes(label = sp4$richness), size=4, alpha=0.8, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_size(range = c(.1, 24), name="Abundância de registros") +
  #geom_text(aes(label = a$sp4.richness),col = 'black',size = 5) +
  labs(title="Curva do coletor", subtitle="Total", y="Riqueza",x="Data", caption="",
       color = "Diversidade", size = "Abundância de registros") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + theme_classic() 
#ggsave("2.Acumul_álbum_count.png",width = 14, height = 6, dpi = 300)
```
## Estimativa de riqueza
Vamos também estimar a riqueza. Vamos selecionar a tabela. Podem ter duas variáveis:
- Localidade;
- Trilha
```
p3<-reshape2::dcast(Data, Localidade ~ Espécie)
p3=data.frame(p3, row.names=1)
```
Agora vamos estimar a riqueza considerando a localidade toda:
```
pool<-specpool(p3)
pool
```
Mas como vamos ver as localidades separadas? Vamos fazer duas tabelas. 
- Uma indicando a abundância de espécies por dia, denominada de p3;
- Uma indicando a localidade ou trilha dessas espécies, denominada p4.
```
p3<-reshape2::dcast(Data, Data + Localidade ~ Espécie)
excluir <- c("Data", "Localidade")
p3 <- p3[,!(names(p3)%in% excluir)]
p4<-reshape2::dcast(Data, Data + Localidade ~ Classe)
```
Agora a estimativa de riqueza por localidade.
```
pool1<-specpool(p3, p4$Localidade) 
pool1
```


# Classe
## Avifauna / Herpetofauna / Mastofauna
#### Selecionar dados
p2 <- subset(planilhatotal, Grupo == "Herpetofauna") 
p2 <- subset(p2, Dados == "Primários") 
p2 <- subset(p2, Origem == "Nativo") 

### Acumulação
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

### Acumulação 
##### Cálculos de rarefação
sp1<-specaccum(acum,method="rarefaction")

sp2<-specaccum(acum,method="exact")

sp3<-specaccum(acum,method="random")

sp4<-specaccum(acum,method="collector")

##### Rarefação Gráficos
###### Gráficos juntos
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Serviços/ES - Baixo Guandu/2021_06_30_granitos_itaguacu/R/1.Acumulmast.png",width=800,height=600) #local e tmamanho
par(mfrow=c(2,2)) 
plot(sp1, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Amostras",ylab="Rarefação")
plot(sp2, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgrey",xlab="Amostras",ylab="Riqueza Esperada")
plot(sp3, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="yellow",xlab="Amostras",ylab="Sítios Aleatórios")
plot(sp4, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Amostras",ylab="Curva do Coletor")
par(mfrow=c(1,1)) #compilado de curvas
#dev.off()

###### Gráficos de coletor
sp4<-specaccum(acum,method="collector")
acum<-reshape2::dcast(p2, Data ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
#names(acum)[grep('Animalia', names(altitude))] <- 'Abundance'
shannon<-diversity(acum)
a<-data.frame(sp4$sites,sp4$richness,acum,shannon,spAbund)
names(a)[grep('shannon', names(a))] <- 'Diversidade de mamíferos'

ggplot(a, aes(x = Data, y = sp4.richness)) + 
  geom_line(size=6, alpha=0.6, color="Gray") + #geom_line(aes(group = sp4.sites))
  geom_point(aes(size=spAbund, colour=`Diversidade de mamíferos`), alpha=0.3) +
    #geom_label_repel(aes(label = sp4$richness), size=4, alpha=0.8, #funciona no zoom
                   #box.padding   = 0.35, 
                   #point.padding = 0.75,
                   #segment.color = 'grey50') +
  scale_size(range = c(.1, 24), name="Abundância de registros") +
  #geom_text(aes(label = a$sp4.richness),col = 'black',size = 5) +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(breaks = 5:7) +
  ggtitle("Curva do coletor") +
  xlab("Dias") +
  ylab("Riqueza") + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + theme_classic() 
#ggsave("2.Acumul_álbum_count.png",width = 14, height = 6, dpi = 300)

### Estimativa de riqueza
faixa<-reshape2::dcast(p2, Data ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
faixa=data.frame(faixa,row.names=1)
pool1<-specpool(faixa)
pool1

### Família
local<-reshape2::dcast(p2, Família ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local,row.names=1)
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
###### Riqueza
S <- specnumber(local) 
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J
#Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")

### Gráfico
acum<-reshape2::dcast(p2, Família + Classe ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
acum<-data.frame(acum, H, simp, unbias.simp, alpha, S, J, abund)

ggplot(acum, aes(x = S, y = Família)) + 
  geom_point(aes(size=abund, colour = Família))+ 
  scale_size(range = c(.1, 18), name = "Abundância") +
  geom_label_repel(aes(label = S), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  #geom_boxplot() +
  scale_x_continuous(breaks = 1:12) +
  ggtitle("Índices de Diversidade") +
  xlab("Riqueza") +
  ylab("Família") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("Famíliamast.png",width = 9, height = 7, dpi = 600)

### Registro
##### Tabela
local<-reshape2::dcast(p2, Registro ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
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
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J
#Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")

### Gráfico
local<-reshape2::dcast(p2, Registro ~ Espécie, value.var = "Abundancia", fun.aggregate = sum) local<-data.frame(local, H, simp, S, J, abund)

ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Registro), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Abundância de registros") +
  geom_label_repel(aes(label = Registro), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade por tipo de registro") +
  xlab("Tipo de registro") +
  ylab("Diversidade") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("1.registro_herp.png",width = 15, height = 8, dpi = 600)

# Influência
##### Planilha
p2 <- subset(planilhatotal, Empresa == "Itaguaçu Granitos")
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
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J
#Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")

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
  ggtitle("Diversidade nas áreas de influência") +
  xlab("Riqueza") +
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

### PCA
#### Impacto
#### Pacotes
pacman::p_load(psych) library("devtools") library("ggbiplot") #install_github("vqv/ggbiplot")

### Riqueza
#### Tabelas
local<-reshape2::dcast(p2, Impacto ~ Classe) #sem abudância para ser a riqueza/diversidade
local <- as.matrix(local[ ,-1])
grupo<-reshape2::dcast(p2, Impacto ~ Classe, value.var = "Abundancia", fun.aggregate = sum)
#grupo2<-reshape2::dcast(planilhatotal, País ~ Gênero, value.var = "Pontos", fun.aggregate = sum)

#### Gráficos
wine.pca <- prcomp(local, scale. = TRUE)

ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         #groups = grupo$Fator, 
         ellipse = TRUE, circle = TRUE) +
  geom_label_repel(aes(label = grupo$Impacto), size=4, alpha= 1, 
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  theme_classic()
#ggsave("3.PCA_riqueza.png",width = 6, height = 5, dpi = 600)

summary(prcomp(local, scale = TRUE))
biplot(prcomp(local, scale = TRUE))

### Abundancia
#### Tabelas
local<-reshape2::dcast(p2, Impacto ~ Classe, value.var = "Abundancia", fun.aggregate = sum)
local <- as.matrix(local[ ,-1])
grupo<-reshape2::dcast(p2, Impacto ~ Classe, value.var = "Abundancia", fun.aggregate = sum)
#grupo<-reshape2::dcast(p2, Impacto ~ Ordem, value.var = "Abundancia", fun.aggregate = sum)
#grupo2<-reshape2::dcast(planilhatotal, País ~ Gênero, value.var = "Pontos", fun.aggregate = sum)

#### Gráficos
wine.pca <- prcomp(local, scale. = TRUE)

ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         #groups = grupo$Fator, 
         ellipse = TRUE, circle = TRUE) +
  geom_label_repel(aes(label = grupo$Impacto), size=4, alpha= 1, 
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  theme_classic()
#ggsave("3.PCA_abundancia.png",width = 6, height = 5, dpi = 600)

summary(prcomp(localt, scale = TRUE))
biplot(prcomp(localt, scale = TRUE))

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
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J
#Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")

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
local<-reshape2::dcast(planilhatotal, Vegetação ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram

### PCA
#### Impacto
#### Pacotes
pacman::p_load(psych) library("devtools") library("ggbiplot") #install_github("vqv/ggbiplot")

### Riqueza
#### Tabelas
local<-reshape2::dcast(p2, Vegetação ~ Classe) #sem abudância para ser a riqueza/diversidade
local <- as.matrix(local[ ,-1])
grupo<-reshape2::dcast(p2, Vegetação ~ Classe, value.var = "Abundancia", fun.aggregate = sum)
#grupo2<-reshape2::dcast(planilhatotal, País ~ Gênero, value.var = "Pontos", fun.aggregate = sum)

#### Gráficos
wine.pca <- prcomp(local, scale. = TRUE)

ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         #groups = grupo$Fator, 
         ellipse = TRUE, circle = TRUE) +
  geom_label_repel(aes(label = grupo$Vegetação), size=4, alpha= 1, 
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  theme_classic()
#ggsave("3.PCA_riqueza.png",width = 6, height = 5, dpi = 600)

summary(prcomp(local, scale = TRUE))
biplot(prcomp(local, scale = TRUE))

### Abundancia
#### Tabelas
local<-reshape2::dcast(p2, Vegetação ~ Classe, value.var = "Abundancia", fun.aggregate = sum)
local <- as.matrix(local[ ,-1])
grupo<-reshape2::dcast(p2, Vegetação ~ Classe, value.var = "Abundancia", fun.aggregate = sum)
#grupo<-reshape2::dcast(p2, Impacto ~ Ordem, value.var = "Abundancia", fun.aggregate = sum)
#grupo2<-reshape2::dcast(planilhatotal, País ~ Gênero, value.var = "Pontos", fun.aggregate = sum)

#### Gráficos
wine.pca <- prcomp(local, scale. = TRUE)

ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         #groups = grupo$Fator, 
         ellipse = TRUE, circle = TRUE) +
  geom_label_repel(aes(label = grupo$Vegetação), size=4, alpha= 1, 
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  theme_classic()
#ggsave("3.PCA_abundancia.png",width = 6, height = 5, dpi = 600)

summary(prcomp(localt, scale = TRUE))
biplot(prcomp(localt, scale = TRUE))

# PCA
#### Total delineamento amostral
#### Pacotes
pacman::p_load(psych) library("devtools") library("ggbiplot") #install_github("vqv/ggbiplot")

### Riqueza
#### Tabelas
local<-reshape2::dcast(p2, Impacto ~ Classe) #sem abudância para ser a riqueza/diversidade
names(local)[grep('Impacto', names(local))] <- 'Fator'
impacto <- data.frame(local)
local<-reshape2::dcast(p2, Vegetação ~ Classe) #sem abudância para ser a riqueza/diversidade
names(local)[grep('Vegetação', names(local))] <- 'Fator'
uso <- data.frame(local)

localt<-rbind(impacto, uso)
grupo<-localt
localt <- as.matrix(localt[ ,-1])

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
local<-reshape2::dcast(p2, Impacto ~ Classe, value.var = "Abundancia", fun.aggregate = sum) #sem abudância para ser a riqueza/diversidade
names(local)[grep('Impacto', names(local))] <- 'Fator'
impacto <- data.frame(local)
local<-reshape2::dcast(p2, Vegetação ~ Classe, value.var = "Abundancia", fun.aggregate = sum) #sem abudância para ser a riqueza/diversidade
names(local)[grep('Vegetação', names(local))] <- 'Fator'
uso <- data.frame(local)

localt<-rbind(impacto, uso)
grupo<-localt
localt <- as.matrix(localt[ ,-1])

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



# Correlação
### Pacotes
pacman::p_load(psych)

#correlaçao família/
local<-reshape2::dcast(p2, Família ~ Vegetação, value.var = "Abundancia", fun.aggregate = sum)
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

