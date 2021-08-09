## Apresentação
Repositório para trelatório técnicos de estudo de fauna para uso pessoal. O intuito são análises de diversidade. Será dividido da seguinte forma:

- 1. Início;
- 2. Acumulação;
- 3. Riqueza estimada;
- 4. Diveridade;
- 5. Cluester;
- 6. PCA;
- 7. Correlação.

## 1. Início
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
Agora vamos filtrar a tabela. Primeiro tirar os dias não amostrados e espécie exótica.
```
planilhatotal <- subset(planilhatotal, !is.na(Dia))
planilhatotal <- subset(planilhatotal, !is.na(Mês))
planilhatotal <- subset(planilhatotal, !is.na(Ano))
planilhatotal <- subset(planilhatotal, !is.na(Abundancia)) 
```
Agora escolher o que analisar e atribuir uma tabela chamada p2 parar as análises:
```
p2 <- planilhatotal
p2 <- subset(p2, Empresa == "DJ Granitos")
```
E ainda vamos atribuir as datas:
```
p3 <- p2 %>% 
  select(Ano,Mês,Dia) %>% 
  mutate(Data = make_date(Ano,Mês,Dia))
Data <- data.frame(p3,p2)
```
Vamos ver:
`ts.plot(Data$Data)`

## 2. Acumulação
Vamos cacular os principais índices de dievrsidade aqui. Primeiro vamos selecionar oa dados que podem ser:
- Gerais;
- Avifauna;
- Herpetofauna;
- Mastofauna.
Além de filtrar para apenas dados primários e espećies nativas.
```
p2 <- Data
p2 <- subset(p2, Empresa == "DJ Granitos")
p2 <- subset(p2, Grupo == "Herpetofauna") 
p2 <- subset(p2, Dados == "Primários") 
p2 <- subset(p2, Origem == "Nativo") 
```
Agora a tabela para trabalhar.
```
acum<-reshape2::dcast(p2, Data ~ Espécie)
acum=data.frame(acum, row.names=1)
```
E vamos gerar a curva:
```
acumplot<-specaccum(acum) #dados de acumulação
plot(acumplot) #curva simples
plot(acumplot,ci.type="poly",col="black",lwd=2,ci.lty=0,ci.col="lightgrey",ylab="Riqueza",
     xlab="Dias de amostragem",main="Curva de acumulação de registros",las=1,font=1.5,font.lab=1.5,cex.lab=1,cex.axis=1) #curva clássica
```
Podemos fazer alguns tipos de gráficos de acumulação, veremos a seguir em três etapas, primeiro selecionando a tabela.
```
acum<-reshape2::dcast(p2, Data ~ Espécie, value.var = "Abundancia", fun = length)
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
plot(sp1, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Dias de amostragem",ylab="Rarefação")
plot(sp2, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgrey",xlab="Dias de amostragem",ylab="Riqueza Esperada")
plot(sp3, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="yellow",xlab="Dias de amostragem",ylab="Sítios Aleatórios")
plot(sp4, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Dias de amostragem",ylab="Curva do Coletor")
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
p3<-data.frame(shannon,spAbund,sp4$sites,sp4$richness,acum)
```
E plotamos:
```
ggplot(p3, aes(x = Data, y = sp4.richness)) + 
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
## 3. Estimativa de riqueza
Vamos também estimar a riqueza. Vamos selecionar a tabela. Podem ter duas variáveis:
- Trilha;
- Localidade;
- Empresa;
- Grupo;
```
p3<-reshape2::dcast(Data, Campanha ~ Espécie, value.var = "Abundancia", fun = length)
p3=data.frame(p3, row.names=1)
```
Agora vamos estimar a riqueza considerando a localidade toda:
```
pool<-specpool(p3)
pool
```
E um gráfico:
```
pool <- poolaccum(p3)
#summary(pool, display = "chao")
plot(pool)
```
Também podemos separar pelas varíaveis seguintes:
- Localidade;
- Trilha;
- Classe.

Precisamos fazer duas tabelas. 
- Uma indicando a abundância de espécies por dia, denominada de p3;
- Uma indicando a variável, denominada p4.
- Lembrar de conferir a variável
```
#p3 <- subset(Data, Empresa == "XXX")
p3<-reshape2::dcast(Data, Data + Grupo ~ Espécie, value.var = "Abundancia", fun = length)
excluir <- c("Data", "Grupo")
p3 <- p3[,!(names(p3)%in% excluir)]
p4<-reshape2::dcast(Data, Data + Grupo ~ Classe, value.var = "Abundancia", fun = length)
```
Agora a estimativa de riqueza por localidade.
```
pool<-specpool(p3, p4$Grupo) 
pool
boxplot(pool$chao) 
```

## 4. Diversidade
Aqui também podemos filtrar a tabela.
- Gerais;
- Avifauna;
- Herpetofauna;
- Mastofauna.
Além de filtrar para apenas dados primários e espećies nativas.
```
p2 <- Data
p2 <- subset(p2, Empresa == "DJ Granitos")
p2 <- subset(p2, Grupo == "Herpetofauna") 
p2 <- subset(p2, Dados == "Primários") 
p2 <- subset(p2, Origem == "Nativo") 
```
Agora vamos filtrar a tabela, ela pode ser por:
- Classe;
- Família;
- Influência;
- Empresa;
- Localidade.
```
local<-reshape2::dcast(p2, Registro ~ Espécie, value.var = "Abundancia", fun = length)
local=data.frame(local,row.names=1)
```
E vamos aos cálculos;
```
S <- specnumber(local) 
spAbund<-rowSums(local) #abunância por faixa
shannon <- diversity(local)
J <- shannon/log(S) #Pielou
simp <- diversity(local, "simpson")
invsimp <- diversity(local, "inv")
```
E vamos plotar em gráfico, mas primeiro a tabela.
```
local<-reshape2::dcast(p2, Registro ~ Espécie, value.var = "Abundancia", fun = length)
local<-data.frame(S, spAbund, shannon,J, local)
```
E agora o gráfico. Lembrar de verificar:
- Se a variável está em colour de geom_point;
- No subtitle de labs.
- No caso de família, trocar o y para S e x para Família.
```
ggplot(local, aes(x = S, y = shannon)) + 
  geom_point(aes(size=spAbund, colour = Registro))+ 
  scale_size(range = c(.1, 18), name = "Abundância") +
  geom_label_repel(aes(label = S), size=4, alpha= 0.7, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  labs(title="Riqueza e diversidade", subtitle="Família", y="Diversidade",x="Riqueza", caption="",
       color = "Empresas", size = "Abundância de registros") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + theme_classic() 
#ggsave("Famíliamast.png",width = 9, height = 7, dpi = 600)
```
Um outro exemplo de gráfico é um de barras:
```
ggplot(local, aes(Família)) + 
  geom_bar(aes(weight = S, fill = shannon), alpha = 0.7) + 
  geom_point(aes(y = S, x = Família, size = spAbund)) +
  geom_label_repel(aes(y = S, x = Família, label = S), size=4, alpha= 1) +
  labs(title="Riqueza e diversidade", subtitle="Família", y="Riqueza",x="Família", caption="Dados primários",
       fill = "Diversidade", size = "Abundância") +
  scale_fill_continuous(type = "viridis") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + 
        coord_flip() + theme_classic() 
```
## 5. Cluster
Fazer um cladogroma de similaridade também pode nos ajudar a desenvolver nosso relatório. Vamos selecionar o que relacionar, podendo ser:
- Impacto;
- Localidade;
- Epresa.
```
pacman::p_load("ade4")
local<-reshape2::dcast(planilhatotal, Empresa ~ Espécie, value.var = "Abundancia", fun = length)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram
```
## 6. PCA
O PCA ou Análise de Componentes Principais ou PCA (Principal Component Analysis) é uma técnica de análise multivariada que pode ser usada para analisar inter-relações entre um grande número de variáveis e explicar essas variáveis em termos de suas dimensões inerentes (Componentes). O objetivo é encontrar um meio de condensar a informação contida em várias variáveis originais em um conjunto menor de variáveis estatísticas (componentes) com uma perda mínima de informação.
Primeiro os pacotes:
```
pacman::p_load(psych) library("devtools") library("ggbiplot") #install_github("vqv/ggbiplot")
```
Agra vamos selecionar os dados.Pode ser:
- Classe;
- Impacto;
- Localidade;
- Empresa.
Outra coisa a se atentar. 
- Riqueza;
- Abundância - add  ,value.var = "Abundancia", fun = length).
```
local<-reshape2::dcast(p2, Empresa ~ Classe)  #sem abudância para ser a riqueza/diversidade
local[ , which(apply(local, 2, var) != 0)]
local <- as.matrix(local[ ,-1])
grupo<-reshape2::dcast(p2, Empresa ~ Classe, value.var = "Abundancia", fun = length)
#grupo2<-reshape2::dcast(p2, País ~ Gênero, value.var = "Abundancia", fun = length)
```
Agora o gráficos:
```
wine.pca <- prcomp(local, scale. = TRUE)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         #groups = grupo$Fator, 
         ellipse = TRUE, circle = TRUE) +
  geom_label_repel(aes(label = grupo$Empresa), size=4, alpha= 1, 
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  theme_classic()
#ggsave("3.PCA_riqueza.png",width = 6, height = 5, dpi = 600)
```
E os resumos:
```
summary(prcomp(local, scale = TRUE))
biplot(prcomp(local, scale = TRUE))
```

## 7. Correlação
Por fim, vamoz fazer uma análise simmples de correlação usando o seguinte pacote.
`pacman::p_load(psych)`
Podemo relacionar:
- Grupo;
- Classe;
- Família;
- Empresa;
- Localidade;
- Vegetação.

Vamos selecionar:
```
local<-reshape2::dcast(p2, Família ~ Empresa, value.var = "Abundancia", fun = length)
local=data.frame(local, row.names=1)
```
E vamos plotar:
```
ind <- sample(2, nrow(local),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- local[ind==1,]
testing <- local[ind==2,]
pairs.panels(training[,-5],
             gap = 0,
             bg = c("red", "yellow", "blue"),
             pch=21)
```
