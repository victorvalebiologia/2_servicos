## Apresentação
Repositório para trelatório técnicos de estudo de fauna para uso pessoal. O intuito são análises de diversidade. Será dividido da seguinte forma:

- 1. Início;
- 2. Acumulação;
- 3. Riqueza estimada;
- 4. Diveridade;
- 5. Clueter;
- 6. PCA;
- 7. Correlação.

## 1. Início
Primeiro, vamos indicar as pastas corretas.
- Prestar a atenção no diretório.
```
getwd()
setwd("/home/user/Área de Trabalho/Serviços/MG - Porteirinhas/2022_10_13_nucleo_ambiental/R") 
```
Agora os principais pacotes utilizados:
```
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
#if(!require(devtools, quietly = TRUE))(install.packages("devtools")) #agrupador de dados
pacman::p_load(magrittr,dplyr,reshape2) #magrittr para operações de pipe/dplyr para manipulador de dados
pacman::p_load(ggplot2, ggrepel, graphics, lubridate) 
pacman::p_load(vegan)  #vegan para estatística ecológica/graphics para os gráficos
pacman::p_load(forcats,iNEXT,tidyr,tibble,iNEXT) #hill,CRAN e riqueza estimada
#pacman::p_load(tidyverse)
```
Agora vamos adicionar a planilha. Algumas coisas devem ser notadas:
- O caminho do arquivo para a tabela de dados brutos;
```
pacman::p_load(openxlsx) 
caminho.do.arquivo <- "/home/user/Área de Trabalho/Serviços/2021_06_07_minas.xlsx"
planilhatotal <- read.xlsx(caminho.do.arquivo, #local do arquivo
                         sheet = 1, # em qual planilha estão os dados
                         colNames = T, # as colunas dos dados possuem nomes?
                         na.strings = "NA") # como estão identificados os dados omissos?
#head(planilhatotal)
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
p2 <- subset(p2, Origem == "Nativo")
p2 <- subset(p2, Dados == "Primários") 

p2 <- subset(p2, Empresa == "Todos os Santos") #escolher uma
#p2 <- subset(p2, Ano == "2022")

#p2 <- subset(p2,Empresa!="Gran Primos") #excluir uma
#p2 <- subset(p2, Campanha == "3")
```
E ainda vamos atribuir as datas:
```
p3 <- p2 %>% 
  select(Ano,Mês,Dia) %>% 
  mutate(Data = make_date(Ano,Mês,Dia))
Data <- data.frame(p3,p2)
Data <- subset(Data, !is.na(Data))

pbase <- Data
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

p2 <- subset(p2, Grupo == "Herpetofauna") 
#p2 <- subset(p2,Grupo!="Herpetofauna") 
```
Agora a tabela para trabalhar.
```
acum<-reshape2::dcast(p2, Data ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
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
acum<-reshape2::dcast(p2, Data ~ Espécie, value.var = "Abundancia", fun = sum)
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
#png(filename="/home/user/Área de Trabalho/Serviços/ES - BaixoGuandu/2021_06_30_granitos_itaguacu/R/1.Acumul.png",width=800,height=600) #local e tmamanho
par(mfrow=c(2,2)) 
plot(sp1, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Dias de amostragem",ylab="Rarefação")
plot(sp2, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgrey",xlab="Dias de amostragem",ylab="Riqueza Esperada")
plot(sp3, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="yellow",xlab="Dias de amostragem",ylab="Sítios Aleatórios")
plot(sp4, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Dias de amostragem",ylab="Curva do Coletor")
par(mfrow=c(1,1)) #compilado de curvas
#dev.off()
```
Podemos focar também a curva do coletor e adicionar a Abundancia por dia. Primeiro vamos aos cálculos de diversidade e Abundancia.
```
sp4<-specaccum(acum,method="collector")
shannon<-diversity(acum)
spAbund<-rowSums(acum)
```
Agora preparamos a tabela:
```
acum<-reshape2::dcast(Data, Data ~ Espécie, value.var = "Abundancia", fun = sum)
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
  scale_size(range = c(.1, 24), name="Abundancia de registros") +
  #geom_text(aes(label = a$sp4.richness),col = 'black',size = 5) +
  labs(title="Curva do coletor", subtitle="Total", y="Riqueza",x="Data", caption="",
       color = "Diversidade", size = "Abundancia de registros") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + theme_classic() 
#ggsave("Acum2.png",width = 14, height = 6, dpi = 300)
```

## 3. Estimativa de riqueza
Vamos também estimar a riqueza. Vamos selecionar a tabela. Podem ter duas variáveis:
- Trilha;
- Localidade;
- Empresa;
- Grupo;

Primeiro vamos foltrar e atribuir as datas:

```
p2 <- pbase

p2 <- subset(p2, Grupo == "Hepertofauna") 

```

## 2. Estimativa de riqueza
Vamos cacular os principais índices de dievrsidade aqui. Primeiro vamos selecionar oa dados que podem ser:
- Gerais;
- Avifauna;
- Herpetofauna;
- Mastofauna.
Além de filtrar para apenas dados primários e espećies nativas.

```
p3<-reshape2::dcast(p2, Data ~ Espécie, value.var = "Abundancia", fun = sum)
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
- Uma indicando a Abundancia de espécies por dia, denominada de p3;
- Uma indicando a variável, denominada p4.
- Lembrar de conferir a variável
```
#p3 <- subset(Data, Empresa == "XXX")

p3<-reshape2::dcast(p2, Data + Grupo ~ Espécie, value.var = "Abundancia", fun = sum)
excluir <- c("Data", "Grupo")
p3 <- p3[,!(names(p3)%in% excluir)]
p4<-reshape2::dcast(Data, Data + Grupo ~ Classe, value.var = "Abundancia", fun = sum)
```
Agora a estimativa de riqueza por localidade.
```
pool<-specpool(p3, p4$Grupo) 
pool
boxplot(pool$chao) 
```
Agora um gráfico unificado.
```
p2 <- pbase

local<-reshape2::dcast(p2, Espécie ~ Grupo, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)

# Mude o q para 1 para comparar a diversidade de Shannon e para 2 para Simpson

out <- iNEXT(local, q = 0,
             datatype = "abundance",
             size = seq(0, 300, length.out=20))

R <- ggiNEXT(out, type = 1) +
  theme_bw() +
  labs(fill = "Áreas") +
  #xlab("Riqueza) + 
  #ylab("Tempo") +
  scale_shape_manual(values = 0:19) +
  theme_classic() +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14), legend.position="bottom")

R

ggsave(width = 20, height = 10, 
       device = "png", filename = "Acum3", plot = R)
       
```

## 4. Diversidade
Aqui também podemos filtrar a tabela.
- Gerais;
- Avifauna;
- Herpetofauna;
- Mastofauna.
Além de filtrar para apenas dados primários e espećies nativas.
```
p2 <- pbase
p2 <- subset(p2, Grupo == "Mastofauna")

#p2 <- subset(p2, !is.na(Impacto))

```
Agora vamos filtrar a tabela, ela pode ser por:
- Classe;
- Família;
- Influência;
- Empresa;
- Localidade.
```
local<-reshape2::dcast(p2, Família ~ Espécie, value.var = "Abundancia", fun = sum)
local=data.frame(local,row.names=1)
```
E vamos aos cálculos;
```
S <- specnumber(local) 
spAbund <-rowSums(local) #abunância por faixa
shannon <- diversity(local)
J <- shannon/log(S) #Pielou
simp <- diversity(local, "simpson")
invsimp <- diversity(local, "inv")
```
E vamos plotar em gráfico, mas primeiro a tabela.
```
local<-reshape2::dcast(p2, Família + Ordem ~ Espécie, value.var = "Abundancia", fun = sum)
local<-data.frame(S, spAbund, shannon,J, local)
```
E agora o gráfico. Lembrar de verificar:
- Se a variável está em colour de geom_point;
- No subtitle de labs.
- No caso de família, trocar o y para S e x para Família.

Um outro exemplo de gráfico é um de barras:
```
ggplot(local, aes(x = reorder(Família, S), y = S)) + 
  geom_col(aes(weight = S, fill = Ordem), alpha = 0.7) + 
  geom_point(aes(y = S, x = Família, size = spAbund, colour = Ordem), alpha = 0.7) +
  geom_label(aes(y = S, x = Família, label = S), size=4, alpha= 1) +
  labs(title="Riqueza e diversidade", subtitle="Diversidade", y="Riqueza", x="Família", caption="Dados primários",
       fill = "Ordem", colour = "Ordem", size = "Abundancia") +
  scale_size_binned(range = c(.1, 18)) +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14)) + 
        coord_flip() + theme_classic() 
#ggsave("famma.png",width = 10, height = 8, dpi = 600)
```
Um gráfico para tipo de registro:
```
p2 <- pbase

ggplot(p2, aes(x = Abundancia, y = Registro)) + 
  geom_point(aes(size=Abundancia, colour = Grupo, shape = Grupo), alpha = 0.6)+ 
  scale_size(range = c(.1, 18), name = "Abundancia") +
  facet_grid(Tipo~., scales = "free_y", space = "free_y") + 
  labs(title="Tipo de registros", y="Tipo",x="Abundancia de registros", caption="",
       color = "Grupo", size = "Abundancia de registros") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + theme_classic() 
#ggsave("registr.png",width = 9, height = 7, dpi = 600)

```        
Um outro gráfico baseado na Série de Hill ajudam a entender a relação de diferentes grupos nos índices de diversidade:

- Impacto;
- IOS.
```        
p2 <- pbase

p3 <- subset(p2, !is.na(Vegetação))

local<-reshape2::dcast(p3, Vegetação ~ Espécie, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)

R <- renyi(local,hill = TRUE)

R <- R %>%  
  rownames_to_column() %>% 
  pivot_longer(-rowname) %>% 
  mutate(name = factor(name, name[1:length(R)])) %>% 
  ggplot(aes(x = name, y = value, group = rowname,
             col = rowname)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlab("Parâmetro de ordem de diversidade (q)") +
  ylab("Diversidade") +
  labs(col = "Tipos") +
  theme_classic() +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14), legend.position="bottom")

R  

ggsave(, width = 20, height = 10, 
       device = "png", filename = "veg", plot = R)
```        

Ou assim para Vegetação e uos:
```
local<-reshape2::dcast(p2, Vegetação ~ Espécie, value.var = "Abundancia", fun = sum)
local<-data.frame(S, spAbund, shannon,J, local)

ggplot(local, aes(Vegetação)) + 
  geom_bar(aes(weight = S, fill = shannon), alpha = 0.7) + 
  geom_point(aes(y = S, x = Vegetação, size = spAbund)) +
  geom_label(aes(y = S, x = Vegetação, label = S), size=4, alpha= 1) +
  #geom_label_repel(aes(y = S, x = Vegetação, label = spAbund), size=4, alpha= 1, colour = "red") +
  labs(title="Riqueza e diversidade", subtitle="Diversidade", y="Riqueza", x="Uso e ocupação do solo", caption="Dados primários",
       fill = "Diversidade", size = "Abundancia") +
  scale_size_binned(range = c(.1, 18)) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + 
        coord_flip() + theme_classic() 
#ggsave("veg.png",width = 9, height = 7, dpi = 600)
```

```
ggplot(local, aes(x = S, y = shannon)) + 
  geom_point(aes(size=spAbund, colour = Família))+ 
  scale_size(range = c(.1, 18), name = "Abundancia") +
  geom_label_repel(aes(label = S), size=4, alpha= 0.7, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  labs(title="Riqueza e diversidade", subtitle="Ordem", y="Diversidade",x="Riqueza", caption="",
       color = "Empresas", size = "Abundancia de registros") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + theme_classic() 
#ggsave("Famíliamast.png",width = 9, height = 7, dpi = 600)
```
## 5. Cluster
Fazer um cladogroma de similaridade também pode nos ajudar a desenvolver nosso relatório. Vamos selecionar o que relacionar, podendo ser:
- Vegetação;
- Localidade;
- Epresa.
```
pacman::p_load("ade4")
local<-reshape2::dcast(planilhatotal, Vegetação ~ Espécie, value.var = "Abundancia", fun = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram
```

## 6. PCA
O PCA ou Análise de Componentes Principais ou PCA (Principal Component Analysis) é uma técnica de análise multivariada que pode ser usada para analisar inter-relações entre um grande número de variáveis e explicar essas variáveis em termos de suas dimensões inerentes (Componentes). O objetivo é encontrar um meio de condensar a informação contida em várias variáveis originais em um conjunto menor de variáveis estatísticas (componentes) com uma perda mínima de informação.
Primeiro os pacotes:
```
pacman::p_load(ggfortify, cluster)

p2 <- pbase

#p2 <- subset(p2, Grupo == "Mastofauna")

local<-reshape2::dcast(p2, Ordem ~ Vegetação, value.var = "Abundancia", fun.aggregate = NULL) #sum ou NULL
local=data.frame(local, row.names=1)

pca_res <- prcomp(local, scale. = TRUE)
#autoplot(pca_res)

local<-reshape2::dcast(p2, Ordem + Grupo ~ Vegetação, value.var = "Abundancia", fun.aggregate = sum) #sum ou NULL
pca <-autoplot(pca_res, data = local, colour = 'Grupo', label = TRUE, label.size = 4, 
frame = TRUE, frame.type = NULL, frame.color = 'Grupo', #ou frame.type = 't'
         loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE, loadings.label.size = 3) +                
         theme_classic() 
pca

ggsave(width = 20, height = 10, device = "png", filename = "PCAriq", plot = pca)
#path = "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_03_Grancol/R"

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
local<-reshape2::dcast(p2, Espécie ~ Vegetação, value.var = "Abundancia", fun = sum)
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
## 8. Diversidade (alfa, beta e gama)
Por fim, vamoz fazer uma análise simples de correlação usando o seguinte pacote.
`pacman::p_load(entropart, hillR)`

Podemo relacionar:
- Grupo;
- Classe;
- Família;
- Empresa;
- Localidade;
- Vegetação.

Vamos com a tabela espécie por localidade:
```
local<-reshape2::dcast(p2, Espécie ~ Vegetação, value.var = "Abundancia", fun = sum)
```

Agora calacular a metacomunidade:
```
MC<-MetaCommunity(local) 
summary(MC)
```
Primeiro a alfa comunidade, podendo mudar os fatores de 0, 1 e 2. Ainda o programa dá um alfa para cada localidade e uma alfa média = $Total
```
AlphaDiversity(MC, 0, Correction = "None") 
```
O mesmo para beta diveridade. Já adiantamos na forma de summary.
```
summary(BetaDiversity(MC, 0, Correction = "None")) 
```
E gama onde 0 = riqueza, 1 =shannon  e 2 = simpson.
```
GammaDiversity(MC, 0, Correction = "None")
```
Finalmente para entendermos a reação entre Alfa, Beta e Gama. Só é preciso mudar entre 0, 1 e 2 para obter para todas as ordens
```
summary(DivPart(q = 0, MC, Biased = FALSE, Correction = "None") -> dp0) ####
```
Agora um gráfico. Para entender os gráficos, pense na relação entre as diversidaded e Whittaker… A barra maior horizontal é a gama, barra menor, marca a alfa no eixo ‘x’ e a beta no eixo ‘y’ De forma que a área da barra menor é sempre isgual à da barra maior, mudando sua forma (mais alta e menos comprida. ou vice-versa) segundo a contribuição de alfa ou beta para a gama.
```
plot(dp0) 
```
