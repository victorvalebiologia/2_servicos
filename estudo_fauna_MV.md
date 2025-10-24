## Apresentação
Repositório para trelatório técnicos de estudo de fauna para uso pessoal. O intuito são análises de diversidade. Será dividido da seguinte forma:

- 1. Início;
- 2. Acumulação;
- 3. Riqueza estimada;
- 4. Diveridade;
- 5. Clueter;
- 6. PCA;
- 7. Correlação;
- 8. Dados secundários

## 1. Início
Primeiro, vamos indicar as pastas corretas.
- Prestar a atenção no diretório.
```
getwd()
setwd("/home/victor-vale/Documentos/Serviço/MV/R") 
```
Agora os principais pacotes utilizados:
```
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
#if(!require(devtools, quietly = TRUE))(install.packages("devtools")) #agrupador de dados
pacman::p_load(magrittr,dplyr,reshape2) #magrittr para operações de pipe/dplyr para manipulador de dados
pacman::p_load(ggplot2, ggrepel, graphics, lubridate, tidyquant, stringr) 
pacman::p_load(vegan)  #vegan para estatística ecológica/graphics para os gráficos
pacman::p_load(forcats,iNEXT,tidyr,tibble,iNEXT) #hill,CRAN e riqueza estimada
#pacman::p_load(tidyverse)
#update.packages(ask = FALSE, checkBuilt = TRUE)


```
Agora vamos adicionar a planilha. Algumas coisas devem ser notadas:
- O caminho do arquivo para a tabela de dados brutos;
```
# Carregar pacotes necessários
pacman::p_load(googledrive, googlesheets4, readxl, dplyr)

# Autenticar no Google Drive
drive_auth()

# 1. Acessar a pasta pelo ID
pasta_id <- "1rM3laNj-koZBwmtpL-mc2LmvkMLqAjWJ"
pasta <- drive_get(as_id(pasta_id))

# 2. Listar os arquivos na pasta
arquivos_na_pasta <- drive_ls(pasta)

# 3. Filtrar pelo nome do arquivo
arquivo <- arquivos_na_pasta %>% filter(name == "fauna_mv.xlsx")

# 4. Verificar se encontrou exatamente um arquivo
if (nrow(arquivo) == 1) {
  # Baixar o arquivo
  drive_download(file = as_id(arquivo$id), path = "fauna_mv.xlsx", overwrite = TRUE)
  message("Arquivo baixado com sucesso!")

  # 5. Ler o arquivo Excel e corrigir nomes de colunas
  planilhatotal <- read_excel("fauna_mv.xlsx", .name_repair = "minimal")

  # Exibir os nomes das colunas para verificação
  print(names(planilhatotal))

  message("Arquivo lido com sucesso!")

} else if (nrow(arquivo) == 0) {
  stop("Erro: Arquivo 'fauna_mv.xlsx' não encontrado na pasta.")
} else {
  stop("Erro: Mais de um arquivo com o mesmo nome encontrado. Verifique manualmente.")
}

```
Agora vamos filtrar a tabela. Primeiro tirar os dias não amostrados e espécie exótica.
```
p1 <- subset(planilhatotal, !is.na(Dia))
p1 <- subset(p1, !is.na(Mês))
p1 <- subset(p1, !is.na(Ano))
p1 <- subset(p1, !is.na(Abundancia)) 
p1 <- subset(p1, !is.na(Espécie))
```
Agora escolher o que analisar e atribuir uma tabela chamada p2 parar as análises:
```
p2 <- subset(p1, Origem == "Nativo")
p2 <- subset(p2, Dados == "Primários") 
psec <- p2

p2 <- subset(p2, Município == "Guarapari") #escolher uma
#p2 <- subset(p2, Localidade == "ALPHAVILLE Três Praias")
#p2 <- subset(p2, R == "R")
#p2 <- subset(p2, Grupo == "Mastofauna")

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

#p2 <- subset(p2, Grupo == "Herpetofauna") 
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
     xlab="Dias de amostragem",main="Curva de acumulação",
     las=1,font=1.5,font.lab=1.5,cex.lab=1,cex.axis=1) #curva clássica
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
  geom_label(aes(label = sp4$richness), size=4, alpha=0.8, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_size(range = c(.1, 24), name="Nº de registros") +
  #geom_text(aes(label = a$sp4.richness),col = 'black',size = 5) +
  labs(title="Curva do coletor", subtitle="Total", y="Riqueza",x="Data", caption="",
       color = "Diversidade de Shannon", size = "Nº de registros") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + 
        #scale_color_tq() + scale_fill_tq() +
        theme_tq()         
        
ggsave("2025_10_Acum2.pdf",width = 14, height = 6, dpi = 300) #, device = "png"
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

#p2 <- subset(p2, Grupo == "Hepertofauna") 

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

#p2 <- subset(p2, Grupo == "Herpetofauna") 

local<-reshape2::dcast(p2, Espécie ~ Grupo, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)
#local <- local[-nrow(local), ]

# Mude o q para 1 para comparar a diversidade de Shannon e para 2 para Simpson

out <- iNEXT(local, q = 0,
             datatype = "abundance",
             size = seq(0, 3500, length.out=20))

R <- ggiNEXT(out, type = 1) +
  theme_bw() +
  labs(fill = "Áreas") +
  #xlab("Riqueza) + 
  #ylab("Tempo") +
  scale_shape_manual(values = 0:19) +
  labs(title="Curva de acumulação por grupo", subtitle="", y="Riqueza",x="Abundâcia", caption="",
       color = "Grupos", size = "") +
  scale_color_tq() + scale_fill_tq() +
        theme_tq()          +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14), legend.position="bottom")

R

ggsave(width = 20, height = 10, 
       device = "pdf", filename = "2025_10_Acum2", plot = R)
       
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

#p2 <- subset(p2, !is.na(Vegetação))

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
  #geom_point(aes(y = S, x = Família, size = spAbund, colour = Ordem), alpha = 0.7) +
  geom_label(aes(y = S, x = Família, label = S), size=4, alpha= 1) +
  #facet_grid(Ordem~., scales = "free_y", space = "free_y") + 
  labs(title="Riqueza e diversidade", subtitle="Diversidade", y="Riqueza", x="Família", caption="Dados primários", fill = "Ordem", colour = "Ordem", size = "Riqueza") +
  scale_size_binned(range = c(.1, 18)) +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14)) + 
        coord_flip() + 
        #scale_color_tq() + scale_fill_tq() +
        theme_tq()         
        
ggsave("2025_10_r_fma.pdf",width = 10, height = 8, dpi = 600)
```
Agora para abundância
```
ggplot(local, aes(x = reorder(Família, spAbund), y = spAbund)) + 
  geom_col(aes(weight = spAbund, fill = Ordem), alpha = 0.7) + 
  #geom_point(aes(y = spAbund, x = Família, size = S, colour = Ordem), alpha = 0.7) +
  geom_label(aes(y = spAbund, x = Família, label = spAbund), size=4, alpha= 1) +
  labs(title="Riqueza e diversidade", subtitle="Diversidade", y="Abundância", x="Família", caption="Dados primários",
       fill = "Ordem", colour = "Ordem", size = "Riqueza") +
  scale_size_binned(range = c(.1, 18)) +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14)) + 
        coord_flip() + 
        #scale_color_tq() + scale_fill_tq() +
        theme_tq()         
        
ggsave("2025_10_a_rma.pdf",width = 10, height = 8, dpi = 600)


```

Um gráfico para tipo de registro:
```
p2 <- pbase
p3 <- tidyr::separate_rows(p2, Registro, sep = "/")
p3 <- tidyr::separate_rows(p3, Aparelho, sep = "/")


ggplot(p3, aes(x = Abundancia, y = Registro)) + 
  geom_jitter(aes(colour = Grupo, shape = Tipo), size = 5, alpha = 0.4)+ 
  scale_size(range = c(3, 17), name = "Abundância") +
  facet_grid(Grupo~., scales = "free_y", space = "free_y") + 
  labs(title="Tipo de registros", y="Registros",x="Abundancia de registros", caption="",
       color = "Tipo") +
  scale_color_tq() + scale_fill_tq() +
  theme_tq() +         
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + 
  guides(color = guide_legend(title = "Tipo"), 
       size = guide_legend(title = "Abundância de registros"))
        
        
ggsave("2025_10_registrn.pdf",width = 12, height = 8, dpi = 600)
```

outro gráfico para tipo de registro:
```

p4 <- subset(p3,Registro!="Busca Ativa") #excluir uma
p4 <- subset(p4,Registro!="Referência") #excluir uma


local<-reshape2::dcast(p4, Espécie + Grupo + Registro ~ Filo, value.var = "Abundancia", fun = sum)
local=data.frame(local,row.names=1)

ggplot(local, aes(x = Chordata, y = Espécie, colour = Grupo)) + 
  geom_boxplot(alpha = 0.6)+ 
  facet_wrap(~ Registro, scales = "free_y", ncol = 2) + 
  labs(title="", y="Tipo",x="Abundancia de registros", caption="",
       color = "Grupo") +
  scale_color_tq() + scale_fill_tq() +
  theme_tq() +         
  theme(axis.title = element_text(size = 8),
        axis.text = element_text(size = 8)) 

ggsave("2025_10_registr2n.pdf",width = 13, height = 6, dpi = 600)

```
E vamos de números:
```
p2 <- pbase
p3 <- tidyr::separate_rows(p2, Registro, sep = "/")
p3 <- tidyr::separate_rows(p3, Aparelho, sep = "/")
p4 <- subset(p3,Registro!="Referência") #excluir uma

local<-reshape2::dcast(p4, Registro ~ Espécie, value.var = "Abundancia", fun = sum)
local=data.frame(local,row.names=1)

S <- specnumber(local) 
spAbund <-rowSums(local) #abunância por faixa
shannon <- diversity(local)
J <- shannon/log(S) #Pielou
simp <- diversity(local, "simpson")
invsimp <- diversity(local, "inv")

# Criar tabela consolidada
tabela_qgis <- data.frame(
  Tipo = rownames(local),
  Riqueza = S,
  Abundancia_Total = spAbund,
  Shannon = shannon,
  Pielou = J,
  Simpson = simp,
  Inv_Simpson = invsimp
)

# Visualizar a tabela
print(tabela_qgis)

pacman::p_load(patchwork)

# Criar múltiplos gráficos
g1 <- ggplot(tabela_qgis, aes(x = reorder(Tipo, Riqueza), y = Riqueza, fill = Tipo)) +
  geom_col() + coord_flip() + theme_minimal() + labs(title = "Riqueza de Espécies", x = "") + theme(legend.position = "none")

g2 <- ggplot(tabela_qgis, aes(x = reorder(Tipo, Abundancia_Total), y = Abundancia_Total, fill = Tipo)) +
  geom_col() + coord_flip() + theme_minimal() + labs(title = "Abundância Total", x = "") + theme(legend.position = "none")

g3 <- ggplot(tabela_qgis, aes(x = reorder(Tipo, Shannon), y = Shannon, fill = Tipo)) +
  geom_col() + coord_flip() + theme_minimal() + labs(title = "Índice de Shannon", x = "") + theme(legend.position = "none")

g4 <- ggplot(tabela_qgis, aes(x = reorder(Tipo, Pielou), y = Pielou, fill = Tipo)) +
  geom_col() + coord_flip() + theme_minimal() + labs(title = "Equitabilidade (Pielou)", x = "") + theme(legend.position = "none")

# Combinar todos os gráficos
(g1 + g2) / (g3 + g4) + plot_annotation(title = "Métricas de Biodiversidade por Tipo")
```        
Um outro gráfico baseado na Série de Hill ajudam a entender a relação de diferentes grupos nos índices de diversidade:

- Vegetação;
- IOS.
```        
p2 <- pbase

p3 <- subset(p2, !is.na(Vegetação))
#p4 <- p3$Vegetação <- tolower(p3$Vegetação)
#p3 <- data.frame(p3,p4)

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
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14), legend.position="bottom") +
  scale_color_tq() + scale_fill_tq() +
  theme_tq() 

R  

ggsave(, width = 20, height = 10,device = "pdf", filename = "2025-veg", plot = R)

```
E uma estimativa tanto por Vegetação quanto pode vegetação.
```
p2 <- pbase
p3 <- subset(p2, !is.na(Vegetação))
p4 <- p3$Vegetação <- tolower(p3$Vegetação)
p3 <- data.frame(p3,p4)

p3<-reshape2::dcast(p2, Data + Vegetação ~ Espécie, value.var = "Abundancia", fun = sum)
excluir <- c("Data", "Vegetação")
p3 <- p3[,!(names(p3)%in% excluir)]
p4<-reshape2::dcast(Data, Data + Vegetação ~ Classe, value.var = "Abundancia", fun = sum)
```
Agora a estimativa de riqueza por localidade.
```
pool<-specpool(p3, p4$Vegetação) 
pool
boxplot(pool$chao) 
```



## 5. Cluster
Fazer um cladogroma de similaridade também pode nos ajudar a desenvolver nosso relatório. Vamos selecionar o que relacionar, podendo ser:
- Vegetação;
- Localidade;
- Epresa.
```
pacman::p_load("ade4")
local<-reshape2::dcast(p2, Vegetação ~ Espécie, value.var = "Abundancia", fun = sum)
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
p3 <- subset(p2, !is.na(Vegetação))
p4 <- p3$Vegetação <- tolower(p3$Vegetação)
p3 <- data.frame(p3,p4)

#p2 <- subset(p2, Grupo == "Mastofauna")

local<-reshape2::dcast(p3, Família ~ Vegetação, value.var = "Abundancia", fun.aggregate = NULL) #sum ou NULL
local=data.frame(local, row.names=1)

pca_res <- prcomp(local, scale. = TRUE)
#autoplot(pca_res)

local<-reshape2::dcast(p3, Família + Grupo ~ Vegetação, value.var = "Abundancia", fun.aggregate = NULL) #sum ou NULL
pca <-autoplot(pca_res, data = local, colour = 'Grupo', label = TRUE, label.size = 4, 
         frame = TRUE, frame.type = NULL, frame.color = 'Grupo', #ou frame.type = 't'
         loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE, loadings.label.size = 3) +                
         scale_color_tq() + scale_fill_tq() +
         theme_tq() 
  
pca

ggsave(width = 20, height = 10, device = "pdf", filename = "2025_10_PCArq", plot = pca)
#path = "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_03_Grancol/R"

```
E os resumos:
```
summary(prcomp(local, scale = TRUE))
biplot(prcomp(local, scale = TRUE))
```

## 7. Correlação
Por fim, vamoz fazer uma análise simmples de correlação usando o seguinte pacote.

Podemo relacionar:
- Grupo;
- Classe;
- Família;
- Empresa;
- Localidade;
- Vegetação.

Vamos selecionar:
```
pacman::p_load(psych)
local<-reshape2::dcast(p2, Espécie ~ Vegetação, value.var = "Abundancia", fun = sum)
local <- data.frame(local, row.names = 1)
local <- local[, -1]
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
## 9. Dados secundários

Vamos observar a localidade analisada em relação a outras. Primeiro selecionar a tabela.

Agora vamos filtrar a tabela. Primeiro tirar os dias não amostrados e espécie exótica.

```
p2 <- planilhatotal
p2 <- subset(p2, !is.na(Espécie))
#p2 <- subset(p2, !is.na(Abundancia))
p2 <- subset(p2, !is.na(Ano))
p2$Mês[is.na(p2$Mês)] <- 6
p2$Dia[is.na(p2$Dia)] <- 6
p2$Abundancia[is.na(p2$Abundancia)] <- 1

#p2 <- subset(p2, Tag == "Rio Doce") 

 
#p2 <- subset(p2,Município!="São Mateus") #excluir uma
#p2 <- subset(p2, Campanha == "3")
```
E ainda vamos atribuir as datas:
```
#p2 <- psec
p3 <- p2 %>% 
  select(Ano,Mês,Dia) %>% 
  mutate(Data = make_date(Ano,Mês,Dia))
Data <- data.frame(p3,p2)
Data <- subset(Data, !is.na(Data))

pbase <- Data
p2 <- Data
p2$Data <- as.Date(p2$Data)

```
E vamos gerar a curva:
```
acum<-reshape2::dcast(p2, Data ~ Espécie, value.var = "Abundancia", fun = sum)
acum=data.frame(acum, row.names=1)

sp4<-specaccum(acum,method="collector")
shannon<-diversity(acum)
spAbund<-rowSums(acum)
```
Agora preparamos a tabela:
```
acum<-reshape2::dcast(Data, Data ~ Espécie, value.var = "Abundancia", fun = sum)
p3<-data.frame(shannon,spAbund,sp4$sites,sp4$richness,acum)

ggplot(p3, aes(x = Data, y = sp4.richness)) + 
  geom_point(aes(size=spAbund), alpha=0.3) +
  geom_line(size=6, alpha=0.6, color="Gray") + #geom_line(aes(group = sp4.sites))
  geom_label(aes(label = sp4$richness), size=4, alpha=0.8, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_size(range = c(.1, 24), name="Nº de registros") +
  labs(title="Curva do coletor", subtitle="Total", y="Riqueza",x="Data", caption="",
       color = "Origem do dado", size = "Nº de registros") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + theme_classic() 

ggsave("2025_10_Acumtn.pdf",width = 14, height = 6, dpi = 300) #, device = "png"

```
Agora um gráfico para ver os grupos
```
p2 <- pbase

local<-reshape2::dcast(p2, Espécie ~ Grupo, value.var = "Abundancia", fun.aggregate = sum)
local=data.frame(local, row.names=1)
local <- local[-nrow(local), ]  #retirar ultima linha

# Mude o q para 1 para comparar a diversidade de Shannon e para 2 para Simpson

out <- iNEXT(local, q = 0,
             datatype = "abundance",
             size = seq(0, 5000, length.out=20))

R <- ggiNEXT(out, type = 1) +
  theme_bw() +
  labs(fill = "Áreas") +
  #xlab("Riqueza) + 
  #ylab("Tempo") +
  scale_shape_manual(values = 0:19) +
  labs(title="Curva de acumulação por grupo", subtitle="", y="Riqueza",x="Abundâcia", caption="",
       color = "Grupos", size = "") +
  theme_classic() +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14), legend.position="bottom")

R

ggsave(width = 20, height = 10, 
       device = "pdf", filename = "2025_10-Acumcltn", plot = R)
       
```
Podemos ver a similaridade entre os municípios. 
```

pacman::p_load("ade4")
local<-reshape2::dcast(p2, Localidade ~ Espécie, value.var = "Abundancia", fun = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram

```
Correlação

```
pacman::p_load(psych)

p2 <- subset(p2,Município!="6") #excluir uma

local<-reshape2::dcast(p2, Espécie ~ Localidade, value.var = "Abundancia", fun = NULL)
local=data.frame(local, row.names=1)
```
E vamos plotar:
```
pdf("plot.pdf")

ind <- sample(2, nrow(local),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- local[ind==1,]
testing <- local[ind==2,]
pairs.panels(training,
             gap = 0,
             bg = c("red", "yellow", "blue"),
             pch=21)

dev.off()

```
Teste
```
pacman::p_load(treemapify) 
pbase <- Data
p2 <- Data

local<-reshape2::dcast(p2, Localidade + Classe + Ordem ~ Filo) #sum ou NULL
#local=data.frame(local, row.names=1)


ggplot(local, aes(area = Chordata, fill = Classe, 
  label = paste (Ordem, Chordata, sep = "\n"), subgroup = Localidade)) +
  geom_treemap() +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.25, colour = "black",
                             fontface = "italic") +
  geom_treemap_text(colour = "white",
                    place = "bottom",
                    size = 10) +
  theme_classic() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), legend.position = "none") 
  
```
OUTROS
Teste

MST (Minimum Spanning Tree): É uma técnica de análise de rede que encontra a árvore de menor custo que conecta todos os vértices de um grafo ponderado. Em biogeografia, a MST pode ser usada para identificar padrões de conectividade entre diferentes áreas geográficas com base em dados de distância ou similaridade.

No gŕafico, as linhas representas as comunidades com menor cursto de coenxão e os pontos sem linhas as comunidades isoladas.
```
pacman::p_load("ggplot2", "spaa", "recluster", "analogue", "ape", "vegan")

local<-reshape2::dcast(p2, Localidade ~ Espécie, value.var = "Abundancia",fun.aggregate = sum)
local <- local[complete.cases(local), ]
local=data.frame(local, row.names=1)

dist_matrix <- vegdist(local, method = "jaccard")
dist_matrix[is.na(dist_matrix)] <- 0  # Por exemplo, substituir NA por 0
mst_tree <- mst(dist_matrix)
nomes_localidades <- rownames(local)
mst_coordinates <- cmdscale(dist_matrix)

# Converter os nomes das linhas em números inteiros
indices_nos <- seq_len(nrow(mst_coordinates))
rownames(mst_coordinates) <- indices_nos

# Criar uma matriz de coordenadas das arestas
edges <- which(mst_tree != 0, arr.ind = TRUE)
edges <- cbind(edges, Value = mst_tree[edges])

# Converter para dataframe
df_arestas <- as.data.frame(edges)
colnames(df_arestas) <- c("X1", "X2", "Value")

# Criar dataframe vazio para as linhas
df_lines <- data.frame()

# Iterar sobre as arestas e adicionar as coordenadas ao dataframe df_lines
for (i in seq_len(nrow(df_arestas))) {
  indice_X1 <- df_arestas$X1[i]
  indice_X2 <- df_arestas$X2[i]
  
  # Obter as coordenadas dos nós
  coord_X1 <- mst_coordinates[indice_X1, 1]
  coord_Y1 <- mst_coordinates[indice_X1, 2]
  coord_X2 <- mst_coordinates[indice_X2, 1]
  coord_Y2 <- mst_coordinates[indice_X2, 2]
  
  # Adicionar as coordenadas ao dataframe df_lines
  df_lines <- rbind(df_lines, data.frame(X1 = coord_X1, Y1 = coord_Y1, X2 = coord_X2, Y2 = coord_Y2))
}

# Convertendo as coordenadas da matriz mst_coordinates em dataframe
df_mst_coordinates <- as.data.frame(mst_coordinates)
colnames(df_mst_coordinates) <- c("X", "Y")

# Plotar a MST com ggplot2
ggplot() +
  geom_segment(data = df_lines, aes(x = X1, y = Y1, xend = X2, yend = Y2), color = "blue") +
  geom_point(data = df_mst_coordinates, aes(x = X, y = Y, colour = nomes_localidades), size = 5) +
  geom_text_repel(data = df_mst_coordinates, aes(x = X, y = Y, label = nomes_localidades), vjust = -0.5) +
  labs(title = "Minimum Spanning Tree", x = "Comunidade", y = "Comunidade") +
  scale_color_tq() + scale_fill_tq() +
  theme_tq() + theme(legend.position = "none")
