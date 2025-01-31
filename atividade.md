## Apresentação
Repositório para trelatório técnicos de estudo de fauna para uso pessoal. O intuito são análises de diversidade. Será dividido da seguinte forma:

- 1. Início;
- 2. Acumulação;
- 3. Riqueza estimada;
- 4. Diveridade;
- 5. Cluster;
- 6. PCA;
- 7. Correlação;
- 8. Dados secundários

## 1. Início
Primeiro, vamos indicar as pastas corretas.
- Prestar a atenção no diretório.
```
getwd()
setwd("/home/kaetes/Documentos/Gráficos/PSCA") 
```
Agora os principais pacotes utilizados:
```
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
#if(!require(devtools, quietly = TRUE))(install.packages("devtools")) #agrupador de dados
pacman::p_load(magrittr,dplyr,reshape2) #magrittr para operações de pipe/dplyr para manipulador de dados
pacman::p_load(ggplot2, ggrepel, graphics, lubridate, stringr) 
pacman::p_load(vegan)  #vegan para estatística ecológica/graphics para os gráficos
pacman::p_load(forcats,iNEXT,tidyr,tibble,iNEXT, hms) #hill,CRAN e riqueza estimada
#pacman::p_load(tidyverse)
#update.packages(ask = FALSE, checkBuilt = TRUE)


```
Agora vamos adicionar a planilha. Algumas coisas devem ser notadas:
- O caminho do arquivo para a tabela de dados brutos;
```
pacman::p_load(openxlsx) 
caminho.do.arquivo <- "/home/valev/Área de Trabalho/Planilhas/PCSA/2024_11_atividade_PCSA.xlsx"
planilhatotal <- read.xlsx(caminho.do.arquivo, #local do arquivo
                         sheet = 1, # em qual planilha estão os dados
                         colNames = T, # as colunas dos dados possuem nomes?
                         na.strings = "NA") # como estão identificados os dados omissos?
#head(planilhatotal)
```
Ou pelo Google Drive
```
pacman::p_load(googledrive, googlesheets4, readxl) 

drive_auth()
#drive_find(pattern = "2024_11_atividade_PCSA.xlsx")


arquivo <- drive_get("2024_11_atividade_PCSA.xlsx")
drive_download(file = arquivo$id, path = "2024_11_atividade_PCSA.xlsx", type = "xlsx", overwrite = TRUE)

library(readxl)
planilhatotal <- read_excel("2024_11_atividade_PCSA.xlsx")

#head(planilhatotal)

```

Agora vamos filtrar a tabela. Primeiro tirar os dias não amostrados e espécie exótica.
```
p1 <- subset(planilhatotal, !is.na(Dia))
p2 <- subset(p1, !is.na(Mês))
p2 <- subset(p2, !is.na(Ano))
p2 <- subset(p2, !is.na(Latitude))

p2 <- subset(p2, Ano == "2024")
p2 <- subset(p2, Mês == "12") 

p2 <- tidyr::separate_rows(p2, Grupo, sep = "/")

```
E ainda vamos atribuir as datas:
```
p3 <- p2 %>% 
  select(Ano,Mês,Dia) %>% 
  mutate(Data = make_date(Ano,Mês,Dia))
Data <- data.frame(p3,p2)
Data <- subset(Data, !is.na(Data))
Data <- Data %>%
  mutate(across(where(is.numeric), ~ ifelse(. == 0, 20, .)))

pbase <- Data
```
## Demanda

Um gráfico para tipo de registro:
```
p2 <- subset(Data, Projeto == "Infraestrutura")

ggplot(p2, aes(x = Data, y = Objetivo)) + 
  geom_jitter(aes(size = Minutos, colour = Grupo, shape = Atividade), alpha = 0.6)+ 
  scale_size(range = c(3, 17), name = "Abundância") +
  facet_grid(Projeto~., scales = "free_y", space = "free_y") + 
  labs(title="Tipo de registros", y="Atividades",x="Data", caption="",
       color = "Tipo", size = "Minutos gastos") +
  theme_minimal() +         
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + 
  guides(color = guide_legend(title = "Responsável"), 
       size = guide_legend(title = "Minutos gastos"))
       
ggsave("2024_12_atividade_IE_dia.pdf",width = 12, height = 8, dpi = 600)
```

Um outro gráfico baseado na Série de Hill ajudam a entender a relação de diferentes grupos nos índices de diversidade:

- Vegetação;
- IOS.
```        
p2 <- Data

p3 <- subset(p2, !is.na(Projeto))
p3 <- subset(p2, !is.na(Objetivo))

local<-reshape2::dcast(p3, Grupo ~ Objetivo, value.var = "Minutos", fun.aggregate = sum)
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
  ylab("Tipos de atividade") +
  labs(col = "Tipos") +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14), legend.position="bottom") +
  theme_minimal() 

R  

ggsave(, width = 20, height = 10,device = "pdf", filename = "2024_-_11_imp2", plot = R)
```
## 6. PCA
O PCA ou Análise de Componentes Principais ou PCA (Principal Component Analysis) é uma técnica de análise multivariada que pode ser usada para analisar inter-relações entre um grande número de variáveis e explicar essas variáveis em termos de suas dimensões inerentes (Componentes). O objetivo é encontrar um meio de condensar a informação contida em várias variáveis originais em um conjunto menor de variáveis estatísticas (componentes) com uma perda mínima de informação.
Primeiro os pacotes:
```
pacman::p_load(ggfortify, cluster)

p2 <- Data
p3 <- subset(p2, !is.na(Atividade))
#p3 <- subset(p2,T.Atividade!="Outros") #excluir uma
#p3 <- subset(p2,Atividade!="Compras") #excluir uma
#p3 <- subset(p2, !(Grupo %in% c("Fábio Moraes", "Valdívia", "Marcelo de Deus", "Fabiano Moraes")))

#p2 <- subset(p2, Grupo == "Mastofauna")

local<-reshape2::dcast(p3, Objetivo ~ Grupo, value.var = "Minutos", fun.aggregate = sum) #sum ou NULL
#local <- local[complete.cases(local), ]
local[is.na(local)] <- 0
local=data.frame(local, row.names=1)

pca_res <- prcomp(local, scale. = TRUE)
#autoplot(pca_res)

local<-reshape2::dcast(p3, Objetivo + Projeto ~ Grupo, value.var = "Minutos", fun.aggregate = sum) #sum ou NULL
#local <- local[complete.cases(local), ]

pca <-autoplot(pca_res, data = local, colour = 'Projeto', label = TRUE, label.size = 4, 
         frame = TRUE, frame.color = 'Projeto', # ou frame.type = 't',
         loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE, loadings.label.size = 3) +                
         theme_minimal() 
  
pca

ggsave(width = 20, height = 10, device = "pdf", filename = "2024_12_PCA_ocupacao", plot = pca)
#path = "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_03_Grancol/R"




```
E os resumos:
```
summary(prcomp(local, scale = TRUE))
biplot(prcomp(local, scale = TRUE))
```
E o contrário
```
pacman::p_load(ggfortify, cluster)

p2 <- Data
p3 <- subset(p2, !is.na(Atividade))
#p3 <- subset(p2,T.Atividade!="Outros") #excluir uma
#p3 <- subset(p2,Atividade!="Compras") #excluir uma
#p3 <- subset(p2, !(Grupo %in% c("Fábio Moraes", "Valdívia", "Marcelo de Deus", "Fabiano Moraes")))

#p2 <- subset(p2, Grupo == "Mastofauna")

local<-reshape2::dcast(p3, Grupo ~ Atividade, value.var = "Minutos", fun.aggregate = sum) #sum ou NULL
local[is.na(local)] <- 0
local=data.frame(local, row.names=1)

pca_res <- prcomp(local, scale. = TRUE)
#autoplot(pca_res)

local<-reshape2::dcast(p3, Grupo ~ Atividade, value.var = "Minutos", fun.aggregate = sum) #sum ou NULL
pca <-autoplot(pca_res, data = local, label = TRUE, label.size = 4, #colour = 'Projeto', 
         #frame = TRUE, frame.color = 'Projeto', # ou frame.type = 't',
         loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE, loadings.label.size = 3) +                
         theme_minimal() 
  
pca

ggsave(width = 20, height = 10, device = "pdf", filename = "2024_12_PCA_ocupacao2", plot = pca)
#path = "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_03_Grancol/R"



```
OUTROS
Teste

MST (Minimum Spanning Tree): É uma técnica de análise de rede que encontra a árvore de menor custo que conecta todos os vértices de um grafo ponderado. Em biogeografia, a MST pode ser usada para identificar padrões de conectividade entre diferentes áreas geográficas com base em dados de distância ou similaridade.

No gŕafico, as linhas representas as comunidades com menor cursto de coenxão e os pontos sem linhas as comunidades isoladas.
```
pacman::p_load("ggplot2", "spaa", "recluster", "analogue", "ape", "vegan")

p2 <- tidyr::separate_rows(Data, Município, sep = "/")
local<-reshape2::dcast(p2, Grupo ~ Objetivo, value.var = "Minutos",fun.aggregate = sum) #ou Município/Grupo
local[is.na(local)] <- 0
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
  theme_minimal() + theme(legend.position = "none")

ggsave(width = 20, height = 10, device = "pdf", filename = "2024_11_mde_gru")


```
Atividade por data
```
pacman::p_load(ggside, stringr) #, tidyverse,tidyquant)

Data <-pbase

Data$Ho <- hms::as_hms(Data$Ho)
class(Data$Ho)
Data$Ho <- hms::as_hms(Data$Ho * 86400)  # 86400 segundos em um dia

p3 <- Data
#p3 <- tidyr::separate_rows(p2, Categoria, sep = "/")
p3 <- subset(p3, !is.na(Atividade))
#p3 <- subset(p3, !(Grupo %in% c("Fábio Moraes", "Valdívia", "Marcelo de Deus", "Fabiano Moraes", "Paula Faccini")))


ggplot(p3, aes(x = Data, y = Ho)) + 
  geom_jitter(aes(colour = Grupo, size = Minutos, shape = Projeto), alpha = 0.6) + 
  scale_shape_manual(values = 0:10) +
  scale_size(range = c(5, 18), name = "Tempo de atividade (min)") +
  #facet_grid(Categoria~., scales = "free_y", space = "free_y") +
  #geom_smooth(method = lm,se = FALSE, alpha = 0.6, aes(colour = Grupo), method.args = list(iter.max = 1000)) +  #method = lm ou loess
  #geom_xsideboxplot(aes(fill = Grupo),alpha = 0.5) +
  geom_xsidedensity(aes(y = after_stat(density), group = Grupo, fill = Grupo),alpha = 0.5, size = 0.5, position = "stack", outline.type = "full") + 
  geom_ysideboxplot(aes(fill = Grupo),alpha = 0.5) + 
  stat_ellipse(geom="polygon", aes(fill = Grupo), alpha = 0.2, show.legend = TRUE,level = 0.25) + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  scale_fill_manual(values = rainbow(length(unique(p3$Grupo)))) +
  theme_minimal() 

ggsave(width = 20, height = 13, device = "pdf", filename = "2024_12_tempo")
```
Gráfico lateral com a escala de tempo para o serviço.
```

ggplot(p3, aes(x = Data, y = Minutos, colour = Grupo)) +
  #geom_boxplot() +
  #geom_smooth(aes(color = Grupo), se = TRUE, method = "loess") +
  stat_density(aes(y = after_stat(count), fill = Grupo), alpha = 0.5, size = 1, position = "stack") + #position = fill
  scale_fill_manual(values = rainbow(length(unique(p3$Grupo)))) +
  theme_minimal() +
  labs(title = "Acumulado de atividades",
       subtitle = "Acumulação",
       x = "Tempo",
       y = "Atividades") +
  theme(ggside.panel.scale.x = 0.2,
    ggside.panel.scale.y = 0.2,
    legend.position = "bottom",  # Posiciona a legenda à direita para melhor clareza
    axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos do eixo X para melhor leitura
  
ggsave("2024_12_overlap2.png",width = 12, height = 8, dpi = 600)

```
Atividade por data
```

p3 <- Data
#p3 <- tidyr::separate_rows(p2, Categoria, sep = "/")
p3 <- subset(p3, !is.na(Atividade))
#p3 <- subset(p3, !(Grupo %in% c("Fábio Moraes", "Valdívia", "Marcelo de Deus", "Fabiano Moraes", "Paula Faccini")))

local<-reshape2::dcast(p3, Grupo + Projeto + Atividade~ Estado, value.var = "Minutos", fun.aggregate = sum) #sum

ggplot(local, aes(x = Projeto, y = `Espírito Santo`, fill = Grupo)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = `Espírito Santo`), vjust = -0.5) + # Exibir valores sobre as barras
  labs(
    title = "Gasto de tempo em minutos",
    x = "Categoria",
    y = "Minutos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = rainbow(length(unique(local$Grupo))))


ggsave(width = 20, height = 10, device = "pdf", filename = "2024_12_tempoprojeto")

```
Gráfico barar
```
p3 <- subset(p2, !is.na(Minutos))

local<-reshape2::dcast(p3, Grupo + Projeto + Atividade~ Estado, value.var = "Minutos", fun.aggregate = sum) #sum

ggplot(local, aes(x = Grupo, y = `Espírito Santo`, fill = Projeto)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = `Espírito Santo`), vjust = -0.5) + # Exibir valores sobre as barras
  labs(
    title = "Gasto de tempo em minutos",
    x = "Categoria",
    y = "Minutos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = rainbow(length(unique(local$Projeto))))

ggsave(width = 20, height = 10, device = "pdf", filename = "2024_12_tempoprojeto")
