## Apresentação
Repositório para trelatório técnicos de estudo de fauna para uso pessoal. O intuito são análises de diversidade. Será dividido da seguinte forma:

## 1. Início
Primeiro, vamos indicar as pastas corretas.
- Prestar a atenção no diretório.
```
getwd()
setwd("/home/kaetes/Documentos/Gráficos/PSCA") 
```
Agora os principais pacotes utilizados:
```
# Verifica se o pacote pacman está instalado; se não, instala
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  magrittr, dplyr, reshape2,   # Manipulação de dados
  ggplot2, ggrepel, graphics,  # Visualização de dados
  lubridate, stringr,          # Manipulação de datas e strings
  vegan,                       # Estatística ecológica
  forcats, iNEXT, tidyr, tibble, hms # Análises ecológicas e manipulação de dados
)

# Atualiza pacotes sem pedir confirmação (com checkBuilt = TRUE para evitar incompatibilidades)
# update.packages(ask = FALSE, checkBuilt = TRUE)


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
# Carregar pacotes necessários
pacman::p_load(googledrive, googlesheets4, readxl, dplyr)

# Autenticar no Google Drive
drive_auth()

# 1. Acessar a pasta pelo ID
pasta_id <- "11woQwkbTf7LtfiuZFFtBggQUClzBKPYD"
pasta <- drive_get(as_id(pasta_id))

# 2. Listar os arquivos na pasta
arquivos_na_pasta <- drive_ls(pasta)

# 3. Filtrar pelo nome do arquivo
arquivo <- arquivos_na_pasta %>% filter(name == "2025_Nemosia.xlsx")

# 4. Verificar se encontrou exatamente um arquivo
if (nrow(arquivo) == 1) {
  # Baixar o arquivo
  drive_download(file = as_id(arquivo$id), path = "2025_Nemosia.xlsx", overwrite = TRUE)
  message("Arquivo baixado com sucesso!")

  # 5. Ler o arquivo Excel e corrigir nomes de colunas
  planilhatotal <- read_excel("2025_Nemosia.xlsx", .name_repair = "minimal")

  # Exibir os nomes das colunas para verificação
  print(names(planilhatotal))

  message("Arquivo lido com sucesso!")

} else if (nrow(arquivo) == 0) {
  stop("Erro: Arquivo '2025_Nemosia.xlsx' não encontrado na pasta.")
} else {
  stop("Erro: Mais de um arquivo com o mesmo nome encontrado. Verifique manualmente.")
}
```

Agora vamos filtrar a tabela. Primeiro tirar os dias não amostrados e espécie exótica.
```
p1 <- subset(planilhatotal, !is.na(Dia))
p2 <- subset(p1, !is.na(Mês))
p2 <- subset(p2, !is.na(Ano))
#p2 <- subset(p2, !is.na(Horário))
p2 <- subset(p2, !is.na(Grupo))

p2 <- subset(p2,Contato!="Auditivo")
#p2 <- tidyr::separate_rows(p2, Tipo de contato, sep = "/")

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

prk <- subset(Data, Município == "Castelo/Vargem Alta")
par <- subset(Data, Município == "Santa Teresa") 
ptr <- subset(Data, TURISMO.CIENTIFICO == "Sim") 

```
## Demanda

Um gráfico para tipo de registro:
```
pacman::p_load(ggside, stringr) #, tidyverse,tidyquant)

#p2 <- subset(Data, Projeto == "Infraestrutura")

ggplot(Data, aes(x = Data, y = Grupo)) + 
  geom_point(aes(colour = Município, shape = Contato), alpha = 0.6, size = 6) +  # Adiciona pontos com cor e tamanho
  geom_smooth(method = loess, se = FALSE, aes(colour = Município), alpha = 0.6) +  
  scale_size(range = c(2, 10), name = "Tamanho do grupo observado") +  # Define o intervalo de tamanho dos pontos
  labs(
    title = "Monitoramento", 
    y = "N° de indivíduos observados", 
    x = "Data", 
    caption = "",
    color = "Município", 
    size = "Número de registros",
    shape = "Tipo de contato") +
  theme_minimal() +  # Aplica um tema minimalista
  geom_xsidedensity(aes(y = after_stat(count), fill = Município), alpha = 0.5) +  # Densidade no eixo X
  geom_ysidedensity(aes(x = after_stat(count), fill = Município), alpha = 0.5) +  # Densidade no eixo Y
  theme(
    axis.title = element_text(size = 18),  # Ajusta o tamanho do título dos eixos
    axis.text = element_text(size = 14)    # Ajusta o tamanho do texto dos eixos
  ) + 
  guides(
    color = guide_legend(title = "Município"),  # Corrige o título da legenda de cor
    size = guide_legend(title = "Número de registros")  # Corrige o título da legenda de tamanho
  )
  
  #ggsave("2024_12_atividade_IE_dia.pdf",width = 12, height = 8, dpi = 600)

```
Vamos ver por estação
```
library(dplyr)
library(lubridate)  # Para facilitar o trabalho com datas

# Adicionar a coluna "Estação" ao seu dataframe
Data2 <- Data %>%
  mutate(
    Estação = case_when(
      month(Data) %in% c(12, 1, 2) ~ "Verão",
      month(Data) %in% c(3, 4, 5) ~ "Outono",
      month(Data) %in% c(6, 7, 8) ~ "Inverno",
      month(Data) %in% c(9, 10, 11) ~ "Primavera",
      TRUE ~ NA_character_  # Caso haja datas fora do esperado
    )
  )

library(ggplot2)
library(ggside)

# Plotar o gráfico
ggplot(Data2, aes(x = Estação, y = Grupo)) + 
  geom_boxplot(aes(fill = Município), alpha = 0.6) +  # Boxplots por estação, preenchidos por Município
  geom_jitter(aes(colour = Município), alpha = 0.2, width = 0.2) +  # Adiciona pontos com jitter
  labs(
    title = "Monitoramento por Estação", 
    y = "N° de indivíduos observados", 
    x = "Estação", 
    caption = "",
    color = "Município", 
    fill = "Município",  # Legenda para o preenchimento dos boxplots
    shape = "Tipo de contato"
  ) +
  theme_minimal() +  # Aplica um tema minimalista
  #geom_xsideboxplot(aes(y = Grupo, fill = Município), alpha = 0.5, orientation = "y") +  # Boxplot no eixo X
  geom_ysideboxplot(aes(x = 1, fill = Município), alpha = 0.5, orientation = "x") +  # Boxplot no eixo Y
  theme(
    axis.title = element_text(size = 18),  # Ajusta o tamanho do título dos eixos
    axis.text = element_text(size = 14)    # Ajusta o tamanho do texto dos eixos
  ) + 
  guides(
    color = guide_legend(title = "Município"),  # Corrige o título da legenda de cor
    fill = guide_legend(title = "Município")    # Corrige o título da legenda de preenchimento
  )

```
Agora estações por ano
```
library(dplyr)
library(lubridate)

# Adicionar a coluna "Ano" e "Ano_Estação" ao seu dataframe
Data2 <- Data2 %>%
  mutate(
    Ano = year(Data),  # Extrai o ano da coluna de datas
    Ano_Estação = paste(Ano, Estação, sep = " - ")  # Combina ano e estação
  )
  
library(ggplot2)
library(ggside)

# Plotar o gráfico
ggplot(Data2, aes(x = Ano_Estação, y = Grupo)) + 
  geom_boxplot(aes(fill = Município), alpha = 0.6) +  # Boxplots por Ano_Estação, preenchidos por Município
  geom_jitter(aes(colour = Município), alpha = 0.3, width = 0.2) +  # Adiciona pontos com jitter
  labs(
    title = "Monitoramento por Estação e Ano", 
    y = "N° de indivíduos observados", 
    x = "Ano - Estação", 
    caption = "",
    color = "Município", 
    fill = "Município",  # Legenda para o preenchimento dos boxplots
    shape = "Tipo de contato"
  ) +
  theme_minimal() +  # Aplica um tema minimalista
  geom_ysideboxplot(aes(x = 1, fill = Município), alpha = 0.5, orientation = "x") +  # Boxplot no eixo Y lateral
  theme(
    axis.title = element_text(size = 18),  # Ajusta o tamanho do título dos eixos
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotaciona e ajusta o texto do eixo X
    axis.text.y = element_text(size = 14)    # Ajusta o tamanho do texto do eixo Y
  ) + 
  guides(
    color = guide_legend(title = "Município"),  # Corrige o título da legenda de cor
    fill = guide_legend(title = "Município")    # Corrige o título da legenda de preenchimento
  )

```
Agora horário
```
# Carregando pacotes necessários
pacman::p_load(ggplot2, dplyr)

install.packages("lubridate")  # Instale o pacote se necessário
library(lubridate)

Data <- subset(Data, !is.na(Horário))
Data$Horário <- as.numeric(Data$Horário)

# Verificar se há NAs resultantes da conversão
sum(is.na(Data$Horário)) 

# Função para converter decimal para HH:MM:SS
decimal_para_hora <- function(decimal) {
  horas <- floor(decimal * 24)
  minutos <- floor((decimal * 24 - horas) * 60)
  segundos <- round((((decimal * 24 - horas) * 60) - minutos) * 60)
  sprintf("%02d:%02d:%02d", horas, minutos, segundos)
}

# Aplicar a função à coluna Horário
Data$Horario_convertido <- sapply(Data$Horário, decimal_para_hora)

# Extrair apenas a hora (HH) da coluna Horario_convertido
Data$Hora <- format(as.POSIXct(Data$Horario_convertido, format = "%H:%M:%S"), "%H")

# Contar a frequência de registros por hora e município
horario_counts <- Data %>%
  group_by(Município, Hora) %>%
  summarise(n = n(), .groups = 'drop')

# Criar o gráfico de relógio com facet_grid por Município
ggplot(horario_counts, aes(x = Hora, y = n, fill = n)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # Barras com contorno branco
  coord_polar(start = 0) +  # Coordenadas polares para criar o efeito de relógio
  scale_x_discrete(limits = sprintf("%02d", 0:23)) +  # Definir as 24 horas no eixo x
  labs(
    title = "Distribuição de Registros por Hora e Município",
    x = "Hora do Dia",
    y = "Número de Registros",
    fill = "Registros"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),  # Remover texto do eixo y
    axis.title.y = element_blank(),  # Remover título do eixo y
    panel.grid.major.y = element_blank(),  # Remover linhas de grade principais do eixo y
    panel.grid.minor.y = element_blank(),  # Remover linhas de grade secundárias do eixo y
    axis.text.x = element_text(size = 10, color = "black"),  # Ajustar texto do eixo x
    strip.text = element_text(size = 12, face = "bold")  # Ajustar texto dos facets
  ) +
  geom_text(
    aes(label = ifelse(n > 0, n, "")),  # Adicionar rótulos com o número de registros
    position = position_stack(vjust = 0.5),
    size = 3, color = "black"
  ) +
  facet_grid(~ Município)  # Criar um gráfico separado para cada município
```
teste de normalidade
```

pacman::p_load(fitdistrplus, dunn.test, agricolae)

fit.MF.normal <- fitdist(Data$Grupo, "norm") #gráfico de distribuição normal
plot(fit.MF.normal)

ggplot(Data, aes(sample = Grupo, colour = Município)) +
  stat_qq() +
  stat_qq_line(colour = "black", linetype = "dashed") +
  facet_wrap(~ Município) +  # Separa os gráficos por município
  theme_minimal()

```
Ambos os gráficos atestam a não normalidade
```
     
shapiro.test(prk$Grupo)
shapiro.test(par$Grupo)

```
Com os testes, os dados são estatisticamente comprovados como não normal
```

kruskal.test(Município~Grupo, data = Data)       

```
Esse teste compara se os dados apresentam a mesma distribuição.
Mas o teste de Duun faz os dois e confirma
```
dunn <- dunn.test(Data$Grupo, Data$Município, method = "bonferroni")
print(dunn)
```
Os municípios Castelo/Vargem Alta e Santa Teresa não apresentam 
diferenças estatisticamente significativas na variável x. 
Isso significa que, com base nos dados, 
não há evidências de que esses grupos tenham distribuições diferentes.
Mesmo usando o Bonferroni.
```


g1 <- Data %>% 
  mutate(Município = as.factor(Município)) %>% 
  ggplot() +
  geom_boxplot(aes(y = Grupo, x = Município)) +
  labs(x = 'Municípios', y = 'Nº de registros', 
       title = '') + theme_minimal()
       
g1 

```
Agora uma previsão para o número de regsitros de cada áreas
```
# Carregar pacotes necessários
pacman::p_load(forecast, ggplot2, gridExtra)

# Verificar se o vetor de Grupo tem pelo menos uma observação para prk
if(length(prk$Grupo) > 0) {
  # Transformar o vetor em uma série temporal (ajuste o valor da frequência conforme necessário)
  prk_ts <- ts(prk$Grupo, frequency = 1)  # Frequência diária (1 por dia)
  
  # Criar o modelo ARIMA para 'prk'
  modelo_arima_prk <- auto.arima(prk_ts)
  
  # Gerar previsões para 'prk' para os próximos 10 períodos
  previsao_prk <- forecast(modelo_arima_prk, h = 10)
  
  # Calcular o último período de prk
  ultimo_periodo_prk <- length(prk$Grupo)
  
  # Criar o dataframe com a previsão para prk
  df_prk <- data.frame(
    Municipio = "Castelo/Vargem Alta",
    Periodo = (ultimo_periodo_prk + 1):(ultimo_periodo_prk + 10),  # Ajusta para o período correto
    Previsao = previsao_prk$mean,
    Lo_80 = previsao_prk$lower[,1],
    Hi_80 = previsao_prk$upper[,1],
    Lo_95 = previsao_prk$lower[,2],
    Hi_95 = previsao_prk$upper[,2]
  )
}

# Verificar se o vetor de Grupo tem pelo menos uma observação para par
if(length(par$Grupo) > 0) {
  # Transformar o vetor em uma série temporal (ajuste a frequência conforme necessário)
  par_ts <- ts(par$Grupo, frequency = 1)  # Frequência diária (1 por dia)
  
  # Criar o modelo ARIMA para 'par'
  modelo_arima_par <- auto.arima(par_ts)
  
  # Gerar previsões para 'par' para os próximos 10 períodos
  previsao_par <- forecast(modelo_arima_par, h = 10)
  
  # Calcular o último período de par
  ultimo_periodo_par <- length(par$Grupo)
  
  # Criar o dataframe com a previsão para par
  df_par <- data.frame(
    Municipio = "Santa Teresa",
    Periodo = (ultimo_periodo_par + 1):(ultimo_periodo_par + 10),  # Ajusta para o período correto
    Previsao = previsao_par$mean,
    Lo_80 = previsao_par$lower[,1],
    Hi_80 = previsao_par$upper[,1],
    Lo_95 = previsao_par$lower[,2],
    Hi_95 = previsao_par$upper[,2]
  )
}

# Criar gráficos para prk e par

grafico_prk <- ggplot(df_prk, aes(x = Periodo, group = Municipio)) +
  geom_line(aes(y = Previsao, color = Municipio), size = 1) +  # Linha de previsão
  geom_ribbon(aes(ymin = Lo_80, ymax = Hi_80, fill = Municipio), alpha = 0.2) +  # Intervalo de 80%
  geom_ribbon(aes(ymin = Lo_95, ymax = Hi_95, fill = Municipio), alpha = 0.1) +  # Intervalo de 95%
  labs(title = "Previsões para Castelo/Vargem Alta",
       x = "Período",
       y = "Número de Indivíduos Observados") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  # Coloca a legenda abaixo do gráfico
        legend.box = "horizontal")

grafico_par <- ggplot(df_par, aes(x = Periodo, group = Municipio)) +
  geom_line(aes(y = Previsao, color = Municipio), size = 1) +  # Linha de previsão
  geom_ribbon(aes(ymin = Lo_80, ymax = Hi_80, fill = Municipio), alpha = 0.2) +  # Intervalo de 80%
  geom_ribbon(aes(ymin = Lo_95, ymax = Hi_95, fill = Municipio), alpha = 0.1) +  # Intervalo de 95%
  labs(title = "Previsões para Santa Teresa",
       x = "Período",
       y = "Número de Indivíduos Observados") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  # Coloca a legenda abaixo do gráfico
        legend.box = "horizontal")

# Unir os gráficos com grid.arrange em uma coluna (um sobre o outro)
grid.arrange(grafico_prk, grafico_par, ncol = 1)  # ncol = 1 para exibir os gráficos um sobre o outro

```
vamos pensar agora na contribuição do turismo científico

```

pacman::p_load(ggside, stringr)

library(ggplot2)
library(ggside)

# Criar a variável "Ano_Estação" (se ainda não existir)
Data <- Data %>%
  mutate(
    Ano = year(Data),  # Extrai o ano da coluna de datas
    Estação = case_when(
      month(Data) %in% c(12, 1, 2) ~ "Verão",
      month(Data) %in% c(3, 4, 5) ~ "Outono",
      month(Data) %in% c(6, 7, 8) ~ "Inverno",
      month(Data) %in% c(9, 10, 11) ~ "Primavera",
      TRUE ~ NA_character_
    ),
    Ano_Estação = paste(Ano, Estação, sep = " - ")  # Combina ano e estação
  )

# Plotar o gráfico
ggplot(Data, aes(x = Ano_Estação, y = Grupo)) + 
  geom_boxplot(aes(fill = TURISMO.CIENTIFICO), alpha = 0.6) +  # Boxplots por Ano_Estação, preenchidos por Turismo Científico
  geom_jitter(aes(colour = TURISMO.CIENTIFICO, shape = Contato), alpha = 0.2, width = 0.2) +  # Adiciona pontos com jitter
  labs(
    title = "Monitoramento por Estação e Ano", 
    y = "N° de indivíduos observados", 
    x = "Ano - Estação", 
    caption = "",
    color = "Turismo Científico",  # Título da legenda de cor
    fill = "Turismo Científico",   # Título da legenda de preenchimento
    shape = "Tipo de contato"
  ) +
  theme_minimal() +  # Aplica um tema minimalista
  geom_ysideboxplot(aes(x = 1, fill = TURISMO.CIENTIFICO), alpha = 0.5, orientation = "x") +  # Boxplot no eixo Y lateral
  #geom_xsidedensity(aes(y = after_stat(density), fill = TURISMO.CIENTIFICO), alpha = 0.5) +  # Densidade no eixo X
  #geom_ysidedensity(aes(x = after_stat(density), fill = TURISMO.CIENTIFICO), alpha = 0.5) +  # Densidade no eixo Y
  theme(
    axis.title = element_text(size = 18),  # Ajusta o tamanho do título dos eixos
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotaciona e ajusta o texto do eixo X
    axis.text.y = element_text(size = 14)    # Ajusta o tamanho do texto do eixo Y
  ) + 
  guides(
    color = guide_legend(title = "Turismo Científico"),  # Corrige o título da legenda de cor
    fill = guide_legend(title = "Turismo Científico"),  # Corrige o título da legenda de preenchimento
    shape = guide_legend(title = "Tipo de contato")     # Corrige o título da legenda de forma
  )
```
Em um gráfico de pizza

```
pacman::p_load(ggplot2, dplyr)

# Substituir NA por "Monitoramento" na coluna TURISMO.CIENTIFICO
df_pizza <- Data %>%
  mutate(TURISMO.CIENTIFICO = ifelse(is.na(TURISMO.CIENTIFICO), "Monitoramento", TURISMO.CIENTIFICO)) %>%
  count(TURISMO.CIENTIFICO) %>%
  mutate(percentage = n / sum(n) * 100)  # Calculando a porcentagem de cada categoria

# Criar o gráfico de pizza com porcentagem
ggplot(df_pizza, aes(x = "", y = percentage, fill = TURISMO.CIENTIFICO)) + 
  geom_bar(stat = "identity", width = 1, color = "white") +  # Barra de pizza
  coord_polar(theta = "y") +  # Convertendo a barra para o formato circular
  labs(title = "Distribuição do Turismo Científico") +
  theme_void() +  # Remove elementos desnecessários
  theme(
    axis.text.x = element_blank(),  # Remove os rótulos do eixo X
    legend.title = element_blank()  # Remove o título da legenda
  ) +
  scale_fill_manual(values = c("blue", "orange", "green")) +  # Personalize as cores para as categorias
  geom_text(aes(label = paste0(round(percentage, 1), "%")),  # Adiciona as porcentagens
            position = position_stack(vjust = 0.5),  # Posiciona o texto no meio da fatia
            color = "white", size = 6)  # Define a cor e o tamanho do texto
