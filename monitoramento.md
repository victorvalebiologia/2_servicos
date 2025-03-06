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
p2 <- subset(p2, !is.na(Horário))
p2 <- subset(p2, !is.na(Grupo))


#p2 <- subset(p2, Ano == "2024")
#p2 <- subset(p2, Mês == "12") 

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
```
## Demanda

Um gráfico para tipo de registro:
```
#p2 <- subset(Data, Projeto == "Infraestrutura")

ggplot(Data, aes(x = Data, y = Grupo)) + 
  geom_line(aes(colour = Município), alpha = 0.6)+ 
  scale_size(range = c(3, 17), name = "Número de registros") +
  #facet_grid(Projeto~., scales = "free_y", space = "free_y") + 
  labs(title="Monitoramento", y="N° de indivíduos",x="Data", caption="",
       color = "Município") +
  theme_minimal() +         
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + 
  guides(color = guide_legend(title = "Responsável"), 
       size = guide_legend(title = "Minutos gastos"))
       
#ggsave("2024_12_atividade_IE_dia.pdf",width = 12, height = 8, dpi = 600)
```
teste de normalidade

pacman::p_load(fitdistrplus)

fit.MF.normal <- fitdist(p2$Grupo, "norm") #gráfico de distribuição normal
plot(fit.MF.normal)

ggplot(p2, aes(sample=Grupo, colour = Município)) +
     stat_qq() + 
     theme_bw()
     
shapiro.test(p2$Grupo)

