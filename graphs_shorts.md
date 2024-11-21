## Apresentação
Gráficos rápidos

getwd()
setwd("/home/valev/Área de Trabalho/R/PCSA/gráficos")
# Instalar o pacote ggplot2 e reshape2 se ainda não estiverem instalados
# install.packages("ggplot2")
# install.packages("reshape2")

# Instalar o pacote ggplot2 se ainda não estiver instalado
# install.packages("ggplot2")

# Carregar a biblioteca ggplot2
library(ggplot2)

# Criar a tabela de dados
dados <- data.frame(
  Ano = c(1870, 1941, 1995, 1998, 2003, 2016, 2020, 2021, 2022),
  PCSA = c(0, 0, 0, 0, 0, 0, 12, 17, 22),
  HISTORICAL = c(0, 8, 1, 4, 12, 3, 0, 0, 0),  # Alterado 1870 para 0 em "HISTORICAL"
  DEAD = c(1, 0, 0, 0, 0, 0, 0, 0, 0)  # Adicionado 1 para o ano de 1870 em "DEAD"
)

# Converter a tabela para o formato long (long format)
dados_long <- reshape2::melt(dados, id.vars = "Ano", variable.name = "Categoria", value.name = "Valor")

# Criar o gráfico com ggplot2
ggplot(dados_long, aes(x = factor(Ano), y = Valor, fill = Categoria)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("PCSA" = "#1f77b4", "HISTORICAL" = "#d62728", "DEAD" = "#7f7f7f")) + 
  labs(title = "Historical Population Increase",
       x = "Year",
       y = "Population",
       fill = "Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.text = element_text(size = 12)  # Aumente o tamanho da legenda aqui
  )

ggsave(width = 20, height = 10, device = "pdf", filename = "barra")

