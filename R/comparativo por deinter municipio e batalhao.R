library(tidyverse)


# Filtrar a base com período específico -------------------------------------------------------

# Definir período
anos <- c(2023, 2024)
meses <- c(1,2)

# Filtrar base
mdip_ssp_recorte <- mdip_ssp |> 
  filter(ano_estatistica %in% anos &
           mes_estatistica %in% meses)


# Comparação por deinter ----------------------------------------------------------------------

# Pré-agrupar os dados por departamento e calcular a contagem de ocorrências
agrupado <- mdip_ssp_recorte |> 
  group_by(departamento_circunscricao, ano_estatistica) |>
  summarise(count = n()) |>
  arrange(departamento_circunscricao, desc(count))

# Definir cores e tema
cores <- c("2023" = "#1f78b4", "2024" = "#e31a1c")
tema <- theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1),
        legend.position = "top")

# Plotar o gráfico
ggplot(agrupado, aes(x = reorder(departamento_circunscricao, count), y = count, fill = factor(ano_estatistica))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "white") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = 0.25, hjust = -0.15) +
  labs(title = "Número de ocorrências de MDIP por Departamento - janeiro e fevereiro de 2023 e 2024",
       x = "Região",
       y = "Número de ocorrências") +
  scale_fill_manual(values = cores) +
  guides(fill = guide_legend(title = "Ano")) +
  coord_flip() +
  tema

  
  



