library(tidyverse)
library(readxl)

# Importa os arquivos base da SSP e do GAESP --------------------------------------------------

mdip_ssp <- read_excel("data-raw/MDIP_2024.xlsx", 
                      col_types = c("text", "text", "text", "text", "text", "text", "text", 
                                    "numeric", "numeric", "date", "text", "numeric","text", 
                                    "text", "text", "text", "date","date", "text", "text", 
                                    "text", "numeric", "numeric", "text", "text", "numeric", 
                                    "date", "text", "text", "text")) |> 
  janitor::clean_names() 

mdip_gaesp <-  read_excel("data-raw/MDIP_MPSP_01-01-2017_06-03-2024.xlsx", 
                          col_types = c("text", "text", "date", "date", "text", "text", "text", 
                                        "text", "text", "text", "text", "numeric", "text", 
                                        "numeric", "text", "text", "numeric", "text", "text", 
                                        "numeric", "text", "text"))|> 
  select(-(17:22)) |> 
  janitor::clean_names() 

# Arruma algumas colunas ----------------------------------------------------------------------

#cria a coluna ano na base mdip_gaesp
mdip_gaesp <- mdip_gaesp |> mutate(ano = str_sub(data, end = 4))

#limpa na base mdip_gaesp
mdip_gaesp <- mdip_gaesp |> mutate(hora = str_sub(hora,  start = 12))

#limpa hora na base mdip_ssp
mdip_ssp <- mdip_ssp |> mutate(hora_fato = str_sub(hora_fato,  start = 12))

#padroniza as variáveis de locais 
mdip_ssp$municipio_circunscricao <- str_to_lower(mdip_ssp$municipio_circunscricao)
mdip_ssp$logradouro <- str_to_lower(mdip_ssp$logradouro)
mdip_ssp$profissao <- str_to_lower(mdip_ssp$profissao)
mdip_ssp$municipio_elaboracao <- str_to_lower(mdip_ssp$municipio_elaboracao)
mdip_gaesp$cidade <- str_to_lower(mdip_gaesp$cidade)
mdip_gaesp$estado <- str_to_lower(mdip_gaesp$estado)
mdip_gaesp$bairro <- str_to_lower(mdip_gaesp$bairro)

# limpar coorporação na base do MP (que vem com dados de GCM, PF, etc)

mdip_gaesp <- mdip_gaesp |> 
  mutate(forca_limpo = case_when
         (forca %in% c("Polícia Militar", "Policia Militar", "Polícia Militr",
                       "Polícia Militar-GCM", "Polícia Militar-Polícia Penal") ~ "PM",
           forca %in% c("Polícia Civil", "Polícia Civil - GCM", "Polícia Civil-GCM",
                        "Polícia Civil") ~ "PC",
           forca %in% c("Polícia Civil-Polícia Militar", "Polícia Militar-Polícia Civil") ~ "PM-PC",
           TRUE ~ as.character(forca))) |> 
  #seleciona apenas as ocorrências da PM e PC
  filter((forca_limpo %in% c("PM", "PC", "PM-PC")))

saveRDS(mdip_ssp, "data-raw/mdip_ssp.rds")
saveRDS(mdip_gaesp, "data-raw/mdip_gaesp.rds")

# Seleciona periodo e recorte territorial -----------------------------------------------------


