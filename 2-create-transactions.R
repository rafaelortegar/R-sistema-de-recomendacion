# Mayo 2020
# Script de creación de transacciones 
# Este código toma los archivos limpios de géneros, elenco, equipo y palabras clave y crea transacciones en formato listo para market basket analysis.


library(tidyverse)

elenco <- read_csv("data/clean/elenco.csv",col_names = TRUE,na = "NA")
equipo <- read_csv("data/clean/equipo.csv",col_names = TRUE,na = "NA")
generos <- read_csv("data/clean/generos.csv",col_names = TRUE,na = "NA")
palabras_clave <- read_csv("data/clean/palabras_clave.csv",col_names = TRUE,na = "NA")


#Transacciones para géneros de películas
df_generos <- generos %>% 
  select(title, genres) %>%
  rename(id = title, value = genres)

transacciones_generos <- NULL
for (g in unique(df_generos$value)) {
  tags = df_generos %>% 
    filter(value == g) %>% 
    select(id) %>%
    summarise(tags = paste(id, collapse = ","))
  transacciones_generos <- rbind(transacciones_generos, tags)
}


#Transacciones para palabras clave
df_palabras_clave <- palabras_clave %>% 
  group_by(keyword) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  select(title, keyword) %>% 
  rename(id = title, value = keyword)

transacciones_palabras_clave <- NULL
for (kw in unique(df_palabras_clave$value)) {
  tags = df_palabras_clave %>% 
    filter(value == kw) %>% 
    select(id) %>%
    summarise(tags = paste(id, collapse = ","))
  transacciones_palabras_clave <- rbind(transacciones_palabras_clave, tags)
}


#Transacciones para el elenco
df_elenco <- elenco %>%
  filter(pos < 5) %>%
  select(title, cast) %>%
  rename(id = title, value = cast)

transacciones_elenco <- NULL
for (e in unique(df_elenco$value)) {
  tags = df_elenco %>% 
    filter(value == e) %>% 
    select(id) %>%
    summarise(tags = paste(id, collapse = ","))
  transacciones_elenco <- rbind(transacciones_elenco, tags)
}



#Transacciones para el equipo de la pelicula (crew)
df_equipo <- equipo %>%
  filter(job %in% c("Director","Screenplay", "Writer")) %>% 
  select(title, crew) %>%
  rename(id = title, value = crew)

transacciones_equipo <- NULL
for (e in unique(df_equipo$value)) {
  tags = df_equipo %>% 
    filter(value == e) %>% 
    select(id) %>%
    summarise(tags = paste(id, collapse = ","))
  transacciones_equipo <- rbind(transacciones_equipo, tags)
}


transacciones <- rbind(transacciones_generos, transacciones_generos)
transacciones <- rbind(transacciones, transacciones_elenco)
transacciones <- rbind(transacciones, transacciones_equipo)


write.csv(transacciones, file = "transacciones.csv", row.names = FALSE, col.names = FALSE, quote = FALSE)
