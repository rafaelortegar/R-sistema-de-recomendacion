"""
Mayo 2020
Script de limpieza de datos. 
Este código toma los archivos resumidos de TMBDb (joins por id de película) y crea archivos limpios que alimentarán al script de creación de transacciones.
Lo más relevante del código es el 'desempaque' y normalización de jsons
"""

library(tidyverse)
library(jsonlite)

peliculas = read_csv("data/movies.csv",col_names = TRUE,na = "NA")
creditos = read_csv("data/credits.csv",col_names = TRUE,na = "NA")

#Los jsons están guardados con comillas singulares en lugar de dobles. Esto causará un error y las tenemos que cambiar.
peliculas <- peliculas %>% mutate(title = gsub("'", "",(peliculas %>% select(title))$title))
creditos <- creditos %>% mutate(title = gsub("'", "",(creditos %>% select(title))$title))

#cambiar nombre de id para poder hacer el unnest
peliculas_2 <- peliculas
names(peliculas_2)
names(peliculas_2)[4] <- "identificador"

#limpar y normalizar el json de géneros de cada película
generos <- peliculas_2 %>%  
  filter(nchar(genres) > 2) %>%
  mutate(objeto_json = lapply(genres,fromJSON)) %>% 
  unnest(objeto_json) %>% 
  select(identificador,title,genres=name) %>% 
  group_by(title) %>% 
  mutate(pos = 1:n()) %>%
  ungroup()

#limpar y normalizar el json de las palabras clave de cada película
palabras_clave <- peliculas_2 %>% 
  filter(nchar(keywords) > 2) %>% 
  mutate(objeto_json = lapply(keywords,fromJSON)) %>% 
  unnest(objeto_json) %>% 
  select(identificador,title,keyword = name) %>% 
  group_by(title) %>% 
  mutate(pos = 1:n()) %>%
  ungroup()

#limpar y normalizar el json del elenco de cada película
elenco <- creditos %>% 
  filter(nchar(cast) > 2) %>% 
  mutate(objeto_json = lapply(cast,fromJSON)) %>% 
  unnest(objeto_json) %>% 
  select(movie_id,title,cast = name) %>%
  group_by(title) %>% 
  mutate(pos = 1:n()) %>% 
  ungroup()

#limpar y normalizar el json del equipo de producción de cada película
equipo <- creditos %>%
  filter(nchar(crew) > 2) %>%
  mutate(objeto_json = lapply(crew, fromJSON)) %>%
  unnest(objeto_json) %>%
  select(-cast, -crew, -credit_id) %>%
  rename(crew = name, crew_id = id) %>%
  mutate_if(is.character, factor) 

#Para ser conosistentes entre diferentes archivos, el ID de la película deberá llamarse id y no movie_id ni identificador
# provenientes de peliculas
generos = generos %>% rename(id = identificador)
palabras_clave = palabras_clave %>% rename(id = identificador)
# provenientes de creditos
elenco = elenco %>% rename(id = movie_id)
equipo = equipo %>% rename(id = movie_id)

#guardamos en data/clean para que los pueda leer el script de transacciones
write.csv(generos, file = "data/clean/generos.csv", row.names = FALSE)
write.csv(palabras_clave, file = "data/clean/palabras_clave.csv", row.names = FALSE)
write.csv(elenco, file = "data/clean/elenco.csv", row.names = FALSE)
write.csv(equipo, file = "data/clean/equipo.csv", row.names = FALSE)
