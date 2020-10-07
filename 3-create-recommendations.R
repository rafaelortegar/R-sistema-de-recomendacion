# Mayo 2020
# Script de creación de recomendaciones 
# Este script ejecuta el algoritmo apriori para crear las recomendaciones.

# IMPORTANTE: se debe ejecutar desde consola con RScript y mandar como argumento la película a analizar.
# Ejemplo:

# $ Rscript 3-create-recommendations.R "The Social Network"


library(Matrix)
library(arules)

args = commandArgs(trailingOnly = TRUE)

#leer archivo de transacciones
transacciones <- read.transactions("transacciones.csv", format = "basket", sep = ",", header = TRUE, cols = NULL, rm.duplicates = TRUE, encoding = 'utf-8')

#función que recibe objeto de `rules` y regresa un dataframe
create_dataframe_from_rules <- function(r){
  df = data.frame(
    lhs = labels(lhs(r)),
    rhs = labels(rhs(r)), 
    r@quality
  )
  return(df)
}

peli <- args[1] # si quieres hacer una predicción desde el script, aqui substituye "nombrepelicula" por el args[1], por ejemplo:
#peli <- "Avatar" #args[1] # si quieres hacer una predicción desde el script, aqui substituye "nombrepelicula" por el args[1]


pars <- list(supp = 0.0001, conf = 0.20, maxlen = 2, minlen = 2)

#algoritmo a priori
rules <- apriori(data = transacciones, parameter = pars, appearance = list(default = "rhs",lhs = peli),control = list(verbose = F))
rules <- sort(rules, decreasing = TRUE, by = "confidence")

#creamos el data frame con los resultados
df_results <- create_dataframe_from_rules(rules)

print(head(df_results))
