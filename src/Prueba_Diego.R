library(readxl)
library(dplyr)
A <- read_excel("C:/Users/Diego/Desktop/PruebaJEP/Prueba_Tecnica/input/A.xlsx")
B <- read_excel("C:/Users/Diego/Desktop/PruebaJEP/Prueba_Tecnica/input/B.xlsx")
A$FUENTE <- "A"
B$FUENTE <- "B"

# Creamos una función para comparar si los nombres de los campos de ambas tablas son iguales

comparar_columnas <- function(tabla1, tabla2) {
  columnas1 <- colnames(tabla1)
  columnas2 <- colnames(tabla2)
  iguales <- all(columnas1 == columnas2)
  if (iguales) {
    return("Son iguales")
  } 
  else {
    return("No son iguales")
  }
}

comparar_columnas(A, B)


# Creo una función para crear un codigo unico por registro por trasavilidad

library(digest)

generar_codigo_unico <- function(registro) {
  
  concatenado <- paste(registro, collapse = "")  
  codigo_unico <- digest(concatenado, algo = "sha256")
  
  return(codigo_unico)
} 

A$CODIGO_UNICO <- apply(A, 1, generar_codigo_unico)
B$CODIGO_UNICO <- apply(B, 1, generar_codigo_unico)


#Creo una función para quitar espacios en blanco si la columna es de tipo caracter

quitar_espacios <- function(tabla) {
  tabla_limpia <- tabla
  
  tabla_limpia[] <- lapply(tabla_limpia, function(columna) {
    if (is.character(columna)) {
      return(trimws(columna))  
    } else {
      return(columna)  
    }
  })
  return(tabla_limpia)
}

A<- quitar_espacios (A)

B<- quitar_espacios (B)

#Unifico las dos Tablas

tabla_consolidada <- bind_rows(A, B)

# Creo identificador unico basado en las columnas que considere relevantes DADO QUE EL HECHO ES HOMICIDIO
tabla_consolidada<- tabla_consolidada %>%
  mutate(identificador = paste(NOMBRE1, NOMBRE2,APELLIDO1,APELLIDO2,NUMERO_DOCUMENTO,SEXO, sep = ""))

unique(tabla_consolidada$identificador)

# Creamos una tabla para saber cuales son los homicidios unicos por cada fuente y cuales son los compartidos

identificadores_A <- tabla_consolidada %>% filter(FUENTE == "A") %>% pull(identificador)
identificadores_B <- tabla_consolidada %>% filter(FUENTE == "B") %>% pull(identificador)

# Identificadores únicos de A
unicos_A <- setdiff(identificadores_A, identificadores_B)

# Identificadores únicos de B
unicos_B <- setdiff(identificadores_B, identificadores_A)

# Identificadores compartidos
compartidos <- intersect(identificadores_A, identificadores_B)


tabla_resumen <- data.frame(
  Categoria=c("Unicos_Fuente_A","Unicos_Fuente_B","Compartidos_AyB"),
  valor=c(length(unicos_A),length(unicos_B),length(compartidos)))

# Mostrar la tabla 
print(tabla_resumen)
write.csv(tabla_resumen,file ="/Users/Diego/Desktop/PruebaJEP/Prueba_Tecnica/output/tablaresumen.csv")

#Creo una gráfica que muestre los registros compartidos y los unicos por Fuente

library(ggplot2)

graficaresumen<-ggplot(tabla_resumen, aes(x = Categoria, y = valor)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue", color = "black") +
  labs(title = "Gráfico de Barras: Promedio de valor por categoría",
       x = "Categoría",
       y = "Valor Promedio") +
  theme_minimal()
plot(graficaresumen)
ggsave("/Users/Diego/Desktop/PruebaJEP/Prueba_Tecnica/output/graficaresumen.png",plot = graficaresumen,width = 6,height = 4)


library(rmarkdown)

# Especificar el archivo .Rmd
input_file <- "/Users/Diego/Desktop/PruebaJEP/Prueba_Tecnica/src/Prueba_Diego.RMD"

# Especificar la ruta donde guardar el PDF
output_file <- "/Users/Diego/Desktop/PruebaJEP/Prueba_Tecnica/output/Prueba_Diego.pdf"

# Generar el PDF
render(input = input_file, output_file = output_file)

