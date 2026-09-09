################# Ejercicios para la evalaución de imputación simple ################# 
# La simulación de los NA fue creada con ayuda de Claude 

################# Brithwt ################# 
### Datos originales
library(MASS)

data("birthwt")

datos_originales <- birthwt
### Datos con NA
set.seed(2026)

datos_faltantes <- datos_originales

n_faltantes <- round(0.15 * nrow(datos_faltantes))

filas_faltantes <- sample(
  seq_len(nrow(datos_faltantes)),
  size = n_faltantes
)

datos_faltantes$lwt[filas_faltantes] <- NA

mean(is.na(datos_faltantes$lwt))

### Imputar co la media
datos_media <- datos_faltantes

media_observada <- mean(
  datos_media$lwt,
  na.rm = TRUE
)

datos_media$lwt[
  is.na(datos_media$lwt)
] <- media_observada

### Imputar con la mediana
datos_mediana <- datos_faltantes

mediana_observada <- median(
  datos_mediana$lwt,
  na.rm = TRUE
)

datos_mediana$lwt[
  is.na(datos_mediana$lwt)
] <- mediana_observada

## Realice una evaluación del antes y el despues

## Realice evaluacion del error
verdaderos <- datos_originales$lwt[filas_faltantes]

imputados_media <- datos_media$lwt[filas_faltantes]
imputados_mediana <- datos_mediana$lwt[filas_faltantes]

MAE_media <- mean(abs(verdaderos - imputados_media))
MAE_mediana <- mean(abs(verdaderos - imputados_mediana))

RMSE_media <- sqrt(
  mean((verdaderos - imputados_media)^2)
)

RMSE_mediana <- sqrt(
  mean((verdaderos - imputados_mediana)^2)
)

data.frame(
  metodo = c("Media", "Mediana"),
  MAE = c(MAE_media, MAE_mediana),
  RMSE = c(RMSE_media, RMSE_mediana)
)

### Impacto sobre una regresión 

modelo_original <- lm(
  bwt ~ lwt + age + smoke,
  data = datos_originales
)

modelo_completo <- lm(
  bwt ~ lwt + age + smoke,
  data = datos_faltantes
)

modelo_media <- lm(
  bwt ~ lwt + age + smoke,
  data = datos_media
)

modelo_mediana <- lm(
  bwt ~ lwt + age + smoke,
  data = datos_mediana
)


