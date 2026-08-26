## Exploracion 
library(NHANES)
data("NHANES")
datos <- df

library(tidyverse)

head(df)
summary(df)
dim(df)
names(df)
glimpse(df)

########### rename() ########### 
# nuevo_nombre = nombre_anterior

df_limpio <- df %>%
  rename(
    edad = Age,
    presion_sistolica = BPSysAve,
    presion_diastolica = BPDiaAve,
    IMC = BMI
  )

# Verificar
head(df_limpio %>% select(edad, presion_sistolica, presion_diastolica, IMC))


########### rename_with() ########### 



########### filter() ########### 

hipertensos <- NHANES_limpio %>%
  filter(presion_sistolica > 140)

nrow(hipertensos)

# Múltiples condiciones
diabeticos_adultos <- NHANES_limpio %>%
  filter(edad >= 50, Diabetes == "Yes")

nrow(diabeticos_adultos)
head(diabeticos_adultos)


riesgo_CV <- NHANES_limpio %>%
  filter(presion_sistolica > 140 | Diabetes == "Yes")

nrow(riesgo_CV)


df |> 
  filter(is.na(BMI)) |> 
  nrow()

df_no_NA <-  df |> 
  filter(!is.na(BMI))
head(df_no_NA)

# Reto

df |> 
  filter(  Age >= 40,
           Age < 65,
           BMI >= 30,
           !is.na(BPSysAve),
           BPSysAve >= 140) |> 
  count(Gender)

########### slice_head() ########### 
#- Seleccione una muestra de los primero 50 pacientes y guardelos en una base de datos nueva
df |> 
  slice_head(n=50)-> df_50

#- Seleccione los 20 pacientes con el IMC mas bajo.

df |> 
  arrange(BMI) |> 
  slice_head(n = 20) |> 
  select(ID, BMI)
#- Seleccione los 20 pacientes con el IMC mas alto. 

df |> 
  arrange(desc(BMI)) |> 
  slice_head(n = 20)|> 
  select(ID, BMI)

#- Seleccione a los hombres mayores de 40 años y luego identifique cuales son los 10 pacientes con el IMC más bajo. 

df |> 
  filter(Gender == "male" & Age>40) |> 
  arrange(BMI) |> 
  slice_head(n =10) |> 
  select(ID, Gender, BMI)

########### mutate() ########### 



########### if_else() ########### 


df |>
  mutate(
    GrupoEdad = if_else(
      Age >= 18,
      "Adulto",
      "Menor"
    )
  )

df <-  df |> 
  mutate(NuevoGrupo =
  ifelse(
    Gender == "female" & Age >25, 
    "Grupo A", 
    "Grupo B"
  )) 

# Pacientes con T-LGLL O con RA asociada
dplyr::if_else(
  diagnostico == "T-LGLL" | diagnostico == "RA",
  "Sí",
  "No"
)

########### case_when() ########### 

df |>
  mutate(
    CategoriaBMI = case_when(
      is.na(BMI) ~ "Sin información",
      BMI < 18.5 ~ "Bajo peso",
      BMI < 25 ~ "Peso esperado",
      BMI < 30 ~ "Sobrepeso",
      BMI >= 30 ~ "Obesidad"
    )
  )


########### transmute() ########### 

df_obesidad <- df |>
  transmute(
    Age,
    Sex,
    BMI,
    GrupoEdad = case_when(
      Age < 18 ~ "Menor de 18",
      Age < 65 ~ "18 a 64",
      Age >= 65 ~ "65 o más"
    ),
    Obesidad = case_when(
      BMI >= 30 ~ "Sí",
      BMI < 30 ~ "No"
    )
  )


###### problema 2

df_presion <- df |>
  transmute(
    Age,
    Sex,
    BPSysAve,
    BPDiaAve,
    PAM = (BPSysAve + 2 * BPDiaAve) / 3,
    Hipertension = case_when(
      BPSysAve >= 140 | BPDiaAve >= 90 ~ "Sí",
      TRUE ~ "No" # Para evitar poner toda la condicion
    )
  )

########### group_by() ########### 

df |>
  group_by(Sex) |>
  summarise(
    BMI_promedio = mean(BMI, na.rm = TRUE)
  )

df |>
  mutate(
    GrupoEdad = case_when(
      Age < 18 ~ "Menor de 18",
      Age < 65 ~ "18 a 64",
      TRUE ~ "65 o más"
    )
  ) |>
  group_by(GrupoEdad) |>
  summarise(
    n = n(),
    Edad_promedio = mean(Age, na.rm = TRUE)
  )

# Problema 3. 

# Queremos explorar si el BMI y la presión arterial sistólica difieren según el sexo y el grupo de edad. Además, queremos calcular la proporción de participantes con obesidad dentro de cada grupo.

df |>
  mutate(
    GrupoEdad = case_when(
      Age < 18 ~ "Menor de 18",
      Age < 65 ~ "18 a 64",
      TRUE ~ "65 o más"
    ),
    Obesidad = BMI >= 30
  ) |>
  group_by(Sex, GrupoEdad) |>
  summarise(
    n = n(),
    BMI_promedio = mean(BMI, na.rm = TRUE),
    BMI_mediana = median(BMI, na.rm = TRUE),
    PAS_promedio = mean(BPSysAve, na.rm = TRUE),
    Obesidad_pct = mean(Obesidad, na.rm = TRUE) * 100,
    .groups = "drop"
  )




df |>
  mutate(
    GrupoEdad = case_when(
      Age < 18 ~ "Menor de 18",
      Age < 65 ~ "18 a 64",
      TRUE ~ "65 o más"
    ),
    Obesidad = BMI >= 30
  ) |>
  group_by(Gender, GrupoEdad) |>
  summarise(
    n = n(),
    BMI_promedio = mean(BMI, na.rm = TRUE),
    BMI_mediana = median(BMI, na.rm = TRUE),
    PAS_promedio = mean(BPSysAve, na.rm = TRUE),
    Obesidad_pct = mean(Obesidad, na.rm = TRUE) * 100,
    .groups = "drop"
  )



## Problema

df |> 
  mutate(
    GrupoEdad = # Varible 1
      case_when(
        Age < 18 ~ "Menos de 18",
        Age <65 ~ "Edad 2", 
        TRUE ~ "Mas de 65"),
    Obesidad = BMI >= 30) |> # Variable 2 solo responde si  y no
  group_by(Gender, GrupoEdad) |> 
  summarise(
    n = n(), 
    "Promedio BMI" = mean(BMI, na.rm= T), 
    "Mediana BMI" = median(BMI, na.rm = T),
    "Promedio presion" = mean(BPSysAve, na.rm = T),
    "Porcentaje obesidad" = mean(Obesidad, na.rm = T)*100, 
.groups = "drop") # desgrupa



df |>
  mutate(
    GrupoEdad = case_when(
      Age < 18 ~ "Menor de 18",
      Age < 65 ~ "18 a 64",
      TRUE ~ "65 o más"
    ),
    Obesidad = case_when(
      BMI >= 30 ~ "Sí",
      BMI < 30 ~ "No"
    )
  ) |>
  group_by(Gender, GrupoEdad) |>
  summarise(
    n = n(),
    BMI_promedio = mean(BMI, na.rm = TRUE),
    BMI_mediana = median(BMI, na.rm = TRUE),
    PAS_promedio = mean(BPSysAve, na.rm = TRUE),
    Obesidad_pct = sum(Obesidad == "Sí", na.rm = TRUE) /
      sum(!is.na(Obesidad)) * 100,
    .groups = "drop"
  )

########### summarise() ########### 

df |>
  mutate(
    GrupoEdad = case_when(
      Age < 18 ~ "Menor de 18",
      Age < 65 ~ "18 a 64",
      TRUE ~ "65 o más"
    )
  ) |>
  group_by(GrupoEdad) |>
  summarise(
    n = n(),
    Colesterol_promedio = mean(TotChol, na.rm = TRUE),
    Colesterol_DE = sd(TotChol, na.rm = TRUE),
    Fumadores = sum(SmokeNow == "Yes", na.rm = TRUE),
    Fumadores_pct = sum(SmokeNow == "Yes", na.rm = TRUE) /
      sum(!is.na(SmokeNow)) * 100,
    .groups = "drop"
  )

########### across() ########### 