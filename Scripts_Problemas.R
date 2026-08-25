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


########### case_when() ########### 


########### transmute() ########### 


########### group_by() ########### 


########### summarise() ########### 

########### across() ########### 