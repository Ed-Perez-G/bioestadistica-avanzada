# =============================================================
# Modelos lineales — Parte 3: Interacciones numérica × numérica
# Soluciones de los ejercicios 6 y 7
# =============================================================
library(ggplot2)
theme_set(theme_minimal(base_size = 13))

# -------------------------------------------------------------
# Ejercicio 6 — Pima: pliegue cutáneo × edad sobre el IMC
# -------------------------------------------------------------
data(Pima.tr, package = "MASS")
data(Pima.te, package = "MASS")
pima <- rbind(Pima.tr, Pima.te)

# a) Modelos sin y con interacción
m_sin <- lm(bmi ~ skin + age, data = pima)
m_con <- lm(bmi ~ skin * age, data = pima)
summary(m_con)
anova(m_sin, m_con)                      # F = 37.9, p < 0.001
c(sin = summary(m_sin)$adj.r.squared,    # 0.418
  con = summary(m_con)$adj.r.squared)    # 0.456

# b) Interpretación (modelo sin centrar):
#  (Intercept) = 11.6 : IMC esperado con pliegue 0 mm y edad 0 años (sin sentido)
#  skin        = 0.755: kg/m² por mm de pliegue A LOS 0 AÑOS (extrapolación)
#  age         = 0.275: kg/m² por año CON PLIEGUE 0 mm (extrapolación; NO es "el efecto de la edad")
#  skin:age    = -0.010: por cada año de edad, la pendiente del pliegue baja 0.010 kg/m² por mm

# c) Pendientes simples del pliegue
b <- coef(m_con)
edades <- c(22, 30, 45)
b["skin"] + b["skin:age"] * edades       # 0.535, 0.455, 0.305

# ... con IC 95 % (recentrar la edad en cada valor de interés)
t(sapply(edades, function(a) {
  pima$age_a <- pima$age - a
  m <- lm(bmi ~ skin * age_a, data = pima)
  c(edad = a, pendiente = coef(m)["skin"], confint(m)["skin", ])
})) |> round(3)
# 22 años: 0.535 (0.481–0.589); 30: 0.455 (0.412–0.498); 45: 0.305 (0.248–0.362)

# d) Centrado
pima$skin_c <- pima$skin - mean(pima$skin)   # 29.2 mm
pima$age_c  <- pima$age  - mean(pima$age)    # 31.6 años
m_cen <- lm(bmi ~ skin_c * age_c, data = pima)
summary(m_cen)
# age_c = -0.017 (p = 0.42): con pliegue PROMEDIO la edad no se asocia con el IMC.
# El 0.275 "significativo" del modelo sin centrar era un artefacto de evaluarlo en skin = 0.
# skin_c = 0.439: pendiente del pliegue a la edad promedio.
# beta3 = -0.010, su EE y su p: idénticos.

# e) Edad en que la pendiente del pliegue sería 0
-b["skin"] / b["skin:age"]               # ~75 años
# Hay muy pocas participantes > 70 años (máximo 81): es una extrapolación.
# La conclusión válida es que la asociación se debilita con la edad, no que desaparece a los 75.

# f) Gráfica de pendientes simples
m_a <- mean(pima$age); s_a <- sd(pima$age)
rej <- expand.grid(skin = seq(7, 60, 1),
                   age  = round(c(m_a - s_a, m_a, m_a + s_a)))
rej$pred <- predict(m_con, newdata = rej)

ggplot(rej, aes(skin, pred, colour = factor(age))) +
  geom_point(data = pima, aes(skin, bmi), colour = "grey70", alpha = 0.5) +
  geom_line(linewidth = 1.2) +
  coord_cartesian(xlim = c(7, 60)) +
  labs(x = "Pliegue del tríceps (mm)", y = "IMC predicho (kg/m²)",
       colour = "Edad (años)")

# Redacción (ejemplo):
# "La asociación entre el pliegue del tríceps y el IMC disminuyó con la edad
#  (beta3 = -0.010 kg/m² por mm por año; IC 95 % -0.013 a -0.007; p < 0.001).
#  A los 22 años, cada mm adicional de pliegue se asoció con 0.54 kg/m² más de IMC
#  (IC 95 % 0.48–0.59), y a los 45 años con 0.31 kg/m² (IC 95 % 0.25–0.36)."
# Posible explicación biológica: con la edad la grasa se redistribuye hacia el
# compartimento visceral, y el pliegue de una extremidad refleja peor la adiposidad total.

# -------------------------------------------------------------
# Ejercicio 7 — G×E: alelos de riesgo × actividad física
# -------------------------------------------------------------
set.seed(6)
n      <- 800
alelos <- rbinom(n, size = 20, prob = 0.5)
act    <- round(pmax(0, rnorm(n, 15, 6)), 1)
imc    <- 26 + 0.6*(alelos - 10) - 0.10*(act - 15) -
          0.025*(alelos - 10)*(act - 15) + rnorm(n, 0, 2.5)
gxe    <- data.frame(imc = round(imc, 1), alelos, act)

# a) alelos (+), actividad (-): efectos simples con signos opuestos -> AMORTIGUADORA
#    (beta3 negativo: la actividad física atenúa el efecto genético)

# b) Modelos
g_sin <- lm(imc ~ alelos + act, data = gxe)
g_con <- lm(imc ~ alelos * act, data = gxe)
summary(g_con)
anova(g_sin, g_con)          # F = 13.7, p < 0.001
# act = +0.159 (p = 0.023) NO significa que la actividad aumente el IMC:
# es la pendiente de la actividad en una persona con 0 alelos de riesgo,
# valor que casi no existe en los datos (alelos ~ 10 ± 2.2).

# c) Centrado y comparación con la verdad (la simulación ya está centrada en 10 y 15)
gxe$alelos_c <- gxe$alelos - mean(gxe$alelos)
gxe$act_c    <- gxe$act    - mean(gxe$act)
round(coef(lm(imc ~ alelos_c * act_c, data = gxe)), 3)
# Estimado vs verdadero: intercepto 26.02 vs 26; alelos 0.65 vs 0.6;
# act -0.093 vs -0.10; interacción -0.025 vs -0.025

# d) Efecto por alelo a 5, 15 y 30 MET-h/semana, con IC 95 %
t(sapply(c(5, 15, 30), function(a) {
  gxe$act_a <- gxe$act - a
  m <- lm(imc ~ alelos * act_a, data = gxe)
  c(act = a, efecto_alelo = coef(m)["alelos"], confint(m)["alelos", ])
})) |> round(3)
# 5 MET-h: 0.91 kg/m² por alelo; 15: 0.65; 30: 0.28 -> el ejercicio reduce ~70 % el efecto

# e) Actividad que anularía el efecto genético
b <- coef(g_con)
-b["alelos"] / b["alelos:act"]   # ~41 MET-h/semana
range(gxe$act)                    # 0 a 34.4: fuera del rango observado -> no concluirlo

# Gráfica: efecto de los alelos según nivel de actividad
rej <- expand.grid(alelos = 3:17, act = c(5, 15, 30))
rej$pred <- predict(g_con, newdata = rej)
ggplot(rej, aes(alelos, pred, colour = factor(act))) +
  geom_line(linewidth = 1.2) +
  labs(x = "Alelos de riesgo", y = "IMC predicho (kg/m²)",
       colour = "Actividad\n(MET-h/sem)")

# f) Poder con n = 150
set.seed(6)
n      <- 150
alelos <- rbinom(n, size = 20, prob = 0.5)
act    <- round(pmax(0, rnorm(n, 15, 6)), 1)
imc    <- 26 + 0.6*(alelos - 10) - 0.10*(act - 15) -
          0.025*(alelos - 10)*(act - 15) + rnorm(n, 0, 2.5)
summary(lm(imc ~ alelos * act))$coefficients["alelos:act", ]

# Poder aproximado (500 réplicas) con n = 150 vs n = 800
poder <- function(n, reps = 500) {
  mean(replicate(reps, {
    alelos <- rbinom(n, 20, 0.5)
    act    <- round(pmax(0, rnorm(n, 15, 6)), 1)
    imc    <- 26 + 0.6*(alelos - 10) - 0.10*(act - 15) -
              0.025*(alelos - 10)*(act - 15) + rnorm(n, 0, 2.5)
    summary(lm(imc ~ alelos * act))$coefficients["alelos:act", 4] < 0.05
  }))
}
set.seed(1); c(n150 = poder(150), n800 = poder(800))   # ~0.35 vs ~0.95
# Con n = 150 la interacción real se detecta en ~1 de cada 3 estudios:
# "no significativa" no equivale a "no existe". Las interacciones requieren muestras grandes.
