# =============================================================
# Modelos lineales — Parte 3: Interacciones
# Solución del Ejercicio 8 — Sodio × edad sobre la PAS (ajustando por IMC)
# =============================================================
library(ggplot2)
theme_set(theme_minimal(base_size = 13))

set.seed(2024)
n     <- 400
edad  <- round(pmin(80, pmax(25, rnorm(n, 50, 12))))
imc   <- round(rnorm(n, 28, 4), 1)
sodio <- round(pmax(0.5, 3.5 + 0.08*(imc - 28) + rnorm(n, 0, 1)), 2)
pas   <- 125 + 0.5*(edad - 50) + 0.6*(imc - 28) + 2*(sodio - 3.5) +
         0.12*(sodio - 3.5)*(edad - 50) + rnorm(n, 0, 10)
d_na  <- data.frame(pas = round(pas), sodio, edad, imc)

# a) Exploración
round(cor(d_na), 2)        # sodio-imc r = 0.34; sodio-edad r ≈ 0
d_na$edad_ter <- cut(d_na$edad, quantile(d_na$edad, c(0, 1/3, 2/3, 1)),
                     include.lowest = TRUE, labels = c("Joven", "Media", "Mayor"))
sapply(split(d_na, d_na$edad_ter),
       function(x) round(coef(lm(pas ~ sodio + imc, data = x))["sodio"], 2))
# Joven (25–46): -0.57; Media (47–56): 2.53; Mayor (57–80): 4.33
# -> la pendiente del sodio crece con la edad: primera pista de interacción

ggplot(d_na, aes(sodio, pas, colour = edad_ter)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Sodio urinario (g/día)", y = "PAS (mmHg)", colour = "Edad (tercil)")
# Nota: categorizar sirve para EXPLORAR; el modelo final usa la edad continua.

# b) Confusión por IMC
b_crudo <- coef(lm(pas ~ sodio, data = d_na))["sodio"]          # 2.50
b_ajust <- coef(lm(pas ~ sodio + imc, data = d_na))["sodio"]    # 2.16
100 * (b_crudo - b_ajust) / b_ajust                             # ~16 % > 10 %
# El IMC se asocia con el sodio y con la PAS -> confunde parcialmente: se mantiene en el modelo.

# c) Interacción
m_sin <- lm(pas ~ sodio + edad + imc, data = d_na)
m_con <- lm(pas ~ sodio * edad + imc, data = d_na)
summary(m_con)
anova(m_sin, m_con)                                  # F = 15.5, p < 0.001
c(R2adj_sin = summary(m_sin)$adj.r.squared,          # 0.216
  R2adj_con = summary(m_con)$adj.r.squared)          # 0.243
c(AIC_sin = AIC(m_sin), AIC_con = AIC(m_con))        # 2994.9 vs 2981.6 -> mejor con interacción

# d) sodio = -5.86 (p = 0.006) es la pendiente del sodio a los 0 AÑOS: extrapolación sin sentido.
d_na$sodio_c <- d_na$sodio - mean(d_na$sodio)   # 3.48 g/día
d_na$edad_c  <- d_na$edad  - mean(d_na$edad)    # 50.6 años
d_na$imc_c   <- d_na$imc   - mean(d_na$imc)
m_cen <- lm(pas ~ sodio_c * edad_c + imc_c, data = d_na)
round(summary(m_cen)$coefficients, 3)
# sodio_c = +2.19 mmHg por g/día a la edad promedio (~51 años)
# edad_c  = +0.41 mmHg por año con consumo promedio de sodio
# imc_c   = +0.37 mmHg por kg/m², ajustado
# sodio_c:edad_c = 0.159: cada año de edad, el efecto de 1 g de sodio aumenta 0.16 mmHg
# (beta3, EE y p idénticos al modelo sin centrar)

# e) Efecto del sodio a 35, 50 y 65 años (recentrar edad en cada valor)
t(sapply(c(35, 50, 65), function(a) {
  d_na$edad_a <- d_na$edad - a
  m <- lm(pas ~ sodio * edad_a + imc, data = d_na)
  c(edad = a, efecto_sodio = coef(m)["sodio"], confint(m)["sodio", ])
})) |> round(2)
# 35 años: -0.29 (IC -1.89 a 1.30)  -> sin efecto detectable
# 50 años:  2.09 (IC  1.07 a 3.11)
# 65 años:  4.48 (IC  2.94 a 6.02)

# f) Edad en que el efecto del sodio sería 0
b <- coef(m_con)
-b["sodio"] / b["sodio:edad"]    # ~37 años, dentro del rango (25–80)
# Por debajo de ~37 años no se observa efecto del sodio sobre la PAS; a partir de ahí
# el efecto crece con la edad -> la restricción de sal tendría mayor beneficio en mayores.
# (La verdad de la simulación: 2 + 0.12*(edad - 50) = 0 -> 33 años)

# g) Pendientes simples (IMC fijo en su media)
m_e <- mean(d_na$edad); s_e <- sd(d_na$edad)
rej <- expand.grid(sodio = seq(1, 6, 0.1),
                   edad  = round(c(m_e - s_e, m_e, m_e + s_e)),
                   imc   = mean(d_na$imc))
rej$pred <- predict(m_con, newdata = rej)

ggplot(rej, aes(sodio, pred, colour = factor(edad))) +
  geom_point(data = d_na, aes(sodio, pas), colour = "grey70", alpha = 0.4,
             inherit.aes = FALSE) +
  geom_line(linewidth = 1.2) +
  labs(x = "Sodio urinario (g/día)", y = "PAS predicha (mmHg)",
       colour = "Edad (años)", caption = "IMC fijo en su media")

# Redacción (ejemplo):
# "Ajustando por IMC, el efecto del consumo de sodio sobre la PAS aumentó con la edad
#  (interacción sodio × edad: 0.16 mmHg por g/día por año; p < 0.001). A los 35 años
#  no se observó asociación (-0.3 mmHg por g/día; IC 95 % -1.9 a 1.3), mientras que a
#  los 50 y 65 años cada gramo adicional de sodio se asoció con 2.1 (IC 95 % 1.1–3.1)
#  y 4.5 mmHg (IC 95 % 2.9–6.0) más de PAS, respectivamente."
