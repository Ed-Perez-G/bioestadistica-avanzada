# =============================================================
# Modelos lineales — Parte 3: Interacciones
# Soluciones de los ejercicios de práctica
# =============================================================
library(ggplot2)
theme_set(theme_minimal(base_size = 13))

# -------------------------------------------------------------
# Ejercicio 1 — Pima.tr: edad x diabetes sobre presión diastólica
# -------------------------------------------------------------
data(Pima.tr, package = "MASS")

# a) Gráfica por grupo: las rectas NO son paralelas
ggplot(Pima.tr, aes(age, bp, colour = type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(x = "Edad (años)", y = "Presión diastólica (mmHg)", colour = "Diabetes")

# b) Modelos sin y con interacción
p_sin <- lm(bp ~ age + type, data = Pima.tr)
p_con <- lm(bp ~ age * type, data = Pima.tr)
summary(p_con)
anova(p_sin, p_con)          # F = 4.55, p = 0.034 -> hay interacción

# c) Interpretación:
#  (Intercept) = 54.5 : PAD esperada en NO diabética de 0 años (sin sentido)
#  age         = 0.51 : pendiente de la edad en NO diabéticas (mmHg/año)
#  typeYes     = 12.5 : diferencia diabética - no diabética A LOS 0 AÑOS
#  age:typeYes = -0.31: la pendiente en diabéticas es 0.31 mmHg/año menor

# d) Pendientes simples con IC 95 %
p_ref_yes <- lm(bp ~ age * relevel(type, ref = "Yes"), data = Pima.tr)
rbind(No  = c(coef(p_con)["age"],     confint(p_con)["age", ]),
      Yes = c(coef(p_ref_yes)["age"], confint(p_ref_yes)["age", ])) |> round(2)
# No diabéticas: 0.51 mmHg/año; diabéticas: 0.51 - 0.31 = 0.20 mmHg/año

# e) Centrado
Pima.tr$age_c <- Pima.tr$age - mean(Pima.tr$age)
p_cen <- lm(bp ~ age_c * type, data = Pima.tr)
summary(p_cen)
# Cambia: intercepto y typeYes (ahora = diferencia a la edad promedio, ~32 años)
# No cambia: beta3, su EE, su p, el R2 ni los valores ajustados

# f) Redacción (ejemplo):
# "La asociación entre la edad y la presión diastólica difirió según el
#  diagnóstico de diabetes (beta3 = -0.31 mmHg/año; p = 0.034): en no
#  diabéticas la PAD aumentó 0.51 mmHg por año, y en diabéticas 0.20 mmHg por año."

# -------------------------------------------------------------
# Ejercicio 2 — birthwt: peso materno x tabaquismo
# -------------------------------------------------------------
data(birthwt, package = "MASS")
birthwt$smoke_f <- factor(birthwt$smoke, labels = c("No fuma", "Fuma"))

b_con <- lm(bwt ~ lwt * smoke_f, data = birthwt)
summary(b_con)               # lwt:smoke_fFuma p = 0.48 -> sin evidencia de interacción

b_sin <- lm(bwt ~ lwt + smoke_f, data = birthwt)
anova(b_sin, b_con)
summary(b_sin)
# Modelo final: sin interacción (si no había hipótesis fuerte que justificara mantenerla).
# smoke_fFuma = diferencia promedio de peso al nacer entre hijos de fumadoras y no
# fumadoras, ajustando por peso materno, IGUAL para cualquier lwt (líneas paralelas).

ggplot(birthwt, aes(lwt, bwt, colour = smoke_f)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(x = "Peso materno (lb)", y = "Peso al nacer (g)", colour = NULL)

# -------------------------------------------------------------
# Ejercicio 3 — Datos Bio3_24a (Celis)
# -------------------------------------------------------------
bio3_24a <- data.frame(
  x1 = c(0, 0, 0, 0, 0, 1, 1, 1, 2, 2, 2),
  x2 = c(0.28, 1.85, 3.73, 7.62, 8.12, 2.07, 2.09, 5.76, 5.83, 8.94, 9.39),
  y  = c(1.04, 2.30, 4.39, 9.49, 8.28, 4.85, 4.91, 7.44, 8.20, 11.04, 12.72))

c_sin <- lm(y ~ x1 + x2, data = bio3_24a)
c_con <- lm(y ~ x1 * x2, data = bio3_24a)
summary(c_sin); summary(c_con)
anova(c_sin, c_con)          # F = 1.08, p = 0.33
c(R2adj_sin = summary(c_sin)$adj.r.squared,
  R2adj_con = summary(c_con)$adj.r.squared)
# La interacción no mejora el modelo (F no significativa; R2 ajustado prácticamente igual). Con n = 11 el poder es muy bajo:
# "no significativo" NO demuestra ausencia de interacción.

# -------------------------------------------------------------
# Ejercicio 4 — ¿Confusión o interacción?
# -------------------------------------------------------------
set.seed(42); n <- 300
sexo <- rbinom(n, 1, 0.5)
act  <- pmax(0, round(3 + 2*sexo + rnorm(n, 0, 1.5), 1))
ldl  <- 120 + 20*sexo - 2*act + rnorm(n, 0, 12)
base_A <- data.frame(ldl, act, sexo = factor(sexo, labels = c("Mujer", "Hombre")))
sexo <- rbinom(n, 1, 0.5)
act  <- pmax(0, round(4 + rnorm(n, 0, 1.5), 1))
ldl  <- 130 - 4*act*(1 - sexo) + rnorm(n, 0, 12)
base_B <- data.frame(ldl, act, sexo = factor(sexo, labels = c("Mujer", "Hombre")))

evaluar <- function(d) {
  list(crudo    = coef(lm(ldl ~ act, d))["act"],
       ajustado = coef(lm(ldl ~ act + sexo, d))["act"],
       interac  = summary(lm(ldl ~ act * sexo, d))$coefficients["act:sexoHombre", ])
}
evaluar(base_A)
# Base A: interacción NO significativa (p = 0.97); crudo = +1.25 vs ajustado = -2.43
#         -> CONFUSIÓN por sexo (¡el efecto crudo hasta cambia de signo!)
evaluar(base_B)
# Base B: interacción significativa (p = 0.001): pendiente en mujeres -3.5,
#         en hombres -3.5 + 2.9 = -0.55 -> MODIFICACIÓN DE EFECTO por sexo

ggplot(rbind(cbind(base_A, base = "A"), cbind(base_B, base = "B")),
       aes(act, ldl, colour = sexo)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE,
              colour = "black", linetype = 2) +
  facet_wrap(~ base) +
  labs(x = "Actividad física (h/sem)", y = "LDL (mg/dL)", colour = NULL)

# -------------------------------------------------------------
# Ejercicio 5 — El costo de omitir la interacción
# -------------------------------------------------------------
# a) Con medias 0, b1 = beta1 + beta3*0 = beta1: el sesgo de b1 y b2 desaparece
#    (aunque los residuales siguen mal comportados).
set.seed(1); N <- 1e5
X <- rnorm(N, 0, 1); Z <- rnorm(N, 0, 1)
sapply(c(-1, -0.5, 0, 0.5, 1), function(b3) {
  Y <- X + Z + b3*X*Z + rnorm(N); coef(lm(Y ~ X + Z))
}) |> round(2)

# b) b2 = beta2 + beta3*muX = 1 + beta3*1 = 0  ->  beta3 = -1

# c) Poder del efecto de X con n = 50 cuando beta3 = -0.5 (b1 esperado = 0)
set.seed(2026)
p_x <- replicate(1000, {
  X <- rnorm(50, 1, 1); Z <- rnorm(50, 2, 1)
  Y <- X + Z - 0.5*X*Z + rnorm(50)
  summary(lm(Y ~ X + Z))$coefficients["X", 4]
})
mean(p_x < 0.05)   # proporción de réplicas "significativas" (aunque beta1 real = 1)
