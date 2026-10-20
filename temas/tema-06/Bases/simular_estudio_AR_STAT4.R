# =============================================================
# Simulación de un estudio de casos y controles: artritis reumatoide (AR)
# y una variante del gen STAT4 (rs7574865, alelo de riesgo T)
#
# DATOS SIMULADOS CON FINES DIDÁCTICOS.
# Los parámetros están inspirados en la literatura (Remmers et al., 2007,
# NEJM: OR por alelo T ~1.3; frecuencia del alelo T ~0.22 en controles
# europeos), pero NO son datos reales de pacientes.
# =============================================================

set.seed(2026)

# ---- 1. Una población grande (fuente de casos y controles) ----
N <- 60000

poblacion <- data.frame(
  sexo = sample(c("Mujer", "Hombre"), N, replace = TRUE),
  edad = round(runif(N, 25, 75)),
  # proporción de ancestría amerindia (0 a 1)
  ancestria = rbeta(N, 2, 2)
)

# PC1: componente principal genético, casi un "espejo" de la ancestría
poblacion$PC1 <- round(4 * (poblacion$ancestria - 0.5) + rnorm(N, 0, 0.25), 3)

# Tabaquismo (3 categorías)
poblacion$tabaquismo <- sample(c("Nunca", "Exfumador", "Actual"), N,
                               replace = TRUE, prob = c(0.55, 0.20, 0.25))

# Genotipo rs7574865: la frecuencia del alelo T depende de la ancestría
# (esto es lo que genera ESTRATIFICACIÓN POBLACIONAL)
p_T <- 0.12 + 0.40 * poblacion$ancestria
dosis_T <- rbinom(N, size = 2, prob = p_T)       # 0, 1 o 2 copias de T
poblacion$rs7574865 <- c("GG", "GT", "TT")[dosis_T + 1]

# Panel de 20 SNPs adicionales (frecuencias alélicas entre 0.10 y 0.45)
maf <- seq(0.10, 0.45, length.out = 20)
for (j in 1:20) {
  poblacion[[sprintf("SNP%02d", j)]] <- rbinom(N, 2, maf[j])
}

# Variante muy rara (para ilustrar separación completa)
poblacion$var_rara <- rbinom(N, 1, 0.0004)

# ---- 2. Modelo verdadero (escala logit) ----
mujer  <- as.numeric(poblacion$sexo == "Mujer")
exfum  <- as.numeric(poblacion$tabaquismo == "Exfumador")
actual <- as.numeric(poblacion$tabaquismo == "Actual")

logit_p <- -5.6 +
  log(1.30) * dosis_T +                 # efecto aditivo del alelo T
  log(2.50) * mujer +                   # mujeres: más riesgo
  0.035     * (poblacion$edad - 50) +   # edad (lineal en el logit)
  log(1.30) * exfum +
  log(1.60) * actual +
  3.0       * (poblacion$ancestria - 0.5) +   # la ancestría también eleva el riesgo
  log(1.35) * dosis_T * actual +        # interacción gen-ambiente (G x E)
  log(1.50) * poblacion$SNP03 +         # SNP del panel con efecto real
  log(0.70) * poblacion$SNP11 +         # SNP protector real
  8         * poblacion$var_rara        # variante rara con efecto enorme

poblacion$caso <- rbinom(N, 1, plogis(logit_p))

# ---- 3. Muestreo caso-control: 400 casos y 400 controles ----
casos     <- poblacion[poblacion$caso == 1, ]
controles <- poblacion[poblacion$caso == 0, ]

# Prevalencia en la población (útil para la discusión de VPP/VPN)
prev <- mean(poblacion$caso)

muestra <- rbind(casos[sample(nrow(casos), 400), ],
                 controles[sample(nrow(controles), 400), ])
muestra <- muestra[sample(nrow(muestra)), ]       # desordenar filas
muestra$id <- sprintf("ID%03d", seq_len(nrow(muestra)))
muestra$ancestria <- round(muestra$ancestria, 3)

muestra <- muestra[, c("id", "caso", "sexo", "edad", "tabaquismo",
                       "ancestria", "PC1", "rs7574865", "var_rara",
                       sprintf("SNP%02d", 1:20))]

write.csv(muestra, "estudio_AR_STAT4.csv", row.names = FALSE)

cat("Prevalencia poblacional de AR simulada:", round(prev, 4), "\n")
cat("Portadores de la variante rara en la muestra:",
    sum(muestra$var_rara), "(casos:", sum(muestra$var_rara[muestra$caso == 1]), ")\n")
