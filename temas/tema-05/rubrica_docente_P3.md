# Rúbrica docente — Ejercicios integradores (Modelos lineales, Parte 3)

> Documento para el profesor. **No compartir con los alumnos.**
> Todos los valores se obtuvieron ejecutando los modelos indicados en R.

## 1. Rúbrica general (por base, 100 puntos)

| Criterio | Pts | Qué buscar |
|---|---|---|
| **Carga y auditoría de datos** | 15 | Carga indicando el paquete. Revisa la estructura, los NA, los duplicados y la unidad de observación (persona vs registro). Define la población de la pregunta (filtros) y la reporta con su *n* final. |
| **Exploración** | 10 | Descriptivos por grupo, distribución del desenlace y de los predictores, y gráficos bivariados. Plantea la interacción antes de modelar, por ejemplo con rectas por grupo o con estratos de la variable continua. |
| **Transformación / normalización** | 10 | Detecta asimetría o no linealidad y justifica la transformación (log) o la decisión de no transformar. Sabe que la escala elegida puede crear o eliminar una interacción. Centra las variables para interpretar los coeficientes, no "por colinealidad". |
| **Modelo aditivo y confusión** | 10 | Ajusta primero un modelo crudo y uno ajustado, y evalúa si la covariable confunde (cambio ≳ 10 %). |
| **Evaluación de la interacción** | 15 | Usa la prueba *t* de β₃ o la F parcial (`anova(m_sin, m_con)`), y la complementa con el R² ajustado y/o el AIC. Respeta el principio de jerarquía. Decide el modelo final con un argumento. |
| **Supuestos y diagnóstico** | 10 | Revisa residuales vs ajustados, Q-Q, homocedasticidad y puntos influyentes (Cook, *leverage*), y actúa en consecuencia. |
| **Gráficos** | 10 | Presenta un gráfico de interacción adecuado al tipo de variables: rectas por grupo, pendientes simples a ±1 DE o superficie. Los ejes están rotulados con unidades y no se extrapola fuera del rango observado. |
| **Interpretación** | 15 | Interpreta los coeficientes condicionales correctamente (no como "efectos principales"). Reporta pendientes simples con IC 95 % y no interpreta coeficientes evaluados en 0 cuando ese valor no tiene sentido. |
| **Conclusión y reproducibilidad** | 5 | Redacta un párrafo de resultados que responde a la pregunta. El `.qmd` se renderiza sin errores. |

**Errores que restan automáticamente:**
- Interpretar β₁ o β₂ como efecto global cuando hay interacción.
- Eliminar los efectos principales por no ser significativos (viola la jerarquía).
- Confundir confusión con interacción.
- Extrapolar fuera del rango observado.

---

## 2. Claves por base

### Base 1 — `faraway::diabetes` (n = 403)

**Preparación esperada.**
- `weight` está en libras y `height` en pulgadas, así que IMC = 703 × peso / talla².
- `glyhb` tiene 13 NA; el *n* del modelo queda en ≈384.
- `glyhb` es muy asimétrica (sesgo ≈ 2.2).

**Resultado clave: la interacción depende de la escala.**

| Modelo | β₃ (`stab.glu:age`) | p |
|---|---|---|
| `glyhb ~ stab.glu * age + gender + bmi` | −0.00025 | 0.019 |
| `log(glyhb) ~ stab.glu * age + gender + bmi` | −0.00004 | 0.022 |
| `log(glyhb) ~ log(stab.glu) * age + gender + bmi` | 0.0003 | **0.88** |

- En escala original, la pendiente de la glucosa baja de 0.036 a los 30 años a 0.024 % de HbA1c por mg/dL a los 75 años.
- Al transformar **ambas** variables a log, la interacción desaparece, porque la relación glucosa–HbA1c es curva.
- Una respuesta excelente detecta esto y concluye que la "interacción" probablemente refleja **no linealidad no modelada** (Rimpler et al., problema 1). No es una modificación de efecto genuina.
- Sexo e IMC no son significativos tras ajustar.

**Gráfico ideal:** pendientes simples por edad (30/45/60 años) en las dos escalas, para contrastarlas.

### Base 2 — `NHANES`

**Trampa de calidad.** La base tiene 10,000 filas pero solo 6,779 participantes únicos (`ID` repetidos).

| Versión de los datos | *n* del modelo | β₃ (`Age:Gendermale`) | EE |
|---|---|---|---|
| Sin eliminar duplicados (adultos) | 6,919 | −0.264 | 0.022 |
| Correcto: `!duplicated(ID)` y `Age >= 20` | 4,424 | −0.259 | 0.027 |

Analizar sin eliminar duplicados **subestima los errores estándar**, porque se viola la independencia. Debe restar puntos en "Carga y auditoría".

**Resultado** (`BPSysAve ~ Age * Gender + BMI`, *n* = 4,424):
- La interacción es clara: β₃ = −0.26 mmHg/año, p < 0.001.
- Mujeres: +0.55 mmHg por año. Hombres: 0.55 − 0.26 = +0.30 mmHg por año.
- `Gendermale = +17.0` es la diferencia **a los 0 años**, una extrapolación. Hay que centrar la edad o evaluar a edades concretas.
- Las rectas se cruzan hacia los 66 años: las mujeres pasan de tener una PAS menor a una mayor que los hombres. Encaja con la menopausia y la rigidez arterial.
- `BPSysAve` es algo asimétrica. Con `log(BPSysAve)` la conclusión no cambia (β₃ p < 0.001); se acepta cualquiera de las dos escalas si está justificada.
- El IMC es una covariable positiva (+0.25 mmHg por kg/m²).

### Base 3 — `riskCommunicator::framingham`

**Trampa de diseño.** La base es longitudinal: 11,627 filas de 4,434 personas en 3 periodos. La pregunta pide la **visita basal**, así que hay que filtrar `PERIOD == 1` (*n* ≈ 4,364 con datos completos). Usar todos los periodos viola la independencia.

- `SEX` está codificado 1 = hombre y 2 = mujer, y debe convertirse a factor con etiquetas.

**Resultado** (`TOTCHOL ~ AGE * SEX + BMI`):
- La interacción es muy fuerte: β₃ = +1.85 mg/dL por año, p < 0.001.
- Hombres: +0.23 mg/dL por año (p = 0.039). Mujeres: +2.07 mg/dL por año.
- `SEXMujer = −85.7` es la diferencia a los 0 años, otra vez una extrapolación (el rango de edad es 32–70).
- **Cruce ≈ 46 años**: antes las mujeres tienen menos colesterol que los hombres, y después más. Se espera que el alumno lo identifique al describir cómo difiere la relación.
- **Explicación biológica:** la pérdida del efecto estrogénico en la perimenopausia/menopausia, que reduce la expresión del receptor de LDL hepático.
- `TOTCHOL` es asimétrica (máximo 696), por lo que se valora que discutan valores extremos o influencia.

**Gráfico ideal:** rectas por sexo con IC y el punto de cruce marcado.

### Base 4 — `survival::flchain` (n = 7,874)

**Preparación esperada.**
- `creatinine` tiene 1,350 NA, que deben reportarse.
- `kappa` y `creatinine` son muy asimétricas, así que hay que transformar ambas a log.
- Valorar positivamente si discuten excluir a los 115 sujetos con MGUS (`mgus == 1`). El resultado no cambia (β₃ p = 0.005).

**Resultado clave: la interacción solo aparece en la escala log** (escala inversa a la de la Base 1).

| Modelo | β₃ | p | R² |
|---|---|---|---|
| `kappa ~ age * creatinine + sex` | −0.0025 | 0.24 | 0.36 |
| `log(kappa) ~ age * log(creatinine) + sex` | 0.0061 | **0.004** | 0.21 |

**Interpretación** (escala log–log): la elasticidad de kappa respecto a la creatinina aumenta con la edad.
- A los 55 años: ≈ 0.36 + 0.0061 × 55 ≈ 0.70.
- A los 85 años: ≈ 0.88.

**Relevancia clínica.**
- Con *n* ≈ 6,500 cualquier efecto pequeño es significativo, y el cambio de pendiente es modesto.
- Una buena respuesta discute que la creatinina explica buena parte de la variación de kappa (aclaramiento renal de las cadenas ligeras). La modificación por edad aporta poco en la práctica, por ejemplo para los intervalos de referencia de κ/λ.
- También debería mencionar que la creatinina es un marcador imperfecto de la función renal en adultos mayores (masa muscular).

**Gráfico ideal:** pendientes simples log–log por edad (55/65/80) o superficie, en escala log o retransformada.

---

## 3. Mensajes transversales que deberían aparecer

1. Las interacciones dependen de la **escala** (Bases 1 y 4): una transformación puede crear o eliminar una interacción.
2. La **unidad de observación** importa (Bases 2 y 3): los duplicados y los datos repetidos inflan el *n* y reducen artificialmente los EE.
3. Con interacción, los coeficientes de los efectos simples se evalúan en 0. Hay que **centrar** o reportar efectos a valores con sentido (Bases 2 y 3).
4. **Significancia estadística ≠ relevancia clínica** (Base 4).
