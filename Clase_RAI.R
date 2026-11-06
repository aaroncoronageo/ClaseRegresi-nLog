library(tidyverse)
library(ggplot2)
library(mice)
library(gtsummary)
library(corrplot)
library(PerformanceAnalytics)
library(gplots)
library(sjPlot )

# Leer datos
df <- read.csv('C:/Users/aaron/OneDrive/Documentos/Clase RAI/data_clase.csv', encoding = 'latin1')
names(df)

# Limpieza y preparacion de los datos -------------------------------------
# Nuevas variables
df <- df %>%
  mutate(
    IMC = Peso / (Talla * Talla),
    IMC_Cat = case_when(IMC <= 18 ~ 'Bajo peso', 
                        IMC > 18 & IMC < 25 ~ 'Normal',
                        IMC >= 25 & IMC < 30 ~ 'Sobrepeso',
                        IMC >= 30 ~ 'Obesidad'),
    Glucosa_Cat = case_when(Glucosa < 100 ~ 'Bajo < 100 mg/dL',
                                  Glucosa >= 100 ~ 'Alto > 100 mg/dL'),
    Trigliceridos_Cat = case_when(Trigliceridos < 150 ~ 'Bajo < 150 mg/dL',
                     Trigliceridos >= 150 ~ 'Alto > 150 mg/dL'),
    HDL_Cat = case_when(HDL < 50 ~ 'Bajo',
                        HDL >= 50 ~ 'Normal'),
    LDL_HDL = LDL - HDL,
    Cadera_Cintura = Cadera - Cintura
    )

# Datos faltantes
md.pattern(df, rotate.names = T)
colSums(is.na(df))
porcentajeMiss <- function(x) {sum(is.na(x)) / length(x) * 100}
apply(df, 2, porcentajeMiss)
# Imputacion
df_imputed <- data.frame(
  original = df$Trigliceridos,
  imputed_pmm = complete(mice(df, method = 'pmm'))$Trigliceridos,
  imputed_cart = complete(mice(df, method = 'cart'))$Trigliceridos,
  imputed_lasso = complete(mice(df, method = 'lasso.norm'))$Trigliceridos
)
ggplot(df_imputed, aes(x = original)) +
  geom_histogram(fill = '#ad1538', color = '#000000', position = 'identity') +
  ggtitle('ORIGINAL') +
  theme_classic()
ggplot(df_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = '#15ad4f', color = '#000000', position = 'identity') +
  ggtitle('PPM') +
  theme_classic()
ggplot(df_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = '#1543ad', color = '#000000', position = 'identity') +
  ggtitle('CART') +
  theme_classic()
ggplot(df_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = '#ad8415', color = '#000000', position = 'identity') +
  ggtitle('LASSO') +
  theme_classic()

# Valores atipicos
hist(df$Trigliceridos, main = 'Trigliceridos', xlab = 'mg/dL')
boxplot(df$Trigliceridos, main = 'Trigliceridos', ylab = 'mg/dL')
boxplot.stats(df$Cadera)$out
summary(df$Glucosa)

df <- df %>% # Mantener datos faltantes
  filter((Cadera <= 132.5 | is.na(Cadera)) &
           (Cintura <= 127.5 | is.na(Cintura)) &
           (Peso >= 32.3 & Peso <= 106.1 | is.na(Peso)) &
           (Talla >= 1.2 | is.na(Talla)) &
           (HDL >= 13.6 & HDL <= 73.3 | is.na(HDL)) &
           (LDL >= 5.5 & LDL <= 182.3 | is.na(LDL)) &
           (Glucosa <= 285 | is.na(Glucosa)) &
           (Trigliceridos <= 392.5 | is.na(Trigliceridos))
           )

df <- df %>% # Eliminar datos faltantes
  filter(Cadera <= 132.5 &
           Cintura <= 127.5 &
           (Peso >= 32.3 & Peso <= 106.1) &
           Talla >= 1.2 &
           (HDL >= 13.6 & HDL <= 73.3) &
           (LDL >= 5.5 & LDL <= 182.3) &
           Glucosa <= 285 &
           Trigliceridos <= 392.5
         )
hist(df_$Trigliceridos, main = 'Trigliceridos', xlab = 'mg/dL')

# Analisis exploratorio de datos
# Univariado numericas
df %>%
  summarize(across(where(is.numeric), list(
    mean = mean,
    sd = sd,
    min = min,
    max = max,
    median = median
  ), .names = '{.col}_{.fn}', na.rm = T)) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(statistic = str_match(name, pattern = '.+_(.+)')[,2],
         name = str_match(name, pattern = '(.+)_.+')[,2]) %>% 
  pivot_wider(names_from = name, values_from = value)
hist(df$Glucosa, main = 'Glucosa central')
plot(density(df$Glucosa), main = 'Glucosa central')
boxplot(df$Glucosa, main = 'Glucosa central')
qqnorm(df$Glucosa, main = 'Glucosa central')
qqline(df$Glucosa, col = 'red', lwd = 2)
# Univariado categoricas
barplot(sort(table(df$Jurisdiccion)))
pie(table(df$Jurisdiccion))
barplot(sort(table(df$IMC_Cat)))
pie(table(df$IMC_Cat))

# Bivariado
# Scatter plot
ggplot(df, aes(x = LDL, y = HDL)) +
  geom_point() +
  labs(title = 'LDL vs HDL') +
  theme_minimal()
chart.Correlation(df %>% select(Edad, Cadera, Cintura, Peso, Talla, HDL, LDL, Glucosa, Trigliceridos, IMC, LDL_HDL, Cadera_Cintura),
                  method = 'pearson', histogram = T,)
# Line plot
ggplot(df, aes(x = IMC, y = Cintura)) +
  geom_line() +
  labs(title = 'IMC vs Cintura') +
  theme_minimal()
# Correlation
df %>%
  select(Edad, Glucosa, Trigliceridos, IMC, LDL_HDL, Cadera_Cintura) %>%
  cor(use = 'complete.obs') %>%
  corrplot(method = 'number', type = 'lower')
# Barplot bivariado
plotdata <- df %>%
  group_by(Diabetes) %>%
  summarize(valor = mean(Glucosa))
ggplot(plotdata, aes(x = IMC_Cat, y = valor)) +
  geom_bar(stat = 'identity') +
  labs(y = 'Cadera_Cintura') +
  theme_minimal()
# Density bivariado
ggplot(df, aes(x = Glucosa, fill = Diabetes)) +
  geom_density(alpha = 0.4) +
  labs(title = 'Glucosa en sangre por Diabetes') +
  theme_minimal()
# Boxplot bivariate
ggplot(df, aes(x = IMC_Cat, y = Cadera_Cintura)) +
  geom_boxplot() +
  labs(title = 'Relacion cadera-cintura por IMC') +
  theme_minimal()
balloonplot(t(table(df$IMC_Cat, df$Dislipidemia)),
            main = 'IMC vs Dislipidemia')

# Analisis de datos
# Determinacion del sindrome metabolico
df <- df %>%
  mutate(SM = case_when((Diabetes == 'Sí' & Obesidad == 'Sí' & Dislipidemia == 'Sí') ~ 1,
                        (Diabetes == 'Sí' & Obesidad == 'Sí' & Hipertension == 'Sí') ~ 1,
                        (Diabetes == 'Sí' & Obesidad == 'Sí' & HDL_Cat == 'Bajo') ~ 1,
                        (Obesidad == 'Sí' & Dislipidemia == 'Sí' & Hipertension == 'Sí') ~ 1,
                        (Obesidad == 'Sí' & Dislipidemia == 'Sí' & HDL_Cat == 'Bajo') ~ 1,
                        (Dislipidemia == 'Sí' & Hipertension == 'Sí' & HDL_Cat == 'Bajo') ~ 1,
                        TRUE ~ 0))
# Analisis de asociacion
tab <- table(df$SM, df$HDL_Cat)
chisq.test(tab)
# Regresion logistica (variables originales)
df_mod <- df %>%
  mutate(
    # Variables categoricas
    SM = factor(SM), # Respuesta
    Diabetes = factor(Diabetes),
    Hipertension = factor(Hipertension),
    Obesidad = factor(Obesidad),
    Dislipidemia = factor(Dislipidemia)
  ) %>%
  select(SM, Diabetes, Hipertension, Obesidad, Dislipidemia,
         Edad, Glucosa, Trigliceridos, IMC, LDL_HDL, Cadera_Cintura)

# Modelo nulo
model.null = glm(SM ~ 1,
                 data = df_mod,
                 family = binomial(link = 'logit'))

# Modelo completo
model.full = glm(SM ~ .,
                 data = df_mod,
                 family = binomial(link = 'logit'))
# Definicion del modelo optimo
mod_sm <- step(model.null,
     scope = list(upper = model.full),
     direction = 'both',
     test = 'Chisq',
     data = df_mod)
summary(mod_sm)

# Regresion logistica (variables calculadas)
df_mod <- df %>%
  mutate(
    # Variables categoricas
    SM = factor(SM), # Respuesta
    IMC_Cat = factor(IMC_Cat),
    Glucosa_Cat = factor(Glucosa_Cat),
    Trigliceridos_Cat = factor(Trigliceridos_Cat),
    HDL_Cat = factor(HDL_Cat)
  ) %>%
  select(SM, Edad, LDL_HDL, Cadera_Cintura,
         IMC_Cat, Glucosa_Cat, Trigliceridos_Cat, HDL_Cat)

# Modelo nulo
model.null = glm(SM ~ 1,
                 data = df_mod,
                 family = binomial(link = 'logit'))

# Modelo completo
model.full = glm(SM ~ .,
                 data = df_mod,
                 family = binomial(link = 'logit'))
# Definicion del modelo optimo
mod_sm <- step(model.null,
               scope = list(upper = model.full),
               direction = 'both',
               test = 'Chisq',
               data = df_mod)
summary(mod_sm)
# Interpretacion
mod_sm <- glm(SM ~ IMC_Cat + HDL_Cat + Trigliceridos_Cat + Edad + Glucosa_Cat,
              df_mod, family = binomial(link = 'logit'))
summary(mod_sm)
data.frame(Probabilidad = round((1 - exp(mod_sm$coefficients)) * 100, 1) * - 1)
# Prediccion
# create data frame of new patient
new_patient <- data.frame(
  IMC_Cat = 'Normal',
  Edad = 25,
  HDL_Cat = 'Bajo',
  Trigliceridos_Cat = 'Alto > 150 mg/dL',
  Glucosa_Cat = 'Alto > 100 mg/dL')
round(predict(mod_sm, newdata = new_patient, type = 'response') * 100, 1)
# Efectos por variable
plot_model(mod_sm, type = 'pred', terms = c('Edad', 'Glucosa_Cat'), ci.lvl = NA) +
  labs(title = 'Desarrollo de SM', y = 'Prob(Síndrome metabólico)') +
  theme_minimal()
