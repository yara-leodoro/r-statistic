# https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset
df_csv=read.csv('sleep.csv', sep = ',')

cat("---\n")

# "1. Medidas de tendencia central:
cat("1. Medidas de tendencia central\n")
moda <- function(v) {
    freq <- table(v)
    moda <- as.numeric(names(freq[freq == max(freq)]))
}
m <- mean(df_csv$Sleep.Duration)
me <- median(df_csv$Sleep.Duration)
mo <- moda(df_csv$Sleep.Duration)
cat("Duração do sono:\n")
cat("Média:", m, " | Mediana:", me, " | Moda:", mo, "\n")
m <- mean(df_csv$Quality.of.Sleep)
me <- median(df_csv$Quality.of.Sleep)
mo <- moda(df_csv$Quality.of.Sleep)
cat("Qualidade do sono:\n")
cat("Média:", m, " | Mediana:", me, " | Moda:", mo, "\n")

cat("---\n")

# 2. Medidas de variancia:
cat("2. Medidas de variancia:\n")
v <- var(df_csv$Sleep.Duration)
dp <- sd(df_csv$Sleep.Duration)
coef <- (dp/m) * 100
cat("Duração do sono:\n")
cat("Variância:", v, " | Desvio Padrão:", dp, " | Coeficiente:", coef, "%\n")
v <- var(df_csv$Quality.of.Sleep)
dp <- sd(df_csv$Quality.of.Sleep)
coef <- (dp/m) * 100
cat("Qualidade do sono:\n")
cat("Variância:", v, " | Desvio Padrão:", dp, " | Coeficiente:", coef, "%\n")

cat("---\n")

# 3. Boxplot com análise:
cat("3. Boxplot com análise:\n")
pq <- quantile(df_csv$Sleep.Duration)
pq1 <- quantile(df_csv$Sleep.Duration, probs = 0.01)
out <- boxplot.stats(df_csv$Sleep.Duration)$out
cat("Quartis:\n")
print(pq)
print(pq1)
# cat("Outliers: ", out, "\n")

cat("---\n")

# 4. Test de normalidade
cat("4. Test de normalidade:\n")
shapiro <- shapiro.test(df_csv$Quality.of.Sleep)
cat("Shapiro-Wilk (p-valor): ", shapiro$p.value, "\n")

cat("---\n")

# 5. Teste de corelação
cat("5. Teste de corelação:\n")
plot(df_csv$Sleep.Duration, df_csv$Quality.of.Sleep, main = "Dispersão", xlab = "Duração", ylab = "Qualidade")
cor_spearman <- cor(df_csv$Sleep.Duration, df_csv$Quality.of.Sleep, method = "spearman")
cor_pearson <- cor(df_csv$Sleep.Duration, df_csv$Quality.of.Sleep, method = "pearson")
cat("Corelação de Spearman: ", cor_spearman, " | Corelação de Pearson: ", cor_pearson, "\n")

cat("---\n")

# 6. Regressão linear
cat("6. Regressão linear:\n")
m <- lm(df_csv$Sleep.Duration ~ df_csv$Quality.of.Sleep)
hist(residuals(m))
shapiro <- shapiro.test(residuals(m))
cat("Shapiro-Wilk (p-valor): ", shapiro$p.value, "\n")
s <- summary(m)
plot(df_csv$Sleep.Duration, df_csv$Quality.of.Sleep, col = "blue", pch = 16)
abline(m, col = "red")