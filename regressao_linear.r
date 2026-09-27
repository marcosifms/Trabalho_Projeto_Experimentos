library("corrplot")
library("car")
library("lmtest")

# dataset <- read.csv("Complete_Dataset.csv", dec=",")
dataset <- read.csv("Complete_Dataset.csv")
for (v in names(dataset)) {
    if (is.character(dataset[[v]])) {
        conv <- suppressWarnings(as.numeric(gsub(",", ".", dataset[[v]])))
        if (mean(is.na(conv)) < 0.1) dataset[[v]] <- conv   # só se for mesmo número
    }
}

dataset_numerico <- dataset[, sapply(dataset, is.numeric)]
#cat("Colunas numéricas:", ncol(dataset_numerico), "\n")


# ANÁLISE DESCRITIVA DO ADG (gráficos na aba Plots do RStudio)

adg <- dataset_numerico$ADG

# Gráfico 1: distribuição do ADG
hist(adg, breaks = 25, freq = FALSE, col = "#cde2fb", border = "white",
     main = "Distribuição do ADG", xlab = "ADG (kg/dia)", ylab = "Densidade")
lines(density(adg), col = "#2a78d6", lwd = 2)
abline(v = mean(adg), col = "#eb6834", lwd = 2)
abline(v = median(adg), col = "grey30", lwd = 2, lty = 2)
legend("topleft", c(paste("média =", round(mean(adg), 3)),
                    paste("mediana =", round(median(adg), 3))),
       col = c("#eb6834", "grey30"), lty = c(1, 2), lwd = 2, bty = "n")

# Gráfico 2: ADG por período
boxplot(ADG ~ PERIOD, data = dataset_numerico, col = "#cde2fb", border = "#2a78d6",
        main = "ADG por período de pesagem", xlab = "Período", ylab = "ADG (kg/dia)")
medias_periodo <- tapply(dataset_numerico$ADG, dataset_numerico$PERIOD, mean)
lines(seq_along(medias_periodo), medias_periodo, col = "#eb6834", lwd = 2, type = "b", pch = 19)
abline(h = 0, lty = 3, col = "grey40")
legend("topright", "média do período", col = "#eb6834", lwd = 2, pch = 19, bty = "n")

# OBSERVAÇÃO (não dá erro neste dataset): se houver NA, cor() devolve NA.
# O mais seguro seria cor(dataset_numerico, use = "pairwise.complete.obs").
matriz_correlacao <- cor(dataset_numerico)
png("matriz_correlacao.png", width = 1200, height = 1200, res = 150)
corrplot(matriz_correlacao, method = "color", type = "upper", tl.cex = 0.5, cl.cex = 0.5)
dev.off()

# Gráfico 3: variáveis mais correlacionadas com o ADG
cor_adg <- matriz_correlacao[setdiff(colnames(matriz_correlacao), c("ADG", "FINAL_WEIGHT")), "ADG"]
top10 <- cor_adg[order(abs(cor_adg), decreasing = TRUE)][1:10]
print(round(top10, 3))
par(mar = c(5, 11, 3, 1))
barplot(rev(top10), horiz = TRUE, las = 1, xlim = c(-1, 1), cex.names = 0.75,
        col = ifelse(rev(top10) > 0, "#2a78d6", "#eb6834"), border = NA,
        main = "10 variáveis mais correlacionadas com o ADG",
        xlab = "Correlação (azul = positiva, laranja = negativa)")
abline(v = 0)
par(mar = c(5, 4, 4, 2) + 0.1)

# Original: vars_remover <- c("ADG", "FINAL_WEIGHT")
vars_remover <- c("ADG", "FINAL_WEIGHT", "ANIMAL", "PERIOD")
todas_var <- names(dataset_numerico)
vars_independentes <- todas_var[!todas_var %in% vars_remover]

#feito para não dar erro no vif, porque quando esse comando dataset <- read.csv("Complete_Dataset.csv")
#trouxe mais colunas algumas que são cópias de outras.
equacao <- as.formula(paste("ADG ~ ", paste(vars_independentes, collapse = " + ")))
modelo_temp <- lm(equacao, data = dataset_numerico)
redundantes <- names(coef(modelo_temp))[is.na(coef(modelo_temp))]
cat("Removidas por redundância:", redundantes, "\n")
vars_independentes <- vars_independentes[!vars_independentes %in% redundantes]

corte_vif <- 5
continuar <- TRUE
while(continuar){
    equacao <- as.formula(paste("ADG ~ ", paste(vars_independentes,
                                        collapse = " + ")))
    modelo_temp <- lm(equacao, data=dataset_numerico)
    vif_temp <- vif(modelo_temp)
    maior_vif <- max(vif_temp)
    if(maior_vif >= corte_vif){
        pior_variavel <- names(vif_temp)[which.max(vif_temp)]
        vars_independentes <- vars_independentes[!vars_independentes %in% pior_variavel]
        cat("Removida variável:", pior_variavel, "(VIF=", maior_vif, ")\n")
    }else{
        continuar <- FALSE
    }
}
cat("\nVariáveis finais:", paste(vars_independentes, collapse = " + "), "\n")

modelo_final <- step(modelo_temp, direction="both", trace=0)

summary(modelo_final)  


shapiro.test(modelo_final$residuals)
bptest(modelo_final)
durbinWatsonTest(modelo_final)
