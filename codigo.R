# Importando os pacotes
library(ggplot2)
library(dplyr)
require(car)
library(corrplot)
library(psych)

# Importando os dados
wine <- read.table("C:\\Users\\lucas.dias\\Downloads\\wine.txt", header=TRUE)
attach(wine)
caminho <- 'C:\\Users\\lucas.dias\\Downloads\\'

# ANÁLISE DESCRITIVA ----
# Histogramas das variáveis
p <- ggplot(wine, aes(x = qualidade)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "gray80", color = "black", alpha = 0.9) +
  geom_density(color = "#a11d21", size = 1) +  # adiciona a linha de densidade
  labs(title = "Distribuição da Qualidade dos Vinhos Pinot Noir",
       x = "Qualidade",
       y = "Densidade") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

ggsave(paste0(caminho,"histograma_qualidade.pdf"), plot = p, width = 8, height = 6)

# Boxplots comparativos
# Caminho para salvar o PDF
pdf(paste0(caminho,"boxplots_variaveis.pdf"), width = 9, height = 6)
boxplot(wine$claridade, wine$aroma, wine$corpo, wine$sabor, wine$aromac,
        names = c("Claridade", "Aroma", "Corpo", "Sabor", "Aromac"),
        col = "gray85",
        border = "black",
        main = "Boxplots das Variáveis Sensoriais",
        ylab = "Valor",
        xlab = "Atributos Sensoriais",
        cex.main = 1.4,
        cex.lab = 1.2,
        cex.axis = 1.1)
dev.off()


# Boxplot Claridade
pdf(paste0(caminho,"DESC_BP_CLARIDADE.pdf"), width = 6, height = 5)
boxplot(wine$claridade,
        names = c("Claridade"),
        col = "gray85",
        border = "black",
        main = "Boxplot da Claridade",
        ylab = "Valor",
        xlab = "Atributo",
        cex.main = 1.4,
        cex.lab = 1.2,
        cex.axis = 1.1)
dev.off()

# Boxplot Aroma
pdf(paste0(caminho,"DESC_BP_AROMA.pdf"), width = 6, height = 5)
boxplot(wine$aroma,
        names = c("Aroma"),
        col = "gray85",
        border = "black",
        main = "Boxplot do Aroma",
        ylab = "Valor",
        xlab = "Atributo",
        cex.main = 1.4,
        cex.lab = 1.2,
        cex.axis = 1.1)
dev.off()

# Boxplot Corpo
pdf(paste0(caminho,"DESC_BP_CORPO.pdf"), width = 6, height = 5)
boxplot(wine$corpo,
        names = c("Corpo"),
        col = "gray85",
        border = "black",
        main = "Boxplot do Corpo",
        ylab = "Valor",
        xlab = "Atributo",
        cex.main = 1.4,
        cex.lab = 1.2,
        cex.axis = 1.1)
dev.off()

# Boxplot Sabor
pdf(paste0(caminho,"DESC_BP_SABOR.pdf"), width = 6, height = 5)
boxplot(wine$sabor,
        names = c("Sabor"),
        col = "gray85",
        border = "black",
        main = "Boxplot do Sabor",
        ylab = "Valor",
        xlab = "Atributo",
        cex.main = 1.4,
        cex.lab = 1.2,
        cex.axis = 1.1)
dev.off()

# Boxplot Aromac
pdf(paste0(caminho,"DESC_BP_AROMAC.pdf"), width = 6, height = 5)
boxplot(wine$aromac,
        names = c("Aroma do Carvalho"),
        col = "gray85",
        border = "black",
        main = "Boxplot do Aroma do Carvalho",
        ylab = "Valor",
        xlab = "Atributo",
        cex.main = 1.4,
        cex.lab = 1.2,
        cex.axis = 1.1)
dev.off()

# Boxplot Qualidade
pdf(paste0(caminho,"DESC_BP_QUALIDADE.pdf"), width = 6, height = 5)
boxplot(wine$qualidade,
        names = c("Qualidade"),
        col = "gray85",
        border = "black",
        main = "Boxplot da Qualidade",
        ylab = "Valor",
        xlab = "Atributo",
        cex.main = 1.4,
        cex.lab = 1.2,
        cex.axis = 1.1)
dev.off()




# Matriz de dispersão
pdf(paste0(caminho,"matriz_dispersao.pdf"), width = 10, height = 10)
pairs(wine,
      main = "Matriz de Dispersão das Variáveis",
      pch = 19,                 # pontos sólidos
      col = "#2E75B6",          # cor azul acadêmica
      cex = 1.0,                # tamanho dos pontos
      labels = c("Claridade","Aroma","Corpo","Sabor","Aromac","Qualidade"),
      gap = 0.5)                # espaçamento entre gráficos
dev.off()


# Medidas Resumo de cada variável
summary(wine$claridade)
summary(wine$aroma)
summary(wine$corpo)
summary(wine$sabor)
summary(wine$aromac)
summary(wine$qualidade)

# Qualidade vs Aroma
scatterplot(qualidade ~ aroma, wine,
            regLine = list(method = lm, col = "#a11d21", lwd = 2), smooth = FALSE, boxplots = FALSE,
            pch = 16, col = "black", xlab = "Aroma", ylab = "Qualidade",
            cex = 1.2, cex.lab = 1.5, cex.axis = 1.3)

# Qualidade vs Claridade
scatterplot(qualidade ~ claridade, wine,
            regLine = list(method = lm, col = "#a11d21", lwd = 2), smooth = FALSE, boxplots = FALSE,
            pch = 16, col = "black", xlab = "Claridade", ylab = "Qualidade",
            cex = 1.2, cex.lab = 1.5, cex.axis = 1.3)

# Qualidade vs Sabor
scatterplot(qualidade ~ sabor, wine,
            regLine = list(method = lm, col = "#a11d21", lwd = 2), smooth = FALSE, boxplots = FALSE,
            pch = 16, col = "black", xlab = "Sabor", ylab = "Qualidade",
            cex = 1.2, cex.lab = 1.5, cex.axis = 1.3)

# Qualidade vs Corpo
scatterplot(qualidade ~ corpo, wine,
            regLine = list(method = lm, col = "#a11d21", lwd = 2), smooth = FALSE, boxplots = FALSE,
            pch = 16, col = "black", xlab = "Corpo", ylab = "Qualidade",
            cex = 1.2, cex.lab = 1.5, cex.axis = 1.3)

# Qualidade vs Sabor
scatterplot(qualidade ~ sabor, wine,
            regLine = list(method = lm, col = "#a11d21", lwd = 2), smooth = FALSE, boxplots = FALSE,
            pch = 16, col = "black", xlab = "Sabor", ylab = "Qualidade",
            cex = 1.2, cex.lab = 1.5, cex.axis = 1.3)

# Qualidade vs Aromac
scatterplot(qualidade ~ aromac, wine,
            regLine = list(method = lm, col = "#a11d21", lwd = 2), smooth = FALSE, boxplots = FALSE,
            pch = 16, col = "black", xlab = "Aroma de Carvalho", ylab = "Qualidade",
            cex = 1.2, cex.lab = 1.5, cex.axis = 1.3)

# Modelos de regressão linear simples para todas as variáveis explicativas
fit1 <- lm(qualidade ~ claridade, data = wine); summary(fit1)
fit2 <- lm(qualidade ~ aroma, data = wine); summary(fit2)
fit3 <- lm(qualidade ~ corpo, data = wine); summary(fit3)
fit4 <- lm(qualidade ~ sabor, data = wine); summary(fit4)
fit5 <- lm(qualidade ~ aromac, data = wine); summary(fit5)



# GRÁFICOS QUE PODEM SER USADOS NAS ANÁLISES
# Grafico de densidades comparando variáveis
ggplot(wine, aes(x = aroma)) +
  geom_density(fill = "#2E75B6", alpha = 0.4) +
  geom_density(aes(x = sabor), fill = "#C0504D", alpha = 0.4) +
  labs(title = "Densidade de Aroma vs Sabor",
       x = "Valor", y = "Densidade") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray90", linetype = "dashed"))

# Correlograma das variáveis
M <- cor(wine)
corrplot(M, method = "color", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, number.cex = 0.8,
         col = colorRampPalette(c("#A11D21","white","#2E75B6"))(200))
