# 1. biblioteki
library(tidyverse)
library(gplots)
library(rpart)
library(randomForest)

# 3. wczytanie danych i 4. przetworzenie brakujących danych
sledzie <- read.csv("sledzie.csv", na.strings = "?")
# sledzie <- sledzie[, -1] # usuwam pierwszy wiersz z numeracja wierszy od 0 do n-1

# 5. rozmiar zbioru i statystyki
dim(sledzie)
str(sledzie)
summary(sledzie)

# sprawdz braki, ile ich jest
apply(sledzie, 2, function(x) sum(is.na(x)))

# sprawdz braki, jaki procent
apply(sledzie, 2, function(x) sum(is.na(x))/length(x)) # ok 0,03%

# czym zastapic braki? moze srednia?
sledzie <- data.frame(apply(sledzie, 2, function(x) {
  ind <- which(is.na(x))
  srednia <- mean(x, na.rm = TRUE)
  x[ind] <- srednia
  x
}))

# a moze pominac i uzyc complete obs przy cor() lub sledzie <- na.omit(sledzie)
sledzie <- na.omit(sledzie)


# 6. prezentacja rozkladow wartosci
for (i in colnames(sledzie)) {
  print(
    ggplot(data = sledzie,
           aes_string(
             x = i
           )) +
      geom_density(fill = "lightskyblue") +
      ggtitle(i)
  )
}

# 7. korelacja miedzy zmiennymi
# zmienne oprocz length nie maja rozkladu normalnego wobec czego stosuje wspolczynnik korelacji spearmana
korelacja <- cor(sledzie, method = "spearman", use = "complete.obs")

my_palette <- colorRampPalette(c("green", "black", "red"))(n = 30)

# rysuje heatmape korelacji
heatmap.2(korelacja,
          col = my_palette,
          trace = "none",
          main = "Heatmapa korelacji",
          cellnote = round(korelacja, 2),
          notecol = "grey")

# 8. interaktywny wykres lub animacja prezentujaca zmiane rozmiaru sledzi w czasie
# poki co wykres nieinteraktywny

ggplot(data = sledzie,
       aes(x = X,
           y = length)) +
  geom_point() +
  ggtitle("Wykres przedstawiajacy zmiane rozmiaru złowionego śledzia w czasie") +
  geom_smooth()


# 9. regresor przewidujacy rozmiar sledzia
fit <- lm(length ~ ., data = sledzie)
summary(fit)

# wyciagam R^2
summary(fit)$r.squared 

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}
rmse(fit$residuals)

# drzewo regresyjne
fit2 <- rpart(length ~ ., data = sledzie, method = "anova")
printcp(fit2) # wyniki
plotcp(fit2) # wizualizacja cross-validation
print(fit2) # nudy
summary(fit2) # podsumowanie splitow
par(mfrow = c(1, 2))
rsq.rpart(fit2) # jak zmienia sie r^2 przy danej ilosci splitow
par(mfrow = c(1, 1))
plot(fit2, uniform = TRUE, main = "Drzewo regresji dla rozmiaru poławianego śledzia")
text(fit2, use.n = TRUE, cex = 1)

# random forest
fit3 <- randomForest(length ~ ., data = na.omit(sledzie))
print(fit3)
waznosc <- importance(fit3)
waznosc <- data.frame(zmienna = rownames(waznosc), wartosc = waznosc[, 1])
waznosc$zmienna <- factor(waznosc$zmienna, levels = waznosc[order(waznosc$wartosc), "zmienna"])

ggplot(waznosc,
       aes(x = zmienna,
           y = wartosc)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Waznosc zmiennych dla randomForest")


# najwazniejsza zmienna sst
# rysuje scatterplot
ggplot(data = sledzie,
       aes(x = length,
           y = sst)) +
  geom_point() +
  geom_smooth(method = "lm")

cor.test(sledzie$length, sledzie$sst)

# Analiza waznosci atrybutow najlepszego znalezionego modelu
# Pyt "Co sprawia ze rozmiar sledzia zaczal w pewnym momencie malec"
# Problem: Rozmiar sledzia pozostawal na podobnym poziomie, nie malal w czasie

