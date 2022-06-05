library("corrplot")
# Lectura de datos
dataset <- read.csv("D:/DATOS MASTER/winequality-red.csv", header = TRUE)
head(dataset)
#Tipo de datos de cada atributo
sapply(dataset, function(x) class(x))
#Varificación de valores vacios
sapply(dataset, function(x) sum(is.na(x)))
#Valores extremos
boxplot.stats(dataset$fixed.acidity)$out
boxplot.stats(dataset$volatile.acidity)$out
boxplot.stats(dataset$citric.acid)$out
boxplot.stats(dataset$residual.sugar)$out
boxplot.stats(dataset$chlorides)$out
boxplot.stats(dataset$free.sulfur.dioxide)$out
boxplot.stats(dataset$total.sulfur.dioxide)$out
boxplot.stats(dataset$density)$out
boxplot.stats(dataset$pH)$out
boxplot.stats(dataset$sulphates)$out
boxplot.stats(dataset$alcohol)$out
boxplot.stats(dataset$quality)$out
#Comprobacion de la normalidad
shapiro.test(dataset$fixed.acidity)
shapiro.test(dataset$volatile.acidity)
shapiro.test(dataset$citric.acid)
shapiro.test(dataset$residual.sugar)
shapiro.test(dataset$chlorides)
shapiro.test(dataset$free.sulfur.dioxide)
shapiro.test(dataset$total.sulfur.dioxide)
shapiro.test(dataset$density)
shapiro.test(dataset$pH)
shapiro.test(dataset$sulphates)
shapiro.test(dataset$alcohol)
shapiro.test(dataset$quality)
#Correlacion de variables
cor(dataset, method = "spearman")
corrplot(cor(dataset, method = "spearman"))
#Regresion lineal
ntrain <- nrow(dataset)*0.8
ntest <- nrow(dataset)*0.2
set.seed(1)
index_train <- sample(1:nrow(dataset), size = ntrain)
train <- dataset[index_train,]
test <- dataset[-index_train,]
#Modelo completo (todas la variables)
model_full <- lm(formula = quality~., data = train)
summary(model_full)
#Modelo parcial (variables mas correladas)
model_par <- lm(formula = quality~volatile.acidity+citric.acid+sulphates+alcohol, data = train)
summary(model_par)
#Verificando prediccion
pre_calidad <- predict(model_full, test, type="response")
comparacion <- data.frame(Original = test$quality, Predicho = as.integer(pre_calidad), 
                          Igual = (test$quality-as.integer(pre_calidad)))
head(comparacion)

resultados <- data.frame(Correctos = sum(comparacion$Igual == 0), Incorrectos = sum(comparacion$Igual != 0) )
resultados

