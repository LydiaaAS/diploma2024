rm(list = ls())

library(haven) # <- enables R to read and write various data formats
library(vtable)
library(stargazer) # <- publication quality tables
library(ggpubr)  # <- plots ("ggscatter" & others)
library(lattice) # <- xyplot
library(plotly)  # <- graphing library ("plot_ly")
library(dplyr) # <- data manipulation
library(psych)
install.packages('readxl')
library (readxl)

#загрузка данных
data <- read_excel('C:\\Users\\Lida\\Desktop\\мага диплом\\Данные\\R tests\\2002-2020 num.xlsx')

summary(data)

describe(data)

str(data)

head(data)
tail(data)

#убираем колонку со странами, чтобы не мешала делать анализ
data <- select(data, -c(Country)
head(data) 

#фактические коэффициенты линейной корреляции для каждой пары переменных:

install.packages("GGally")
library(GGally)
ggpairs(data) 

pairs(data) 

hist(data$'INVNORM')

cor(data)


#регрессия

#1
model <- lm(INVNORM ~ YEAR + CARBONTAX + CARBONTRADE + TEMP + REGION + REGION + PARISAGR, data = data)
hist(residuals(model), col = "steelblue")
summary(model)
car::vif(model)

#2
model <- lm(INVNORM ~ YEAR + GDPPC + GDPPER + CARBONTAX + CARBONTRADE + TEMP + REGION + REGION + PARISAGR, data = data)
hist(residuals(model), col = "steelblue")
summary(model)

#3
model <- lm(INVNORM ~ YEAR + ENERGYPC + CARBONTAX + CARBONTRADE + TEMP + REGION + REGION + PARISAGR, data = data)
hist(residuals(model), col = "steelblue")
summary(model)

#4
model <- lm(INVNORM ~ YEAR + ENERGYPC + CO2SHARE + TEMP + REGION + REGION + PARISAGR, data = data)
hist(residuals(model), col = "steelblue")
summary(model)

#5
model <- lm(INVNORM ~ YEAR + GDPPC + GDPPER + ENERGYPC + CO2SHARE + TEMP + REGION + REGION + PARISAGR, data = data)
hist(residuals(model), col = "steelblue")
summary(model)

#fixed model
library(plm)
fixed <- plm(INVNORM ~ YEAR + CARBONTAX + CARBONTRADE + TEMP + REGION + REGION + PARISAGR, data = data, index=c("COUNTRY"), model="within") 
summary(fixed)


library(plm)
fixed <- plm(INVNORM ~ CARBONTAX + CARBONTRADE + TEMP + REGION + REGION + PARISAGR, data = data, index=c("COUNTRY", "YEAR"), model="within") 
summary(fixed)



