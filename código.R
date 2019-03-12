# attach the package 'quantmod'
library(readxl)
library(quantmod)
library(AER)
library(forecast)

# Abrimos os dados do pacote


# load US macroeconomic data
USMacroSWQ <- read_xlsx("Data_StockWatson/us_macro_quarterly.xlsx",
                        sheet = 1,
                        col_types = c("text", rep("numeric", 9)))

colnames(USMacroSWQ) <- c("Date", "GDPC96", "JAPAN_IP", "PCECTPI", 
                          "GS10", "GS1", "TB3MS", "UNRATE", "EXUSUK", "CPIAUCSL")

# format date column
USMacroSWQ$Date <- as.yearqtr(USMacroSWQ$Date, format = "%Y:0%q")

# Inflação
INF <- xts(USMacroSWQ$CPIAUCSL, USMacroSWQ$Date)["1960::2013"]
INF.Taxa <- xts(400 * log(INF/lag(INF)))

# Desemprego
UNEMP <- xts(USMacroSWQ$UNRATE, USMacroSWQ$Date)["1960::2013"]
UNEMP.Taxa <- xts(400 * log(UNEMP/lag(UNEMP)))


# reproduce Figure 14.1 (b) of the book
plot(as.zoo(UNEMP),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "U.S. Unemployment Rate",
     ylim = c(0,max(UNEMP)))


plot(as.zoo(INF.Taxa),
     col = "red",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "U.S. Inflation Rate")


# compute logarithms, annual growth rates and 1st lag of growth rates
# É criada uma função que cria lags
quants <- function(series) {
  s <- series
  return(
    data.frame("Level" = s,
               "Logarithm" = log(s),
               "AnnualGrowthRate" = 400 * log(s / lag(s)),
               "1stLagAnnualGrowthRate" = lag(400 * log(s / lag(s))),
               "Variação na inflação" = 400 * log(s / lag(s)) - lag(400 * log(s / lag(s))))
  )
}


# Essa função mostra o lag, a taxa anual e a primeira diferença da taxa anual
quants(INF["1999::2000"])


## Usamos AFC() para tirar a autocorrelação

acf(na.omit(INF.Taxa), lag.max = 4, plot = F)
acf(na.omit(UNEMP), lag.max = 4, plot = F)


## Testar MQO
N <-length(INF.Taxa)
INF.TAXA.ols.1 <- ar.ols(INF.Taxa["1962::2009"],order.max = 1, demean = F, intercept = T)
INF.TAXA.ols.1 <- lm(INF.Taxa[-1] ~ INF.Taxa[-N])
coeftest(INF.TAXA.ols.1, type = "HC1")
# Forecast 
INF.Taxa.Nova <- data.frame("INF.Taxa_lags" = INF.Taxa[N-1])

INF.Taxa.Forecast <- forecast(ar.ols(INF.Taxa["1962::2009"],order.max = 1, demean = F, intercept = T))
forecast(INF.TAXA.ols.1)
summary(INF.TAXA.ols.1)$sigma
  
# modelo com p ordens
INF.TAXA.ols.2 <- ar.ols(INF.Taxa["1962::2009"],order.max = 2, demean = F, intercept = T)
INF.Taxa.Forecast.2 <- forecast(INF.TAXA.ols.2)

summary(INF.Taxa.Forecast.2)
INF.Taxa["2010:01"]-INF.Taxa.Forecast.2$mean[1]


# Tópico 14.3 - Dados de ativos
# Importa dados
SReturn <- read_excel("Data_StockWatson/Stock_Returns_1931_2002.xlsx")

# Transforma tempo em formato de meses por ano
SReturn$time <- as.yearmon(paste0(SReturn$time,":",SReturn$Month), "%Y:%m")

# Transforma em um XTS
StockReturns <- xts(x = SReturn[,3:4],order.by = SReturn$time)

# Estimaremos um AR(1), um AR(2) e um AR(4)
SR_AR1 <- ar.ols(x = StockReturns$ExReturn["1960::2000"], order.max = 1, demean = F, intercept = T)
SR_AR1

SR_AR2 <- ar.ols(x = StockReturns$ExReturn["1960::2000"],demean = F,intercept = T, order.max = 2)

y= StockReturns$ExReturn["1960:03::2000"]~StockReturns$ExReturn["1960:02::2000:11"]+StockReturns$ExReturn["1960::2000:10"]

coeftest(lm(y),df = "HC1")
SR_AR2

SR_AR4 <- ar.ols(x = StockReturns["1960::2000"], order.max = 4, demean = F, intercept = T)
SR_AR4


save(list = ls(), file = "dados14")
load("dados14")

