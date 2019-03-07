# attach the package 'quantmod'
library(readxl)
library(quantmod)
library(AER)

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
               "1stLagAnnualGrowthRate" = lag(400 * log(s / lag(s))))
  )
}


# Essa função mostra o lag, a taxa anual e a primeira diferença da taxa anual
quants(INF["2004::2005"])


## Usamos AFC() para tirar a autocorrelação

acf(na.omit(INF.Taxa), lag.max = 4, plot = F)
acf(na.omit(UNEMP), lag.max = 4, plot = F)

save(list = ls(), file = "dados14")
load("dados14")

