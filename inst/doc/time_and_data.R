## ----library------------------------------------------------------------------
library(tdata)

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  oil_price <- Quandl::Quandl("OPEC/ORB", start_date="2010-01-01")

## ----eval=TRUE, include=FALSE-------------------------------------------------
oil_price <- tdata::oil_price

## -----------------------------------------------------------------------------
start_freq <- f.list.date(oil_price$Date)

## -----------------------------------------------------------------------------
var_dl <- variable(oil_price$Value, start_freq, "Oil Price")

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  print(var_dl)

## ----echo=FALSE---------------------------------------------------------------
invisible(print(var_dl))

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  df_var_dl <- as.data.frame(var_dl)

## -----------------------------------------------------------------------------
var_daily <- convert.to.daily(var_dl)

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  print(var_daily$startFrequency)

## ----echo=FALSE---------------------------------------------------------------
invisible(print(var_daily$startFrequency))

## -----------------------------------------------------------------------------
class_id <- get.class.id(var_daily$startFrequency)
str_rep <- as.character(var_daily$startFrequency)

## ----echo=FALSE---------------------------------------------------------------
invisible(print(paste0("class_id: ", class_id, ", str_rep: ", str_rep)))

## -----------------------------------------------------------------------------
var_weekly <- convert.to.weekly(var_daily, "mon", "last")

## ----results='hide', fig.cap='Plotting the generated weekly data',fig.show='hold',fig.width=6----
df_var_weekly <- as.data.frame(var_weekly)
par(las = 2, cex.axis = 0.8)
plot(factor(rownames(df_var_weekly)), 
     df_var_weekly$`Oil Price`, 
     xlab = NULL, ylab = "$", 
     main = "Weekly Oil Price")

