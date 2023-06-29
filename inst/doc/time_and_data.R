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

## -----------------------------------------------------------------------------
y_kor <- variable(data = c(12656, 13882, 14591, 15436, 16698, 18120, 19365, 20368, 19184, 21233, 22964, 23894, 25591, 26260, 27516, 28641, 29991, 31570, 32275, 32364, 34394, 35389, 36049, 37021, 37967, 38829, 39815, 40957, 41966, 42759, 42397, 44232),
                    startFrequency = f.yearly(1990),
                    name = "Korea, GDP per capita, PPP (constant 2017 international $), World Bank")

y_iri <- variable(data = c(9442, 10240, 10331, 10114, 9904, 10007, 10504, 10495, 10548, 10590, 11026, 11098, 11879, 12786, 13127, 13329, 13781, 14690, 14526, 14474, 15099, 15302, 14542, 14113, 14539, 14011, 14969, 15163, 14629, 14084, 14432, 15005),
                    startFrequency = f.yearly(1990),
                    name = "Iran, Rep., GDP per capita, PPP (constant 2017 international $), World Bank")

## ---- echo=FALSE, fig.width=6, fig.height=4, fig.cap="GDP per capita of IRI and KOR: Data and longrun trend over time"----
kor_rate_c <- get.longrun.growth(y_kor$data, TRUE, FALSE)
kor_rate_d <- get.longrun.growth(y_kor$data, FALSE, FALSE)
iri_rate_c <- get.longrun.growth(y_iri$data, TRUE, FALSE)
iri_rate_d <- get.longrun.growth(y_iri$data, FALSE, FALSE)
 
s_kor <- y_kor$data[1] * exp(kor_rate_c/100 * (0:(length(y_kor$data) - 1)))
s_iri <- y_iri$data[1] * exp(iri_rate_c/100 * (0:(length(y_iri$data) - 1)))
# s_kor_d <- data[1] * (1 + kor_rate_d) ^ (0:(length(y_kor) - 1))
# s_iri_d <- data[1] * (1 + iri_rate_d) ^ (0:(length(y_iri) - 1))

all <- c(s_kor, s_iri, y_kor$data, y_iri$data)
labels <- row.names(as.data.frame(y_kor))

# plot data
plot(labels, y_kor$data, type = "l", col="black", lwd = 2, lty = 1, 
     ylab = "PPP (constant 2017 international $)",
     xlab = "Time",
     cex.lab = 0.8,
     cex.axis = 0.8,
     ylim = c(min(all),max(all))*1.1)
lines(labels, y_iri$data, col = "blue", lwd = 2, lty = 1)

# plot trends
lines(labels, s_kor, col = "green", lwd = 1, lty = 3)
lines(labels, s_iri, col = "red", lwd = 1, lty = 3)

legend("topleft", legend = c("KOR", "IRI", 
                             sprintf("KOR Trend (con.=%.1f%%, dis.=%.1f%%",kor_rate_c, kor_rate_d), 
                             sprintf("IRI Trend (con.=%.1f%%, dis.=%.1f%%",iri_rate_c, iri_rate_d)),
       bg = "transparent", 
       col = c("black", "blue", "green", "red"),
       lwd = c(2,2,1,1),
       lty = c(1,1,3,3),
       bty = "n",
       cex = 0.8)

## -----------------------------------------------------------------------------
st <- remove.na.strategies(data = matrix(c(NA,2,NA,NA,5,6,7,8,9,NA,
                                           11,12,13,14,NA,NA,17,18,19,20),
                                         ncol = 4,
                                         byrow = TRUE))

## ----echo=FALSE---------------------------------------------------------------
first <- st[[1]]
second <- st[[2]]
fc1 = paste0(first$colRemove, collapse = ", ")
fr1 = paste0(first$rowRemove, collapse = ", ")
fc2 = paste0(second$colRemove, collapse = ", ")
fr2 = paste0(second$rowRemove, collapse = ", ")

