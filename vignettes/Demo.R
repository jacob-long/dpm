## ------------------------------------------------------------------------
library(clfe)
wages <- foreign::read.dta("http://www3.nd.edu/~rwilliam/statafiles/wages.dta")

## ------------------------------------------------------------------------
wages <- panel_data(wages, id = id, wave = t)

## ------------------------------------------------------------------------
cl <- clfe(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages, err_inv = T,
           information = "observed")
summary(cl)

## ------------------------------------------------------------------------
cl <- clfe(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages, err_inv = F,
           information = "observed")
summary(cl)

## ------------------------------------------------------------------------
cl <- clfe(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages, const_inv = T,
           information = "observed")
summary(cl)

## ------------------------------------------------------------------------
cl <- clfe(wks ~ lag(union) + lag(lwage) | ed, data = wages,
           information = "observed")
summary(cl)

## ------------------------------------------------------------------------
cl <- clfe(wks ~ pre(lag(union)) + pre(lag(lwage)) | ed, data = wages,
           information = "observed")
summary(cl)

