#library(dplyr)
rm(list = ls())

# define bonds' properties
bonds <- list(
  t_note = list(100, 1 / 100, 10, 1.5 / 100, 104.44, 0.2),
  RR_6 = list(100, 2.79 / 100, 6, 3.625 / 100, 102.719, 0.2),
  Boeing = list(100, 3.17 / 100, 18, 3.55 / 100, 108.82, 0.4),
  Google = list(100, 2.26 / 100, 4, 3.375 / 100, 107.90, 0.2)
)



portfolio<-function(bonds){
tryCatch(  expr = {
  
  
  # define bond pricing function

  bondprc <- function(p, r, ttm, y) {
    cf <- c(rep(p * r, ttm - 1), p * (1 + r))
    cf <- data.frame(cf)
    cf$t <- as.numeric(rownames(cf))
    cf$pv_factor <- 1 / (1 + y) ^ cf$t
    pv <- cf$cf %*% cf$pv_factor
    return(pv)
  }
  
  
  names <-
    c("par", "ytm", "t to maturity", "coupon rate", "price", "weight")
  

  # assgn  bonds' properties their respective names
  name_bonds <- lapply(bonds, function(x)
    setNames(x, names))
  
  # check sum of shares  is 1
  p <- sum(as.numeric(purrr::map(name_bonds, 'weight')))
  #  25 basis point scebario
  delta_ytm <- 0.25 / 100
  
  # approximate modified duration per each bond
  MD <-
    mapply(
      `-`,
      lapply(name_bonds, function(x)
        bondprc(x[["par"]], x[["coupon rate"]], x[["t to maturity"]], x[["ytm"]] - delta_ytm) /
          (2 * delta_ytm * x[["price"]])) ,
      
      lapply(name_bonds, function(x)
        bondprc(x[["par"]], x[["coupon rate"]], x[["t to maturity"]], x[["ytm"]] + delta_ytm) /
          (2 * delta_ytm * x[["price"]]))
    )
  
  
  #portfolio modified duration (weighted average)
  p_dur <- MD %*%  as.numeric(purrr::map(name_bonds, 'weight'))
  
  #portfolio price (weighted average)
  p_price <-
    as.numeric(purrr::map(name_bonds, 'price'))  %*%  as.numeric(purrr::map(name_bonds, 'weight'))
  
  #portfolio yield (weighted average)
  p_ytm <-
    as.numeric(purrr::map(name_bonds, 'ytm'))  %*%  as.numeric(purrr::map(name_bonds, 'weight'))
  
  # percentage censitivity to bp change in ytm
  sens <- -p_dur * (-delta_ytm)
  dollar_sens <- sens * p_price
  
  # resulting $ price of the portfolio (if change in ytm) 
  p_delta <- p_price + dollar_sens
  
 cat( "Portfolio allocation", paste(paste(100 * as.numeric(
 purrr::map(name_bonds, 'weight') ), '%') , names(name_bonds)) , "Portfolio
 value", paste(round(p_price, 2), '$'), "Portfolio yield", paste(round(100 *
 p_ytm, 2), '%'), "Portfolio  sensitvity to 25 basis point change in yield",
 paste(round(100 * sens, 2), '%'), "Portfolio duration", paste(round(p_dur,
 2), "years"), fill = 3 )
  return(p)
},
error = function(e) {
  if (p != 1) {
    print(p)
    message(e)
  }
  
  
})
}

portfolio(bonds)
