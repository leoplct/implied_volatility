#+ Including libraries, message=FALSE
library(plotly)
library(fOptions)
library(quantmod)
library(devtools) #install.packages("devtools")
library(Quandl) #install_github('quandl/R-package')

#' <h2> 2. Volatilità Implicita e Volatilità storica</h2>
#' <p> In questo esempio viene calcolata la volatilità storica annualizzata e viene poi
#' confrontata con la volatilità implicità.</p>

#' Volatilità Storica (S&P 500)
returns <- diff(log(prices))
rolling_sd <- rollapply(returns,21,sd,fill=0)*sqrt(252)
rolling_sd <- na.omit(rolling_sd)
plot_ly(y=rolling_sd, x=time(rolling_sd), name="Volatilità storica (std)")

#' <h4> VIX </h4>
#' <p>Per il calcolo della Volatilità Implicità verrà utilizzato l'indice VIX 
#' che misura in % la volatiltià implicita delle opzioni a 30gg</p>

vix <- Quandl("CBOE/VIX.4", start_date=first(index(returns)), type="zoo")
plot_ly(y=vix/100, x=time(vix), name="VIX")


#' Scatter Plot
rolling_sd <- as.zoo(rolling_sd)
vix_lagged <- lag(vix, 21)
volatility <- na.omit(merge(rolling_sd, vix_lagged))

plot_ly(y=volatility$rolling_sd, x=volatility$vix_lagged, type="scatter", mode="markers")

#' In questo Scatter Plot vengono mostrati i rendimenti del VIX confrontati
#' con la volatilità passata dei 30gg precedenti.
#' La volatilità dello S&P500 è misurata come la deviazione standard dei rendimenti giornalieri
#' per il periodo di 30gg e poi annualizzata



#' <h2> 3. Calcolo del Beta utilizzando la Volatilità implicita </h2>
#' <p> Il beta (β) è il coefficiente che misura il comportamento di un titolo rispetto al mercato, ovvero la variazione che un titolo storicamente assume rispetto alle variazioni del mercato. 
#' Matematicamente, esso è il rapporto tra la covarianza tra i rendimenti dell'asset i-esimo e i rendimenti del portafoglio di mercato e la varianza dei rendimenti di mercato (δm) </p>
#' <p> A tal proposito si è ritenuto che potesse essere usata la volatilità implicità
#' delle opzioni per avere una migliore stima del Beta in quanto viene costruito con informazioni
#' che guardano ad informazioni future anzichè passate.</p>
#' <p> Per lo scarto quadratico del titolo verrà usata la sua Volatilità Implicità, mentre per lo scarto quadratico
#' del Portafoglio di Mercato verrà usata quella dello S&P 500 (VIX). Verranno poi confrontati i due Beta (storico e implicito)</p>
#' <p>Titolo: <strong>United Stated Oil</strong> (USO)</p>
#' <p>Mercato:<strong>S&P 500</strong></p>
#' Per il titolo esiste un indice costruito da CBOE che riporta giornalmente i valori della
#' volatilità implicità 

# Titolo
crude_oil    <- Quandl("GOOG/NYSE_USO.4", start_date="2008-01-01", type="zoo")
crude_oil_iv <- Quandl("CBOE/OVX", start_date="2008-01-01",type="zoo")

# Mercato
sp500        <- Quandl("YAHOO/INDEX_GSPC.4", start_date="2008-01-01", type="zoo")
sp500_iv     <- vix

## Log-returns
crude_oil_returns <- diff(log(crude_oil))
sp500_returns <- diff(log(sp500))

##########
## Plot
#' <h4> Crude Oil / S&P500 </h4>
#p<-plot_ly(y = cumsum(na.omit(Delt(crude_oil))), x = index(crude_oil), name="Crude Oil")
#add_trace(p, y = cumsum(na.omit(Delt(sp500))), name="S&P 500")
p<-plot_ly(y = cumsum(crude_oil_returns), x = index(crude_oil), name="Crude Oil")
add_trace(p, y = cumsum(sp500_returns), name="S&P 500")

# Non tutti i vettori di dati hanno la stessa lunghezza perciò vengono ridotti al minimo comune multiplo
max_common_data_legth_available <- min(
  length(crude_oil),
  length(sp500),
  length(crude_oil_iv),
  length(sp500_iv)
)

# Utilizzo solo gli ultimi n-dati
crude_oil <- tail(crude_oil, max_common_data_legth_available)
sp500     <- tail(sp500, max_common_data_legth_available)
crude_oil_iv <- tail(crude_oil_iv, max_common_data_legth_available)
sp500_iv     <- tail(sp500_iv, max_common_data_legth_available)


# Faccio un Merge delle serie temporali per allinearle sullo stesso indice temporale ed approssimo i dati mancanti
df <- merge(crude_oil, crude_oil_iv, sp500, sp500_iv, all=TRUE)
df <- na.approx(df)


# Historical data per il calcolo per Beta classico
crude_oil_sd <- rollapply(crude_oil_returns, width=30, sd, fill=0)*sqrt(252)
sp500_sd     <- rollapply(sp500_returns, width=30, sd, fill=0)*sqrt(252)
rolling_rho  <- rollapply( zoo(cbind(crude_oil_returns,sp500_returns)), width=30 ,function(x) cor(x[,1],x[,2]), by.column=FALSE)


#' <h4> Crude Oil Implied Volatility vs. Historical Volatility </h4>
#' Prima del calcolo del Beta vengono mostrate le due Volatilità (storica ed implicità) sul titolo Crude Oil
plot(lag(crude_oil_iv, 21), col="black")
lines(crude_oil_sd*100, col="green")

#' Scatter plot (Crude oil)
lag_crude_oil_iv <- lag(crude_oil_iv,21, na.pad = TRUE)
scatter_iv_v <- plot_ly(y=crude_oil_sd*100, x=lag_crude_oil_iv, mode="markers")
scatter_iv_v

#' Il Beta è definito come il rapporto della covarianza (tra titolo e mercato) sulla varianza dei rendimenti del mercato
#Beta
beta <- function(stock_sd, market_sd, rho){
    (rho*stock_sd*market_sd)/(market_sd^2)
}

# Historical Beta & Implied Beta
historical_beta <- beta(crude_oil_sd, sp500_sd, rolling_rho)
implied_beta    <- beta(df$crude_oil_iv, df$sp500_iv, rolling_rho) 

# Dataframe per plotting
betas <- merge(historical_beta,implied_beta, all=TRUE)
betas <- na.approx(betas)
betas <- na.omit(betas)

#' <h4> Historical Beta vs. Implied Beta </h4>
beta_plot <- plot_ly(
  y=betas$implied_beta, 
  x=time(betas), 
  name="Implied Beta"
)
beta_plot<-add_trace(beta_plot, y=betas$historical_beta, name="Historical Beta")
beta_plot

#' <p> I due Beta assumono valori circa uguali nella maggior parte del tempo.
#' Hanno differito di molto nell'Aprile 2011 ed Agosto 2014 </p>

beta_diff <- betas$implied_beta-betas$historical_beta
p <- plot_ly(y=beta_diff, x=time(beta_diff))
p

#' L'implied Beta non sembra essere una buona stima per il Beta futuro
lag_implied_beta <- lag(betas$implied_beta,21, na.pad = TRUE)
scatter_ib_b <- plot_ly(y=betas$historical_beta, x=lag_implied_beta, mode="markers")
scatter_ib_b

#' Tuttavia l'Implied Beta sembra essere una buona stima dell'historical Beta nel presente
scatter_ib_b <- plot_ly(y=betas$historical_beta, x=betas$implied_beta, mode="markers")
scatter_ib_b
