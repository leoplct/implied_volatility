#' ---
#' title: "Volatilità Implicità"
#' author: "Leonardo Pellicciotta"
#' ---

#' <h2>1. Effetto Smile </h2>
#' <h2>2. Volatilità implicita e storica </h2>
#' <h2>3. Utilizzo della volatilità implicità nel calcolo del Beta </h2>
#' 
#' <hr />

#' <h1> 1. Effetto Smile </h1>
#' L’effetto “Smile” è un pattern che emerge nei valori della volatilità implicita del prezzo delle opzioni.
#' Per una data scadenza, le opzioni che hanno uno “strike price” lontano dal prezzo corrente di mercato del sottostante presentano un prezzo, e perciò una volatilità implicità, più alta di quella che è suggerita dai modeli standard per i prezzi delle opzioni.
#'
#' Questo fenomeno si è iniziato a riscontrare dopo il Crash del 1987 ad indicare che gli investitori hanno iniziato a valutare la probabilità di eventi sulle code con prezzi più alti per opzioni out-of-the-money.
#' Questo fenomeno empirico dimostra le carenze del modello di Black-Sholes il quale assume una volatilità costante ed una distribuzione log-normale dei rendimenti del sottostante.
#' 
#' Si parla di superficie di volatilità nel grafico 3D in cui viene valutata la volatilità implicità per tutti gli strikes e per tutte le opzioni scambiate sul mercato (quindi per tutte le maturities).

#' <h4> Superficie di Volatilità </h4>

#+ Including libraries, message=FALSE
library(plotly)
library(fOptions)
library(quantmod)
library(devtools) #install.packages("devtools")
library(Quandl) #install_github('quandl/R-package')


#' S&P 500 (Prices)
##########################
# SPDR S&P 500 ETF (SPY)
symbol <- 'SPY'

prices<- Quandl("YAHOO/INDEX_SPY.4", start_date="2004-01-01", type="zoo")
plot_ly(y=prices, x=time(prices))

#' Download options data
Data <- getOptionChain(symbol, '2015')
r     =  0.0051 #risk-free
b     =  r #dividend yield 
expires <- names(Data)
expires

#' Esempio dati Opzioni
head(get(expires[1], Data)$calls)


#' L'array Strikes contiene tutti gli Strikes per tutte le scadenze
Strikes = NULL
for(expire in expires){
   calls <- get(expire, Data)$calls
    for(s in 1:nrow(calls)){
      Strikes <- append(Strikes, calls[s,]$Strike)
    }  
}

Strikes <- sort(unique(Strikes))#Ordino ed elimino i duplicati


#' Vengono create due Matrici S x M. Una per le Opzioni CALLs, l'altra per le PUTs.\n
#' Nelle righe ci sono tutti gli Strike, e nelle colonne tutte le Maturities.\n
#' Ciascuna celle conterrà il valore della volatilità implicita calcolata con il Modello di Black & Scholes\n

# CALLs
iv_calls <- matrix(0, nrow=length(Strikes), ncol=length(expires))
colnames(iv_calls) <- expires
rownames(iv_calls) <- Strikes

# PUTs
iv_puts <- matrix(0, nrow=length(Strikes), ncol=length(expires))
colnames(iv_puts) <- expires
rownames(iv_puts) <- Strikes
#iv["315", "Dic.11.2015"] <- 100


#' La Matrice delle Volatilità Implicite viene inzializzata a 0. 
#' Dopodichè viene ri-scansionata e per ciascuna cella viene calcolato il valore della volatilità implicita

for(expire in expires){
  for(s in Strikes){
    
    strike <- as.double(s)
    
    # Get all options
    calls <- get(expire, Data)$calls
    call  <- calls[ which(calls$Strike ==strike),]
    
    puts <- get(expire, Data)$puts
    put  <- puts[ which(puts$Strike ==strike),]
    
    #Options price
    price_call <- call$Last
    price_put  <- put$Last
    
      
    # Calculate Maturity
    expiration <- strptime(expire, "%b.%d.%Y ")
    diff_in_days = as.double( difftime(expiration, Sys.time(), units = "days") ) # days
    Time  =  diff_in_days/365
    
    if(Time <= 0){ #accade il giorno della scadenza dell'opzione
      next
    }
    
    # Tasso risk-free giornaliero composto per i giorni fino alla maturity
    r_time <- ((1+r)^(1/365) -1)*diff_in_days
    
    # Prezzo del sottostante
    underlying_price <- last(prices)[[1]]
    
    #########################
    ## IMPLIED VOLATILITY
    
    ## CALLs
    
    if(length(price_call) > 0 && strike >= underlying_price){
      
#       print(price_call)
#       print(underlying_price)
#       print(strike)
#       print(Time)
#       print(r_time)
      
      implied_volatility_call <- GBSVolatility(price_call, "c", underlying_price, strike, Time, r_time, r_time)
      
      if(implied_volatility_call > 0.01){ #elimino valori troppo bassi
        
        #Assegno alla cella (Strike, Scadenza) il valore della IV
        
        iv_calls[as.character(strike), as.character(expire)] <- implied_volatility_call
      
      }
    }
    
    # PUTs
    
    if(length(price_put) > 0  && strike <= underlying_price){
      
      implied_volatility_put <- GBSVolatility(price_put, "p", underlying_price, strike, Time, r_time, r_time)
      
      #elimino valori troppo bassi
      if(implied_volatility_put > 0.01){
        
        #Assegno alla cella (Strike, Scadenza) il valore della IV
        
        iv_puts[as.character(strike), as.character(expire)] <- implied_volatility_put
      }
    }
  }
}


################
## Plot

#' <h5>Volatility surface (Put)
plot_ly(z = iv_puts, x=expires, y=Strikes, type = "surface")
#' <h5>Volatility surface (Call)
plot_ly(z = iv_calls, x=expires, y=Strikes, type = "surface")

#' <p></p><p></p>
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
