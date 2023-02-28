#Funktion zur Berechnung für den Zusammenhang zweier bivariater kategorieller Variablen
cramers_V <- function(x,y){  #Cramers V als Index für Korrelation
  if(is.null(x) || is.null(y)){
    stop("Error: Einer der beiden Variablen ist leer")
  } # Abbruchkriterium
  anzSpalten <- length(colSums(table(alter,studienfach))) # Anzahl an Spalten in der
  anzZeilen <- length(rowSums(table(alter,studienfach)))  # Tabelle
#Allgemeine Formel von Cramers v: 
#Chi_squared/ (Anzahl Beobachtungen * Minimum(Zeilen,Spalten) - 1)
    v <- sqrt(chiquadrat_rechner(x,y)/(length(x) * # Allgemeine Formel von Cramers V
              (min(anzSpalten,anzZeilen) - 1)))
  return(v) #Output zwischen 0 und 1
  # 0 -> kein Zusammenhang, ab 0.1 schwacher Zusammenhang,
  # ab 0.4 mäßiger Zusammenhang ab 0.8 starker Zusammenahng
}


#Funktionen für Zusammenhang einer metrischen und dichotomen Variable 
r_pbis <- function(x,y){ #Punkt biseriale Korrelation, nimmt Werte zwischen -1 und 1 an
  if(any(x > 1) || any(x < 0)) {
    stop("Error: x muss die dichotome Variable sein und aus 0 und 1 bestehen!")
  } #Ausnahmefall.
  yx_0 <- mean(y[x == 0]) #Mittelwert von y wo x = 0 ist
  yx_1 <- mean(y[x == 1]) #Mittelwert von y wo x = 1 ist 
  # Allgemeine Formel für die Rechnung von Punktbiserialen Korrelation
  r <- (yx_1 - yx_0)/sd(y) * sqrt(length(x[x == 0])*length(x[x == 0]))/length(x)
  return(r) #Output zwischen -1 und 1, sonst Fehler.
  # -1 starker neg. Zusammenhang, 0 keinen, 1 starker pos. Zusammenhang
}
       
