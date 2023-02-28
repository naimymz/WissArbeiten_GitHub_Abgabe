#Aufgabe 3
#für Aufgabenteil f
library(ggplot2)

#a
funktion_a<- function(x) #(a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für metrische Variablen berechnet und ausgibt, 
{
  erg <- data.frame(c(summary(x)))
  erg <- rbind(erg,standartabweichung =sd(x),varianz = var(x),Interquantilsabstand = IQR(x))
  return(erg)
}

#b
funktion_b <- function(var) {
  
  # Berechnung der Häufigkeitstabelle
  tab <- table(var)
  
  # Berechnung des Modus
  mode <- names(which.max(tab))
  
  # Berechnung der Anzahl der verschiedenen Ausprägungen
  num_levels <- length(tab)
  
  # Ausgabe der Ergebnisse
  cat("Häufigkeitstabelle:\n")
  print(tab)
  cat("\nModus:", mode)
  cat("\nAnzahl der verschiedenen Ausprägungen:", num_levels)
  
}


#c
#Funktion zur Berechnung fuer den Zusammenhang zweier bivariater kategorieller Variablen
funktion_c <- function(x,y){  #Cramers V als Index fuer Korrelation
  if(is.null(x) || is.null(y)){
    stop("Error: Einer der beiden Variablen ist leer")
  } # Abbruchkriterium
  anzSpalten <- length(colSums(table(x,y))) # Anzahl an Spalten in der
  anzZeilen <- length(rowSums(table(x,y)))  # Tabelle
  #Allgemeine Formel von Cramers v: 
  #Chi_squared/ (Anzahl Beobachtungen * Minimum(Zeilen,Spalten) - 1)
  v <- sqrt(chiquadrat_rechner(x,y)/(length(x) * # Allgemeine Formel von Cramers V
                                       (min(anzSpalten,anzZeilen) - 1)))
  return(v) #Output zwischen 0 und 1
  # 0 -> kein Zusammenhang, ab 0.1 schwacher Zusammenhang,
  # ab 0.4 maessiger Zusammenhang ab 0.8 starker Zusammenahng
}


#d
#Funktionen fuer den Zusammenhang einer metrischen und dichotomen Variable 
funktion_d <- function(x,y){ #Punkt biseriale Korrelation, nimmt Werte zwischen -1 und 1 an
  if(any(x > 1) || any(x < 0)) {
    stop("Error: x muss die dichotome Variable sein und aus 0 und 1 bestehen!")
  } #Ausnahmefall.
  yx_0 <- mean(y[x == 0]) #Mittelwert von y wo x = 0 ist
  yx_1 <- mean(y[x == 1]) #Mittelwert von y wo x = 1 ist 
  # Allgemeine Formel fÃ¼r die Rechnung von Punktbiserialen Korrelation
  r <- (yx_1 - yx_0)/sd(y) * sqrt(length(x[x == 0])*length(x[x == 0]))/length(x)
  return(r) #Output zwischen -1 und 1, sonst Fehler.
  # -1 starker neg. Zusammenhang, 0 keinen, 1 starker pos. Zusammenhang
}

#e
funktion_e <- function(x) {#eine Fuktion die eine mindestens ordinal skalierte Variable anhand der quantile kategorisiert und einen Faktor zurück gibt.
  # x: die zu kategorisierende Variable
  
  #Quantile berechnen
  q <- quantile(x, probs = seq(0, 1, 1/3))
  #die unterste Quantilsgrenze herabsetzten, damit der kleinste Wert kategorisiert wird
  q[1] <- q[1]-1
  
  # kategorisieren in hoch, mittel, niedrig
  einteilung <- cut(x, breaks = q, labels = c("niedrig","mittel","hoch"))
  
  # Rückgabe 
  return(einteilung)
}


#f
funktion_f<- function(data, var1, var2, var3   , var4 = NULL) {
  #eine Funktion die mehrere kategoriale variablen visualisiert
  #die  zweite Variable als Faktor darstellen
  var3 <- as.factor(var3)
  #data: Dataframe mit den zu plottenden Variablen
  #var1, var2, var3, var4: Namen der Variablen
  
  #Plot erstellen
  plot <- ggplot(data, aes(x = var1, y = var3)) +
    geom_count(position = "dodge") + facet_grid(var2)
  labs(x = var1, y = "Count")
  
  #Plot ausgeben
  print(plot)
}




                 
                 