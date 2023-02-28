#Aufgabe 3
#für Aufgabenteil f
library(ggplot2)

#a
funktiona <- function(Alter) #(a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für metrische Variablen berechnet und ausgibt, hier am Beispiel "Alter", welche durch andere metrische Variablen ersetzt werden kann 
{summary (Alter) #gibt Minimum, Maximum, Median(2. Quartil), Mittelwert, erstes und drittes Quartil aus
  sd (Alter) # Standardabweichung
  var (Alter) #Varianz
  skew (Alter) #Schiefe, bei <0 -> linksschiefe Verteilung, bei >0 -> rechtsschiefe Verteilung
  IQR (Alter) #Interquartilsabstand, also 3. Quartil minus 1. Quartil
}

#b
funktionb <- function(Interesse_an_Mathematik) #(b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für kategoriale Variablen berechnet und ausgibt, hier am Beispiel "Interesse_an_Mathematik", welche durch eine andere kategorielle Variable ersetzt werden kann
{#Berechnung des Modus
  M <- table(sim_dat$Interesse_an_Mathematik) ## Häufigkeitstabelle von Variable Intersse_an_Mathematik erstellen aus dem Datensat sim_dat
  maximum <- max(M) ## das Objekt maximum enthält nun die größte Häufigkeit
  which(M == maximum) ## mit which können wir die Ausprägungen von M erhalten, die die größte Häufigkeit aufweisen
}

#c
#Funktion zur Berechnung fuer den Zusammenhang zweier bivariater kategorieller Variablen
cramers_V <- function(x,y){  #Cramers V als Index fÃ¼r Korrelation
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
  # ab 0.4 mÃ¤ÃŸiger Zusammenhang ab 0.8 starker Zusammenahng
}


#d
#Funktionen fuer den Zusammenhang einer metrischen und dichotomen Variable 
r_pbis <- function(x,y){ #Punkt biseriale Korrelation, nimmt Werte zwischen -1 und 1 an
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
funktione <- function(x) {#eine Fuktion die eine mindestens ordinal skalierte Variable anhand der quantile kategorisiert und einen Faktor zurück gibt.
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
funktionf<- function(data, var1, var2, var3   , var4 = NULL) {
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


#Hilfsfunktion zur Berechnung von cramers_V()
chiquadrat_rechner <- function(x,y){ #Chi Quadrat Formel
  if(is.null(x) || is.null(y)){
    stop("Error: Einer der beiden Variablen ist leer")
  } # Abbruchkriterium
  chi <- 0
  dummy <- table(x,y) #Kreuztabelle wird erzeugt zwischen x und y
  rs <- rowSums(dummy)  #Summen der einzelnen Zeilen der Tabelle
  cs <- colSums(dummy)  #Summen der einzelnen Spalten der Tabelle
  
  for (i in 1:length(rs)) { #Allgemeine Rechnenformel fÃ¼r Chi Quadrat 
    for(j in 1:length(cs)){
      chi <- chi + ((rs[i] * cs[j])/length(x) - dummy[i,j])^2/((rs[i] * cs[j])/length(x))
    }
  }
  return(chi) #Output
}


                 
                 