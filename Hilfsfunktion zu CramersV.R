# R-Skript Hilfsfunktion zu Funktion um Zusammenhang zwei bivariater 
# kategorieller Variablen bestimmen.

#Hilfsfunktion zur Berechnung von cramers_V()
chiquadrat_rechner <- function(x,y){ #Chi Quadrat Formel
  if(is.null(x) || is.null(y)){
    stop("Error: Einer der beiden Variablen ist leer")
  } # Abbruchkriterium
  chi <- 0
  dummy <- table(x,y) #Kreuztabelle wird erzeugt zwischen x und y
  rs <- rowSums(dummy)  #Summen der einzelnen Zeilen der Tabelle
  cs <- colSums(dummy)  #Summen der einzelnen Spalten der Tabelle
  
  for (i in 1:length(rs)) { #Allgemeine Rechnenformel für Chi Quadrat 
    for(j in 1:length(cs)){
      chi <- chi + ((rs[i] * cs[j])/length(x) - dummy[i,j])^2/((rs[i] * cs[j])/length(x))
    }
  }
  return(chi) #Output
}

