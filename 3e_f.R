library(ggplot2)

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


