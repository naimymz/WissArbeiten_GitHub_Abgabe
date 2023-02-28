funktiona <- function(Alter) #(a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für metrische Variablen berechnet und ausgibt, hier am Beispiel "Alter", welche durch andere metrische Variablen ersetzt werden kann 
{summary (Alter) #gibt Minimum, Maximum, Median(2. Quartil), Mittelwert, erstes und drittes Quartil aus
  sd (Alter) # Standardabweichung
  var (Alter) #Varianz
  skew (Alter) #Schiefe, bei <0 -> linksschiefe Verteilung, bei >0 -> rechtsschiefe Verteilung
  IQR (Alter) #Interquartilsabstand, also 3. Quartil minus 1. Quartil
  }

funktionb <- function(Interesse_an_Mathematik) #(b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für kategoriale Variablen berechnet und ausgibt, hier am Beispiel "Interesse_an_Mathematik", welche durch eine andere kategorielle Variable ersetzt werden kann
{#Berechnung des Modus
  M <- table(sim_dat$Interesse_an_Mathematik) ## Häufigkeitstabelle von Variable Intersse_an_Mathematik erstellen aus dem Datensat sim_dat
maximum <- max(M) ## das Objekt maximum enthält nun die größte Häufigkeit
which(M == maximum) ## mit which können wir die Ausprägungen von M erhalten, die die größte Häufigkeit aufweisen
}