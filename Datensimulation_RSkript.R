# R-Skript 1 für die Simulation des Datensatzes.

set.seed(2023) # mit set.seed() können die selben zufällig gezogen Variablen
               # erneut erzeugt werden. So könne die Werte für außenstehende 
               # Personen reproduziert werden. 

sim_Datensatz <- data.frame(
  # Erzeugen des Vektors "Alter"
  Alter = round(rnorm(n = 100, mean = 25, sd = 2)), 
  # Variable Alter erzeugt über die Normalverteilung mit Funktion rnorm().
  # n = 100 für 100 Einträge, mean = 25 für den Erwartungswert
  # 25, sd = 2 für eine Standardabweichung von 2. Der Vektor wird dabei mit 
  # der Funktion round() gerundet, damit im Datensatz ganze Zahlen sind.
  
  # Erzeugen des Vektor "Studienfach"
  Studienfach = sample(c("Statistik", "Data Science", "Informatik", "Mathe"),
                       size = 100, prob = c(0.3,0.3,0.25,0.15), replace = TRUE),
  # Die Funktion sample zieht aus einem vorgegeben Vektor zufällig heraus.
  # Mit size = 100 wird festgeleget wie oft gezogen wird, hier also 100 mal.
  # durch prob legen wir fest, mit welcher Wahrscheinlichkeit die Variablen aus 
  # aus dem Vektor gezogen werden, so bezieht sich der Wert 0.3 auf Statistik und
  # Data Science. Über die letzten Funktion replace = TRUE, zieht die Funktion 
  # MIT zurücklegen.

  # Erzeugen des Vektors "Mathe_LK"
  Mathe_LK = sample(c(0,1), size = 100, replace = TRUE)
  # Hier wird festgelegt ob die zufällig erzeugten Personen in der Oberstufe 
  # Mathe LK belegt hatten oder nicht. Dabei haben wir uns gegen einen Zusammenhang 
  # mit den jeweiligen Studienfächern entschieden. So wird mit sample zufällig 
  # entschieden, ob die Personen Mathe LK hatten.
)


IaM <- NULL # Zwischenspeicher für die nachfolgende Funktion
  for(i in 1:100) { # Mit dieser for-Schleife wird der Vektor "Interesse an Mathematik"
    # in Abhängigkeit des Studiengangs erzeugt 
    if(sim_Datensatz$Studienfach[i] == "Mathe" || sim_Datensatz$Studienfach[i] == "Statistik"){
     IaM <- c(IaM, sample(1:7, size = 1, prob = c(0.05,0.05,0.1,0.1,0.2,0.25,0.25)))
      } #Mit der if-Schleife wird bei den Studiengängen Mathe und Statistik ein 
    # positiver Zusammenhang mit Interesse an Mathematik erzeugt.
   else{
      IaM <- c(IaM, sample(1:7, size = 1))
      } # Falls die Bedingung nicht zutrifft, ist der Zusammenhang bei Informatik
    # und Data Scienece mit Interesse an Mathematik zufällig.
}


IaI <- NULL # Analog zu oben wird der Vektor "Interesse an Informatik" erzeugt
  for(i in 1:100) {
    if(sim_Datensatz$Studienfach[i] == "Data Science" || sim_Datensatz$Studienfach[i] == "Informatik"){
      IaI <- c(IaI, sample(1:7, size = 1, prob = c(0.05,0.05,0.05,0.1,0.2,0.25,0.35)))
    }
    else{
      IaI <- c(IaI, sample(1:7, size = 1))
    }
  }

sim_Datensatz$Interesse_an_Mathematik <- IaM # Beide Vektoren zu den Interessen werden
sim_Datensatz$Interesse_an_Informatik <- IaI # in den Dataframe eingefügt

write.csv(sim_Datensatz, file = "Simulierter Datensatz.csv", row.names = TRUE)
# Dataframe wird als csv Datei exportiert
