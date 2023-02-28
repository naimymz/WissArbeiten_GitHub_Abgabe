#Aufgabe 4 deskriptive und visuelle Analyse

analyse_alter <- funktion_a(data$Alter)
View(analyse_alter)
#das Alter hat ein minimum von 21 Jahren, ein Maximum von 30 Jahren, eine Varianz von ca. 4 und ein Median sowie 
#ein Arithmetisches Mittel von ca. 25 Jahren


analyse_LK <- funktion_b(data$Mathe_LK)
View(analyse_LK)
#es wurde 48 mal kein Mathe LK besucht und 52 wurde es besucht.

analyse_fach <- funktion_b(data$Studienfach)
View(analyse_fach)
#es wurde 30 mal Data Science, 31 mal Informatik, 16 mal Mathe und 23 mal Statistik gewählt

analyse_Info <- funktion_b(data$Interesse_an_Informatik)
View(analyse_Info)
#am Häufigsten haben die Peronen eine 7 angegeben (23 mal) bei dem Interesse an Informatik 
#und am seltesten wurde die 1 mit 7 mal angegeben

analyse_mathe <- funktion_b(data$Interesse_an_Mathematik)
View(analyse_mathe)
#28 Personen und damit die meisten haben eine 7 angegeben bei der Frage nach dem Interesse an Mathematik
#jeweils 7 Personen haben 1,3,4 angegeben und damit am wenigsten Personen


funktion_c(data$Studienfach,data$Mathe_LK)
#fuer Studienfach und Mathe LK erhält man einen Schwachen Zusammenhang(0.125)
funktion_c(data$Studienfach,data$Interesse_an_Mathematik)
#für Studienfach und Interesse an Mathematik erhält man ein etwas staerkeren Zusammenhang von 0.279
#dieser ist allerdings immernoch recht schwach
funktion_c(data$Studienfach,data$Interesse_an_Informatik)
#für den Zusammenhang von Studienfach und Interesse an Informatik erhält man mit
#0.41 den staerksten Zusammenhang. Dieser

funktion_d(data$Mathe_LK,data$Alter)
#das Ergebnis ist ca. 0.06 damit besteht kein starker zusammenhang zwischen dem Alter
#und dem belegen des Mathe LK


install.packages("plyr")
library("plyr")

count(funktion_e(data$Alter))
#40 personen haben ein niedriges Alter, 36 Personen haben ein mittleres Alter und 24 ein hohes Alter
count(funktion_e(data$Interesse_an_Mathematik))
#35 Personen haben ein niedriges Interesse an Mathe, 37 ein mittleres Interesse und 28 ein hohes
count(funktion_e(data$Interesse_an_Informatik))
#37 Personen haben ein niedriges Interesse an Informatik, 40 ein mittleres Interesse und 23 ein hohes Interesse

funktion_f(data,data$Studienfach,data$Mathe_LK,data$Interesse_an_Mathematik)
#die Funktion f zeigt keinen offensichtlichen Zusammenhang zwischen dem Studiengang, dem Interesse an Mathematik 
#und dem belegen des Mathe LK
funktion_f(data,data$Studienfach,data$Mathe_LK,data$Interesse_an_Informatik)
#auch hier scheint kein Zusammenhang zwischen Studienfach, Mathe LK und Interesse an Informatik zu bestehen

#Abschließend kann man sagen, dass sich zwischen keinen der variablen ein starker Zusammenhang feststellen lässt.
#Nur funktion_c zeigt einen mäßigen Zusammenhang zwischen dem Studienfach und dem Interesse an Informatik.