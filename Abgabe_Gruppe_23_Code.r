# Aufgabe 3.1  (Wichtelunglück) 

# Zuweisung der Variablen, zu den in der Aufgabenstellung daüfr gegebenen Werten.
N<- 10                   # Anzahl der Personen
mein_Geschenk <- 1       # Position von meinem eigenen Geschenk
Iterations <- 1000       # Anzahl der Simulationen
alle_Geschenke <- 1:10   # Anzahl aller Geschenke 
counter <- 0             # Zählvaribale für Personen die ihr eigenes Geschenk erhalten. Beginnt bei 0.

# Eine "For-Schleife", um die W'keit zu simulieren, dass unter 10 Personen min. eine das eigene Geschenk beim Wichteln erhält. 
for (i in 1:Iterations) { 
  Ünglücksfall <- sample(x = alle_Geschenke, size = N, replace = FALSE)
  if(mein_Geschenk %in% Ünglücksfall[1]) {
   counter <- counter + 1
  }
}


counter     # Gibt die Anzahl der personen aus, die ihr eigenes Geschenk erhalten (unter 1000 Simulationen).

counter / Iterations     # Gibt die Wahrscheinlichkeit aus, dass eine Person ihr eigenes Geschenk erhält (unter 1000 Simulationen). 


# Aufgabe 3.2     # Funktion zur bestimmung der Wahrscheinlichkeit, dass mindestens k unter n Personen ihr eigenes Geschenk zurückerhalten


wichtel_unglueck <- function(                                 # Funktion wird durch Variablenzuordnung erstellt
  n , k, iterationen = 1000, counter = 0                      # Variablenbedeutung: n=Anzahl der Personen, k= Anzahl der Personen, die ihr eigenes Geschenk erhalten.
){                                                                  # Iterationen= Anzahll der Wichteldurchläufe (Standardmäßig 1000), Counter= Wird =0 gesetzt, sodass die Funktion nach jeder Ausführung wieder beu 0 startet.
  if (n >= 0 & k >= 0 & iterationen > 0 & n >= k){            # "if"-Bedingung für gültige Werte der Variablen: n, k und Iterationen.
  for (i in 1:iterationen) {                                  # Schleife für die Berechnung der W'keit.
    Unglücksfall <- sample(x = 1:n, size = n, replace = FALSE)# Unglücksfall= Zuteilung eines zufällig gezogenen Geschenks.
    if(mein_Geschenk == k %in% Unglücksfall[k]) {             # Überprüft, ob zufällig gezogenes Geschenk aus "Unglücksfall" von der selben Person stammt.
      counter <- counter + 1                                  # Personen die ihr eigenes Geschenk erhalten werden gezählt.
      if (k > counter) {                                      # Es wird so lange gezählt, bis die gewünschte Anzahl k erfüllt ist.
        next
        }
       }
  }
  }
    else {
      stop("Error")                                           # Stop-Befehl, um die Funktion zu beenden, wenn die Anzahl k erreicht wurde.
    }
 return(counter/iterationen)                                  # Ausgabe der W'keit.
}

    
wichtel_unglueck(10,1,iterationen = 3000)                     #Probefall, um zu testen, dass die erstellte Funktion funktionsfähig ist.



# Aufgabe 3.4 (4 Testfälle)

install.packages("testthat")                                  
library("testthat")

# Testfall 1: wichtel_unglueck mit negativer Iterationsanzahl sollte immer einen Fehler zurückgeben.
test_that("wichtel_unglueck throws an error for negative iterationen", {
  expect_error(wichtel_unglueck(3, 3, iterationen = -100), "Error")
})

# Testfall 2: wichtel_unglueck mit k > n sollte immer einen Fehler zurückgeben.
test_that("wichtel_unglueck throws an error for k > n", {
  expect_error(wichtel_unglueck(3, 5), "Error")
})

# Testfall 3: wichtel_unglueck mit n = 0 und k = 0 sollte immer 0 zurückgeben.
test_that("wichtel_unglueck returns 0 for n = 0", {
  result <- wichtel_unglueck(0, 0)
  expect_equal(result, 0)
})

# Testfall 4: wichtel_unglueck mit n=1 und k=1 sollte immer 1 zurückgeben.
test_that("wichtel_unglueck returns 1 for n = 1 and k = 1", {
  result <- wichtel_unglueck(1, 1, iterationen = 1000)
  expect_equal(result, 1)
})



# Aufgabe 3.5 (Bikeshare)

data <- read.csv(                                               # Einlesen der "Bikeshare" Daten aus der Abgabe 1.
  file = "~/Downloads/Capital_bikeshare_data_2022_with_NAs.csv",
  header = TRUE ,
  sep = "," ,
  dec = "."
)
class(data)                                                     # Gibt die Klasse der eingelesenen Daten aus.
Daten_23 <- subset(data, station == "14th & Irving St NW")      # Eingelesenen Daten für unsere Straße speichern wir in die Variable "Daten_23" ab.

anyNA(Daten_23)                                                 # Prüft Datensatz auf vorhandene NA´s.
apply(Daten_23,2,anyNA)                                         # Zeigt an in welchen Spalten sich NA´s befinden.

ids <- which(is.na(Daten_23$count))                             # Speichert Inahlte mit NA´s in "ids" ab.
Daten_23[ids, ]                                                 # Ausgabe der "ids" in Daten_23.
median_count <- median(Daten_23$count, na.rm = TRUE)            # Berechnet den Median der count-Spalte ohne NA´s.
Daten_23[ids, "count"] <- median_count                          # Abspeichern des Medians an die Stellen in der count-Spalte, die vorher NA ausgegeben haben.

Temperaturen <- which(is.na(Daten_23$mean_temperature))         # Selbe vorgehensweise wie in Zeile 93-96, hier für die Spalte "mean_temperature".
Daten_23[ids, ]
median_temperature<- median(Daten_23$mean_temperature, na.rm = TRUE)
Daten_23[Temperaturen, "mean_temperature"]<- median_temperature
apply(Daten_23,2,anyNA)

# Suche nach Annomalien im Datensatz vom kleinsten bis zum größten Wert in jeder Spalte.
range(Daten_23$date)
range(Daten_23$station)
range(Daten_23$count)
range(Daten_23$wind_speed)
range(Daten_23$precipitation)
range(Daten_23$snowfall)
range(Daten_23$snow_depth)
range(Daten_23$mean_temperature)
range(Daten_23$max_temperature)

id<- which(Daten_23$precipitation == 4.05)         # Löschen eines Ausreißers in den Daten, um diese plausibler zu machen.
Daten_23 <- Daten_23[-id, ]
range(Daten_23$precipitation)

idn<- which(Daten_23$wind_speed == -1)             # Löschen von negativen Windgeschwindigkeiten, die zuvor als "-1" in den Daten vorhanden waren
Daten_23 <- Daten_23[-idn, ]
range(Daten_23$wind_speed)


# Aufgabe 4.1 (Grafische Zusammenhänge zwischen der Anzahl ausgeliehner Fahrräder und der Temp.,Niederschlagsmenge, Windgeschwindigkeit und Zeit)

install.packages("ggplot2")                                 
library(ggplot2)
install.packages("dplyr")
library(dplyr)

# In Grafik 1-4 wird der Gesamte Beobachtungszeitraum 01.01.2022 - 30.11.2022 betrachtet.
# Wir verwenden (ggplot 2) und (dplyr) zur Darstellung der Zusammengänge.

# Grafik 1 (Zusammenhang zwischen den ausgeliehenen Fahrrädern und der Temperatur)
ggplot(data = Daten_23) +                                                         
  geom_point(aes(x = count , y = mean_temperature))+
  xlab("Anzahl ausgeliehner Fährrader (pro Tag)") +
  ylab("Durschnittstemeratur (in Fahrenheit)")+
  ggtitle("Anzahl ausgeliehner Fährrader im Verhältnis zur Durschnittstemperatur")


# Grafik 2 (Zusammenhang zwischen den ausgeliehenen Fahrrädern und der Niederschlagsmenge)
ggplot(data = Daten_23) +  
  geom_point(aes(x = count , y = precipitation))+
  xlab("Anzahl ausgeliehner Fährrader (pro Tag)") +
  ylab("Niederschlag (pro Tag in inches)")+
  ggtitle("Anzahl ausgeliehner Fährrader im Verhältnis zum Niederschlag")


# Grafik 3 (Zusammenhang zwischen den ausgeliehenen Fahrrädern der Windgeschwindigkeit)
ggplot(data = Daten_23) +
  geom_point(aes(x = count , y = wind_speed))+
  xlab("Anzahl ausgeliehner Fährrader (pro Tag)") +
  ylab("Windgeschwindigkeit (pro Tag in mph)")+
  ggtitle("Anzahl ausgeliehner Fährrader im Verhältnis zur Windgeschwindigkeit")


# Grafik 4 (Zusammenhang zwischen den ausgeliehenen Fahrrädern und der Zeit)
head(Daten_23)
Daten_23$date <- as.Date(Daten_23$date, format = "%Y-%m-%d")
ggplot(data = Daten_23) +
  geom_line(aes(x = date , y = count))+
  ylab("Anzahl ausgeliehner Fährrader (pro Tag)") +
  xlab("Zeit")+
  ggtitle("Anzahl ausgeliehner Fährrader im Verhältnis zur Zeit")
   

# 4.2  (Zusammenhang zwischen der Anzahl ausgeliehener Fahrräder und Regen- und nicht-Regentage)

# An Rgentagen:
ggplot(data = filter(Daten_23, precipitation > 0)) +                                                        # Zusammenhang zwischen der Anzahl ausgeliehener Fahrräder und den Wetterbedingungen
  geom_point(aes(x = count , y = mean_temperature))+                                                        # percipitation (Niederschlagsmenge) > 0 beinhaltet alle Tage an denen es Regnet
  xlab("Anzahl ausgeliehner Fährrader (pro Tag)") +
  ylab("Durschnittstemeratur (pro Tag)")+
  ggtitle("Anzahl ausgeliehner Fährrader im Verhältnis zur Durschnittstemperatur an Regentagen")

# An nicht-Regentagen:
ggplot(data = filter(Daten_23, precipitation == 0)) +                                                       # Für percipitation == 0 werden nur Tage betrachtet an denen es nicht regnet
  geom_point(aes(x = count , y = mean_temperature))+
  xlab("Anzahl ausgeliehner Fährrader (pro Tag)") +
  ylab("Durschnittstemeratur (pro Tag)")+
  ggtitle("Anzahl ausgeliehner Fährrader im Verhältnis zur Durschnittstemperatur an nicht Regentagen")


# 4.3 (Grafische Darstellung der Verteilungen)

#Grafik 1 (Anzahl ausgeliehener Fahrräder)
ggplot(data = Daten_23) +                                                   # Liniendiagramm zur Darstellung der Verteilungen mit "ggplot" und "geom_line"
  geom_line(aes(x = date , y = count))+
  ylab("Anzahl ausgeliehner Fährrader (pro Tag)") +
  xlab("Datum")+
  ggtitle("Verteilung der Fahrradausleihen im Messzeitraum")

#Grafik 2 (Temperatur)
ggplot(data = Daten_23) +
  geom_line(aes(x = date , y = mean_temperature))+
  ylab("Durschnitstemperatur am Tag (in Fahrenheit") +
  xlab("Datum")+
  ggtitle("Durschnittstemperatur über den Zeitraum")

#Grafik 3 (Niederschlagsmenge)
ggplot(data = Daten_23) +
  geom_line(aes(x = date , y = precipitation))+
  ylab("Niederschlag am Tag (in Inches)") +
  xlab("Datum")+
  ggtitle("Niederschlag verteilt über den Messzeitraum")

#Grafik 4 (Windgeschwindigkeit)
ggplot(data = Daten_23) +
  geom_line (aes(x = date , y = wind_speed ), col = "black") +
  ylab("Durschnittliche Windgeschwindigkeit (in mph)") +
  xlab("Datum")+
  ggtitle("Durschnittliche Windgeschwindigkeit pro Tag (in mph) im Messzeitraum")
  

#4.4 (Trennung der Verteilung der Anzahl ausgeliehener Fahrräder nach Jahreszeiten)

install.packages("gridExtra")
library(gridExtra)

#4 Jahreszeiten:
Winter <- Daten_23$count[1:79]

Frühling <- Daten_23$count[80:171]

Sommer <-  Daten_23$count[172:265]

Herbst <- Daten_23$count[266:334] 


ggplot(data =filter(Daten_23, date >= 2022-01-01 , date <= 2022-11-30))+                  # Erstellen einer Grafik, in der die Dichteverteilungen der Jahreszeiten im Bezugszeitraum 01.01.- 31.11.2022 überlappen.
  geom_density(aes(x = Winter), col= "black")+                                            # Zuweisung von verschiednen Farben zu den vier Jahreszeiten
  geom_density(aes(x = Sommer), col = "green")+
  geom_density(aes(x=Frühling), col = "orange")+
  geom_density(aes(x = Herbst), col = "red")+
  ggtitle("Verteilung ausgeliehner Fahrräder in den Verschiedenen Jahreszeiten")+
  xlab("Anzahl ausgeliehenr Fahrräder (pro Tag)")+
  ylab("Dichter der Verteilung ausgeliehener Fahrräder (pro Tag)")+
  facet_grid(cols = vars("Rot = Herbst","Schwarz = Winter","Grün = Sommer", "Orange = Frühling"))            # Erstellen einer Legende in der Grafik 

# 4.5  (3-D Scatterplot mit den Variablen Temperatur, Windgeschwindigkeit und Anzahl ausgeliehener Fahrräder)

install.packages("plotly")
library(plotly)


Meine_Grafik <- plot_ly(data = Daten_23, x = ~mean_temperature, y = ~wind_speed, z = ~count, type = "scatter3d",          # Eingabevariablen x-Achse = Temperatur, y-Achse = Windgeschwindigkeit, z-Achse = Anzahl ausgeliehner Fahrräder
                        mode = "markers", marker = list(size = 5, opacity = 0.5), color = ~count)
Meine_Grafik %>% layout(scene = list(xaxis = list(title = "Durschnittstemperatur (in F°)"),
                                     yaxis = list(title = "Windegeschwindigkeit (in mph)"),
                                     zaxis = list(title = "Anzahl ausgeliehener Fährrader")))
