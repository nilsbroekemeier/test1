###3.1 Wichteln

N <- 10                   # Anzahl Personen/Geschenke
all_present <- 1:N          # Geschenke
iterations <- 10000  # Anzahl Simulationen
counter <- 0         # zaehlt positive Faelle

for (i in 1:iterations) #Schleife "iterations" mal durchfühlen, approximation
{
  new_present <- sample(unique(x = all_present, size = N, replace = TRUE))
  #Geschenke zufaellig verteilen
  if (any(new_present == all_present))#Ueberprüfen ob mind. einer sein eigenes Geschenk hat
  {  
    counter <- counter + 1
  }
}
counter/iterations #Wahrscheinlichkeit ermitteln


###3.2-3.4 Wichtelfunktion

wichtel_unglueck <- function(n=20, k=1, iterationen = 1000)
###Wichtelfunktion mit standartwerten fuer Anzahl der Geschenke n, Anzahl des selbstgezogenen
###Geschenks k und Anzahl der Wiederholungen unserer Simulation iterationen 
{
  if(iterationen<0){
  iterationen<- 1000
  print("Anzahl an Wiederholungen (iterationen) war negativ und wurde standartmaeßig 
         wieder auf 1000 gestellt")
  }###Fall, wenn wiederholungen negativ ist
  
  if(is.numeric(iterationen)==FALSE){
     stop("iterationen muss mindestens numerisch sein. Im besten Fall sogar ein 
           positiver Integer")
  }###Fall, wenn Wiederholungen nicht numerisch ist
  
  if(is.integer(iterationen:iterationen)==FALSE){
    iterationen<-round(iterationen, digits = 0)
   print("Wiederholung (iterationen) war nicht ganzzahlig, wurde aber gerundet")
  }###Fall,wenn Wiederholungen Kommazahlen sind, dann wird gerundet auf eine ganze Zahl
    
  if(iterationen<250){
   print("Wahrscheinlichkeit ungenau, da Schleife weniger als 250 mal durchgefuehrt wird")
  }###Bemerkung, falls Wiederholungen gering sind und dadurch Wahrscheinlichkeit ungenau ist
  
  if(n<k){
    stop("Fehler: Es gibt mehr vertauschungen als Geschenke (es muss: n>=k)")
  }###Fall, wenn Anzahl an Geschenken kleiner ist als Anzahl der selbstgezogenen Geschenke
  
  if(n<0 || k<0){
   stop("Anzahl an Geschenke n oder Anzahl der selbstgezogener Geschenke k darf nicht 
         negativ sein")
  }###Fall, wenn Anzahl der Geschenke oder Anzahl der selbstgezogenen Geschenke negativ ist 
  
  ###Ab hier beginnt eigentliche Simulation
  
  counter=0               ###Zaehler,der postiven Faelle, wird auf Null gesetzt
  all_present <- 1:n      ###Geschenke werden durch numeriert
  
  for (i in 1:iterationen) #Schleife "iterationen" mal durchfuehren, approximation
  {
    new_present <- sample(unique(x = all_present, size = n, replace = TRUE))
    #Geschenke zufaellig verteilen
    
    if (length(which(new_present == all_present, TRUE))>=k)#Ueberpruefen ob mind. k Personen 
                                                           # sein eigenes Geschenk gezogen hat
    {  
      counter <- counter + 1          #wenn Bedingung erfuellt ist wird Zähler hochgesetzt 
    }
  }
    counter/iterationen #Wahrscheinlichkeit ermitteln(P=guenstige Faelle/moegliche Faelle) 
                        #und ausgeben
}

wichtel_unglueck()
###Standartausfuehrung der Funktion mit defnierten standartwerten n=20,k=1,iterationen=1000
###Wir wissen, dass es ungefaehr 63% ist mit standartwerten

###Testen-----------------------------------------------------------------

install.packages("testthat")
library("testthat")
###Notwendig fuer die Installation der Testoptionen

###1.Test
test_that("Wahrscheinlichkeit zwischen 0 und 1",{
  expect_gte(wichtel_unglueck(20,1),0)
  expect_lte(wichtel_unglueck(20,1),1)
})
###Ueberprueft, ob die Wahrscheinlichkeit zwischen 0 und 1 ist.(Werte unter 0 und 
###ueber 1 machen keinen Sinn)

###2.Test
test_that("iterationen muss mindestens nummerisch sein, am besten ein positiver integer",{
  expect_error(wichtel_unglueck(20,1, iterationen = "eine große Zahl"))
  expect_no_error(wichtel_unglueck(20,1,-20))
  expect_no_error(wichtel_unglueck(20,1,100000.5))
})
### Testet ob iterationen nummerisch ist, wenn nicht gibts einen Fehler, 
### wenn es keinen Fehler gibt, ist iterationen mindestens nummerisch und wird
### falls noetig auf eine ganze Zahl gerundet oder wenns negativ ist, auf 1000 
###standartaeßig gesetzt

###3.Test
test_that("Wenn n=k=1 ist, liegt die Wahrscheinlichkeit bei 100%",{
  expect_identical(wichtel_unglueck(1,1),1)
})
###Testet ob Wahrscheinlichkeit bei 100% liegt, wenn es nur eine Person und 
###ein Geschenk gibt beim Wichteln

###4.Test
test_that("n >= k und n,k sind nicht negtiv",{
  expect_error(wichtel_unglueck(-20,-30))
  expect_error(wichtel_unglueck(100,-30))
  expect_error(wichtel_unglueck(0,1))
})
### Testet ob n immer groeßer oder gleich k ist und ueberprueft, dass n und k
### nicht negativ sein darf

#------------------------------------------------------------------------------------------

###Möglicher weiterer Test
###5.Test stoppt wenn n oder k nicht ganzzahlig oder positiv ist