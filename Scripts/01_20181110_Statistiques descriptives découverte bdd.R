### Découverte base de données tarification données de santé ###
## QUelques statistiques descriptives
## 10 novembre 2018
library(ggplot2)

# Import données : on se place sur les données 2018, pour les hôpitaux publics à ce stade.
setwd("C:/Users/maxim/Documents/ensae-statapp-2019/Data/Données tarification/2018")
public2018 <- read.csv("ghs_pub.csv", sep = ";", stringsAsFactors = FALSE, dec = ",")

## Descriptif des variables
summary(public2018)

## Plot des variables numériques
public2018$logGHSPrix <- log(public2018$GHS.PRI)

p1 <- hist(public2018$SEU.BAS)                    
p2 <- hist(public2018$SEU.HAU)                    
plot(p1, col="red",xlim=c(0,200)) 
plot(p2, col="blue", add=T)  
legend("topright", 95, legend=c("Gravité 1", "Gravité 2", "Gravité 3", "Gravité 4"),
       col=c("blue", "green", "red", "orange"), lty=1)

# Sur seuils sur les jours d'hospitalisation
p1 <- hist(public2018$SEU.BAS, breaks = 50)    
p2 <- hist(public2018$SEU.HAU, breaks = 250)                
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,50),main = "Seuils sur nb de jours d'hospitalisation")
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,50), add=T)  


## Tarification séjour
# Au global
plot(density(public2018$GHS.PRI), col = "blue", main = "Remboursement du séjour (euros) selon le GHM")
plot(density(public2018$logGHSPrix), col = "blue", main = "Remboursement du séjour (log-euros) selon le GHM")


# Selon la gravité
?hist
gravite1 <- subset(public2018, substr(public2018$GHM.NRO, 6, 6)== "1")
gravite2 <- subset(public2018, substr(public2018$GHM.NRO, 6, 6)== "2")
gravite3 <- subset(public2018, substr(public2018$GHM.NRO, 6, 6)== "3")
gravite4 <- subset(public2018, substr(public2018$GHM.NRO, 6, 6)== "4")

# Prix
plot(density(gravite1$GHS.PRI), col = "blue", main = "Remboursement du séjour selon la gravité")
lines(density(gravite2$GHS.PRI), col = "green")
lines(density(gravite3$GHS.PRI), col = "red")
lines(density(gravite4$GHS.PRI), col = "orange")
legend("topright", 95, legend=c("Gravité 1", "Gravité 2", "Gravité 3", "Gravité 4"),
       col=c("blue", "green", "red", "orange"), lty=1)

# Log Prix : avec package ggplot
public2018$Gravite <- as.character(substr(public2018$GHM.NRO, 6, 6))
public2018$Gravite[!(public2018$Gravite %in% c("1", "2", "3", "4"))] = "Autre"
ggplot(public2018, aes(logGHSPrix, fill =  public2018$Gravite)) + geom_density(alpha = 0.2) + ggtitle("Logarithme du prix du séjour selon sa gravité")
