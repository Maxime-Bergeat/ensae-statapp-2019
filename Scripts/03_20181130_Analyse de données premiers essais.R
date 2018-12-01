## 30  novembre 2018 : éléments d'analyse de données pour regarder différentes variables,
# Notamment sur ce qu'il se passe pour les CMD

library(ggplot2)
library(FactoMineR)
library(factoextra)
library(explor)

#####################################################################
############# A. PREPARATION DONNEES ACM  ###########################
#####################################################################

# Champ : 2017, public et privé 
# Pondération : nombre de séjours (normalisé pour éviter d'avoir 
# des problèmes numériques pour les CAH)
# Variables actives : tranches de prix, gravité, nombre de nuitées, 
# âge moyen, CMD, type d'hôpital

# Import données : on se place sur les données 2018, pour les hôpitaux publics à ce stade.
setwd("C:/Users/maxim/Documents/ensae-statapp-2019/Data/Données tarification")
df <- read.csv("data_compile_scraping_20181130.csv", sep = ",", stringsAsFactors = FALSE,
                       dec = ".", encoding = "UTF-8")

data_2017 <- subset(df, subset = (annee == 2017))
colnames(data_2017)
##colnames(data_2017)<- c("id", "annee","type","ghm.nro","ghs.nro","cmd.cod",
 ##                     "dcs.mco","ghm.lib","seu.bas","seu.haut","ghs.prix","exh.pri",
  ##                    "date.effet","exf.forfait","exb.journalier","effectif","duree.moy",
   ###                   "duree.sd", "duree.var", "duree.cv",
    ###                  "duree.min","duree.max","duree.mode", "duree.mediane", "duree.p10","duree.p95",
     ##                 "deces.nb","deces.pct","age.moy","age.sd","diag.moy","actes.moy","actesCl.moy",
      #                "actesNCl.moy")


# Suppression NAs (poids à 0 d'aprè légère vérif)
# nrow(data_2017[complete.cases(data_2017[ , "effectif"]),])
data_2017 <- data_2017[complete.cases(data_2017[ , "effectif"]),]

# Varibales avec NAs pour le prix ? SUprression aussi (voir ce que ça veut dire)
summary(data_2017$cmd.cod)
## Attention : 20 maladies pour lesquelles on a des infos scrapées mais pas de prix...
# On les écarte pour l'ACM à ce stade.
data_2017 <- data_2017[complete.cases(data_2017[ , "ghs.pri"]),]


# On utilise les poids normalisés afin de ne pas avoir de problème pour la CAH...
data_2017$effectif_norm <- data_2017$effectif/sum(data_2017$effectif)

# Création des variables catégorielles - Prix
summary(data_2017$ghs.pri)
summary(data_2017$effectif)
data_2017$prix_tranche = cut(data_2017$ghs.pri,c(0, 1000, 2500, 5000, 100000))
levels(data_2017$prix_tranche) = c("- 1000€","1000 à 2500€","2500 à 5000€","+ 5000€")               
table(data_2017$prix_tranche)

## Pour le moment on garde gravité 1 à 4 / J / T / Autre (renommé en X)
data_2017$Gravite <- as.character(substr(data_2017$ghm.nro, 6, 6))
data_2017$Gravite[!(data_2017$Gravite %in% c("1", "2", "3", "4", "J", "T"))] = "X"
data_2017$Gravite<-paste("Gravité :",data_2017$Gravite)
table(data_2017$Gravite)

# Pour les nuitées
summary(data_2017$nuitées.moy)
# Prix en 4 catégories : pour avoir environ un quart des obs à chaque fois
data_2017$nuitees_tranche = cut(data_2017$nuitées.moy,c(-1, 2, 6, 12, 1000))
levels(data_2017$nuitees_tranche) = c("- 2 nuits","2-6 nuits","6-12 nuits","+ 12 nuits")               
table(data_2017$nuitees_tranche)

# CMD avec indication de la variable 
data_2017$cmd.cod<-paste("CMD :",data_2017$cmd.cod)

# Âge 
summary(data_2017$age.moy)
# 4 tranches pour les quartiles comme d'hab
data_2017$age_tranche = cut(data_2017$age.moy,c(-1, 45, 60, 70, 120))
levels(data_2017$age_tranche) = c("-45 ans","45-60 ans","60-70 ans","+ 70 ans")               
table(data_2017$age_tranche)


# Type hôpital 
data_2017$type <- paste("Hôpital :", data_2017$type)


#####################################################################
#############           B.ACM             ###########################
#####################################################################


input_MCA1 <- subset(data_2017, select = c("cmd.cod", "Gravite",
                        "nuitees_tranche", "prix_tranche", "type",
                        "age_tranche"))
input_MCA1$Gravite<- as.factor(input_MCA1$Gravite)
input_MCA1$cmd.cod<- as.factor(input_MCA1$cmd.cod)
input_MCA1$type<- as.factor(input_MCA1$type)


mon_ACM1 <- MCA(input_MCA1, row.w = data_2017$effectif_norm, ncp = 10)
explor(mon_ACM1)

#plot.MCA(mon_ACM1, invisible = "ind")


#####################################################################
#############  C. Robustesse de l'analyse ###########################
#####################################################################

## TODO
# Voir comment jouent les poids
# Distinguer public/privé
# Tester différents jeux de variables actives/supplémantaires...

