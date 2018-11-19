## 19 novembre 2018
# Poursuite stats descriptives et modélisation basique tarification pour données projet stat app
#### Champ : données les plus récentes (structurel), séjours dans le public
library(ggplot2)

# Import données : on se place sur les données 2018, pour les hôpitaux publics à ce stade.
setwd("C:/Users/maxim/Documents/ensae-statapp-2019/Data/Données tarification/2018")
public2018 <- read.csv("ghs_pub.csv", sep = ";", stringsAsFactors = FALSE, dec = ",")

# Création des variables delog du prix, de catégorie majeure de diagnostic et de gravité.
# Ces variables seront les principaux régresseurs dans l'analyse du prix du séjour.
public2018$logGHSPrix <- log(public2018$GHS.PRI)
public2018$Gravite <- as.character(substr(public2018$GHM.NRO, 6, 6))
public2018$CMD <- as.character(substr(public2018$GHM.NRO, 1, 2))


#####################################################################
############# A. STATISTIQUES DESCRPTIVES ###########################
#####################################################################
table(public2018$CMD)
table(public2018$Gravite)
table(public2018$DCS.MCO)
# Pour la gravité
# A, B, C, D, E : niveaux de gravité pour les nouveaux-nés (E : décès...) -> Voir si on les 
# recase avec 1/2/3/4 ?
# J : ambulatoire
# T : très courte durée
# Z : à voir, semble varié...

## Pour le moment on garde gravité 1 à 4 / J / T / Autre (renommé en X)
public2018$Gravite[!(public2018$Gravite %in% c("1", "2", "3", "4", "J", "T"))] = "X"

## Distribution des variables SEU.BAS et SEU.HAU ?
ggplot(public2018, aes(SEU.BAS)) + geom_histogram(alpha = 0.2) + ggtitle("Distribution de la variable seuil bas")
ggplot(public2018, aes(SEU.BAS, fill =  public2018$Gravite)) + geom_histogram(alpha = 0.2) + ggtitle("Distribution de la variable seuil bas selon la gravité")

ggplot(public2018, aes(SEU.HAU)) + geom_histogram(alpha = 0.2) + ggtitle("Distribution de la variable seuil haut")
ggplot(public2018, aes(SEU.HAU, fill =  public2018$Gravite)) + geom_histogram(alpha = 0.2) + ggtitle("Distribution de la variable seuil haut selon la gravité")
## Quand même beaucoup de manques pour SEU.HAU... Galère...

#####################################################################
############# B. MODELISATION        ################################
#####################################################################

# Avec le prix
modele_base1 <- lm(public2018$GHS.PRI~ public2018$CMD + public2018$DCS.MCO + public2018$Gravite)
summary(modele_base1)
# NB : Obstrétrique / DCS = "O" (médecine pour grossesse et accouchement) = CMD 14

# Analyse rapide résidus
modele_base1.res = resid(modele_base1) 
summary(modele_base1.res)
plot(public2018$GHS.PRI, modele_base1.res, 
     ylab="Résidus", xlab="Prix du séjour", 
     main="Tarification des séjours hospitaliers - Résidus") 
abline(0, 0)       
## Du coup pas mal de droites pour ceux qui ont les mêmes covariables pour un Y différent??


# Avec le log du prix
modele_base2 <- lm(public2018$logGHSPrix~ public2018$CMD + public2018$DCS.MCO + public2018$Gravite)
summary(modele_base2)
## Analyse résidus pour ce modèle.
modele_base2.res = resid(modele_base2) 
summary(modele_base2.res)
plot(public2018$logGHSPrix, modele_base2.res, 
  ylab="Résidus", xlab="Prix du séjour", 
  main="Tarification des séjours hospitaliers - Résidus") 
abline(0, 0)       
## Du coup pas mal de droites pour ceux qui ont les mêmes covariables pour un Y différent??

# Ajout de SEU.HAUT
modele_base3 <- lm(public2018$GHS.PRI~ public2018$CMD + 
                     public2018$DCS.MCO + public2018$Gravite + public2018$SEU.HAU)
summary(modele_base3)
# La variable SEU.HAUT sor plutôt mais il faudrait catégoriser.
# (voire croiser avec niveau de gravité)... 
# Mais mieux : récupérer des infos ex post sur les séjours effectivement réalisés.

# TODO:
# (a) Récupérer les poids pour la régression et voir ce que ça change
# (b) Regarder l'erreur moyenne de prédiction réalisée avec les modèles, 
# et selon les covariables, et selon la variable d'intérêt
# (c) Récupérer les données ex post sur les GHM pour les prendre en considération dans les modèles
# (d) Poursuivre plus proprement l'analyse des résidus et faire des tests de robustesse.
# (e) Analyser plus en détails les covariables incluses dans les modèles
# Notamment créer des catégories pour regrouper des modalités (suite ADD ?)
# Et étudier les interactions entre variables

# On pourrait faire matrice des corrélations linéaires...

