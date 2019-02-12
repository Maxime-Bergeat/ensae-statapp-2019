rm(list = ls())
load(file="C:/Users/maxim/Documents/ensae-statapp-2019/Data/objets.Rdata")

## Données 2017
sum(ghs2017$effectif.ghs, na.rm = T)
sum(ghs2017$effectif.ghm, na.rm = T)

## Données 2016
sum(ghs2016$effectif.ghs, na.rm = T)
sum(ghs2016$effectif.ghm, na.rm = T)

# Attention ici la variable gravité est un regroupement (a priori malin).
ghs$gravite_complet <- substr(ghs$ghm.nro, 6,6)
table(ghs$gravite,ghs$gravite_complet)

### Modélisation
