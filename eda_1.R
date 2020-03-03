
#***************************************************************************
#*********************** Chargement des packages ***************************
#***************************************************************************


packages = c("gplots","prettyR","binom","Epi")

package_check <- lapply(packages, FUN = function(x){
  
  if (!require(x, character.only=T)) {
    
    install.packages(x, dependencies = T)  
    library(x, character.only=T)
  }
  
})




#***************************************************************************
#*********************** Chargement des fichiers de données ****************
#***************************************************************************


smp=read.csv("smp1.csv", header = T, sep = ";", stringsAsFactors = T)
dim(smp)
str(smp)
attach(smp)

smp_2=read.csv("smp2.csv", header = T, sep = ";", stringsAsFactors = T)
dim(smp_2)
str(smp_2)
attach(smp_2)


repdat=read.csv("repdat.csv", header = T, sep = ";", stringsAsFactors = T)
dim(repdat)
str(repdat)
attach(repdat)



# DISTRIBUTION DES VARIABLES ET REPRESENTATIONS GRAPHIQUES

#Représentation graphique de professions:

barplot(table(prof))
pie(table(prof))


#Représentation d'une variable quantitative discrète

hist(age, col="blue", main = "distribution de l'age", xlab = "âge", ylab = "fréquences")
boxplot(age, col = "lightgreen", xlab = "âge")

# Les BAM sont utiles pour représenter la distribution d'une variable quantitative 
# en fonction de sous-groupes :
#exemple distribution de l'age en fonction de la variable rs(recherche de sensation)

boxplot(age~rs, xlab="recherche de sensations", ylab="age", 
        main="distribution de rs selon l'age", col=c("red", "yellow", "green"))


# Distribution conjointe de 2 variables aléatoires quantitatives

plot(n.enfant~age) 

# Plus un détenu est âgé, plus son nombre d'enfants est élevé

# Pour éviter la superposition des points on peut utiliser la plot jitter

plot(jitter(age), jitter(n.enfant))


# Représentation temporelle d'une variable aléatoire quantitative avec la fonction plotmeans
install.packages("gplots")
library(gplots)
plotmeans(HDRS~VISIT, barcol = "green")

# On note la progression de l'état symptomatique des patients au fil du temps

# Mesure de la variabilité du phénomène d'un sujet à l'autre
# On utilise la fonction interaction.plot:

interaction.plot(VISIT, # variable temporelle
                 NUMERO, # variable qui indique chaque sujet   
                 HDRS,  # variable représentée
                 lty =6, legend = FALSE)


# MESURES DE POSITION ET DE DISPERSION

# les mesures agrégées servent à synthétiser l'information (exemple moyenne et écart-type)
# pour une variable catégorielle il s'agit de lister les pourcentages de toutes les modalités
# pour une variable il faut prendre d'autres paramètres comme la moyenne et la médiane
# moyenne : baricentre = sens physique
# dans une distribution symétrique mean=median
# dans le cas contraire il ya des différences entre les 2
# Ecarts interquartiles
# Dans une distribution normale , l'intervalle :[m-e.t., m+e.t.] contient approximativement 2/3 des données
# 
# En pratique : on utilise la fonction summary

summary(smp)

# Pour une visualisation plus commode on utilise la fonction describe du package (prettyR)

install.packages("prettyR")
library(prettyR)
describe(smp) # elle ne présente pas les quartiles, le minimum et le maximum (ceux-ci permettent de détecter les valeurs aberrantes)

# On peut ajouter ces paramètres avec :

describe(smp, num.desc = c("mean", "sd", "median", "min", "max", "valid.n"))

# GESTION DES VARIABLES DANS UN DATAFRAME

# explications sur l'interface de Rstudio 
# On va utiliser le fichier smp2
str(smp_2)
dim(smp_2)
names(smp_2)
summary(smp_2)
summary(age)
describe(smp_2)
# Explication sur le rôle de les fonctions attach et detach

smp_2$age[2]  # On afficche le deuxième individu de la variable age
smp_2$age[3:10] # On affiche de la 1ere à la 10eme observation
head(smp_2$age, n=10) # équivalent à la précédente
min(smp_2$age)  # on a NA en retour car il ya des valeurs manquantes
min(smp_2$age, na.rm = TRUE) # Option pour éviter le décompte des valeurs manquantes
unique(smp_2$abus) # Pour afficher les niveaux ou modalités de la variable abus
length(smp_2$abus) # nombre d'observations d'une variable
nrow(smp_2)   # nombre de lignes
ncol(smp_2)   # nombre de colonnes
table(smp_2$rs, useNA = "always") # ajouter l'option usena pour s'assurer de compter les valeurs manquantes
summary(smp_2$abus)

# On va traiter la variable abus comme une variable catégorielle (factor)

class(smp_2$abus)
smp_2$abus=as.factor(as.integer(smp_2$abus))
smp_2$abus=factor(smp_2$abus, levels = c("0","1"), labels = c("Non","Oui"))
levels(smp_2$abus)
table(smp_2$abus, useNA = "always")

# Variable nombre d'enfants

names(smp_2)
class(smp_2$n.enfant)
summary(smp_2$n.enfant)
table(smp_2$n.enfant)
table(smp_2$n.enfant > 4)

# On crée une variable nombre d'enfants supérieur à 4

smp_2$n.enfant.cat=factor(smp_2$n.enfant)
levels(smp_2$n.enfant.cat)
nlevels(smp_2$n.enfant.cat)  # vérification du nombre de niveaux

# Création d'une catégorie 5 enfants et +
levels(smp_2$n.enfant.cat)[6:13]<- "5+"
table(smp_2$n.enfant.cat)

# on le fait avec la fonction ifelse en créant une nouvelle variable
smp_2$nenf_bis=ifelse(smp_2$n.enfant>=5, "oui", "non")
head(smp_2$nenf_bis)
table(smp_2$nenf_bis)    # CHECK WHY I HAVE THE NA FIX

# Sauvegarder le fichier en format R
save(smp_2, file = "smp_v1.rda")  # il sera chargé dans notre répertoire de travail

# On sauve l'historique des commandes
savehistory(file = "commandes_1.R")


# INDEXATIONS AVEC CRITERES D'OBSERVATION

# Rechargement du dernier fichier de travail

load("smp_v1.rda")
names(smp_2)

# indexation de la 1ere observation et de la 1ere variable

smp_2[1,1]
smp_2[1,"prof"] # obs 1 , variable prof
smp_2[1:2,c(3,4)] # obs 1 à 2, variable 3 à 4

# On cherche les professions agriculteurs

head(smp_2$prof=="agriculteur")
table(smp_2$prof=="agriculteur")  # solution 1

head(smp_2$prof[which(smp_2$prof %in% c("agriculteur"))]) # solution 2 avec which
which(smp_2$prof=="agriculteur")   # renvoi des numéros d'observation

# On indexe les valeurs de la variable âge pour lesquels profession==agriculteur

smp_2$age[which(smp_2$prof=="agriculteur")]

# usage de la commande subset

attach(smp_2)
subset(smp_2, prof=="agriculteur", age)  # dans l'ordre la base, le filtre et la variable
subset(smp_2, prof=="agriculteur", c(2,5)) # on étend la sélection à plusieurs variables
subset(smp_2, prof=="agriculteur", 1:6)

# Combinaison de 2 filtres (ouvriers et nombre d'enfants supérieur à 2)

subset(smp_2, prof=="ouvrier" & n.enfant>2, c(age, duree, discip, n.enfant))

# ajout de filtres en excluant les NA sur la variable durée

subset(smp_2, prof=="ouvrier" & n.enfant>2 & complete.cases(duree), c(age, duree, discip, n.enfant))


# On stocke les résultats de la variable nenfantcat dans une table et on effectue des opérations
tab<-table(n.enfant.cat)
sum(tab)
tab/sum(tab)

# On peut faire la même chose avec les fonctions round et prop.table
round(prop.table(tab), digits = 2) 


# Représentation graphique pour la variable catégorielle (barplot)
barplot(prop.table(tab)*100, col = "blue", main = "diagramme en bâton")

# On ajuste les labels verticaux
barplot(prop.table(tab)*100, ylim = c(0,30), las=1,  col = "blue", main = "diagramme en bâton")


# Représentation graphique pour la variable numérique (histogramme)
summary(age)
hist(age, col = "blue", main = "distribution de la variable age")
  # Changement du nombre de classes:
  hist(age, nclass=8, col = "blue", main = "distribution de la variable age")
  
  # ajout des densités non paramétriques
  hist(age, nclass=8, probability = TRUE, las=1)
  lines(density(smp_2$age, na.rm = TRUE))
  
  
# INTERVALLES DE CONFIANCE
  
# passage de l'échelle de l'échantillon à celle de la population: l'objectif est de généraliser 
  #la mesure d'un paramètre (moyenne, pourcentages, variances , etc.)
  # si on a un paramètre avec distribution normale, le paramètre estimé par "m" et "e.t",
  # L"intevalle de confiance  à 95% du paramètre est de :
                  #  [m-1.96*e.t.,m+1.96*e.t.]
  
  library(prettyR)
  describe(smp$age)
  summary(smp$age)
  borne_inf=38.9-1.96*13.28/sqrt(799)
  borne_sup=38.9+1.96*13.28/sqrt(799)
  borne_inf
  borne_sup
  
  # Un intervalle de confiance à 95% de l'âge est compris entre 38 et 40 ans
  
  # Estimation automatique à l'aide de R : on estime l'intervalle de confiance en utilisant toutes les méthodes
  
  install.packages("binom")
  library(binom)
  binom.confint(3,10, methods = "all") # 3 est le nombre d'individus tirés
                                       #  10 est la taille de l'échantillon
                                       #  "all" spécifie de prendre toutes les méthodes d'estimation
  
  # Il faut choisir la méthode "exact" avec un intervalle de confiance à 7 et 65%
  
  # quand la taille de l'échantillon s'accroit, toutes les méthodes convergent vers la même valeur
  
  # On change la taille de l'échantillon et le nombre de sujets tirés pour vérifier cela
  
  binom.confint(300,1000, methods = "all") 
  
  # les estimations convergent vers des valeurs identiques
  
  # COEFFICIENTS DE CORRELATION
  # La liaison entre 2 variable est dite forte quand la connaissance
  # de la valeur de l'une donne une indication sur la valeur de l'autre
  # degré de liaison : entre -1 et 1 (exemple : la forte majorité de la population 
  # carcérale est d'origine noire ou arabe:
    # 1 -corrélation entre ces deux phénomènes = oui
    # 2 - causalité  = non (cequi reviendrait à dire que les noir et les arabes sont des délinquants))
  
 # r=0 ; il y a absence de corrélation sssi les 2 variables suivent une loi normale  
  
  
  # Représention graphique de la liaison entre age et nombre d'enfants
  plot(jitter(smp$age), jitter(smp$n.enfant))
  
  # calcul de la corrélation
  cor(smp$age,smp$n.enfant, use = "complete.obs")
  
  
  # LIAISON ENTRE 2 VARIABLES BINAIRES (ODDS RATIO ET RISQUE RELATIF)
  
  # exemple :maladie vs facteur de risque
  # association entre évitement du danger(ed) et trouble dépressif (dep.cons)
  names(smp)
  smp$ed_2=ifelse(smp$ed>2, 1, 0)
  str(smp)  
  table(smp$ed_2, smp$ed, deparse.level = 2, useNA = "always")  
  
  #calcul odds ratio
  install.packages("Epi")
  library(Epi)
  twoby2(1-smp$ed_2, 1-smp$dep.cons)

  tab=table(smp$dep.cons)  
  round(prop.table(tab), digits = 3) #  à 40% , nous ne sommes pas en présence d'une
  # pathologie rare, donc inutile de faire usage de l'odds ratio . On se focalise sur le 
  # risque relatif qui est égal à 2 : donc 2 fois plus de chance d'avoir un état dépressif
  # quand on a un évitement du danger élevé par rapport au contraire

 smp[c(20, 221, 342, 446, 531),]
 tab=table(smp$prof)
 round(prop.table(tab)*100, digits = 2)
 t1=subset(smp,smp$age>=20 & smp$age<=30)


  # TESTS STATISTIQUES
 # valeur de p pour savoir si les phénomènes ne sont pas dus au hasard
 # p dépend de la taille des échantillons
 # Neyman et Pearson = on détermine le seuil de risque a priori (prise de décision importante :exemple des tests médicamenteux)
 # Fischer = on calcule la valeur de p a posteriori (plus utilisé dans les autres cas)
 
 # TEST DU CHI2
 smp$ed_bis<-ifelse(smp$ed>2, 1, 0)
 names(smp)
 # On croise l'évitement du danger et le diagnostic de dépression et l'évitement du danger
 tab=table(smp$ed_bis, smp$dep.cons, deparse.level = 2, useNA = "always")
 tab_2=table(smp$ed_bis, smp$dep.cons, deparse.level = 2)
 
  prop.table(tab_2, 1) # On édite le % de dépression selon le fait d'avoir ou non un haut niveau d'ed.
  prop.table(tab_2, 2) # On édite le % de l'inverse
  
  #usage du Test de chi2 avec la fonction chisq.test
  chisq.test(smp$ed_bis, smp$dep.cons, correct = FALSE) # correct=false Pour éviter le test de correction Robuste)
  
  #Avec un p=10 puisance -12, très largement inférieur à 5% : donc avec certitude, le hasard ne peut pas 
  # à lui tout seul une telle différence de prévalence de dépression
  
  #Test alternatif de Fisher dans le cas où les conditions de validité du chi2 ne sont pas respectés(petits échantillons)
  fisher.test(smp$ed_bis, smp$dep.cons)
  
  
  #COMPARAISON DE DEUX MOYENNES
  # test t de student
  # conditions de validité : 
    # 1: au moins 30 individus dans chaque groupe d'observations
    # 2 :la variable étudiée doit suivre une loi normale
    # 3 : égalité des variances entre les groupes considérés
  # exemple : compraraison de l'âge(age) en fonction de l'évitement du danger(ed)
  
  # distribution de la variable age : ce n'est pas tout à fait une loi normale
  hist(smp$age, col = "blue")
  
  # On essaye un diagramme de normalité
  qqnorm(smp$age);qqline(smp$age)
  
  # Il ya un écart à la normalité sur la partie inférieure gauche du graphe.
  # Test dégalité des variances (ou des ET) dans les groupes à comparer
  by(smp$age, smp$ed_bis, sd, na.rm=T)
  # La valeur des ET dans les groupes de la variable ed_bis est quasiment la même
  # quand un ET est >= à 1,5 fois l'autre ET ça compromet la pertinence de l'usage du T de student
  
  # calcul du test t de student
  
  t.test(smp$age~smp$ed_bis, var.equal=TRUE)
  # on a p>0.05 : donc on ne peut pas dire qu'il ya une différence significative d'age entre les 
  # différents groupes de la variable ed_bis
  
  
  # Si les conditions d'usage de t student ne sont pas réunies, on peut utiliser les tests
  # de Wilcoxon ou de Man withney
  
  wilcox.test(smp$age, smp$ed_bis)

  #TEST DE NULLITE DE CORRELATION de Pearson ENTRE 2 VARIABLES:
    # condition: il faut que l'une au moins des 2 variables suive une loi normale
  # Exemple : corrélation entre age et recherche de sensations
  
  cor.test(smp$age,smp$rs, method = "pearson")
  # p est très faible : donc corrélation très significativement non nulle
  # cor = -0.22 donc corrélation négative entre age et recherche de sensations
  # int conf du coefficient de corrélation : 95% de chances que cor soit contenu entre :
    # -0.2922516  et -0.1509579 dans la population totale
  
  # Test de nullité du coef de corrélation de Spearman : test basé sur les rangs des individus des deux variables
  # Ce test est donc plus robuste car on fait la corrélation des rangs des individus des 2 vars 
  
  cor.test(smp$age,smp$rs, method = "spearman")
 # le p est très petit et la corrélation est significativement négative
  
  #RESUME :
    # WILCOXON : usage dans des cas de non normalité de la variable
    # SPEARMAN : usage quand aucune des 2 vars ne suit une loi normale : a posteriori on 
      # ne peut plus utiliser les techniques statistiques qui nécessitent la normalité des variables
      # il faut toujours a priori vérifier la normalité de la variable avant de se lancer dans un choix de méthode
  
  # COMPARAISON D'UNE MOYENNE A UNE REFERENCE
  # quand on connait la moyenne de référence d'une variable dans une population de référence
  t.test(smp$age, mu=24)
  
  # TESTS APPARIES : MESURES AVANT ET APRES D'UN PHENOMENE
  # on suit un groupe d'individus et on mesure le phénomène avant et après
  
  
  mcnemar.test("variable avant", "variable après")
  t.test("variable avant", "variable après", paired = TRUE)
  
  # TESTS D'ASSOCIATION : GRAPHIQUES BIVARIES
  
  load("smp_v1.rda")
  table(smp_2$subst.cons)
  tab=table(smp_2$subst.cons, smp_2$abus)  
prop.table(tab, margin = 1) # Les effectifs sont rapportés aux totaux lignes    
prop.table(tab, margin = 2) # Les effectifs sont rapportés aux totaux colonnes
  
# Même chose que la fonction prop.table
xtabs(~ subst.cons+abus, smp_2)

# barplot pour afficher les modalités
barplot(xtabs(~ subst.cons+abus, smp_2), beside = TRUE)

# Test du chi2 entre les 2 vars qualitatives
res=chisq.test(tab)

# Affichage des effectifs observés dans l'objet res
res$observed

# Affichage des effectifs attendus dans l'objet res
res$expected

# Test de Fisher
fisher.test(tab)

# Description du comportement de la variable age en fonction des 2 modalités de subst.cons

tapply(smp_2$age, smp_2$subst.cons, mean, na.rm=TRUE) # comportement de l'age moyen en fonction de des deux modalités



# Test de student pour comparer les moyennes d'age selon les modalités 0 et 1 de subst.cons
t.test(smp_2$age[smp_2$subst.cons==0],smp_2$age[smp_2$subst.cons==1] , var.equal = TRUE)

# Même résultat avec le t.test (on n'est pas obligé de préfixer la var avec le nom de la table)
t.test(age~subst.cons, smp_2)
aggregate(age~subst.cons, smp_2, mean) # age moyen distribué selon les 2 modalités de subst.cons (même chose que tapply, sauf qu'on n'a pas besoin de faire na.rm=true)

# Représentation graphique et distributions conditionnelles avec boxplot
boxplot(smp_2$age, smp_2$subst.cons, col = c("blue", "green"))

library(gplots)

with(smp_2, tapply(smp_2$age, smp_2$subst.cons, mean, na.rm=TRUE))


#******************************************
#REGRESSION LINEAIRE SIMPLE
#******************************************

# définition droite de régression : droite qui minimise la somme des carrés des distances entre y et ÿ
plot(smp_2$age, smp_2$dur.interv)

library(psych)
abline(lm(smp_2$))