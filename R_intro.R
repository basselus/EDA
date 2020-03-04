#*****************************************
#****** CHARGEMENT DES PACKAGES **********
#*****************************************


packages = c("funModeling","tidyverse","caret","dplyr")

package_check <- lapply(packages, FUN = function(x){
  
  if (!require(x, character.only=T)) {
    
    install.packages(x, dependencies = T)  
    library(x, character.only=T)
  }
  
})




#*****************************************
#****** TYPES DE VARIABLES ***************
#*****************************************

# assignation d'une variable numérique

age <- c(18, 27, 34, 18, 24, NA, 30, 28, 19, 19)

# assignation d'une variable catégorielle

sexe <- c("F", "F", "M", "F", "M", "M", "M", "F", "M", "F")


# Commandes sur les caractéristiques des variables 

length(age)
length(sexe)
mode(age)
mode(sexe)


# Transformation de variables 

var_factor=factor(c("3_high", "2_mid", "1_low"))
var_ordered=factor(var_factor, ordered = T) # on indique que les valeurs de la variable sont ordonnées
var_ordered

  # Numérique en factor

opinion <- c(1, 3, 2, 1, 4, 1, 5, 3, 2, 2)
opinion = as.factor(as.numeric(opin))

 # Caractère en facteur

sexe = factor(sexe)
levels(sexe)
nlevels(sexe)

# Facteur en numérique (usage du package caret)

library(caret) # contains dummyVars function
library(dplyr) # data munging library
library(funModeling) # df_status function

data=read.delim("https://raw.githubusercontent.com/pablo14/data-integrity/master/messy_data.txt", sep = ';')
di=data_integrity(data)

# On applique un filtre pour sélectionner les variables catégorielles 

status=df_status(heart_disease, print_results = F)
filter(status, type %in% c("factor", "character")) %>% select(variable)


# It converts all categorical variables (factor and character) into numerical variables
# It skips the original variable, so no need to remove it after the conversion, the data is ready to use.
dmy = dummyVars (" ~ .", data = heart_disease)
heart_disease_2 = data.frame(predict(dmy, newdata = heart_disease))
df_status(heart_disease_2)

# Avant
as.numeric(heart_disease[7, "chest_pain"])

# Après
heart_disease_2[7, c("chest_pain.1", "chest_pain.2", "chest_pain.3", "chest_pain.4")]


# Checking the new numerical data set:
colnames(heart_disease_2)



  # asssignation de labels 

opinion <- factor(opinion, labels=c("Pas du tout d'accord", "Moyennement d'accord", 
                              "Sans Opinion", "Assez d'accord", "Tout a fait d'accord"))
opinion

levels(opinion)
nlevels(opinion)

 # changement de l'ordre des niveaux
sexe = relevel(sexe, ref = "M")
levels(sexe)


# Dangers of discretization

df_pc=data.frame(visits=c(10, 59, 27, 33), postal_code=c("AA1", "BA5", "CG3", "HJ1"), transformation_1=c(1,2,3,4), transformation_2=c(1, 4, 2, 3 ))


# print table

knitr::kable(df_pc)

library(gridExtra)

# transformation 1
plot_1=ggplot(df_pc, aes(x=transformation_1, y=visits, label=postal_code)) +  geom_point(aes(color=postal_code), size=4)+ geom_smooth(method=loess, group=1, se=FALSE, color="lightblue", linetype="dashed") + theme_minimal()  + theme(legend.position="none") + geom_label(aes(fill = factor(postal_code)), colour = "white", fontface = "bold")


# transformation 2
plot_2=ggplot(df_pc, aes(x=transformation_2, y=visits, label=postal_code)) +  geom_point(aes(color=postal_code), size=4)+ geom_smooth(method=lm, group=1, se=FALSE, color="lightblue", linetype="dashed") + theme_minimal()  + theme(legend.position="none") + geom_label(aes(fill = factor(postal_code)), colour = "white", fontface = "bold")

# arranging plots side-by-side
grid.arrange(plot_1, plot_2, ncol=2)

# linear transformation is sometimes better than non linear transformation


# Numérique en facteur: discrétisation


#*****************************************
#****** FONCTIONS D'INDEXATION ***********
#*****************************************

 # Indexation avec critères : des observations spécifiques dans une variable

age[1]
age[c(1,3,8)]

# Véfication de la présence de données manquantes
is.na(age)
which(is.na(age))

# information sur le sexe de cette observation

sexe[which(is.na(age))]
sexe[which(age>18)]



#*****************************************
#****** OPERATIONS SUR VARIABLES *********
#*****************************************

# renvoi des valeurs stockées dans une variable numérique

sum(age)
sum(age, na.rm = T)
sum(age[-6])


# Remplacement des données manquantes

age[is.na(age)] <- mean(age, na.rm = T)
n <- length(age)      ## nombre d'observations
age - mean(age)       ## écarts à la moyenne

# Calcul de la variance 

sum((age - mean(age))^2)/(n-1)

# Calcul de l'écart-type

sqrt(sum((age - mean(age))^2)/(n-1))
sd(age)



#*****************************************
#****************MATRICES*****************
#*****************************************

# construction d'une matrice avec définition de ses dimensions

M=matrix(c(1:10), nrow = 5, ncol = 2,
         dimnames=list(c('a','b','c','d','e'), c('A','B')))


# Indexation de valeurs à l'intérieur d'une matrice 

M[c(1:3),]  
M[,1]
M[3,2]
M['e','A']

# Quelle est la diagonale de B ?

B=matrix(c(1:9),nrow=3,ncol=3, 
         dimnames = list(c('a','b','c'), c('A','B',C)))
diag(B)

# On définit la diagonale (5) et le nombre de dimensions en une seule ligne de code 

diag(5,3,3)

# Quelle est la valeur de rownames(I) et de colnames(I)
  
  # L'option byrow remplit la matrice colonne par colonne

I=matrix(c(1:9),3,3, byrow = T, 
         dimnames = list(c('a','b','c'),c('d','e','f')))
rownames(I)
colnames(I)

# fonction upper.tri()

upper.tri(I)
upper.tri(I, diag = T)
lower.tri(I)

# calcul matriciel

A=matrix(c(1:9),3,3, byrow = T, 
         dimnames = list(c('a','b','c'),c('d','e','f')))

B=matrix(c(1:9),3,3,
         dimnames = list(c('a','b','c'),c('d','e','f')))

# produit des Matrices A et B terme à terme
A*B

# Addition terme à terme
A+B

#autres opérations
A%*%B
(A+B)^2
A/B


# ANALYSES NUMERIQUES


#*****************************************
# LES VECTEURS
#*****************************************
#un vecteur est une structure qui peutêtre construit avec la fonction c() ou assign() et affecté 
#à un objet en utilisant l'opérateur <- ou =

x<-c(1,2,3,4,5)
x<3

p <- c (3, 5, 6, 8)
b <- c (3, 2, 7)
p+b # Impossibilité d'additionner des vecteurs de longueur différente (message d'erreur)

#avec la fonction assign

assign('v', c("samba", "youssou", "arame","yoro"))
View(v)

# Concaténation de vecteurs

Age <- c(22, 25, 18, 20)
Name <- c("James", "Mathew", "Olivia", "Stella")
Gender <- c("M", "M", "F", "F")
t=data.frame(cbind(Age,Name,Gender))
tg=subset(t, Gender=="M")
tg

#Opérations vectorielles

x=c(4,6,5,7,10,9,4,15) 
y=c(0,10,1,8,2,3,4,1) 
t=x*y
x

# La fonction cbind() permet de concaténer des colonnes de 2 vecteurs différents
a=c(1,2,4,5,6)
b=c(3,2,4,1,9)
c=cbind(a,b)
View(c)

# La fonction rbind() permet de concaténer des lignes de 2 vecteurs différents
a=c(1,2,4,5,6)
b=c(3,2,4,1,9)
c=as.matrix(cbind(a,b),
            dimnames(list(c("a","b"), c("g","f","s","o","m")))) # to be checked
View(c)



x=c(1:12)
dim(x)
length(x)

a=c(12:5)
is.numeric(a)


#Consider two vectors, x, y

x=c(12:4)
y=c(0,1,2,0,1,2,0,1,2)
#What is the value of: which(!is.finite(x/y)
test=x/y
test
which(!is.infinite(x/y))
x
y

x=c('blue','red','green','yellow')
is.character(x)

x=c('blue',10,'green',20)
is.character(x)


#*****************************************
# LES FACTEURS
#*****************************************
# Ils représentent des catégories correspondant à des variables nominales. Les factors sont des 
#types particuliers de vecteurs qui sont utilisés pour représenter des variables caractère ou ordinales

genre<-factor(c("homme", "femme"))
genre
is.factor(genre)

x = c(1, 2, 3, 3, 5, 3, 2, 4, NA)
is.factor(x)
x=factor(x, levels = c(1,2,3), ordered = T)
x

x <- c(11, 22, 47, 47, 11, 47, 11)
factor(x, levels = c(11, 22, 47), ordered = T)


#remplacer un  élément à l'intérieur d'un vecteur de facteurs

z <- c("p", "a" , "g", "t", "b")
z[3]<-"i"

#If z <- factor(c("p", "q", "p", "r", "q")) and levels of z are "p", "q" ,"r", write an R expression that will change the 
#level "p" to "w" so that z is equal to: "w", "q" , "w", "r" , "q". 

z<-factor(c("p", "q", "p", "r", "q"))
levels(z)[1]<-"w"
levels(z)
z

#Concatenating 2 vectors of factors in one vector of factors

s1 <- factor(sample(letters, size=5, replace=TRUE))
s2 <- factor(sample(letters, size=5, replace=TRUE))
s3<-factor(c(levels(s1)[s1],levels(s2)[s2]))
s3

#*****************************************
# LES DATA FRAMES
#*****************************************

# Création d'un dataframe

df = data.frame(age, sexe)

# dimensions : nombre de lignes et de colonnes
dim(df)

# noms des variables 
names(df)
colnames(df)


# numéros d'observations
rownames(df)


# structure du dataframe : description du types de variables présentes
str(df)

# indexation de variables
df$age

# ajout de variables 
df$var1 <- 1:10
head(df, 4)
df[1:4,]
df$opinion= opinion
str(df)

# Suppression de variables
d[,3] <- NULL
d[1:3,]

# résumé détaillé de la distribution des variables
summary(df)


#*****************************************
#************ IMPORT DE DONNEES **********
#*****************************************

# fonctions read.table, read.csv et read.csv2

fic1 <- read.csv("fic1.csv", header = T,  sep = ";", na.strings ="NA", dec = "," ,strip.white= T)
fic2 <- read.csv("fic1.csv", header = T,  sep = ";", na.strings ="NA", dec = "," ,strip.white= T)
fic3 <- read.csv("fic1.csv", header = T,  sep = ";", na.strings ="NA", dec = "," ,strip.white= T)


#*****************************************
# Les sous populations : fonctions subsets
#*****************************************

#Breakup function nesting in R

datasets::mtcars
str(mtcars)

# 1rst case : classic nesting

head(mtcars[with(mtcars, cyl==8), c("mpg","cyl","gear","wt")])


# 2nd case : use magritt & dplyr

library(dplyr)
mtcars %>%
  filter(cyl==8) %>%
  select(mpg,cyl,gear,wt) %>%
  head

# 3rd case : name reuse

result=mtcars
result=filter(result, cyl==8)
result=select(result, mpg,cyl,gear,wt)
head(result)

# 4th case: dot intermediates

.<-mtcars
.<-subset(., cyl==8)
.<-.[, c("mpg","cyl","gear","wt")]
result<-.
head(result)


# Comparaison des temps de traitement
install.packages("microbenchmark")
library(microbenchmark)
library(ggplot2)

timings<-microbenchmark(
  
  base={
    .<-mtcars
    .<-subset(., cyl==8)
    .<-.[, c("mpg","cyl","gear","wt")]
    nrow(.)
  },
  
  dplyr= {
    mtcars  %>%
      filter(cyl==8)%>%
      select(mpg,cyl,gear,wt) %>%
      nrow
  })

print(timings)

autoplot(timings)

g=matrix(c(1:15), ncol=3, nrow = 5,
         dimnames = list(c(),c()))


#****************
#Vecteurs logiques et opérateurs
#****************
datasets::mtcars
str(mtcars)

# Output only those rows of  data  where column  cyl  is equal to 6 and column  am  is not 0.

#Méthode 1 : fonction subset
test=subset(mtcars,mtcars$cyl==6 & mtcars$am!=0) 
test=test[,c(2,9)]

#Méthode 2: avec dplyr
mtcars %>%
  filter(cyl==6 & am!=0)%>%
  select(cyl, am)



# gear or carb==4  
mtcars %>%
  filter(gear==4 | carb==4)%>%
  select(gear, carb)


#*****************************************
#************ USEFUL FUNCTIONS ***********
#*****************************************

ls() #	Display all objects in the current workspace

rm(a, b, ..) 	# Removes the objects a, b… from your workspace

rm(list = ls()) 	# Removes all objects in your workspace

getwd() 	# Returns the current working directory

setwd(file = "dir") 	#Changes the working directory to a specified file location

list.files() 	# Returns the names of all files in the working directory

write.table(x, file = "mydata.txt", sep = ";") # writes the object x to a text file called mydata.txt. Define how the columns will be separated with sep (e.g.; sep = "," for a comma–separated file, and sep = \t" for a tab–separated file).

save(a, b, .., file = "myimage.RData") 	# Saves objects a, b, … to myimage.RData

save.image(file = "myimage.RData") 	# Saves all objects in your workspace to myimage.RData

load(file = "myimage.RData") 	# Loads objects in the file myimage.RData

read.table(file = "mydata.txt", sep, header) #	Reads a text file called mydata.txt, define how columns are separated with sep (e.g. sep = "," for comma-delimited files, and sep = "\t" for tab-delimited files), and whether there is a header column with header = TRUE
