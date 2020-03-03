
# 1-chargement des données 

data=read.csv("data_hotels.csv", header = T, sep=";", na.strings = c("","NA"))
str(data)

#********************************************************************************
#Problème de qualité numéro 1 : informations manquantes sur le nombre de chambres
#********************************************************************************

#On crée une variable d'évaluation de la métrique 
EVAL_ROOMS=data$EVAL_ROOMS
data$EVAL_ROOMS[which(is.na(data$NBCHAMBRES))]<-"missing"
data$EVAL_ROOMS[which(!is.na(data$NBCHAMBRES))]<-"non_missing"


#On Visualise le pourcentage d'hotels avec des données manquantes sur la variable nombre de chambres 
data<-within(data,
             EVAL_ROOMS<-factor(EVAL_ROOMS,
                                       levels = names(sort(table(EVAL_ROOMS),
                                                           decreasing = TRUE))))
counts=table(data$EVAL_ROOMS)
relfreq=counts/sum(counts)
relfreq
vec.col1=c("blue","yellow")
barplot(relfreq, col=vec.col1, names.arg = levels(data$EVAL_ROOMS), main = "données manquantes sur le nombre de chambres",
        ylab="données manquantes en %", las = 1, 
        cex.names=0.8,
        font.axis = 2)

#*******************************************************************************************************
#Problème de qualité numéro 2 : existence de plusieurs liens dans le champ photo
#*******************************************************************************************************

# On crée une sous table links pour traiter les liens photos

library(stringr)
data$PHOTOS_2=as.character(data$PHOTOS)
links=data.frame(str_split_fixed(data$PHOTOS_2, ":", 3))

#On supprime les lignes des hotels sans aucun lien d'images

links$X1=as.character(links$X1)
links$X1[links$X1==""]<-NA
links$X1<-as.factor(links$X1)
which(is.na(links$X1))
links=links[-c(76,  96, 109, 126, 152, 153, 157, 161, 164, 167, 170, 174),]

# Traitement préalable des NA pour filtrer les hotels ayant un seul lien d'image
links$X3=as.character(links$X3)
links$X3[links$X3==""]<-NA
links$X3<-as.factor(links$X3)

#On crée une variable de décompte des hotels avec un seul lien d'image
lien_uniq=links$lien_uniq

#filtres conditionnels avec which
links$X3=as.character(links$X3)
links$lien_uniq[which(is.na(links$X3))]<-"one link" # si la colonne X3 est vide, cela veut dire  qu'il ya un seul lien
links$lien_uniq[which(!is.na(links$X3))]<-"several links"  # si la colonne X3 n'est pas vide cela veut dire qu'il yen a plusieurs


#On Visualise le pourcentage de d'hôtels avec plusieurs liens vs celui avec un seul lien d'image

links<-within(links,
             lien_uniq<-factor(lien_uniq,
                                       levels = names(sort(table(lien_uniq),
                                                           decreasing = TRUE))))
counts=table(links$lien_uniq)
relfreq=counts/sum(counts)
vec.col2=c("blue","lightblue")
barplot(relfreq, col=vec.col2, names.arg =levels(links$lien_uniq), main = "pourcentage de lignes avec plusieurs url ou 1 url
        ",
        ylab="résultats en %", las = 1, 
        cex.names=0.8,
        font.axis = 2)


#*******************************************************************************************************
#Problème de qualité numéro 3 : la non correspondance entre le nom de domaine et l'adresse de messagerie
#*******************************************************************************************************

library(data.table)
library(stringr)

#Construction de la sous-table mails pour traiter le problème des mails
x=as.vector(data$WEB)
mailmatch=data.frame(t(do.call("cbind",strsplit(as.character(data$MAIL),"@"))))
mails=data.frame(cbind(x,mailmatch))#
mails$X1<-NULL#
mails=setnames(mails,old=c("x","X2"), new=c("web","messagerie"))

#On crée une variable secure avec un filtre conditionnel:
  # 1- si une partie des caractères du site web se trouve dans les caractères de messagerie= TRUE
  # 2- s'il n'ya pas correspondance entre les 2 on met =FALSE

mails$secure=
  with(mails,
       str_detect(as.character(web), as.character(messagerie))
            )

# #On Visualise le pourcentage d'hotels concernés par ce problème de sécurité
# des données personnelles :

mails<-within(mails,
             secure<-factor(secure,
                                       levels = names(sort(table(secure),
                                                           decreasing = TRUE))))
counts=table(mails$secure)
relfreq=counts/sum(counts)
vec.col3=c("blue","green")
barplot(relfreq, col=vec.col3, names.arg =levels(mails$secure), main = "pourcentage d'hotels avec domaine de messagerie propre",
        ylab="résultats en %", las = 1, 
        cex.names=0.8,
        font.axis = 2)
