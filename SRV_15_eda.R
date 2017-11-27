
# DEFINE COLOR Vectors for plotting

vec.col2=c("blue","yellow")
vec.col3=c("blue", "yellow","red")
vec.col4=c("blue","yellow","green","grey")
vec.col5=c("blue","yellow","green","grey","red")
vec.col6=c("blue","yellow","green","grey","red","black")
vec.col7=c("yellow","blue")
vec.col8=c("yellow","red", "blue")
vec.col9=c("red", "blue", "yellow")
colfunc<-colorRampPalette(c("Blue","royalblue","lightblue"))
colfunc_5<-colorRampPalette(c("lightblue","Blue"))
colfunc_4<-colorRampPalette(c("Blue","royalblue","lightblue","lightgreen"))
mypalette_1<-brewer.pal(8,"Blues")
mypalette_2<-brewer.pal(8,"Accent")
mypalette_3<-brewer.pal(6,"Accent")
mypalette_4<-brewer.pal(4,"Accent")
mypalette_5<-brewer.pal(5,"Accent")
pal1<-brewer.pal(8,"Dark2")
#****************************
#Répertoire de travail
#****************************

setwd("C:/bassel/MACHINE LEARNING")
getwd()

#****************************
#chargement de la base
#****************************

SRVdata<-read.csv("SRVdata.csv", header = TRUE, sep = ";", na.strings = "")
str(SRVdata)
SRVdata
#Recodage variable dipmetier et certifs
INSTALL_1<-SRVdata$INSTALL_1
INSTALL_2<-SRVdata$INSTALL_2
INSTALL_3<-SRVdata$INSTALL_3
INSTALL_4<-SRVdata$INSTALL_4

levels(SRVdata$DIPMETIER3)
which(is.na(SRVdata$DIPMETIER3))
table(SRVdata$DIPMETIER3)

SRVdata$INSTALL_1[which(is.na(SRVdata$DIPMETIER3))]<-NA
SRVdata$INSTALL_1[which(SRVdata$DIPMETIER3=="Avant et après installation"|
                            SRVdata$DIPMETIER3=="Avant et après. Je me forme chaque année."|
                            SRVdata$DIPMETIER3=="avant et pendant"|
                            SRVdata$DIPMETIER3=="Avant et pendant"|
                            SRVdata$DIPMETIER3=="Avant votre installation"|          
                            SRVdata$DIPMETIER3=="Avant, après, pendant"|                                          
                            SRVdata$DIPMETIER3=="Avant, et après"|                                             
                            SRVdata$DIPMETIER3=="avant, pendant : chaque année un ajout"|                            
                            SRVdata$DIPMETIER3=="DU avant mon installation, VAE licence dans les mois qui ont suivi"|
                            SRVdata$DIPMETIER3=="HEC avant, Master au moment de l'installation"|                      
                            SRVdata$DIPMETIER3=="Avant, après et pendant et dans le futur ;)")]<-"Avant installation"
SRVdata$INSTALL_1[which(SRVdata$DIPMETIER3=="et pendant activite en vae"|
                            SRVdata$DIPMETIER3=="pendant mon activité"|
                            SRVdata$DIPMETIER3=="Au mitan de ma vie professionnelle"|
                            SRVdata$DIPMETIER3=="A votre installation en tant qu'indépendant"|
                            SRVdata$DIPMETIER3=="thérapeute avant mon installation et superviseur en 2016")]<-"Pendant l'activité"
SRVdata$INSTALL_1[which(SRVdata$DIPMETIER3=="8 ans après mon installation"|
                            SRVdata$DIPMETIER3=="Changement de profession"|
                            SRVdata$DIPMETIER3=="Dans les mois ou années qui ont suivi votre début d'activité"|
                            SRVdata$DIPMETIER3=="en 2017"|
                            SRVdata$DIPMETIER3=="Après 15 ans d'exercice dans pusieurs cabinets"|
                            SRVdata$DIPMETIER3=="les 3")]<-"Après l'installation"
SRVdata$INSTALL_1<-as.factor(as.character(SRVdata$INSTALL_1))



SRVdata$INSTALL_2[which(is.na(SRVdata$DIPFORMA3))]<-NA
SRVdata$INSTALL_2[which(SRVdata$DIPFORMA3=="Avant votre installation")]<-"Avant installation"
SRVdata$INSTALL_2[which(SRVdata$DIPFORMA3=="A votre installation en tant qu'indépendant")]<-"Pendant l'activité"
SRVdata$INSTALL_2[which(SRVdata$DIPFORMA3=="8 ans après mon installation"|SRVdata$DIPFORMA3=="Dans les mois ou années qui ont suivi votre début d'activité")]<-"Après l'installation"
SRVdata$INSTALL_2<-as.factor(as.character(SRVdata$INSTALL_2))
levels(SRVdata$INSTALL_2)

SRVdata$INSTALL_3[which(is.na(SRVdata$DIPCONSUL3))]<-NA
SRVdata$INSTALL_3[which(SRVdata$DIPCONSUL3 %in% c("projet europeen"))]<-NA
SRVdata$INSTALL_3[which(SRVdata$DIPCONSUL3=="Avant et pendant"|SRVdata$DIPCONSUL3=="Avant votre installation")]<-"Avant installation"
SRVdata$INSTALL_3[which(SRVdata$DIPCONSUL3=="A votre installation en tant qu'indépendant")]<-"Pendant l'activité"
SRVdata$INSTALL_3[which(SRVdata$DIPCONSUL3=="2006 et 2016"|SRVdata$DIPCONSUL3=="Dans les mois ou années qui ont suivi votre début d'activité")]<-"Avant installation"
SRVdata$INSTALL_3<-as.factor(as.character(SRVdata$INSTALL_3))
levels(SRVdata$INSTALL_3)

SRVdata$INSTALL_4[which(is.na(SRVdata$CERTIF3))]<-NA
SRVdata$INSTALL_4[which(SRVdata$CERTIF3=="A la fois avant mon installation et dans les mois qui ont suivis mon installation."|
                            SRVdata$CERTIF3=="A mon installation et dans les années qui ont suivi"|
                            SRVdata$CERTIF3=="Avant et après (je me forme tous les ans)"|
                            SRVdata$CERTIF3=="Avant et pendant"|
                            SRVdata$CERTIF3=="Avant votre installation"|
                            SRVdata$CERTIF3=="certains avant l'installation et d'autres après")]<-"Avant installation"
SRVdata$INSTALL_4[which(SRVdata$CERTIF3=="A votre installation en tant qu'indépendant"|
                            SRVdata$CERTIF3=="ET DANS LES MOIS QUI ONT SUIVIS"|
                            SRVdata$CERTIF3=="formation continue dans la vie reelle"|
                            SRVdata$CERTIF3=="Formations suivies au cours des 10 dernières années")]<-"Pendant l'activité"
SRVdata$INSTALL_4[which(SRVdata$CERTIF3=="2016 avec la réforme"|
                            SRVdata$CERTIF3=="2017"|
                            SRVdata$CERTIF3=="Changement de profession"|
                            SRVdata$CERTIF3=="Dans les mois ou années qui ont suivi votre début d'activité"|
                            SRVdata$CERTIF3=="L'année dernière"|
                            SRVdata$CERTIF3=="Lannée derniere"|
                            SRVdata$CERTIF3=="SFCoach début, RPCFI 2017")]<-"Après l'installation"
SRVdata$INSTALL_4<-as.factor(as.character(SRVdata$INSTALL_4))
levels(SRVdata$INSTALL_4)



SRVdata$PRINCIPAL_1[which(SRVdata$PRINCIPAL %in% c("La formation"))]<-"Formation"
SRVdata$PRINCIPAL_1[which(SRVdata$PRINCIPAL=="Le conseil"|
                    SRVdata$PRINCIPAL=="Le conseil, L'accompagnement et/ou coaching"|
                    SRVdata$PRINCIPAL=="Le conseil, La formation"|
                    SRVdata$PRINCIPAL=="Le conseil, La formation, L'accompagnement et/ou coaching")]<-"Conseil"
SRVdata$PRINCIPAL_1[which(SRVdata$PRINCIPAL=="La formation, L'accompagnement et/ou coaching"|SRVdata$PRINCIPAL=="L'accompagnement et/ou coaching")]<-"Accompagnement/Coaching"
SRVdata$PRINCIPAL_1<-as.factor(as.character(SRVdata$PRINCIPAL_1))
levels(SRVdata$PRINCIPAL_1)

table(SRVdata$PRINCIPAL)
#****************************
#chargement des labels des variables
#****************************

SRVlabels<-read.csv("labels_SRV.csv", header = TRUE,sep = ";")
SRVlabels
SRVvars<-data.frame(names(SRVdata))
SRV_var_label<-data.frame(SRVvars,SRVlabels)
library(xlsx)
library(xlsxjars)
write.table(SRV_var_label, "C:/bassel/MACHINE LEARNING/SRV_var_label.txt", sep="\t" )

#****************************
# EDA
#****************************


#exploration of NAs treatment

names(SRVdata)
levels(addNA(SRVdata$DIPMETIER))
table(addNA(SRVdata$DIPCONSUL))


#********************************************************
#implement a function to check Nas

na_check<-function(table, var)
  
  {

  table<-data.frame(SRVdata)
  var<-colnames(table)
  tabs<-table(addNA(table$var))
  
return(tabs)
                        
}


na_check(SRVdata, DIPMETIER)
#********************************************************

round(prop.table(table(addNA(SRVdata$DIPMETIER)))*100, digits = 1)    
round(prop.table(table(addNA(SRVdata$DIPFORMA)))*100, digits = 1) 
round(prop.table(table(addNA(SRVdata$DIPCONSUL)))*100, digits = 1) 
round(prop.table(table(addNA(SRVdata$CERTIF)))*100, digits = 1) 
round(prop.table(table(addNA(SRVdata$LOI2015)))*100, digits = 1) 
round(prop.table(table(addNA(SRVdata$LABEL)))*100, digits = 1) 
round(prop.table(table(addNA(SRVdata$COPYRIGHT)))*100, digits = 1) 
round(prop.table(table(addNA(SRVdata$CHARTE)))*100, digits = 1) 
round(prop.table(table(addNA(SRVdata$REFERENTIEL)))*100, digits = 1) 
round(prop.table(table(addNA(SRVdata$FREINS)))*100, digits = 1) 
round(prop.table(table(addNA(SRVdata$SATISJURI)))*100, digits = 1) 
round(prop.table(table(addNA(SRVdata$RECONNPROF)))*100, digits = 1) 
round(prop.table(table(addNA(SRVdata$REGUL)))*100, digits = 1) 
round(prop.table(table(addNA(SRVdata$REPREZ)))*100, digits = 1) 
round(prop.table(table(addNA(SRVdata$SUITEENQU)))*100, digits = 1) 
round(prop.table(table(addNA(SRVdata$ADHERENT)))*100, digits = 1) 


which(is.na(SRVdata$INSTALL_1))


#subset data with nas and remove them

SRV_binary<-subset(SRVdata, !is.na(DIPCONSUL) & !is.na(DIPFORMA) & !is.na(DIPMETIER) 
                     & !is.na(CERTIF) & !is.na(LOI2015) &!is.na(LABEL) &!is.na(COPYRIGHT)
                     &!is.na(CHARTE) & !is.na(REFERENTIEL) & !is.na(FREINS) &!is.na(SATISJURI)
                     & !is.na(RECONNPROF) & !is.na(REGUL) & !is.na(REPREZ) & !is.na(SUITEENQU)
                     & !is.na(ADHERENT) & !is.na(INSTALL_1)& !is.na(INSTALL_2)& !is.na(INSTALL_3)
                     & !is.na(INSTALL_4),
                    select=c(DIPCONSUL, DIPFORMA, DIPMETIER,CERTIF,LOI2015,LABEL,COPYRIGHT,
                        CHARTE,REFERENTIEL,FREINS,SATISJURI,RECONNPROF,REGUL,REPREZ,
                        SUITEENQU,ADHERENT, INSTALL_1, INSTALL_2, INSTALL_3, INSTALL_4))

attach(SRV_binary)

#EAD on single variables



SRV_binary<-within(SRV_binary,
                     DIPCONSUL<-factor(DIPCONSUL,
                                    levels = names(sort(table(DIPCONSUL),
                                                        decreasing = TRUE))))
counts=table(DIPCONSUL)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col2, names.arg = levels(DIPCONSUL), main = "diplôme de consultant",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8)


table(SRV_binary$INSTALL_1)
SRV_binary<-within(SRV_binary,
                     INSTALL_1<-factor(INSTALL_1,
                                       levels = names(sort(table(INSTALL_1),
                                                           decreasing = TRUE))))
counts=table(SRV_binary$INSTALL_1)
relfreq=counts/sum(counts)
barplot(na.omit(relfreq[order(relfreq,decreasing = TRUE)]), col=vec.col3, names.arg = levels(SRV_binary$INSTALL_1), main = "test",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8)


SRVdata<-within(SRVdata,
                     DIPFORMA<-factor(DIPFORMA,
                                       levels = names(sort(table(DIPFORMA),
                                                           decreasing = TRUE))))
counts=table(SRVdata$DIPFORMA)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col2, names.arg = levels(SRVdata$DIPFORMA), main = "diplôme de formateur",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8,
        font.axis = 2)

SRV_binary<-within(SRV_binary,
                     DIPMETIER<-factor(DIPMETIER,
                                       levels = names(sort(table(DIPMETIER),
                                                           decreasing = TRUE))))
counts=table(DIPMETIER)
relfreq=counts/sum(counts)
barplot(relfreq[order(relfreq,decreasing = TRUE)], col=vec.col2, names.arg = levels(DIPMETIER), main = "diplôme dans l'expertise métier",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8)


table(SRVdata$CERTIF)
SRVdata<-within(SRVdata,
                     CERTIF<-factor(CERTIF,
                                       levels = names(sort(table(CERTIF),
                                                           decreasing = TRUE))))
counts=table(SRVdata$CERTIF)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col7, names.arg = levels(SRVdata$CERTIF), main = "certifications en rapport avec l'activité exercée",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8,
        font.axis = 2)

SRV_binary<-within(SRV_binary,
                     LOI2015<-factor(LOI2015,
                                       levels = names(sort(table(LOI2015),
                                                           decreasing = TRUE))))
counts=table(LOI2015)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col2, names.arg = levels(LOI2015), main = "connaissance de la loi de 2015",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8 )

table(SRVdata$LABEL)
SRVdata<-within(SRVdata,
                     LABEL<-factor(LABEL,
                                       levels = names(sort(table(LABEL),
                                                           decreasing = TRUE))))
counts=table(SRVdata$LABEL)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col2, names.arg = levels(SRVdata$LABEL), main = "existence d'un label",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8,
        font.axis = 2
        )

table(SRVdata$COPYRIGHT)
SRVdata<-within(SRVdata,
                     COPYRIGHT<-factor(COPYRIGHT,
                                       levels = names(sort(table(COPYRIGHT),
                                                           decreasing = TRUE))))
counts=table(SRVdata$COPYRIGHT)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col2, names.arg = levels(SRVdata$COPYRIGHT), main = "protection intellectuelle",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8,
        font.axis=2)

SRV_binary<-within(SRV_binary,
                     CHARTE<-factor(CHARTE,
                                       levels = names(sort(table(CHARTE),
                                                           decreasing = TRUE))))
counts=table(CHARTE)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col2, names.arg = levels(CHARTE), main = "adhésion à une charte",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8 )

SRV_binary<-within(SRV_binary,
                     REFERENTIEL<-factor(REFERENTIEL,
                                       levels = names(sort(table(REFERENTIEL),
                                                           decreasing = TRUE))))
counts=table(REFERENTIEL)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col2, names.arg = levels(REFERENTIEL), main = "adhésion à un référentiel",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8 )

SRV_binary<-within(SRV_binary,
                     FREINS<-factor(FREINS,
                                       levels = names(sort(table(FREINS),
                                                           decreasing = TRUE))))
counts=table(SRV_binary$FREINS)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col7, names.arg = levels(SRV_binary$FREINS), main = "freins au développement de l'activité",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8)

levels(SRVdata$NATUREFREIN)
SRVdata<-within(SRVdata,
                     NATUREFREIN<-factor(NATUREFREIN,
                                    levels = names(sort(table(NATUREFREIN),
                                                        decreasing = TRUE))))
counts=table(SRVdata$NATUREFREIN)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col7, names.arg = levels(SRV_binary$NATUREFREIN), main = "nature des freins au développement de l'activité",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8)

table(SRVdata$SATISJURI)
SRVdata<-within(SRVdata,
                     SATISJURI<-factor(SATISJURI,
                                    levels = names(sort(table(SATISJURI),
                                                        decreasing = TRUE))))
counts=table(SRVdata$SATISJURI)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col7, names.arg = levels(SRVdata$SATISJURI), main = "satisfaction sur le statut juridique",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8,
        font.axis=2)

table(SRVdata$RECONNPROF)
SRVdata<-within(SRVdata,
                     RECONNPROF<-factor(RECONNPROF,
                                    levels = names(sort(table(RECONNPROF),
                                                        decreasing = TRUE))))
counts=table(SRVdata$RECONNPROF)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col8, names.arg = levels(SRVdata$RECONNPROF), main = "favorable à un dispositif de reconnaissance de la profession",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8,
        font.axis=2)

SRV_binary<-within(SRV_binary,
                     REGUL<-factor(REGUL,
                                    levels = names(sort(table(REGUL),
                                                        decreasing = TRUE))))
counts=table(SRV_binary$REGUL)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col8, names.arg = levels(SRV_binary$REGUL), main = "favorable à une meilleure réglementation de la profession",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8 )


table(SRVdata$REPREZ)
op <- par(mar=c(6,4,4,2))
SRVdata<-within(SRVdata,
                  REPREZ<-factor(REPREZ,
                                 levels = names(sort(table(REPREZ),
                                                     decreasing = TRUE))))
counts=table(SRVdata$REPREZ)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col8, names.arg = levels(SRVdata$REPREZ), main = "favorable à un dispositif de représentation de la profession" ,
        ylab="Réponses en %", las = 3, 
        cex.names=0.8,
        font.axis=2)


SRV_binary<-within(SRV_binary,
                     SUITEENQU<-factor(SUITEENQU,
                                    levels = names(sort(table(SUITEENQU),
                                                        decreasing = TRUE))))
counts=table(SRV_binary$SUITEENQU)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col7, names.arg = levels(SRV_binary$SUITEENQU), main = "souhaite être informé des suites de létude",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8)


SRV_binary<-within(SRV_binary,
                     ADHERENT<-factor(ADHERENT,
                                    levels = names(sort(table(ADHERENT),
                                                        decreasing = TRUE))))
counts=table(ADHERENT)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col2, names.arg = levels(ADHERENT), main = "adhésion au SRV",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8 )


#variables from SRVdata global

#change levels of var SEXE :
SRVdata$SEXE<-revalue(SRVdata$SEXE, c("un homme"="homme", "une femme"="femme" ))
SRVdata <-within(SRVdata ,
                     SEXE<-factor(SEXE,
                                    levels = names(sort(table(SEXE),
                                                        decreasing = TRUE))))
counts=table(SRVdata$SEXE)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col2, names.arg = levels(SRVdata$SEXE), main = "Sexe des répondants",
        ylab="Réponses en %", las = 1, 
        cex.names=0.8,
        font.axis=2)

op <- par(mar=c(8,4,4,2))
SRVdata <-within(SRVdata ,
                   SATISHORAI<-factor(SATISHORAI,
                                levels = names(sort(table(SATISHORAI),
                                                    decreasing = TRUE))))
counts=table(SRVdata$SATISHORAI)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$SATISHORAI), main = "satisfaction sur les tarifs horaires",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8,
        font.axis=2)

table(SRVdata$SOUTRAIT)
SRVdata <-within(SRVdata ,
                   SOUTRAIT<-factor(SOUTRAIT,
                                      levels = names(sort(table(SOUTRAIT),
                                                          decreasing = TRUE))))
counts=table(SRVdata$SOUTRAIT)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$SOUTRAIT), main = "satisfaction sur les tarifs en sous-traitance",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8,
        font.axis=2)

table(SRVdata$MARCHES)
SRVdata <-within(SRVdata ,
                   MARCHES<-factor(MARCHES,
                                    levels = names(sort(table(MARCHES),
                                                        decreasing = TRUE))))
counts=table(SRVdata$MARCHES)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$MARCHES), main = "satisfaction sur l'accès aux marchés publics",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8,
        font.axis=2)

table(SRVdata$EVALCFI)
SRVdata <-within(SRVdata ,
                   EVALCFI<-factor(EVALCFI,
                                   levels = names(sort(table(EVALCFI),
                                                       decreasing = TRUE))))
counts=table(SRVdata$EVALCFI)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$EVALCFI), main = "satisfaction sur l'évaluation des prestations des CFI",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8,
        font.axis=2)

levels(SRVdata$RELATIONS)
table(SRVdata$RELATIONS)
SRVdata <-within(SRVdata ,
                   RELATIONS<-factor(RELATIONS,
                                   levels = names(sort(table(RELATIONS),
                                                       decreasing = TRUE))))
counts=table(SRVdata$RELATIONS)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$RELATIONS), main = "les relations avec les donneurs d'ordre",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8,
        font.axis=2)


SRVdata <-within(SRVdata ,
                   REFEREN<-factor(REFEREN,
                                     levels = names(sort(table(REFEREN),
                                                         decreasing = TRUE))))
counts=table(SRVdata$REFEREN)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$REFEREN), main = "critères de référencement des CFI",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8 )

SRVdata <-within(SRVdata ,
                   NUMERIQUE<-factor(NUMERIQUE,
                                   levels = names(sort(table(NUMERIQUE),
                                                       decreasing = TRUE))))
counts=table(SRVdata$NUMERIQUE)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$NUMERIQUE), main = "disponibilité d'outils de FOAD",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8 )

SRVdata <-within(SRVdata ,
                   NUMERIQUE<-factor(NUMERIQUE,
                                     levels = names(sort(table(NUMERIQUE),
                                                         decreasing = TRUE))))
counts=table(SRVdata$NUMERIQUE)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$NUMERIQUE), main = "intégration des outils numériques",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8)


SRVdata <-within(SRVdata ,
                   TECHFOAD<-factor(TECHFOAD,
                                     levels = names(sort(table(TECHFOAD),
                                                         decreasing = TRUE))))
counts=table(SRVdata$TECHFOAD)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$TECHFOAD), main = "disponibilité d'outils de FOAD",
        ylab="Réponses en %", las = 3, 
        cex.names=0.8 )


op <- par(mar=c(4,4,4,2)) # the 10 allows the names.arg below the barplot
SRVdata <-within(SRVdata ,
                   AGECAT<-factor(AGECAT,
                                    levels = names(sort(table(AGECAT),
                                                        decreasing = TRUE))))
counts=table(SRVdata$AGECAT)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$AGECAT), main = "Distribution de l'âge des CFI",
        ylab="pourcentage de répondants", las = 1, 
        xlab="Tranches d'âge",
        cex.names=0.8 ,
        font.axis=2)
summary(SRVdata$AGE)

#change 0 par 0% dans les niveaux des vars suivants avant plot :

SRVdata$PARTPRIVE<-revalue(SRVdata$PARTPRIVE, c("0"="0%"))
SRVdata$PARTPUBLIC<-revalue(SRVdata$PARTPUBLIC, c("0"="0%"))
SRVdata$PARTICULIERS<-revalue(SRVdata$PARTICULIERS, c("0"="0%"))
SRVdata$OPCAS<-revalue(SRVdata$OPCAS, c("0"="0%"))



SRVdata <-within(SRVdata ,
                   PARTPRIVE<-factor(PARTPRIVE,
                                  levels = names(sort(table(PARTPRIVE),
                                                      decreasing = TRUE))))
counts=table(SRVdata$PARTPRIVE)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$PARTPRIVE), main = "part du privé dans l'activité des CFI",
        ylab="part des répondants en %", las = 1, 
        xlab="pourcentage de l'activité",
        cex.names=0.8 )

SRVdata <-within(SRVdata ,
                   PARTPUBLIC<-factor(PARTPUBLIC,
                                     levels = names(sort(table(PARTPUBLIC),
                                                         decreasing = TRUE))))
counts=table(SRVdata$PARTPUBLIC)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$PARTPUBLIC), main = "part du public dans l'activité des CFI",
        ylab="part des répondants en %", las = 1, 
        xlab="pourcentage de l'activité",
        cex.names=0.8 )

SRVdata <-within(SRVdata ,
                   PARTICULIERS<-factor(PARTICULIERS,
                                      levels = names(sort(table(PARTICULIERS),
                                                          decreasing = TRUE))))
counts=table(SRVdata$PARTICULIERS)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$PARTICULIERS), main = "part des particuliers dans l'activité des CFI",
        ylab="part des répondants en %", las = 1, 
        xlab="pourcentage de l'activité",
        cex.names=0.8 )

SRVdata <-within(SRVdata ,
                   OPCAS<-factor(OPCAS,
                                        levels = names(sort(table(OPCAS),
                                                            decreasing = TRUE))))
counts=table(SRVdata$OPCAS)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$OPCAS), main = "part des opcas dans l'activité des CFI",
        ylab="part des répondants en %", las = 1, 
        xlab="pourcentage de l'activité",
        cex.names=0.8 )

SRVdata <-within(SRVdata ,
                   INSTITUTIONS<-factor(INSTITUTIONS,
                                 levels = names(sort(table(INSTITUTIONS),
                                                     decreasing = TRUE))))
counts=table(SRVdata$INSTITUTIONS)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$INSTITUTIONS), main = "part des institutions dans l'activité des CFI",
        ylab="part des répondants en %", las = 1, 
        xlab="pourcentage de l'activité",
        cex.names=0.8 )

SRVdata <-within(SRVdata ,
                   INSTALL_1<-factor(INSTALL_1,
                                        levels = names(sort(table(INSTALL_1),
                                                            decreasing = TRUE))))
counts=table(SRVdata$INSTALL_1)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col3, names.arg = levels(SRVdata$INSTALL_1), main = "période d'obtention du diplôme dans l'expertise métier",
        ylab="part des répondants en %", las = 1, 
        xlab="période",
        cex.names=0.8 )

SRVdata <-within(SRVdata ,
                   INSTALL_2<-factor(INSTALL_2,
                                     levels = names(sort(table(INSTALL_2),
                                                         decreasing = TRUE))))
counts=table(SRVdata$INSTALL_2)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col3, names.arg = levels(SRVdata$INSTALL_2), main = "période d'obtention du diplôme de formateur",
        ylab="part des répondants en %", las = 1, 
        xlab="période",
        cex.names=0.8 )

levels(SRVdata$INSTALL_3)
SRVdata <-within(SRVdata ,
                   INSTALL_3<-factor(INSTALL_3,
                                     levels = names(sort(table(INSTALL_3),
                                                         decreasing = TRUE))))
counts=table(SRVdata$INSTALL_3)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col2, names.arg = levels(SRVdata$INSTALL_3), main = "période d'obtention du diplôme de consultant",
        ylab="part des répondants en %", las = 1, 
        xlab="période",
        cex.names=0.8 )


SRVdata <-within(SRVdata ,
                   INSTALL_4<-factor(INSTALL_4,
                                     levels = names(sort(table(INSTALL_4),
                                                         decreasing = TRUE))))
counts=table(SRVdata$INSTALL_4)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col3, names.arg = levels(SRVdata$INSTALL_4), main = "période d'obtention de la certification",
        ylab="part des répondants en %", las = 1, 
        xlab="période",
        cex.names=0.8 )

SRVdata <-within(SRVdata ,
                   PRINCIPAL_1<-factor(PRINCIPAL_1,
                                     levels = names(sort(table(PRINCIPAL_1),
                                                         decreasing = TRUE))))
counts=table(SRVdata$PRINCIPAL_1)
relfreq=counts/sum(counts)
barplot(relfreq, col=colfunc(3), names.arg = levels(SRVdata$PRINCIPAL_1), main = "Activité dominante des CFI",
        ylab="part des répondants en %", las = 1, 
        cex.names=0.8,
        font.axis = 2)


levels(SRVdata$POSTURE_2)
table(SRVdata$POSTURE_2)
SRVdata <-within(SRVdata ,
                   POSTURE_2<-factor(POSTURE_2,
                                     levels = names(sort(table(POSTURE_2),
                                                         decreasing = TRUE))))
counts=table(SRVdata$POSTURE_2)
relfreq=counts/sum(counts)
op <- par(mar=c(11,4,4,2)) # the 10 allows the names.arg below the barplot
barplot(relfreq, col=mypalette_2, names.arg = levels(SRVdata$POSTURE_2), main = "postures du CFI dans l'exercice de l'activité",
        ylab="part des répondants en %", las = 2, 
        cex.names=0.8 ,
        font.axis=2)

rm(op)

op <- par(mar=c(11,4,4,2)) # the 10 allows the names.arg below the barplot
SRVdata <-within(SRVdata ,
                   FORMREPREZ_2<-factor(FORMREPREZ_2,
                                     levels = names(sort(table(FORMREPREZ_2),
                                                         decreasing = TRUE))))
counts=table(SRVdata$FORMREPREZ_2)
relfreq=counts/sum(counts)
barplot(relfreq, col=mypalette_3, names.arg = levels(SRVdata$FORMREPREZ_2), main = "Forme juridique de la représentation souhaitée",
        ylab="part des répondants en %", las = 2, 
        cex.names=0.8,
        font.axis=2)
rm(op)

# analyse de la var annee (de commencement de l'activité)

is.numeric(SRVdata$DUREEACTIV)

#Traitement de la variable spécification des freins à l'activité

freins<-read.csv("SRV_freins_activ.csv", header = TRUE, sep = ";")
str(freins)

op <- par(mar=c(4,20,4,2), cex=0.8) # the 10 allows the names.arg below the barplot
barplot(freins$MESURE[order(freins$MESURE, decreasing = FALSE)], names.arg = c("choix du statut","relations avec urssaf","statut juridique inadapté","gestion complexe de la tva","relations avec le RSI","qualité des formations",
                                     "mutuelle/Prévoyance","fiscalité","complexité de l'activité","assurance retraite","prise en charge des formations personnelles",
                                     "besoin de compétences autres","prise en charge des formations des clients","complexité de l'environnement","difficultés d'accès aux appels d'offre","contraintes réglementaires"
                                     ), main="nature des freins à l'activité des CFI",
        las=2,
        horiz = TRUE,
        col = colfunc_5(16),
        xlab = "Nombre de réponses",
             font.axis=2)
rm(op)
par(op)

SRVdata$DUREEACTIV[which(SRVdata$DUREEACTIV==1)]<-2016
SRVdata$DUREEACTIV[which(SRVdata$DUREEACTIV==3)]<-2014
SRVdata$DUREEACTIV[which(SRVdata$DUREEACTIV==4)]<-2013
SRVdata$DUREEACTIV[which(SRVdata$DUREEACTIV==5)]<-2012
SRVdata$DUREEACTIV[which(SRVdata$DUREEACTIV==7)]<-2010
SRVdata$DUREEACTIV[which(SRVdata$DUREEACTIV==10)]<-2007
SRVdata$DUREEACTIV[which(SRVdata$DUREEACTIV==15)]<-2002
SRVdata$DUREEACTIV[which(SRVdata$DUREEACTIV==25)]<-1992

#Create another date variable

DUREEACTIV_2<-SRVdata$DUREEACTIV_2
SRVdata$DUREEACTIV_2=2017-SRVdata$DUREEACTIV

#Recode the ones starting in 2017 as having 1 year of activity
SRVdata$DUREEACTIV_2[which(SRVdata$DUREEACTIV_2==0)]<-1

summary(SRVdata$DUREEACTIV_2)

hist(SRVdata$DUREEACTIV_2, col = "blue", las=3 , main = "nombre d'années d'exercice des CFI")

#catEgorize variable DUREEACTIV_2
SRVdata$DUREEACTIV_CHAR<-cut(SRVdata$DUREEACTIV_2,c(1,10,20,30,40,45))
table(SRVdata$DUREEACTIV_CHAR)

round(prop.table(table(SRVdata$DUREEACTIV_CHAR))*100)

SRVdata <-within(SRVdata ,
                   DUREEACTIV_CHAR<-factor(DUREEACTIV_CHAR,
                                  levels = names(sort(table(DUREEACTIV_CHAR),
                                                      decreasing = TRUE))))
counts=table(SRVdata$DUREEACTIV_CHAR)
relfreq=counts/sum(counts)
barplot(relfreq, col=vec.col5, names.arg = levels(SRVdata$DUREEACTIV_CHAR), main = "la durée d'activité du CFI",
        ylab="pourcentage de répondants", las = 1, 
        xlab="durée d'activité par intervalles de nombre d'années",
        cex.names=0.8 ,
        font.axis=2)

#rename levels for DUREEACTIV_CHAR variable and AGECAT variable

SRVdata$DUREEACTIV_CHAR<-revalue(SRVdata$DUREEACTIV_CHAR, c("(1,10]"="1 à 10 ans",
                                                                "(10,20]"="10 à 20 ans",
                                                                "(20,30]"="20 à 30 ans",
                                                                "(30,40]"="30 à 40 ans",
                                                                "(40,45]"="40 à 45 ans"))


levels(SRVdata$AGECAT)
SRVdata$AGECAT<-revalue(SRVdata$AGECAT, c("(50,60]"="50 à 60 ans",
                                                                "(40,50]"="40 à 50 ans",
                                                                "(60,73]"="60 à 73 ans",
                                                                "(30,40]"="30 à 40 ans",
                                                                "(0,30]"="0 à 30 ans"))


levels(SRVdata$DUREEACTIV_CHAR)



#variable age

summary(SRVdata$AGE)
hist(SRVdata$AGE, col = "blue", las=3 , main = "distribution de la variable age")

#catégorize age variable
head(SRVdata)
SRVdata$AGECAT<-cut(SRVdata$AGE,c(0,30,40,50,60,73))
table(SRVdata$AGECAT)


#***********************************************************
#******************ANALYSE BIVARIEE*************************
#***********************************************************

#adherent vs
#age groups vs nb of years of practice

ggplot(SRVdata, aes(x=AGECAT, y=DUREEACTIV_2, fill=AGECAT))+ geom_bar(stat = "identity")+ ggtitle("le nombre d'année d'exercice en fonction des classes d'âge")





#********************************************************************

#plot categorical variables

#diplomes dans l'expertise métier
a<-ggplot(data = subset(SRVdata, !is.na(DIPMETIER)), aes(x=DIPMETIER, fill=DIPMETIER))+
  geom_bar()+ 
  labs(x="", y="") +
  theme(legend.title = element_blank())+
  ggtitle("diplome dans l'expertise métier")

ggplotly(a)



#*************************************
#theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
    #  axis.ticks.x=element_blank())+ggtitle("obtention d'un diplome métier")
#*************************************

#SORT THE CATEGORICAL VARIABLES

summary(SRVdata$AGE)

hist(SRVdata$AGE, col = "lightblue")


SRVdata<-within(SRVdata,
                    DIPMETIER<-factor(DIPMETIER,
                                    levels = names(sort(table(DIPMETIER),
                                                        decreasing = TRUE))))

SRVdata<-within(SRVdata,
                  DIPFORMA<-factor(DIPFORMA,
                                    levels = names(sort(table(DIPFORMA),
                                                        decreasing = TRUE))))


SRVdata<-within(SRVdata,
                  DIPCONSUL<-factor(DIPCONSUL,
                                   levels = names(sort(table(DIPCONSUL),
                                                       decreasing = TRUE))))

SRVdata<-within(SRVdata,
                  CERTIF<-factor(CERTIF,
                                    levels = names(sort(table(CERTIF),
                                                        decreasing = TRUE))))

SRVdata<-within(SRVdata,
                  LABEL<-factor(LABEL,
                                 levels = names(sort(table(LABEL),
                                                     decreasing = TRUE))))

SRVdata<-within(SRVdata,
                  COPYRIGHT<-factor(COPYRIGHT,
                                 levels = names(sort(table(COPYRIGHT),
                                                     decreasing = TRUE))))

SRVdata<-within(SRVdata,
                  CHARTE<-factor(CHARTE,
                                    levels = names(sort(table(CHARTE),
                                                        decreasing = TRUE))))


SRVdata<-within(SRVdata,
                  REFERENTIEL<-factor(REFERENTIEL,
                                 levels = names(sort(table(REFERENTIEL),
                                                     decreasing = TRUE))))

SRVdata<-within(SRVdata,
                  FREINS<-factor(FREINS,
                                      levels = names(sort(table(FREINS),
                                                          decreasing = TRUE))))


SRVdata<-within(SRVdata,
                  SATISJURI<-factor(SATISJURI,
                                 levels = names(sort(table(SATISJURI),
                                                     decreasing = TRUE))))

SRVdata<-within(SRVdata,
                  RECONNPROF<-factor(RECONNPROF,
                                    levels = names(sort(table(RECONNPROF),
                                                        decreasing = TRUE))))

SRVdata<-within(SRVdata,
                  REGUL<-factor(REGUL,
                                     levels = names(sort(table(REGUL),
                                                         decreasing = TRUE))))

SRVdata<-within(SRVdata,
                  REPREZ<-factor(REPREZ,
                                levels = names(sort(table(REPREZ),
                                                    decreasing = TRUE))))

SRVdata<-within(SRVdata,
                  ADHERENT<-factor(ADHERENT,
                                 levels = names(sort(table(ADHERENT),
                                                     decreasing = TRUE))))


a<-ggplot(data = subset(SRVdata, !is.na(DIPMETIER)), aes(x=DIPMETIER, fill=DIPMETIER))+
  geom_bar()+ 
  labs(x="", y="") +
  theme(legend.title = element_blank())+
  ggtitle("diplome dans l'expertise métier")+
  geom_text()


ggplotly(a)


b<-ggplot(data = SRV_binary, aes(x=DIPFORMA,fill=DIPFORMA))+
  geom_bar()+
  labs(x="", y="") +
  theme(legend.title = element_blank())+
  ggtitle("diplôme de formateur")

ggplotly(b)


c<-ggplot(data = subset(SRVdata, !is.na(CERTIF)), aes(x=CERTIF, y=AGE, fill=DIPFORMA)+
  geom_bar(stat = "identity", position = position_dodge())+
  ggtitle("certification en fonction age")

ggplotly(c)

#stacked bar charts


