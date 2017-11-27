
#Directory setup
setwd("C:/bassel/MACHINE LEARNING")
getwd()

#Load tables to perform text mining on 2 vars
reconn<-read.csv("form_reconn.csv",stringsAsFactors = FALSE, sep=";", header = TRUE)
jurid<-read.csv("satis_jurid.csv",stringsAsFactors = FALSE, sep=";", header = TRUE)

#Check data structure
str(reconn)
str(jurid)

#Convert FORMRECONN and SATISJURI2 into factors
reconn$FORMRECONN_2<-factor((reconn$FORMRECONN_2))
jurid$SATISJURI2_2<-factor((jurid$SATISJURI2_2 ))


#Table distribution of vars
levels(reconn$FORMRECONN_2)
levels(jurid$SATISJURI2_2)


#PLOTS for 2 vars
op <- par(mar=c(10,4,4,2)) # the 10 allows the names.arg below the barplot
reconn<-within(reconn ,
               FORMRECONN_2<-factor(FORMRECONN_2,
                                       levels = names(sort(table(FORMRECONN_2),
                                                           decreasing = TRUE))))
counts=table(reconn$FORMRECONN_2)
relfreq=counts/sum(counts)
barplot(relfreq, col=mypalette_5, names.arg = levels(reconn$FORMRECONN_2), main = "Formes du dispositif de reconnaissance de la profession",
                ylab="part des répondants en %", las = 3, 
                cex.names=0.8,
        font.axis=2)



table(jurid$SATISJURI2_2)
jurid<-within(jurid ,
               SATISJURI2_2<-factor(SATISJURI2_2,
                                    levels = names(sort(table(SATISJURI2_2),
                                                        decreasing = TRUE))))
counts=table(jurid$SATISJURI2_2)
relfreq=counts/sum(counts)
barplot(relfreq, col=mypalette_4, names.arg = levels(jurid$SATISJURI2_2), main = "motifs d'insatisfaction sur le statut juridique",
        ylab="part des répondants en %", las = 1, 
        cex.names=0.8,
        font.axis=2)

legend("topright", cex = 0.8,
       legend = c("charges et fiscalité", "statut inadapté", "protection sociale", "précarité"),
       fill =mypalette_4)



#check vars distributions
round(prop.table(table(reconn$FORMRECONN_2))*100)
round(prop.table(table(jurid$SATISJURI2_2))*100)

#*****************************************************************************
#*****************************TEXT MINING*************************************
#***************************RECONN DATAFRAME**********************************
#*****************************************************************************
#*****************************************************************************

#****************************************************
#***********CLEAN AND STANDARDIZE TEXT DATA**********
#****************************************************  

#Install tm package
install.packages("tm")
library(tm)

#Use VectorSource function to create a source object from the sms_raw$TEXT vector
#1 indicate source of document and create a source object =VectorSource
#2 Suplly it to Vcorpus function
#3 Save the corpus object in a final object sms_corpus

#create the corpus  
reconn_corpus<-VCorpus(VectorSource(reconn$FORMRECONN))

#print the corpus
print(reconn_corpus)



#selection and indexation within the corpus
inspect(reconn_corpus[1:2])
as.character(reconn_corpus[[1]])  
lapply(reconn_corpus[1:2], as.character)

#standardize to use only lowercase
reconn_corpus_clean<-tm_map(reconn_corpus,content_transformer(tolower))
reconn_corpus_clean

#Verification
as.character(reconn_corpus[[22]])
as.character(reconn_corpus_clean[[22]])

#***************
#REMOVE NUMBERS 
#***************

getTransformations() #display text functions

reconn_corpus_clean<-tm_map(reconn_corpus_clean, removeNumbers)

#***************************************************************
#REMOVE STOP WORDS using stopwords function with "kind" argument
#***************************************************************

stopwords(kind = "fr")
reconn_corpus_clean<-tm_map(reconn_corpus_clean, removeWords, stopwords(kind = "fr"))

#******************
#REMOVE PUNCTUATION
#******************

reconn_corpus_clean <- tm_map(reconn_corpus_clean, removePunctuation)

#***********************
# STEMMING DOCUMENT WORDS
#***********************

# Install library snowball for stemming
install.packages("SnowballC")
library(SnowballC)



#apply to entire document
reconn_corpus_clean <- tm_map(reconn_corpus_clean, stemDocument)

#******************
#STRIP WHITE SPACES 
#******************

reconn_corpus_clean <- tm_map(reconn_corpus_clean, stripWhitespace)

#**********************
#DATA PREPARATION : DTM
#**********************

# 1 with default settings since the file has been manually prepared
reconn_dtm <- DocumentTermMatrix(reconn_corpus_clean)

# 2 directly from the raw data, unprocessed and overriding the default settings
reconn_dtm_2 <- DocumentTermMatrix (reconn_corpus_clean, control = list (
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE

))

#To force the two prior document term matrices to be identical, we can
#override the default stop words function with our own that uses the
#original replacement function. Simply replace stopwords = TRUE
#with the following:
#stopwords = function(x) { removeWords(x, stopwords())  

reconn_dtm
reconn_dtm_2    

#********************************************
#VISUALIZE WORD TEXT DATA WITH CLOUD FUNCTION
#********************************************

#install wordcloud package
install.packages("wordcloud")
library(wordcloud)

#Creating a wordcloud
wordcloud(reconn_corpus_clean, min.freq = 5, random.order = TRUE,scale=c(3, 0.5), colors = pal1)

#Comparing the clouds for different levels of the text target variable
levels(reconn$FORMRECONN_2)
table(reconn$FORMRECONN_2)

structuration<-subset(reconn, FORMRECONN_2 == "dispositif structuré")
nsp<-subset(reconn, FORMRECONN_2 == "ne sait pas")
autre<-subset(reconn, FORMRECONN_2 == "autre")
clients<-subset(reconn, FORMRECONN_2 == "clients")
pair_s<-subset(reconn, FORMRECONN_2 == "reconnaissance par pairs")


#create a wordcloud for each of these subsets
#use the max.words parameter to look at the 20 most common words in each of the two sets.
#scale parameter allows us to adjust the maximum and minimum font size for words in the cloud

wordcloud(structuration$FORMRECONN, max.words = 30, scale = c(4, 1), colors = pal1)
wordcloud(nsp$FORMRECONN, max.words = 15, scale = c(3, 2), colors = pal1)
wordcloud(autre$FORMRECONN, max.words = 15, scale = c(3, 2), colors = pal1)
wordcloud(clients$FORMRECONN, max.words = 10, scale = c(3, 0.5), colors = pal1)
wordcloud(pair_s$FORMRECONN, max.words = 10, scale = c(3, 2), colors = pal1)

#*****************************************************************************
#*****************************TEXT MINING************************************
#***************************JURID DATAFRAME***********************************
#*****************************************************************************
#*****************************************************************************

#****************************************************
#***********CLEAN AND STANDARDIZE TEXT DATA**********
#****************************************************  

#Install tm package
install.packages("tm")
library(tm)

#create the corpus  
jurid_corpus<-VCorpus(VectorSource(jurid$SATISJURI2))

#print the corpus
print(jurid_corpus)



#selection and indexation within the corpus

inspect(jurid_corpus[1:2])

as.character(jurid_corpus[[1]])  

lapply(jurid_corpus[1:2], as.character)

#standardize to use only lowercase


jurid_corpus_clean<-tm_map(jurid_corpus,content_transformer(tolower))
jurid_corpus_clean

#Vérification
as.character(jurid_corpus[[1]])
as.character(jurid_corpus_clean[[1]])

#***************
#REMOVE NUMBERS 
#***************

getTransformations() #display text functions

jurid_corpus_clean<-tm_map(jurid_corpus_clean, removeNumbers)

#***************
#REMOVE STOP WORDS using stopwords function with "kind" argument
#***************

stopwords(kind = "fr")


jurid_corpus_clean<-tm_map(jurid_corpus_clean, removeWords, stopwords(kind = "fr"))

#******************
#REMOVE PUNCTUATION
#******************

jurid_corpus_clean <- tm_map(jurid_corpus_clean, removePunctuation)

#***********************
# STEMMING DOCUMENT WORDS
#***********************

# Install library snowball for stemming

install.packages("SnowballC")
library(SnowballC)



#apply to entire document
jurid_corpus_clean <- tm_map(jurid_corpus_clean, stemDocument)

#******************
#STRIP WHITE SPACES 
#******************

jurid_corpus_clean <- tm_map(jurid_corpus_clean, stripWhitespace)

#**********************
#DATA PREPARATION : DTM
#**********************

# 1 with default settings since the file has been manually prepared
jurid_dtm <- DocumentTermMatrix(jurid_corpus_clean)

# 2 directly from the raw data, unprocessed and overriding the default settings
jurid_dtm_2 <- DocumentTermMatrix (jurid_corpus_clean, control = list (
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))


jurid_dtm
jurid_dtm_2    

#********************************************
#VISUALIZE WORD TEXT DATA WITH CLOUD FUNCTION
#********************************************

#install wordcloud package
install.packages("wordcloud")
library(wordcloud)

#Creating a wordcloud
wordcloud(jurid_corpus_clean, min.freq = 2, random.order = TRUE, colors = pal1)

#Comparing the clouds for different levels of the text target variable
levels(jurid$SATISJURI2_2)
table(jurid$SATISJURI2_2)

chargfisc<-subset(jurid, SATISJURI2_2 == "charges et fiscalité")
statut<-subset(jurid, SATISJURI2_2 == "statut inadapté")
protecsoc<-subset(jurid, SATISJURI2_2 == "protection sociale")
preca<-subset(jurid, SATISJURI2_2 == "précarité")

#create a wordcloud for each of these subsets
#Define color palette
pal1=brewer.pal(8, "Dark2")
wordcloud(chargfisc$SATISJURI2, max.words = 20, scale = c(3, 1), colors = pal1)
wordcloud(statut$SATISJURI2, max.words = 15, scale = c(3, 2),colors = pal1)
wordcloud(protecsoc$SATISJURI2, max.words = 20, scale = c(3, 2), colors = pal1)
wordcloud(preca$SATISJURI2, max.words = 15, scale = c(3, 1),colors = pal1)


#*****************************************************************************
#*****************************TEXT MINING************************************
#***************************VARIABLE PROPOZ***********************************
#*****************************************************************************
#*****************************************************************************

#Install tm package
install.packages("tm")
library(tm)

#create the corpus  
propoz_corpus<-VCorpus(VectorSource(SRVdata$PROPOZ))

#print the corpus
print(propoz_corpus)



#selection and indexation within the corpus

inspect(propoz_corpus[1:2])

as.character(propoz_corpus[[1]])  

lapply(propoz_corpus[1:2], as.character)

#standardize to use only lowercase


propoz_corpus_clean<-tm_map(propoz_corpus,content_transformer(tolower))
propoz_corpus_clean

#Vérification
as.character(propoz_corpus[[12]])
as.character(propoz_corpus_clean[[12]])

#***************
#REMOVE NUMBERS 
#***************

getTransformations() #display text functions

propoz_corpus_clean<-tm_map(propoz_corpus_clean, removeNumbers)

#***************
#REMOVE STOP WORDS using stopwords function with "kind" argument
#***************

stopwords(kind = "fr")


propoz_corpus_clean<-tm_map(propoz_corpus_clean, removeWords, stopwords(kind = "fr"))

#******************
#REMOVE PUNCTUATION
#******************

propoz_corpus_clean <- tm_map(propoz_corpus_clean, removePunctuation)

#***********************
# STEMMING DOCUMENT WORDS
#***********************

# Install library snowball for stemming

install.packages("SnowballC")
library(SnowballC)



#apply to entire document
propoz_corpus_clean <- tm_map(propoz_corpus_clean, stemDocument)

#******************
#STRIP WHITE SPACES 
#******************

propoz_corpus_clean <- tm_map(propoz_corpus_clean, stripWhitespace)

#**********************
#DATA PREPARATION : DTM
#**********************

# 1 with default settings since the file has been manually prepared
propoz_dtm <- DocumentTermMatrix(propoz_corpus_clean)

# 2 directly from the raw data, unprocessed and overriding the default settings
propoz_dtm_2 <- DocumentTermMatrix (propoz_corpus_clean, control = list (
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))


propoz_dtm
propoz_dtm_2    

#********************************************
#VISUALIZE WORD TEXT DATA WITH CLOUD FUNCTION
#********************************************

#install wordcloud package
install.packages("wordcloud")
library(wordcloud)


#Creating a wordcloud
wordcloud(propoz_corpus_clean, min.freq = 8, scale=c(4,0.8),random.order = TRUE, colors = pal1)

