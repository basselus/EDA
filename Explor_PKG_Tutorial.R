# Explor Package demo

install.packages("remotes")
remotes::install_github("juba/scatterD3")
remotes::install_github("juba/explor")
install.packages("explor")
library(FactoMineR)
library(explor)
library(remotes)
data("hobbies")
data("decathlon")

# Explore datasets

dim(hobbies)
status=df_status(hobbies, print_results = F)
status_h
str(hobbies)

dim(decathlon)
status=df_status(decathlon, print_results = F)
status_d
str(decathlon)



# Example 1 : principal component analysis (PCA)

pca <- PCA(decathlon[,1:12], quanti.sup = 11, graph = FALSE)
explor(pca)


# Example 2 : multiple correspondance analysis (MCA)

mca <- MCA(hobbies,quali.sup = 19:22, quanti.sup = 23, graph = FALSE)
explor(mca)

       