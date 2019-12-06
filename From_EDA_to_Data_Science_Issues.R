
#***************************************************************************
#*********************** Load Necessary Packages ***************************
#***************************************************************************


packages = c("funModeling","tidyverse","caret","dplyr")

package_check <- lapply(packages, FUN = function(x){
  
  if (!require(x, character.only=T)) {
    
    install.packages(x, dependencies = T)  
    library(x, character.only=T)
  }
  
})


#***************************************************************************
#*********************** Load Dataset **************************************
#***************************************************************************



data=read.delim("https://raw.githubusercontent.com/pablo14/data-integrity/master/messy_data.txt", sep = ';')
di=data_integrity(data)
summary(di)

status=df_status(heart_disease, print_results = F)
status


#***************************************************************************
#*********************** Start exploring the data **************************
#***************************************************************************


library(funModeling)
library(dplyr)
data(heart_disease)


#***************************************************************************
#**** 1 - Checking missing values, zeros, data type, and unique values *****
#***************************************************************************

# SUMMARY FUNCTIONS : describe is in the Hmisc package and all the rest is from funmODELLING
# df_status(data): Profiling dataset structure
# describe(data): Numerical and categorical profiling (quantitative)
# freq(data): Categorical profiling (quantitative and plot).
# profiling_num(data): Profiling for numerical variables (quantitative)
# plot_num(data): Profiling for numerical variables (plots)


# call df_status function in Funmodelling package to search for missing values

df_status(heart_disease)

#Legend :

  # q_zeros: quantity of zeros (p_zeros: in percent)
  # q_inf: quantity of infinite values (p_inf: in percent)
  # q_na: quantity of NA (p_na: in percent)
  # type: factor or numeric
  # unique: quantity of unique values

#***************************************************************************
#************************ 2 - Filtering unwanted cases *********************
#***************************************************************************

  # Removing variables with a high number of zeros
  # Profiling the Data Input

  my_data_status=df_status(heart_disease, print_results = F)
  
  # Removing variables with 60% of zero values
  vars_to_remove = filter(my_data_status, p_zeros > 60)  %>% .$variable
  vars_to_remove

  # Keeping all columns except the ones present in 'vars_to_remove' vector
  heart_disease_2 = select(heart_disease, -one_of(vars_to_remove))

  
#***************************************************************************
#***************** 3 - Ordering data by percentage of zeros*****************
#***************************************************************************    

  
  arrange(my_data_status, -p_zeros) %>% select(variable, q_zeros, p_zeros)

  
#***************************************************************************
#*********************** 4 - Getting to know the data***********************
#***************************************************************************    
  
  dim(heart_disease)
  colnames(heart_disease)  
  glimpse(heart_disease)  
  str(heart_disease)
    
  # Use the describe function
  heart_disease_3 = select(heart_disease, thal, chest_pain)
  describe(heart_disease_3)
  
  
  # n: quantity of non-NA rows. In this case, it indicates there are 301 patients containing a number.
  # missing: number of missing values. Summing this indicator to n gives us the total number of rows.
  # unique: number of unique (or distinct) values.
  
  
#***************************************************************************
#***************** 5 - Profiling categorical variables *********************
#***************************************************************************    
  
  freq(data = heart_disease, input = c('thal', 'chest_pain'))
  
  #if input is missing, then it will run for all factor or character variables present in a given data frame:
  
  freq(data = heart_disease_2)
  
  # remove plots and NAs 
  freq(data=heart_disease$thal, plot = FALSE, na.rm = TRUE)

  # Add parameters to export plots
  freq(data=heart_disease, na.rm = TRUE, path_out = "D:/coding/rstudio/data science_projects")
  
#***************************************************************************
#***************** 6 - Profiling numerical variables ***********************
#***************************************************************************   

    # using world bank data
  
  library("Hmisc")
  # Loading data from the book repository without altering the format
  data_world=read.csv(file = "https://goo.gl/2TrDgN", header = T, stringsAsFactors = F, na.strings = ".." )
  
  # Excluding missing values in Series.Code. The data downloaded from the web page contains four lines with "free-text" at the bottom of the file.
  data_world=filter(data_world, Series.Code!="")
  
  
  # The magical function that keeps the newest values for each metric.
  # The magical function that keeps the newest values for each metric. If you're not familiar with R, then skip it.
  max_ix<-function(d) 
  {
    ix=which(!is.na(d))
    res=ifelse(length(ix)==0, NA, d[max(ix)])
    return(res)
  }
  
  data_world$newest_value=apply(data_world[,5:ncol(data_world)], 1, FUN=max_ix)
  
  # Printing the first three rows
  head(data_world, 3)
  
  # Get the list of indicator descriptions.
  names=unique(select(data_world, Series.Name, Series.Code))
  head(names, 5)
  
  #Translate new indicators into clearer ones 
  # Convert a few
  df_conv_world=data.frame(
    new_name=c("urban_poverty_headcount", 
               "rural_poverty_headcount", 
               "gini_index", 
               "pop_living_slums",
               "poverty_headcount_1.9"), 
    Series.Code=c("SI.POV.URHC", 
                  "SI.POV.RUHC",
                  "SI.POV.GINI",
                  "EN.POP.SLUM.UR.ZS",
                  "SI.POV.DDAY"), 
    stringsAsFactors = F)
  
  # adding the new indicator value
  data_world_2 = left_join(data_world, 
                           df_conv_world, 
                           by="Series.Code", 
                           all.x=T)
  
  data_world_2 = 
    mutate(data_world_2, Series.Code_2=
             ifelse(!is.na(new_name), 
                    as.character(data_world_2$new_name), 
                    data_world_2$Series.Code)
    )
  
  
  # The package 'reshape2' contains both 'dcast' and 'melt' functions
  library(reshape2)
  
  data_world_wide=dcast(data_world_2, Country.Name  ~ Series.Code_2, value.var = "newest_value")
  
  
# DOING THE NUMERICAL PROFILING IN R
  
  library(Hmisc) # contains the `describe` function
  
  
  vars_to_profile=c("gini_index", "poverty_headcount_1.9")
  data_subset=select(data_world_wide, one_of(vars_to_profile))
  
  # Using the `describe` on a complete dataset. # It can be run with one variable; for example, describe(data_subset$poverty_headcount_1.9)
  
  describe(data_subset)
  
  
  
  
  