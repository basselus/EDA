
Current_sales <- c("25% Coffee", "50% Pizza", "10/1 Granola", "BOGO Cookies")
Current_sales.factor <- factor(Current_sales)
Current_sales.factor



#convert to a number
Current_sales <- c("1_Brie Cheese", "3_Blue Cheese", 
                   "4_Parmesan", "2_Cheddar", "2_Cheddar")
Current_sales.factor <- factor(Current_sales)
Numeric_key_sales<- as.numeric(Current_sales.factor)
Numeric_key_sales

#convert a number to a factor to find unique values
numbers <- factor(c(9, 8, 10, 8, 9))
numbers
a = table(numbers)
a

#Frequency dat summaries
cheese <- c("Blue Cheese", "Brie", "Cheddar", "Parmesan","Blue Cheese", 
            "Cheddar", "Blue Cheese", "Parmesan", "Cheddar", "Blue Cheese",
            "Blue Cheese", "Brie", "Cheddar", "Parmesan")

factor.cheese <-factor(cheese)

t = table(factor.cheese)
t


# 2-Way Frequency Table
items_sold <-read.csv("table.csv",header = TRUE, sep = ",")

table(items_sold$Items,items_sold$Store) # A will be rows, B will be columns


# 3-Way Frequency Table
ftable(items_sold$Store,items_sold$Promo,items_sold$Items)



#Preparing qualitative data


current_promos <- read.table(text="
   Promo       Cust.ID      
   25%Coffee        12          
   BOGOCookies      34          
   50%Pizza         56         
   10%Tea           34",
                             header=TRUE)

# indicator flags will show which customer will use promo (1) or not (0) : function model.matrix

with(current_promos,
     data.frame(model.matrix(~Promo-1,current_promos),
                Cust.ID))

current_promos_binary<-with(current_promos,
                            data.frame
                            (Cust.ID,
                              (model.matrix(~Promo-1,
                                            current_promos)
                              )))

#Prepare encoding for modelling

items_sold <-read.csv("items_sold.csv",header = TRUE, sep = ",")

with(items_sold,
     data.frame(model.matrix(~Item-1,items_sold), Customer.ID))

for_association<-with(items_sold,
                      data.frame
                      (Customer.ID,
                        (model.matrix(~Item-1,
                                      items_sold)),
                        Price,
                        Date
                      ))
