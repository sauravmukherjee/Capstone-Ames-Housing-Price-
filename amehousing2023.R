##########################################################
# Housing Price Prediction Model
# Dataset ames -> modeldata 
# Reference for Dataset https://modeldata.tidymodels.org/reference/ames.html
##########################################################

##https://modeldata.tidymodels.org/reference/ames.html


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(rstudioapi)) install.packages("rstudioapi", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(modeldata)) install.packages("modeldata", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(moments)) install.packages("moments", repos = "http://cran.us.r-project.org")
#if(!require(corrr)) install.packages("corrr", repos = "http://cran.us.r-project.org")
#if(!require(tidymodels)) install.packages("tidymodels", repos = "http://cran.us.r-project.org")
#if(!require(lares)) install.packages("lares", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(lubridate)
library(dplyr)
library(here)
library(rstudioapi)
library(broom)
library(modeldata)
library(ggplot2)
library(moments)
library(GGally)
#library(corrr)


# Load ames Dataset
data(ames)

# To make graphs more readable disabling scientific notation
options(scipen = 100)

options(repr.plot.width = 4, repr.plot.height =4)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))


options(timeout = 120)


#head(ames)



# Exploratory Data Analysis

knitr::kable(dim(ames),caption = "Ames Housing Dataset dimension")

knitr::kable(colnames(ames),caption = "Ames Housing Dataset Columns")

knitr::kable(str(ames),caption = "Ames Housing Dataset")

#knitr::kable(summary(ames),caption = "Ames Housing Dataset")


############### Data Exploration and Visualization




knitr::kable(dim(ames),caption = "Ames Housing Dataset dimension")

#knitr::kable(colnames(ames),caption = "Ames Housing Dataset Columns")

knitr::kable(str(ames),caption = "Ames Housing Dataset")

# Sale Price Characteristics 

ames %>% 
  ggplot(aes(Sale_Price)) + 
    geom_histogram(aes(y = ..density..), bins = 30,
                   colour = 1, fill = "black") +
  labs(title = "Distribution of house prices", x = "Price($)", y = "Frequency") +
  geom_density() +
  #theme_minimal() +
  theme_bw(base_size = 15)

# The distribution of SalePrice is right-skewed. Let's check its Skewness and Kurtosis statistics.
cat("\nSale Price skewness :", skewness(ames$Sale_Price))
cat("\nSale Price kurtosis :", kurtosis(ames$Sale_Price))


## Building Age

ames %>%
  ggplot(aes(Year_Built)) +
  geom_bar(color = "black") +
#  scale_x_continuous() +
 # scale_x_discrete(breaks = 10)  +
  labs(title = "Year Built", x = "Year Built", y = "No of Houses") 

# Overall COndition

ames %>%
  ggplot(aes(Overall_Cond)) +
  geom_bar(color = "black") +
  #  scale_x_continuous() +
  labs(title = "Overall Condition of the houses", x = "Overall Cond.", y = "No of Houses")




# Let's see median prices per neighborhood


ames %>% ggplot(aes(x = Neighborhood, y = Sale_Price)) +
  geom_boxplot() +
  ylab("Sale Price") +
  xlab("Neighbothood") +
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))


## House Price varies with the neighborhood with few outliers by neighborhood. 
#Also, the median house price by neighborhood is roughly between 200,000 and 400,000. 
#It seems Neighborhood would have some impact on housing price.
  
# Correlation between Sale Price and other variables 
  

  
  ames_num <- ames %>% select_if(~is.numeric(.x)) 
  
  
  ames_num %>% ggcorr() +
              ggplot2::labs(title = "Correlation between Numeric Variables")
  
  # Correlation of Sales Price with other numeric variables
  
  var_cors <- sapply(ames_num, function(var){
    var_cor <- cor(ames$Sale_Price,var )
    return(var_cor)
  })
  

  knitr::kable(var_cors,caption = "Ames Housing Dataset - correlated numeric variables with the Sale Price")
  
  # Correlation of Sales Price with other non-numeric variables
  
  ames_nonnum <- ames %>% select(where(~!is.numeric(.x) ))
  
  
  var_cors <- sapply(ames_nonnum, function(var){
    var_cor <- cor(ames$Sale_Price,rank(var) )
    return(var_cor)
  })
  
  knitr::kable(var_cors,caption = "Ames Housing Dataset - correlated non-numeric variables with the Sale Price")
  
  # Looking at the non-numeric variable, I identified few variables which are highly correlated - 
  # MS_Zoning, Lot_Shape, Foundation, Sale_Condition , Garage_Finish, House_Style, Heating_QC, 
  



#unique(ames$Overall_Cond)

#as.numeric(unique(ames$Overall_Cond))

  

  
#  housing <- housing %>%
#    mutate(TotalArea = as.integer(X1stFlrSF),
#           price = as.numeric(SalePrice))
  
  

  

  
#housing <- housing %>%
  #  mutate(Year.Built = as.integer(Year.Built),
  #         TotalArea = as.integer(TotalArea),
  #         SalePrice = as.numeric(SalePrice))
  


summarise(ames)

### Feature Engineering

########################################################

# Created a variable total_area = First_Flr_SF + Second_Flr_SF + Total_Bsmt_SF 


ames <- ames %>%
  mutate(total_Area = First_Flr_SF + Second_Flr_SF + Total_Bsmt_SF )


cat("\nCorelation between Total Area and Sale Price :", cor(ames$total_Area,ames$Sale_Price))


# Total_Bathroom considering full bath and half bath
ames <- ames %>%
  mutate(total_Bathroom =  Full_Bath + Bsmt_Full_Bath + 0.5* Half_Bath+ 0.5 * Bsmt_Half_Bath)

cat("\nCorelation between Total Bathroom and Sale Price :", cor(ames$total_Bathroom,ames$Sale_Price))

# Age of the house 
ames <- ames %>%
  mutate(house_Age =  Year_Sold - Year_Built)

cat("\nCorelation between Age of House and Sale Price :",  cor(ames$house_Age,ames$Sale_Price))

#X['reModeled'] = np.where(X.YearRemodAdd == X.YearBuilt, 0, 1)

### SalePrice_T -> Sales Price in Thousands

ames <- ames %>%
  mutate(Sale_Price_T = round(Sale_Price/1000))

ames$Sale_Price_T[is.na(ames$Sale_Price_T)] <- 0

### Overall Condition
#Levels: Very_Poor Poor Fair Below_Average Average Above_Average Good Very_Good Excellent Very_Excellent

ames <- ames %>%
  mutate(Overall_Cond_n = dplyr::recode(
    Overall_Cond,
    "Very_Excellent" = 10,
    "Excellent" = 9,
    "Very_Good" = 8,
    "Good" = 7,
    "Above_Average" = 6,
    "Average" = 5,
    "Below_Average" =4,
    "Fair" = 3,
    "Poor" = 2,
    "Very_Poor" =1
  )) 

cat("\nCorelation between Overall Condition and Sale Price :", cor(ames$Sale_Price,ames$Overall_Cond_n))



# Total area
ames %>% 
  ggplot(aes(total_Area)) + 
  geom_histogram(bins = 25, color = "black") + 
  #scale_x_log10() + 
  scale_x_continuous() +
  labs(title = "Distribution of Area", x = "Area (sqft)", y = "Frequency") +
  theme_minimal()


### Linear 
# Total Area vs. Price
ames %>%
  ggplot(aes(total_Area,Sale_Price_T)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Total Area vs. Sales Price", x = "Total Area (sqft)", y = "Sale Price ($,000)")


# Age of the House vs. Price

ames %>%
  ggplot(aes(house_Age ,Sale_Price_T)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Age of the house vs. Sales Price", x = "Age", y = "Sale Price ($,000)")



# Overall Condition vs. Price

ames %>%
  ggplot(aes(Overall_Cond_n,Sale_Price_T)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Overall Condition vs. Sales Price", x = "Overall Condition", y = "Sale Price ($,000)")




## Excluded Overall Condition from the parameter set 

ames <- ames %>% select (Sale_Price_T,total_Area, Gr_Liv_Area, house_Age, total_Bathroom ,Garage_Cars,Garage_Area,
                         Year_Remod_Add, Mas_Vnr_Area,  Lot_Shape, Foundation, Sale_Condition , Garage_Finish, House_Style, Heating_QC, 
                         MS_Zoning, Neighborhood  )



# Let's see the correlation matrix
ggcorr(ames, size = 3, label = TRUE, label_size = 4, label_round = 2, label_alpha = TRUE)

## "SalePrice"


#housing<- housing %>% 
#mutate_if(is.numeric, ~replace_na(., 0)) %>%
#  mutate_if(is.character, ~replace_na(., ""))

#housing<- housing %>% 
#  mutate(across(everything(), ~replace(.x, is.nan(.x), 0)))




# test set will be 20% of housing_data data
set.seed(2023, sample.kind="Rounding")
test_index <- createDataPartition(y = ames$Sale_Price_T, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- ames[-test_index,]
test_set <- ames[test_index,]



############## Data Exploration and Visualization



knitr::kable(dim(ames),caption = "Ames Housing Dataset dimension")

knitr::kable(head(ames), caption = "Ames Housing Dataset")

knitr::kable(summary(ames), caption = "Ames Housing Dataset Summary") 

#distinct_Housing <- housing_data %>% summarize(n_users = n_distinct(userId),
#                              n_movies = n_distinct(movieId), n_genres = n_distinct(genres))

#knitr::kable(distinct_Housing, "pandoc", caption = "Unique users, movies, and genres")



 



###########################################################################################################################
# Recommendation System Model - develop, train and test
###########################################################################################################################



# Linear Model

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm = TRUE))
}


# Calculate the overall average rating across all movies included in the training set

mu_hat <- mean(train_set$Sale_Price_T)


# Calculate RMSE based on naive model
naive_rmse <- round(RMSE(test_set$Sale_Price_T, mu_hat),2)
rmse_results <- tibble(method = "Just the average in ,000", RMSE = naive_rmse)
cat("\nNaive RMSE in ,000 :",naive_rmse)

# Linear Model Sale Price ~ total area + total bathroom 


#head(train_set)

model_ln1 <- train_set %>%
  #filter(yearID %in% 1961:2001) %>%
  #mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
  lm(Sale_Price_T ~ total_Area + total_Bathroom , data = .)

y_hat <- predict(model_ln1, newdata = test_set)

#RMSE(test_set$Sale_Price_T,y_hat)

summary(model_ln1)


# Calculate RMSE based on area effects
model_rmse <- RMSE(test_set$Sale_Price_T,y_hat)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Total Area and Total Bathroom Effect Model in in ,000",
                                     RMSE = model_rmse ))

rmse_results %>% knitr::kable()

# Estimate age of house effect along with total area  effect

model_ln2 <- ames %>%
  #filter(yearID %in% 1961:2001) %>%
  lm(Sale_Price_T ~ total_Area +total_Bathroom + house_Age + Garage_Cars + Garage_Area +
     Year_Remod_Add +Mas_Vnr_Area, data = .)

y_hat <- predict(model_ln2, newdata = test_set)

RMSE(test_set$Sale_Price_T,y_hat)


tidy(model_ln2, conf.int = TRUE)

summary(model_ln2)


# Calculate RMSE based on numeric attributes
model_rmse <- RMSE(test_set$Sale_Price_T,y_hat)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Model based on Numeric attributes of the dataset in ,000",  
                                     RMSE = model_rmse ))
rmse_results %>% knitr::kable()


## Non-Linear Model

## K Nearest Neighbor 



train_knn <- train(Sale_Price_T ~ ., method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))

summary(train_knn)

ggplot(train_knn, highlight = TRUE)

y_hat <- predict(train_knn, test_set, type = "raw")

# Calculate RMSE based on Knn Model
model_rmse <- RMSE(test_set$Sale_Price_T,y_hat)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Knn Model in ,000",  
                                 RMSE = model_rmse ))
rmse_results %>% knitr::kable()

#head(y_hat)
#head(test_set$Sale_Price_T)

#confusionMatrix(factor(y_hat,levels=1:490),factor(test_set$Sale_Price_T,levels=1:490))$overall["Accuracy"]


# fit a classification tree and plot it
train_rpart <- train(Sale_Price_T ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = train_set)
plot(train_rpart)


y_hat <- predict(train_rpart, test_set)

#y_hat <- factor(predict(train_rpart, test_set))

# Calculate RMSE based on Knn Model
model_rmse <- RMSE(test_set$Sale_Price_T,y_hat)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Random Forrest Model in ,000",  
                                 RMSE = model_rmse ))
rmse_results %>% knitr::kable()




#######################
