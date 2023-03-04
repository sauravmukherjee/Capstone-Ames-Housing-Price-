##########################################################
# Create edx and final_holdout_test sets 
##########################################################


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(rstudioapi)) install.packages("rstudioapi", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)
library(dplyr)
library(here)
library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

#Ames Iawa Housing Price dataset:
# https://www.openintro.org/data/csv/ames.csv

options(timeout = 120)

dl <- "data/AmesHousing.csv"


#dl <- "ames.csv"
#if(!file.exists(dl))
#  download.file("https://www.openintro.org/data/csv/ames.csv", dl)


#sdata <- read.csv(dl, header = TRUE, sep = ",")
#sdata

# views the data frame formed from the csv file
#View(sdata)


housing_data <- read.csv(dl)


# Exploratory Data Analysis

# Check for the missing values
na_count <-sapply(housing_data, function(housing_data) sum(length(which(is.na(df)))))
na_count




head(housing_data)

head(housing_data$SalePrice)

colnames(housing_data)


# House prices


housing_data %>% 
  ggplot(aes(SalePrice/1000)) + 
  geom_histogram(bins = 25, color = "black") + 
  #scale_x_log10() + 
  scale_x_continuous() +
  labs(title = "Distribution of house prices", x = "Price(,000)", y = "Frequency") +
  #ggtitle("House Price in (,000)", x = "Price (,000)", y = "Frequency")
  theme_minimal()

# Building Age

barplot(table(housing_data$Year.Built), 
        main = "Year built?", 
        xlab = "Year",
        ylab = "No of houses") #,
       # col = brewer.pal(9, "Blues"))

# Overall COndition

barplot(table(housing_data$Overall.Cond), 
        main = "In what condition are the most houses on the market?", 
        xlab = "Condition -> Worst to  Best",
        ylab = "Number of houses") #,
#        col = brewer.pal(10, "RdYlBu"))


# Living area
housing_data %>% 
  ggplot(aes(area)) + 
  geom_histogram(bins = 25, color = "black") + 
  #scale_x_log10() + 
  scale_x_continuous() +
  labs(title = "Distribution of Area", x = "Area (sqft)", y = "Frequency") +
  theme_minimal()

ggplot(df, aes(x = Gr.Liv.Area)) +
  geom_histogram(color = "black", fill = "pink1", bins = 50) + 
  scale_x_continuous() #(labels = comma) +
  labs(title = "Distribution of Living Area sizes", x = "Living area (sqft)", y = "Frequency") +
  theme_minimal()



## "SalePrice"
housing_data<- housing_data %>% 
  mutate(across(everything(), ~replace(.x, is.nan(.x), 0)))



housing_data <- housing_data %>%
  mutate(area = as.integer(area),
         price = as.numeric(price))


# test set will be 20% of housing_data data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = housing_data$price, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- housing_data[-test_index,]
test_set <- housing_data[test_index,]

head(test_set$price)



############### Data Exploration and Visualization



knitr::kable(dim(housing_data),caption = "Ames Housing Dataset dimension")

knitr::kable(head(housing_data), caption = "Ames Housing Dataset")

knitr::kable(summary(housing_data), caption = "Ames Housing Dataset Summary") 

distinct_Housing <- housing_data %>% summarize(n_users = n_distinct(userId),
                              n_movies = n_distinct(movieId), n_genres = n_distinct(genres))

knitr::kable(distinct_Housing, "pandoc", caption = "Unique users, movies, and genres")



 
#########################################################
### Data Wrangling 

########################################################




train$TotalArea<-train$X1stFlrSF+train$X2ndFlrSF+train$TotalBsmtSF


edx <- edx %>% mutate(review_date = round_date(as_datetime(timestamp), unit = "week"))

final_holdout_test <- final_holdout_test %>% mutate(review_date = round_date(as_datetime(timestamp), unit = "week"))

#extract release year from title

pattern <- "(?<=\\()\\d{4}(?=\\))"


edx <- edx %>% mutate(temp = str_extract(title, regex( pattern     )),    
                      release_yr = str_extract(temp, regex(   "(\\d{4})"   )),      
                      release_yr = as.numeric(release_yr))  #...convert to a number



final_holdout_test <- final_holdout_test %>% mutate(temp = str_extract(title, regex( pattern     )),  
                                                    release_yr = str_extract(temp, regex(   "(\\d{4})"   )),      
                                                    release_yr = as.numeric(release_yr))  #...convert to a number


knitr::kable(head(edx), caption = "EDX Dataset after wrangling")


# Data Visualization to identify some patterns

# Review a sample of 100 users and their ratings 

users <- sample(unique(edx$userId), 100)

rafalib::mypar()

edx %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(xlab="Movies", ylab="Users")

# Movies Rated by users - some movies are rated more than the others

edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 25, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies Rates")

# Users rating the movies - some users are more active than the others

edx %>%
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 25, color = "black") + 
  scale_x_log10() +
  ggtitle("Active Users")


# Plot number of ratings vs. year of release

edx %>% group_by(release_yr) %>%
  summarise(count = n()) %>%
  ggplot(aes(release_yr, count)) +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 800000, 100000), labels = seq(0, 800, 100)) +
  labs(x = "Release Year", y = "No of Ratings (thousands)", caption = "No of Rating vs. Year")

#####





###########################################################################################################################
# Recommendation System Model - develop, train and test
###########################################################################################################################



#library(caret)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


# Calculate the overall average rating across all movies included in the training set

mu_hat <- mean(train_set$price)

head (test_set$price)

RMSE(test_set$price, mu_hat)

# Calculate RMSE based on naive model
naive_rmse <- RMSE(test_set$price, mu_hat)
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
cat("\nNaive RMSE :",naive_rmse)

# Estimate Area effect (b_a)

# fit <- lm(rating ~ as.factor(userId), data = movielens)

area_avgs <- train_set %>% 
  group_by(area) %>% 
  summarize(b_a = mean(price - mu_hat))

area_avgs %>% qplot(b_a, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_price <- mu_hat + test_set %>% 
  left_join(area_avgs, by='area') %>%
  .$b_a

head(predicted_price)

typeof(predicted_price)

predicted_price %>% mutate(b_a = ifelse(is.na(b_a), 0, b_a))

predicted_price$test <- ifelse(is.na(predicted_price$b_a), 0, predicted_price$b_a)

# Calculate RMSE based on area effects
model_1_rmse <- RMSE(test_set$price, predicted_price)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Area Effect Model",
                                     RMSE = model_1_rmse ))

rmse_results %>% knitr::kable()

# Estimate user effect (b_u) along with Movie effect (b_i)

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")


# lm(rating ~ as.factor(movieId) + as.factor(userId))
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

#head(user_avgs)

# Predict ratings adjusting model for movie and user effects

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  .$pred

# Calculate RMSE based on movie and user effects
model_2_rmse <- RMSE(test_set$rating, predicted_ratings )

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

# Predict ratings adjusting model for movie(b_i) and user effects (b_u) and  genre effect (b_g)

genre_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarise(b_g = mean(rating - mu_hat - b_i - b_u))

# Predict ratings adjusting for movie, user and genre effects
predicted_i_b_g <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  .$pred

# Calculate RMSE based on movie, user and genre effects
model_i_u_g_rmse <- RMSE( test_set$rating, predicted_i_b_g)


rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User + genre Effects Model",  
                                 RMSE = model_i_u_g_rmse ))

rmse_results %>% knitr::kable()



# Predict ratings adjusting model for movie(b_i) and user effects (b_u), genre effect (b_g) and time effect (b_y)

# Estimate release year effect (b_y)
release_yr_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  group_by(release_yr) %>%
  summarise(b_y = mean(rating - mu_hat - b_i - b_u - b_g))
# Predict ratings adjusting for movie, user, genre and release year effects
predicted_i_b_g_t <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(release_yr_avgs, by = "release_yr") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_y) %>%
  pull(pred)



# Calculate RMSE based on movie, user, genre and time effects
model_i_u_g_t_rmse <- RMSE( test_set$rating, predicted_i_b_g_t)


rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User + genre + release year Effects Model",  
                                 RMSE = model_i_u_g_t_rmse ))

rmse_results %>% knitr::kable()

######################

# Estimate review date effect (b_r)
date_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(release_yr_avgs, by = "release_yr") %>%
  group_by(review_date) %>%
  summarise(b_r = mean(rating - mu_hat - b_i - b_u - b_g - b_y))

# Predict ratings adjusting for movie, user, genre, year and review date effects
predicted_i_b_g_t_r <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(release_yr_avgs, by = "release_yr") %>%
  left_join(date_avgs, by = "review_date") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_y + b_r) %>%
  pull(pred)

# Calculate RMSE based on movie, user, genre, time and review date effects
model_i_u_g_t_r_rmse <- RMSE( test_set$rating, predicted_i_b_g_t_r)


rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User + genre + release year + review date Effects Model",  
                                 RMSE = model_i_u_g_t_r_rmse ))

rmse_results %>% knitr::kable()


#######################


# Generate a sequence of values for lambda ranging from 4 to 6 with 0.1 increments (inc)
inc <- 0.1
lambdas <- seq(4, 6, inc)
# Regularise model, predict ratings and calculate RMSE for each value of lambda
rmses <- sapply(lambdas, function(l){
  movie_avgs <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu_hat)/(n()+l))
  user_avgs <- train_set %>%
    left_join(movie_avgs, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu_hat)/(n()+l))
  genre_avgs <- train_set %>%
    left_join(movie_avgs, by="movieId") %>%
    left_join(user_avgs, by="userId") %>%
    group_by(genres) %>%
    summarise(b_g = sum(rating - b_i - b_u - mu_hat)/(n()+l))
  release_yr_avgs <- train_set %>%
    left_join(movie_avgs, by="movieId") %>%
    left_join(user_avgs, by="userId") %>%
    left_join(genre_avgs, by="genres") %>%
    group_by(release_yr) %>%
    summarise(b_y = sum(rating - b_i - b_u - b_g - mu_hat)/(n()+l))
  date_avgs <- train_set %>%
    left_join(movie_avgs, by="movieId") %>%
    left_join(user_avgs, by="userId") %>%
    left_join(genre_avgs, by="genres") %>%
    left_join(release_yr_avgs, by="release_yr") %>%
    group_by(review_date) %>%
    summarise(b_r = sum(rating - b_i - b_u - b_g - mu_hat)/(n()+l))
  predicted_ratings <- test_set %>%
    left_join(movie_avgs, by="movieId") %>%
    left_join(user_avgs, by="userId") %>%
    left_join(genre_avgs, by="genres") %>%
    left_join(release_yr_avgs, by="release_yr") %>%
    left_join(date_avgs, by="review_date") %>%
    mutate(pred = mu_hat + b_i + b_u + b_g + b_y + b_r) %>%
    pull(pred)
  return(RMSE( test_set$rating, predicted_ratings))
})

# Plot lamdas vs. rmse 

qplot(lambdas, rmses)

# Assign optimal tuning parameter (lambda)
lambda <- lambdas[which.min(rmses)]
# Minimum RMSE achieved
regularised_rmse <- min(rmses)

cat("\nThe optimal lamda :",lambda)


rmse_results <- bind_rows(rmse_results,
                          tibble(method="regularised Movie + User + genre + release year + review date Effects Model",  
                                 RMSE = regularised_rmse ))

rmse_results %>% knitr::kable()

###### Final Result


#Train the final model
l<- lambda

mu_hat <- mean(edx$rating)

movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_hat)/(n()+l))

user_avgs <- edx %>% 
  left_join(movie_avgs, by="movieId") %>%
#  mutate(b_i = ifelse(is.na(b_i), 0, b_i)) %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_hat)/(n()+l))

genre_avgs <- edx %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  mutate(b_u = ifelse(is.na(b_u), 0, b_u)) %>%
  group_by(genres) %>%
  summarise(b_g = sum(rating - b_i - b_u - mu_hat)/(n()+l))

release_yr_avgs <- edx %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(genre_avgs, by="genres") %>%
  mutate(b_g = ifelse(is.na(b_g), 0, b_g)) %>%
  group_by(release_yr) %>%
  summarise(b_y = sum(rating - b_i - b_u - b_g - mu_hat)/(n()+l))

date_avgs <- edx %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(genre_avgs, by="genres") %>%
  left_join(release_yr_avgs, by="release_yr") %>%
  group_by(review_date) %>%
  summarise(b_r = sum(rating - b_i - b_u - b_g - mu_hat)/(n()+l))


predicted_ratings <- 
  final_holdout_test %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by="genres") %>%  
  left_join(release_yr_avgs, by="release_yr") %>%  
  left_join(date_avgs, by="review_date") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_y + b_r) %>%
  .$pred

model_final_rmse <- RMSE( final_holdout_test$rating, predicted_ratings)


rmse_results <- bind_rows(rmse_results,
                          tibble(method="Final RMSE -> regularised Movie + User + genre + release year + review date Effects Model",  
                                 RMSE = model_final_rmse ))


rmse_results %>% knitr::kable()

## Final RMSE of the final holdout set

cat("\nThe final RMSE :",  model_final_rmse)

rmse_results %>% knitr::kable()






