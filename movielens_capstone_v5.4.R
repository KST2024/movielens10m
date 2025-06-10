## ----setup, include=TRUE, warning=FALSE, echo=TRUE, message=FALSE------------------------------------------------------------------------------------------------------------------------------------------------

##########################################################
# BEGIN MOVIELENS PROJECT
##########################################################

##########################################################
# Reproduced AS-IS from the Course Guidelines
##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##########################################################
# Reproduction AS-IS from the Course Guidelines Ends
##########################################################

# Remove Variables used to hold filenames as they are not required anymore
rm(movies_file, ratings_file)



## ----Install required Libraries, echo=FALSE, message=FALSE, warning=FALSE, include=TRUE--------------------------------------------------------------------------------------------------------------------------

# Let us include libraries that will be required through our Project. We will install them if they are not already available
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")



## ----Initialise required Libraries, echo=TRUE, message=FALSE, warning=FALSE, include=TRUE------------------------------------------------------------------------------------------------------------------------

# Let us load the libraries
library(tidyverse)
library(caret)
library(ggthemes)
library(gridExtra)
library(stringr)
library(kableExtra)


## ----understand_dataset 1 , include=TRUE, warning=FALSE, echo = FALSE, message = FALSE, out.width= 120-----------------------------------------------------------------------------------------------------------

# Let us get the total number of entries 
print(c("The Total Number of Entries in the edx Dataset is :", nrow(edx)), quote = FALSE, justify = "left")


## ----understand_dataset 2 , include=TRUE, warning=FALSE, echo = FALSE, message = FALSE, out.width= 120-----------------------------------------------------------------------------------------------------------

# Let us get an idea of what the dataset contains by looking at it's structure
knitr::kable(head(edx, n=10),  align = 'cccccc', caption = "First 10 entries of edx") 



## ----understand_dataset 3 , include=TRUE, warning=FALSE, echo = FALSE, message = FALSE---------------------------------------------------------------------------------------------------------------------------
# Let us get the number of unique userId and movieId
users_movies <- edx %>% 
  summarize(unique_users = n_distinct(userId), unique_movies = n_distinct(movieId)) 
print(c("The Number of Unique Users are : ", users_movies$unique_users), quote = FALSE, justify = "left")
print(c("The Number of Unique Movies are : ", users_movies$unique_movies), quote = FALSE, justify = "left")
#remove data that is no longer required
rm(users_movies)


## ----understand_dataset 4 , include=TRUE, warning=FALSE, echo = FALSE, out.width="75%", message = FALSE----------------------------------------------------------------------------------------------------------

# Let us get the top Users  in terms of ratings. We will group Users by userId and Summarise the Mean of their ratings. 
# We will use GGplot to Visualise the Data using Bar Graphs. We will also ensure that we do not overload GGplot with too much Information to render by only feeding it with 10 entries. 
# UserId is set as character so that GGPlot does not try to interpret it as a numerical value. 
# Bar Graphs are Ordered by Number of Ratings Provided

top_10_users <- edx %>% 
  group_by(userId) %>% 
  summarize(avg_rating= mean(rating), ratings_provided = n(), .groups = "drop") %>% 
  top_n(10) %>% arrange(desc(ratings_provided))

p1 <- top_10_users %>%  
  ggplot(aes(x=reorder(as.character(userId),ratings_provided),y =ratings_provided)) + 
  geom_text (aes(label=ratings_provided), vjust = -0.25) +
  labs(x= "userId", y = "Ratings Provided", title = "Top Users by Ratings Provided ") +
  geom_col(width = 0.5) + theme_clean() + ylim(0,7500)

p1a <- top_10_users %>%  
  ggplot(aes(x=reorder(as.character(userId),ratings_provided),y =avg_rating)) + 
  geom_text (aes(label=round(avg_rating,digits = 2)), vjust = -0.25) +
  labs(x= "userId", y = "Average Rating", title = "Average Ratings Provided by Top Users ") +
  geom_col(width = 0.5) + theme_clean() + ylim(0,5)

grid.arrange(p1, p1a, nrow = 2)

# Remove Data that is no longer required 
rm(p1,p1a)

# Many Thanks to contributrs of the article https://stackoverflow.com/questions/25664007/reorder-bars-in-geom-bar-ggplot2-by-value



## ----understand_dataset 4a , include=TRUE, warning=FALSE, echo = FALSE, out.width="75%", message = FALSE---------------------------------------------------------------------------------------------------------

# Generate distibution of Ratings for Top 10 Users  

edx %>% filter (userId %in% top_10_users$userId) %>% 
  ggplot(aes( x=rating)) + geom_histogram(binwidth = 0.2) + 
  facet_wrap( ~ userId,  nrow =2, ncol =5 ) +  
  labs(x= "Ratings Provided", y = "Count", title = "Ratings Distributions of Top Users ") + theme_minimal()



## ----understand_dataset 5 , include=TRUE, warning=FALSE, echo = FALSE, out.width="75%", message = FALSE----------------------------------------------------------------------------------------------------------
# Extract the Top 10 Movies by number of ratings received

top_rated_10 <- edx %>% 
              group_by(title) %>% summarize(ratings_received = n()) %>% 
              top_n(10) %>% arrange(desc(ratings_received))

# Plot the Top 10 Movies by Number of Ratings Received

p2 <- top_rated_10 %>% 
  ggplot(aes(x=reorder(title, ratings_received),y =ratings_received)) + 
  geom_text(aes(label=ratings_received), hjust = -0.25) +
  labs(x= "title", y = "Ratings Received", title = "Top Movies by Number of Ratings ") + 
  geom_col() + coord_flip() + ylim(0,40000) + theme_clean() + theme(plot.title = element_text(hjust = 2))

# Plot the Average Rating of the Movies in the above list 

p2a <- edx %>% filter(title %in% top_rated_10$title) %>% 
  group_by(title) %>% summarize(ratings_received = n(), avg_rating = mean(rating)) %>% 
  top_n(10) %>% arrange(desc(ratings_received)) %>%
  ggplot(aes(x=reorder(title, ratings_received), y = avg_rating)) + 
  geom_text(aes(label=round(avg_rating, digits = 2), hjust = -0.25)) +
  labs(x= "title", y = "Average Rating", title = "Average Rating of Movies with Highest Number of Ratings ") +  geom_col() + coord_flip() + ylim(0,6) + theme_clean() + theme(plot.title = element_text(hjust = 1.15))

# Let us combine the Plots together and Present them

grid.arrange(p2, p2a, nrow = 2)

# Remove Data that is no longer required
rm( p2, p2a)


## ----understand_dataset 5a , include=TRUE, warning=FALSE, echo = FALSE, out.width="75%", message = FALSE---------------------------------------------------------------------------------------------------------

# Generate distibution of Ratings for Top 10 Movies
  
edx %>% filter (title %in% top_rated_10$title) %>% ggplot(aes( x=rating)) + geom_histogram(binwidth = 0.2) + facet_wrap( ~ title,  nrow =2, ncol =5, labeller = label_wrap_gen(width = 18, multi_line = TRUE) ) +  labs(x= "Ratings Received", y = "Count", title = "Ratings Distributions for Movies with Most Ratings") + theme_minimal()



## ----understand_dataset 6 , include=TRUE, warning=FALSE, echo = FALSE, out.width="75%", message = FALSE----------------------------------------------------------------------------------------------------------


# Let us get and plot the top Movies  in terms of Average Values of ratings. We will only consider Movies having 100 or more Ratings
p3 <- edx %>% 
  group_by(title) %>% 
  summarize(ratings_received = n(), avg_rating = round(mean(rating),digits=2)) %>% 
  filter(ratings_received >=100) %>% 
  top_n(10) %>% arrange(desc(avg_rating)) %>% 
  ggplot(aes(x=reorder(title, avg_rating),y =avg_rating)) + 
  geom_text(aes(label=avg_rating), hjust = -0.25) +
  labs(x= "title", y = "Average Rating", title = "Top Movies by Average Rating (100+ ratings) - Average Rating") + 
  geom_col() + coord_flip() + ylim(0,6) +  theme_clean() + theme(plot.title = element_text(hjust = 1))

# Let us plot the Ratings Received for Movies in the list above.

p3a <- edx %>% 
  group_by(title) %>% 
  summarize(ratings_received = n(), avg_rating = round(mean(rating),digits=2)) %>% 
  filter(ratings_received >=100) %>% 
  top_n(10) %>% arrange(desc(avg_rating)) %>% 
  ggplot(aes(x=reorder(title, avg_rating),y =ratings_received)) + 
  geom_text(aes(label=ratings_received), hjust = -0.25) +
  labs(x= "title", y = "Ratings Received", title = "Top Movies by Average Rating (100+ ratings) - Ratings Received") + 
  geom_col() + coord_flip() + ylim(0,40000) +  theme_clean() + theme(plot.title = element_text(hjust = 1))

# Present the Plots
grid.arrange(p3, p3a, nrow=2)



## ----understand_dataset 6a, include=TRUE, warning=FALSE, echo = FALSE, out.width="75%", message = FALSE----------------------------------------------------------------------------------------------------------

# Code Chunk Deliberately Broken up because of PDF conversion issues,
# Let us get the top Movies  in terms of Average Values of ratings. We will consider all Movies

p4 <- edx %>% 
  group_by(title) %>% 
  summarize(ratings_received = n(), avg_rating = round(mean(rating),digits=2)) %>% 
  top_n(10) %>% arrange(desc(avg_rating)) %>% 
  ggplot(aes(x=reorder(title, avg_rating),y =avg_rating)) + 
  geom_text(aes(label=avg_rating), hjust = -0.25) +
  labs(x= "title", y = "Average Rating", title = "Top Movies by Average Rating - Average Rating") + 
  geom_col() + coord_flip() + ylim(0,6.5) +  theme_clean() + theme(plot.title = element_text(hjust = 1))

# Let us plot the Ratings Received for Movies in the list above.

p4a <-  edx %>% 
  group_by(title) %>% 
  summarize(ratings_received = n(), avg_rating = round(mean(rating),digits=2)) %>% 
  top_n(10) %>% arrange(desc(avg_rating)) %>% 
  ggplot(aes(x=reorder(title, avg_rating),y =ratings_received)) + 
  geom_text(aes(label=ratings_received), hjust = -0.25) +
  labs(x= "title", y = "Ratings Received", title = "Top Movies by Average Rating - Ratings Received") + 
  geom_col() + coord_flip() + ylim(0,6.5) +  theme_clean() + theme(plot.title = element_text(hjust = 1))
 
# Present the Plots 
grid.arrange(p4, p4a, nrow = 2)

# Remove Data that is no longer required
rm(p3,p3a, p4, p4a)


## ----understand_dataset 7 , include=TRUE, warning=FALSE, echo = TRUE, message = FALSE----------------------------------------------------------------------------------------------------------------------------

# Extract Unique MovieId along with title and genres
movie_map <- edx %>% 
  select(movieId, title, genres) %>% 
  group_by(movieId, title, genres) %>% summarise(movieId= unique(movieId), .groups= "keep")

# Compute Average Rating for each movie along with number of ratings
movie_rating <- edx %>% group_by(movieId) %>% 
  summarise(avg_rating = mean(rating), number_of_ratings = n()) 

# Create Movie List by Combining above information 
movie_list <- left_join(movie_map,movie_rating,by="movieId")

# remove datasets that are no longer needed
rm(movie_rating, movie_map, users_movies)


## ----Build Loss and Clamp Functions, warning = FALSE, echo = TRUE, include = TRUE, message = FALSE---------------------------------------------------------------------------------------------------------------

# Let us build the loss function which we will use for evaluation
rmse <- function(actual_rating, predicted_rating) {
  sqrt(mean((actual_rating -predicted_rating)^2))
}
# Let us use the Clamp Function as provided in the text book to ensure that ratings stay within the defined lower and upper boundaries viz. 0.5 <= rating <= 5.0. It helps improve our RMSE quite substantially.
clamp <- function(x, min = 0.5, max = 5) pmax(pmin(x, max), min)


## ----create_cross_validation_sets, include=TRUE, warning=FALSE, echo=TRUE, message = FALSE-----------------------------------------------------------------------------------------------------------------------

# Create Data Sets for Cross Validation from the "edx" Dataset. Set Seed same as original to maintain consistency. Create edx_cv_train_set to contain 90% of Observations. We will use it for Training.

set.seed(1, sample.kind="Rounding")

edx_cv_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE) 
edx_cv_train_set <- edx[-edx_cv_test_index,]

temp_cv<- edx[edx_cv_test_index,]

# Create edx_cv_test_set to contain 10% of Observations. We will use it for Testing/Validation
# Make sure userId and movieId in edx_cv_test_set set are also in edx_cv_train_set.

edx_cv_test_set<- temp_cv %>% 
  semi_join(edx_cv_train_set, by = 'userId') %>% 
  semi_join(edx_cv_train_set, by = 'movieId') 

# Add rows removed from edx_cv_test_set back into edx_cv_train_set
removed_cv <- anti_join(temp_cv, edx_cv_test_set, by = join_by(userId, movieId, rating, timestamp, title, genres))
edx_cv_train_set <- rbind(edx_cv_train_set, removed_cv)

# Set userId and movieId as factors for further processing. Their numerical value has no significance

edx_cv_train_set <- edx_cv_train_set %>% 
    mutate(userId = as.factor(userId), movieId = as.factor(movieId))

edx_cv_test_set <- edx_cv_test_set %>% 
    mutate(userId = as.factor(userId), movieId = as.factor(movieId))

# Create Mean and SD for each user for future reference

user_mean_sd <- edx_cv_train_set %>% 
    group_by(userId) %>% summarise(mean=mean(rating), sd = sd(rating))

# remove datasets that are no longer required. 
rm(edx_cv_test_index, temp_cv, removed_cv)



## ----model 1 - Base - Arithmetic Mean of Ratings as Prediction, include=TRUE, warning=FALSE, echo=TRUE, message = FALSE------------------------------------------------------------------------------------------
# Compute values of Mean & Standard deviation
mu <- round(mean(edx_cv_train_set$rating), digits = 5)
sigma <- round(sd(edx_cv_train_set$rating), digits = 5)

# Compare Actual Ratings against the Mean as Prediction
rmse_base <-round(rmse(edx_cv_test_set$rating, mu),digits = 5)

# Print RMSE, Mean & Standard Deviation
print( c(" RMSE Base : ", round(rmse_base, digits = 5)), quote = FALSE, justify = "left")
print( c(" Mean Value (mu) : ", round(mu, digits = 5)), quote = FALSE, justify = "left")
print( c(" Standard Deviation (sigma) : ", round(sigma,digits = 5)), quote = FALSE, justify = "left")


## ----Model 2 - User Effects, include=TRUE, warning=FALSE, echo=TRUE, message = FALSE-----------------------------------------------------------------------------------------------------------------------------


# The Code is more suited for Memory Constrained Machines. 

# THE PIVOT WIDER METHOD FROM THE TEXT BOOK WITH "userId" AS ROWS AND "movieId" AS COLUMNS IS RECOMMENDED ON A STUDENT LAPTOP WITH SUFFICIENT MEMORY AND/OR CLOUD SERVICES PROVIDING SIMILAR AMOUNTS OF MEMORY. FOR REFERENCE THE CREATED MATRIX TAKES UP ALMOST 6GB OF SPACE ON ITS OWN IN R AND RSTUDIO USES UP ANOTHER 6GB ADDITIONAL WHEN THE MATRIX IS CREATED.

# For each User, Compute User Effects with alpha_i = mean (rating - mu) and Store in a new dataframe

user_effects <- edx_cv_train_set %>% 
  group_by(userId) %>% 
  summarize(alpha_i = mean(rating - mu)) 

# Prepare Predictions for edx_cv_test_set. Our Prediction for each Rating will be (mu + alpha_i) 

predicted_ratings_user <- mu + 
  edx_cv_test_set %>% left_join(user_effects, by= 'userId') %>%
  pull(alpha_i)

# Compute and record the RMSE by comparing our Predictions with the actual ratings provided by the user.

rmse_user <- round(rmse(edx_cv_test_set$rating, predicted_ratings_user), digits = 5)

# Print the RMSE as "RMSE User Effects"
print( c(" RMSE User Effects : ", rmse_user), quote = FALSE, justify = "left") 

# Remove Data that is no longer required.
rm(predicted_ratings_user)


## ----Model 2 User Effects - User distributions, include=TRUE, warning=FALSE, echo = FALSE, out.width="75%", message = FALSE--------------------------------------------------------------------------------------

# Create plot to visualise the Distribution of Ratings for Top 10 Users with the User Effects Included

user_effects %>% filter (userId %in% top_10_users$userId) %>% 
  ggplot(aes( x= round(mu + alpha_i, digits = 2))) + geom_histogram(binwidth = 0.2) + 
  facet_wrap( ~ userId,  nrow =2, ncol =5 ) +  
  labs(x= "userId", y = "Count", title = "Prediction Distribution for Top Users with User Effects ") + theme_minimal()



## ----Model 2 User Effects - Movie distributions, include=TRUE, warning=FALSE, echo = FALSE, out.width="75%", message = FALSE-------------------------------------------------------------------------------------

# Create plot to visualise the Distribution of Ratings for Top 10 Movies with the User Effects Included

user_effects %>% 
  left_join(edx_cv_train_set, by = join_by("userId")) %>% 
  filter (title %in% top_rated_10$title) %>% 
  ggplot(aes( x= round(mu + alpha_i, digits = 2))) + geom_histogram(binwidth = 0.2) + 
  facet_wrap( ~ title,  nrow =2, ncol =5 ) +  
  labs(x= "Movie Title", y = "Count", title = "Prediction Distribution for Top Movies with User Effects ") + theme_minimal()



## ----Model 3 - Movie Effects, include=TRUE, warning=FALSE, echo=TRUE, message = FALSE----------------------------------------------------------------------------------------------------------------------------

#  For each Movie, compute Movie Effects as beta_j = mean(rating - mu - alpha_i) 

movie_effects <- edx_cv_train_set %>% left_join(user_effects, by = "userId") %>% 
  group_by(movieId) %>% 
  summarize(beta_j = mean(rating - mu - alpha_i)) 

# Prepare Predictions for edx_cv_test_set. Our Prediction for each Rating will be mu + alpha_i + beta_j 
predicted_ratings_user_movie <- edx_cv_test_set %>% 
      left_join(movie_effects, by='movieId') %>%
      left_join(user_effects, by='userId') %>%
      mutate(pred = clamp(mu + beta_j + alpha_i)) %>%
      pull(pred)

# Compute and record the RMSE by comparing our Predictions with the actual ratings provided by the user.
rmse_user_movie <- round(rmse(edx_cv_test_set$rating, predicted_ratings_user_movie), digits = 5)

# Print the RMSE as "RMSE User & Movie Effects"
print( c(" RMSE User & Movie Effects : ", rmse_user_movie), quote = FALSE, justify = "left") 

# Remove Data that is no longer required
rm(predicted_ratings_user_movie)


## ----Model 3 - Movie Effects - User Distributions , include=TRUE, warning=FALSE, echo = FALSE, out.width="75%", message = FALSE----------------------------------------------------------------------------------

# Create plot to visualise the Distribution of Predictions for Top 10 Users with the User Effects & Movie Effects Included
  
movie_effects %>% 
  left_join(edx_cv_train_set, by = join_by("movieId")) %>% 
  left_join(user_effects, by = join_by("userId")) %>%
  filter (userId %in% top_10_users$userId) %>%  
  ggplot(aes( x= clamp(mu + beta_j + alpha_i))) + geom_histogram(binwidth = 0.2) + facet_wrap ( ~ userId,  nrow =2, ncol =5, labeller = label_wrap_gen(width = 18, multi_line = TRUE)) +  labs(x= "Movie Title", y = "Count", title = "Prediction Distribution for Top Users with User & Movie Effects ") + theme_minimal() + xlim(0.5,5.0)



## ----Model 3 - Movie Effects - Movie Distributions , include=TRUE, warning=FALSE, echo = FALSE, out.width="75%", message = FALSE---------------------------------------------------------------------------------

# Create plot to visualise the Distribution of Predictions for Top 10 Movies with the User Effects & Movie Effects Included
  
movie_effects %>% 
  left_join(edx_cv_train_set, by = join_by("movieId")) %>% 
  left_join(user_effects, by = join_by("userId")) %>%
  filter (title %in% top_rated_10$title) %>% 
  ggplot(aes( x= clamp(mu + beta_j + alpha_i))) + geom_histogram(binwidth = 0.2) + facet_wrap ( ~ title,  nrow =2, ncol =5, labeller = label_wrap_gen(width = 18, multi_line = TRUE)) +  labs(x= "Movie Title", y = "Count", title = "Prediction Distribution for Top Movies with User & Movie Effects") + theme_minimal() + xlim(0.5,5.0)



## ----Model 3 - Movie Effects - Demonstrating the need for Regularisation, include=TRUE, warning=FALSE, echo=FALSE,message = FALSE, out.width="75%"---------------------------------------------------------------

# Prepare a list of Top 10 Movies with the Highest Average Rating by Predictions. Extract the Predictions. Prepare a Bar Plot for the same. Format the Bar Plot so that it is easier to Read and Fit on the Screen.

pm3 <- edx_cv_train_set%>% 
        left_join(movie_effects, by='movieId') %>%
        left_join(user_effects, by='userId') %>%
        mutate(pred = clamp(mu + beta_j + alpha_i)) %>% 
        group_by(title) %>% 
        summarize(ratings_received = n(), avg_rating = round(mean(pred),digits=2)) %>% 
        top_n(10) %>% arrange(desc(avg_rating)) %>% 
        ggplot(aes(x=reorder(title,avg_rating),y =avg_rating)) + 
        geom_text(aes(label=avg_rating), hjust = -0.25) +
        labs(x= "title", y = "Predicted Rating", title = "Top Movies by Predicted Rating -  Average Rating") +  geom_col() + coord_flip() + ylim(0,6.5) +  theme_clean() +  theme(plot.title = element_text(hjust = 1))

# Prepare a list of Top 10 Movies with the Highest Average Rating by Predictions. Extract the Ratings for Each Movie. Prepare a Bar Plot of the same. Format the Bar Plot so that it is easier to Read and Fit on the Screen.

pm3a <- edx_cv_train_set %>% 
        left_join(movie_effects, by='movieId') %>%
        left_join(user_effects, by='userId') %>%
        mutate(pred = clamp(mu + beta_j + alpha_i)) %>% 
        group_by(title) %>% 
        summarize(ratings_received = n(), avg_rating = round(mean(pred),digits=2)) %>% 
        top_n(10) %>% arrange(desc(avg_rating)) %>% 
        ggplot(aes(x=reorder(title,avg_rating),y =ratings_received)) + 
        geom_text(aes(label=ratings_received), hjust = -0.25) +
        labs(x= "title", y = "Ratings Received", title = "Top Movies by Predicted Rating - Ratings Received") + geom_col() + coord_flip() + ylim(0,5) + theme_clean() + theme(plot.title = element_text(hjust = 1))

# Present the Plots

grid.arrange(pm3, pm3a, nrow = 2)

# Many thanks to https://stackoverflow.com/questions/46287086/how-to-center-ggplot-plot-title

# Remove Data that is no longer needed
rm(pm3,pm3a)


## ----GC-3, warning = FALSE, echo = FALSE, include= FALSE, message = FALSE----------------------------------------------------------------------------------------------------------------------------------------

# Run Garbage Collector
gc()



## ----Model 4 - Regularisation for Movie Effects, include=TRUE, warning=FALSE, echo=TRUE, message = FALSE, out.width="75%"----------------------------------------------------------------------------------------

# Create a dataframe for computing the new values of Movie Effects using Sum of (rating - mu - alpha_i) and the Number of Ratings (n) for each Movie. 

penalised_least_squares_movie <- edx_cv_train_set %>% 
  left_join(user_effects, by = "userId") %>% 
  group_by(movieId) %>% 
  summarize(sums = sum(rating - mu -alpha_i), n = n())

# Set up a sequence of values for lambda_j to compute the least RMSE. 

lambdas_j <- seq(1, 4, 0.1)

# Write a function to compute the RMSE and store it in a Table. 

rmses_penalised_movie <- sapply(lambdas_j, function (lambda_j){
  
        # Compute the new value for Movie Effects. We will use beta_r (r for regularised) instead of         reusing beta_j to avoid confusion. The value is beta_r = sums/(lambda_j + n). Store the              values in a new dataframe.
  
  beta_r_df <- penalised_least_squares_movie %>% mutate("beta_r" = sums/(n + lambda_j))

        # Prepare Predictions for different values of lambda_j
  
  predicted_ratings_penalised_movie <- edx_cv_test_set%>%          
    left_join(beta_r_df, by='movieId') %>% 
    left_join(user_effects, by='userId') %>%
    mutate(pred = clamp(mu + beta_r + alpha_i)) %>%
    pull(pred)

        # Compare our predictions against the actual ratings in edx_cv_test_set and return the same          from the function
  
  rmse(edx_cv_test_set$rating, predicted_ratings_penalised_movie)
  
})

# Find and record the Minimum RMSE and the corresponding lambda_j value
min_lambda_j <- lambdas_j[which.min(rmses_penalised_movie)]
min_rmse_penalised_movie <- round(min(rmses_penalised_movie), digits = 5)

# Prepare final regularised Predictions with the Minimum computed RMSE and corresponding Lambda. 

beta_r_df_initial <- penalised_least_squares_movie %>% 
                    mutate(beta_r = sums/(n + min_lambda_j)) %>%
                    select(movieId, beta_r)




## ----Model 4 - Regularisation for Movie Effects - Plot of lambda_movie, include=TRUE, warning=FALSE, echo=FALSE, message = FALSE, out.width="50%"----------------------------------------------------------------

# Plot the RMSE against the lambda_j values to understand how the RMSE changes. 

pl4 <- data.frame("lambda_j" = lambdas_j, "RMSE" = rmses_penalised_movie)
pl4 %>% ggplot(aes(x=lambda_j, y=RMSE )) + geom_line() + labs(x= "lambda j", y = "RMSE", title = "Plot of RMSE against lambda_j") + theme_clean()

# Print the Minimum lambda_j and RMSE Values 
print( c(" Min lambda j : ", min_lambda_j), quote = FALSE, justify = "left") 
print( c(" Min RMSE Regularised: ", min_rmse_penalised_movie), quote = FALSE, justify = "left")
print( c(" RMSE User & Movie Effects (R): ", min_rmse_penalised_movie), quote = FALSE, justify = "left")

# Print change in RMSE from Model 3 to present improvement due to Regularisation
print (c("Improvement in RMSE with Regularisation :", round(rmse_user_movie - min_rmse_penalised_movie, 5)), quote = FALSE, justify = "Left")

# Clean up Data that is no longer required. 
rm(lambdas_j, rmses_penalised_movie, pl4)


## ----Model 4 - Regularisation for Movie Effects - check Top Movies by Average of predictions , echo=FALSE, warning=FALSE, include=TRUE, message = FALSE, out.width="75%"-----------------------------------------

# Prepare a list of Top 10 Movies with the Highest Average Rating by Predictions.Use Regularised Values "beta_r" . Prepare a Bar Plot for the same. 

pm4 <- edx_cv_train_set %>% 
        left_join(beta_r_df_initial, by='movieId') %>%
        left_join(user_effects, by='userId') %>%
        mutate(pred = clamp(mu + beta_r + alpha_i)) %>% 
        group_by(title) %>% 
        summarize(ratings_received = n(), avg_rating = round(mean(pred),digits=2)) %>% 
        top_n(10) %>% arrange(desc(avg_rating)) %>% 
        ggplot(aes(x=reorder(title,avg_rating), y =avg_rating)) + 
        geom_text(aes(label=avg_rating), hjust = -0.15) +
        labs(x= "title", y = "Average Rating", title = "Top Movies by Predicted Rating - Average Rating") +
        geom_col() + coord_flip() + ylim(0,6) +  theme_clean() +  theme(plot.title = element_text(hjust = 1))

# Prepare a list of Top 10 Movies with the Highest Average Rating by Predictions. Use Regularised Values "beta_r". Extract the Ratings Received for Each Movie. Prepare a Bar Plot of the same. 

pm4a <- edx_cv_train_set %>% 
        left_join(beta_r_df_initial, by='movieId') %>%
        left_join(user_effects, by='userId') %>%
        mutate(pred = clamp(mu + beta_r + alpha_i)) %>% 
        group_by(title) %>% 
        summarize(ratings_received = n(), avg_rating = round(mean(pred),digits=2)) %>% 
        top_n(10) %>% arrange(desc(avg_rating)) %>% 
        ggplot(aes(x=reorder(title, avg_rating),y =ratings_received)) + 
        geom_text(aes(label=ratings_received), hjust = -0.15) +
        labs(x= "title", y = "ratings_received", title = "Top Movies by Predicted Rating - Ratings Received") + 
        geom_col() + coord_flip() + ylim(0, 40000) + theme_clean() + theme(plot.title = element_text(hjust = 1))

#present plots
grid.arrange(pm4, pm4a, nrow = 2)

# Remove after plots are generated 
rm(pm4,pm4a)


## ----Model 4 - Regularisation for Movie Effects - Manual Adjustment of lambda_j to demonstrate how it affects the top 10 ratings, include=TRUE, warning=FALSE, echo=FALSE, message = FALSE, out.width="75%"------

#Print Current Value of lambda_j
print( c(" Min lambda_j Regularised : ", min_lambda_j), quote = FALSE, justify = "left")
# Create Temp Variable to avoid disturbing Existing Min Lambda Value
min_lambda_j_temp <- min_lambda_j + 1
print( c(" lambda_j Manually Modified : ", min_lambda_j_temp), quote = FALSE, justify = "left") 

#create new dataframe to avoid disturbing existing values
beta_r_df_initial_temp <- penalised_least_squares_movie %>% mutate("beta_temp" = sums/(n + min_lambda_j_temp))

 
# Create new variables to hold the plots for Average Rating and Ratings Received
pm4b <- mutate(edx_cv_train_set, userId = as.factor(userId), movieId = as.factor(movieId)) %>% 
  left_join(beta_r_df_initial_temp, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  mutate(pred = clamp(mu + beta_temp + alpha_i)) %>% 
  group_by(title) %>% 
  summarize(ratings_received = n(), avg_rating = round(mean(pred),digits=2)) %>% 
  top_n(10) %>% arrange(desc(avg_rating)) %>% 
  ggplot(aes(x=reorder(title,avg_rating),y =avg_rating)) + 
  geom_text(aes(label=avg_rating), hjust = -0.15) +
  labs(x= "title", y = "Average Rating", title = "Top Movies by Predicted Rating - Average Rating") + 
  geom_col() + coord_flip() + ylim(0,6) +  theme_clean() +  theme(plot.title = element_text(hjust = 1))

pm4c <- mutate(edx_cv_train_set, userId = as.factor(userId), movieId = as.factor(movieId)) %>% 
  left_join(beta_r_df_initial_temp, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  mutate(pred = clamp(mu + beta_temp + alpha_i)) %>% 
  group_by(title) %>% 
  summarize(ratings_received = n(), avg_rating = round(mean(pred),digits=2)) %>% 
  top_n(10) %>% arrange(desc(avg_rating)) %>% 
  ggplot(aes(x=reorder(title,avg_rating),y =ratings_received)) + 
  geom_text(aes(label=ratings_received), hjust = -0.15) +
  labs(x= "title", y = "ratings_received", title = "Top Movies by Predicted Rating- Ratings Received") + 
  geom_col() + coord_flip() + ylim(0, 35000) + theme_clean() + theme(plot.title = element_text(hjust = 1))

# print the plots
grid.arrange(pm4b, pm4c, nrow = 2)

# Remove after plots are generated 
rm(pm4b, pm4c)


## ----Model 4 - Regularisation for Movie Effects - Manual Adjustment of lambda_j to demonstrate how it affects the top 10 ratings-2, include=TRUE, warning=FALSE, echo=FALSE, message = FALSE, out.width="75%"----

# Sections Separated out due to Plots getting cropped and overlaid during PDF Creation

# Recreate plot for top Movies  in terms of Ratings Received. We will only consider Movies having 100 or more Ratings. The Plot is the extact same as p5a from code chunk "understand_dataset 6". Arranged in two rows with one row empty so that Plots look the same.

pm4d <- edx %>% 
  group_by(title) %>% 
  summarize(ratings_received = n(), avg_rating = round(mean(rating),digits=2)) %>% 
  filter(ratings_received >100) %>% 
  top_n(10) %>% arrange(desc(avg_rating)) %>% 
  ggplot(aes(x=reorder(title,avg_rating),y =avg_rating)) + 
  geom_text(aes(label=avg_rating), hjust = -0.25) +
  labs(x= "title", y = "Average Rating", title = "Top Movies by Average Rating (100+ ratings)") + 
  geom_col() + coord_flip() + ylim(0,6) +  theme_clean() +  theme(plot.title = element_text(hjust = 1))



grid.arrange(pm4d, nrow = 2)

# Remove after plots are generated 
rm(pm4d)




## ----Model 4 - Regularisation for Movie Effects - Manual Adjustment of lambda_j to demonstrate how it affects the top 10 ratings - check RMSE, include=TRUE, warning=FALSE, echo=FALSE, message = FALSE, out.width="75%"----

# Compute Predictions for the manually incremented value of lambda_j

  predicted_ratings_penalised_movie_temp <- edx_cv_test_set %>%          
  left_join(beta_r_df_initial_temp, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  mutate(pred = clamp(mu + beta_temp + alpha_i)) %>% 
    pull(pred)

# Compare our Predictions against the actual ratings in edx_cv_test_set to generate RMSE
  rmse_movie_temp <- round(rmse(edx_cv_test_set$rating, predicted_ratings_penalised_movie_temp),digits = 5)

# Print RMSE 
print( c(" RMSE with manually incremented value of lambda j: ", rmse_movie_temp), quote = FALSE, justify = "left")


# Remove Data that is no longer required

rm(min_lambda_j_temp, beta_r_df_initial_temp, rmse_movie_temp)
rm(penalised_least_squares_movie, predicted_ratings_penalised_movie_temp)



## ----rmse_summary - 4, echo=FALSE, include=TRUE, warning=FALSE, message = FALSE----------------------------------------------------------------------------------------------------------------------------------

# Record and Print the RMSE Summary for Models so far
rmse_consolidated = data.frame("Model" = c("1-Base", "2-User Effects", "3-User & Movie Effects", "4-User & Movie Effects (R)"), "RMSE" = c(rmse_base, rmse_user, rmse_user_movie, min_rmse_penalised_movie))

knitr::kable( (rmse_consolidated), "simple", align = "lc", caption = "RMSE Summary", digits = 5)

print (c("Improvement in RMSE with Regularisation :", round(rmse_user_movie - min_rmse_penalised_movie, 5)), quote = FALSE, justify = "Left")



## ----Model 4 - Regularisation for Movie Effects - Understanding how Movie Effects modify the Rating, warning=FALSE, echo = FALSE, include=TRUE, message = FALSE--------------------------------------------------

# Lookup Manually Programmed for Ease of Programming as it is only used twice. 

# Look up Movie List for Movie Details
knitr::kable(movie_list %>% 
        filter(title == "Fists in the Pocket (I Pugni in tasca) (1965)" ), "simple", align = 'cllcc', caption = "Movie Details")

#Lookup edx_cv_train_set for User Details
knitr::kable((edx_cv_train_set %>% 
        filter(movieId == 40833) %>% 
        select(userId, movieId, title, rating)), "simple", align = "cclc", caption = "User Details")

#Compute Mean Rating for User
knitr::kable(edx_cv_train_set %>% 
        filter(userId == 14598) %>% group_by(userId) %>% 
        summarise(mean_rating = mean(rating)),"simple", align = "cc", caption = "Mean Rating for User")

#Lookup alpha_i value for User
knitr::kable(user_effects %>% 
        filter(userId == 14598),"simple", align = "cc", caption = " alpha_i Value for User")

#Lookup beta_j value for Movie
knitr::kable(movie_effects  %>% 
        filter(movieId == 40833), "simple", align = "cc", caption = "beta_j value for Movie")

#Lookup beta_r value for Movie
knitr::kable(beta_r_df_initial  %>% 
        filter(movieId == 40833) %>% select(movieId, beta_r) , "simple", align = "cc", caption = "beta_r value for Movie")


## ----GC-4, warning = FALSE, echo = FALSE, include= FALSE, message = FALSE----------------------------------------------------------------------------------------------------------------------------------------

# Remove Data that will no longer be required
rm(user_effects, movie_effects)

# Run Garbage Collector
gc()


## ----Model 5a -Regularisation for User Effects & Movie Effects - Combined , include=TRUE, warning=FALSE, echo=TRUE, message = FALSE------------------------------------------------------------------------------

# We will set up a sequence of values for lambda_ij to compute the least RMSE. 
lambdas_ij <- seq(0, 10, 0.1)

# Setup a function to return the RMSE for each value of lambda_ij
rmses_penalised_user_movie  <- sapply(lambdas_ij, function(lambda_ij){
  # Create & update new dataframe to store values of beta_ij 
     penalised_movie_effects <- edx_cv_train_set %>% 
          group_by(movieId) %>%
          summarize(beta_ij = sum(rating - mu)/(n()+lambda_ij))
     
  # Create & update new dataframe to store values of alpha_ij 
     penalised_user_movie_effects <- edx_cv_train_set %>% 
          left_join(penalised_movie_effects, by="movieId") %>%
          group_by(userId) %>%
          summarize(alpha_ij = sum(rating - beta_ij - mu)/(n()+lambda_ij))
     
  # Prepare predictions for Testing/Validation set using the computed beta_ij and alpha_ij
     predicted_ratings_user_movie <- 
          edx_cv_test_set %>% 
          left_join(penalised_movie_effects, by = "movieId") %>%
          left_join(penalised_user_movie_effects, by = "userId") %>%
          mutate(pred = clamp( mu + alpha_ij + beta_ij)) %>%
          pull(pred)
     
  # Compute and return RMSE  
     return(rmse(predicted_ratings_user_movie, edx_cv_test_set$rating))
})

# We will find and record the Minimum RMSE and the corresponding lambda_ij value
min_lambda_ij <- lambdas_ij[which.min(rmses_penalised_user_movie )]
min_rmse_penalised_user_movie <- round(min(rmses_penalised_user_movie ),digits = 5)

# Compute final values for alpha_ij and beta_ij for later reference

beta_r_df_combined <- edx_cv_train_set %>% 
                      group_by(movieId) %>%
                      summarize(beta_ij = sum(rating - mu)/(n() + min_lambda_ij)) %>%
                      select(movieId, beta_ij) 

alpha_r_df_combined <- edx_cv_train_set %>% 
                      left_join(beta_r_df_combined, by="movieId") %>%
                      group_by(userId) %>%
                      summarize(alpha_ij = sum(rating - beta_ij - mu)/(n() + min_lambda_ij)) %>%
                      select(userId, alpha_ij)



## ----RMSE Summary 5a - User Effects Regularisation, echo=FALSE, include=TRUE, warning=FALSE, message = FALSE, out.width="50%"------------------------------------------------------------------------------------

# Let us plot the RMSE against the lambda_i values to understand how the RMSE changes. 
pl5a <- data.frame("lambda_ij" = lambdas_ij, "RMSE" = rmses_penalised_user_movie)
pl5a %>% ggplot(aes(x=lambda_ij, y=RMSE )) + geom_line() + labs(x= "lambda_ij", y = "RMSE", title = "Plot of RMSE against lambda_ij") + theme_clean()

# We will print the Minimum RMSE and the corresponding lambda_i value
print( c("Min lambda_ij : ", min_lambda_ij), quote = FALSE, justify = "left") 
print( c("RMSE User Effects (R): ", min_rmse_penalised_user_movie), quote = FALSE, justify = "left")

rmse_consolidated = data.frame("Model" = c("1-Base", "2-User Effects", "3-User & Movie Effects", "4-User & Movie Effects (R)", "5a-User Effects (R) & Movie Effects (R)"), "RMSE" = c(rmse_base, rmse_user, rmse_user_movie, min_rmse_penalised_movie, min_rmse_penalised_user_movie))

knitr::kable( (rmse_consolidated), "simple", align = "lc", caption = "RMSE Summary", digits = 5 )

print (c("Improvement in RMSE with User Regularisation over User Effects :", round( rmse_user- min_rmse_penalised_user_movie, 6)), quote = FALSE, justify = "Left")

print (c("Improvement in RMSE with User Regularisation over User and Movie Effects (R):", round( min_rmse_penalised_movie - min_rmse_penalised_user_movie, 6)), quote = FALSE, justify = "Left")

# Clean up Data that is no longer required. 

rm(penalised_least_squares_user, predicted_ratings_penalised_user, lambdas_i, rmses_penalised_user, pl5a)


## ----Model 5a - Regularised User & Movie Effects - Check Top Movies by Average predictions, include=TRUE, warning=FALSE, echo=FALSE, message = FALSE, out.width="75%"--------------------------------------------

# Prepare a list of Top 10 Movies with the Highest Average Rating by Predictions.Use Regularised Values "alpha_ij" and "beta_ij" . Prepare a Bar Plot for the same. 

pm5a <- edx_cv_train_set %>% 
      left_join(beta_r_df_combined, by='movieId') %>%
      left_join(alpha_r_df_combined, by='userId') %>%
      mutate(pred = clamp(mu + beta_ij + alpha_ij)) %>% 
      group_by(title) %>% 
      summarize(ratings_received = n(), avg_rating = round(mean(pred),digits=2)) %>% 
      top_n(10) %>% arrange(desc(avg_rating)) %>% 
      ggplot(aes(x=reorder(title,avg_rating),y =avg_rating)) + 
      geom_text(aes(label=avg_rating), hjust = -0.15) +
    labs(x= "title", y = "Average Rating", title = "Top Movies by Predicted Rating - Average Rating") + 
    geom_col() + coord_flip() + ylim(0,6) +  theme_clean() +  theme(plot.title = element_text(hjust = 1))

# Prepare a list of Top 10 Movies with the Highest Average Rating by Predictions.Use Regularised Values "alpha_ij" and "beta_ij". Extract the Ratings for Each Movie. Prepare a Bar Plot of the same. 

pm5a1 <- edx_cv_train_set %>% 
      left_join(beta_r_df_combined, by='movieId') %>%
      left_join(alpha_r_df_combined, by='userId') %>%
      mutate(pred = clamp(mu + beta_ij + alpha_ij)) %>% 
      group_by(title) %>% 
      summarize(ratings_received = n(), avg_rating = round(mean(pred),digits=2)) %>% 
      top_n(10) %>% arrange(desc(avg_rating)) %>% 
      ggplot(aes(x=reorder(title,avg_rating),y =ratings_received)) + 
      geom_text(aes(label=ratings_received), hjust = -0.15) +
      labs(x= "title", y = "ratings_received", title = "Top Movies by Predicted Rating - Ratings Received") + 
      geom_col() + coord_flip() + ylim(0, 40000) + theme_clean() + theme(plot.title = element_text(hjust = 1))

# Present Plots
grid.arrange(pm5a, pm5a1, nrow = 2)

# Remove after plots are generated 
rm(pm5a,pm5a1)



## ----Model 5a - Understanding User Effects & Movie Feffects Regularisation on ratings - Outliers, warning=FALSE, echo = FALSE, include=TRUE, message = FALSE-----------------------------------------------------

#Lookup edx_cv_train_set for User Details
knitr::kable((edx_cv_train_set %>% filter(movieId == 40833) %>% select(userId, movieId, title, rating)), "simple", align = "cclc", caption = "User Details")

#Lookup alpha_ij value for User
knitr::kable(alpha_r_df_combined %>% filter(userId == 14598) %>% select(userId, alpha_ij),"simple", align = "cc", caption = "alpha_ij for User")

#Lookup beta_ij value for Movie
knitr::kable(beta_r_df_combined %>% filter(movieId == 40833) %>% select(movieId, beta_ij), "simple", align = "cc", caption = "beta_ij value for Movie")



## ----GC-5a, warning=FALSE, echo = FALSE, include=FALSE, message = FALSE------------------------------------------------------------------------------------------------------------------------------------------

gc()

rm( lambdas_ij, rmses_penalised_user_movie)



## ----Model 6 - Understand How Genre influence user ratings, include=TRUE, warning=FALSE, echo = TRUE, message = FALSE--------------------------------------------------------------------------------------------

# There is probably a much better way to do this than what has been done. An exercise for the Reader who has more expertise in R. 
# Thanks to the authors of the Articles below for providing the right guidance
# https://stackoverflow.com/questions/38708529/how-to-use-str-split-in-r#38708610 
# https://stringr.tidyverse.org/reference/str_detect.html


# Create a list of genre, used whenever we need to address the genre collectively. Genre for Sci-Fi and Film-Noir have been changed to Sci_Fi and Film_Noir respectively. This is done as R otherwise interprets the Symbol for the Hyphen (-) as the Minus/Subtraction Operator

genre_list <- c( "Action", 	"Adventure", 	"Animation", 	"Children", 	"Comedy", 	"Crime", 	"Documentary", 	"Drama", 	"Fantasy", 	"Film_Noir", 	"Mystery", 	"Horror", 	"Thriller", 	"Musical", 	"Sci_Fi", 	"Romance", 	"War", 	"Western" )

# String Detection and Separation is a computationally expensive, single threaded operation and takes a long time, we will retrieve the file from disk if it exists.  

genre_assignment_file <- "genre_assignment.RData"

if(file.exists(genre_assignment_file)){
      load(genre_assignment_file)
      
      genre_assignment <- genre_assignment %>% 
      mutate(userId = as.factor(userId), movieId = as.factor(movieId))
      
      rm(genre_assignment_file)
      
} else {
  

# Genre are all combined together in the "genres" column in a single string. Individual Genres within the string are separated using the pipe (|) symbol.
  
# Split genre individually for further Analysis. Use the "stringr" library and str_split function. 
  
# The pipe (|) symbol happens to have its own meaning in Regular Expressions so must be escaped using a double backslash.

# Assign genre individually to each movie based on the Split Strings. Each genre is tracked individually in its own column. Assign 1 if genre matches, 0 if it does not. Based on the code below, the "mutate" function from the "Tidyverse" Package will automatically create a Column if it does not exist already. If the Column exists, it will just update the corresponding row. Please note that the Column names for Genres Sci-Fi and Film-Noir have been changed to Sci_Fi and Film_Noir respectively. This is done so that it is easier to address the columns as R otherwise interprets the Symbol for the Hyphen (-) as the Minus/Subtraction Operator. 

# Add a genre "count" column for counting & tracking the number of genre attached to each movie, this will help us normalise the genre treatment effects values later. 


genre_assignment <-  edx_cv_train_set %>% 
  mutate("individual_genre" = str_split(genres, "\\|")) %>% 
  mutate (Action  =  ifelse (str_detect ( individual_genre, "Action"), 1 , 0 ) )   %>%
  mutate (Adventure  =  ifelse (str_detect ( individual_genre, "Adventure") , 1 , 0 ) )  %>%
  mutate (Animation  =  ifelse (str_detect ( individual_genre, "Animation") , 1 , 0 ) )  %>%
  mutate (Children  =  ifelse (str_detect ( individual_genre, "Children") , 1 , 0 ) )  %>%
  mutate (Comedy  =  ifelse (str_detect ( individual_genre, "Comedy") , 1 , 0 ) )  %>%
  mutate (Crime  =  ifelse (str_detect ( individual_genre, "Crime") , 1 , 0 ) )  %>%
  mutate (Documentary  =  ifelse (str_detect ( individual_genre, "Documentary") , 1 , 0 ) )  %>%
  mutate (Drama  =  ifelse (str_detect ( individual_genre, "Drama") , 1 , 0 ) )  %>%
  mutate (Fantasy  =  ifelse (str_detect ( individual_genre, "Fantasy") , 1 , 0 ) )  %>%
  mutate (Film_Noir  =  ifelse (str_detect ( individual_genre, "Film-Noir") , 1 , 0 ) )  %>%
  mutate (Mystery  =  ifelse (str_detect ( individual_genre, "Mystery") , 1 , 0 ) )  %>%
  mutate (Horror  =  ifelse (str_detect ( individual_genre, "Horror") , 1 , 0 ) )  %>%
  mutate (Thriller  =  ifelse (str_detect ( individual_genre, "Thriller") , 1 , 0 ) )  %>%
  mutate (Musical  =  ifelse (str_detect ( individual_genre, "Musical") , 1 , 0 ) )  %>%
  mutate (Sci_Fi  =  ifelse (str_detect ( individual_genre, "Sci-Fi") , 1 , 0 ) )  %>%
  mutate (Romance  =  ifelse (str_detect ( individual_genre, "Romance") , 1 , 0 ) )  %>%
  mutate (War  =  ifelse (str_detect ( individual_genre, "War") , 1 , 0 ) )  %>%
  mutate (Western  =  ifelse (str_detect ( individual_genre, "Western") , 1 , 0 ) ) %>%
  mutate("genre_count" = Action + Adventure + Animation + Children + Comedy + Crime + Documentary + Drama + Fantasy + Film_Noir + Mystery + Horror + Thriller + Musical + Sci_Fi + Romance + War + Western )

# Save file in R's native Format as it offers excellent compression. CSV is almost 4 times larger and the number of rows are more than most spreadsheet programs can safely handle, so saving it in CSV format is of limited utility.

save(genre_assignment, file = "genre_assignment.RData")
  
}
rm(genre_assignment_test_file) # Remove variable used to hold file name

# Let us modify our edx_cv_test_set beforehand to test our Predictions.

# We will repeat the Operations for our edx_cv_test_set. We will import it from the Disk if the file exists as it a computationally expensive operation and takes some time. 

genre_assignment_test_file <- "genre_assignment_test.RData"

if(file.exists(genre_assignment_test_file)){
      load(genre_assignment_test_file)
      genre_assignment_test <- genre_assignment_test %>% 
      mutate(userId = as.factor(userId), movieId = as.factor(movieId))
    rm(genre_assignment_test_file)
} else {

# Split consolidated genre Field into individual genre 

# Create Genre Column and Populate the individual Genre Column with a 1 if the Genre is applicable for the movie or a 0 otherwise. 

# Add a Count column to track number of genre attached to the movie

genre_assignment_test <- edx_cv_test_set %>% 
  mutate("individual_genre" = str_split(genres, "\\|")) %>% 
  mutate (Action  =  ifelse (str_detect ( individual_genre, "Action"), 1 , 0 ) )   %>%
  mutate (Adventure  =  ifelse (str_detect ( individual_genre, "Adventure") , 1 , 0 ) )  %>%
  mutate (Animation  =  ifelse (str_detect ( individual_genre, "Animation") , 1 , 0 ) )  %>%
  mutate (Children  =  ifelse (str_detect ( individual_genre, "Children") , 1 , 0 ) )  %>%
  mutate (Comedy  =  ifelse (str_detect ( individual_genre, "Comedy") , 1 , 0 ) )  %>%
  mutate (Crime  =  ifelse (str_detect ( individual_genre, "Crime") , 1 , 0 ) )  %>%
  mutate (Documentary  =  ifelse (str_detect ( individual_genre, "Documentary") , 1 , 0 ) )  %>%
  mutate (Drama  =  ifelse (str_detect ( individual_genre, "Drama") , 1 , 0 ) )  %>%
  mutate (Fantasy  =  ifelse (str_detect ( individual_genre, "Fantasy") , 1 , 0 ) )  %>%
  mutate (Film_Noir  =  ifelse (str_detect ( individual_genre, "Film-Noir") , 1 , 0 ) )  %>%
  mutate (Mystery  =  ifelse (str_detect ( individual_genre, "Mystery") , 1 , 0 ) )  %>%
  mutate (Horror  =  ifelse (str_detect ( individual_genre, "Horror") , 1 , 0 ) )  %>%
  mutate (Thriller  =  ifelse (str_detect ( individual_genre, "Thriller") , 1 , 0 ) )  %>%
  mutate (Musical  =  ifelse (str_detect ( individual_genre, "Musical") , 1 , 0 ) )  %>%
  mutate (Sci_Fi  =  ifelse (str_detect ( individual_genre, "Sci-Fi") , 1 , 0 ) )  %>%
  mutate (Romance  =  ifelse (str_detect ( individual_genre, "Romance") , 1 , 0 ) )  %>%
  mutate (War  =  ifelse (str_detect ( individual_genre, "War") , 1 , 0 ) )  %>%
  mutate (Western  =  ifelse (str_detect ( individual_genre, "Western") , 1 , 0 ) ) %>%
  mutate("genre_count" = Action + Adventure + Animation + Children + Comedy + Crime + Documentary + Drama + Fantasy + Film_Noir + Mystery + Horror + Thriller + Musical + Sci_Fi + Romance + War + Western )

# Save file to disk
save(genre_assignment_test, file="genre_assignment_test.RData")
}


rm(genre_assignment_file, genre_assignment_test_file)  # Remove variables used to hold file name


## ----Model 6 - Create Sample Matrix for Reference , include=TRUE, echo=TRUE, warning=FALSE, message = FALSE------------------------------------------------------------------------------------------------------

# Create a new dataframe that will be populated with the Genre Specific Ratings going forward. We will use the dataframe created in the earlier section for identifying the Genre associated with each movie. The new dataframe only retains numerical values for ease of operations, speed up computation and reduce memory usage by retaining only necessary fields.

genre_assignment_matrix <- genre_assignment %>% 
            select (userId, movieId, rating, all_of(genre_list), genre_count, timestamp) 

# Replicate the User rating for each movie to the individual Genre Columns as applicable. Use a temporary dataframe to store the results
temp <- genre_assignment_matrix %>% ungroup %>% select(rating, all_of(genre_list)) %>%
            mutate (  Action = ifelse ( Action == 1 , rating , 0 )  )  %>%
            mutate (  Adventure = ifelse ( Adventure == 1 , rating , 0 )  )  %>%
            mutate (  Animation = ifelse ( Animation == 1 , rating , 0 )  )  %>%
            mutate (  Children = ifelse ( Children == 1 , rating , 0 )  )  %>%
            mutate (  Comedy = ifelse ( Comedy == 1 , rating , 0 )  )  %>%
            mutate (  Crime = ifelse ( Crime == 1 , rating , 0 )  )  %>%
            mutate (  Documentary = ifelse ( Documentary == 1 , rating , 0 )  )  %>%
            mutate (  Drama = ifelse ( Drama == 1 , rating , 0 )  )  %>%
            mutate (  Fantasy = ifelse ( Fantasy == 1 , rating , 0 )  )  %>%
            mutate (  Film_Noir = ifelse ( Film_Noir == 1 , rating , 0 )  )  %>%
            mutate (  Mystery = ifelse ( Mystery == 1 , rating , 0 )  )  %>%
            mutate (  Horror = ifelse ( Horror == 1 , rating , 0 )  )  %>%
            mutate (  Thriller = ifelse ( Thriller == 1 , rating , 0 )  )  %>%
            mutate (  Musical = ifelse ( Musical == 1 , rating , 0 )  )  %>%
            mutate (  Sci_Fi = ifelse ( Sci_Fi == 1 , rating , 0 )  )  %>%
            mutate (  Romance = ifelse ( Romance == 1 , rating , 0 )  )  %>%
            mutate (  War = ifelse ( War == 1 , rating , 0 )  )  %>%
            mutate (  Western = ifelse ( Western == 1 , rating , 0 )) %>% 
            select ( all_of(genre_list))

# Compute the Genre Mean by manually calculating the Individual Genre Sums and dividing them by the number of ratings. Using "colMeans" will provide erroneous results as it will also consider rows where no rating has been assigned. So this computation needs to be done manually.  

individual_genre_sums <- colSums(temp) # compute sum of ratings for each column
individual_genre_sums <- round(individual_genre_sums, digits = 0) # Round Sum to remove decimal values
individual_genre_count <- colSums(temp != 0) # for each column count rows where a rating is available 
# compute & print Overall Genre Mean & Overall Mean for comparison

mu_g <- round(sum(individual_genre_sums)/sum(individual_genre_count),digits = 5)

print(c("Overall Genre Mean (mu_g): ", round(mu_g, digits = 5)), quote = FALSE, justify = "Left")
print(c("Overall Mean (mu): ", round(mu, digits = 5)), quote = FALSE, justify = "Left")
print(c("Difference (mu_g - mu): ", round(mu_g -mu, digits = 5)), quote = FALSE, justify = "Left")

# compute Individual Genre Mean and Difference from Overall Genre Mean and Store in a common Table, 

genre_summary <- data.frame(individual_genre_sums,individual_genre_count) 
genre_summary <- genre_summary %>% mutate(individual_genre_mean = (individual_genre_sums/individual_genre_count), difference_from_genre_mean = (individual_genre_mean - mu_g), genre_significance = round(difference_from_genre_mean*individual_genre_count, digits = 0))



# Remove Temp Dataset from Memory
rm(temp)

# Remove Data that is no longer required
rm(individual_genre_count, individual_genre_sums)




## ----Model 6 - Print Genre Summary , include=TRUE, echo=FALSE, warning=FALSE, message = FALSE--------------------------------------------------------------------------------------------------------------------

# Deliberately broken up into a separate chunk to avoid cluttering PDF output 

# Print Table               
knitr::kable(genre_summary, "simple", align = "c", caption = "Genre Summary", col.names = c("Genre Name", "Genre Sum", "Genre Count", "Genre Mean", "Difference from Overall Genre Mean", " Genre Significance "), digits = 5) %>% kable_styling(font_size = 8)

# Print Sorted Values of Genre Count. Arrange in Descending Order for ease of Visualisation
knitr::kable(genre_summary  %>% select(individual_genre_count) %>% arrange(desc(individual_genre_count)),"simple", align = "c", caption = "Genre Summary Sorted by Count ", col.names = c("Genre Name", "Count"), digits = 5) %>% kable_styling(font_size = 8)

# Print Sorted Values of Difference from Genre Mean
knitr::kable(genre_summary  %>% select(difference_from_genre_mean) %>% arrange(desc(difference_from_genre_mean)),"simple", align = "c", caption = "Genre Summary Sorted by Difference from the Genre Mean ", col.names = c("Genre Name", "Difference from Overall Genre Mean"), digits = 5) %>% kable_styling(font_size = 8)

# Print Values of Genre Significance
knitr::kable(genre_summary  %>% select(genre_significance) %>% arrange(desc(genre_significance)),"simple", align = "c", caption = "Genre Significance - Product of Genre Count & Difference from Genre Mean ", col.names = c("Genre Name", "Genre Significance"), digits = 0) %>% kable_styling(font_size = 8)



## ----GC-6,  warning=FALSE, echo = FALSE, include=FALSE, message = FALSE------------------------------------------------------------------------------------------------------------------------------------------

gc()



## ----Model 6a - choose movie to illustrate genre effects---------------------------------------------------------------------------------------------------------------------------------------------------------

knitr::kable(movie_list %>% filter(title == "Toy Story (1995)"), "simple", align = "cllcc", caption = "Genre attached to Movie - Toy Story (1995) ")



## ----Model 6a - Build Genre Effects for each User for every Genre, include=TRUE, echo=TRUE,warning=FALSE, message = FALSE----------------------------------------------------------------------------------------

# For each user compute their average rating for every genre. We then compute the difference between a User's Average Rating across all genres against the genre under analysis. If for some reason, the user has not rated even a single movie belonging to a particular genre, then ensure that genre specific ratings are not considered for such cases. The "ifelse" statements in the denominators are necessary to avoid divide by 0 problems. 

# Code can be built using a single "summarise" statement. However, it is deliberately broken into each genre to avoid too much complexity and to keep it modular and clear. It also helps avoid lookup errors due to typos as each genre has its own statement and cannot lookup values from another genre erroneously.

# Use "b" rather than "beta" as prefix to genre names to keep Variable names more concise and easier to work with

genre_action <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_action" = sum(Action == 1), "sum_action" = sum((ifelse(Action==0,0,rating)))  , "mean_action" = sum_action/(ifelse(count_action==0,1,count_action)), "b_action"  = ifelse(sum_action==0,0,(mean_action - mean_rating ))) %>% select(userId, mean_action, b_action)

genre_adventure <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_adventure" = sum(Adventure == 1), "sum_adventure" = sum((ifelse(Adventure==0,0,rating)))  , "mean_adventure" = sum_adventure/(ifelse(count_adventure==0,1,count_adventure)), "b_adventure"  = ifelse(sum_adventure==0,0,(mean_adventure - mean_rating ))) %>% select(userId, mean_adventure, b_adventure)

genre_animation <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_animation" = sum(Animation == 1), "sum_animation" = sum((ifelse(Animation==0,0,rating)))  , "mean_animation" = sum_animation/(ifelse(count_animation==0,1,count_animation)), "b_animation"  = ifelse(sum_animation==0,0,(mean_animation - mean_rating  ))) %>% select(userId, mean_animation, b_animation)

genre_children <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_children" = sum(Children == 1), "sum_children" = sum((ifelse(Children==0,0,rating)))  , "mean_children" = sum_children/(ifelse(count_children==0,1,count_children)), "b_children"  = ifelse(sum_children==0,0,(mean_children - mean_rating ))) %>% select(userId, mean_children, b_children)

genre_comedy <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_comedy" = sum(Comedy == 1), "sum_comedy" = sum((ifelse(Comedy==0,0,rating)))  , "mean_comedy" = sum_comedy/(ifelse(count_comedy==0,1,count_comedy)), "b_comedy"  = ifelse(sum_comedy==0,0,(mean_comedy - mean_rating ))) %>% select(userId, mean_comedy, b_comedy)

genre_crime <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_crime" = sum(Crime == 1), "sum_crime" = sum((ifelse(Crime==0,0,rating)))  , "mean_crime" = sum_crime/(ifelse(count_crime==0,1,count_crime)), "b_crime"  = ifelse(sum_crime==0,0,(mean_crime - mean_rating  ))) %>% select(userId, mean_crime, b_crime)

genre_documentary <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating) , "count_documentary" = sum(Documentary == 1), "sum_documentary" = sum((ifelse(Documentary==0,0,rating)))  , "mean_documentary" = sum_documentary/(ifelse(count_documentary==0,1,count_documentary)), "b_documentary"  = ifelse(sum_documentary==0,0,(mean_documentary - mean_rating  ))) %>% select(userId, mean_documentary, b_documentary)

genre_drama <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating), "count_drama" = sum(Drama == 1), "sum_drama" = sum((ifelse(Drama==0,0,rating)))  , "mean_drama" = sum_drama/(ifelse(count_drama==0,1,count_drama)), "b_drama"  = ifelse(sum_drama==0,0,(mean_drama - mean_rating ))) %>% select(userId, mean_drama, b_drama)

genre_fantasy <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_fantasy" = sum(Fantasy == 1), "sum_fantasy" = sum((ifelse(Fantasy==0,0,rating)))  , "mean_fantasy" = sum_fantasy/(ifelse(count_fantasy==0,1,count_fantasy)), "b_fantasy"  = ifelse(sum_fantasy==0,0,(mean_fantasy - mean_rating ))) %>% select(userId, mean_fantasy, b_fantasy)

genre_film_noir <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_film_noir" = sum(Film_Noir == 1), "sum_film_noir" = sum((ifelse(Film_Noir==0,0,rating)))  , "mean_film_noir" = sum_film_noir/(ifelse(count_film_noir==0,1,count_film_noir)), "b_film_noir"  = ifelse(sum_film_noir==0,0,(mean_film_noir - mean_rating ))) %>% select(userId, mean_film_noir, b_film_noir)

genre_mystery <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating) , "count_mystery" = sum(Mystery == 1), "sum_mystery" = sum((ifelse(Mystery==0,0,rating)))  , "mean_mystery" = sum_mystery/(ifelse(count_mystery==0,1,count_mystery)), "b_mystery"  = ifelse(sum_mystery==0,0,(mean_mystery - mean_rating  ))) %>% select(userId, mean_mystery, b_mystery)

genre_horror <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating) , "count_horror" = sum(Horror == 1), "sum_horror" = sum((ifelse(Horror==0,0,rating)))  , "mean_horror" = sum_horror/(ifelse(count_horror==0,1,count_horror)), "b_horror"  = ifelse(sum_horror==0,0,(mean_horror - mean_rating ))) %>% select(userId, mean_horror, b_horror)

genre_thriller <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_thriller" = sum(Thriller == 1), "sum_thriller" = sum((ifelse(Thriller==0,0,rating)))  , "mean_thriller" = sum_thriller/(ifelse(count_thriller==0,1,count_thriller)), "b_thriller"  = ifelse(sum_thriller==0,0,(mean_thriller - mean_rating ))) %>% select(userId, mean_thriller, b_thriller)

genre_musical <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_musical" = sum(Musical == 1), "sum_musical" = sum((ifelse(Musical==0,0,rating)))  , "mean_musical" = sum_musical/(ifelse(count_musical==0,1,count_musical)), "b_musical"  = ifelse(sum_musical==0,0,(mean_musical - mean_rating ))) %>% select(userId, mean_musical, b_musical)

genre_sci_fi <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_sci_fi" = sum(Sci_Fi == 1), "sum_sci_fi" = sum((ifelse(Sci_Fi==0,0,rating)))  , "mean_sci_fi" = sum_sci_fi/(ifelse(count_sci_fi==0,1,count_sci_fi)), "b_sci_fi"  = ifelse(sum_sci_fi==0,0,(mean_sci_fi - mean_rating   ))) %>% select(userId, mean_sci_fi, b_sci_fi)

genre_romance <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_romance" = sum(Romance == 1), "sum_romance" = sum((ifelse(Romance==0,0,rating)))  , "mean_romance" = sum_romance/(ifelse(count_romance==0,1,count_romance)), "b_romance"  = ifelse(sum_romance==0,0,(mean_romance - mean_rating  ))) %>% select(userId, mean_romance, b_romance)

genre_war <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating) , "count_war" = sum(War == 1), "sum_war" = sum((ifelse(War==0,0,rating)))  , "mean_war" = sum_war/(ifelse(count_war==0,1,count_war)), "b_war"  = ifelse(sum_war==0,0,(mean_war - mean_rating ))) %>% select(userId, mean_war, b_war)

genre_western <- genre_assignment_matrix %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_western" = sum(Western == 1), "sum_western" = sum((ifelse(Western==0,0,rating)))  , "mean_western" = sum_western/(ifelse(count_western==0,1,count_western)), "b_western"  = ifelse(sum_western==0,0,(mean_western - mean_rating   ))) %>% select(userId, mean_western, b_western)

# Create a Common dataframe for all genre coefficients to reduce repetitions and errors later. Order is slightly different because it is as per Genre Significance. Should not make any difference to the end result.

genre_user_summary <- genre_drama %>% 
  left_join(genre_comedy, by='userId') %>%
  left_join(genre_action, by='userId') %>%
  left_join(genre_thriller, by='userId') %>%
  left_join(genre_adventure, by='userId') %>%
  left_join(genre_romance, by='userId') %>%
  left_join(genre_sci_fi, by='userId') %>%
  left_join(genre_crime, by='userId') %>%
  left_join(genre_fantasy, by='userId') %>%
  left_join(genre_children, by='userId') %>%
  left_join(genre_horror, by='userId') %>%
  left_join(genre_mystery, by='userId') %>%
  left_join(genre_war, by='userId') %>%
  left_join(genre_animation, by='userId') %>%
  left_join(genre_musical, by='userId') %>%
  left_join(genre_western, by='userId') %>%
  left_join(genre_film_noir, by='userId') %>%
  left_join(genre_documentary, by='userId') 

# Extract all genre coefficients values for all Users
genre_effects_user <- genre_user_summary %>% select(userId, b_action, b_adventure, b_animation, b_children, b_comedy, b_crime, b_documentary, b_drama, b_fantasy, b_film_noir, b_horror, b_musical, b_mystery, b_romance, b_sci_fi, b_thriller, b_war, b_western)

# Extract all genre_mean values for all Users. We will use them later
genre_user_mean <- genre_user_summary %>% select(userId, mean_action, mean_adventure, mean_animation, mean_children, mean_comedy, mean_crime, mean_documentary, mean_drama, mean_fantasy, mean_film_noir, mean_horror, mean_musical, mean_mystery, mean_romance, mean_sci_fi, mean_thriller, mean_war, mean_western)

# Rmeove Individual Genre dataframes to save memory and avoid confusion
rm(genre_action,genre_adventure,genre_animation, genre_children, genre_comedy, genre_crime, genre_documentary, genre_drama, genre_fantasy, genre_film_noir, genre_horror, genre_musical, genre_mystery, genre_romance, genre_sci_fi, genre_thriller, genre_war, genre_western)



## ----Model 6a - Genre Effects User - Test Predictions, include=TRUE, echo=TRUE, warning=FALSE, message = FALSE---------------------------------------------------------------------------------------------------


# Compute the Predicted Ratings taking into account the Regularised User Effects, Regularised Movie Effects, Genre Effects Overall and Genre User Effects

# The beta_ig values are computed based on the combination of User and Movie and the entries are unique, hence entries in the genre_assignment_matrix will not match with any entries in the edx_cv_test_set or genre_assigment_test set, hence beta_ig will need to be computed everytime for the genre_assigment_test set for cross validation.

predicted_ratings_genre_user <- genre_assignment_test %>% 
  left_join(alpha_r_df_combined, by='userId') %>%
  left_join(beta_r_df_combined, by='movieId') %>%
  left_join(genre_effects_user, by = 'userId') %>%
  mutate(beta_ig = (b_drama*Drama + b_comedy*Comedy + b_action*Action + b_thriller*Thriller + b_adventure*Adventure + b_romance*Romance + b_sci_fi*Sci_Fi + b_crime*Crime + b_fantasy*Fantasy + b_children*Children + b_horror*Horror + b_mystery*Mystery + b_war*War + b_animation*Animation + b_musical*Musical + b_western*Western + b_film_noir*Film_Noir + b_documentary*Documentary)/(ifelse(genre_count==0,1,  genre_count  ))) %>% 
      mutate("pred" = clamp(mu + alpha_ij +  beta_ij + beta_ig  ))  %>%   pull(pred)

# Record and Store RMSE with Genre User Effects included
rmse_genre_effects_user <- round(rmse(edx_cv_test_set$rating, predicted_ratings_genre_user),digits = 5)

# Remove Data that is no longer required
rm(predicted_ratings_genre_user)



# Print RMSE with Genre User Effects Included
print( c(" RMSE User, Movie & Genre Effects User: ", rmse_genre_effects_user), quote = FALSE, justify = "left")



## ----Model 6a - Check Top 10 List after incorporating Genre User Effects, include=TRUE, warning=FALSE, echo=FALSE, message = FALSE, out.width="75%"--------------------------------------------------------------

# Be Careful not to load the whole dataframe into GGPlot by error, it will cause a Memory Overload. Choose only the first few Entries after Sorting. Use the top_n() command with the required number of Entries

# Check our Top 10 List of Movies after including the Genre User Effect
sm6a <- genre_assignment %>%
  left_join(alpha_r_df_combined, by='userId') %>%
  left_join(beta_r_df_combined, by='movieId') %>%
  left_join(genre_effects_user, by = 'userId') %>%
    mutate(beta_ig = (b_drama*Drama + b_comedy*Comedy + b_action*Action + b_thriller*Thriller + b_adventure*Adventure + b_romance*Romance + b_sci_fi*Sci_Fi + b_crime*Crime + b_fantasy*Fantasy + b_children*Children + b_horror*Horror + b_mystery*Mystery + b_war*War + b_animation*Animation + b_musical*Musical + b_western*Western + b_film_noir*Film_Noir + b_documentary*Documentary)/(ifelse(genre_count==0,1,genre_count))) %>% 
    mutate("pred" = clamp(mu + alpha_ij +  beta_ij + beta_ig  )) %>%
    group_by(title) %>% 
    summarize(ratings_received = n(), avg_rating = round(mean(pred),digits=2)) %>% 
    top_n(10) %>% arrange(desc(avg_rating))
  
pm6a1 <- sm6a %>%  ggplot(aes(x=reorder(title,avg_rating),y =avg_rating)) + 
  geom_text(aes(label=avg_rating), hjust = -0.15) +
  labs(x= "title", y = "Average Rating", title = "Top Movies by Predicted Rating - Average Rating") + 
  geom_col() + coord_flip() + ylim(0,6) +  theme_clean() +  theme(plot.title = element_text(hjust = 1))

pm6a2 <- sm6a %>%  ggplot(aes(x=reorder(title,avg_rating),y =ratings_received)) + 
  geom_text(aes(label=ratings_received), hjust = -0.15) +
  labs(x= "title", y = "ratings_received", title = "Top Movies by Predicted Rating - Ratings Received") + 
  geom_col() + coord_flip() + ylim(0, 35000) + theme_clean() + theme(plot.title = element_text(hjust = 1))

grid.arrange(pm6a1, pm6a2, nrow = 2)

rm(sm6a, pm6a1, pm6a2)


## ----GC-6a1,  warning=FALSE, echo = FALSE, include=FALSE, message = FALSE----------------------------------------------------------------------------------------------------------------------------------------

gc()



## ----Model 6a - Regularisation for Genre User Effects, message=FALSE, echo=TRUE, warning=FALSE, include=TRUE, out.width="50%"------------------------------------------------------------------------------------

# Let use first compute the Regularisation for Genre User Effects.

# Set up a sequence of Lambdas. Sequence Values chosen to avoid unnecessary computations. If required, Reader can check how the RMSE varies by changing the Sequence and Increments

lambdas_ig <- seq(0, 3, 0.05)

# Set up a function to calculate the RMSE for each value of lambdas_ig

rmses_genre_function_user <- function(lambda_ig) {

# Check our Predictions for each value of lambdas_ig
  predicted_ratings_genre_user <- mutate(genre_assignment_test, userId = as.factor(userId), movieId = as.factor(movieId)) %>%
  left_join(alpha_r_df_combined, by='userId') %>%
  left_join(beta_r_df_combined, by='movieId') %>%
  left_join(genre_effects_user, by = 'userId') %>%
    mutate(beta_ig = (b_drama*Drama + b_comedy*Comedy + b_action*Action + b_thriller*Thriller + b_adventure*Adventure + b_romance*Romance + b_sci_fi*Sci_Fi + b_crime*Crime + b_fantasy*Fantasy + b_children*Children + b_horror*Horror + b_mystery*Mystery + b_war*War + b_animation*Animation + b_musical*Musical + b_western*Western + b_film_noir*Film_Noir + b_documentary*Documentary)/(ifelse(genre_count==0,1,genre_count) + lambda_ig )) %>% 
    mutate("pred" = clamp(mu + alpha_ij +  beta_ij + beta_ig )) %>%  
        pull(pred)

# return the RMSE value for the current value of lambdas_ig
  rmse_genre_regularised <- rmse(edx_cv_test_set$rating, predicted_ratings_genre_user)
  
}

rmses_genre_user <- sapply(lambdas_ig, rmses_genre_function_user)

# Let us record the Min RMSE and the Corresponding value of lambda_ig
min_lambda_ig <- lambdas_ig[which.min(rmses_genre_user)]
min_rmse_genre_user <- round(min(rmses_genre_user),digits = 5)

# Create Dataframe to compute and store Regularised Values of beta_ig - Valid only for edx_cv_train_set, genre_assignment, genre_assignment_matrix for the reason cited earlier. 

# The beta_ig values are computed based on the combination of User and Movie and the entries are unique, hence entries in the genre_assignment_matrix will not match with any entries in the edx_cv_test_set or genre_assigment_test set, hence beta_ig will need to be computed everytime for the genre_assigment_test set for cross validation.

beta_ig_df_final <- genre_assignment_matrix %>%
    left_join(genre_effects_user, by='userId') %>%
    mutate(beta_ig =  (b_drama*Drama + b_comedy*Comedy + b_action*Action + b_thriller*Thriller + b_adventure*Adventure + b_romance*Romance + b_sci_fi*Sci_Fi + b_crime*Crime + b_fantasy*Fantasy + b_children*Children + b_horror*Horror + b_mystery*Mystery + b_war*War + b_animation*Animation + b_musical*Musical + b_western*Western + b_film_noir*Film_Noir + b_documentary*Documentary)/((ifelse(genre_count==0,1,genre_count))+ min_lambda_ig )) %>%
    ungroup() %>%
    select(userId,movieId,beta_ig)



## ----RMSE Summary 6a, echo=FALSE, include=TRUE, warning=FALSE, message = FALSE, out.width="50%"------------------------------------------------------------------------------------------------------------------

# Let us plot the RMSE against the lambda_movie values to understand how the RMSE changes. 

pl6a <- data.frame("lambda_ig" = lambdas_ig, "RMSE" = rmses_genre_user)
pl6a %>% ggplot(aes(x=lambdas_ig, y=RMSE )) + geom_line() + labs(x= "lambda_ig", y = "RMSE", title = "Plot of RMSE against lambda_ig") + theme_clean()

print( c(" Min lambda gi: ", min_lambda_ig), quote = FALSE, justify = "left") 
print( c(" Min RMSE User, Movie & Genre User Effects - Regularised: ", min_rmse_genre_user), quote = FALSE, justify = "left")

print( c(" Improvement in RMSE with Genre User Effects: ", round(min_rmse_penalised_user_movie - min_rmse_genre_user, 6)), quote = FALSE, justify = "left")

rmse_consolidated = data.frame("Model" = c("1-Base", "2-User Effects", "3-User & Movie Effects", "4-User & Movie Effects (R)", "5-User Effects (R) & Movie Effects (R)", "6-User Effects (R), Movie Effects (R) & Genre Effects User (R)"), "RMSE" = c(rmse_base, rmse_user, rmse_user_movie,min_rmse_penalised_movie, min_rmse_penalised_user_movie, min_rmse_genre_user))

knitr::kable( (rmse_consolidated), "simple", align = "lc", caption = "RMSE Summary", digits = 5)

# Remove Data that is no longer required
rm(pl6a, lambdas_ig, rmses_genre_user)


## ----GC-6a2,  warning=FALSE, echo = FALSE, include=FALSE, message = FALSE----------------------------------------------------------------------------------------------------------------------------------------

gc()



## ----Time Effects Movie, include=TRUE, warning=FALSE, message = FALSE, echo=FALSE, out.width="75%"---------------------------------------------------------------------------------------------------------------

# Convert the time stamp field to the Date year. Let us work with Yearly figures.

time_assignment <- edx %>% mutate(date = as_datetime(timestamp)) %>% 
  mutate(date_year = year(date)) %>%  select(movieId,title,rating,date_year) %>% 
  group_by(movieId, title, date_year) %>% 
  summarise(mean_yearly_rating = mean(rating, na.rm = TRUE), .groups = "drop")

# Extract Top 10 Movies with more than 100 Ratings
top_10_titles <- edx %>% 
  group_by(movieId, title) %>% 
  summarize(ratings_received = n(), avg_rating = round(mean(rating),digits=2)) %>% 
  filter(ratings_received >=100) %>% arrange(desc(avg_rating)) %>% select(title) %>% head(10)

# Plot Average Yearly Ratings on the Y Axis against the Year on the X Axis. Smoothen Line Plots using LOESS. Use Degree 1 to prevent oversmoothing. Do not include Confidence Intervals as it causes shading of the plots. 

time_assignment %>% filter(title %in% top_10_titles$title) %>%
  ggplot(aes(x=date_year, y= mean_yearly_rating, color = title)) + 
  labs(x= "Year", y = "Average Rating", title = "Top Movies by Average Rating >=100 Ratings - Time Trends") + geom_smooth(method = "loess",  method.args = list(degree = "1") , formula = "y~x", se = FALSE) + theme_clean() + guides(color = guide_legend(nrow= 5, byrow=TRUE)) +  theme(legend.position= "bottom", legend.justification = "right") + theme(plot.title = element_text(hjust = 1))

# Many Thanks to 
# https://www.statology.org/ggplot2-legend-multiple-rows/
# https://statisticsglobe.com/move-position-of-ggplot2-legend-in-r

# Remove Data that is not needed anymore
rm(time_assignment, top_10_titles)


## ----Time Effects User, include=TRUE, warning=FALSE, message = FALSE, echo=FALSE, out.width="75%"----------------------------------------------------------------------------------------------------------------

# Convert the time stamp field to the Date year. Let us work with Yearly figures.

# Compute Annual Averages for Ratings and record as mean_yearly_rating

time_assignment_user <- edx %>% mutate(userId = as.factor(userId)) %>% 
  mutate(date = as_datetime(timestamp)) %>% 
  mutate(date_year = year(date)) %>%  select(userId,rating,date_year) %>% 
  group_by(userId, date_year) %>% 
  summarise(mean_yearly_rating = mean(rating, na.rm = TRUE), .groups = "drop")

# Extract list of top 10 users by ratings provided

top_10_users <- edx %>% 
  group_by(userId) %>% 
  summarize(ratings_provided = n(), .groups = "drop") %>% 
  top_n(10) %>% arrange(desc(ratings_provided))


# Plot Average Yearly Ratings on the Y Axis against the Year on the X Axis. Smoothen Line Plots using LOESS. Use Degree 1 to prevent oversmoothing. Do not include Confidence Intervals as it causes shading of the plots. 

time_assignment_user %>% filter(userId %in% top_10_users$userId) %>%
  ggplot(aes(x=date_year, y= mean_yearly_rating, color = userId)) + 
  labs(x= "Year", y = "Average Rating", title = "Top Users by Ratings Provided - Time Trends") + geom_smooth(method = "loess",  method.args = list(degree = "1") , formula = "y~x", se = FALSE) + theme_clean() + guides(color = guide_legend(nrow= 5, byrow=TRUE)) +  theme(legend.position= "bottom", legend.justification = "right") + theme(plot.title = element_text(hjust = 1))


# Remove Data that is not needed anymore
rm(time_assignment_user)



## ----Model 7 - Time Effects, include=TRUE, echo=TRUE, message=FALSE, warning=FALSE, out.width="75%"--------------------------------------------------------------------------------------------------------------

# Compute Time Effects for the edx_cv_train_set for Movies

# Extract Year from the Time Stamp, Record as date_year_j. Group by Year and Compute Mean Yearly Rating for each movie. 
time_assignment_movie_edx_cv_train_set <- edx_cv_train_set %>% 
  mutate(date = as_datetime(timestamp)) %>% 
  mutate(date_year_j = year(date)) %>%  select(movieId,rating, date_year_j) %>% 
  group_by(movieId, date_year_j) %>% 
  summarise(mean_yearly_rating = mean(rating, na.rm = TRUE), .groups = "drop")

# Group and Select by movieId and Year to Extract Number of Years for which Ratings are available. Record as n_years_j

time_assignment_n_years_movie_edx_cv_train_set <- time_assignment_movie_edx_cv_train_set %>% 
  group_by(movieId) %>% 
  select(movieId, date_year_j) %>% 
  summarise(movieId, date_year_j, n_years_j = n(), .groups = "drop")

# Compute Average Rating for each movie for edx_cv_train_set
movie_list_edx_cv_train_set <- edx_cv_train_set %>% group_by(movieId) %>% 
  summarise(avg_rating = mean(rating)) 

# Compute b_time_m as difference between Mean Yearly Rating and Average Rating. Combine Tables to consolidate movieId, date_year_j, beta_jt and n_years_j  

time_effects_movie_edx_cv_train_set <- movie_list_edx_cv_train_set %>% 
  ungroup %>% mutate(movieId = as.factor(movieId)) %>% 
  select(movieId, avg_rating) %>%   
  left_join(time_assignment_movie_edx_cv_train_set, by = "movieId") %>%
  mutate("beta_jt" = (mean_yearly_rating - avg_rating)) %>%
  left_join(time_assignment_n_years_movie_edx_cv_train_set, by = join_by(movieId,date_year_j)) %>%
  select(movieId, date_year_j, beta_jt, n_years_j)

# Compute Time Effects for the edx_cv_train_set for Users 

# Extract Year from the Time Stamp, Record as date_year_u. Group by Year and Compute Mean Yearly Rating for each User. 

time_assignment_user_edx_cv_train_set <- edx_cv_train_set %>% 
  mutate(userId = as.factor(userId)) %>% 
  mutate(date = as_datetime(timestamp)) %>% 
  mutate(date_year_i = year(date)) %>%  select(userId,rating,date_year_i) %>% 
  group_by(userId, date_year_i) %>% 
  summarise(mean_yearly_rating = mean(rating, na.rm = TRUE), .groups = "drop")

# Group and Select by userId and Year to Extract Number of Years for which Ratings are available. Record as n_years_i
time_assignment_n_years_user_edx_cv_train_set <- time_assignment_user_edx_cv_train_set %>% 
  group_by(userId) %>% 
  select(userId, date_year_i) %>% 
  summarise(userId, date_year_i, n_years_i = n(), .groups = "drop")

# Compute beta_it as difference between Mean Yearly Rating and Average Rating. Combine Tables to consolidate userId, date_year_i, beta_it and n_years_i 

time_effects_user_edx_cv_train_set <- edx_cv_train_set %>% mutate(userId = as.factor(userId)) %>% 
  group_by(userId) %>% summarise(avg_rating = mean(rating)) %>% 
  select(userId, avg_rating) %>%
  left_join(time_assignment_user_edx_cv_train_set, by = "userId") %>%
  mutate("beta_it" = (mean_yearly_rating - avg_rating)) %>%
  left_join(time_assignment_n_years_user_edx_cv_train_set, by = join_by(userId,date_year_i)) %>%
  select(userId, date_year_i, beta_it, n_years_i)


# Let us prepare the test set for input of time effects. We will reuse the existing Dataset to avoid rework 

# For Some very strange reason, R Generates NaN when beta_t is divided by n_years i.e. beta_t/n_years= NaN. This is a bit surprising because n_years cannot be 0 or -ve. The smallest value of n_years is 1. Offsetting n_years even by some very miniscule value causes the NaN to disappear. 

  predicted_ratings_time <- genre_assignment_test %>% 
    mutate(date = as_datetime(timestamp)) %>% 
    mutate(date_year_j = year(date)) %>% 
    mutate(date_year_i = year(date)) %>%
  left_join(alpha_r_df_combined, by='userId') %>%
  left_join(beta_r_df_combined, by='movieId') %>%
  left_join(genre_effects_user, by = 'userId') %>%
    mutate(beta_ig =  (b_drama*Drama + b_comedy*Comedy + b_action*Action + b_thriller*Thriller + b_adventure*Adventure + b_romance*Romance + b_sci_fi*Sci_Fi + b_crime*Crime + b_fantasy*Fantasy + b_children*Children + b_horror*Horror + b_mystery*Mystery + b_war*War + b_animation*Animation + b_musical*Musical + b_western*Western + b_film_noir*Film_Noir + b_documentary*Documentary)/((ifelse(genre_count==0,1,genre_count)) + min_lambda_ig  )) %>%  
    left_join(time_effects_movie_edx_cv_train_set, by= join_by (movieId, date_year_j)) %>% 
    left_join(time_effects_user_edx_cv_train_set, by= join_by (userId, date_year_i)) %>% 
    mutate_all(list(~replace_na(., 0))) %>% 
        mutate("pred" = clamp(mu + alpha_ij +  beta_ij + beta_ig + beta_jt/(0.0001 + sqrt(n_years_j)) + beta_it/(0.0001 + sqrt(n_years_i)) )) %>%
        select(userId, movieId, pred)
  
  rmse_time <- round(rmse(edx_cv_test_set$rating, predicted_ratings_time$pred),digits = 5)

rm(time_assignment_movie_edx_cv_train_set, time_assignment_n_years_movie_edx_cv_train_set, time_assignment_user_edx_cv_train_set, time_assignment_n_years_user_edx_cv_train_set)

print( c(" RMSE Time Effects: ", rmse_time), quote = FALSE, justify = "left") 


## ----RMSE Summary update 6, echo=FALSE, include=TRUE, warning=FALSE, message = FALSE-----------------------------------------------------------------------------------------------------------------------------

print( c(" RMSE User Effects (R), Movie Effects (R), Genre Effects (All) & Time Effects", rmse_time), quote = FALSE, justify = "left")

print( c(" Improvement in RMSE with Time Effects ", round(min_rmse_genre_user - rmse_time, 5)), quote = FALSE, justify = "left")


rmse_consolidated = data.frame("Model" = c("1-Base", "2-User Effects", "3-User & Movie Effects", "4-User & Movie Effects (R)", "5-User Effects (R) & Movie Effects (R)", "6-User Effects (R), Movie Effects (R) & Genre Effects User(R)", "7-User Effects (R), Movie Effects (R), Genre Effects User (R) & Time Effects"), "RMSE" = c(rmse_base, rmse_user, rmse_user_movie, min_rmse_penalised_movie, min_rmse_penalised_user_movie, min_rmse_genre_user, rmse_time))


knitr::kable( (rmse_consolidated), "simple", align = "lc", caption = "RMSE Summary", digits = 5)

rm(pl7a,pl7b)



## ----GC-7, warning=FALSE, echo = FALSE, include=FALSE, message = FALSE-------------------------------------------------------------------------------------------------------------------------------------------

gc()




## ----Model 7 Check Top 10 Ratings with Time Effects, include=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="75%"-------------------------------------------------------------------------------------

# Check ratings including Time Effects

sm7 <- edx_cv_train_set  %>%
    mutate(date = as_datetime(timestamp)) %>% 
    mutate(date_year_j = year(date)) %>% 
    mutate(date_year_i = year(date)) %>%
    left_join(alpha_r_df_combined, by='userId') %>%
    left_join(beta_r_df_combined, by='movieId') %>%
    left_join(beta_ig_df_final, by= join_by(userId, movieId)) %>%
    left_join(time_effects_movie_edx_cv_train_set, by= join_by (movieId, date_year_j)) %>% 
    left_join(time_effects_user_edx_cv_train_set, by= join_by (userId, date_year_i)) %>% 
    mutate_all(list(~replace_na(., 0))) %>% 
      mutate("pred" = clamp(mu + alpha_ij +  beta_ij + beta_ig +  beta_jt/(0.0001 + sqrt(n_years_j)) + beta_it/(0.0001 + sqrt(n_years_i)) ))  %>%  
  group_by(title) %>% 
  summarize(ratings_received = n(), avg_rating = round(mean(pred),digits=2)) %>% 
  top_n(10) %>% arrange(desc(avg_rating)) 

pm7a <- sm7 %>% ggplot(aes(x=reorder(title,avg_rating),y =avg_rating)) + 
  geom_text(aes(label=avg_rating), hjust = -0.15) +
  labs(x= "title", y = "Average Rating", title = "Top Movies by Predicted Rating - Average Rating") + 
  geom_col() + coord_flip() + ylim(0,6) +  theme_clean() +  theme(plot.title = element_text(hjust = 1))

  
pm7b <- sm7 %>%  ggplot(aes(x=reorder(title,avg_rating),y =ratings_received)) + 
  geom_text(aes(label=ratings_received), hjust = -0.15) +
  labs(x= "title", y = "ratings_received", title = "Top Movies by Predicted Rating - Ratings Received") + 
  geom_col() + coord_flip() + ylim(0, 35000) + theme_clean() + theme(plot.title = element_text(hjust = 1))

grid.arrange(pm7a, pm7b, nrow = 2)

rm(sm7, pm7a, pm7b)


## ----Model 8 - User, Movie, Genre & Time Effects - User Distribution Discrete , include=TRUE, warning=FALSE, echo = TRUE, out.width="75%", message = FALSE-------------------------------------------------------

# Check Distributions with User, Movie, Genre & Time Effects Included and Predictions rounded to Discrete Values for Top 10 Users

genre_assignment %>% 
    mutate(date = as_datetime(timestamp)) %>% 
    mutate(date_year_j = year(date)) %>% 
    mutate(date_year_i = year(date)) %>%
    left_join(alpha_r_df_combined, by='userId') %>%
    left_join(beta_r_df_combined, by='movieId') %>%
    left_join(genre_effects_user,  by='userId') %>%
    left_join(time_effects_movie_edx_cv_train_set, by= join_by (movieId, date_year_j)) %>% 
    left_join(time_effects_user_edx_cv_train_set, by= join_by (userId, date_year_i)) %>% 
    mutate_all(list(~replace_na(., 0))) %>% 
    mutate(beta_ig =  (b_drama*Drama + b_comedy*Comedy + b_action*Action + b_thriller*Thriller + b_adventure*Adventure + b_romance*Romance + b_sci_fi*Sci_Fi + b_crime*Crime + b_fantasy*Fantasy + b_children*Children + b_horror*Horror + b_mystery*Mystery + b_war*War + b_animation*Animation + b_musical*Musical + b_western*Western + b_film_noir*Film_Noir + b_documentary*Documentary)/((ifelse(genre_count==0,1,genre_count))+ min_lambda_ig )) %>% 
    mutate("pred" = clamp(mu + alpha_ij +  beta_ij + beta_ig +  beta_jt/(0.0001 + sqrt(n_years_j)) + beta_it/(0.0001 + sqrt(n_years_i))  )) %>% 
    mutate(pred = signif(pred/5, digits = 1)) %>% 
    mutate(pred = (pred*5)) %>% 
  filter (userId %in% top_10_users$userId) %>% 
  ggplot(aes( x= pred)) + geom_histogram(binwidth = 0.2) + 
  facet_wrap( ~ userId,  nrow =2, ncol =5 ) +  
  labs(x= "userId", y = "Count", title = "Prediction Distribution for Top Users with Discrete Values" ) + theme_minimal() + theme(plot.title = element_text(hjust = 0,size = 12))



## ----GC-8a,  warning=FALSE, echo = FALSE, include=FALSE, message = FALSE-----------------------------------------------------------------------------------------------------------------------------------------

gc()


## ----Model 8 - User, Movie, Genre & Time Effects Regularised - Movie Distribution - Discrete , include=TRUE, warning=FALSE, echo = FALSE, out.width="75%", message = FALSE---------------------------------------

# Check Distributions with User, Movie, Genre & Time Effects Included and Predictions rounded to Discrete Values for Top 10 Movies

genre_assignment %>% 
    mutate(date = as_datetime(timestamp)) %>% 
    mutate(date_year_j = year(date)) %>% 
    mutate(date_year_i = year(date)) %>%
    left_join(alpha_r_df_combined, by='userId') %>%
    left_join(beta_r_df_combined, by='movieId') %>%
    left_join(genre_effects_user,  by='userId') %>%
    left_join(time_effects_movie_edx_cv_train_set, by= join_by (movieId, date_year_j)) %>% 
    left_join(time_effects_user_edx_cv_train_set, by= join_by (userId, date_year_i)) %>% 
    mutate_all(list(~replace_na(., 0))) %>% 
    mutate(beta_ig =  (b_drama*Drama + b_comedy*Comedy + b_action*Action + b_thriller*Thriller + b_adventure*Adventure + b_romance*Romance + b_sci_fi*Sci_Fi + b_crime*Crime + b_fantasy*Fantasy + b_children*Children + b_horror*Horror + b_mystery*Mystery + b_war*War + b_animation*Animation + b_musical*Musical + b_western*Western + b_film_noir*Film_Noir + b_documentary*Documentary)/((ifelse(genre_count==0,1,genre_count))+ min_lambda_ig )) %>% 
    mutate("pred" = clamp(mu + alpha_ij +  beta_ij + beta_ig  +  beta_jt/(0.0001 + sqrt(n_years_j)) + beta_it/(0.0001 + sqrt(n_years_i))  )) %>% 
    mutate(pred = signif(pred/5, digits = 1)) %>% 
    mutate(pred = (pred*5)) %>% 
  filter (title %in% top_rated_10$title) %>% 
  ggplot(aes( x= pred)) + geom_histogram(binwidth = 0.2) + facet_wrap( ~ title,  nrow =2, ncol =5, labeller = label_wrap_gen(width = 18, multi_line = TRUE)) +  labs(x= "Movie Titles", y = "Count", title = "Prediction Distribution for Top Movies with Discrete Values") + theme_minimal() + xlim(0.5,5.0) + theme(plot.title = element_text(hjust = 0, size =12))



## ----GC-8b,  warning=FALSE, echo = FALSE, include=FALSE, message = FALSE-----------------------------------------------------------------------------------------------------------------------------------------

gc()


## ----Model 8 Check Top 10 Ratings with Discrete Values, include=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="75%"----------------------------------------------------------------------------------

# Check ratings including Rounding to Discrete Values

sm8 <- genre_assignment  %>%
    mutate(date = as_datetime(timestamp)) %>% 
    mutate(date_year_j = year(date)) %>% 
    mutate(date_year_i = year(date)) %>%
    left_join(alpha_r_df_combined, by='userId') %>%
    left_join(beta_r_df_combined, by='movieId') %>%
    left_join(genre_effects_user,  by='userId') %>%
    left_join(time_effects_movie_edx_cv_train_set, by= join_by (movieId, date_year_j)) %>% 
    left_join(time_effects_user_edx_cv_train_set, by= join_by (userId, date_year_i)) %>% 
    mutate_all(list(~replace_na(., 0))) %>% 
    mutate(beta_ig =  (b_drama*Drama + b_comedy*Comedy + b_action*Action + b_thriller*Thriller + b_adventure*Adventure + b_romance*Romance + b_sci_fi*Sci_Fi + b_crime*Crime + b_fantasy*Fantasy + b_children*Children + b_horror*Horror + b_mystery*Mystery + b_war*War + b_animation*Animation + b_musical*Musical + b_western*Western + b_film_noir*Film_Noir + b_documentary*Documentary)/((ifelse(genre_count==0,1,genre_count))+ min_lambda_ig )) %>% 
    mutate("pred" = clamp(mu + alpha_ij +  beta_ij + beta_ig  +  beta_jt/(0.0001 + sqrt(n_years_j)) + beta_it/(0.0001 + sqrt(n_years_i)) ))  %>%  
  mutate(pred = signif(pred/5, digits = 1)) %>% 
  mutate(pred = (pred*5)) %>% 
  group_by(title) %>% 
  summarize(ratings_received = n(), avg_rating = round(mean(pred),digits=2)) %>% 
  filter(ratings_received >=100) %>% 
  top_n(10) %>% arrange(desc(avg_rating)) 

pm8a <- sm8 %>% ggplot(aes(x=reorder(title,avg_rating),y =avg_rating)) + 
  geom_text(aes(label=avg_rating), hjust = -0.15) +
  labs(x= "title", y = "Average Rating", title = "Top Movies by Predicted Rating - Average Rating") + 
  geom_col() + coord_flip() + ylim(0,6) +  theme_clean() +  theme(plot.title = element_text(hjust = 1))

  
pm8b <- sm8 %>%  ggplot(aes(x=reorder(title,avg_rating),y =ratings_received)) + 
  geom_text(aes(label=ratings_received), hjust = -0.15) +
  labs(x= "title", y = "ratings_received", title = "Top Movies by Predicted Rating (100+ ratings)") + 
  geom_col() + coord_flip() + ylim(0, 35000) + theme_clean() + theme(plot.title = element_text(hjust = 1))

grid.arrange(pm8a, pm8b, nrow = 2)

rm(sm8, pm8a, pm8b)




## ----Model 8 - RMSE with Predictions Rounded to closest values - Significant Predictors, include=TRUE, warning=FALSE, echo = FALSE, out.width="75%", message = FALSE---------------------------------------------

# Compute RMSE with all User, Movie and Genre Effects Included and Predictions rounded to Discrete Values 

predicted_ratings_rounded <- genre_assignment_test %>% 
    mutate(date = as_datetime(timestamp)) %>% 
    mutate(date_year_j = year(date)) %>% 
    mutate(date_year_i = year(date)) %>%
    left_join(alpha_r_df_combined, by='userId') %>%
    left_join(beta_r_df_combined, by='movieId') %>%
    left_join(genre_effects_user, by = 'userId') %>%
    left_join(time_effects_movie_edx_cv_train_set, by= join_by (movieId, date_year_j)) %>% 
    left_join(time_effects_user_edx_cv_train_set, by= join_by (userId, date_year_i)) %>% 
        mutate_all(list(~replace_na(., 0))) %>% 
        mutate(beta_ig =  (b_drama*Drama + b_comedy*Comedy + b_action*Action + b_thriller*Thriller + b_adventure*Adventure + b_romance*Romance + b_sci_fi*Sci_Fi + b_crime*Crime + b_fantasy*Fantasy + b_children*Children + b_horror*Horror + b_mystery*Mystery + b_war*War + b_animation*Animation + b_musical*Musical + b_western*Western + b_film_noir*Film_Noir + b_documentary*Documentary)/((ifelse(genre_count==0,1,genre_count))+ min_lambda_ig )) %>%  
        mutate("pred" = clamp(mu + alpha_ij +  beta_ij + beta_ig  + beta_jt/(0.0001 + sqrt(n_years_j)) + beta_it/(0.0001 + sqrt(n_years_i)) )) %>% 
        mutate(pred = signif(pred/5, digits = 1)) %>% 
        mutate(pred = (pred*5)) %>% 
        select(userId,movieId,rating,pred)
        

rmse_rounded_significant <- round(rmse(predicted_ratings_rounded$rating, predicted_ratings_rounded$pred),digits = 5)

print( c(" RMSE with Predictions Rounded to Discrete Values - Significant Predictors: ", round(rmse_rounded_significant, digits = 5)), quote = FALSE, justify = "left") 

# Remove Datasets that are no longer needed
rm(predicted_ratings_rounded)



## ----Print final summary of Analysis Section, include=TRUE, echo=FALSE, warning=FALSE, message = FALSE-----------------------------------------------------------------------------------------------------------

# Print Final RMSE Values 

rmse_consolidated = data.frame("Model" = c("1-Base", "2-User Effects", "3-User & Movie Effects", "4-User & Movie Effects (R)", "5-User Effects (R) & Movie Effects(R)", "6-User Effects (R), Movie Effects (R) & Genre Effects User (R)", "7-User Effects (R), Movie Effects (R), Genre Effects User (R) & Time Effects", "8- RMSE with Discrete Values for Rating"), "RMSE" = c(rmse_base, rmse_user, rmse_user_movie, min_rmse_penalised_movie, min_rmse_penalised_user_movie, min_rmse_genre_user, rmse_time, rmse_rounded_significant))


knitr::kable( (rmse_consolidated), "simple", align = "lc", caption = "RMSE Summary", digits = 5)



## ----Let us calculate the residuals after the Time Effects, include=TRUE, warning=FALSE, message = FALSE---------------------------------------------------------------------------------------------------------

# Let us calcuate the Residuals as the Difference between the Actual Ratings and Our Predictions for the edx_cv_train_set, We will also extract the userId, movieId and all the Co-efficients.

predicted_ratings_final_errors_file <- "predicted_ratings_final_errors.RData"

if(file.exists(predicted_ratings_final_errors_file)){
     load(predicted_ratings_final_errors_file)
      rm(predicted_ratings_final_errors_file)
} else {

  predicted_ratings_final_errors <- genre_assignment %>%
    mutate(date = as_datetime(timestamp)) %>% 
    mutate(date_year_i = year(date)) %>% 
    mutate(date_year_j = year(date)) %>% 
    left_join(alpha_r_df_combined, by='userId') %>%
    left_join(beta_r_df_combined, by='movieId') %>%
    left_join(genre_effects_user, by="userId") %>%
    left_join(time_effects_movie_edx_cv_train_set, by= join_by (movieId, date_year_j)) %>% 
    left_join(time_effects_user_edx_cv_train_set, by= join_by (userId, date_year_i)) %>% 
    mutate_all(list(~replace_na(., 0))) %>% 
    mutate(beta_ig =  (b_drama*Drama + b_comedy*Comedy + b_action*Action + b_thriller*Thriller + b_adventure*Adventure + b_romance*Romance + b_sci_fi*Sci_Fi + b_crime*Crime + b_fantasy*Fantasy + b_children*Children + b_horror*Horror + b_mystery*Mystery + b_war*War + b_animation*Animation + b_musical*Musical + b_western*Western + b_film_noir*Film_Noir + b_documentary*Documentary)/((ifelse(genre_count==0,1,genre_count))+ min_lambda_ig )) %>%  
      mutate("pred" = clamp(mu + alpha_ij +  beta_ij + beta_ig + beta_jt/(0.0001 + sqrt(n_years_j)) + beta_it/(0.0001 + sqrt(n_years_i)) )) %>%
    mutate(pred = signif(pred/5, digits = 1)) %>% 
    mutate(pred = (pred*5)) %>%
    mutate(errors = rating - pred) %>%
    select("userId", "movieId", "rating", "alpha_ij", "beta_ij", 'beta_ig', "beta_it", "beta_jt","pred", "errors")

save(predicted_ratings_final_errors, file = "predicted_ratings_final_errors.RData")
}

rm(predicted_ratings_final_errors_file)


## ----clean up data sets that are no longer required, include=FALSE, echo=FALSE, warning=FALSE, message = FALSE---------------------------------------------------------------------------------------------------
# Remove Data sets that are no longer required to recover Memory and Erroneous Lookup in later code
rm(alpha_r_df_final, beta_r_df_initial, genre_assignment_matrix,  genre_effects_user,time_assignment, time_effects_user_edx_cv_train_set,time_effects_movie_edx_cv_train_set, movie_list_edx_cv_train_set, genre_effects_movie, genre_summary, genre_user_summary, genre_user_mean ,predicted_ratings_genre, predicted_ratings_genre_movie, predicted_ratings_time, beta_ig_df, beta_ig_df_final, alpha_r_df_combined, beta_r_df_combined)

rm(edx_cv_train_set, edx_cv_test_set)


rm(genre_difference_from_overall_genre_mean,individual_genre_count, individual_genre_mean, individual_genre_sums, min_lambda_genre, min_lambda_movie, min_lambda_movie_2, min_lambda_time, min_lambda_user,min_lambda_genre_movie, min_lambda_genre_user, min_rmse_genre_user, min_rmse_penalised_movie, min_rmse_penalised_movie_2, min_rmse_penalised_user, min_rmse_time, overall_genre_mean, rmse_base, rmse_genre_effects, rmse_consolidated, rmse_user, rmse_user_movie, rmse_genre_effects_user, rmse_genre_effects_movie, rmse_time, rmses_genre_function, rmses_time_fn, rmses_time_movie, rmses_time_user, min_rmse_time_movie, min_rmse_time_user, min_lambda_it,min_lambda_jt, rmse_genre_overall, rmse_rounded_all,rmse_rounded_significant, min_rmse_penalised_user_2, min_rmse_penalised_movie_3, min_rmse_penalised_user_movie)

rm(rmses_genre_function_movie, rmses_genre_function_user, rmses_time_function_movie,rmses_time_function_user)


gc()



## ----Error Analysis - 1, include=TRUE, echo=FALSE, warning=FALSE, message=FALSE----------------------------------------------------------------------------------------------------------------------------------

# Let us extract the errors from our earlier saved computation and filter by each step of the rating range 
# Let us compute and store the RMSE for each step
ratings_0.5 <- predicted_ratings_final_errors %>% 
                          filter(rating == 0.5) 
rmse_0.5 <- rmse(ratings_0.5$rating, ratings_0.5$pred)

ratings_1.0 <- predicted_ratings_final_errors %>% 
                          filter(rating == 1.0) 
rmse_1.0 <- rmse(ratings_1.0$rating, ratings_1.0$pred)

ratings_1.5 <- predicted_ratings_final_errors %>% 
                          filter(rating == 1.5) 
rmse_1.5 <- rmse(ratings_1.5$rating, ratings_1.5$pred)

ratings_2.0 <- predicted_ratings_final_errors %>% 
                          filter(rating == 2.0) 
rmse_2.0 <- rmse(ratings_2.0$rating, ratings_2.0$pred)

ratings_2.5 <- predicted_ratings_final_errors %>% 
                          filter(rating == 2.5) 
rmse_2.5 <- rmse(ratings_2.5$rating, ratings_2.5$pred)


ratings_3.0 <- predicted_ratings_final_errors %>% 
                          filter(rating == 3.0) 
rmse_3.0 <- rmse(ratings_3.0$rating, ratings_3.0$pred)

ratings_3.5 <- predicted_ratings_final_errors %>% 
                          filter(rating == 3.5) 
rmse_3.5 <- rmse(ratings_3.5$rating, ratings_3.5$pred)

ratings_4.0 <- predicted_ratings_final_errors %>% 
                          filter(rating == 4.0) 
rmse_4.0 <- rmse(ratings_4.0$rating, ratings_4.0$pred)

ratings_4.5 <- predicted_ratings_final_errors %>% 
                          filter(rating == 4.5) 
rmse_4.5 <- rmse(ratings_4.5$rating, ratings_4.5$pred)

ratings_5.0 <- predicted_ratings_final_errors %>% 
                          filter(rating == 5.0) 
rmse_5.0 <- rmse(ratings_5.0$rating, ratings_5.0$pred)

# Let us create a Dataframe to store each of the rating steps, the corresponding RMSE and the number of ratings for each rating step
rmse_error_summary <- data_frame("RATINGS" = c(seq(0.5,5.0,0.5)), "RMSE"=c(rmse_0.5,rmse_1.0,rmse_1.5,rmse_2.0,rmse_2.5,rmse_3.0,rmse_3.5,rmse_4.0,rmse_4.5,rmse_5.0), "COUNT" = c(nrow(ratings_0.5), nrow(ratings_1.0), nrow(ratings_1.5), nrow(ratings_2.0), nrow(ratings_2.5), nrow(ratings_3.0), nrow(ratings_3.5), nrow(ratings_4.0), nrow(ratings_4.5), nrow(ratings_5.0)))

# Let us print the  rating steps, the corresponding RMSE and the number of ratings for each step. Font Size chosen for ensuring that Tables do not get cropped udring PDF conversion. 

knitr::kable(rmse_error_summary, caption = "RMSE Error Summary for Different Ratings", align = "c") 




## ----Error Analysis 2a, include=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="75%"------------------------------------------------------------------------------------------------------------------

# Error Analysis for Ratings = 0.5, Erroneous Predictions 
# Compute Mean of the absolute value of errors by userId. Absolute values of error required to avoid positive & negative errors from cancelling each other out. Arrange by Descending Order. 
# Choose Top 10 Users with the Largest Mean errors

top_10_errors_0.5 <- ratings_0.5 %>% 
      group_by(userId) %>% mutate("mean_error" = mean(abs(errors))) %>% 
      arrange(desc(mean_error)) %>% head(10)

# Print userId, movieId, rating, regression coefficients, prediction and errors for Top 10 users with the Largest Mean errors
knitr::kable(top_10_errors_0.5 %>% select("userId", "movieId", "rating", "alpha_ij", "beta_ij", 'beta_ig', "beta_it", "beta_jt","pred", "errors"), caption = "Top 10 Error Entries for Rating = 0.5", align = "l", col.names = c("userId", "movieId", "rating", "alpha_ij", "beta_ij", 'beta_ig', "beta_it", "beta_jt","pred", "errors"), digits = 5) 

# Print movieId, title, genres and average rating for Movies with the Largest Mean errors
knitr::kable((movie_list %>% ungroup %>% select(movieId,title,genres,avg_rating) %>% filter(movieId %in% c(top_10_errors_0.5$movieId))), caption = "List of Movies in Top 10 Errors for Rating = 0.5", align = "c") 

# Print userId, Mean and Standard Deviation for Top 10 users with the Largest Mean errors
knitr::kable(user_mean_sd %>% filter(userId %in% c(top_10_errors_0.5$userId)), caption = "Mean and SD for Users in the Top 10 Errors for Rating = 0.5" , align = "c") 

# Extract the Ratings and Predictions for Top 10 Users with the Largest Mean errors 
top_10_erros_0.5_users <- predicted_ratings_final_errors %>% filter(userId %in% c(top_10_errors_0.5$userId)) %>% select(userId,rating,pred)

# Extract the Ratings and Predictions for Movies rated by Top 10 Users with the Largest Mean errors 
top_10_erros_0.5_movies <- predicted_ratings_final_errors %>% filter(movieId %in% c(top_10_errors_0.5$movieId))  %>% select(movieId,rating,pred)

# Print RMSE for  Top 10 Users with the Largest Mean errors and the RMSE for Movies rated by these Users
knitr::kable(data.frame(round(rmse(top_10_erros_0.5_users$rating, top_10_erros_0.5_users$pred),digits = 5),round(rmse(top_10_erros_0.5_movies$rating, top_10_erros_0.5_movies$pred),digits = 5)), col.names = c("User", "Movie"), caption = "RMSE - Top 10 Errors for Rating = 0.5", align = "c") 


## ----Error Investigations 2b, include=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="75%"------------------------------------------------------------------------------------------------------------

# Error Analysis for Ratings = 0.5, Accurate  Predictions 
# Compute Mean of the absolute value of errors by userId. Absolute values of error required to avoid positive & negative errors from cancelling each other out. Arrange by Ascending Order. 
# Choose Top 10 Users with the Smallest Mean errors

top_10_errors_0.5_asc  <- ratings_0.5 %>% group_by(userId) %>% 
      mutate("mean_error" = mean(abs(errors))) %>% 
      arrange((mean_error)) %>% head(10)


# Print userId, movieId, rating, regression coefficients, prediction and errors for Top 10 users with the Smallest Mean errors
knitr::kable(top_10_errors_0.5_asc %>% select("userId", "movieId", "rating", "alpha_ij", "beta_ij", 'beta_ig', "beta_it", "beta_jt","pred", "errors"), caption = "Top 10 Accurate Predictions for Rating = 0.5", align = "c", col.names = c("userId", "movieId", "rating", "alpha_ij", "beta_ij", "beta_ig", "beta_it", "beta_jt","pred", "errors"), digits = 5) 

# Print movieId, title, genres and average rating for Movies with the Smallest Mean errors
knitr::kable((movie_list %>% ungroup %>% select(movieId,title,genres,avg_rating) %>% filter(movieId %in% c(top_10_errors_0.5_asc$movieId))), caption = "List of Movies in Top 10 Accurate Predictions for Rating = 0.5", align = "c") 

# Print userId, Mean and Standard Deviation for Top 10 users with the Smallest Mean errors
knitr::kable(user_mean_sd %>% filter(userId %in% c(top_10_errors_0.5_asc$userId)), caption = "Mean and SD for Users in the Top 10 Accurate Predictions for Rating = 0.5" , align = "c") 

# Extract the Ratings and Predictions for Top 10 Users with the Smallest Mean errors 
top_10_erros_0.5_users_asc <- predicted_ratings_final_errors %>% filter(userId %in% c(top_10_errors_0.5_asc$userId)) %>% select(userId,rating,pred)

# Extract the Ratings and Predictions for Movies rated by Top 10 Users with the Smallest Mean errors 
top_10_erros_0.5_movies_asc <- predicted_ratings_final_errors %>% filter(movieId %in% c(top_10_errors_0.5_asc$movieId)) %>% select(movieId,rating,pred)

# Print RMSE for  Top 10 Users with the Smallest Mean errors and the RMSE for Movies rated by these Users
knitr::kable(data.frame(round(rmse(top_10_erros_0.5_users_asc$rating, top_10_erros_0.5_users_asc$pred),digits = 5),round(rmse(top_10_erros_0.5_movies_asc$rating, top_10_erros_0.5_movies_asc$pred),digits = 5)), col.names = c("User", "Movie"), caption = "RMSE - Top 10 Accurate Predictions for Rating = 0.5", align = "c") 


## ----Error Investigations 3a, include=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="75%"------------------------------------------------------------------------------------------------------------

# Error Analysis for Ratings = 1.0, Erroneous Predictions 
# Compute Mean of the absolute value of errors by userId. Absolute values of error required to avoid positive & negative errors from cancelling each other out. Arrange by Descending Order. 
# Choose Top 10 Users with the Largest Mean errors
top_10_errors_1.0  <- ratings_1.0 %>% group_by(userId) %>% 
      mutate("mean_error" = mean(abs(errors))) %>% 
      arrange(desc(mean_error)) %>% head(10)

# Print userId, movieId, rating, regression coefficients, prediction and errors for Top 10 users with the Largest Mean errors  
knitr::kable(top_10_errors_1.0 %>% select("userId", "movieId", "rating", "alpha_ij", "beta_ij", 'beta_ig', "beta_it", "beta_jt","pred", "errors"), caption = "Top 10 Error Entries for Rating = 1.0", align = "c", col.names = c("userId", "movieId", "rating", "alpha_ij", "beta_ij", 'beta_ig',"beta_it", "beta_jt","pred", "errors"), digits = 5) 

# Print movieId, title, genres and average rating for Movies with the Largest Mean errors
knitr::kable((movie_list %>% ungroup %>% select(movieId,title,genres,avg_rating) %>% filter(movieId %in% c(top_10_errors_1.0$movieId))), caption = "List of Movies in Top 10 Errors for Rating = 1.0", align = "c")

# Print userId, Mean and Standard Deviation for Top 10 users with the Largest Mean errors
knitr::kable(user_mean_sd %>% filter(userId %in% c(top_10_errors_1.0$userId)), caption = "Mean and SD for Users in the Top 10 Errors for Rating = 1.0" , align = "c")

# Extract the Ratings and Predictions for Top 10 Users with the Largest Mean errors 
top_10_erros_1.0_users <- predicted_ratings_final_errors %>% filter(userId %in% c(top_10_errors_1.0$userId)) %>% select(userId,rating,pred)

# Extract the Ratings and Predictions for Movies rated by Top 10 Users with the Largest Mean errors 
top_10_erros_1.0_movies <- predicted_ratings_final_errors %>% filter(movieId %in% c(top_10_errors_1.0$movieId))  %>% select(movieId,rating,pred)

# Print RMSE for  Top 10 Users with the Largest Mean errors and the RMSE for Movies rated by these Users
knitr::kable(data.frame(round(rmse(top_10_erros_1.0_users$rating, top_10_erros_1.0_users$pred),digits = 5),round(rmse(top_10_erros_1.0_movies$rating, top_10_erros_1.0_movies$pred),digits = 5)), col.names = c("User", "Movie"), caption = "RMSE - Top 10 Errors for Rating = 1.0", align = "c") 



## ----Error Investigations 3b, include=TRUE, echo=FALSE, warning=FALSE, message=FALSE,out.width="75%"-------------------------------------------------------------------------------------------------------------

# Error Analysis for Ratings = 1.0, Accurate Predictions 
# Compute Mean of the absolute value of errors by userId. Absolute values of error required to avoid positive & negative errors from cancelling each other out. Arrange by Ascending Order. 
# Choose Top 10 Users with the Smallest Mean errors
top_10_errors_1.0_asc <- ratings_1.0 %>% group_by(userId) %>% 
      mutate("mean_error" = mean(abs(errors))) %>% 
      arrange((mean_error)) %>% head(10)

# Print userId, movieId, rating, regression coefficients, prediction and errors for Top 10 users with the Smallest Mean errors  
knitr::kable(top_10_errors_1.0_asc %>% select("userId", "movieId", "rating", "alpha_ij", "beta_ij", 'beta_ig', "beta_it", "beta_jt","pred", "errors"), caption = "Top 10 Accurate Predictions for Rating = 1.0", align = "c", col.names = c("userId", "movieId", "rating", "alpha_ij", "beta_ij", 'beta_ig', "beta_it", "beta_jt","pred", "errors"), digits = 5) 

# Print movieId, title, genres and average rating for Movies with the Smallest Mean errors
knitr::kable((movie_list %>% ungroup %>% select(movieId,title,genres,avg_rating) %>% filter(movieId %in% c(top_10_errors_1.0_asc$movieId))), caption = "List of Movies in Top 10 Accurate Predictions for Rating = 1.0", align = "c") 

# Print userId, Mean and Standard Deviation for Top 10 users with the Smallest Mean errors
knitr::kable(user_mean_sd %>% filter(userId %in% c(top_10_errors_1.0_asc$userId)), caption = "Mean and SD for Users in the Top 10 Accurate Predictions for Rating = 1.0" , align = "c") 

# Extract the Ratings and Predictions for Top 10 Users with the Smallest Mean errors 
top_10_erros_1.0_users_asc <- predicted_ratings_final_errors %>% filter(userId %in% c(top_10_errors_1.0_asc$userId))  %>% select(userId,rating,pred)

# Extract the Ratings and Predictions for Movies rated by Top 10 Users with the Smallest Mean errors 
top_10_erros_1.0_movies_asc <- predicted_ratings_final_errors %>% filter(movieId %in% c(top_10_errors_1.0_asc$movieId))  %>% select(movieId,rating,pred)

# Print RMSE for  Top 10 Users with the Smallest Mean errors and the RMSE for Movies rated by these Users
knitr::kable(data.frame(round(rmse(top_10_erros_1.0_users_asc$rating, top_10_erros_1.0_users_asc$pred),digits = 5),round(rmse(top_10_erros_1.0_movies_asc$rating, top_10_erros_1.0_movies_asc$pred),digits = 5)), col.names = c("User", "Movie"), caption = "RMSE - Top 10 Accurate Predictions for Rating = 1.0", align = "c")


## ----Error Investigations 4a, include=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="75%"------------------------------------------------------------------------------------------------------------

# Error Analysis for Ratings = 5.0, Erroneous Predictions 
# Compute Mean of the absolute value of errors by userId. Absolute values of error required to avoid positive & negative errors from cancelling each other out. Arrange by Descending Order. 
# Choose Top 10 Users with the Largest Mean errors
top_10_errors_5.0 <- ratings_5.0 %>% group_by(userId) %>% 
      mutate("mean_error" = mean(abs(errors))) %>% 
      arrange(desc(mean_error)) %>% head(10)

# Print userId, movieId, rating, regression coefficients, prediction and errors for Top 10 users with the Largest Mean errors    
knitr::kable(top_10_errors_5.0 %>% select("userId", "movieId", "rating", "alpha_ij", "beta_ij", 'beta_ig',"beta_it", "beta_jt","pred", "errors"), caption = "Top 10 Error Entries for Rating = 5.0", align = "c", col.names = c("userId", "movieId", "rating", "alpha_ij", "beta_ij", 'beta_ig',"beta_it", "beta_jt","pred", "errors"), digits = 5) 

# Print movieId, title, genres and average rating for Movies with the Largest Mean errors
knitr::kable((movie_list %>% ungroup %>% select(movieId,title,genres,avg_rating) %>% filter(movieId %in% c(top_10_errors_5.0$movieId))), caption = "List of Movies in Top 10 Errors for Rating = 5.0", align = "c") 

# Print userId, Mean and Standard Deviation for Top 10 users with the Largest Mean errors
knitr::kable(user_mean_sd %>% filter(userId %in% c(top_10_errors_5.0$userId)), caption = "Mean and SD for Users in the Top 10 Errors for Rating = 5.0" , align = "c") 

# Extract the Ratings and Predictions for Top 10 Users with the Largest Mean errors 
top_10_erros_5.0_users <- predicted_ratings_final_errors %>% filter(userId %in% c(top_10_errors_5.0$userId))  %>% select(userId,rating,pred)

# Extract the Ratings and Predictions for Movies rated by Top 10 Users with the Largest Mean errors 
top_10_erros_5.0_movies <- predicted_ratings_final_errors %>% filter(movieId %in% c(top_10_errors_5.0$movieId))  %>% select(movieId,rating,pred)

# Print RMSE for  Top 10 Users with the Largest Mean errors and the RMSE for Movies rated by these Users
knitr::kable(data.frame(round(rmse(top_10_erros_5.0_users$rating, top_10_erros_5.0_users$pred),digits = 5),round(rmse(top_10_erros_5.0_movies$rating, top_10_erros_5.0_movies$pred),digits = 5)), col.names = c("User", "Movie"), caption = "RMSE - Top 10 Errors for Rating = 5.0", align = "c") 


## ----Error Investigations 4b, include=TRUE, echo=FALSE, warning=FALSE, message=FALSE,out.width="75%"-------------------------------------------------------------------------------------------------------------

# Error Analysis for Ratings = 5.0, Accurate Predictions 
# Compute Mean of the absolute value of errors by userId. Absolute values of error required to avoid positive & negative errors from cancelling each other out. Arrange by Ascending Order.
# Choose Top 10 Users with the Smallest Mean errors
top_10_errors_5.0_asc  <- ratings_5.0 %>% group_by(userId) %>% 
      mutate("mean_error" = mean(abs(errors))) %>% 
      arrange((mean_error)) %>% head(10)

# Print userId, movieId, rating, regression coefficients, prediction and errors for Top 10 users with the Smallest Mean errors    
knitr::kable(top_10_errors_5.0_asc %>% select("userId", "movieId", "rating", "alpha_ij", "beta_ij", 'beta_ig', "beta_it", "beta_jt","pred", "errors"), caption = "Top 10 Accurate Predictions for Rating = 5.0", align = "c", col.names = c("userId", "movieId", "rating", "alpha_ij", "beta_ij", 'beta_ig', "beta_it", "beta_jt","pred", "errors"), digits = 5) 

# Print movieId, title, genres and average rating for Movies with the Smallest Mean errors
knitr::kable((movie_list %>% ungroup %>% select(movieId,title,genres,avg_rating) %>% filter(movieId %in% c(top_10_errors_5.0_asc$movieId))), caption = "List of Movies in Top 10 Accurate Predictions for Rating = 5.0", align = "c")

# Print userId, Mean and Standard Deviation for Top 10 users with the Smallest Mean errors
knitr::kable(user_mean_sd %>% filter(userId %in% c(top_10_errors_5.0_asc$userId)), caption = "Mean and SD for Users in the Top 10 Accurate Predictions for Rating = 5.0" , align = "c") 

# Extract the Ratings and Predictions for Top 10 Users with the Smallest Mean errors 
top_10_erros_5.0_users_asc <- predicted_ratings_final_errors %>% filter(userId %in% c(top_10_errors_5.0_asc$userId))  %>% select(userId,rating,pred)

# Extract the Ratings and Predictions for Movies rated by Top 10 Users with the Smallest Mean errors 
top_10_erros_5.0_movies_asc <- predicted_ratings_final_errors %>% filter(movieId %in% c(top_10_errors_5.0_asc$movieId))  %>% select(movieId,rating,pred)

# Print RMSE for  Top 10 Users with the Smallest Mean errors and the RMSE for Movies rated by these Users
knitr::kable(data.frame(round(rmse(top_10_erros_5.0_users_asc$rating, top_10_erros_5.0_users_asc$pred),digits = 5),round(rmse(top_10_erros_5.0_movies_asc$rating, top_10_erros_5.0_movies_asc$pred),digits = 5)), col.names = c("User", "Movie"), caption = "RMSE - Top 10 Accurate Predictions for Rating = 5.0", align = "c") 


## ----clean up and remove data that is no longer required, include=FALSE, echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------

rm(ratings_0.5,ratings_1.0,ratings_1.5,ratings_2.0,ratings_2.5,ratings_3.0,ratings_3.5,ratings_4.0,ratings_4.5,ratings_5.0, rmse_0.5,rmse_1.0,rmse_1.5,rmse_2.0,rmse_2.5,rmse_3.0,rmse_3.5,rmse_4.0,rmse_4.5,rmse_5.0)

rm(errors_0.5, errors_0.5_asc, rmse_error_summary, errors_1.0, errors_1.0_asc, errors_5.0, errors_5.0_asc)

rm(top_10_errors_0.5,top_10_errors_0.5_asc,top_10_erros_0.5_users,top_10_erros_0.5_users_asc)

rm(top_10_errors_1.0, top_10_errors_1.0_asc,top_10_erros_0.5_movies, top_10_erros_0.5_movies_asc, top_10_erros_1.0_movies, top_10_erros_1.0_movies_asc, top_10_erros_1.0_users, top_10_erros_1.0_users_asc, top_10_errors_5.0, top_10_errors_5.0_asc, top_10_erros_5.0_movies, top_10_erros_5.0_movies_asc, top_10_erros_5.0_users, top_10_erros_5.0_users_asc)

rm(user_mean_sd, predicted_ratings_final_errors, sigma, user_mean_sd, mu, mu_g)

rm(edx_cv_train_set,  edx_cv_test_set)

rm(genre_assignment, genre_assignment_test)

rm(top_10_users, top_rated_10)

gc()



## ----Model 1 - Prediction as mean of Ratings Final, include=TRUE, warning=FALSE, echo=TRUE-----------------------------------------------------------------------------------------------------------------------

# Compute mu which is the base for the rest of the models
mu_edx <- mean(edx$rating) 


## ----Model 5 -Regularisation for User Effects & Movie Effects Combined - Final, include=TRUE, warning=FALSE, echo=TRUE, message = FALSE--------------------------------------------------------------------------

# Compute Penalised values for Movie Effects and User Effects with Regularisation factor lambda_ij 
# Use lambda_ij computed during Analysis 

     beta_r_df_combined_edx <- edx %>% 
          mutate("userId" = as.factor(userId),"movieId" = as.factor(movieId)) %>% 
          group_by(movieId) %>%
          summarize(beta_ij = sum(rating - mu_edx)/(n()+ min_lambda_ij))
     alpha_r_df_combined_edx <- edx %>% 
          mutate("userId" = as.factor(userId),"movieId" = as.factor(movieId)) %>% 
          left_join(beta_r_df_combined_edx, by="movieId") %>%
          group_by(userId) %>%
          summarize(alpha_ij = sum(rating - beta_ij - mu_edx)/(n()+ min_lambda_ij))




## ----Model 6 - Regularised User & Movie Effects & Genre Effects Final, include=TRUE, warning=FALSE, echo=TRUE, message=FALSE-------------------------------------------------------------------------------------

# Extract Genre, Load from Disk if already available to avoid repeated computation. Saves time, effort and CPU cycles.
genre_assignment_file_edx <- "genre_assignment_edx.RData"

if(file.exists(genre_assignment_file_edx)){
      load(genre_assignment_file_edx)
      
      genre_assignment_edx <- genre_assignment_edx %>% 
      mutate(userId = as.factor(userId), movieId = as.factor(movieId))
      
      rm(genre_assignment_file_edx)
      
} else {
  
# Genre are all combined together in the "genres" column in a single string. Individual Genres within the string are separated using the pipe (|) symbol.
  
# Split genre individually for further Analysis. Use the "stringr" library and str_split function. 
  
# The pipe (|) symbol happens to have its own meaning in Regular Expressions so must be escaped using a double backslash.

# Assign genre individually to each movie based on the Split Strings. Each genre is tracked individually in its own column. Assign 1 if genre matches, 0 if it does not. Based on the code below, the "mutate" function from the "Tidyverse" Package will automatically create a Column if it does not exist already. If the Column exists, it will just update the corresponding row. Please note that the Column names for Genres Sci-Fi and Film-Noir have been changed to Sci_Fi and Film_Noir respectively. This is done so that it is easier to address the columns as R otherwise interprets the Symbol for the Hyphen (-) as the Minus/Subtraction Operator. 

# Add a genre "count" column for counting & tracking the number of genre attached to each movie, this will help us normalise the genre treatment effects values later.
  
genre_assignment_edx <- edx %>% mutate("individual_genre" = str_split(genres, "\\|")) %>%
        mutate (Action = ifelse (str_detect ( individual_genre, "Action"), 1 , 0 ) ) %>%
        mutate (Adventure = ifelse (str_detect ( individual_genre, "Adventure") , 1 , 0 ) ) %>% 
        mutate (Animation = ifelse (str_detect ( individual_genre, "Animation") , 1 , 0 ) ) %>% 
        mutate (Children = ifelse (str_detect ( individual_genre, "Children") , 1 , 0 ) ) %>% 
        mutate (Comedy = ifelse (str_detect ( individual_genre, "Comedy") , 1 , 0 ) ) %>% 
        mutate (Crime = ifelse (str_detect ( individual_genre, "Crime") , 1 , 0 ) ) %>% 
        mutate (Documentary = ifelse (str_detect ( individual_genre, "Documentary") , 1 , 0 ) ) %>% 
        mutate (Drama = ifelse (str_detect ( individual_genre, "Drama") , 1 , 0 ) ) %>% 
        mutate (Fantasy = ifelse (str_detect ( individual_genre, "Fantasy") , 1 , 0 ) ) %>% 
        mutate (Film_Noir = ifelse (str_detect ( individual_genre, "Film-Noir") , 1 , 0 ) ) %>% 
        mutate (Mystery = ifelse (str_detect ( individual_genre, "Mystery") , 1 , 0 ) ) %>% 
        mutate (Horror = ifelse (str_detect ( individual_genre, "Horror") , 1 , 0 ) ) %>% 
        mutate (Thriller = ifelse (str_detect ( individual_genre, "Thriller") , 1 , 0 ) ) %>% 
        mutate (Musical = ifelse (str_detect ( individual_genre, "Musical") , 1 , 0 ) ) %>% 
        mutate (Sci_Fi = ifelse (str_detect ( individual_genre, "Sci-Fi") , 1 , 0 ) ) %>% 
        mutate (Romance = ifelse (str_detect ( individual_genre, "Romance") , 1 , 0 ) ) %>% 
        mutate (War = ifelse (str_detect ( individual_genre, "War") , 1 , 0 ) ) %>% 
        mutate (Western = ifelse (str_detect ( individual_genre, "Western") , 1 , 0 ) ) %>% 
        mutate("genre_count" = Action + Adventure + Animation + Children + Comedy + Crime + Documentary + Drama + Fantasy + Film_Noir + Mystery + Horror + Thriller + Musical + Sci_Fi + Romance + War + Western ) %>% 
        mutate("userId" = as.factor(userId), "movieId" = as.factor(movieId))

# Save file to disk for quick retrieval later if required

save(genre_assignment_edx, file = "genre_assignment_edx.RData")
}


# Create Genre Assignment Matrix. contains only numerical values for ease of processing 

genre_assignment_matrix_edx <- genre_assignment_edx %>% 
  group_by(userId) %>% 
  select (userId, movieId, rating, all_of(genre_list), genre_count) 

# Use a temp file to collect and store extrapolated ratings for the genre
temp <- genre_assignment_matrix_edx %>% 
        ungroup %>% select(rating, all_of(genre_list)) %>% 
        mutate ( Action = ifelse ( Action == 1 , rating , 0 ) ) %>% 
        mutate ( Adventure = ifelse ( Adventure == 1 , rating , 0 ) ) %>% 
        mutate ( Animation = ifelse ( Animation == 1 , rating , 0 ) ) %>% 
        mutate ( Children = ifelse ( Children == 1 , rating , 0 ) ) %>% 
        mutate ( Comedy = ifelse ( Comedy == 1 , rating , 0 ) ) %>% 
        mutate ( Crime = ifelse ( Crime == 1 , rating , 0 ) ) %>% 
        mutate ( Documentary = ifelse ( Documentary == 1 , rating , 0 ) ) %>% 
        mutate ( Drama = ifelse ( Drama == 1 , rating , 0 ) ) %>% 
        mutate ( Fantasy = ifelse ( Fantasy == 1 , rating , 0 ) ) %>% 
        mutate ( Film_Noir = ifelse ( Film_Noir == 1 , rating , 0 ) ) %>% 
        mutate ( Mystery = ifelse ( Mystery == 1 , rating , 0 ) ) %>% 
        mutate ( Horror = ifelse ( Horror == 1 , rating , 0 ) ) %>% 
        mutate ( Thriller = ifelse ( Thriller == 1 , rating , 0 ) ) %>% 
        mutate ( Musical = ifelse ( Musical == 1 , rating , 0 ) ) %>% 
        mutate ( Sci_Fi = ifelse ( Sci_Fi == 1 , rating , 0 ) ) %>% 
        mutate ( Romance = ifelse ( Romance == 1 , rating , 0 ) ) %>% 
        mutate ( War = ifelse ( War == 1 , rating , 0 ) ) %>% 
        mutate ( Western = ifelse ( Western == 1 , rating , 0 )) %>% 
        select(  all_of(genre_list))

# compute individual sums

individual_genre_sums_edx <- colSums(temp) 

# compute individual counts

individual_genre_count_edx <- colSums(temp != 0)

# Compute Overall Genre Mean

mu_g_edx <- sum(individual_genre_sums_edx)/sum(individual_genre_count_edx)


# compute Individual Genre Mean and Difference from Overall Genre Mean and Store in a common Table, 

genre_summary_edx <- data.frame(individual_genre_sums_edx,individual_genre_count_edx) 

genre_summary_edx <- genre_summary_edx %>% 
      mutate(individual_genre_mean_edx = (individual_genre_sums_edx/individual_genre_count_edx), difference_from_genre_mean = (individual_genre_mean_edx - mu_g_edx)) 

# remove Temp file 
rm(temp)

# To compare our Predictions with the Final Holdout Test Set Data. Repeat operations performed on 'edx' dataset for 'final_holdout_test' dataset. Load Data Set from Disk if already available to save on time and computing efforts.

genre_assignment_test_file_final <- "genre_assignment_test_final.RData"

if(file.exists(genre_assignment_test_file_final)){
      load(genre_assignment_test_file_final)
      genre_assignment_test_file_final <- genre_assignment_test_final %>% 
      mutate(userId = as.factor(userId), movieId = as.factor(movieId))
    rm(genre_assignment_test_file_final)
} else {
  
genre_assignment_test_final <- final_holdout_test %>% 
        mutate("individual_genre" = str_split(genres, "\\|"))%>% 
        mutate (Action = ifelse (str_detect ( individual_genre, "Action") , 1 , 0 ) ) %>% 
        mutate (Adventure = ifelse (str_detect ( individual_genre, "Adventure") , 1 , 0 ) ) %>% 
        mutate (Animation = ifelse (str_detect ( individual_genre, "Animation") , 1 , 0 ) ) %>% 
        mutate (Children = ifelse (str_detect ( individual_genre, "Children") , 1 , 0 ) ) %>% 
        mutate (Comedy = ifelse (str_detect ( individual_genre, "Comedy") , 1 , 0 ) ) %>% 
        mutate (Crime = ifelse (str_detect ( individual_genre, "Crime") , 1 , 0 ) ) %>% 
        mutate (Documentary = ifelse (str_detect ( individual_genre, "Documentary") , 1 , 0 ) ) %>%
        mutate (Drama = ifelse (str_detect ( individual_genre, "Drama") , 1 , 0 ) ) %>% 
        mutate (Fantasy = ifelse (str_detect ( individual_genre, "Fantasy") , 1 , 0 ) ) %>% 
        mutate (Film_Noir = ifelse (str_detect ( individual_genre, "Film-Noir") , 1 , 0 ) ) %>% 
        mutate (Mystery = ifelse (str_detect ( individual_genre, "Mystery") , 1 , 0 ) ) %>%
        mutate (Horror = ifelse (str_detect ( individual_genre, "Horror") , 1 , 0 ) ) %>% 
        mutate (Thriller = ifelse (str_detect ( individual_genre, "Thriller") , 1 , 0 ) ) %>% 
        mutate (Musical = ifelse (str_detect ( individual_genre, "Musical") , 1 , 0 ) ) %>% 
        mutate (Sci_Fi = ifelse (str_detect ( individual_genre, "Sci-Fi") , 1 , 0 ) ) %>% 
        mutate (Romance = ifelse (str_detect ( individual_genre, "Romance") , 1 , 0 ) ) %>% 
        mutate (War = ifelse (str_detect ( individual_genre, "War") , 1 , 0 ) ) %>% 
        mutate (Western = ifelse (str_detect ( individual_genre, "Western") , 1 , 0 ) ) %>% 
        mutate("genre_count" = Action + Adventure + Animation + Children + Comedy + Crime + Documentary + Drama + Fantasy + Film_Noir + Mystery + Horror + Thriller + Musical + Sci_Fi + Romance + War + Western )

# Save file to disk for quick retrieval later if required
save(genre_assignment_test_final, file="genre_assignment_test_final.RData")
}

# Remove variables used to hold filenames
rm(genre_assignment_file_edx, genre_assignment_test_file_final)


## ----Model 6a - Genre User Effects - Final, include=TRUE, warning=FALSE, message = FALSE, echo=TRUE--------------------------------------------------------------------------------------------------------------


# Compute Genre User Effects for each Genre

genre_action_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_action" = sum(Action == 1), "sum_action" = sum((ifelse(Action==0,0,rating)))  , "mean_action" = sum_action/(ifelse(count_action==0,1,count_action)), "b_action"  = ifelse(sum_action==0,0,(mean_action - mean_rating ))) %>% select(userId, mean_action, b_action)

genre_adventure_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_adventure" = sum(Adventure == 1), "sum_adventure" = sum((ifelse(Adventure==0,0,rating)))  , "mean_adventure" = sum_adventure/(ifelse(count_adventure==0,1,count_adventure)), "b_adventure"  = ifelse(sum_adventure==0,0,(mean_adventure - mean_rating ))) %>% select(userId, mean_adventure, b_adventure)

genre_animation_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_animation" = sum(Animation == 1), "sum_animation" = sum((ifelse(Animation==0,0,rating)))  , "mean_animation" = sum_animation/(ifelse(count_animation==0,1,count_animation)), "b_animation"  = ifelse(sum_animation==0,0,(mean_animation - mean_rating  ))) %>% select(userId, mean_animation, b_animation)

genre_children_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_children" = sum(Children == 1), "sum_children" = sum((ifelse(Children==0,0,rating)))  , "mean_children" = sum_children/(ifelse(count_children==0,1,count_children)), "b_children"  = ifelse(sum_children==0,0,(mean_children - mean_rating ))) %>% select(userId, mean_children, b_children)

genre_comedy_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_comedy" = sum(Comedy == 1), "sum_comedy" = sum((ifelse(Comedy==0,0,rating)))  , "mean_comedy" = sum_comedy/(ifelse(count_comedy==0,1,count_comedy)), "b_comedy"  = ifelse(sum_comedy==0,0,(mean_comedy - mean_rating ))) %>% select(userId, mean_comedy, b_comedy)

genre_crime_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_crime" = sum(Crime == 1), "sum_crime" = sum((ifelse(Crime==0,0,rating)))  , "mean_crime" = sum_crime/(ifelse(count_crime==0,1,count_crime)), "b_crime"  = ifelse(sum_crime==0,0,(mean_crime - mean_rating  ))) %>% select(userId, mean_crime, b_crime)

genre_documentary_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating) , "count_documentary" = sum(Documentary == 1), "sum_documentary" = sum((ifelse(Documentary==0,0,rating)))  , "mean_documentary" = sum_documentary/(ifelse(count_documentary==0,1,count_documentary)), "b_documentary"  = ifelse(sum_documentary==0,0,(mean_documentary - mean_rating  ))) %>% select(userId, mean_documentary, b_documentary)

genre_drama_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating), "count_drama" = sum(Drama == 1), "sum_drama" = sum((ifelse(Drama==0,0,rating)))  , "mean_drama" = sum_drama/(ifelse(count_drama==0,1,count_drama)), "b_drama"  = ifelse(sum_drama==0,0,(mean_drama - mean_rating ))) %>% select(userId, mean_drama, b_drama)

genre_fantasy_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_fantasy" = sum(Fantasy == 1), "sum_fantasy" = sum((ifelse(Fantasy==0,0,rating)))  , "mean_fantasy" = sum_fantasy/(ifelse(count_fantasy==0,1,count_fantasy)), "b_fantasy"  = ifelse(sum_fantasy==0,0,(mean_fantasy - mean_rating ))) %>% select(userId, mean_fantasy, b_fantasy)

genre_film_noir_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_film_noir" = sum(Film_Noir == 1), "sum_film_noir" = sum((ifelse(Film_Noir==0,0,rating)))  , "mean_film_noir" = sum_film_noir/(ifelse(count_film_noir==0,1,count_film_noir)), "b_film_noir"  = ifelse(sum_film_noir==0,0,(mean_film_noir - mean_rating ))) %>% select(userId, mean_film_noir, b_film_noir)

genre_mystery_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating) , "count_mystery" = sum(Mystery == 1), "sum_mystery" = sum((ifelse(Mystery==0,0,rating)))  , "mean_mystery" = sum_mystery/(ifelse(count_mystery==0,1,count_mystery)), "b_mystery"  = ifelse(sum_mystery==0,0,(mean_mystery - mean_rating  ))) %>% select(userId, mean_mystery, b_mystery)

genre_horror_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating) , "count_horror" = sum(Horror == 1), "sum_horror" = sum((ifelse(Horror==0,0,rating)))  , "mean_horror" = sum_horror/(ifelse(count_horror==0,1,count_horror)), "b_horror"  = ifelse(sum_horror==0,0,(mean_horror - mean_rating ))) %>% select(userId, mean_horror, b_horror)

genre_thriller_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_thriller" = sum(Thriller == 1), "sum_thriller" = sum((ifelse(Thriller==0,0,rating)))  , "mean_thriller" = sum_thriller/(ifelse(count_thriller==0,1,count_thriller)), "b_thriller"  = ifelse(sum_thriller==0,0,(mean_thriller - mean_rating ))) %>% select(userId, mean_thriller, b_thriller)

genre_musical_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_musical" = sum(Musical == 1), "sum_musical" = sum((ifelse(Musical==0,0,rating)))  , "mean_musical" = sum_musical/(ifelse(count_musical==0,1,count_musical)), "b_musical"  = ifelse(sum_musical==0,0,(mean_musical - mean_rating ))) %>% select(userId, mean_musical, b_musical)

genre_sci_fi_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_sci_fi" = sum(Sci_Fi == 1), "sum_sci_fi" = sum((ifelse(Sci_Fi==0,0,rating)))  , "mean_sci_fi" = sum_sci_fi/(ifelse(count_sci_fi==0,1,count_sci_fi)), "b_sci_fi"  = ifelse(sum_sci_fi==0,0,(mean_sci_fi - mean_rating   ))) %>% select(userId, mean_sci_fi, b_sci_fi)

genre_romance_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_romance" = sum(Romance == 1), "sum_romance" = sum((ifelse(Romance==0,0,rating)))  , "mean_romance" = sum_romance/(ifelse(count_romance==0,1,count_romance)), "b_romance"  = ifelse(sum_romance==0,0,(mean_romance - mean_rating  ))) %>% select(userId, mean_romance, b_romance)

genre_war_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating) , "count_war" = sum(War == 1), "sum_war" = sum((ifelse(War==0,0,rating)))  , "mean_war" = sum_war/(ifelse(count_war==0,1,count_war)), "b_war"  = ifelse(sum_war==0,0,(mean_war - mean_rating ))) %>% select(userId, mean_war, b_war)

genre_western_edx <- genre_assignment_matrix_edx %>% group_by(userId) %>% summarise ("mean_rating" = mean(rating)  , "count_western" = sum(Western == 1), "sum_western" = sum((ifelse(Western==0,0,rating)))  , "mean_western" = sum_western/(ifelse(count_western==0,1,count_western)), "b_western"  = ifelse(sum_western==0,0,(mean_western - mean_rating   ))) %>% select(userId, mean_western, b_western)

# Create consolidated Genre User Effects dataframe

genre_user_summary_edx <- genre_drama_edx %>% 
  left_join(genre_comedy_edx, by='userId') %>%
  left_join(genre_action_edx, by='userId') %>%
  left_join(genre_thriller_edx, by='userId') %>%
  left_join(genre_adventure_edx, by='userId') %>%
  left_join(genre_romance_edx, by='userId') %>%
  left_join(genre_sci_fi_edx, by='userId') %>%
  left_join(genre_crime_edx, by='userId') %>%
  left_join(genre_fantasy_edx, by='userId') %>%
  left_join(genre_children_edx, by='userId') %>%
  left_join(genre_horror_edx, by='userId') %>%
  left_join(genre_mystery_edx, by='userId') %>%
  left_join(genre_war_edx, by='userId') %>%
  left_join(genre_animation_edx, by='userId') %>%
  left_join(genre_musical_edx, by='userId') %>%
  left_join(genre_western_edx, by='userId') %>%
  left_join(genre_film_noir_edx, by='userId') %>%
  left_join(genre_documentary_edx, by='userId') 

# Extract all Genre User Effects values for all Users
genre_effects_user_edx <- genre_user_summary_edx %>% select(userId, b_action, b_adventure, b_animation, b_children, b_comedy, b_crime, b_documentary, b_drama, b_fantasy, b_film_noir, b_horror, b_musical, b_mystery, b_romance, b_sci_fi, b_thriller, b_war, b_western)


# Rmeove Individual Genre Tables to Save Memory and avoid Confusion
rm(genre_action_edx,genre_adventure_edx,genre_animation_edx, genre_children_edx, genre_comedy_edx, genre_crime_edx, genre_documentary_edx, genre_drama_edx, genre_fantasy_edx, genre_film_noir_edx, genre_horror_edx, genre_musical_edx, genre_mystery_edx, genre_romance_edx, genre_sci_fi_edx, genre_thriller_edx, genre_war_edx, genre_western_edx)



## ----Model 7 Final - Regularised User, Regularised Movie, Regularised Genre, Time Effects, include=TRUE, echo=TRUE, warning=FALSE, message=FALSE, out.width="75%"------------------------------------------------

# Compute Time Effects for Movies for edx

time_assignment_movie_edx <- edx %>% mutate(movieId = as.factor(movieId)) %>% 
  mutate(date = as_datetime(timestamp)) %>% 
  mutate(date_year_j = year(date)) %>%  select(movieId,rating, date_year_j) %>% 
  group_by(movieId, date_year_j) %>% 
  summarise(mean_yearly_rating = mean(rating, na.rm = TRUE), .groups = "drop")

time_assignment_n_years_movie_edx <- time_assignment_movie_edx %>% 
  group_by(movieId) %>% 
  select(movieId, date_year_j) %>% 
  summarise(movieId, date_year_j, n_years_j = n(), .groups = "drop")

# Extract Unique MovieId 
movie_map_edx <- edx %>% 
  select(movieId) %>% 
  group_by(movieId) %>% summarise(movieId= unique(movieId), .groups= "drop")

# Compute Average Rating for each movie along with number of ratings for edx
movie_rating_edx <- edx %>% group_by(movieId) %>% 
  summarise(avg_rating = mean(rating), number_of_ratings = n()) 

# Combine above information into existing Movie Map
movie_list_edx <- left_join(movie_map_edx, movie_rating_edx,by="movieId")

# remove datasets that are no longer needed
rm(movie_rating_edx, movie_map_edx)

time_effects_movie_edx <- movie_list_edx %>% 
  ungroup %>% mutate(movieId = as.factor(movieId)) %>% 
  select(movieId, avg_rating) %>%   
  left_join(time_assignment_movie_edx, by = "movieId") %>%
  mutate("beta_jt" = (mean_yearly_rating - avg_rating)) %>%
  left_join(time_assignment_n_years_movie_edx, by = join_by(movieId,date_year_j)) %>%
  select(movieId, date_year_j, beta_jt, n_years_j)

# Compute Time Effect for Users

time_assignment_user_edx <- edx %>% 
  mutate(userId = as.factor(userId)) %>% 
  mutate(date = as_datetime(timestamp)) %>% 
  mutate(date_year_i = year(date)) %>%  select(userId,rating,date_year_i) %>% 
  group_by(userId, date_year_i) %>% 
  summarise(mean_yearly_rating = mean(rating, na.rm=TRUE), .groups = "drop")

time_assignment_n_years_user_edx <- time_assignment_user_edx %>% 
  group_by(userId) %>% 
  select(userId, date_year_i) %>% 
  summarise(userId, date_year_i, n_years_i = n(), .groups = "drop")

time_effects_user_edx <- edx %>% mutate(userId = as.factor(userId)) %>% 
  group_by(userId) %>% summarise(avg_rating = mean(rating)) %>% 
  select(userId, avg_rating) %>%
  left_join(time_assignment_user_edx, by = "userId") %>%
  mutate("beta_it" = (mean_yearly_rating - avg_rating)) %>%
  left_join(time_assignment_n_years_user_edx, by = join_by(userId,date_year_i)) %>%
  select(userId, date_year_i, beta_it, n_years_i)

# Let us prepare the test set for input of time effects. We will reuse the existing dataset to avoid rework 

genre_assignment_test_final <- genre_assignment_test_final %>% 
  mutate(date = as_datetime(timestamp)) %>% 
  mutate(date_year_i = year(date)) %>% 
  mutate(date_year_j = year(date)) 

rm(time_assignment_edx, time_assignment_n_years_edx)


## ----Model 8 - Regularised User, Regularised Movie, Regularised Genre, Time Effects Final - Discrete, include=TRUE, echo=FALSE, warning=FALSE, message=FALSE, out.width="75%"------------------------------------

predicted_ratings_rounded_edx <- genre_assignment_test_final %>% 
    mutate(userId = as.factor(userId), movieId = as.factor(movieId)) %>%
    left_join(alpha_r_df_combined_edx, by='userId') %>%
    left_join(beta_r_df_combined_edx, by='movieId') %>%
    left_join(genre_effects_user_edx, by='userId') %>%
    left_join(time_effects_movie_edx, by= join_by (movieId, date_year_j)) %>% 
    left_join(time_effects_user_edx, by= join_by (userId, date_year_i)) %>% 
    mutate_all(list(~replace_na(., 0))) %>% 
    mutate(beta_ig =  (b_drama*Drama + b_comedy*Comedy + b_action*Action + b_thriller*Thriller + b_adventure*Adventure + b_romance*Romance + b_sci_fi*Sci_Fi + b_crime*Crime + b_fantasy*Fantasy + b_children*Children + b_horror*Horror + b_mystery*Mystery + b_war*War + b_animation*Animation + b_musical*Musical + b_western*Western + b_film_noir*Film_Noir + b_documentary*Documentary)/((ifelse(genre_count==0,1,genre_count))+ min_lambda_ig )) %>%  
      mutate("pred" = clamp(mu_edx + alpha_ij + beta_ij + beta_ig + beta_jt/(0.0001 + sqrt(n_years_j)) + beta_it/(0.0001 + sqrt(n_years_i)) )) %>% 
        mutate(pred = signif(pred/5, digits = 1)) %>% 
        mutate(pred = (pred*5)) %>% select(userId,movieId,rating,pred)

rmse_rounded_edx <- round(rmse(predicted_ratings_rounded_edx$rating, predicted_ratings_rounded_edx$pred), digits = 5)

print( c(" RMSE with Predictions Rounded to Discrete Values: ", round(rmse_rounded_edx,digits = 5)), quote = FALSE, justify = "left") 


# Remove Datasets that are no longer needed
rm(predicted_ratings_rounded_edx)



## ----Linear Models - Final Best RMSE, echo=FALSE, include=TRUE, warning=FALSE, message=FALSE---------------------------------------------------------------------------------------------------------------------

print( c(" The Best RMSE is: ", round(rmse_rounded_edx ,digits = 5)), quote = FALSE, justify = "left") 


## ----Final cleanup, echo=FALSE, include=FALSE, warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------------------------------------------

# Clean up data and run garbage collector
rm(alpha_r_df_edx, beta_r_df_edx, edx, final_holdout_test, genre_assignment_edx, genre_assignment_matrix_edx, genre_assignment_test_final, genre_effects_user_edx, genre_effects_movie_edx, genre_user_summary_edx,genre_user_mean_edx, genre_user_mean_edx, genre_summary_edx, movie_list, rmse_consolidated_final, time_assignment_movie_edx,time_assignment_user_edx, time_assignment_n_years_movie_edx,time_assignment_n_years_user_edx ,time_effects_movie_edx,time_effects_user_edx, user_effects_edx, movie_effects_edx, movie_list_edx, alpha_r_df_combined_edx, beta_r_df_combined_edx, mu_g_edx)

rm(predicted_ratings_genre_movie_edx, predicted_ratings_genre_user_edx, predicted_ratings_time_edx)

rm(genre_list, individual_genre_count_edx, individual_genre_sums_edx, movies_file, mu_edx, overall_genre_mean_edx, ratings_file, rmse_rounded_edx) 

rm(min_lambda_ig, min_lambda_jg, min_lambda_i, min_lambda_i2, min_lambda_j, min_lambda_j2, min_lambda_j3, min_lambda_ij)

rm(clamp, rmse, rmses_genre_function_edx, rmses_time_edx_fn, rmses_genre_function_movie_edx, rmses_genre_function_user_edx, rmses_time_function_user_edx)


gc()




## ----Project End Marker, include=TRUE, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------

##########################################################
# END MOVIELENS PROJECT
##########################################################

