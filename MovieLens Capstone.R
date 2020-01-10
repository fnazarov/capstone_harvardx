################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1)
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]

temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#Remove unnessesary data to unload the memory
rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Create a new test and training set from edx set that we have already created
set.seed(1)
test_index_edx <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
test_set <- edx[ test_index_edx, ]
train_set <- edx[ -test_index_edx, ]

#Make sure we do not include movies and users in the test set that do not appear in training set
test_set <- test_set%>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#In order to detect premier year in title column in  all sets we use regex two times 
#First we have to detect all the year in brackets and from brackets should get premier 3years 
pattern_1 <- "\\(\\d{4}"
pattern_2 <- "\\d{4}"

# Detect Premier years for training set and write into seperate column
strings <- regmatches(train_set$title, regexpr(pattern_1, train_set$title))
premier_year <- as.numeric(regmatches(strings, regexpr(pattern_2, strings)))

train_set <- train_set %>% mutate(premier_year = premier_year, 
                                  age_of_movie = max(premier_year) - premier_year)

# Detect Premier years for test set and write into seperate column
strings <- regmatches(test_set$title, regexpr(pattern_1, test_set$title))
premier_year <- as.numeric(regmatches(strings, regexpr(pattern_2, strings)))

test_set <- test_set %>% mutate(premier_year = premier_year, 
                                age_of_movie = max(premier_year) - premier_year)


#We should find the best lambda that minimize the RMSE. So we implement cross validation method

lambdas <- seq(4.5, 5, 0.1)
rmses <- sapply(lambdas, function(l) {
 
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>%
  group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n() + l))
  
  b_t <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(premier_year) %>%
    summarize(b_t = sum(rating - mu - b_i - b_u)/(n() + l))
  
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = "premier_year") %>%
    mutate(pred = mu + b_i + b_u + b_t) %>%
    pull(pred)
  
 return(RMSE(predicted_ratings, test_set$rating))
  
})
min(rmses)
qplot(lambdas, rmses)
lambdas[which.min(rmses)]

