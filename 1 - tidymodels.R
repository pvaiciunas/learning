# https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/

library(tidymodels)

# This collection of packages (which subsumes the tidyverse) focuses on three things
# 1. PRe-processing
# 2. Training
# 3. Validation

# Iris will be the example dataset


# Preprocessing -----------------------------------------------------------

# split the iris dataset into training and testing.
iris_split <- initial_split(iris, prop = 0.6)
iris_split

# Examine the training data. The test data can also be viewed with testing()
iris_split %>% 
  training() %>% 
  glimpse

# the recipes() function works like ggplot and can be applied to the training data
# the prep() function executes the recipe transformations
# 
# The recipe() function takes in 'step_' functions. These can be applied across 
# all predictors and can ignore outcomes with existings functions
iris_recipe <- training(iris_split) %>% 
  recipe(Species ~ .) %>% 
  step_corr(all_predictors()) %>% 
  step_center(all_predictors(), -all_outcomes()) %>% 
  step_scale(all_predictors(), -all_outcomes()) %>% 
  prep()

iris_recipe

# You can now use the exact same recipe on the testing data using the recipe
# object. Note that you are now calling testing() here instead of training()
# Use the bake() function for the application of the recipe and so that the 
# resulting object is the actual data
iris_testing <- iris_recipe %>% 
  bake(testing(iris_split))

glimpse(iris_testing)

# to extract the underling data from the initial recipe, you could use bake(), 
# but this is redundant since we've already applied the transformation to the 
# data earlier. Instead you can use juice() on teh recipe object and assign
# it to a variable
iris_training <- juice(iris_recipe)

glimpse(iris_training)


# Model Training ----------------------------------------------------------

# The tidymodels package is an evolution of caret I think. So it tries to create
# a single set of functions and tools that stay constant across different models.
# In this exmpale, we use the 'ranger' packages implementation of a random forest
# model. The set_engine() function lets us cahnge implementations.
iris_ranger <- rand_forest(trees = 100, mode = "classification") %>% 
  set_engine("ranger") %>% 
  fit(Species ~ ., data = iris_training)

# to change the random forest implementation to the one used by the randomForest
# package, we just make that change inside set_engine
iris_rf <- rand_forest(trees = 100, mode = "classification") %>% 
  set_engine("randomForest") %>% 
  fit(Species ~ ., data = iris_training)

# the predict function returns a tibble, with .pred_class as the heading name
predict(iris_ranger, iris_testing)

# You can combine the predictions with the baked testing dataframe very easily
iris_ranger %>% 
  predict(iris_testing) %>% 
  bind_cols(iris_testing) %>% 
  glimpse()



# Validation --------------------------------------------------------------

# The metrics() function helps measure teh performance of the model. It'll 
# automatically choose the appropriate metrics given the type of model. It expects
# a tibble, with the 'truth' and 'estimate' defined
iris_ranger %>% 
  predict(iris_testing) %>% 
  bind_cols(iris_testing) %>% 
  metrics(truth = Species, estimate = .pred_class)

# And you can keep this code teh same and measure it against the rf model
iris_rf %>% 
  predict(iris_testing) %>% 
  bind_cols(iris_testing) %>% 
  metrics(truth = Species, estimate = .pred_class)

# Also, really easy to obtain probablity of each class's prediction.
# change the type argument to 'prob' and you end with a tibble with a variable
# for each class, and the values are their probabilities
iris_ranger %>% 
  predict(iris_testing, type = "prob") %>% 
  glimpse()

# You can then append this to the testing df, which can then be used for further graphing
# (see further down)
iris_probs <- iris_ranger %>%
  predict(iris_testing, type = "prob") %>%
  bind_cols(iris_testing)

# gain_curves can be calcd with this new object
iris_probs %>% 
  gain_curve(Species, .pred_setosa:.pred_virginica) %>% 
  glimpse()

# We can use autoplot() to make an automatic plot of the stuff
iris_probs %>% 
  gain_curve(Species, .pred_setosa:.pred_virginica) %>% 
  autoplot()

# Instead of a gain_curve, we can also calc roc_curves just as easily
iris_probs %>% 
  roc_curve(Species, .pred_setosa:.pred_virginica) %>% 
  autoplot()

# You can also easily create a df with the predicted value, as well as the 
# probabilities of each class
predict(iris_ranger, iris_testing, type = "prob") %>% 
  bind_cols(predict(iris_ranger, iris_testing)) %>% 
  bind_cols(select(iris_testing, Species)) %>% 
  glimpse()

# You can pipe this table into metrics() to get some perf stats
predict(iris_ranger, iris_testing, type = "prob") %>% 
  bind_cols(predict(iris_ranger, iris_testing)) %>% 
  bind_cols(select(iris_testing, Species)) %>% 
  metrics(Species, .pred_setosa:.pred_virginica, estimate = .pred_class)
