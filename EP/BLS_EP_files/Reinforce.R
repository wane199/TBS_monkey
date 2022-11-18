# [Reinforcement Learning in R](https://cran.r-project.org/web/packages/ReinforcementLearning/vignettes/ReinforcementLearning.html)
devtools::install_github("nproellochs/ReinforcementLearning")
# Package loading
library(ReinforcementLearning)

# Learning from pre-defined observations
data("tictactoe")
head(tictactoe, 5)

# Dynamic learning from an interactive environment function
environment <- function(state, action) {
  ...
  return(list("NextState" = newState,
              "Reward" = reward))
}

# Define state and action sets
states <- c("s1", "s2", "s3", "s4")
actions <- c("up", "down", "left", "right")

env <- gridworldEnvironment

# Sample N = 1000 random sequences from the environment
data <- sampleExperience(N = 1000, 
                         env = env, 
                         states = states, 
                         actions = actions)

# Learning phase
# General setup
# Load dataset
data("tictactoe")

# Perform reinforcement learning
model <- ReinforcementLearning(data = tictactoe, 
                               s = "State", 
                               a = "Action", 
                               r = "Reward", 
                               s_new = "NextState", 
                               iter = 1)
# Parameter configuration
# Define control object
control <- list(alpha = 0.1, gamma = 0.1, epsilon = 0.1)

# Pass learning parameters to reinforcement learning function
model <- ReinforcementLearning(data, iter = 10, control = control)

# Diagnostics
# Print policy
computePolicy(model)

# Print state-action table
print(model)

# Print summary statistics
summary(model)


####################
# Working example 1: Gridworld
# Problem definition

# Defining an environment function
# Define state and action sets
states <- c("s1", "s2", "s3", "s4")
actions <- c("up", "down", "left", "right")
# Load built-in environment function for 2x2 gridworld 
env <- gridworldEnvironment
print(env)

# Learning an optimal policy
# Sample N = 1000 random sequences from the environment
data <- sampleExperience(N = 1000, 
                         env = env, 
                         states = states, 
                         actions = actions)
head(data)

# Define reinforcement learning parameters
control <- list(alpha = 0.1, gamma = 0.5, epsilon = 0.1)

# Perform reinforcement learning
model <- ReinforcementLearning(data, 
                               s = "State", 
                               a = "Action", 
                               r = "Reward", 
                               s_new = "NextState", 
                               control = control)

# Evaluating policy learning
# Print policy
computePolicy(model)

# Print state-action function
print(model)

# Print summary statistics
summary(model)

# Applying a policy to unseen data
# Example data
data_unseen <- data.frame(State = c("s1", "s2", "s1"), 
                          stringsAsFactors = FALSE)

# Pick optimal action
data_unseen$OptimalAction <- predict(model, data_unseen$State)

data_unseen

# Updating an existing policy
# Sample N = 1000 sequences from the environment
# using epsilon-greedy action selection
data_new <- sampleExperience(N = 1000, 
                             env = env, 
                             states = states, 
                             actions = actions, 
                             actionSelection = "epsilon-greedy",
                             model = model, 
                             control = control)

# Update the existing policy using new training data
model_new <- ReinforcementLearning(data_new, 
                                   s = "State", 
                                   a = "Action", 
                                   r = "Reward", 
                                   s_new = "NextState", 
                                   control = control,
                                   model = model)

# Print result
print(model_new)

# Plot reinforcement learning curve
plot(model_new)

###########################
# Working example 2: Tic-Tac-Toe
# Problem definition
# Load dataset
data("tictactoe")

# Define reinforcement learning parameters
control <- list(alpha = 0.2, gamma = 0.4, epsilon = 0.1)

# Perform reinforcement learning
model <- ReinforcementLearning(tictactoe, s = "State", a = "Action", r = "Reward", 
                               s_new = "NextState", iter = 1, control = control)

# Calculate optimal policy
pol <- computePolicy(model)

# Print policy
head(pol)



##########################################
# Estimation and comparison of dynamic treatment regimes (DTRs) from sequentially randomized clinical trials
data("PHdata")
f <- PHfit(data=PHdata, covar="V")
summary(f)
plot(f)

est <- WRSEestimate(data=PHdata)
plot(est)







