# Load required packages
library(deSolve)
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define the vector field (differential equations)
vector_field <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dx <- a * sin(y) + b * x * (1 - x^2 - y^2)
    dy <- c * cos(x) + d * y * (1 - x^2 - y^2)
    list(c(dx, dy))
  })
}

# Parameters for the differential equations
parameters <- c(a = 1.5, b = 0.5, c = 1.2, d = 0.3)

# Time sequence for integration
times <- seq(0, 20, by = 0.01)

# Generate multiple initial conditions in a grid
n_points <- 20
x_init <- seq(-2, 2, length.out = n_points)
y_init <- seq(-2, 2, length.out = n_points)
initial_conditions <- expand.grid(x = x_init, y = y_init)

# Function to solve ODE for a single initial condition
solve_trajectory <- function(init, times, parameters) {
  state <- c(x = init$x, y = init$y)
  out <- ode(y = state, times = times, func = vector_field, parms = parameters)
  as.data.frame(out) %>% mutate(id = paste(init$x, init$y, sep = "_"))
}

# Solve ODE for all initial conditions
trajectories <- lapply(1:nrow(initial_conditions), function(i) {
  solve_trajectory(initial_conditions[i, ], times, parameters)
}) %>% bind_rows()

# Create the plot
ggplot(trajectories, aes(x = x, y = y, group = id)) +
  geom_path(aes(color = id), alpha = 0.4, size = 0.3) +
  scale_color_viridis_d(option = "plasma") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black", color = NA)) +
  coord_equal()

#########

# Load required packages
library(deSolve)
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define the vector field (differential equations)
vector_field <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dx <- a * sin(y) + b * x * (1 - x^2 - y^2)
    dy <- c * cos(x) + d * y * (1 - x^2 - y^2)
    list(c(dx, dy))
  })
}

# Parameters for the differential equations
parameters <- c(a = 1.5, b = 0.5, c = 1.2, d = 0.3)

# Time sequence for integration
times <- seq(0, 20, by = 0.01)

# Generate multiple initial conditions in a grid
n_points <- 20
x_init <- seq(-2, 2, length.out = n_points)
y_init <- seq(-2, 2, length.out = n_points)
initial_conditions <- expand.grid(x = x_init, y = y_init)

# Function to solve ODE for a single initial condition
solve_trajectory <- function(init, times, parameters) {
  state <- c(x = init$x, y = init$y)
  out <- ode(y = state, times = times, func = vector_field, parms = parameters)
  as.data.frame(out) %>% mutate(id = paste(init$x, init$y, sep = "_"))
}

# Solve ODE for all initial conditions
trajectories <- lapply(1:nrow(initial_conditions), function(i) {
  solve_trajectory(initial_conditions[i, ], times, parameters)
}) %>% bind_rows()

# Create the plot with varying line thickness
 ggplot(trajectories, aes(x = x, y = y, group = id)) +
  geom_path(aes(color = id, size = sin(time * 2) + 1.5), alpha = 0.4) +
  scale_color_viridis_d(option = "plasma") +
  scale_size_identity() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black", color = NA)) +
  coord_equal()




 #########
 

library(deSolve)
library(ggplot2)
library(dplyr)

# Define a nonlinear vector field as a differential equation system
vector_field <- function(t, state, parameters) {
  with(as.list(state), {
    dx <- sin(y) - 0.5 * x
    dy <- cos(x) - 0.5 * y
    list(c(dx, dy))
  })
}

# Generate random initial conditions for trajectories
generate_trajectories <- function(n = 200, tmax = 20, steps = 500) {
  times <- seq(0, tmax, length.out = steps)
  all_trajectories <- list()
  
  for (i in 1:n) {
    state <- c(x = runif(1, -5, 5), y = runif(1, -5, 5))
    out <- ode(y = state, times = times, func = vector_field, parms = NULL)
    out_df <- as.data.frame(out)
    out_df$trajectory <- i
    all_trajectories[[i]] <- out_df
  }
  
  bind_rows(all_trajectories)
}

# Create the art
set.seed(42)
trajectories <- generate_trajectories(n = 300, tmax = 15)

# Plot using ggplot2
ggplot(trajectories, aes(x = x, y = y, group = trajectory, color = trajectory)) +
  geom_path(alpha = 0.3, size = 0.3) +
  scale_color_viridis_c(option = "A") +
  theme_void() +
  theme(legend.position = "none") +
  coord_equal() 

#########


library(deSolve)
library(ggplot2)
library(dplyr)

# Define the vector field (same as before)
vector_field <- function(t, state, parameters) {
  with(as.list(state), {
    dx <- sin(y) - 0.5 * x
    dy <- cos(x) - 0.5 * y
    list(c(dx, dy))
  })
}

# Generate trajectory data with progress (normalized time)
generate_trajectories <- function(n = 200, tmax = 20, steps = 500) {
  times <- seq(0, tmax, length.out = steps)
  all_trajectories <- list()
  
  for (i in 1:n) {
    state <- c(x = runif(1, -5, 5), y = runif(1, -5, 5))
    out <- ode(y = state, times = times, func = vector_field, parms = NULL)
    out_df <- as.data.frame(out)
    out_df$trajectory <- i
    out_df$progress <- seq(0, 20, length.out = nrow(out_df))  # normalized [0,1]
    all_trajectories[[i]] <- out_df
  }
  
  bind_rows(all_trajectories)
}

# Create the data
set.seed(42)
trajectories <- generate_trajectories(n = 300, tmax = 15)

# Plot: lines + dots with size = progress
ggplot(trajectories, aes(x = x, y = y, group = trajectory, color = trajectory)) +
  geom_path(alpha = 0.3, size = 0.3) +
  geom_point(aes(size = progress), shape = 16, stroke = 0) +
  scale_size(range = c(0.1, 1.5)) +
  scale_color_viridis_c(option = "A") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA)
  ) +
  coord_equal()
