# Load the 'grid' package for advanced graphics
# If you don't have it installed, run: install.packages("grid")
library(grid)

# Define a color palette inspired by the provided image
# These are hex codes for cream/beige, red, dark blue/purple, dark green, orange, and dark grey/black
colors <- c(
  "#F0EAD6", # Cream/Beige
  "#E63946", # Red
  "#3D5A80", # Dark Blue/Purple
  "#4A7C59", # Dark Green
  "#F4A261", # Orange
  "#212121"  # Dark Grey/Black
)

# Function to draw a filled quarter circle (pie slice)
# This function calculates the coordinates for a pie slice and draws it using grid.polygon.
# Arguments:
#   center_x, center_y: Coordinates of the circle's center (where the arc originates from).
#   radius: The radius of the circle from which the quarter-circle is taken.
#   start_angle_deg, end_angle_deg: The starting and ending angles in degrees for the arc.
#   fill_color: The color to fill the quarter circle.
draw_filled_quarter_circle <- function(center_x, center_y, radius, start_angle_deg, end_angle_deg, fill_color) {
  # Convert angles from degrees to radians for trigonometric functions
  angles_rad <- seq(start_angle_deg * pi / 180, end_angle_deg * pi / 180, length.out = 50) # 50 segments for a smooth curve
  
  # Calculate x and y coordinates for the arc
  x_coords <- center_x + radius * cos(angles_rad)
  y_coords <- center_y + radius * sin(angles_rad)
  
  # Add the center point to close the polygon (forming a pie slice)
  x_coords <- c(center_x, x_coords, center_x)
  y_coords <- c(center_y, y_coords, center_y)
  
  # Draw the filled polygon (quarter circle)
  grid.polygon(x_coords, y_coords, gp = gpar(fill = fill_color, col = NA), default.units = "native")
}

# Set the dimensions of the grid (number of rows and columns of squares)
n_rows <- 6
n_cols <- 8

# Initialize a new graphics page
grid.newpage()

# Set up a viewport for drawing. This defines the coordinate system for our grid.
# xscale and yscale define the range of the x and y axes (0 to n_cols, 0 to n_rows).
# gp = gpar(fill = "white") sets a white background for the entire plot area.
pushViewport(viewport(xscale = c(0, n_cols), yscale = c(0, n_rows), gp = gpar(fill = "white")))

# Loop through each square in the grid
for (r in 0:(n_rows - 1)) { # Iterate through rows (y-coordinate)
  for (c in 0:(n_cols - 1)) { # Iterate through columns (x-coordinate)
    # Define the bottom-left corner coordinates of the current square
    current_x <- c
    current_y <- r
    
    # Randomly select a background color for the current square from the palette
    bg_color <- sample(colors, 1)
    
    # Draw the background rectangle for the current square
    # width and height are 1 unit, matching our xscale/yscale
    # just = c("left", "bottom") aligns the rectangle by its bottom-left corner
    # default.units = "native" uses the coordinate system defined by the viewport's xscale/yscale
    grid.rect(x = current_x, y = current_y, width = 1, height = 1,
              gp = gpar(fill = bg_color, col = NA), # col = NA removes the border
              just = c("left", "bottom"), default.units = "native")
    
    # Define various pattern types that can appear within a square
    # "solid": The square remains just its background color.
    # "single_qc": A single quarter-circle originating from one of the square's corners.
    # "full_circle": A full circle centered within the square.
    # "two_qc_diagonal": Two quarter-circles placed diagonally opposite each other.
    # "two_qc_adjacent": Two quarter-circles placed on adjacent corners.
    pattern_choices <- c("solid", "single_qc", "full_circle", "two_qc_diagonal", "two_qc_adjacent")
    
    # Assign probabilities for each pattern type. Adjust these to change the visual mix.
    # Higher probability for "single_qc" and "full_circle" to match the original image's density.
    pattern_probs <- c(0.1, 0.5, 0.2, 0.1, 0.1) 
    
    # Randomly choose a pattern for the current square based on the probabilities
    chosen_pattern <- sample(pattern_choices, 1, prob = pattern_probs)
    
    # Apply the chosen pattern
    if (chosen_pattern == "single_qc") {
      # Choose a random color for the quarter-circle
      qc_color <- sample(colors, 1)
      
      # Randomly decide which of the four corners the quarter-circle will originate from
      # 1: Bottom-left corner of the square (draws the top-right quarter of a circle)
      # 2: Bottom-right corner (draws the top-left quarter)
      # 3: Top-left corner (draws the bottom-right quarter)
      # 4: Top-right corner (draws the bottom-left quarter)
      corner_choice <- sample(1:4, 1)
      
      if (corner_choice == 1) { # Origin: (current_x, current_y) - bottom-left
        draw_filled_quarter_circle(current_x, current_y, 1, 0, 90, qc_color)
      } else if (corner_choice == 2) { # Origin: (current_x + 1, current_y) - bottom-right
        draw_filled_quarter_circle(current_x + 1, current_y, 1, 90, 180, qc_color)
      } else if (corner_choice == 3) { # Origin: (current_x, current_y + 1) - top-left
        draw_filled_quarter_circle(current_x, current_y + 1, 1, 270, 360, qc_color)
      } else { # Origin: (current_x + 1, current_y + 1) - top-right
        draw_filled_quarter_circle(current_x + 1, current_y + 1, 1, 180, 270, qc_color)
      }
    } else if (chosen_pattern == "full_circle") {
      # Draw a full circle centered in the square
      circle_color <- sample(colors, 1)
      grid.circle(x = current_x + 0.5, y = current_y + 0.5, r = 0.5, # Radius 0.5 fills the square
                  gp = gpar(fill = circle_color, col = NA), default.units = "native")
    } else if (chosen_pattern == "two_qc_diagonal") {
      # Draw two quarter-circles on diagonal corners
      qc1_color <- sample(colors, 1)
      qc2_color <- sample(colors, 1)
      
      # Randomly choose between the two diagonal pairs
      if (runif(1) < 0.5) { # Pair 1: Bottom-left origin and Top-right origin
        draw_filled_quarter_circle(current_x, current_y, 1, 0, 90, qc1_color) # Top-right quarter
        draw_filled_quarter_circle(current_x + 1, current_y + 1, 1, 180, 270, qc2_color) # Bottom-left quarter
      } else { # Pair 2: Bottom-right origin and Top-left origin
        draw_filled_quarter_circle(current_x + 1, current_y, 1, 90, 180, qc1_color) # Top-left quarter
        draw_filled_quarter_circle(current_x, current_y + 1, 1, 270, 360, qc2_color) # Bottom-right quarter
      }
    } else if (chosen_pattern == "two_qc_adjacent") {
      # Draw two quarter-circles on adjacent corners
      qc1_color <- sample(colors, 1)
      qc2_color <- sample(colors, 1)
      
      # Randomly choose between the four possible adjacent pairs
      adj_pair_choice <- sample(1:4, 1)
      if (adj_pair_choice == 1) { # Top-left and Top-right corners
        draw_filled_quarter_circle(current_x, current_y + 1, 1, 270, 360, qc1_color) # TL origin, BR quarter
        draw_filled_quarter_circle(current_x + 1, current_y + 1, 1, 180, 270, qc2_color) # TR origin, BL quarter
      } else if (adj_pair_choice == 2) { # Top-left and Bottom-left corners
        draw_filled_quarter_circle(current_x, current_y + 1, 1, 270, 360, qc1_color) # TL origin, BR quarter
        draw_filled_quarter_circle(current_x, current_y, 1, 0, 90, qc2_color) # BL origin, TR quarter
      } else if (adj_pair_choice == 3) { # Bottom-left and Bottom-right corners
        draw_filled_quarter_circle(current_x, current_y, 1, 0, 90, qc1_color) # BL origin, TR quarter
        draw_filled_quarter_circle(current_x + 1, current_y, 1, 90, 180, qc2_color) # BR origin, TL quarter
      } else { # Bottom-right and Top-right corners
        draw_filled_quarter_circle(current_x + 1, current_y, 1, 90, 180, qc1_color) # BR origin, TL quarter
        draw_filled_quarter_circle(current_x + 1, current_y + 1, 1, 180, 270, qc2_color) # TR origin, BL quarter
      }
    }
    # If "solid" pattern is chosen, no additional shapes are drawn, just the background.
  }
}

# Pop the viewport to finalize the drawing and return to the previous viewport (or the device).
popViewport()

# Load the 'grid' package for advanced graphics
# If you don't have it installed, run: install.packages("grid")
library(grid)

# --- Creative Enhancements ---
# 1. Expanded Color Palette: Added a few more vibrant and muted tones for richer combinations.
colors <- c(
  "#F0EAD6", # Cream/Beige
  "#E63946", # Red
  "#3D5A80", # Dark Blue/Purple
  "#4A7C59", # Dark Green
  "#F4A261", # Orange
  "#212121", # Dark Grey/Black
  "#A8DADC", # Light Blue
  "#C0D9B5", # Light Green
  "#F7B2BD"  # Muted Pink
)

# Function to draw a filled quarter circle (pie slice)
# Now includes a 'radius_factor' to draw smaller quarter-circles.
# Arguments:
#   center_x, center_y: Coordinates of the circle's center.
#   radius: The base radius of the square (1 unit).
#   radius_factor: A multiplier (0-1) for the radius, allowing smaller quarter-circles.
#   start_angle_deg, end_angle_deg: The starting and ending angles in degrees for the arc.
#   fill_color: The color to fill the quarter circle.
draw_filled_quarter_circle <- function(center_x, center_y, radius, radius_factor, start_angle_deg, end_angle_deg, fill_color) {
  current_radius <- radius * radius_factor
  angles_rad <- seq(start_angle_deg * pi / 180, end_angle_deg * pi / 180, length.out = 50)
  
  x_coords <- center_x + current_radius * cos(angles_rad)
  y_coords <- center_y + current_radius * sin(angles_rad)
  
  x_coords <- c(center_x, x_coords, center_x)
  y_coords <- c(center_y, y_coords, center_y)
  
  grid.polygon(x_coords, y_coords, gp = gpar(fill = fill_color, col = NA), default.units = "native")
}

# --- Main Pattern Generation ---

# Set the dimensions of the grid
n_rows <- 6
n_cols <- 8

# --- Reproducibility ---
# Set a random seed for reproducible patterns.
# Uncomment the line below and change the number to get a specific pattern again.
# set.seed(123) 

# Initialize a new graphics page
grid.newpage()

# Set up a viewport for drawing. This defines the coordinate system for our grid.
pushViewport(viewport(xscale = c(0, n_cols), yscale = c(0, n_rows), gp = gpar(fill = "white")))

# Loop through each square in the grid
for (r in 0:(n_rows - 1)) {
  for (c in 0:(n_cols - 1)) {
    current_x <- c
    current_y <- r
    
    # Randomly select a background color for the current square
    bg_color <- sample(colors, 1)
    
    # Draw the background rectangle for the current square
    grid.rect(x = current_x, y = current_y, width = 1, height = 1,
              gp = gpar(fill = bg_color, col = NA),
              just = c("left", "bottom"), default.units = "native")
    
    # Define various pattern types, including new creative ones
    pattern_choices <- c(
      "solid",             # The square remains just its background color.
      "single_qc",         # A single quarter-circle.
      "full_circle",       # A full circle centered within the square.
      "two_qc_diagonal",   # Two quarter-circles placed diagonally.
      "half_ring",         # A 'C' or 'L' shape formed by two concentric quarter-circles.
      "concentric_circles" # Two circles, one inside the other.
    )
    
    # Assign probabilities for each pattern type. Adjusted for more variety.
    pattern_probs <- c(0.05, 0.40, 0.20, 0.15, 0.10, 0.10) # Sum should be 1
    
    chosen_pattern <- sample(pattern_choices, 1, prob = pattern_probs)
    
    # Helper function to get a foreground color different from the background
    get_fg_color <- function(exclude_color) {
      sample(colors[colors != exclude_color], 1)
    }
    
    # Apply the chosen pattern
    if (chosen_pattern == "single_qc") {
      qc_color <- get_fg_color(bg_color)
      radius_factor <- sample(c(0.8, 1), 1, prob = c(0.3, 0.7)) # Sometimes smaller, mostly full size
      
      corner_choice <- sample(1:4, 1)
      if (corner_choice == 1) { # Origin: (current_x, current_y) - bottom-left
        draw_filled_quarter_circle(current_x, current_y, 1, radius_factor, 0, 90, qc_color)
      } else if (corner_choice == 2) { # Origin: (current_x + 1, current_y) - bottom-right
        draw_filled_quarter_circle(current_x + 1, current_y, 1, radius_factor, 90, 180, qc_color)
      } else if (corner_choice == 3) { # Origin: (current_x, current_y + 1) - top-left
        draw_filled_quarter_circle(current_x, current_y + 1, 1, radius_factor, 270, 360, qc_color)
      } else { # Origin: (current_x + 1, current_y + 1) - top-right
        draw_filled_quarter_circle(current_x + 1, current_y + 1, 1, radius_factor, 180, 270, qc_color)
      }
    } else if (chosen_pattern == "full_circle") {
      circle_color <- get_fg_color(bg_color)
      radius_factor <- sample(c(0.4, 0.5), 1) # Smaller or full size within the cell
      grid.circle(x = current_x + 0.5, y = current_y + 0.5, r = radius_factor,
                  gp = gpar(fill = circle_color, col = NA), default.units = "native")
    } else if (chosen_pattern == "two_qc_diagonal") {
      qc1_color <- get_fg_color(bg_color)
      qc2_color <- get_fg_color(c(bg_color, qc1_color)) # Ensure qc2_color is different from bg and qc1
      radius_factor <- sample(c(0.8, 1), 1, prob = c(0.3, 0.7))
      
      if (runif(1) < 0.5) {
        draw_filled_quarter_circle(current_x, current_y, 1, radius_factor, 0, 90, qc1_color)
        draw_filled_quarter_circle(current_x + 1, current_y + 1, 1, radius_factor, 180, 270, qc2_color)
      } else {
        draw_filled_quarter_circle(current_x + 1, current_y, 1, radius_factor, 90, 180, qc1_color)
        draw_filled_quarter_circle(current_x, current_y + 1, 1, radius_factor, 270, 360, qc2_color)
      }
    } else if (chosen_pattern == "half_ring") {
      # Draw a larger quarter-circle, then overlay a smaller one of the background color
      ring_color <- get_fg_color(bg_color)
      
      # Randomly choose a corner for the half-ring
      corner_choice <- sample(1:4, 1)
      
      # Define the angles for the outer and inner quarter-circles
      angles <- switch(corner_choice,
                       "1" = c(0, 90),   # Bottom-left origin, top-right arc
                       "2" = c(90, 180),  # Bottom-right origin, top-left arc
                       "3" = c(270, 360), # Top-left origin, bottom-right arc
                       "4" = c(180, 270)  # Top-right origin, bottom-left arc
      )
      
      # Draw the outer part of the ring
      draw_filled_quarter_circle(
        current_x + (corner_choice %in% c(2, 4)), # Adjust center_x for right corners
        current_y + (corner_choice %in% c(3, 4)), # Adjust center_y for top corners
        1, 1, angles[1], angles[2], ring_color
      )
      
      # Draw the inner part of the ring (same color as background to create the "hole")
      draw_filled_quarter_circle(
        current_x + (corner_choice %in% c(2, 4)),
        current_y + (corner_choice %in% c(3, 4)),
        1, 0.6, angles[1], angles[2], bg_color # Smaller radius for the inner circle
      )
    } else if (chosen_pattern == "concentric_circles") {
      # Draw two circles, one inside the other, with different colors
      outer_circle_color <- get_fg_color(bg_color)
      inner_circle_color <- get_fg_color(c(bg_color, outer_circle_color))
      
      # Outer circle
      grid.circle(x = current_x + 0.5, y = current_y + 0.5, r = 0.5,
                  gp = gpar(fill = outer_circle_color, col = NA), default.units = "native")
      
      # Inner circle (smaller)
      grid.circle(x = current_x + 0.5, y = current_y + 0.5, r = 0.3,
                  gp = gpar(fill = inner_circle_color, col = NA), default.units = "native")
    }
  }
}

# Pop the viewport to finalize the drawing and return to the previous viewport (or the device).
popViewport()
