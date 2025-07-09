library(grid)

grid.newpage()
pushViewport(viewport(width = unit(1, "npc"), height = unit(1, "npc")))

# Colors and line setup
bg_color <- "#f4e8d3"
line_color <- "#203e2d"

grid.rect(gp = gpar(fill = bg_color, col = NA))

draw_u_arc <- function(center_x, center_y, r_min, r_max,
                       orientation = "horizontal",
                       direction = "right",
                       arc_open = "left",
                       arc_curve = "concave",  # new: concave or convex
                       n_lines = 12,
                       line_spacing = 0.012,
                       line_color = "#203e2d",
                       line_width = 3.5) {
  
  for (i in 0:(n_lines - 1)) {
    r <- r_min + i * line_spacing
    
    if (orientation == "horizontal") {
      # Classic horizontal U arc
      theta <- switch(
        arc_open,
        "left" = seq(pi/2, 3*pi/2, length.out = 200),   # opens left
        "right" = seq(-pi/2, pi/2, length.out = 200),   # opens right
        stop("arc_open must be 'left' or 'right' when orientation is horizontal")
      )
      x_arc <- center_x + r * cos(theta)
      y_arc <- center_y + r * sin(theta)
      
    } else if (orientation == "vertical") {
      # Modified: vertical arc that opens left/right and curves up/down
      theta <- switch(
        paste(arc_open, arc_curve),
        "left concave"  = seq(0, pi, length.out = 200),       # U shape pointing down, open left
        "left convex"   = seq(pi, 2*pi, length.out = 200),     # ∩ shape, open left
        "right concave" = seq(pi, 2*pi, length.out = 200),     # U shape pointing down, open right
        "right convex"  = seq(0, pi, length.out = 200),        # ∩ shape, open right
        stop("arc_open must be left/right and arc_curve concave/convex")
      )
      x_arc <- center_x + r * cos(theta)
      y_arc <- center_y + r * sin(theta)
      
    } else {
      stop("orientation must be either 'horizontal' or 'vertical'")
    }
    
    # Draw only the arc without extensions
    grid.lines(
      x = unit(x_arc, "npc"),
      y = unit(y_arc, "npc"),
      gp = gpar(col = line_color, lwd = line_width)
    )
  }
}

# Top cluster: left to right U
draw_u_arc(center_x = 0.41, center_y = 0.50, r_min = 0.02, r_max = 0.02 + (n_lines - 1) * line_spacing, direction = "right")
draw_u_arc(center_x = 0.1, center_y = 0.50, r_min = 0.02, r_max = 0.02 + (n_lines - 1) * line_spacing, direction = "left", arc_open = "right")

draw_u_arc(center_x = 0.57, center_y = 0.50, r_min = 0.02, r_max = 0.02 + (n_lines - 1) * line_spacing, direction = "down", orientation = "vertical")
draw_u_arc(center_x = 0.73, center_y = 0.50, r_min = 0.02, r_max = 0.02 + (n_lines - 1) * line_spacing, direction = "up", orientation = "vertical", arc_curve = "convex")

draw_u_arc(center_x = 0.25, center_y = 0.19, r_min = 0.02, r_max = 0.02 + (n_lines - 1) * line_spacing, direction = "right")
draw_u_arc(center_x = 0.25, center_y = 0.19, r_min = 0.02, r_max = 0.02 + (n_lines - 1) * line_spacing, direction = "left", arc_open = "right")

draw_u_arc(center_x = 0.83, center_y = 0.19, r_min = 0.02, r_max = 0.02 + (n_lines - 1) * line_spacing, direction = "right")
draw_u_arc(center_x = 0.52, center_y = 0.19, r_min = 0.02, r_max = 0.02 + (n_lines - 1) * line_spacing, direction = "left", arc_open = "right")

draw_u_arc(center_x = 0.70, center_y = 0.81, r_min = 0.02, r_max = 0.02 + (n_lines - 1) * line_spacing, direction = "right", line_width = 8)
draw_u_arc(center_x = 0.70, center_y = 0.81, r_min = 0.02, r_max = 0.02 + (n_lines - 1) * line_spacing, direction = "left", arc_open = "right", line_width = 8)

draw_u_arc(center_x = 0.31, center_y = 0.81, r_min = 0.02, r_max = 0.02 + (n_lines - 1) * line_spacing, direction = "down", orientation = "vertical")
draw_u_arc(center_x = 0.15, center_y = 0.81, r_min = 0.02, r_max = 0.02 + (n_lines - 1) * line_spacing, direction = "up", orientation = "vertical", arc_curve = "convex")
