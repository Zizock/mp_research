rm(list=ls())

# Load necessary libraries
library(plotly)

# Set preset values for constants
H <- 0.95
L <- 0.65
g_bar <- 1/0.85
lambda_bar <- 0.1
sigma <- 2

# Define the sub-function for delta
delta_function <- function(nu, varphi) {
  return(1 / (varphi * H * (L * H / g_bar)^(-nu) * (1 - lambda_bar) / ((1 + varphi) * (1 - L)^(-nu)) + 1))
}

# Define the main function
main_function <- function(nu, varphi) {
  delta <- delta_function(nu, varphi)
  # Calculate the denominator first to check for undefined values
  denom <- (1 + varphi) - delta * (nu - sigma) * ((0.85 * H * L) / ((delta * (0.85 * H * L)^(1 - nu) + (1 - delta) * (1 - L)^(1 - nu))^(1 / (1 - nu))))^(1 - nu)
  
  if (abs(denom) < 1e-6) {
    return(NA)  # Mark undefined points as NA
  }
  
  # Calculate the main function's value if denominator is not zero
  num <- delta * (nu - sigma) * ((0.85 * H * L) / ((delta * (0.85 * H * L)^(1 - nu) + (1 - delta) * (1 - L)^(1 - nu))^(1 / (1 - nu))))^(1 - nu) +
    (1 - delta) * (nu - sigma) * ((1 - L) / ((delta * (0.85 * H * L)^(1 - nu) + (1 - delta) * (1 - L)^(1 - nu))^(1 / (1 - nu))))^(1 - nu) * (-L / (1 - L)) +
    nu * L / (1 - L)
  
  return(num / denom)
}

# Create a grid of nu and varphi values
nu_vals <- seq(0, 50, by = 0.1)
varphi_vals <- seq(0, 50, by = 0.1)
z_vals <- outer(nu_vals, varphi_vals, Vectorize(function(nu, varphi) main_function(nu, varphi)))

# Define colors based on positive, negative, or undefined (NA) values
colors <- ifelse((is.na(z_vals) | z_vals >50), 'gray', ifelse(z_vals > 0, 'blue', 'red'))

# Plot the surface with color based on positive, negative, or undefined values
plot_ly(x = ~nu_vals, y = ~varphi_vals, z = ~z_vals, colorscale = list(c(0, 0.5, 1), c("gray", "red", "blue")),
        type = "surface", showscale = FALSE, surfacecolor = ~ifelse(is.na(z_vals), 0, ifelse(z_vals > 0, 1, 0.5)),
        lighting = list(ambient = 0.4, diffuse = 0.5, specular = 0.9, roughness = 0.9, fresnel = 0.2),
        lightposition = list(x = 30, y = 50, z = 100)) %>%
  layout(title = "3D Surface Plot Showing Positive, Negative, and Undefined Regions with Shading",
         scene = list(yaxis = list(title = "\u03BD"),
                      xaxis = list(title = "\u03C6"),
                      zaxis = list(title = 'H-L relation', range = c(-50, 50), zeroline = TRUE, nticks = 10),
                      camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))))