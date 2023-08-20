# Loading libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Loading dataset
data <- read.csv('C:/Users/user/Desktop/pythonProject3/reg_data.csv')

# creating the list of the sites
sites <- unique(data$Site)

plot_list <- list()

for (site in sites) {
  site_data <- filter(data, Site == site)
  
  # Fitting linear model
  model <- lm(Mass ~ Count, data = site_data)
  
  # Calculating the R-squared
  r2 <- summary(model)$r.squared
  slope <- coef(model)["Count"]
  intercept <- coef(model)["(Intercept)"]
  
  # Extracting p-value for slope
  p_value <- summary(model)$coefficients[2,4]
  
  # Creating a scatter plot with regression line
  p <- ggplot(site_data, aes(x = Count, y = Mass)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x) +
    labs(title = paste("Regression Plot for Site", site),
         x = "Count (items/m2)",
         y = "Mass (g/m2)") +
    annotate("text", x = min(site_data$Count), y = max(site_data$Mass), 
             label = paste("R²:", round(r2, 2), "\nEquation: y =", round(slope, 2), "*x +", round(intercept, 2), "\np-value:", round(p_value, 4)), 
             hjust = 0, vjust = 1)
  
  plot_list[[site]] <- p
}

# Arranging plots in a grid 
plot_grid <- grid.arrange(grobs = plot_list, ncol = 2)

# Saving the grid as a high-resolution PNG
ggsave("plottgrid.png", plot_grid, width = 16, height = 20, dpi = 300)
