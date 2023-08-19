library(vegan)
library(readr)
library(psych)
library(dplyr)
library(ggplot2)

data = read_csv('C:/Users/user/Desktop/pythonProject3/ANOVA_BALANCED.csv')
### Box plot visualization ######
# Reshaping the data into a suitable format
data_long <- reshape2::melt(data, id.vars = c("Barrier", "Erosion", "Site"))

# Creating grouped box plot for the barrier group
ggplot(data_long, aes(x = Barrier, y = value, fill = Barrier)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  theme_bw() +
  labs(x = "Barrier", y = "Value", title = "Boxplot of Count and Weight grouped by Barrier") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "right")

# Creating grouped box plot for Erosion group
ggplot(data_long, aes(x = Erosion, y = value, fill = Erosion)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  theme_bw() +
  labs(x = "Erosion", y = "Value", title = "Boxplot of Count and Weight grouped by Level of Erosion") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "right")

## PERMANOVA Test ####
# calculating the Bray-Curtis dissimilarity matrix
data_matrix <- data[,c("Count", "Weight")]
bc_dissimilarity <- vegdist(data_matrix, method = "bray")

# creating the data frame for the independent variables
data_factors <- data[,c("Barrier", "Erosion", "Site")]

# running PERMANOVA
result <- adonis2(bc_dissimilarity ~ Barrier * Erosion, data = data_factors)
print(result)

## Non-metric Multidimensional Scaling ###

# Creating color and shape vector
colors <- c("red", "blue", "green")   # replace with colors of your choice
shapes <- c(16, 17, 18)   # replace with shapes of your choice

# Matching the colors and shapes to the Barrier levels
color_mapping_B <- colors[as.numeric(as.factor(data$Barrier))]
shape_mapping_B <- shapes[as.numeric(as.factor(data$Barrier))]

# Match the colors and shapes to Barrier levels
color_mapping_E <- colors[as.numeric(as.factor(data$Erosion))]
shape_mapping_E <- shapes[as.numeric(as.factor(data$Erosion))]

# Creating NMDS ordination plot for Barrier Type
mds <- metaMDS(bc_dissimilarity)
stressplot(mds)

ordiplot(mds, display = "sites", type = "n") 
points(mds, display = "sites", col = color_mapping_B, pch = shape_mapping_B)

# Adding stress value to the plot
best_stress <- round(mds$stress, 3)
text(x = min(mds$points[,1]), y = min(mds$points[,2]), paste("Stress:", best_stress), cex=0.8, adj = c(0,6))
print(best_stress)
data$Barrier <- as.factor(data$Barrier)
text(mds$points, labels = data$Site, cex = 0.6, adj = c(1,2))
levels <- levels(data$Barrier)

# Defining a vector of colors for each group
ellipse_colors <- c("red", "blue", "green") 

# Adding ellipses without labels and with different colors
ordiellipse(mds, groups = data$Barrier, kind = "se", conf = 0.95, label = FALSE, col = ellipse_colors)

legend("topright", legend = levels, fill = unique(color_mapping_B), bty = "n", cex = 0.8, title = "Barrier Type")



#    Creating NMDS ordination plot for Erosion Type
mds <- metaMDS(bc_dissimilarity)
stressplot(mds)
# Creating a blank ordination plot
ordiplot(mds, display = "sites", type = "n") # "n" specifies no plotting of points

# Adding points with desired colors and shapes
points(mds, display = "sites", col = color_mapping_E, pch = shape_mapping_E)

# Adding stress value to the plot
best_stress <- round(mds$stress, 3)
text(x = min(mds$points[,1]), y = min(mds$points[,2]), paste("Stress:", best_stress), cex=0.8, adj = c(0,6))
print(best_stress)
data$Erosion <- as.factor(data$Erosion)
levels <- levels(data$Erosion)
text(mds$points, labels = data$Site, cex = 0.6, adj = c(1,2))

# Defining the vector of colors for each group
ellipse_colors <- c("red", "blue", "green") # add or change colors as per your preference

# Adding ellipses without labels and with different colors
ordiellipse(mds, groups = data$Erosion, kind = "se", conf = 0.95, label = FALSE, col = ellipse_colors)

legend("topright", legend = levels, fill = unique(color_mapping_E), bty = "n", cex = 0.8, title = "Erosion Type")

# Creating interaction effect between the two independent variables
data$Interaction <- interaction(data$Barrier, data$Erosion)

# Calculate Bray Curtis dissimilarity
bc_dissimilarity <- vegdist(data[,c("Count","Weight")], method = "bray")

# Creating color and shape mappings for the Interaction variable
colors <- c("red", "blue", "green", "purple", "orange", "pink")#, "brown", "grey", "cyan")
shapes <- c(16, 17, 18, 19, 20, 21)#, 22, 23, 24)

color_mapping_I <- colors[as.numeric(as.factor(data$Interaction))]
shape_mapping_I <- shapes[as.numeric(as.factor(data$Interaction))]

# NMDS ordination
mds <- metaMDS(bc_dissimilarity)

# Creating ordination plot
ordiplot(mds, display = "sites", type = "n") # Create a blank plot
points(mds$points, pch = shape_mapping_I, col = color_mapping_I, cex = 1.5) # Add points
text(mds$points, labels = data$Site, cex = 0.6, adj = c(1,2)) # Add site labels

# Adding legend
legend("topright", legend = levels(data$Interaction), pch = unique(shape_mapping_I), col = unique(color_mapping_I), cex = 0.8)

# Adding stress value to the plot
text(x = min(mds$points[,1]), y = min(mds$points[,2]), paste("Stress:", round(mds$stress, digits = 4)), cex=0.8, adj = c(0,6))

