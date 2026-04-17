# importing the package to use for the project

library(tidyverse)

# fetching for the data
getwd()

list.dirs(recursive = FALSE)

setwd("C:/Users/Felix/Downloads/R-Project")
# look through and identifying the data 
list.files()


# calling and using the data by assigning a name to it
forest_fire <- read.csv("Forest_Fire_Data.csv")

# now calling the name used for the project
forest_fire
names(forest_fire)

glimpse(forest_fire)

sum(is.na(forest_fire), na.rm = TRUE)

summary(forest_fire)

unique(forest_fire$month)
unique(forest_fire$outcome)


# but before that, i like to change month and outcome to a factor data type
forest_fire$month <- as.factor(forest_fire$month)
unique(forest_fire$month)

forest_fire$outcome <- as.factor(forest_fire$outcome)

class(forest_fire$outcome)

# confirming it is a factor
is.factor(forest_fire$month)
is.factor(forest_fire$outcome)



# changing the values and giving it a set value
forest_fire <- forest_fire %>%
  mutate(outcome_text = recode(outcome,
                               `0` = "No Fire",   
                               `1` = "Fire"        
  ),
month_name = recode(month,
                      `1` = "jan", `2` = "feb", `3` = "mar", `4` = "apr",
                      `5` = "may", `6` = "jun", `7` = "jul", `8` = "aug",
                      `9` = "sep", `10` = "oct", `11` = "nov", `12` = "dec"
                      )
)

forest_fire
View(forest_fire)

# viewing the columns, old and new in the table
names(forest_fire)

# target columns
Descrip_forest_fire <- forest_fire %>%
  select(month_name, day, outcome_text, Temperature, RH, wind, rain, FFMC, DMC, DC, ISI)
View(Descrip_forest_fire)

glimpse(Descrip_forest_fire)


names(Descrip_forest_fire)



# Finding the descriptives for the main key columns to use in the project

                 

                           # Exploratory Data Analytics
# Descriptive Statistics
numeric_cols <- Descrip_forest_fire %>%
  select(where(is.numeric))

# Mean
colMeans(numeric_cols, na.rm = TRUE)
# Standard Deviation
apply(numeric_cols, 2, sd, na.rm = TRUE)


# EDA: Creating box plot to visualize the distribution of temperature
boxplot(Descrip_forest_fire$Temperature, col = "lightblue", xlab = "Temperature", ylab = "Value")

# to remove outliers  i used the outline function
boxplot(Descrip_forest_fire$Temperature, col = "lightblue", xlab = "Temperature", ylab = "Value", outline = FALSE)

# EDA: Creating density plot to visualize the distribution of temperature
plot(density(Descrip_forest_fire$Temperature), col = "lightblue", main = "Density Plot of Temperature")


# EDA: Creating scatter plot matrix to visualize pairwise relationships
install.packages("GGally")
library(GGally)

# Beautified pairs plot
ggpairs(Descrip_forest_fire, columns = c("Temperature", "RH", "wind", "rain", "FFMC", "DMC", "DC", "ISI"), 
        lower = list(continuous = "points"), diag = list(continuous = "density"), 
        upper = list(continuous = "cor", combo = "box"))

pairs(Descrip_forest_fire)

# EDA: Creating correlation matrix
cor_matrix <- cor(Descrip_forest_fire[, 
                                    c("Temperature", "RH", "wind", "rain", "FFMC", "DMC", "DC", "ISI")])
cor_matrix
corrplot::corrplot(cor_matrix, method = "circle")


# EDA: Creating histogram grid for multiple variables
par(mfrow = c(2, 2))
hist(Descrip_forest_fire$Temperature, col = "lightblue", xlab = "Temperature", main = "Temperature Histogram")
hist(Descrip_forest_fire$RH, col = "lightgreen", xlab = "RH", main = "RH Histogram")
hist(Descrip_forest_fire$wind, col = "lightpink", xlab = "Wind", main = "Wind Histogram")
hist(Descrip_forest_fire$rain, col = "lightyellow", xlab = "Rain", main = "Rain Histogram")








                              # unsupervised machine learning
library(dplyr)

numeric_cols <- Descrip_forest_fire %>%
  select(Temperature, RH, wind, rain, FFMC, DMC, DC, ISI)

# Step 2: Remove any rows with NA
numeric_cols <- na.omit(numeric_cols)

# Step 3: Scale the data
scaled_data <- scale(numeric_cols)


# Performing K-means clustering
set.seed(123)
kmeans_model <- kmeans(scaled_data, centers = 3, nstart = 25)

# View the cluster centers
kmeans_model$centers

# Assign cluster labels to each data point
Descrip_forest_fire$cluster <- kmeans_model$cluster


# visualizing
# Assume Cluster variable exists in your data set or you have computed it beforehand
# Example: Main_fire_service$Cluster <- k-means_result$cluster
View(Descrip_forest_fire)

ggplot(Descrip_forest_fire, aes(x = Temperature, y = RH, color = FFMC)) +
  geom_point() +
  labs(x = "Temperature", y = "Relative Humidity", color = "FFMC") +
  ggtitle("Effect of Temperature and Relative Humidity on FFMC") +
  theme_minimal()


# beautification
palette <- viridisLite::viridis(10)

ggplot(Descrip_forest_fire, aes(x = Temperature, y = RH, color = FFMC)) +
  geom_point(alpha = 0.7, size = 3) +  # Adjust transparency and point size
  scale_color_gradientn(colors = palette, limits = c(min(Descrip_forest_fire$FFMC), 
                                                     max(Descrip_forest_fire$FFMC)),
                        name = "FFMC") +  # Add gradient legend
  labs(x = "Temperature (°C)", y = "Relative Humidity (%)", title = "Effect of Temperature and Relative Humidity on FFMC") +
  theme_minimal() +  # Use minimal theme for clean appearance
  theme(legend.position = "bottom") +  # Move legend to the bottom
  geom_smooth(method = "loess", color = "black", fill = "grey", alpha = 0.2) +  # Add smoother
  guides(color = guide_colorbar(barwidth = 20, barheight = 1, title.position = "top"))  # Adjust colorbar appearance

# Add cluster labels to the Main_fire_service data set
Descrip_forest_fire$Cluster <- as.factor(kmeans_model$cluster)

# Create the ggplot scatterplot
ggplot(Descrip_forest_fire, aes(x = Temperature, y =Cluster , color =outcome_text )) +
  geom_point() +
  labs(x = "Temperature", y = "Fire observation", color = "Cluster") +
  ggtitle("K-means Clustering of Temperature vs Fire outcome") +
  theme_minimal()


# Define color palette for clusters
cluster_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")

ggplot(Descrip_forest_fire, aes(x = DMC, y = FFMC, color = factor(outcome_text))) +
  geom_point(alpha = 0.7, size = 3) +  # Adjust transparency and point size
  scale_color_manual(values = cluster_palette, name = "Cluster") +  # Set custom color palette
  labs(x = "Duff Moisture Code ", y = "Fine Fuel Moisture Code ", title = "K-means Clustering of DMC and FFMC") +
  theme_minimal() +  # Use minimal theme for clean appearance
  theme(legend.position = "bottom") +  # Move legend to the bottom
  guides(color = guide_legend(override.aes = list(size = 4)))  # Adjust legend size for better readability
