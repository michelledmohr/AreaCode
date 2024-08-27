# Load required libraries
library(sf)
library(ggplot2)
library(sp)
library(readr)

# Read the CSV file
waypoints <- read_csv("pila_datum.csv")

# Check the first few rows of the dataframe
head(waypoints)

# Ensure columns are numeric
waypoints$plot_beg_UTM_E <- as.numeric(waypoints$plot_beg_UTM_E)
waypoints$plot_beg_UTM_N <- as.numeric(waypoints$plot_beg_UTM_N)
waypoints$zone <- as.numeric(waypoints$zone)

# Check for NA values
na_rows <- which(is.na(waypoints$plot_beg_UTM_E) | is.na(waypoints$plot_beg_UTM_N) | is.na(waypoints$zone))
if (length(na_rows) > 0) {
  cat("Warning: The following rows have NA values and will be removed:\n")
  print(waypoints[na_rows, ])
  waypoints <- waypoints[-na_rows, ]
}

# Function to convert UTM to Lat/Lon for a single row
convert_utm_to_latlon <- function(easting, northing, zone) {
  proj_string <- paste0("+proj=utm +zone=", zone, " +datum=WGS84")
  coords <- sp::SpatialPoints(cbind(easting, northing), proj4string = sp::CRS(proj_string))
  latlon <- sp::spTransform(coords, CRS("+proj=longlat +datum=WGS84"))
  return(data.frame(lat = latlon@coords[, 2], lon = latlon@coords[, 1]))
}

# Apply the conversion row by row
latlon_coords <- do.call(rbind, mapply(function(easting, northing, zone) {
  tryCatch({
    convert_utm_to_latlon(easting, northing, zone)
  }, error = function(e) {
    # Handle conversion errors
    return(data.frame(lat = NA, lon = NA))
  })
}, waypoints$plot_beg_UTM_E, waypoints$plot_beg_UTM_N, waypoints$zone, SIMPLIFY = FALSE))

# Combine the lat/lon with the original dataframe
waypoints <- cbind(waypoints, latlon_coords)

# Remove rows with NA in lat/lon
waypoints <- waypoints[!is.na(waypoints$lat) & !is.na(waypoints$lon), ]

# Create an sf object
waypoints_sf <- st_as_sf(waypoints, coords = c("lon", "lat"), crs = 4326)

# Plot the points
ggplot(data = waypoints_sf) +
  geom_sf() +
  ggtitle("Waypoints")

# Calculate the Convex Hull
convex_hull <- st_convex_hull(st_union(waypoints_sf))

# Plot the convex hull
ggplot() +
  geom_sf(data = waypoints_sf) +
  geom_sf(data = convex_hull, fill = NA, color = "blue") +
  ggtitle("Waypoints with Convex Hull")

# Calculate the area (in square meters)
area <- st_area(convex_hull)
print(paste("Area:", area, "square meters"))


########## Calculate area for each specific plotID
# Load required libraries
library(sf)
library(ggplot2)
library(sp)
library(readr)
library(dplyr)

# Read the CSV file
waypoints <- read_csv("pila_datum.csv")

# Check the first few rows of the dataframe
head(waypoints)

# Ensure columns are numeric
waypoints$plot_beg_UTM_E <- as.numeric(waypoints$plot_beg_UTM_E)
waypoints$plot_beg_UTM_N <- as.numeric(waypoints$plot_beg_UTM_N)
waypoints$zone <- as.numeric(waypoints$zone)

# Check for NA values
na_rows <- which(is.na(waypoints$plot_beg_UTM_E) | is.na(waypoints$plot_beg_UTM_N) | is.na(waypoints$zone))
if (length(na_rows) > 0) {
  cat("Warning: The following rows have NA values and will be removed:\n")
  print(waypoints[na_rows, ])
  waypoints <- waypoints[-na_rows, ]
}

# Function to convert UTM to Lat/Lon for a single row
convert_utm_to_latlon <- function(easting, northing, zone) {
  proj_string <- paste0("+proj=utm +zone=", zone, " +datum=WGS84")
  coords <- sp::SpatialPoints(cbind(easting, northing), proj4string = sp::CRS(proj_string))
  latlon <- sp::spTransform(coords, CRS("+proj=longlat +datum=WGS84"))
  return(data.frame(lat = latlon@coords[, 2], lon = latlon@coords[, 1]))
}

# Apply the conversion row by row
latlon_coords <- do.call(rbind, mapply(function(easting, northing, zone) {
  tryCatch({
    convert_utm_to_latlon(easting, northing, zone)
  }, error = function(e) {
    # Handle conversion errors
    return(data.frame(lat = NA, lon = NA))
  })
}, waypoints$plot_beg_UTM_E, waypoints$plot_beg_UTM_N, waypoints$zone, SIMPLIFY = FALSE))

# Combine the lat/lon with the original dataframe
waypoints <- cbind(waypoints, latlon_coords)

# Remove rows with NA in lat/lon
waypoints <- waypoints[!is.na(waypoints$lat) & !is.na(waypoints$lon), ]

# Create an sf object
waypoints_sf <- st_as_sf(waypoints, coords = c("lon", "lat"), crs = 4326)

# Group by PlotID and calculate area for each group
plot_areas <- waypoints_sf %>%
  group_by(plotID) %>%
  summarise(
    convex_hull = st_convex_hull(st_union(geometry)),
    area = st_area(convex_hull)
  )

# Print the results
print(plot_areas)

# Plot each PlotID with its Convex Hull
ggplot() +
  geom_sf(data = waypoints_sf, aes(color = factor(plotID))) +
  geom_sf(data = plot_areas, fill = NA, color = "blue") +
  ggtitle("Waypoints with Convex Hull by PlotID") +
  theme(legend.position = "right")

#### Create the column to calculate area
#####

# Load required libraries
library(sf)
library(ggplot2)
library(sp)
library(readr)
library(dplyr)

# Read the CSV file
waypoints <- read_csv("pila_datum.csv")

# Check the first few rows of the dataframe
head(waypoints)

# Ensure columns are numeric
waypoints$plot_beg_UTM_E <- as.numeric(waypoints$plot_beg_UTM_E)
waypoints$plot_beg_UTM_N <- as.numeric(waypoints$plot_beg_UTM_N)
waypoints$zone <- as.numeric(waypoints$zone)

# Check for NA values
na_rows <- which(is.na(waypoints$plot_beg_UTM_E) | is.na(waypoints$plot_beg_UTM_N) | is.na(waypoints$zone))
if (length(na_rows) > 0) {
  cat("Warning: The following rows have NA values and will be removed:\n")
  print(waypoints[na_rows, ])
  waypoints <- waypoints[-na_rows, ]
}

# Function to convert UTM to Lat/Lon for a single row
convert_utm_to_latlon <- function(easting, northing, zone) {
  proj_string <- paste0("+proj=utm +zone=", zone, " +datum=WGS84")
  coords <- sp::SpatialPoints(cbind(easting, northing), proj4string = sp::CRS(proj_string))
  latlon <- sp::spTransform(coords, CRS("+proj=longlat +datum=WGS84"))
  return(data.frame(lat = latlon@coords[, 2], lon = latlon@coords[, 1]))
}

# Apply the conversion row by row
latlon_coords <- do.call(rbind, mapply(function(easting, northing, zone) {
  tryCatch({
    convert_utm_to_latlon(easting, northing, zone)
  }, error = function(e) {
    # Handle conversion errors
    return(data.frame(lat = NA, lon = NA))
  })
}, waypoints$plot_beg_UTM_E, waypoints$plot_beg_UTM_N, waypoints$zone, SIMPLIFY = FALSE))

# Combine the lat/lon with the original dataframe
waypoints <- cbind(waypoints, latlon_coords)

# Remove rows with NA in lat/lon
waypoints <- waypoints[!is.na(waypoints$lat) & !is.na(waypoints$lon), ]

# Create an sf object
waypoints_sf <- st_as_sf(waypoints, coords = c("lon", "lat"), crs = 4326)

# Group by PlotID, calculate convex hull, and compute area
plot_areas <- waypoints_sf %>%
  group_by(plotID) %>%
  summarise(
    convex_hull = st_convex_hull(st_union(geometry)),
    area = st_area(convex_hull)
  ) %>%
  st_set_geometry(NULL)  # Remove geometry column for merging

# Join the calculated areas back to the original dataframe
waypoints_with_area <- waypoints %>%
  left_join(plot_areas, by = "plotID")

# Create an sf object with the new column
waypoints_sf_with_area <- st_as_sf(waypoints_with_area, coords = c("lon", "lat"), crs = 4326)

# Print the updated dataframe with areas
print(waypoints_with_area)

# Plot each PlotID with its Convex Hull
ggplot() +
  geom_sf(data = waypoints_sf_with_area, aes(color = factor(plotID))) +
  geom_sf(data = plot_areas, aes(fill = area), color = "blue", alpha = 0.2) +
  ggtitle("Waypoints with Convex Hull by PlotID") +
  theme(legend.position = "right")

# Optional: Save the updated dataframe to a new CSV file
write_csv(waypoints_with_area, "pila_datum_with_area.csv")

#######################
# Load required libraries
library(sf)
library(ggplot2)
library(sp)
library(readr)
library(dplyr)

# Read the CSV file
waypoints <- read_csv("pila_datum.csv")

# Check the first few rows of the dataframe
head(waypoints)

# Ensure columns are numeric
waypoints$PILA_UTM_E <- as.numeric(waypoints$PILA_UTM_E)
waypoints$PILA_UTM_N <- as.numeric(waypoints$PILA_UTM_N)
waypoints$zone <- as.numeric(waypoints$zone)

# Check for NA values
na_rows <- which(is.na(waypoints$PILA_UTM_E) | is.na(waypoints$PILA_UTM_N) | is.na(waypoints$zone))
if (length(na_rows) > 0) {
  cat("Warning: The following rows have NA values and will be removed:\n")
  print(waypoints[na_rows, ])
  waypoints <- waypoints[-na_rows, ]
}

# Function to convert UTM to Lat/Lon for a single row
convert_utm_to_latlon <- function(easting, northing, zone) {
  proj_string <- paste0("+proj=utm +zone=", zone, " +datum=WGS84")
  coords <- sp::SpatialPoints(cbind(easting, northing), proj4string = sp::CRS(proj_string))
  latlon <- sp::spTransform(coords, CRS("+proj=longlat +datum=WGS84"))
  return(data.frame(lat = latlon@coords[, 2], lon = latlon@coords[, 1]))
}

# Apply the conversion row by row
latlon_coords <- do.call(rbind, mapply(function(easting, northing, zone) {
  tryCatch({
    convert_utm_to_latlon(easting, northing, zone)
  }, error = function(e) {
    # Handle conversion errors
    return(data.frame(lat = NA, lon = NA))
  })
}, waypoints$PILA_UTM_E, waypoints$PILA_UTM_N, waypoints$zone, SIMPLIFY = FALSE))

# Combine the lat/lon with the original dataframe
waypoints <- cbind(waypoints, latlon_coords)

# Remove rows with NA in lat/lon
waypoints <- waypoints[!is.na(waypoints$lat) & !is.na(waypoints$lon), ]

# Create an sf object
waypoints_sf <- st_as_sf(waypoints, coords = c("lon", "lat"), crs = 4326)

# Re-project to a projected CRS (e.g., UTM zone 33N)
waypoints_sf <- st_transform(waypoints_sf, crs = 32633)  # Change to your specific UTM zone if needed


### Working with Joan 

plot(waypoints_sf$plotID) 

waypoints_sf$plotID = as.factor(waypoints_sf$plotID)

library(tmap)
tmap_mode("view") 
tm_shape(waypoints_sf) + 
  tm_dots(col = "plotID") +
  tm_layout(legend.position = c("left", "bottom"))

