library(sf)
library(ggplot2)

# Specify map ranges
ymin = 54; ymax = 60; xmin = 2; xmax = 21

map_data <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf",
  continent = "europe")

# Crop the polygon for plotting and efficiency:
# st_bbox(map_data) # find the rough coordinates
sf::sf_use_s2(FALSE)

swe_coast <- suppressWarnings(suppressMessages(
  st_crop(map_data,
          c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax))))

ggplot(swe_coast) + 
  geom_sf()

# Transform our map into UTM 33 coordinates, which is the equal-area projection we fit in:
utm_zone33 <- 32633
swe_coast_proj <- sf::st_transform(swe_coast, crs = utm_zone33)

ggplot(swe_coast_proj) + 
  geom_sf()

# Define plotting theme for main plot
theme_plot <- function(base_size = 11, base_family = "") {
  theme_light(base_size = base_size, base_family = "") +
    theme(
      axis.text = element_text(color = "grey5"),
      legend.position = "bottom",
      legend.key.height = unit(0.2, "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-5, -5, -5, -5),
      strip.background = element_rect(fill = "grey95"),
      strip.text = element_text(color = "grey10"),
      strip.text.x = element_text(margin = margin(b = 2, t = 2), color = "grey10", size = 10),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank()
      #, axis.line = element_line(colour = "grey20"),
    )
}

# Define plotting theme for facet_wrap map with years
theme_facet_map <- function(base_size = 10, base_family = "") {
  theme_light(base_size = 10, base_family = "") +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.text = element_text(size = 6),
      strip.background = element_rect(fill = "gray95"),
      strip.text = element_text(margin = margin(b = 2, t = 2), color = "grey10", size = 9), 
      #legend.position = c(0.82, 0.04),
      legend.position = "bottom"
      #legend.direction = "horizontal"
    )
}

# Make default base map plot
#sf::st_boundary(swe_coast_proj)

xmin2 <- -61896.44*1.005
xmax2 <- 893074.5*0.91
ymin2 <- 5983578*1.025
ymax2 <- 6691902*0.99

plot_map <-
  ggplot(swe_coast_proj) +
  xlim(xmin2, xmax2) +
  ylim(ymin2, ymax2) +
  labs(x = "Longitude", y = "Latitude") +
  geom_sf(size = 0.3) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_plot() +
  #guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
  NULL

plot_map_west <-
  ggplot(swe_coast_proj) +
  xlim(200000, xmax2*0.45) +
  ylim(ymin2*1.015, ymax2*0.99) +
  labs(x = "Longitude", y = "Latitude") +
  geom_sf(size = 0.3) +
  theme_plot() +
  theme(axis.text.x = element_text(angle = 90)) +
  #guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
  NULL

# dd <- d %>% mutate(keep = ifelse(lon > 10.18, "Y", "N"),
#                    keep = ifelse(lat < 56.5 & lon < 11.5, "N", keep))

# plot_map_west + 
#   geom_point(data = dd, aes(X, Y, color = keep, shape = trend_area)) +
#   geom_sf(size = 0.1) +  
#   facet_wrap(~Year, ncol = 3) +
#   NULL

# plot_map_fc <- 
#   ggplot(swe_coast_proj) + 
#   xlim(xmin2, xmax2) +
#   ylim(ymin2, ymax2) +
#   labs(x = "Longitude", y = "Latitude") +
#   geom_sf(size = 0.3) + 
#   theme_facet_map() +
#   NULL
# 
# 
# plot_map_labels <- 
#   plot_map + 
#   annotate("text", label = "Sweden", x = xmin2 + 0.25*xrange, y = ymin2 + 0.75*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Denmark", x = xmin2 + 0.029*xrange, y = ymin2 + 0.32*yrange, color = "black", size = 1.9, angle = 75) +
#   annotate("text", label = "Germany", x = xmin2 + 0.07*xrange, y = ymin2 + 0.022*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Poland", x = xmin2 + 0.55*xrange, y = ymin2 + 0.08*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Russia", x = xmin2 + 0.95*xrange, y = ymin2 + 0.18*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Lithuania", x = xmin2 + 1*xrange, y = ymin2 + 0.43*yrange, color = "black", size = 1.9, angle = 75) +
#   annotate("text", label = "Latvia", x = xmin2 + 0.99*xrange, y = ymin2 + 0.65*yrange, color = "black", size = 1.9, angle = 75)
# 
# plot_map_labels_fc <- 
#   plot_map_fc + 
#   annotate("text", label = "Sweden", x = xmin2 + 0.25*xrange, y = ymin2 + 0.75*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Denmark", x = xmin2 + 0.029*xrange, y = ymin2 + 0.32*yrange, color = "black", size = 1.9, angle = 75) +
#   annotate("text", label = "Germany", x = xmin2 + 0.07*xrange, y = ymin2 + 0.022*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Poland", x = xmin2 + 0.55*xrange, y = ymin2 + 0.08*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Russia", x = xmin2 + 0.95*xrange, y = ymin2 + 0.18*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Lithuania", x = xmin2 + 1*xrange, y = ymin2 + 0.43*yrange, color = "black", size = 1.9, angle = 75) +
#   annotate("text", label = "Latvia", x = xmin2 + 0.99*xrange, y = ymin2 + 0.65*yrange, color = "black", size = 1.9, angle = 75)

