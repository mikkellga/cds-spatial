## Week 03: Projections - Let's Look at Greenland!

# 1. Setup ----------------------------------------------------------------
library(pacman)
p_load(sf, tidyverse, tmap, rnaturalearth, mapview, units, patchwork)

# 2. Load Data ------------------------------------------------------------
# We use rnaturalearth to get country boundaries
# scale = "medium" gives us enough detail without being too heavy
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter for Denmark and Greenland
# Note: In natural earth, Greenland might be listed separately or under Denmark depending on attribute
# Let's check typical codes or just filter by name to be safe
dk_gr <- world %>% 
  filter(admin %in% c("Denmark", "Greenland"))

# 3. Analysis: The Mercator Projection ------------------------------------
# Transform to Web Mercator (EPSG:3857) - The "Google Maps" projection
dk_gl_merc <- dk_gr %>% 
  st_transform(crs = 3857) %>% # Greenland disappears if you do this
  select(admin,pop_est,pop_rank, gdp_md)

plot(dk_gl_merc$geometry)
mapview(dk_gl_merc)

# 4. Visualization: The Lying Map -----------------------------------------
# This visualizes the distortion: Greenland looks massive compared to Denmark
ggplot(data = dk_gl_merc) +
  geom_sf(aes(fill = admin)) +
  scale_fill_viridis_d(name = "Country") +
  theme_minimal() +
  labs(title = "Mercator Projection (EPSG:3857)",
       subtitle = "Greenland vs Denmark",
       caption = "Greenland is massive, Denmark is tiny!")


# 5. Analysis: The Truth (Area Calculation) -------------------------------
# Calculate actual area in square kilometers
# st_area calculates area based on the CRS. 
# If we mistakenly use the projected Mercator unit (sq meters) as is, it's huge distortion.
# But st_area on unprojected (long/lat) or equal-area projected data usually returns m^2 correctly.
# Let's calculate from the original (WGS84) object which sf handles geodesically, 
# or transform to an Equal Area projection like Mollweide (ESRI:54009) or Global Equal Area.

# We will calculate area on the original sf object (usually WGS84) which uses Great Circle references

# --- Greenland in pseudo-Mercator projection --- WGS84 (3587/4326) ---
dk_gr <- dk_gr %>% 
  mutate(area_sqkm = as.numeric(st_area(.)) / 10^6)

# Let's compare the numbers
dk_area <- dk_gr %>% filter(admin == "Denmark") %>% pull(area_sqkm)
gl_area <- dk_gr %>% filter(admin == "Greenland") %>% pull(area_sqkm)

cat("Area of Denmark (sq km):", dk_area, "\n")
cat("Area of Greenland (sq km):", gl_area, "\n")
cat("Greenland is approximately", round(gl_area / dk_area, 1), "times larger than Denmark.\n")


# --- Mollweide (ESRI:54009) or Equal Earth (EPSG:8857) ---
dk_gl_equal <- dk_gr %>% 
  st_transform("ESRI:54009")

# Let's compare the numbers
dk_area_e <- dk_gl_equal %>%  mutate(area_sqkm = as.numeric(st_area(.)) / 10^6) %>% filter(admin == "Denmark") %>% pull(area_sqkm)
gl_area_e <- dk_gl_equal %>%  mutate(area_sqkm = as.numeric(st_area(.)) / 10^6)  %>% filter(admin == "Greenland") %>% pull(area_sqkm)

cat("Area of Denmark under equal area (sq km):", dk_area_e, "\n")
cat("Area of Greenland under equal area  (sq km):", gl_area_e, "\n")
cat("Greenland is approximately", round(gl_area_e / dk_area_e, 1), "times larger than Denmark.\n")

# --- UTM 32 ---
dk_gl_utm <- dk_gr %>% 
  st_transform("EPSG:25832")
dk_area_utm <- dk_gl_utm %>%  mutate(area_sqkm = as.numeric(st_area(.)) / 10^6) %>% filter(admin == "Denmark") %>% pull(area_sqkm)
gl_area_utm <- dk_gl_utm %>%  mutate(area_sqkm = as.numeric(st_area(.)) / 10^6)  %>% filter(admin == "Greenland") %>% pull(area_sqkm)

cat("Area of Denmark in utm projection (sq km):", dk_area_utm, "\n")
cat("Area of Greenland in utm projection (sq km):", gl_area_utm, "\n")
cat("Greenland is approximately", round(gl_area_utm / dk_area_utm, 1), "times larger than Denmark.\n")

# --- UTM 26 ---
dk_gl_utm26 <- dk_gr %>% 
  st_transform("EPSG:32626")
dk_area_utm26 <- dk_gl_utm26 %>%  mutate(area_sqkm = as.numeric(st_area(.)) / 10^6) %>% filter(admin == "Denmark") %>% pull(area_sqkm)
gl_area_utm26 <- dk_gl_utm26 %>%  mutate(area_sqkm = as.numeric(st_area(.)) / 10^6)  %>% filter(admin == "Greenland") %>% pull(area_sqkm)

cat("Area of Denmark in utm26 projection (sq km):", dk_area_utm26, "\n")
cat("Area of Greenland in utm26 projection (sq km):", gl_area_utm26, "\n")
cat("Greenland is approximately", round(gl_area_utm26 / dk_area_utm26, 1), "times larger than Denmark.\n")


# 7. Reflection and Challenge -----------------------------------------------------------
# While Greenland IS bigger, Mercator makes it look even bigger (area distortion increases at poles).
# A good map for comparing size would use an Equal Area projection. 
# Create a map of Greenland that shows the distortions 

# Tissot indicatrix per CRS
tissot_manual <- function(x, n = c(8, 8), radius = 100000) {
  pts <- st_make_grid(st_as_sfc(st_bbox(x)), n = n, what = "centers") |> st_as_sf()
  st_buffer(pts, dist = radius)
}

# Create regular grid of points in lon/lat
panel <- function(x, title, n = c(8, 8)) {
  
  bb   <- st_bbox(x)
  crs  <- st_crs(x)
  
  grat <- st_as_sf(st_graticule(bb, crs = crs))
  tis  <- tissot_manual(x, n = n)   #
  
  ggplot() +
    geom_sf(data = tis,  fill = NA, linewidth = 0.3, alpha = 0.8) +
    geom_sf(data = grat, linewidth = 0.25, alpha = 0.6) +
    geom_sf(data = x, fill = "grey85", color = "grey20", linewidth = 0.35) +
    ggtitle(title) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

p1 <- panel(dk_gl_utm,   "UTM32")
p2 <- panel(dk_gl_equal, "Equal-area")
p3 <- panel(dk_gl_merc,  "WGS84 / Mercator-ish")
p4 <- panel(dk_gl_utm26, "UTM26")

library(patchwork)
p1 + p2 + p3 + p4+ plot_layout(nrow = 2)


# 9. Sequential series of visuals
library(units)

# Define grid (tune spacing for your teaching goal)
lon <- seq(-80,  40, by = 20)
lat <- seq( 50,  85, by = 10)

latlon <- lapply(lat, data.frame, lon) |> bind_rows()
names(latlon) <- c("lat", "lon")

tissots <- latlon |>
  st_as_sf(coords = c("lon", "lat")) |>
  st_set_crs(4326) |>
  st_buffer(dist = set_units(100, "km"))  # start 100 km; try 50 km later

base_map <- function(title, crs_target) {
  ggplot() +
    geom_sf(data = dk_gl_merc, fill = "grey85", color = "grey20", linewidth = 0.35) +
    geom_sf(data = tissots, fill = NA, linewidth = 0.3, alpha = 0.8) +
    coord_sf(crs = crs_target) +
    ggtitle(title) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(face = "bold"))
}

# Plot UTM
p_utm <- base_map(
  "UTM (your dk_gl_utm CRS)",
  st_crs(dk_gl_utm)
)
p_utm


# Plot Mollweide 
p_equal <- base_map(
  "Equal-area (your dk_gl_equal CRS)",
  st_crs(dk_gl_equal)
)
p_equal


# Plot Mercator
p_ll <- base_map(
  "Geographic (EPSG:4326 lon/lat)",
  st_crs(4326)
)
p_ll

# --- Get a better view of Mercator -- 
# Plot Mercator on a sphere (https://rstudio-pubs-static.s3.amazonaws.com/1135653_0fae07141a5a41099d1022c5371a62c2.html)
ggplot()+
  geom_sf(data = world)+
  geom_sf(data = tissots)+
  coord_sf( crs= "+proj=ortho +lat_0=50 +lon_0 = 310")
