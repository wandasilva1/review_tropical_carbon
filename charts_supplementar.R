# Charts for supplementary #----
library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)

#importing all data
data <- read_excel("tropical_9_1_2025.xlsx", 
                   sheet = "all_data_organized")

#character
data$Publication_date <- as.character(data$Publication_date)
#numeric
data[ , c(10, 11, 29:32,34:41,43,44,46,47)] <- lapply(data[ , c(10, 11, 29:32,34:41,43,44,46,47)], as.numeric)

####

#filtering per title
data_p<- data %>%
  distinct(Title, .keep_all = TRUE)

##chart for continent-----------------------------------------------------------
cont_count <- data_p %>%
  dplyr::count(Continent)

(g_bar_h <- ggplot(data = cont_count, aes(x = reorder(Continent, n), y = n)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = n),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black",
                               legend.position = "none",
                               plot.title.position = "plot",      # título alinhado à esquerda
                               plot.title = element_text(hjust = 0) # 0 = esquerda, 0.5 = centro, 1 = direita
      )) +
    labs(x = NULL, 
         y = "Number of publications"))

#saving
ggsave("cont_bar.png", plot = g_bar_h, dpi = 300, width = 5, height = 4, units = "in")

##chart for country-----------------------------------------------------------
coun_count <- data_p %>%
  dplyr::count(Region_Country)
(country_bar <- ggplot(data = coun_count, aes(x = reorder(Region_Country, n), y = n)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = n),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black",
                               legend.position = "none",
                               plot.title.position = "plot",      # título alinhado à esquerda
                               plot.title = element_text(hjust = 0) # 0 = esquerda, 0.5 = centro, 1 = direita
      )) +
    labs(x = NULL, 
         y = "Number of publications"))

#saving
ggsave("coun_bar.png", plot = country_bar, dpi = 300, width = 5, height = 6, units = "in")

##chart for cities-----------------------------------------------------------
city_count <- data_p %>%
  dplyr::count(City)
(city_bar <- ggplot(data = city_count, aes(x = reorder(City, n), y = n)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = n),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black",
                               legend.position = "none",
                               plot.title.position = "plot",      # título alinhado à esquerda
                               plot.title = element_text(hjust = 0) # 0 = esquerda, 0.5 = centro, 1 = direita
      )) +
    labs(x = NULL, 
         y = "Number of publications"))

#saving
ggsave("city_bar.png", plot = city_bar, dpi = 300, width = 5, height = 9, units = "in")

##chart for above/below-----------------------------------------------------------
carb_count <- data_p %>%
  dplyr::count(Carbon_metric)
(carb_bar <- ggplot(data = carb_count, aes(x = reorder(Carbon_metric, n), y = n)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = n),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black",
                               legend.position = "none",
                               plot.title.position = "plot",      # título alinhado à esquerda
                               plot.title = element_text(hjust = 0) # 0 = esquerda, 0.5 = centro, 1 = direita
      )) +
    labs(x = NULL, 
         y = "Number of publications"))

#saving
ggsave("city_bar.png", plot = city_bar, dpi = 300, width = 5, height = 9, units = "in")

##chart for sampling method-----------------------------------------------------
name <- c("Remote sensing", "Field sampling")
num <- c(22, 56)

method <- data.frame(name, num)

(meth_bar <- ggplot(data = method, aes(x = reorder(name, num), y = num)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = num),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black",
                               legend.position = "none",
                               plot.title.position = "plot",      # título alinhado à esquerda
                               plot.title = element_text(hjust = 0) # 0 = esquerda, 0.5 = centro, 1 = direita
      )) +
    labs(x = NULL, 
         y = "Number of publications"))

#saving
ggsave("meth_bar.png", plot = meth_bar, dpi = 300, width = 5, height = 4, units = "in")

##chart for estimation method---------------------------------------------------
name1 <- c("Destructive", "Allometric equations")
num1 <- c(3, 69)
method1 <- data.frame(name1, num1)

(est_bar <- ggplot(data = method1, aes(x = reorder(name1, num1), y = num1)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = num1),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black",
                               legend.position = "none",
                               plot.title.position = "plot",      # título alinhado à esquerda
                               plot.title = element_text(hjust = 0) # 0 = esquerda, 0.5 = centro, 1 = direita
      )) +
    labs(x = NULL, 
         y = "Number of publications"))

#saving
ggsave("est_bar.png", plot =est_bar, dpi = 300, width = 5, height = 4, units = "in")

##chart for measurements---------------------------------------------------------
name2 <- c("DBH", "Total height", "Wood density", "Crown diameter", "Vegetation cover")
num2 <- c(55, 56, 10, 5, 13)
method2 <- data.frame(name2, num2)

(meas_bar <- ggplot(data = method2, aes(x = reorder(name2, num2), y = num2)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = num2),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black",
                               legend.position = "none",
                               plot.title.position = "plot",      # título alinhado à esquerda
                               plot.title = element_text(hjust = 0) # 0 = esquerda, 0.5 = centro, 1 = direita
      )) +
    labs(x = NULL, 
         y = "Number of publications"))

#saving
ggsave("meas_bar.png", plot = meas_bar, dpi = 300, width = 5, height = 4, units = "in")

#importing city data
data_c <- read_excel("tropical_9_1_2025.xlsx", 
                     sheet = "preview_city")

##chart for clima--------------------------------------------------------
clima_count <- data_c %>%
  dplyr::count(Climatic_region)

(g_bar <- ggplot(data = clima_count, aes(x = reorder(Climatic_region, n), y = n)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = n),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black",
                               legend.position = "none",
                               plot.title.position = "plot",      # título alinhado à esquerda
                               plot.title = element_text(hjust = 0) # 0 = esquerda, 0.5 = centro, 1 = direita
      )) +
    labs(x = NULL, 
         y = "Number of cities"))

#saving
ggsave("clima_bar.png", plot = g_bar, dpi = 300, width = 6.5, height = 4, units = "in")

##chart for biome---------------------------------------------------------------
bio_count <- data_c %>%
  dplyr::count(Biome)

(g_bio <- ggplot(
  data = bio_count, 
  aes(x = reorder(Biome, n), y = n)
) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = n),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black",
                               legend.position = "none",
                               plot.title.position = "plot",      # título alinhado à esquerda
                               plot.title = element_text(hjust = 0) # 0 = esquerda, 0.5 = centro, 1 = direita
      )) +
    labs(
      x = NULL,              # remove o nome do eixo X (que agora é o eixo Y visualmente)
      y = "Number of cities"# mantém o rótulo do eixo X original
    ))

#saving
ggsave("bio_bar.png", plot = g_bio, dpi = 300, width = 7.5, height = 4, units = "in")

#climate and biome together:
cli_bio <- g_bar / g_bio
ggsave("climate_bio_bw.png", plot = cli_bio, dpi = 300, width = 7, height = 6, units = "in")

##chart for city size-----------------------------------------------------------

#Defining classes:
rotulos <- c("Small", "Medium-size", "Large", "Metropolitan", "Large Metropolitan", "Global-size")
limites <- c(0, 100000, 250000, 500000, 1000000, 5000000, Inf)

data <- data%>%
  mutate(pop_classes = cut(
    x = `Population(hab)`,
    breaks = limites,
    labels = rotulos,
    include.lowest = TRUE,
    right = TRUE
    # right = TRUE: (0, 10]. right = FALSE: [0, 10)
  ))
cs_count <- data %>%
  dplyr::count(pop_classes)

(cs_bar <- ggplot(data = cs_count, aes(x = reorder(pop_classes, n), y = n)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = n),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black",
                               legend.position = "none",
                               plot.title.position = "plot",      # título alinhado à esquerda
                               plot.title = element_text(hjust = 0) # 0 = esquerda, 0.5 = centro, 1 = direita
      )) +
    labs(x = NULL, 
         y = "Number of cities"))

#saving
ggsave("cs_bar.png", plot = cs_bar, dpi = 300, width = 5, height = 4, units = "in")

##chart for urban classes-----------------------------------------------------------
ui_count <- data %>%
  dplyr::count(Urbanization_intensity_class)
(ui_bar <- ggplot(data = ui_count, aes(x = reorder(Urbanization_intensity_class, n), y = n)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = n),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black",
                               legend.position = "none",
                               plot.title.position = "plot",      # título alinhado à esquerda
                               plot.title = element_text(hjust = 0) # 0 = esquerda, 0.5 = centro, 1 = direita
      )) +
    labs(x = NULL, 
         y = "Number of sites"))

#saving
ggsave("ui_bar.png", plot = ui_bar, dpi = 300, width = 5, height = 4, units = "in")

##chart for UGS -----------------------------------------------------------
ugs_count <- data %>%
  dplyr::count(Habitat)
(ugs_bar <- ggplot(data = ugs_count, aes(x = reorder(Habitat, n), y = n)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = n),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black",
                               legend.position = "none",
                               plot.title.position = "plot",      # título alinhado à esquerda
                               plot.title = element_text(hjust = 0) # 0 = esquerda, 0.5 = centro, 1 = direita
      )) +
    labs(x = NULL, 
         y = "Number of sites"))

#saving
ggsave("ugs_bar.png", plot = ugs_bar, dpi = 300, width = 5, height = 4, units = "in")

##chart for veg. type-----------------------------------------------------------
veg_count <- data %>%
  dplyr::count(Vegetetion_type)
(veg_bar <- ggplot(data = veg_count, aes(x = reorder(Vegetetion_type, n), y = n)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = n),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black",
                               legend.position = "none",
                               plot.title.position = "plot",      # título alinhado à esquerda
                               plot.title = element_text(hjust = 0) # 0 = esquerda, 0.5 = centro, 1 = direita
      )) +
    labs(x = NULL, 
         y = "Number of sites"))

#saving
ggsave("veg_bar.png", plot = veg_bar, dpi = 300, width = 5, height = 4, units = "in")

