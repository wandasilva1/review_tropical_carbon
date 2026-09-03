library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)

#importing data
data <- read_excel("tropical_4_23_2026_newarticles.xlsx", 
                     sheet = "all_data_organized")
### lat long in word map
#setting as numeric data
data$Latitude <- as.numeric(as.character(data$Latitude))
data$Longitude <- as.numeric(as.character(data$Longitude))

#filtering per title
data_c <- data %>%
  distinct(Latitude, .keep_all = TRUE)

# Creating column for continents
data_c <- data_c %>%
  mutate(Continent = case_when(
    Region_Country %in% c("India", "Malaysia", "Indonesia", "Thailand", "Singapore", 
                          "Vietnam", "Bangladesh", "Myanmar", "Philippines", "China","Saudi Arabia") ~ "Asia",
    
    
    Region_Country %in% c("Brazil", "Bolivia", "Colombia", "Ecuador", "Peru") ~ "South America",
    
    Region_Country %in% c("Mexico", "Cuba", "Puerto Rico") ~ "North America",
    
    Region_Country %in% c("Zambia", "Nigeria", "Ethiopia", "Benin", "Ghana", 
                          "Burkina Faso", "Cameroon", "Niger", "Ivory Coast") ~ "Africa",
    
    
    TRUE ~ "Other"
  ))

# Defining colors and shape
continent_colors <- c(
  "Africa" = "#009E73",
  "Asia" = "#E69F00",
  "North America" = "#56B4E9", 
  "South America" = "#D55E00"
)

continent_shape <- c(
  "Africa" = 23,
  "Asia" = 21,
  "North America" = 22, 
  "South America" = 24
)

#map cities---------------------------------------------------------------------
# Zoom in the tropical area
p <- ggplot() +
  annotation_borders("world", colour = "gray80", fill = "gray90") +
  geom_point(data = data_c, aes(x = Longitude, y = Latitude, fill = Continent, shape = Continent),
             color = "black",
             size = 2.5,
             stroke = 0.5) +
  geom_hline(yintercept = 23.5, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = -23.5, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  scale_fill_manual(
    name = " ",
    values = continent_colors,
    breaks = names(continent_colors)
  ) +
  
  scale_shape_manual(
    name = " ",
    values = continent_shape,
    breaks = names(continent_shape)
  ) +
  coord_quickmap(xlim = c(-110, 140), ylim = c(-35, 35)) +  # Zoom
  theme_bw(base_size = 13) +
  labs(x = "Longitude", y = "Latitude", fill = " ") +
  ggtitle("(a)") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text  = element_text(size = 12))
p


#importing all data
data <- read_excel("tropical_4_23_2026_newarticles.xlsx", 
                   sheet = "all_data_organized")

#filtering per title
data_p <- data %>%
  distinct(Title, .keep_all = TRUE)

#publication date line chart
dados_para_grafico <- data_p %>%
  mutate(Publication_date = as.integer(Publication_date)) %>%
  group_by(Publication_date) %>%
  summarise(
    Frequencia_Publicacoes = n(), 
    .groups = 'drop' 
  )# %>%
#filter(!is.na(Publication_date))
zeros <- tibble(
  Publication_date = c(2011, 2012),
  Frequencia_Publicacoes = c(0, 0)
)
dados_para_grafico<- bind_rows(dados_para_grafico, zeros)

dados_linha <- dados_para_grafico %>%
  arrange(Publication_date) %>%
  slice(1:(n() - 1))

ultimo_ponto <- dados_para_grafico %>%
  filter(Publication_date == max(Publication_date))

ultimo_segmento <- dados_para_grafico %>%
  arrange(Publication_date) %>%
  summarise(
    x    = Publication_date[n() - 1],
    y    = Frequencia_Publicacoes[n() - 1],
    xend = Publication_date[n()],
    yend = Frequencia_Publicacoes[n()]
  )


#chart publication--------------------------------------------------------------
(pub_chart <- dados_para_grafico %>%
   ggplot() +
   geom_line(
     data = dados_linha,
     aes(x = Publication_date, y = Frequencia_Publicacoes),
     color = "black",
     linewidth = 1
   ) +
   geom_segment(
     data = ultimo_segmento,
     aes(x = x, y = y, xend = xend, yend = yend),
    # linetype = "dashed",
     linewidth = 1,
     color = "black"
   ) +
   geom_point(
     data = dados_para_grafico,
     aes(x = Publication_date, y = Frequencia_Publicacoes),
     shape = 21, fill = "black", color = "black", size = 2
   ) +
   labs( x = "Year", y = "Number of publications in tropical zone" ) +
   #geom_text(
     #data = ultimo_segmento,
     #aes(x = xend, y = yend, label = "*"),
     #vjust = 1.5,
     #size = 8)+
   scale_y_continuous(labels = scales::label_number(accuracy = 1))+
   theme_minimal(base_size = 13) +
   theme(
     panel.grid = element_blank(),
     panel.background = element_rect(fill = "white", color = NA),
     plot.background  = element_rect(fill = "white", color = NA),
     axis.line = element_line(color = "black"),
     axis.ticks = element_line(color = "black"),
     axis.ticks.length = unit(0.15, "cm")
   ) +
   scale_x_continuous(
     limits = c(2009, 2025),
     breaks = seq(2009, 2025, by = 4),
     expand = expansion(mult = c(0.1, 0.1))
   )
 + ggtitle("(b)")
 + coord_fixed(ratio = 0.5)
 
)


#saving together----------------------------------------------------------------

library(patchwork)
(m <- (p / pub_chart + plot_layout(heights = c(1,0.9), widths = c(1, 2))))


ggsave("map_fig2_2026_review.png", plot = m, dpi = 300, width = 7.3, height = 7.6, units = "in")

#saving in pdf
ggsave(
  "map_fig2_2026_review.pdf",
  plot = m,
  width = 7.3,
  height = 7.6,
  units = "in",
  device = cairo_pdf
)
