### first data visualization ###
library(readxl)
library(skimr)
library(ggplot2)
library(dplyr)
library(leaflet)
library(patchwork)

#importing all data
data <- read_excel("tropical_9_1_2025.xlsx", 
                   sheet = "all_data_organized")
View(data)
summary(data)

#character
data$Publication_date <- as.character(data$Publication_date)
#numeric
data[ , c(10, 11, 29:32,34:41,43,44,46,47)] <- lapply(data[ , c(10, 11, 29:32,34:41,43,44,46,47)], as.numeric)

####

#Overview per PAPER-------------------------------------------------------------

#filtering per title
data_p<- data %>%
  distinct(Title, .keep_all = TRUE)
View(data_p)
summary(data_p)

skim(data_p)

length(data_p$Title)#72 papers
length(unique(data_p$Region_Country))#26 countries
prop.table(table(data_p$Continent))
prop.table(table(data_p$Language))
prop.table(table(data_p$Vegetetion_type))
prop.table(table(data_p$Region_Country))
prop.table(table(data_p$Hemisphere))
prop.table(table(data_p$Short_long_term))
prop.table(table(data_p$Urban_scale))
prop.table(table(data_p$Carbon_metric))
prop.table(table(data_p$Publication_date))

#categorical variables
categorical_p <- names(data_p)[sapply(data_p, function(x) is.factor(x) | is.character(x))]

categorical_p <- categorical_p[categorical_p %in% c("Language","Publication_date","Short_long_term","Hemisphere", "Region_Country", "Seasons", "Urban_scale", "Scale_of_measurement", "Carbon_metric", "Continent")]

# Loop for frequency charts
for (col in categorical_p) {
  print(
    ggplot(data_p, aes_string(x = col)) +
      geom_bar(fill = "orange") +
      geom_text(stat = "count", aes(label = ..count..), vjust = 1.5, color = "black") +
      labs(title = paste("Frequency of", col),
           x = col, y = "Frequency") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

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
##chart for publication---------------------------------------------------------
(pub_chart <- subset(dados_para_grafico, Publication_date < 2025)
  %>%
  ggplot(aes(x = Publication_date, y = Frequencia_Publicacoes)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(fill = "darkorange",
             color = "black",   # contorno
             shape = 21, size = 2) +
  labs(
    x = "Year",
    y = "Number of publications"
  ) +
  scale_y_continuous(
    labels = scales::comma_format(accuracy = 1),
    breaks = scales::breaks_pretty(n = 8) 
  ) +
  theme_minimal() +
    ggtitle("(B)") + coord_fixed())



#Overview per CITY--------------------------------------------------------------

#importing city data
data_c <- read_excel("tropical_9_1_2025.xlsx", 
                     sheet = "preview_city")
View(data_c)
summary(data_c)

data_c$Publication_date <- as.character(data_c$Publication_date)

#population classes
limites <- c(0, 100000, 250000, 500000, 1000000, 5000000, Inf)

# Defina os rótulos (nomes) que cada classe terá = 'Número de Limites - 1'.
rotulos <- c("Small (0-100,000)", "Medium-size (100,000-250,000)", "Large (250,000-500,000)", "Metropolitan (500,000-1 million)", "Large Metropolitan (1-5 million)", "Global (> 5 million)")

data_c <- data_c%>%
  mutate(pop_classes = cut(
    x = `Population(hab)`,
    breaks = limites,
    labels = rotulos,
    include.lowest = TRUE,
    right = TRUE
    # right = TRUE: (0, 10]. right = FALSE: [0, 10)
  )
  )

prop.table(table(data_c$pop_classes))

#data overview per city
skim(data_c)

length(data_c$Title)#90 occurences
length(unique(data_c$City))#77 cities
table(data_c$City)

#categorical variables
categorical_c<- names(data_c)[sapply(data_c, function(x) is.factor(x) | is.character(x))]

categorical_c <- categorical_c[categorical_c %in% c("City", "Climatic_region", "Biome")]

# Loop for frequency charts
for (col in categorical_c) {
  print(
    ggplot(data_p, aes_string(x = col)) +
      geom_bar(fill = "lightblue") +
      geom_text(stat = "count", aes(label = ..count..), vjust = 1.5, color = "black") +
      labs(title = paste("Frequency of", col),
           x = col, y = "Frequency") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

prop.table(table(data_c$City))
prop.table(table(data_c$Climatic_region))
prop.table(table(data_c$Biome))
prop.table(table(data_c$Continent))
prop.table(table(data_c$pop_classes))


### lat long in word map
#setting as numeric data
data_c$Latitude <- as.numeric(as.character(data_c$Latitude))
data_c$Longitude <- as.numeric(as.character(data_c$Longitude))

##normal map---------------------------------------------------------------------
(p <- ggplot() +
  borders("world", colour = "gray80", fill = "gray90") +
  geom_point(data = data_c, aes(x = Longitude, y = Latitude),
             fill = "darkorange",
             color = "black",   # contorno
             shape = 21,        # círculo com contorno + preenchimento
             size = 2) +
  geom_hline(yintercept = 23.5, linetype = "dashed", color = "black") +
  geom_hline(yintercept = -23.5, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid" , color = "black") +
  coord_quickmap() +
  theme_bw()+
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("(A)"))

ggsave("map.png", plot = p, dpi = 300, width = 8, height = 6, units = "in")

#old map and publication chart:
m <- (p / pub_chart)#ver como arrumar o tamanho dos graficos
ggsave("map_pub.png", plot = m, dpi = 300, width = 7.3, height = 7.4, units = "in")


#interative map
leaflet(data_c) %>%
  addTiles() %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    radius = 5,
    color = "magenta",
    fillOpacity = 0.7,
    popup = ~paste("Lat:", round(Latitude, 4), "<br>Lon:", round(Longitude, 4))) %>% #click
  addPolylines(lng = c(-180, 180), lat = c(23.45, 23.45),
               color = "black", weight = 2, dashArray = "5,5") %>%
  addPolylines(lng = c(-180, 180), lat = c(-23.45, -23.45),
               color = "black", weight = 2, dashArray = "5,5")


####

#Overview by data point (habitat/urban/vegetation)------------------------------
#normality test

cols <- c(29:32,34:41,43,44,46,47)

for (i in cols) {
  data[[i]] <- as.numeric(data[[i]])
}

results <- list()

for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    test <- shapiro.test(data[[col]])
    results[[col]] <- test$p.value
  }
}

for (col in names(results)) {
  if (results[[col]] < 0.05) {
    cat(paste("Variable", col, "is not normal (p-value:", results[[col]], ")\n"))
  } else {
    cat(paste("Variable", col, "is normal (p-value:", results[[col]], ")\n"))
  }
}

#scanning data
skim(data)

prop.table(table(data$Urbanization_intensity_class))
prop.table(table(data$Habitat))
prop.table(table(data$Vegetetion_type))
prop.table(table(data$Climatic_region))
prop.table(table(data$Biome))

#visually testing: CARBON STOCK
hist(data$`Carbon_ton/year`)
hist(data$`B_area_ton/ha`)
hist(data$`Population(hab)`)
boxplot(data$`C_area_ton/ha`~ data$Urbanization_intensity_class)
boxplot(data$`C_area_ton/ha` ~ data$Habitat)
boxplot(data$`C_area_ton/ha` ~ data$Biome)
plot(data$`C_area_ton/ha`~ data$`Pluviosity(average annual rainfall-mm)`)
plot(data$`C_area_ton/ha`~ data$`Temperature(average annual temperature-°C)`)
plot(data$`C_area_ton/ha`~ data$`Population(hab)`)
plot(data$`C_area_ton/ha`~ data$`Densi_pop(hab/km2)`)
plot(data$`C_area_ton/ha`~ data$`B_area_ton/ha`)

#visually testing: CARBON SEQUESTRATION
hist(data$`C_area_ton/ha`)
boxplot(`Carbon_ton/year/ha`~ Urbanization_intensity_class, data = subset(data,`Carbon_ton/year/ha`< 100))
boxplot(data$`Carbon_ton/year/ha` ~ data$Habitat)
plot(`Carbon_ton/year/ha`~ `Pluviosity(average annual rainfall-mm)`, data = data)
plot(`Carbon_ton/year/ha`~ `Temperature(average annual temperature-°C)`, data = data)
plot(`Carbon_ton/year/ha`~ `Population(hab)`, data = data)
plot(`Carbon_ton/year/ha`~ `Densi_pop(hab/km2)`, data = data)

plot(`C_area_ton/ha`~`Carbon_ton/year/ha`, data = data)
plot(`C_area_ton/ha`~`Carbon_ton/year/ha`, data = subset(data,`Carbon_ton/year/ha`< 15))#the point higher than 15 is from a confused paper, should I delete it?

plot(`C_area_ton/ha`~ Latitude, data = data)

##interactions between explanatory variables:------------------------------------
plot(data$`Densi_pop(hab/km2)`~ data$`Population(hab)`)#no
plot(data$`Temperature(average annual temperature-°C)`~ data$`Pluviosity(average annual rainfall-mm)`)#maybe
boxplot(data$`Temperature(average annual temperature-°C)`~ data$Urbanization_intensity_class)#maybe
boxplot(data$`Pluviosity(average annual rainfall-mm)`~ data$Urbanization_intensity_class)#maybe
boxplot(data$`Temperature(average annual temperature-°C)`~ data$Habitat)#no
boxplot(data$`Pluviosity(average annual rainfall-mm)`~ data$Habitat)#yes

boxplot(data$`Densi_pop(hab/km2)`~ data$Habitat)#maybe
boxplot(data$`Population(hab)`~ data$Habitat)#yes
boxplot(data$`Densi_pop(hab/km2)`~ data$Urbanization_intensity_class)#no
boxplot(data$`Population(hab)`~ data$Urbanization_intensity_class)#no
boxplot(data$`Densi_pop(hab/km2)`~ data$Habitat)#maybe

plot(data$`Temperature(average annual temperature-°C)`~ data$`Densi_pop(hab/km2)`)#no
plot(data$`Temperature(average annual temperature-°C)`~ data$`Population(hab)`)#no
plot(data$`Pluviosity(average annual rainfall-mm)`~ data$`Densi_pop(hab/km2)`)#no
plot(data$`Pluviosity(average annual rainfall-mm)`~ data$`Population(hab)`)#no


#data overview
length(unique(data$City))#77 cities
length(unique(data$Region_Country))#26 countries

categorical <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x))]

#filtering
categorical <- categorical[categorical %in% c("Climatic_region", "Biome", "Habitat", "Urbanization_intensity_class","Vegetation_type")]

##Loop for charts

for (col in categorical) {
  print(
    ggplot(data, aes_string(x = col)) +
      geom_bar(fill = "green4") +
      labs(title = paste("Frequency of", col),
           x = col, y = "Frequency") +
      geom_text(stat = "count", aes(label = ..count..), vjust = 1.5, color = "black") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

#filtering data by habitat type:
df_build <- data %>% filter(Habitat == "Build-up")
df_crop <- data %>% filter(Habitat == "Croplands")
df_est <- data %>% filter(Habitat == "Estuary")
df_forest <- data %>% filter(Habitat == "Forest")
df_gard <- data %>% filter(Habitat == "Gardens")
df_park <- data %>% filter(Habitat == "Parks")
df_ugs <- data %>% filter(Habitat == "Urban green spaces (UGS)")

skim(df_build)
View(df_gard)
skim(df_forest)

#filtering data by urban class:
df_urban <- data %>% filter(Urbanization_intensity_class == "Urban")
df_rural <- data %>% filter(Urbanization_intensity_class == "Rural")
df_peri <- data %>% filter(Urbanization_intensity_class == "Periurban")
df_urb_pe <- data %>% filter(Urbanization_intensity_class == "Urban-Periurban")
df_urb_ru <- data %>% filter(Urbanization_intensity_class == "Urban-Rural")

plot(`C_area_ton/ha`~ `Pluviosity(average annual rainfall-mm)`, data = df_urban)
plot(`C_area_ton/ha`~ `Temperature(average annual temperature-°C)`, data = df_urban)
boxplot(`C_area_ton/ha` ~ Habitat, data = df_urb_ru)

plot(`C_area_ton/ha`~ `Pluviosity(average annual rainfall-mm)`, data = df_peri)
plot(`C_area_ton/ha`~ `Temperature(average annual temperature-°C)`, data = df_urban)


## separating stock and sequestration data sets:

#C sequestration (ton/year/ha) 
sequestration <- data %>%
  filter(!is.na(`Carbon_ton/year/ha`))
View(sequestration)# 40 data points
skim(sequestration)# 22 studies
length(unique(sequestration$Title))
prop.table(table(sequestration$Urbanization_intensity_class))#no rural points
prop.table(table(sequestration$Habitat))#no garden points

s_urban <- sequestration %>% filter(Urbanization_intensity_class == "Urban")
table(s_urban$Habitat)
s_peri <- sequestration %>% filter(Urbanization_intensity_class == "Periurban")
table(s_peri$Habitat)
s_urb_ru <- sequestration %>% filter(Urbanization_intensity_class == "Urban-Rural")
table(s_urb_ru$Habitat)

#C stock data (ton/ha)
storage <- data %>%
  filter(!is.na(`C_area_ton/ha`))
View(storage)# 99 data points
skim(storage)# 60 studies
length(unique(storage$Title))
prop.table(table(storage$Urbanization_intensity_class))
prop.table(table(storage$Habitat))

st_urban <- storage %>% filter(Urbanization_intensity_class == "Urban")
table(st_urban$Habitat)
se2 <- sd(st_urban$`C_area_ton/ha`) / sqrt(length(st_urban$`C_area_ton/ha`))
st_peri <- storage %>% filter(Urbanization_intensity_class == "Periurban")
table(st_peri$Habitat)
se3 <- sd(st_peri$`C_area_ton/ha`) / sqrt(length(st_peri$`C_area_ton/ha`))
st_rural <- storage %>% filter(Urbanization_intensity_class == "Rural")
table(st_rural$Habitat)
se4 <- sd(st_rural$`C_area_ton/ha`) / sqrt(length(st_rural$`C_area_ton/ha`))
st_urb_pe <- storage %>% filter(Urbanization_intensity_class == "Urban-Periurban")
table(st_urb_pe$Habitat)
st_urb_ru <- storage %>% filter(Urbanization_intensity_class == "Urban-Rural")
table(st_urb_ru$Habitat)
se5 <- sd(st_urb_ru$`C_area_ton/ha`) / sqrt(length(st_urb_ru$`C_area_ton/ha`))

#data of C stock and sequestration
seq_stock <- storage %>%
  filter(!is.na(`Carbon_ton/year/ha`))
View(seq_stock)# 29 data points
skim(seq_stock)
table(sequestration$pop_classes)

prop.table(table(seq_stock$Urbanization_intensity_class))#no rural points -all urban scale-
prop.table(table(seq_stock$Habitat))#build-up, estuary (1), forest and UGS

####

##population classes------------------------------------------------------------
limites <- c(0, 100000, 250000, 500000, 1000000, 5000000, Inf)

# Defina os rótulos (nomes) que cada classe terá = 'Número de Limites - 1'.
rotulos <- c("Small (0-100,000)", "Medium-size (100,000-250,000)", "Large (250,000-500,000)", "Metropolitan (500,000-1 million)", "Large Metropolitan (1-5 million)", "Global (> 5 million)")

data_classes <- data%>%
  mutate(pop_classes = cut(
      x = `Population(hab)`,
      breaks = limites,
      labels = rotulos,
      include.lowest = TRUE,
      right = TRUE
      # right = TRUE: (0, 10]. right = FALSE: [0, 10)
    )
  )

prop.table(table(data_classes$pop_classes))

boxplot(data_classes$`C_area_ton/ha`~ data_classes$pop_classes)
boxplot(data_classes$`Carbon_ton/year/ha`~ data_classes$pop_classes)

####

##averages for numeric variables (all data)-------------------------------------

#pop density
mean(data$`Densi_pop(hab/km2)`, na.rm = T)#5093.4
sd(data$`Densi_pop(hab/km2)`, na.rm = T)#5957.141
max(data$`Densi_pop(hab/km2)`, na.rm = T)
min(data$`Densi_pop(hab/km2)`, na.rm = T)

#pluviosity
mean(data$`Pluviosity(average annual rainfall-mm)`, na.rm = T)#1588.75
sd(data$`Pluviosity(average annual rainfall-mm)`, na.rm = T)#829.1909

#temperature
mean(data$`Temperature(average annual temperature-°C)`, na.rm = T)#24.13
sd(data$`Temperature(average annual temperature-°C)`, na.rm = T)#3.97
se0 <- sd(data$`Temperature(average annual temperature-°C)`) / sqrt(length(data$`Temperature(average annual temperature-°C)`))
se0

#area
mean(data$Area_ha, na.rm = T)#113221.2
sd(data$Area_ha, na.rm = T)#760282

#cities population
mean(data$`Population(hab)`, na.rm = T)#2010100
sd(data$`Population(hab)`, na.rm = T)#2937916
max(data$`Population(hab)`, na.rm = T)
min(data$`Population(hab)`, na.rm = T)


#C stock
storage <- data %>%
  filter(!is.na(`C_area_ton/ha`))
mean(storage$`C_area_ton/ha`)#89.77
sd(storage$`C_area_ton/ha`)#122.91
se <- sd(storage$`C_area_ton/ha`) / sqrt(length(storage$`C_area_ton/ha`))
se
max(data$`C_area_ton/ha`, na.rm = T)#690.57
min(data$`C_area_ton/ha`, na.rm = T)#0.003



#URBAN
mean(df_urban$`C_area_ton/ha`, na.rm = T)#74.90
sd(df_urban$`C_area_ton/ha`, na.rm = T)#122.92
#RURAL
mean(df_rural$`C_area_ton/ha`, na.rm = T)#102.32
sd(df_rural$`C_area_ton/ha`, na.rm = T)#66.46
#PERIURBAN
mean(df_peri$`C_area_ton/ha`, na.rm = T)#80.44
sd(df_peri$`C_area_ton/ha`, na.rm = T)#102.12
#URB-RUR
mean(df_urb_ru$`C_area_ton/ha`, na.rm = T)#125.25
sd(df_urb_ru$`C_area_ton/ha`, na.rm = T)#150.41


#BUILD-UP
mean(df_build$`C_area_ton/ha`, na.rm = T)#75.77
sd(df_build$`C_area_ton/ha`, na.rm = T)#104.54
summary(df_build$`C_area_ton/ha`)
#FOREST
mean(df_forest$`C_area_ton/ha`, na.rm = T)#103.22
sd(df_forest$`C_area_ton/ha`, na.rm = T)#128.44
summary(df_forest$`C_area_ton/ha`)
#UGS
mean(df_ugs$`C_area_ton/ha`, na.rm = T)#30.17
sd(df_ugs$`C_area_ton/ha`, na.rm = T)#48.39
summary(df_ugs$`C_area_ton/ha`)
#PARK
mean(df_park$`C_area_ton/ha`, na.rm = T)#219.37
sd(df_park$`C_area_ton/ha`, na.rm = T)#233
summary(df_park$`C_area_ton/ha`)
#CROPLAND
mean(df_crop$`C_area_ton/ha`, na.rm = T)#105.74
sd(df_crop$`C_area_ton/ha`, na.rm = T)#84.22
#GARDEN
mean(df_gard$`C_area_ton/ha`, na.rm = T)#104.7
sd(df_gard$`C_area_ton/ha`, na.rm = T)#32.10
#ESTUARY
mean(df_est$`C_area_ton/ha`, na.rm = T)#120.70
sd(df_est$`C_area_ton/ha`, na.rm = T)#119.22

#C sequestration
sequestration <- data %>%
  filter(!is.na(`Carbon_ton/year/ha`))
mean(sequestration$`Carbon_ton/year/ha`)#2.92
sd(sequestration$`Carbon_ton/year/ha`)#3.82
se1 <- sd(sequestration$`Carbon_ton/year/ha`) / sqrt(length(sequestration$`Carbon_ton/year/ha`))
se1
max(data$`Carbon_ton/year/ha`, na.rm = T)#16.07
min(data$`Carbon_ton/year/ha`, na.rm = T)#0.01

##
#URBAN
mean(df_urban$`Carbon_ton/year/ha`, na.rm = T)#2.93
sd(df_urban$`Carbon_ton/year/ha`, na.rm = T)#4.36
#PERIURBAN
mean(df_peri$`Carbon_ton/year/ha`, na.rm = T)#2.42
sd(df_peri$`Carbon_ton/year/ha`, na.rm = T)#1.75
#URB-RUR
mean(df_urb_ru$`Carbon_ton/year/ha`, na.rm = T)#3.42
sd(df_urb_ru$`Carbon_ton/year/ha`, na.rm = T)#2.73


#total biomass
mean(data$`B_area_ton/ha`, na.rm = T)#186.18
sd(data$`B_area_ton/ha`, na.rm = T)#278.32

#CO2 sequestration
mean(data$`CO2_ton/year/ha`, na.rm = T)#10.75
sd(data$`CO2_ton/year/ha`, na.rm = T)#14.02


