#extrating data from raster/shapefile
library(readxl)
library(skimr)
library(ggplot2)
library(dplyr)

#importing all data
data <- read_excel("tropical_9_1_2025.xlsx", 
                   sheet = "all_data_organized")
View(data)

## biome definition (WWF)

lalo <- data[, c("CODE", "Latitude", "Longitude")]

library(sf)
points <- st_as_sf(lalo, coords = c("Longitude", "Latitude"), crs = 4326)
biomas <- st_read("Y:/Home/dasilvaw/private/systematic review/official/wwf_terr_ecos.shp")
biomas <- st_transform(biomas, st_crs(points))#transforming for same crs of points
biomas <- st_make_valid(biomas)#validation to avoid errors
biome <- st_join(points, biomas, join = st_intersects)

View(biome)
#writing biome names
bio <- data.frame(BIOME = 1:14, NOME_BIOMA = c(
  "Tropical & Subtropical Moist Broadleaf Forests",
  "Tropical & Subtropical Dry Broadleaf Forests",
  "Tropical & Subtropical Coniferous Forests",
  "Temperate Broadleaf & Mixed Forests",
  "Temperate Conifer Forests",
  "Boreal Forests/Taiga",
  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
  "Temperate Grasslands, Savannas & Shrublands",
  "Flooded Grasslands & Savannas",
  "Montane Grasslands & Shrublands",
  "Tundra",
  "Mediterranean Forests, Woodlands & Scrub",
  "Deserts & Xeric Shrublands",
  "Mangroves"  )
)

bioma_final <- biome %>%
  left_join(bio, by = "BIOME")
View(bioma_final)

#joining biomes with all data
data <- data %>%
  left_join(bioma_final, by = "CODE") %>%
  select(geometry, ECO_NAME, NOME_BIOMA, 
         everything())
View(data)
write.csv(x = data, file = "tropical_biomes.csv")

#checking ecoregion and biome
length(unique(data$NOME_BIOMA))
length(unique(data$ECO_NAME))
table(data$NOME_BIOMA)
table(data$ECO_NAME)

vb <- data %>%
  select(NOME_BIOMA, Biome)
View(vb)

#Koppen climate classification
library(kgc)
lalo <- data.frame(lalo,
                   rndCoord.lon = RoundCoordinates(lalo$Longitude),
                   rndCoord.lat = RoundCoordinates(lalo$Latitude))
data <- data.frame(data,ClimateZ=LookupCZ(lalo,res='fine'))
View(data)
vc <- data %>%
  select(Climatic_region, ClimateZ)
View(vc)## diferencas com o climate data


library(terra)
library(dplyr)

# diretory
diretorio_destino <- "Y:/Home/dasilvaw/private/systematic review"
if (!dir.exists(diretorio_destino)) {
  dir.create(diretorio_destino, recursive = TRUE)
}

# annual temp (BioClim 1)
url_tmean <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio1_1981-2010_V.2.1.tif"
arquivo_tmean <- paste0(diretorio_destino, "CHELSA_bio1_1981-2010_V.2.1.tif")

# Baixar o arquivo
cat("Baixando Temperatura Média Anual...\n")
download.file(url_tmean, destfile = arquivo_tmean, mode = "wb")

# --- Dados de Precipitação Média Anual (BioClim 12) ---
url_precip <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio12_1981-2010_V.2.1.tif"
arquivo_precip <- paste0(diretorio_destino, "CHELSA_bio12_1981-2010_V.2.1.tif")

# Baixar o arquivo
cat("Baixando Precipitação Média Anual...\n")
download.file(url_precip, destfile = arquivo_precip, mode = "wb")

# 3. Carregar os dados baixados no R usando TERRA::rast()
cat("Carregando RASTERS no R...\n")
tmedia_raster <- rast(arquivo_tmean)
precip_raster <- rast(arquivo_precip)

# 4. Verificar se os dados foram carregados corretamente
print(tmedia_raster)
print(precip_raster)

# Supondo que 'dados_pontos' é seu objeto sf com as coordenadas

# Extração
dados_tmedia_extraidos <- terra::extract(tmedia_raster, points, method = "simple")
dados_precip_extraidos <- terra::extract(precip_raster, points, method = "simple")

# Junção final (exemplo):
dados_com_clima <- points %>%
  mutate(
    T_MEDIA_CHELSA = dados_tmedia_extraidos[[2]],  # Coluna 2 é o valor do raster
    PRECIP_CHELSA = dados_precip_extraidos[[2]]
  )
data <- data %>%
  left_join(dados_com_clima, by = "CODE") %>%
  select(T_MEDIA_CHELSA, PRECIP_CHELSA, 
         everything())

## population per point
library(remotes)
remotes::install_github("wpgp/wopr")
install.packages("devtools")
library(devtools)
devtools::install_github("wpgp/wopr", force = TRUE)
library(wopr)
library(terra)
library(sf)
library(dplyr)

#points
lalo <- data[, c("CODE", "Latitude", "Longitude")]
points <- st_as_sf(lalo, coords = c("Longitude", "Latitude"), crs = 4326)

#WORLDPOP
pop_raster <- rast("Y:/Home/dasilvaw/private/systematic review/global_pop_2020_CN_1km_R2025A_UA_v1.tif")

# Certifique-se que as projeções coincidem
pontos_proj <- st_transform(points, crs(pop_raster))

# Extrair densidade populacional para cada ponto
valores_pop <- terra::extract(pop_raster, vect(pontos_proj))

# Juntar ao dataframe original
data_resultado <- cbind(data_cidades, valores_pop)

#GHSL
pop_ghsl <- rast("Y:/Home/dasilvaw/private/systematic review/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0.tif")

# Certifique-se que as projeções coincidem
pontos_proj1 <- st_transform(points, crs(pop_ghsl))

# Extrair densidade populacional para cada ponto
valores_ghsl <- terra::extract(pop_ghsl, vect(pontos_proj1))
View()

library(wppExplorer)
library(wpp2019)

# Carrega base populacional
data("pop")

head(pop)
data(package = "wpp2019")
