# HEDGES’ G comparation for categorical variables
library(metafor)
library(dplyr)
library(tidyverse)
library(purrr)
library(tidyr)
library(ggplot2)
library(readxl)
library(skimr)
library(stringr)
library(patchwork)

#data
#importing all data
data <- read_excel("tropical_4_23_2026_newarticles.xlsx", 
                   sheet = "all_data_organized")

#numeric
data[ , c(10, 11, 29:32,34:41,43,44,46,47,48)] <- lapply(data[ , c(10, 11, 29:32,34:41,43,44,46,47,48)], as.numeric)


# HABITATS/UGS #--------------------------------------------------------------------

# C stock data (ton/ha)
storage <- data %>%
  filter(!is.na(`C_area_ton/ha`))

#Assumption tests:
# Normal test
by(log(storage$`C_area_ton/ha`), storage$Habitat, shapiro.test)

# Homogeneity of variances
lev <- leveneTest(log(`C_area_ton/ha`) ~ Habitat, data = storage)
lev

#log in (`C_area_ton/ha`)
storage$`C_area_ton/ha` <- log(storage$`C_area_ton/ha`)

# Statistics by habitat type:
resumo <- storage %>%
  group_by(Habitat) %>%
  summarise(
    mean = mean(`C_area_ton/ha`, na.rm = TRUE),
    sd = sd(`C_area_ton/ha`, na.rm = TRUE),
    n = n(),
    .groups = "drop")


print(resumo)

# Generating all possible comparisons
pairs <- combn(unique(resumo$Habitat), 2, simplify = FALSE)

# Calculating Hedges’ g for each pair
resultados <- map_dfr(pairs, function(par) {
  g1 <- resumo %>% filter(Habitat == par[1])
  g2 <- resumo %>% filter(Habitat == par[2])
  
  esc <- escalc(
    measure = "SMDH",
    m1i = g1$mean, sd1i = g1$sd, n1i = g1$n,
    m2i = g2$mean, sd2i = g2$sd, n2i = g2$n
  )
  
  mod <- rma(yi, vi, data = esc)
  
  data.frame(
    group1 = par[1],
    group2 = par[2],
    yi     = esc$yi,
    vi     = esc$vi,
    se     = sqrt(esc$vi),
    ci_low = esc$yi - 1.96 * sqrt(esc$vi),
    ci_high= esc$yi + 1.96 * sqrt(esc$vi),
    pval   = mod$pval
  )
})

# Significant
resultados <- resultados %>%
  mutate(sig = case_when(
    pval < 0.001 ~ "***",
    pval < 0.01  ~ "**",
    pval < 0.05  ~ "*",
    TRUE ~ "ns"
  ))
# Results interpretation
resultados <- resultados %>%
  mutate(
    magnitude = case_when(
      abs(yi) < 0.2 ~ "Small",
      abs(yi) < 0.5 ~ "Moderate",
      abs(yi) < 0.8 ~ "Large",
      TRUE ~ "Very Large"
    )
  ) %>%
  arrange(desc(abs(yi)))

print(resultados)

# Chart
ggplot(resultados, aes(x = reorder(paste(group1, "-", group2), yi),
                       y = yi)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(sig == "ns", "", sig),
                y = yi + 0.05 * (max(yi) - min(yi))),
            hjust = 0, size = 6.5, fontface = "bold")+
  coord_flip() +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    y = "Hedges’ g",
    subtitle = "(* p < 0.05, ** p < 0.01, *** p < 0.001)"
  )

# Global model
global_hab <- rma.mv(yi, vi, random = ~ 1 | group1/group2, data = resultados)
summary(global_hab)
###O modelo multivariado revelou heterogeneidade significativa entre habitats (Q = 58.13, p < 0.001), indicando variação real nas diferenças de estoque de carbono entre pares de habitats urbanos. Entretanto, o efeito médio global (Hedges’ g = 0.25; p = 0.44) não foi significativo, sugerindo que não existe uma tendência geral de um habitat acumular mais carbono do que outro — as diferenças observadas dependem de condições locais e características ecológicas específicas de cada ambiente.

leave1out(global_hab)



# C sequestration (ton/year/ha) 
sequestration <- data %>%
  filter(!is.na(`Carbon_ton/year/ha`))

sequestration <- sequestration %>%
  filter(!str_detect(Habitat, "Estuary"))
sequestration <- sequestration %>%
  filter(!str_detect(Habitat, "Parks"))
sequestration <- sequestration %>%
  filter(!str_detect(Habitat, "Agriculture"))

#Assumption tests:
# Normal test
by((sequestration$`Carbon_ton/year/ha`), sequestration$Habitat, shapiro.test)

# Homogeneity of variances
lev <- leveneTest((`Carbon_ton/year/ha`) ~ Habitat, data = sequestration)
lev

#log in (`Carbon_ton/year/ha`)
sequestration$`Carbon_ton/year/ha` <- log(sequestration$`Carbon_ton/year/ha`)

# Statistics by habitat type:
resumo_s <- sequestration %>%
  group_by(Habitat) %>%
  summarise(
    mean = mean(`Carbon_ton/year/ha`, na.rm = TRUE),
    sd = sd(`Carbon_ton/year/ha`, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print(resumo_s)

# Generating all possible comparisons
pairs_s <- combn(unique(resumo_s$Habitat), 2, simplify = FALSE)

# Calculating Hedges’ g for each pair
resultados_s <- map_dfr(pairs_s, function(par) {
  g1 <- resumo_s %>% filter(Habitat == par[1])
  g2 <- resumo_s %>% filter(Habitat == par[2])
  
  esc <- escalc(
    measure = "SMDH",
    m1i = g1$mean, sd1i = g1$sd, n1i = g1$n,
    m2i = g2$mean, sd2i = g2$sd, n2i = g2$n
  )
  
  mod <- rma(yi, vi, data = esc)
  
  data.frame(
    group1 = par[1],
    group2 = par[2],
    yi     = esc$yi,
    vi     = esc$vi,
    se     = sqrt(esc$vi),
    ci_low = esc$yi - 1.96 * sqrt(esc$vi),
    ci_high= esc$yi + 1.96 * sqrt(esc$vi),
    pval   = mod$pval
  )
})

# Significant
resultados_s <- resultados_s %>%
  mutate(sig = case_when(
    pval < 0.001 ~ "***",
    pval < 0.01  ~ "**",
    pval < 0.05  ~ "*",
    TRUE ~ "ns"
  ))
# Results interpretation
resultados_s <- resultados_s %>%
  mutate(
    magnitude = case_when(
      abs(yi) < 0.2 ~ "Small",
      abs(yi) < 0.5 ~ "Moderate",
      abs(yi) < 0.8 ~ "Large",
      TRUE ~ "Very Large"
    )
  ) %>%
  arrange(desc(abs(yi)))

print(resultados_s)

# Chart
ggplot(resultados_s, aes(x = reorder(paste(group1, "-", group2), yi),
                       y = yi)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(sig == "ns", "", sig),
                y = yi + 0.05 * (max(yi) - min(yi))),
            hjust = 0, size = 6.5, fontface = "bold")+
  coord_flip() +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    y = "Hedges’ g",
    subtitle = "(* p < 0.05, ** p < 0.01, *** p < 0.001)"
  )

# Global model
global_hab_s <- rma.mv(yi, vi, random = ~ 1 | group1/group2, data = resultados_s)
summary(global_hab_s)


## ## merging the results for habitat ## ##

resultados$Tipo <- "Carbon storage"
resultados_s$Tipo <- "Carbon sequestration"

resultados_all <- rbind(resultados, resultados_s)

## plot habitat ----------------------------------------------------------------

(habitat <- ggplot(resultados_all, aes(x = reorder(paste(group1, "to", group2), yi),
                           y = yi,
                           color = Tipo,
                           shape = Tipo)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.5, size = 1,
                position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  # Asteriscos de significância — deslocados para a direita
  geom_text(aes(label = ifelse(sig == "ns", "", sig),
                y = yi + 0.05 * (max(yi) - min(yi))),
            hjust = 0, size = 6.5, fontface = "bold",
            position = position_dodge(width = 0.6), color = "black",
            show.legend = FALSE) +
  
  coord_flip() +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Carbon storage" = "darkorange", "Carbon sequestration" = "purple4")) +
  scale_shape_manual(values = c("Carbon storage" = 16, "Carbon sequestration" = 17)) +
  labs(
    y = "Hedges’ g (log response)",
    title = "(b) Urban green spaces",
    subtitle = "(* p < 0.05, ** p < 0.01)"
  ) +
  theme(
    plot.title.position = "plot",  # usa toda a área do gráfico
    plot.title = element_text(hjust = 0, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 1, size = 14),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")
    
  ))




# URBAN CLASSES #---------------------------------------------------------------

#importing all data
data <- read_excel("tropical_4_23_2026_newarticles.xlsx", 
                   sheet = "all_data_organized")

#numeric
data[ , c(10, 11, 29:32,34:41,43,44,46,47,48)] <- lapply(data[ , c(10, 11, 29:32,34:41,43,44,46,47,48)], as.numeric)


# C stock data (ton/ha)
storage <- data %>%
  filter(!is.na(`C_area_ton/ha`))

#Assumption tests
# Normal test
by(log(storage$`C_area_ton/ha`), storage$`Urbanization_intensity_class`, shapiro.test)

# Homogeneity of variances
lev <- leveneTest(log(`C_area_ton/ha`) ~ `Urbanization_intensity_class`, data = storage)
lev

#log in (`C_area_ton/ha`)
storage$`C_area_ton/ha` <- log(storage$`C_area_ton/ha`)

# Statistics by habitat type:
resumo1 <- storage %>%
  group_by(`Urbanization_intensity_class`) %>%
  summarise(
    mean = mean(`C_area_ton/ha`, na.rm = TRUE),
    sd = sd(`C_area_ton/ha`, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print(resumo1)

# Generating all possible comparisons
pairs1 <- combn(unique(resumo1$`Urbanization_intensity_class`), 2, simplify = FALSE)

# Calculating Hedges’ g for each pair
resultados1 <- map_dfr(pairs1, function(par) {
  g1 <- resumo1 %>% filter(`Urbanization_intensity_class` == par[1])
  g2 <- resumo1 %>% filter(`Urbanization_intensity_class` == par[2])
  
  esc <- escalc(
    measure = "SMDH",
    m1i = g1$mean, sd1i = g1$sd, n1i = g1$n,
    m2i = g2$mean, sd2i = g2$sd, n2i = g2$n
  )
  
  mod <- rma(yi, vi, data = esc)
  
  data.frame(
    group1 = par[1],
    group2 = par[2],
    yi     = esc$yi,
    vi     = esc$vi,
    se     = sqrt(esc$vi),
    ci_low = esc$yi - 1.96 * sqrt(esc$vi),
    ci_high= esc$yi + 1.96 * sqrt(esc$vi),
    pval   = mod$pval
  )
})


# Significant
resultados1 <- resultados1 %>%
  mutate(sig = case_when(
    pval < 0.001 ~ "***",
    pval < 0.01  ~ "**",
    pval < 0.05  ~ "*",
    TRUE ~ "ns"
  ))
# Results interpretation
resultados1 <- resultados1 %>%
  mutate(
    magnitude = case_when(
      abs(yi) < 0.2 ~ "Small",
      abs(yi) < 0.5 ~ "Moderate",
      abs(yi) < 0.8 ~ "Large",
      TRUE ~ "Very Large"
    )
  ) %>%
  arrange(desc(abs(yi)))

print(resultados1)

# Chart
ggplot(resultados1, aes(x = reorder(paste(group1, "-", group2), yi),
                       y = yi)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(sig == "ns", "", sig),
                y = yi + 0.05 * (max(yi) - min(yi))),
            hjust = 0, size = 6.5, fontface = "bold")+
  coord_flip() +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    y = "Hedges’ g",
    subtitle = "(* p < 0.05)"
  )

global_urb <- rma.mv(yi, vi, random = ~ 1 | group1/group2, data = resultados1)
summary(global_urb)
###A meta-análise multivariada indicou ausência de efeito médio significativo de diferenças de estoque de carbono entre as classes de urbanizacao (estimate = −0.05, SE = 0.25, p = 0.83, 95% CI [−0.54, 0.43]).No entanto, observou-se heterogeneidade significativa entre as comparações (Q(5) = 12.34, p = 0.03), sugerindo que as variações entre as classes dependem de fatores locais e não refletem um padrão global.


# C sequestration (ton/year/ha) 
sequestration <- data %>%
  filter(!is.na(`Carbon_ton/year/ha`))

sequestration <- sequestration %>%
  filter(!str_detect(Urbanization_intensity_class, "Rural"))

#Assumption tests
# Normal test
by((sequestration$`Carbon_ton/year/ha`), sequestration$`Urbanization_intensity_class`, shapiro.test)

#log in (`Carbon_ton/year/ha`)
sequestration$`Carbon_ton/year/ha` <- log(sequestration$`Carbon_ton/year/ha`)

# Homogeneity of variances
lev <- leveneTest(log(`Carbon_ton/year/ha`) ~ `Urbanization_intensity_class`, data = sequestration)
lev

# Statistics by habitat type:
resumo1_s <- sequestration %>%
  group_by(`Urbanization_intensity_class`) %>%
  summarise(
    mean = mean(`Carbon_ton/year/ha`, na.rm = TRUE),
    sd = sd(`Carbon_ton/year/ha`, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print(resumo1_s)

# Generating all possible comparisons
pairs1_s <- combn(unique(resumo1_s$`Urbanization_intensity_class`), 2, simplify = FALSE)

# Calculating Hedges’ g for each pair
resultados1_s <- map_dfr(pairs1_s, function(par) {
  g1 <- resumo1_s %>% filter(`Urbanization_intensity_class` == par[1])
  g2 <- resumo1_s %>% filter(`Urbanization_intensity_class` == par[2])
  
  esc <- escalc(
    measure = "SMDH",
    m1i = g1$mean, sd1i = g1$sd, n1i = g1$n,
    m2i = g2$mean, sd2i = g2$sd, n2i = g2$n
  )
  
  mod <- rma(yi, vi, data = esc)
  
  data.frame(
    group1 = par[1],
    group2 = par[2],
    yi     = esc$yi,
    vi     = esc$vi,
    se     = sqrt(esc$vi),
    ci_low = esc$yi - 1.96 * sqrt(esc$vi),
    ci_high= esc$yi + 1.96 * sqrt(esc$vi),
    pval   = mod$pval
  )
})

# Significant
resultados1_s <- resultados1_s %>%
  mutate(sig = case_when(
    pval < 0.001 ~ "***",
    pval < 0.01  ~ "**",
    pval < 0.05  ~ "*",
    TRUE ~ "ns"
  ))
# Results interpretation
resultados1_s <- resultados1_s %>%
  mutate(
    magnitude = case_when(
      abs(yi) < 0.2 ~ "Small",
      abs(yi) < 0.5 ~ "Moderate",
      abs(yi) < 0.8 ~ "Large",
      TRUE ~ "Very Large"
    )
  ) %>%
  arrange(desc(abs(yi)))

print(resultados1_s)

# Chart
ggplot(resultados1_s, aes(x = reorder(paste(group1, "-", group2), yi),
                        y = yi)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = ifelse(sig == "ns", "", sig),
                y = yi + 0.05 * (max(yi) - min(yi))),
            hjust = 0, size = 6.5, fontface = "bold")+
  coord_flip() +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    y = "Hedges’ g",
    subtitle = "(* p < 0.05)"
  )

global_urb_s <- rma.mv(yi, vi, random = ~ 1 | group1/group2, data = resultados1_s)
summary(global_urb_s)

## ## merging the results for urban class## ##

resultados1$Tipo <- "Carbon storage"
resultados1_s$Tipo <- "Carbon sequestration"

resultados_all2 <- rbind(resultados1, resultados1_s)

## plot urban ------------------------------------------------------------------

(urbanc <- ggplot(resultados_all2, aes(x = reorder(paste(group1, "to", group2), yi),
                           y = yi,
                           color = Tipo,
                           shape = Tipo)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.5, size = 1,
                position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  # Asteriscos de significância — deslocados para a direita
  geom_text(aes(label = ifelse(sig == "ns", "", sig),
                y = yi + 0.05 * (max(yi) - min(yi))),
            hjust = 0, size = 6.5, fontface = "bold",
            position = position_dodge(width = 0.6), color = "black",
            show.legend = FALSE) +
  
  coord_flip() +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Carbon storage" = "darkorange", "Carbon sequestration" = "purple4")) +
  scale_shape_manual(values = c("Carbon storage" = 16, "Carbon sequestration" = 17)) +
  labs(
    y = "Hedges’ g (log response)",
    title = "(a) Urban classes",
    subtitle = "(** p < 0.01)"
  ) +
  theme(
    plot.title.position = "plot",  # usa toda a área do gráfico
    plot.title = element_text(hjust = 0, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 1, size = 14),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")
  ))


#Uniting the plots----

layout <- "AB"

box_h <- urbanc + habitat +
  plot_layout(design = layout, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  )

ggsave("urban_ugs_hedges.png", plot = box_h, dpi = 300, width = 10, height = 7, units = "in")

#saving in pdf
ggsave(
  "urban_ugs_hedges_review.pdf",
  plot = box_h,
  width = 10,
  height = 7,
  units = "in",
  device = cairo_pdf
)



#saving the pairs results-------------------------------------------------------

library(openxlsx)

# carbon storage #
df_export <- resultados %>%
  mutate(
    yi = round(yi, 3),
    ci_low = round(ci_low, 3),
    ci_high = round(ci_high, 3),
    pval = signif(pval, 3)
  )
write.xlsx(df_export, 
           file = "results_habitats_2026.xlsx",
           sheetName = "Comparacoes",
           rowNames = FALSE)


df_export1 <- resultados1 %>%
  mutate(
    yi = round(yi, 3),
    ci_low = round(ci_low, 3),
    ci_high = round(ci_high, 3),
    pval = signif(pval, 3)
  )
write.xlsx(df_export1, 
           file = "results_urban_2026.xlsx",
           sheetName = "Comparacoes",
           rowNames = FALSE)


# carbon sequestration #

df_export_s <- resultados_s %>%
  mutate(
    yi = round(yi, 3),
    ci_low = round(ci_low, 3),
    ci_high = round(ci_high, 3),
    pval = signif(pval, 3)
  )
write.xlsx(df_export_s, 
           file = "results_habitats_s_2026.xlsx",
           sheetName = "Comparacoes",
           rowNames = FALSE)


df_export1_s <- resultados1_s %>%
  mutate(
    yi = round(yi, 3),
    ci_low = round(ci_low, 3),
    ci_high = round(ci_high, 3),
    pval = signif(pval, 3)
  )
write.xlsx(df_export1_s, 
           file = "results_urban_s_2026.xlsx",
           sheetName = "Comparacoes",
           rowNames = FALSE)
