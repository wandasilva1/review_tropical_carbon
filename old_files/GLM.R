##GLM for numeric variables
library(readxl)
library(dplyr)
library(ggplot2)
library(qacReg)
library(MuMIn)
library(car)
library(vegan)
library(lme4)
library(skimr)
library(DHARMa)
library(MASS)

#importing all data
data <- read_excel("tropical_9_1_2025.xlsx", 
                   sheet = "all_data_organized")

#character
data$Publication_date <- as.character(data$Publication_date)
data$ID <- as.character(data$ID)
#numeric
data[ , c(10, 11, 29:32,34:41,43,44,46,47,48)] <- lapply(data[ , c(10, 11, 29:32,34:41,43,44,46,47,48)], as.numeric)

skim(data)

####

#MODELS:
#STORAGE - SEQUESTRATION--------------------------------------------------------
#1- stock ~ sequestration

#data of C stock and sequestration

storage <- data %>%
  filter(!is.na(`C_area_ton/ha`))

seq_stock <- storage %>%
  filter(!is.na(`Carbon_ton/year/ha`))
View(seq_stock)# 29 data points
skim(seq_stock)

shapiro.test(log(seq_stock$`C_area_ton/ha`))
shapiro.test(log(seq_stock$`Carbon_ton/year/ha`))

#testing random effect of the paper ID
lm_p <- glm(`C_area_ton/ha`~ ID,
            data = subset(seq_stock,`Carbon_ton/year/ha`< 15))#stock
summary(lm_p)

lm_pp<- glm(`Carbon_ton/year/ha`~ ID,
            data = subset(seq_stock,`Carbon_ton/year/ha`< 15))#sequestration
summary(lm_pp)

lm_ps<- glm(`C_area_ton/ha`~`Carbon_ton/year/ha`* ID,
            data = subset(seq_stock,`Carbon_ton/year/ha`< 15))#interaction
summary(lm_ps)

#log10--------------------------------------------------------------------------
seq_stock <- seq_stock[seq_stock$`Carbon_ton/year/ha` < 15, ]

seq_stock$Sequ  <- log10(seq_stock$`Carbon_ton/year/ha`)
seq_stock$Stock <- log10(seq_stock$`C_area_ton/ha`)

hist(seq_stock$Sequ)
hist(seq_stock$Stock)

lm_l<- lm(Stock ~ Sequ, data = seq_stock)
anova(lm_l)
r.squaredLR(lm_l)
summary(lm_l)
AIC(lm_l)#AIC = 4.35

par(mfrow = c(2, 2))
plot(lm_l)
par(mfrow = c(1,1))

s<- simulateResiduals(lm_l)
plot(s)

shapiro.test(resid(lm_l))

plot(Stock ~ Sequ, data = seq_stock)

##chart log10-------------------------------------------------------------------
(ss1 <- ggplot(data = seq_stock, aes(x = Sequ, y = Stock)) +
    stat_smooth(method = "lm",
                se = TRUE,
                color = "black") +
    labs(x = "Carbon sequestration (Mg C ha⁻¹ year⁻¹) (Log10)", 
         y = "Carbon storage (Mg C ha⁻¹) (Log10)",
         subtitle = "(p = < 0.001, R² = 0.65)" ) +
    geom_point(size = 3, shape = 21, fill = "black", color = "black", alpha = 0.7) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",  # usa toda a área do gráfico
      plot.title = element_text(hjust = 0, size = 13),
      plot.subtitle = element_text(hjust = 1, size = 12),
    ))

ggsave("stoc_seq.png", plot = ss1, dpi = 300, width = 5, height = 4.5, units = "in")

#model used in the review paper:
lm_ss<- glm(`C_area_ton/ha`~`Carbon_ton/year/ha`, family = Gamma(link = "identity"),
           data = subset(seq_stock,`Carbon_ton/year/ha`< 15))
summary(lm_ss)##positive SIGNIFICATIVE influence R2= 0.32 AIC = 236.06
anova(lm_ss)
r.squaredLR(lm_ss)

#residuous
par(mfrow = c(2, 2))
plot(lm_ss)
par(mfrow = c(1,1))

simulated_residuals <- simulateResiduals(lm_ss)
plot(simulated_residuals)


#comparing the links:
mod_log <- glm(`C_area_ton/ha` ~ `Carbon_ton/year/ha`,
               family = Gamma(link="log"),
               data = subset(seq_stock, `Carbon_ton/year/ha` < 15))

mod_inverse <- glm(`C_area_ton/ha` ~ `Carbon_ton/year/ha`,
                   family = Gamma(link="inverse"),
                   data = subset(seq_stock, `Carbon_ton/year/ha` < 15))

mod_identity <- glm(`C_area_ton/ha` ~ `Carbon_ton/year/ha`,
                    family = Gamma(link="identity"),
                    data = subset(seq_stock, `Carbon_ton/year/ha` < 15))

AIC(mod_log, mod_inverse, mod_identity)#identity

id <- simulateResiduals(mod_identity)
plot(id)

iN <- simulateResiduals(mod_inverse)
plot(iN)

log <- simulateResiduals(mod_log)
plot(log)


#including the random effect of the paper ID:
seq_stock$`C_area_ton/ha` <- log(seq_stock$`C_area_ton/ha`)
seq_stock$`Carbon_ton/year/ha` <- log(seq_stock$`Carbon_ton/year/ha`)

glmm_ss <- glmer(`C_area_ton/ha`~`Carbon_ton/year/ha`+ (1|ID), family = Gamma(link = "log"),
                data = subset(seq_stock,`Carbon_ton/year/ha`< 15))
summary(glmm_ss)
nulo <- glmer(`C_area_ton/ha` ~ 1 + (1|ID), family = Gamma(link = "log"),
                data = subset(seq_stock,`Carbon_ton/year/ha`< 15))#null model
summary(nulo)
anova(glmm_ss, nulo)

s<- simulateResiduals(glmm_ss)
plot(s)

# Calcular R² marginal e condicional
r2_bio <- r.squaredGLMM(glmm_ss)
r2_bio_m <- r2_bio[1]  # Explicado pelos efeitos fixos
r2_bio_c <- r2_bio[2]  # Explicado pelos efeitos fixos + aleatórios

# Exibir os valores
print(r2_bio)

##chart storage ~ sequestration--------------------------------------------------

(ss <- ggplot(data = subset(seq_stock,`Carbon_ton/year/ha`< 15), aes(x = `Carbon_ton/year/ha`, y =`C_area_ton/ha`)) +
  stat_smooth(method = "glm",
              method.args = list(family = Gamma(link = "identity")),
              se = TRUE,
              color = "black") +
  labs(x = "Carbon sequestration (t/ha/year)", 
       y = "Carbon storage (t/ha)",
       title = "(A)",
       subtitle = "(p = 0.003, adj R² = 0.32)" ) +
  geom_point(size = 3, shape = 21, fill = "darkorange", color = "black", alpha = 0.7) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",  # usa toda a área do gráfico
      plot.title = element_text(hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 1, size = 12),
    ))

ggsave("stoc_seq.png", plot = ss, dpi = 300, width = 6, height = 5, units = "in")

####

#STORAGE
#C stock data (ton/ha)
storage <- data %>%
  filter(!is.na(`C_area_ton/ha`))

##TEMPERATURE--------------------------------------------------------------------
lm_t <- glm(`C_area_ton/ha`~ `Temperature(average annual temperature-°C)`, 
           family = Gamma (link = "log"), 
           data = storage)
summary(lm_t)#non-significative
anova(lm_t)
r.squaredLR(lm_t)

#residuous
par(mfrow = c(2, 2))
plot(lm_t)
par(mfrow = c(1,1))

shapiro.test(resid(lm_t))#non-normal
hist(resid(lm_t))

(temp_stoc <- ggplot(data = storage, aes(x =`Temperature(average annual temperature-°C)`, y = `C_area_ton/ha`)) + 
  labs(x = "Mean annual temperature (°C)", 
       y = "Carbon storage (Mg C ha⁻¹)",
       title = "(a)",
       subtitle = "(p = 0.73, R² = 0.001)" ) +
  geom_point(size = 3, shape = 21, fill = "black", color = "black", alpha = 0.7) +
    stat_smooth(method = "glm",
                method.args = list(family = Gamma(link = "log")),
                se = FALSE,
                color = "black")+
  theme_minimal(base_size = 13) +
  theme(
      plot.title.position = "plot",  # usa toda a área do gráfico
      plot.title = element_text(hjust = 0, size = 13),
      plot.subtitle = element_text(hjust = 1, size = 12),
    ))

## separated for 
#build-up
df_build <- storage %>% filter(Habitat == "Build-up")
lm_bu <- glm(`C_area_ton/ha`~ `Temperature(average annual temperature-°C)`,
            family = Gamma,
            data = df_build)
summary(lm_bu)#non-significative
anova(lm_bu)

#residuous
par(mfrow = c(2, 2))
plot(lm_bu)
par(mfrow = c(1,1))

shapiro.test(resid(lm_bu))#normal
hist(resid(lm_bu))

ggplot(data = df_build, aes(x =`Temperature(average annual temperature-°C)`, y = sqrt(`C_area_ton/ha`))) + 
  labs(x = "Mean annual temperature (°C)", 
       y = "C storage (t/ha)") +
  geom_point(size = 3, shape = 21, fill = "darkorange", color = "black", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_bw()

#forest
df_forest <- storage %>% filter(Habitat == "Forest")
lm_fo <- glm(`C_area_ton/ha`~ `Temperature(average annual temperature-°C)`,
             family = Gamma,
             data = df_forest)
summary(lm_fo)#non-significative
anova(lm_fo)

#residuous
par(mfrow = c(2, 2))
plot(lm_t)
par(mfrow = c(1,1))

shapiro.test(resid(lm_fo))#non-normal
hist(resid(lm_fo))

ggplot(data = df_forest, aes(x =`Temperature(average annual temperature-°C)`, y = `C_area_ton/ha`)) + 
  labs(x = "Mean annual temperature (°C)", 
       y = "C storage (t/ha)") +
  geom_point(size = 3, shape = 21, fill = "darkorange", color = "black", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_bw()

#UGS
df_ugs <- storage %>% filter(Habitat == "Urban green spaces (UGS)")
lm_ugs <- glm(`C_area_ton/ha`~ `Temperature(average annual temperature-°C)`,
             data = df_ugs)
summary(lm_ugs)#non-significative
anova(lm_fo)

#residuous
par(mfrow = c(2, 2))
plot(lm_ugs)
par(mfrow = c(1,1))

shapiro.test(resid(lm_ugs))#non-normal
hist(resid(lm_fo))

ggplot(data = df_ugs, aes(x =`Temperature(average annual temperature-°C)`, y = `C_area_ton/ha`)) + 
  labs(x = "Mean annual temperature (°C)", 
       y = "C storage (t/ha)") +
  geom_point(size = 3, shape = 21, fill = "darkorange", color = "black", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_bw()


##Pluviosity--------------------------------------------------------------------
lm_p <- glm(`C_area_ton/ha`~ `Pluviosity(average annual rainfall-mm)`,
           family = Gamma (link = "log"), 
           data = storage)
summary(lm_p)#non-significative
anova(lm_p)
r.squaredLR(lm_p)
#residuous
par(mfrow = c(2, 2))
plot(lm_p)
par(mfrow = c(1,1))

shapiro.test(resid(lm_p))#non-normal
hist(resid(lm_p))

(pluv_stoc <- ggplot(data = storage, aes(x =`Pluviosity(average annual rainfall-mm)`, y = `C_area_ton/ha`)) + 
  labs(x = "Mean annual precipitation (mm)", 
       y = "Carbon storage (Mg C ha⁻¹)",
       title = "(b)",
       subtitle = "(p = 0.19, R² = 0.02)" ) +
  geom_point(size = 3, shape = 21, fill = "black", color = "black", alpha = 0.7) +
    stat_smooth(method = "glm",
                method.args = list(family = Gamma(link = "log")),
                se = FALSE,
                color = "black")+
    theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",  # usa toda a área do gráfico
      plot.title = element_text(hjust = 0, size = 13),
      plot.subtitle = element_text(hjust = 1, size = 12),
    ))

#forest
df_forest <- storage %>% filter(Habitat == "Forest")
p_fo <- glm(`C_area_ton/ha`~ `Pluviosity(average annual rainfall-mm)`,
             data = df_forest)
summary(p_fo)#non-significative
anova(p_fo)

#residuous
par(mfrow = c(2, 2))
plot(lm_t)
par(mfrow = c(1,1))

shapiro.test(resid(lm_fo))#non-normal
hist(resid(lm_fo))

ggplot(data = df_forest, aes(x =`Pluviosity(average annual rainfall-mm)`, y = sqrt(`C_area_ton/ha`))) + 
  labs(x = "Mean annual pluviosity (°C)", 
       y = "C storage (t/ha)") +
  geom_point(size = 3, shape = 21, fill = "darkorange", color = "black", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_bw()

#UGS
df_ugs <- storage %>% filter(Habitat == "Urban green spaces (UGS)")
p_ugs <- glm(`C_area_ton/ha`~ `Pluviosity(average annual rainfall-mm)`,
              data = df_ugs)
summary(p_ugs)#non-significative
anova(lm_fo)

#residuous
par(mfrow = c(2, 2))
plot(lm_ugs)
par(mfrow = c(1,1))

shapiro.test(resid(lm_ugs))#non-normal
hist(resid(lm_fo))

ggplot(data = df_ugs, aes(x =`Pluviosity(average annual rainfall-mm)`, y = `C_area_ton/ha`)) + 
  labs(x = "Mean annual pluviosity (°C)", 
       y = "C storage (t/ha)") +
  geom_point(size = 3, shape = 21, fill = "darkorange", color = "black", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_bw()

#population(hab)
lm_pop <- glm(`C_area_ton/ha`~ `Population(hab)`,
             family = Gamma(link = "log"),
             data = storage)
summary(lm_pop)#non-significative
anova(lm_pop)
r.squaredLR(lm_pop)

#residuous
par(mfrow = c(2, 2))
plot(lm_pop)
par(mfrow = c(1,1))

shapiro.test(resid(lm_pop))#non-normal
hist(resid(lm_pop))

ggplot(data = storage, aes(x =`Population(hab)`, y = `C_area_ton/ha`)) + 
  labs(x = "Number of inhabitants", 
       y = "C storage (t/ha)") +
  geom_point(size = 3, shape = 21, fill = "darkorange", color = "black", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_bw()

#municipal area (km2)
lm_area <- glm(`C_area_ton/ha`~ City_size, family = Gamma(), data = storage)
summary(lm_area)#non-significant
anova(lm_area)
r.squaredLR(lm_area)

#residuous
par(mfrow = c(2, 2))
plot(lm_area)
par(mfrow = c(1,1))

shapiro.test(resid(lm_area))#normal
hist(resid(lm_area))

ggplot(data = storage, aes(x = City_size, y = `C_area_ton/ha`)) + 
  labs(x = "City area (km2)", 
       y = "Carbon storage (t/ha)") +
  geom_point(size = 3, shape = 21, fill = "darkorange", color = "black", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_bw()

#Density of population(hab/km2)-------------------------------------------------

lm_den <- glm(`C_area_ton/ha`~ `Densi_pop(hab/km2)`, family = Gamma (link = "log"), data = storage)
summary(lm_den)##positive SIGNIFICATIVE influence R2= 0.10 AIC = 1078.8
anova(lm_den)
r.squaredLR(lm_den)

#residuous
par(mfrow = c(2, 2))
plot(lm_den)
par(mfrow = c(1,1))

simu <- simulateResiduals(lm_den)
plot(simu)#ok

## including interaction with UGS-----------------------------------------------

lg_den <- glm(`C_area_ton/ha`~ `Densi_pop(hab/km2)`*Habitat, family = Gamma (link = "log"), data = storage)
summary(lg_den)##positive SIGNIFICATIVE influence R2= 0.33 AIC = 1072.7
library(openxlsx)
write.xlsx(lg_den, 
           file = "density_stoq.xlsx",
           sheetName = "indices",
           rowNames = FALSE)

library(emmeans)

emmeans(lg_den, ~ Habitat)
emmeans(lg_den, ~ Habitat | `Densi_pop(hab/km2)`)


simu <- simulateResiduals(lg_den)
plot(simu)#ok

AIC(lg_den, lm_den)
r.squaredLR(lg_den)


#including the random effect of the paper ID:
lm_d <- glm(`C_area_ton/ha` ~ ID, family = Gamma (link = "log"), data = storage)
summary(lm_d)

glmm_ds <- glmer(`C_area_ton/ha`~ `Densi_pop(hab/km2)`+ (1|ID), family = Gamma (link = "log"),
                 data = storage)
summary(glmm_ds)
nulo1 <- glmer(`C_area_ton/ha` ~ 1 + (1|ID), family = Gamma (link = "log"),
              data = storage)#null model
summary(nulo1)
anova(glmm_ds, nulo1)

s1<- simulateResiduals(glmm_ds)
plot(s1)

# Calcular R² marginal e condicional
r2_bio <- r.squaredGLMM(glmm_ds)
r2_bio_m <- r2_bio[1]  # Explicado pelos efeitos fixos
r2_bio_c <- r2_bio[2]  # Explicado pelos efeitos fixos + aleatórios

# Exibir os valores
print(r2_bio)

##chart density of human population---------------------------------------------

(dp <- ggplot(data = storage, aes(x = `Densi_pop(hab/km2)`, y = `C_area_ton/ha`)) + 
  labs(x = "Density of human population (inh/km2)", 
       y = "Carbon storage (Mg C ha-1)",
       title = "(B)",
       subtitle = "(p = 0.006, R2 = 0.10)") +
    
  geom_point(size = 3, shape = 21, fill = "darkorange", color = "black", alpha = 0.7) +
  stat_smooth(method = "glm",
              method.args = list(family = Gamma(link = "log")),
              se = TRUE,  # se quiser o intervalo de confiança
              color = "black")+
    theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",  # usa toda a área do gráfico
      plot.title = element_text(hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 1, size = 12),
    ))

##chart density * UGS-----------------------------------------------------------
(dpp <- ggplot(
  data = storage,
  aes(
    x = `Densi_pop(hab/km2)`,
    y = `C_area_ton/ha`,
    color = Habitat,
    shape = Habitat
  )
) + scale_shape_manual(values = c(
    "Forest" = 19,
    "Parks" = 17,
    "Gardens" = 20,
    "All green areas" = 18,
    "Adj. green" = 15,
    "Agriculture" = 12,
    "Estuary" = 8
  ))+
  scale_color_manual(values = c(
      "Forest" = "darkorange",
      "Parks" = "#009E73",
      "Gardens" = "cornsilk4",
      "All green areas" = "purple",
      "Adj. green" = "#56B4E9",
      "Agriculture" = "black",
      "Estuary" = "deeppink3"
  ))+
  labs(
    x = "Density of human population (inh/km²)",
    y = "Carbon storage (Mg C ha⁻¹)",
    title = "(b)",
    subtitle = "(R² = 0.33)",
    color = "UGS",
    shape = "UGS"
  ) +
  geom_point(
    size = 3,
    alpha = 0.7
  ) +
  stat_smooth(
    method = "glm",
    method.args = list(family = Gamma(link = "log")),
    se = FALSE
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, size = 13),
    plot.subtitle = element_text(hjust = 0.65, size = 12)
  ))


ggsave("stoc_dp.png", plot = dp, dpi = 300, width = 6, height = 5, units = "in")

GLM <- ss1 + dpp

ggsave("GLM1.png", plot = GLM, dpi = 300, width = 10, height = 4, units = "in")

####

#SEQUESTRATION------------------------------------------------------------------

#C sequestration (ton/year/ha) 
sequestration <- data %>%
  filter(!is.na(`Carbon_ton/year/ha`))

##Temperature-------------------------------------------------------------------
lm_ts <- glm(`Carbon_ton/year/ha`~ `Temperature(average annual temperature-°C)`, family = Gamma(link = "log"), data = sequestration)
summary(lm_ts)#non-significative
anova(lm_ts)
r.squaredLR(lm_ts)

#residuous
par(mfrow = c(2, 2))
plot(lm_ts)
par(mfrow = c(1,1))

shapiro.test(resid(lm_ts))#non-normal
hist(resid(lm_ts))

(temp_seq <- ggplot(data = sequestration, aes(x =`Temperature(average annual temperature-°C)`, y = `Carbon_ton/year/ha`)) + 
  labs(x = "Mean annual temperature (°C)", 
       y = "Carbon sequestration (Mg C ha⁻¹ y⁻¹)",
       title = "(c)",
       subtitle = "(p = 0.87, R² = < 0.001)" ) +
  geom_point(size = 3, shape = 21, fill = "black", color = "black", alpha = 0.7) +
    stat_smooth(method = "glm",
                method.args = list(family = Gamma(link = "log")),
                se = FALSE,
                color = "black")+
    theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",  # usa toda a área do gráfico
      plot.title = element_text(hjust = 0, size = 13),
      plot.subtitle = element_text(hjust = 1, size = 12),
    ))

#forest
ds_forest <- sequestration %>% filter(Habitat == "Forest")
lm_fos <- glm(`Carbon_ton/year/ha`~ `Temperature(average annual temperature-°C)`,
             family = Gamma,
             data = df_forest)
summary(lm_fos)#non-significative
anova(lm_fos)

#residuous
par(mfrow = c(2, 2))
plot(lm_fos)
par(mfrow = c(1,1))

shapiro.test(resid(lm_fos))#non-normal
hist(resid(lm_fos))

ggplot(data = ds_forest, aes(x =`Temperature(average annual temperature-°C)`, y = `Carbon_ton/year/ha`)) + 
  labs(x = "Mean annual temperature (°C)", 
       y = "C storage (t/ha)") +
  geom_point(size = 3, shape = 21, fill = "darkorange", color = "black", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_bw()

#UGS
ds_ugs <- sequestration %>% filter(Habitat == "Urban green spaces (UGS)")
t_ugs <- glm(`Carbon_ton/year/ha`~ `Temperature(average annual temperature-°C)`,
              family = Gamma,
              data = ds_ugs)
summary(t_ugs)#non-significative
anova(t_ugs)

#residuous
par(mfrow = c(2, 2))
plot(t_ugs)
par(mfrow = c(1,1))

shapiro.test(resid(lm_fos))#non-normal
hist(resid(lm_fos))

ggplot(data = ds_ugs, aes(x =`Temperature(average annual temperature-°C)`, y = `Carbon_ton/year/ha`)) + 
  labs(x = "Mean annual temperature (°C)", 
       y = "C storage (t/ha)") +
  geom_point(size = 3, shape = 21, fill = "darkorange", color = "black", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_bw()

##Pluviosity--------------------------------------------------------------------
lm_ps <- glm(`Carbon_ton/year/ha`~ `Pluviosity(average annual rainfall-mm)`,
            family = Gamma(link = "log"), data = sequestration)
summary(lm_ps)#non-significative
anova(lm_ps)
r.squaredLR(lm_ps)
#residuous
par(mfrow = c(2, 2))
plot(lm_ps)
par(mfrow = c(1,1))

shapiro.test(resid(lm_ps))#non-normal
hist(resid(lm_ps))

(pluv_seq <- ggplot(data = sequestration, aes(x =`Pluviosity(average annual rainfall-mm)`, y = `Carbon_ton/year/ha`)) + 
  labs(x = "Mean annual precipitation (mm)", 
       y = "Carbon sequestration (Mg C ha⁻¹ y⁻¹)",
       title = "(d)",
       subtitle = "(p = 0.60, R² = 0.01)" ) +
  geom_point(size = 3, shape = 21, fill = "black", color = "black", alpha = 0.7) +
    stat_smooth(method = "glm",
                method.args = list(family = Gamma(link = "log")),
                se = FALSE,
                color = "black")+
  theme_minimal(base_size = 13) +
  theme(
    plot.title.position = "plot",  # usa toda a área do gráfico
    plot.title = element_text(hjust = 0, size = 13),
    plot.subtitle = element_text(hjust = 1, size = 12),
  ))


#forest
ds_forest <- sequestration %>% filter(Habitat == "Forest")
p_fos <- glm(`Carbon_ton/year/ha`~ `Pluviosity(average annual rainfall-mm)`,
              family = Gamma,
              data = df_forest)
summary(p_fos)#non-significative

ggplot(data = ds_forest, aes(x =`Pluviosity(average annual rainfall-mm)`, y = `Carbon_ton/year/ha`)) + 
  labs(x = "Mean annual pluviosity (mm)", 
       y = "C storage (t/ha)") +
  geom_point(size = 3, shape = 21, fill = "darkorange", color = "black", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_bw()

#UGS
ds_ugs <- sequestration %>% filter(Habitat == "Urban green spaces (UGS)")
p_ugs <- glm(`Carbon_ton/year/ha`~ `Pluviosity(average annual rainfall-mm)`,
             family = Gamma,
             data = ds_ugs)
summary(t_ugs)#non-significative

ggplot(data = ds_ugs, aes(x =`Pluviosity(average annual rainfall-mm)`, y = `Carbon_ton/year/ha`)) + 
  labs(x = "Mean annual pluviosity (mm)", 
       y = "C storage (t/ha)") +
  geom_point(size = 3, shape = 21, fill = "darkorange", color = "black", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_bw()



#population(hab)
lm_pops <- glm(`Carbon_ton/year/ha`~ `Population(hab)`, family = Gamma (link= "log"), data = sequestration)
summary(lm_pops)#non-significative
anova(lm_pops)
r.squaredLR(lm_pops)

#residuous
par(mfrow = c(2, 2))
plot(lm_pops)
par(mfrow = c(1,1))

shapiro.test(resid(lm_pops))#non-normal
hist(resid(lm_pops))

ggplot(data = sequestration, aes(x =`Population(hab)`, y = `Carbon_ton/year/ha`)) + 
  labs(x = "Number of inhabitants", 
       y = "C sequestration (t/ha/year)") +
  geom_point(size = 3, shape = 21, fill = "darkorange", color = "black", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_bw()

##density of population(hab/km2)------------------------------------------------
lm_dens <- glm(`Carbon_ton/year/ha`~`Densi_pop(hab/km2)`* Habitat, family = Gamma (link= "log"), data = sequestration)
summary(lm_dens)##non-significant
anova(lm_dens)
r.squaredLR(lm_dens)
library(openxlsx)
write.xlsx(lm_dens, 
           file = "density_seq.xlsx",
           sheetName = "indices",
           rowNames = FALSE)
#residuous
par(mfrow = c(2, 2))
plot(lm_dens)
par(mfrow = c(1,1))

shapiro.test(resid(lm_dens))#non-normal
hist(resid(lm_dens))

(dens_seq <- ggplot(
  data = sequestration,
  aes(
    x = `Densi_pop(hab/km2)`,
    y = `Carbon_ton/year/ha`))+
  labs(
    x = "Density of human population (inh/km²)",
    y = "Carbon sequestration (Mg C ha⁻¹ y⁻¹)",
         title = "(E)",
         subtitle = "(p = 0.053, R² = 0.07)" ) +
      geom_point(size = 3, shape = 21, fill = "black", color = "black", alpha = 0.7) +
    stat_smooth(method = "glm",
                method.args = list(family = Gamma(link = "log")),
                se = FALSE,
                color = "black")+
      theme_minimal(base_size = 13) +
      theme(
        plot.title.position = "plot",  # usa toda a área do gráfico
        plot.title = element_text(hjust = 0, size = 13),
        plot.subtitle = element_text(hjust = 1, size = 12),
      ))



(dps <- ggplot(
  data = sequestration,
  aes(
    x = `Densi_pop(hab/km2)`,
    y = `Carbon_ton/year/ha`,
    color = Habitat,
    shape = Habitat
  )
) + scale_shape_manual(values = c(
  "Forest" = 19,
  "Parks" = 17,
  "Gardens" = 20,
  "All green areas" = 18,
  "Adj. green" = 15,
  "Agriculture" = 12,
  "Estuary" = 8
))+
    scale_color_manual(values = c(
      "Forest" = "darkorange",
      "Parks" = "#009E73",
      "Gardens" = "cornsilk4",
      "All green areas" = "purple",
      "Adj. green" = "#56B4E9",
      "Agriculture" = "black",
      "Estuary" = "deeppink3"
    ))+
    labs(
      x = "Density of human population (inh/km²)",
      y = "Carbon sequestration (Mg C ha⁻¹ y⁻¹)",
      title = "(a)",
      subtitle = "(R² = 0.32)",
      color = "UGS",
      shape = "UGS"
    ) +
    geom_point(
      size = 3,
      alpha = 0.7
    ) +
    stat_smooth(
      method = "glm",
      method.args = list(family = Gamma(link = "log")),
      se = FALSE
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0, size = 13),
      plot.subtitle = element_text(hjust = 0.65, size = 12),
      legend.position = "none"
    ))






library(patchwork)

design <- "1122
           3344"

ap <- wrap_elements(full =  temp_stoc ) + pluv_stoc + temp_seq + pluv_seq + 
  plot_layout(
    design = design, 
    widths = 1, 
  #  guides = 'collect'
  )


ggsave("temp_pluv.png", plot = ap, dpi = 300, width = 8, height = 8, units = "in")

(densi <- (dps/dpp) +
    plot_layout(guides = "collect")
    )

ggsave("densities.png", plot = densi, dpi = 300, width = 6, height = 8, units = "in")

