## LMM models with interactions##
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(MuMIn)
library(car)
library(vegan)
library(lme4)
library(lmerTest)
library(reshape2)
library(DHARMa)
library(MASS)

#importing all data
data <- read_excel("tropical_4_23_2026_newarticles.xlsx", 
                   sheet = "all_data_organized")

#numeric
data[ , c(10, 11, 29:32,34:41,43,44,46,47,48)] <- lapply(data[ , c(10, 11, 29:32,34:41,43,44,46,47,48)], as.numeric)

#Models:

#CARBON STORAGE-----------------------------------------------------------------
storage <- data %>%
  filter(!is.na(`C_area_ton/ha`))

#categorical variables as factor
storage$Habitat <- as.factor(storage$Habitat)
storage$Urbanization_intensity_class <- as.factor(storage$Urbanization_intensity_class)
storage$ID <- as.factor(storage$ID)

storage$Temp_z  <- log(storage$`Temperature(average annual temperature-°C)`)
storage$Pluv_z  <- log(storage$`Pluviosity(average annual rainfall-mm)`)
storage$Pop_z   <- log(storage$`Population(hab)`)
storage$Dens_z  <- log(storage$`Densi_pop(hab/km2)`)
storage$Stock <- log(storage$`C_area_ton/ha`)

hist(storage$Temp_z)
hist(storage$Pluv_z)
hist(storage$Pop_z)
hist(storage$Dens_z)
hist(storage$Stock)

table(storage$Habitat)
table(storage$Urbanization_intensity_class)

levels(storage$Habitat)
levels(storage$Urbanization_intensity_class)

storage$Habitat <- relevel(storage$Habitat, ref = "Forest")
storage$Urbanization_intensity_class <- relevel(storage$Urbanization_intensity_class, ref = "Urban")

# no interactions
glmm1 <- lmer(
  Stock ~ 
    Temp_z + Pluv_z + Pop_z + 
    Dens_z + Urbanization_intensity_class + Habitat +
    (1 | ID),
  data = storage
)
summary(glmm1)
vcov(glmm1)
AIC(glmm1)

# complete model: all interactions
glmm2 <- lmer(
  Stock ~ 
    Temp_z + Pluv_z + Habitat + Pop_z + Dens_z + Urbanization_intensity_class +
    Temp_z:Pluv_z +
    Temp_z:Urbanization_intensity_class +
    Pluv_z:Urbanization_intensity_class +
    Temp_z:Habitat +
    Pluv_z:Habitat +
    Habitat:Pop_z +
    Habitat:Dens_z +
    (1 | ID),
  data = storage
)
summary(glmm2)
AIC(glmm2)

#good model stock (random)!----
lmm2 <- lmer(
  Stock ~ 
    Habitat + Pop_z + Dens_z + Temp_z +
    Temp_z:Habitat +
    (1 | ID),
  data = storage
)
summary(lmm2)# hab, hab-temp, pop, dens
AIC(lmm2)#best value

(stp_glmm <- drop1(lmm2, test = "Chisq"))

sr <- simulateResiduals(lmm2)
plot(sr)

plot(lmm2) # Homocedasticidade
qqnorm(residuals(lmm2)); qqline(residuals(lmm2)) # Normalidade
shapiro.test(residuals(lmm2))

anova(lmm2)

library(broom.mixed)
library(openxlsx)

det_lmm2 <- tidy(lmm2,
     effects = c("fixed", "ran_pars"),
     conf.int = TRUE)
write.xlsx(det_lmm2, 
           file = "lmm_storage.xlsx",
           sheetName = "Comparacoes",
           rowNames = FALSE)


#plots for article: ----

(lmm2_dens <- ggplot(data = storage, aes(x = Dens_z, y = Stock)) +
    stat_smooth(method = "lm",
                se = TRUE,
                color = "black") +
    labs(x = "Log density of human population (inh km-²)", 
         y = "Log carbon storage (Mg C ha-¹)",
         title = "(a)",
         subtitle = "(p = 0.002)" ) +
    geom_point(size = 3, shape = 21, fill = "black", color = "black", alpha = 0.7) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",  # usa toda a área do gráfico
      plot.title = element_text(hjust = 0, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 1, size = 13),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.15, "cm")
    ))

(lmm2_pop <- ggplot(data = storage, aes(x = Pop_z, y = Stock)) +
    stat_smooth(method = "lm",
                se = TRUE,
                color = "black") +
    labs(x = "Log of human population (inh)", 
         y = "Log of carbon storage (Mg C ha-¹)",
         title = "(b)",
         subtitle = "(p = 0.01)" ) +
    geom_point(size = 3, shape = 21, fill = "black", color = "black", alpha = 0.7) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",  # usa toda a área do gráfico
      plot.title = element_text(hjust = 0, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 1, size = 13),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.15, "cm")
    ))

lmm_dp <- lmm2_dens + lmm2_pop

ggsave("dens_pop.png", plot = lmm_dp, dpi = 300, width = 9.5, height = 5, units = "in")

#saving in pdf
ggsave(
  "dens_pop_review.pdf",
  plot = lmm_dp,
  width = 9.5,
  height = 5,
  units = "in",
  device = cairo_pdf
)

library(emmeans)

# 1. estimated means
emm_habitat <- emmeans(lmm2, ~ Habitat)

# 2. comparisons with tukey adjust
comp_habitat <- pairs(emm_habitat, adjust = "tukey")

summary(comp_habitat)

resultados <- as.data.frame(comp_habitat)

resultados <- resultados %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )

tuk_export <- resultados %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  mutate(
    ci_low = estimate - 1.96 * SE,
    ci_high = estimate + 1.96 * SE
  )

print(tuk_export)

#saving comparisons post-hoc UGS
write.xlsx(tuk_export, 
           file = "tukey_ugs_storage.xlsx",
           sheetName = "Comparacoes",
           rowNames = FALSE)

##plot tukey UGS ----
(tuk_ugs <- ggplot(tuk_export,
       aes(x = reorder(paste(group1, group2, sep = " to "), estimate),
           y = estimate)) +
  
  geom_point(size = 3, position = position_dodge(width = 0.6), color = "darkorange") +
  
geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
              width = 0.5, size = 1,
              position = position_dodge(width = 0.6),
              color = "darkorange")+
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  geom_text(
    aes(
      label = ifelse(sig == "ns", "", sig),
      y = ci_high + 0.05 * max(abs(ci_high))
    ),
    size = 6,
    fontface = "bold"
  ) +
  
  coord_flip() +
  
  theme_minimal(base_size = 13) +
  
  labs(
    y = "Pairwise effect size (log response)",
    x = " ",
    title = "(b) Urban green spaces",
    subtitle = "(** p < 0.01, *** p < 0.001)")
+
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold", size = 14),
    plot.caption = element_text(hjust = 1, size = 13),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")
  ))



library(emmeans)

# 1. Calcular as inclinações (slopes) para cada habitat
inclina <- emtrends(lmm2, ~ Habitat, var = "Temp_z")

# 2. Comparar se as inclinações são diferentes entre si (par a par)
comp_inclina <- pairs(inclina, adjust = "tukey")

# 3. Ver o resultado
summary(inclina)      # Mostra o valor da inclinação para cada habitat
summary(comp_inclina) # Mostra se as inclinações são significativamente diferentes

comp_inclina <- as.data.frame(comp_inclina)

comp_inclina <- comp_inclina%>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )

comp_inclina <- comp_inclina %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  mutate(
    ci_low = estimate - 1.96 * SE,
    ci_high = estimate + 1.96 * SE
  )

print(comp_inclina)

#saving comparisons post-hoc temperature * UGS

write.xlsx(comp_inclina, 
           file = "tukey_ugstemp_storage.xlsx",
           sheetName = "Comparacoes",
           rowNames = FALSE)

##plot temp - UGS ----
(tuk_tugs <- ggplot(comp_inclina,
       aes(x = reorder(paste(group1, group2, sep = " to "), estimate),
           y = estimate)) +
  
  geom_point(size = 3, position = position_dodge(width = 0.6), color = "darkorange") +
  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.5, size = 1,
                position = position_dodge(width = 0.6),
                color = "darkorange")+
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  geom_text(
    aes(
      label = ifelse(sig == "ns", "", sig),
      y = ci_high + 0.05 * max(abs(ci_high))
    ),
    size = 6,
    fontface = "bold"
  ) +
  
  coord_flip() +
  
  theme_minimal(base_size = 13) +
  
  labs(
    y = "Pairwise effect size (log response)",
    x = " ",
    title = "(a) Interaction Temperature * UGS",
    subtitle = "(* p < 0.05, *** p < 0.001)")
+
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold", size = 14),
    plot.caption = element_text(hjust = 1, size = 13),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")))


tuk_sig <- tuk_tugs + tuk_ugs

ggsave("temp_ugs_tukey.png", plot = tuk_sig, dpi = 300, width = 10, height = 6, units = "in")

#saving in pdf
ggsave(
  "temp_ugs_tukey_review.pdf",
  plot = tuk_sig,
  width = 10,
  height = 6,
  units = "in",
  device = cairo_pdf
)


# Gerar o gráfico de interação com linhas de tendencia
ugs_temp <- emmip(lmm2, Habitat ~ Temp_z, cov.reduce = range) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  labs(y = "Log of carbon storage (Mg C ha⁻¹)",
       x = "Log of temperature (°C)")

ggsave("ugs_temp.png", plot = ugs_temp, dpi = 300, width = 5, height = 5.5, units = "in")

library(emmeans)
library(multcomp)
library(multcompView)
library(ggplot2)

# 1. Calcular as inclinações (slopes)
trends_habitat <- emtrends(lmm2, ~ Habitat, var = "Temp_z")

# 2. Gerar as letras de significância (Compact Letter Display)
# Grupos com letras diferentes têm inclinações significativamente diferentes
cld_trends <- cld(trends_habitat, Letters = letters, adjust = "tukey")

# Converter para data frame para o ggplot
df_plot <- as.data.frame(cld_trends)

# Limpar espaços em branco nas letras, se houver
df_plot$.group <- trimws(df_plot$.group)

ggplot(df_plot, aes(x = reorder(Habitat, Temp_z.trend), y = Temp_z.trend)) +
  # Adiciona o ponto médio da inclinação
  geom_point(size = 4, color = "darkgreen") +
  # Adiciona as barras de erro (Intervalo de Confiança de 95%)
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  # Adiciona as letras de significância acima das barras
  geom_text(aes(label = .group, y = upper.CL), vjust = -0.5, size = 5) +
  # Linha de referência no zero (se a barra cruzar o zero, o efeito da temperatura não é significativo naquele habitat)
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Efeito da Temperatura no Estoque de Carbono por Habitat",
    subtitle = "As letras indicam diferenças significativas entre as taxas de variação (inclinações)",
    x = "Habitat",
    y = "Inclinação (Variação de Carbono por 1°C)"
  ) +
  coord_flip() # Inverter para facilitar a leitura dos nomes dos habitats


#SEQUESTRATION------------------------------------------------------------------
sequestration <- data %>%
  filter(!is.na(`Carbon_ton/year/ha`))
table(sequestration$Habitat)
table(sequestration$Urbanization_intensity_class)

sequestration <- sequestration %>%
  filter(!str_detect(Habitat, "Estuary"))#2 observation
sequestration <- sequestration %>%
  filter(!str_detect(Habitat, "Parks"))#3 obs
sequestration <- sequestration %>%
  filter(!str_detect(Habitat, "Agriculture"))#4 obs


sequestration <- sequestration %>%
  filter(!str_detect(Urbanization_intensity_class, "Rural"))#2 observation


#categorical variables as factor
sequestration$Habitat <- as.factor(sequestration$Habitat)
sequestration$Urbanization_intensity_class <- as.factor(sequestration$Urbanization_intensity_class)
sequestration$ID <- as.factor(sequestration$ID)

sequestration$Temp_z  <- log(sequestration$`Temperature(average annual temperature-°C)`)
sequestration$Pluv_z  <- log(sequestration$`Pluviosity(average annual rainfall-mm)`)
sequestration$Pop_z   <- log(sequestration$`Population(hab)`)
sequestration$Dens_z  <- log(sequestration$`Densi_pop(hab/km2)`)
sequestration$Sequ <- log(sequestration$`Carbon_ton/year/ha`)

hist(sequestration$Temp_z)
hist(sequestration$Pluv_z)
hist(sequestration$Pop_z)
hist(sequestration$Dens_z)
hist(sequestration$Sequ)

levels(sequestration$Habitat)
levels(sequestration$Urbanization_intensity_class)

sequestration$Habitat <- relevel(sequestration$Habitat, ref = "Forest")
sequestration$Urbanization_intensity_class <- relevel(sequestration$Urbanization_intensity_class, ref = "Urban")

# no interactions
glmm11 <- lmer(
  Sequ ~ 
    Temp_z + Pluv_z + Habitat + Pop_z + 
    Dens_z + Urbanization_intensity_class +
    (1 | ID),
  data = sequestration
)
summary(glmm11)
AIC(glmm11)

#complete model: all interactions----
lmm22 <- lmer(
  Sequ ~ 
    Temp_z + Pluv_z + Habitat + Pop_z + Dens_z + Urbanization_intensity_class +
    Temp_z:Pluv_z +
    Temp_z:Habitat +
    Pluv_z:Habitat +
    Temp_z:Urbanization_intensity_class +
    Pluv_z:Urbanization_intensity_class +
    Habitat:Pop_z +
    Habitat:Dens_z +
    (1 | ID),
  data = sequestration
)
summary(lmm22)
AIC(lmm22)

anova(lmm22)

sr22 <- simulateResiduals(lmm22)
plot(sr22)

det_lmm22 <- tidy(lmm22,
                 effects = c("fixed", "ran_pars"),
                 conf.int = TRUE)
write.xlsx(det_lmm22, 
           file = "lmm_sequestration.xlsx",
           sheetName = "Comparacoes",
           rowNames = FALSE)


#good model seq (random)
glmm22 <- lmer(
  Sequ ~ 
    Temp_z + Pluv_z + Habitat + Pop_z + Dens_z + Urbanization_intensity_class +
    Pluv_z:Urbanization_intensity_class +
    Habitat:Pop_z +
    Habitat:Dens_z +
    (1 | ID),
  data = sequestration
)
summary(glmm22)# hab, hab-temp

(stp_glmm1 <- drop1(glmm22, test = "Chisq"))
AIC(glmm22)

sr22 <- simulateResiduals(glmm22)
plot(sr22)

plot(glmm22) # Homocedasticidade
qqnorm(residuals(glmm22)); qqline(residuals(glmm22)) # Normalidade

anova(glmm22)

#stock and sequestration
#data of C stock and sequestration

storage <- data %>%
  filter(!is.na(`C_area_ton/ha`))

seq_stock <- storage %>%
  filter(!is.na(`Carbon_ton/year/ha`))

hist(seq_stock$`C_area_ton/ha`)
hist(seq_stock$`Carbon_ton/year/ha`)

seq_stock$stock  <- log10(seq_stock$`C_area_ton/ha`)
seq_stock$seq <- log10(seq_stock$`Carbon_ton/year/ha`)

hist(seq_stock$stock)
hist(seq_stock$seq)

stse <- lmer(
  stock ~ seq  +
    (1 | ID),
  data = seq_stock
)
summary(stse)
AIC(stse)

plot(stock ~ seq, data = seq_stock)

ss22 <- simulateResiduals(stse)
plot(ss22)

plot(stse)
qqnorm(resid(stse))
qqline(resid(stse))

ggplot(seq_stock, aes(x = seq, y = stock)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm",
              color = "black",
              fill = "grey70",
              se = TRUE,
              linewidth = 1.1) +
  theme_classic(base_size = 14) +
  labs(
    x = "Log carbon sequestration",
    y = "Log carbon stock"
  ) +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

## Ian's code: extracting metrics

extract_metric <- function(x, candidates = NULL) {
  if (is.null(candidates)) candidates <- character(0)
  
  if (is.data.frame(x)) {
    for (nm in candidates) {
      if (nm %in% names(x)) {
        val <- suppressWarnings(as.numeric(x[[nm]][1]))
        if (length(val) == 1 && is.finite(val)) return(val)
      }
    }
    num_cols <- names(x)[vapply(x, is.numeric, logical(1))]
    if (length(num_cols) >= 1) {
      val <- suppressWarnings(as.numeric(x[[num_cols[1]]][1]))
      if (length(val) == 1 && is.finite(val)) return(val)
    }
  }
  
  if (is.list(x) && !is.data.frame(x)) {
    for (nm in candidates) {
      if (!is.null(x[[nm]])) {
        val <- suppressWarnings(as.numeric(x[[nm]][1]))
        if (length(val) == 1 && is.finite(val)) return(val)
      }
    }
  }
  
  if (is.atomic(x) && !is.null(names(x))) {
    for (nm in candidates) {
      if (nm %in% names(x)) {
        val <- suppressWarnings(as.numeric(x[[nm]][1]))
        if (length(val) == 1 && is.finite(val)) return(val)
      }
    }
  }
  
  if (is.atomic(x)) {
    vals <- suppressWarnings(as.numeric(x))
    vals <- vals[is.finite(vals)]
    if (length(vals) >= 1) return(vals[1])
  }
  
  return(NA_real_)
}

safe_r2 <- function(model) {
  out <- performance::r2_nakagawa(model)
  list(
    conditional = extract_metric(out, c("R2_conditional", "Conditional R2", "R2_cond")),
    marginal = extract_metric(out, c("R2_marginal", "Marginal R2", "R2_marg"))
  )
}

safe_icc <- function(model) {
  out <- performance::icc(model)
  extract_metric(out, c("ICC_adjusted", "ICC", "Adjusted ICC"))
}

safe_rmse <- function(model) {
  out <- performance::rmse(model)
  extract_metric(out, c("RMSE", "RMSE_adjusted"))
}

get_model_metrics <- function(model, label) {
  r2_vals <- safe_r2(model)
  
  tibble::tibble(
    Model = label,
    AIC = as.numeric(AIC(model)),
    BIC = as.numeric(BIC(model)),
    `Conditional R2` = r2_vals$conditional,
    `Marginal R2` = r2_vals$marginal,
    ICC = safe_icc(model),
    RMSE = safe_rmse(model)
  )
}

table_model_performance <- dplyr::bind_rows(
  get_model_metrics(glmm2,   "Carbon storage all interactions"),
  get_model_metrics(lmm2,   "Carbon storage reduced"),
  get_model_metrics(lmm22,   "Carbon sequestration all interactions"),
  get_model_metrics(glmm22,   "Carbon sequestration reduced"),
) %>%
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~ round(.x, 3))
  )

print(table_model_performance)
table_model_performance <- data.frame(table_model_performance)

library(openxlsx)

write.xlsx(table_model_performance, 
           file = "reduced_models.xlsx",
           sheetName = "Comparacoes",
           rowNames = FALSE)
