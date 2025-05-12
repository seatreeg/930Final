library(car)        
library(lmtest)     
library(tidyverse)

dfThisYear <- read.csv("C:/Users/trogi/Downloads/consultDentistry/secondSampleOnlyFINALGITHUB.csv")

traits <- c("phqScore", "burnScore", "gadScore")

for (trait in traits) {
  cat("\n==== Testing:", trait, "====\n")
  
  fml <- as.formula(paste(trait, "~ DENTALYR_NESTED"))
  mod <- lm(fml, data = dfThisYear)
  
  shapiro_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk p =", shapiro_res$p.value, "\n")
  
  levene_res <- leveneTest(fml, data = dfThisYear)
  cat("Levene Test p =", levene_res[1, "Pr(>F)"], "\n")
  
  bp_res <- bptest(mod)
  cat("Breusch-Pagan p =", bp_res$p.value, "\n")
}



dfThisYear <- read.csv("C:/Users/trogi/Downloads/consultDentistry/secondSampleOnlyFINALGITHUB.csv")
mod <- lm(phqScore ~ DENTALYR_NESTED, data = dfThisYear)

par(mfrow = c(2, 2))
plot(mod)







dfThisYear <- read.csv("C:/Users/trogi/Downloads/consultDentistry/secondSampleOnlyFINALGITHUB.csv")
traits <- c("phqScore", "burnScore", "gadScore")


for (trait in traits) {
  fml <- as.formula(paste(trait, "~ DENTALYR_NESTED"))
  mod <- lm(fml, data = dfThisYear)
  
  file_name <- paste0("4panel_", toupper(trait), ".png")
  
  png(filename = file_name, width = 800, height = 800)
  
  par(mfrow = c(2, 2))
  plot(mod, main = trait)
  
  dev.off()
  
  cat("Saved:", file_name, "\n")
}



# WRONG, HOW I DID IT IN PRESENTATION
dfThisYear <- read.csv("C:/Users/trogi/Downloads/consultDentistry/mhDataFINALGITHUB.csv")

traits <- c("phqScore", "burnScore", "gadScore")

for (trait in traits) {
  cat("\n==== Testing:", trait, "====\n")
  
  fml <- as.formula(paste(trait, "~ DENTALYR_NESTED"))
  mod <- lm(fml, data = dfThisYear)
  
  shapiro_res <- shapiro.test(residuals(mod))
  cat("Shapiro-Wilk p =", shapiro_res$p.value, "\n")
  
  levene_res <- leveneTest(fml, data = dfThisYear)
  cat("Levene Test p =", levene_res[1, "Pr(>F)"], "\n")
  
  bp_res <- bptest(mod)
  cat("Breusch-Pagan p =", bp_res$p.value, "\n")
}



#now for the pairwise

library(car)
library(lmtest)
library(tidyverse)

df <- read.csv("C:/Users/trogi/Downloads/consultDentistry/mhDataFINALGITHUB.csv")

traits <- c("phqScore", "burnScore", "gadScore")
pairs <- list(
  c("D1.2024", "D1.2025"),
  c("D3.2024", "D3.2025")
)

for (trait in traits) {
  for (grp in pairs) {
    cat("\n====", trait, " |", grp[1], "vs", grp[2], "====\n")
    
    df_sub <- df %>%
      filter(DENTALYR_NESTED %in% grp) %>%
      mutate(DENTALYR_NESTED = factor(DENTALYR_NESTED))
    
    fml <- as.formula(paste(trait, "~ DENTALYR_NESTED"))
    mod <- lm(fml, data = df_sub)
    
    sw_p <- shapiro.test(residuals(mod))$p.value
    cat("Shapiro-Wilk (normality): p =", sw_p, "\n")
    

    lev_p <- leveneTest(fml, data = df_sub)[1, "Pr(>F)"]
    cat("Levene’s Test (equal variance): p =", lev_p, "\n")
    
    bp_p <- bptest(mod)$p.value
    cat("Breusch-Pagan (heteroskedasticity): p =", bp_p, "\n")
  }
}
