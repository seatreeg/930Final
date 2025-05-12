library(dplyr)
library(moments)
library(formattable)


df1 <- read.csv("C:\\Users\\trogi\\Downloads\\consultDentistry\\mhDataFINALGITHUB.csv")


psych_scores <- c("phqScore", "gadScore", "burnScore")

summary_table <- df1 %>%
  group_by(DENTALYR_NESTED) %>%
  summarise(across(all_of(psych_scores), list(
    mean = ~mean(. , na.rm = TRUE),
    median = ~median(. , na.rm = TRUE),
    skew = ~skewness(. , na.rm = TRUE),
    kurtosis = ~kurtosis(. , na.rm = TRUE),
    variance = ~var(. , na.rm = TRUE)
  ), .names = "{.col}_{.fn}"))



#write.csv(summary_table, "summaryStatTable.csv")



library(psych)


dep_items <- df1[, c("DEP1", "DEP2", "DEP3", "DEP4", "DEP5", "DEP6", "DEP7", "DEP8")]

fa_dep <- fa(dep_items, nfactors = 1, rotate = "none")

print(fa_dep$loadings)
alpha(dep_items)



anx_items <- df1[, c("ANX1", "ANX2", "ANX3", "ANX4", "ANX5", "ANX6", "ANX7")]

fa_anx <- fa(anx_items, nfactors = 1, rotate = "none")
print(fa_anx$loadings)

alpha(anx_items)

