setwd('~/Desktop/SP3275')

library(readxl)
library(ggplot2)
library(class)
library(rpart)
library(rpart.plot)
library(e1071)
library(ROCR)
library(arules)
library(arulesViz)

### -------------------------------
### 1. SHOOT DENSITY
### -------------------------------
data <- read_excel("shoot_density.xlsx")

if (nrow(data) > 0 && !all(is.na(data))) {
  boxplot(
    data,
    col   = c("red", "blue", "yellow", "green"),
    names = c("Left Behind", "Left Betweeen", "Right Behind", "Right Between"),
    main  = "Boxplots for shoot density by Zone",
    xlab  = "Zone",
    ylab  = "Shoot Density (shoots/m²)",
    cex.lab  = 1.4,   # axis label size
    cex.axis = 1.2,   # tick label size
    cex.main = 1.5,   # title size
    pch      = 19,    # point symbol
    col.out  = "red"  # outliers in red
  )
} else {
  message("Skipping shoot_density.xlsx (empty data)")
}

### -------------------------------
### 2. BIOMASS
### -------------------------------
data <- read_excel("biomass.xlsx")

if (nrow(data) > 0 && !all(is.na(data))) {
  boxplot(
    data,
    col   = c("red", "blue", "yellow", "green"),
    names = c("Left Behind", "Left Betweeen", "Right Behind", "Right Between"),
    main  = "Boxplots for total biomass by Zone",
    xlab  = "Zone",
    ylab  = "Biomass (g/quadrat)",
    cex.lab  = 1.4,
    cex.axis = 1.2,
    cex.main = 1.5,
    pch      = 19,
    col.out  = "red"
  )
} else {
  message("Skipping biomass.xlsx (empty data)")
}

### -------------------------------
### 3. COVERAGE
### -------------------------------
data <- read_excel("coverage.xlsx")

if (nrow(data) > 0 && !all(is.na(data))) {
  boxplot(
    data,
    col   = c("red", "blue", "yellow", "green"),
    names = c("Left Behind", "Left Betweeen", "Right Behind", "Right Between"),
    main  = "Boxplots for coverage by Zone",
    xlab  = "Zone",
    ylab  = "Coverage (%)",
    cex.lab  = 1.4,
    cex.axis = 1.2,
    cex.main = 1.5,
    pch      = 19,
    col.out  = "red"
  )
} else {
  message("Skipping coverage.xlsx (empty data)")
}


library(readxl)

# Helper: remove outliers based on boxplot rule (same as base R)
remove_outliers <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(x)
  
  stats <- boxplot.stats(x)
  # keep only values between min and max whisker (non-outliers)
  x_clean <- x[x >= stats$stats[1] & x <= stats$stats[5]]
  return(x_clean)
}

make_two_boxplots <- function(filename, title_text, y_label) {
  
  data <- read_excel(filename)
  
  if (nrow(data) == 0 || all(is.na(data))) {
    message(paste("Skipping", filename, "(empty data)"))
    return(NULL)
  }
  
  # --- Separate groups ---
  LB <- data$`Left Behind`
  LBt <- data$`Left Between`
  RB <- data$`Right Behind`
  RBt <- data$`Right Between`
  
  # --- Remove outliers within each group (same rule as 4-group boxplots) ---
  LB_clean  <- remove_outliers(LB)
  LBt_clean <- remove_outliers(LBt)
  RB_clean  <- remove_outliers(RB)
  RBt_clean <- remove_outliers(RBt)
  
  # --- Combine cleaned groups into Left and Right ---
  Left_clean  <- c(LB_clean,  LBt_clean)
  Right_clean <- c(RB_clean, RBt_clean)
  
  # In case everything got removed
  if (length(Left_clean) == 0 || length(Right_clean) == 0) {
    message(paste("After outlier removal, one side is empty in", filename))
    return(NULL)
  }
  
  boxplot(
    list(Left = Left_clean, Right = Right_clean),
    col      = c("blue", "red"),     # Left = blue, Right = red
    names    = c("Left Zone", "Right Zone"),
    main     = title_text,
    xlab     = "Zone",
    ylab     = y_label,
    cex.lab  = 1.4,
    cex.axis = 1.2,
    cex.main = 1.5,
    pch      = 19,
    col.out  = "red"
  )
}

# -------------------------------------------------
# 1. SHOOT DENSITY (Left vs Right, outliers removed per group)
# -------------------------------------------------
make_two_boxplots(
  "shoot_density.xlsx",
  "Shoot Density by Left vs Right (outliers removed)",
  "Shoot Density (shoots/m²)"
)

# -------------------------------------------------
# 2. BIOMASS
# -------------------------------------------------
make_two_boxplots(
  "biomass.xlsx",
  "Total Biomass by Left vs Right (outliers removed)",
  "Biomass (g/quadrat)"
)

# -------------------------------------------------
# 3. COVERAGE
# -------------------------------------------------
make_two_boxplots(
  "coverage.xlsx",
  "Coverage by Left vs Right (outliers removed)",
  "Coverage (%)"
)

library(readxl)

make_two_boxplots <- function(filename, title_text, y_label) {
  
  data <- read_excel(filename)
  
  if (nrow(data) == 0 || all(is.na(data))) {
    message(paste("Skipping", filename, "(empty data)"))
    return(NULL)
  }
  
  # Combine the groups into Left and Right
  Left  <- c(data$`Left Behind`,  data$`Left Between`)
  Right <- c(data$`Right Behind`, data$`Right Between`)
  
  # Remove NAs
  Left  <- Left[!is.na(Left)]
  Right <- Right[!is.na(Right)]
  
  # Use a list so lengths can differ
  boxplot(
    list(Left = Left, Right = Right),
    col      = c("blue", "red"),     # Left = blue, Right = red
    names    = c("Left Zone", "Right Zone"),
    main     = title_text,
    xlab     = "Zone",
    ylab     = y_label,
    cex.lab  = 1.4,
    cex.axis = 1.2,
    cex.main = 1.5,
    pch      = 19,
    col.out  = "red"
  )
}

# -------------------------------------------------
# 1. SHOOT DENSITY
# -------------------------------------------------
make_two_boxplots(
  "shoot_density.xlsx",
  "Shoot Density by Left vs Right",
  "Shoot Density (shoots/m²)"
)

# -------------------------------------------------
# 2. BIOMASS
# -------------------------------------------------
make_two_boxplots(
  "biomass.xlsx",
  "Total Biomass by Left vs Right",
  "Biomass (g/quadrat)"
)

# -------------------------------------------------
# 3. COVERAGE
# -------------------------------------------------
make_two_boxplots(
  "coverage.xlsx",
  "Coverage by Left vs Right",
  "Coverage (%)"
)




library(readxl)
library(ggplot2)

# 1. Read file
df <- read_excel("shoot_density.xlsx")

# 2. Clean column names if they contain spaces
names(df) <- make.names(names(df))

# Now columns are:
# X, Y, Left.Behind, Left.Between, Right.Behind, Right.Between

# --- Plot 1: Left Behind ---
ggplot(df, aes(x = X, y = Y, colour = Left.Behind)) +
  geom_point(size = 4) +
  scale_colour_viridis_c() +
  theme_minimal() +
  labs(title = "Spatial Scatter Plot — Left Behind",
       x = "Longitude", y = "Latitude", colour = "Density")

# --- Plot 2: Left Between ---
ggplot(df, aes(x = X, y = Y, colour = Left.Between)) +
  geom_point(size = 4) +
  scale_colour_viridis_c() +
  theme_minimal() +
  labs(title = "Spatial Scatter Plot — Left Between",
       x = "Longitude", y = "Latitude", colour = "Density")

# --- Plot 3: Right Behind ---
ggplot(df, aes(x = X, y = Y, colour = Right.Behind)) +
  geom_point(size = 4) +
  scale_colour_viridis_c() +
  theme_minimal() +
  labs(title = "Spatial Scatter Plot — Right Behind",
       x = "Longitude", y = "Latitude", colour = "Density")

# --- Plot 4: Right Between ---
ggplot(df, aes(x = X, y = Y, colour = Right.Between)) +
  geom_point(size = 4) +
  scale_colour_viridis_c() +
  theme_minimal() +
  labs(title = "Spatial Scatter Plot — Right Between",
       x = "Longitude", y = "Latitude", colour = "Density")


library(readxl)

# --------------------------------------------------
# 1. List your four files
# --------------------------------------------------
files <- c("Right_Behind.xlsx",
           "Right_Between.xlsx",
           "Left_Behind.xlsx",
           "Left_Between.xlsx")

# --------------------------------------------------
# 2. Function: plot 3 scatter plots for ONE dataset
# --------------------------------------------------
plot_three_scatter <- function(df, title_prefix) {
  
  # Extract columns
  shoot_density <- df$shoot_density
  biomass       <- df$biomass
  coverage      <- df$coverage
  
  # Remove rows with any NA
  valid <- complete.cases(shoot_density, biomass, coverage)
  shoot_density <- shoot_density[valid]
  biomass       <- biomass[valid]
  coverage      <- coverage[valid]
  
  # Set up 1 row, 3 plots for this file
  par(mfrow = c(1, 3))
  
  # 1) shoot_density vs biomass
  plot(shoot_density, biomass,
       pch = 19,
       xlab = "Shoot density",
       ylab = "Biomass",
       main = paste(title_prefix, "\nShoot density vs Biomass"))
  
  # 2) shoot_density vs coverage
  plot(shoot_density, coverage,
       pch = 19,
       xlab = "Shoot density",
       ylab = "Coverage",
       main = paste(title_prefix, "\nShoot density vs Coverage"))
  
  # 3) biomass vs coverage
  plot(biomass, coverage,
       pch = 19,
       xlab = "Biomass",
       ylab = "Coverage",
       main = paste(title_prefix, "\nBiomass vs Coverage"))
}

# --------------------------------------------------
# 3. Loop over the 4 files → 3 plots per file
# --------------------------------------------------
all_data <- list()  # to store for combining later

for (f in files) {
  df <- read_excel(f)
  title_prefix <- tools::file_path_sans_ext(basename(f))
  plot_three_scatter(df, title_prefix)
  all_data[[title_prefix]] <- df
}

# --------------------------------------------------
# 4. Combine all 4 datasets and make 3 overall plots
# --------------------------------------------------
combined <- do.call(rbind, all_data)

shoot_density_all <- combined$shoot_density
biomass_all       <- combined$biomass
coverage_all      <- combined$coverage

valid_all <- complete.cases(shoot_density_all, biomass_all, coverage_all)
shoot_density_all <- shoot_density_all[valid_all]
biomass_all       <- biomass_all[valid_all]
coverage_all      <- coverage_all[valid_all]

# 3 combined scatter plots
par(mfrow = c(1, 3))

# A) Shoot density vs Biomass
plot(shoot_density_all, biomass_all,
     pch = 19,
     xlab = "Shoot density",
     ylab = "Biomass",
     main = "COMBINED\nShoot density vs Biomass")

# B) Shoot density vs Coverage
plot(shoot_density_all, coverage_all,
     pch = 19,
     xlab = "Shoot density",
     ylab = "Coverage",
     main = "COMBINED\nShoot density vs Coverage")

# C) Biomass vs Coverage
plot(biomass_all, coverage_all,
     pch = 19,
     xlab = "Biomass",
     ylab = "Coverage",
     main = "COMBINED\nBiomass vs Coverage")


library(readxl)

# --------------------------------------------------
# 1. List your four files
# --------------------------------------------------
files <- c("Right_Behind.xlsx",
           "Right_Between.xlsx",
           "Left_Behind.xlsx",
           "Left_Between.xlsx")

# --------------------------------------------------
# 2. Function: plot 3 scatter plots + regression lines
# --------------------------------------------------
plot_three_scatter <- function(df, title_prefix) {
  
  # Extract columns
  shoot_density <- df$shoot_density
  biomass       <- df$biomass
  coverage      <- df$coverage
  
  # Remove rows with any NA
  valid <- complete.cases(shoot_density, biomass, coverage)
  shoot_density <- shoot_density[valid]
  biomass       <- biomass[valid]
  coverage      <- coverage[valid]
  
  # Set up 1 row, 3 plots for this file
  par(mfrow = c(1, 3))
  
  ## 1) shoot_density vs biomass
  plot(shoot_density, biomass,
       pch = 19,
       xlab = "Shoot density",
       ylab = "Biomass",
       main = paste(title_prefix, "\nShoot density vs Biomass"))
  # linear regression line
  fit1 <- lm(biomass ~ shoot_density)
  abline(fit1, col = "red", lwd = 2)
  
  ## 2) shoot_density vs coverage
  plot(shoot_density, coverage,
       pch = 19,
       xlab = "Shoot density",
       ylab = "Coverage",
       main = paste(title_prefix, "\nShoot density vs Coverage"))
  fit2 <- lm(coverage ~ shoot_density)
  abline(fit2, col = "red", lwd = 2)
  
  ## 3) biomass vs coverage
  plot(biomass, coverage,
       pch = 19,
       xlab = "Biomass",
       ylab = "Coverage",
       main = paste(title_prefix, "\nBiomass vs Coverage"))
  fit3 <- lm(coverage ~ biomass)
  abline(fit3, col = "red", lwd = 2)
}

# --------------------------------------------------
# 3. Loop over the 4 files → 3 plots per file
# --------------------------------------------------
all_data <- list()  # to store for combining later

for (f in files) {
  df <- read_excel(f)
  title_prefix <- tools::file_path_sans_ext(basename(f))
  plot_three_scatter(df, title_prefix)
  all_data[[title_prefix]] <- df
}

# --------------------------------------------------
# 4. Combine all 4 datasets and make 3 overall plots
# --------------------------------------------------
combined <- do.call(rbind, all_data)

shoot_density_all <- combined$shoot_density
biomass_all       <- combined$biomass
coverage_all      <- combined$coverage

valid_all <- complete.cases(shoot_density_all, biomass_all, coverage_all)
shoot_density_all <- shoot_density_all[valid_all]
biomass_all       <- biomass_all[valid_all]
coverage_all      <- coverage_all[valid_all]

# 3 combined scatter plots + regression lines
par(mfrow = c(1, 3))

# A) Shoot density vs Biomass
plot(shoot_density_all, biomass_all,
     pch = 19,
     xlab = "Shoot density",
     ylab = "Biomass",
     main = "COMBINED\nShoot density vs Biomass")
fitA <- lm(biomass_all ~ shoot_density_all)
abline(fitA, col = "red", lwd = 2)

# B) Shoot density vs Coverage
plot(shoot_density_all, coverage_all,
     pch = 19,
     xlab = "Shoot density",
     ylab = "Coverage",
     main = "COMBINED\nShoot density vs Coverage")
fitB <- lm(coverage_all ~ shoot_density_all)
abline(fitB, col = "red", lwd = 2)

# C) Biomass vs Coverage
plot(biomass_all, coverage_all,
     pch = 19,
     xlab = "Biomass",
     ylab = "Coverage",
     main = "COMBINED\nBiomass vs Coverage")
fitC <- lm(coverage_all ~ biomass_all)
abline(fitC, col = "red", lwd = 2)




library(readxl)

# ----------------------------
# Helper: 3 plots + 3 fits
# ----------------------------
plot_and_fit <- function(df, title_prefix) {
  
  # Remove NA rows
  valid <- complete.cases(df$shoot_density, df$biomass, df$coverage)
  sd  <- df$shoot_density[valid]
  bm  <- df$biomass[valid]
  cov <- df$coverage[valid]
  
  par(mfrow = c(1,3))
  
  # 1) shoot density vs biomass
  plot(sd, bm,
       pch=19, xlab="Shoot density", ylab="Biomass",
       main=paste(title_prefix,"\nShoot density vs Biomass"))
  fit1 <- lm(biomass ~ shoot_density, data=data.frame(shoot_density=sd, biomass=bm))
  abline(fit1, col="red", lwd=2)
  
  # 2) shoot density vs coverage
  plot(sd, cov,
       pch=19, xlab="Shoot density", ylab="Coverage",
       main=paste(title_prefix,"\nShoot density vs Coverage"))
  fit2 <- lm(coverage ~ shoot_density, data=data.frame(shoot_density=sd, coverage=cov))
  abline(fit2, col="red", lwd=2)
  
  # 3) biomass vs coverage
  plot(bm, cov,
       pch=19, xlab="Biomass", ylab="Coverage",
       main=paste(title_prefix,"\nBiomass vs Coverage"))
  fit3 <- lm(coverage ~ biomass, data=data.frame(biomass=bm, coverage=cov))
  abline(fit3, col="red", lwd=2)
  
  return(list(fit1=fit1, fit2=fit2, fit3=fit3))
}

## ==========================================================
## 0. Load packages
## ==========================================================
library(readxl)

## ==========================================================
## 1. Helper function: 3 scatter plots + 3 regressions
##    - biomass ~ shoot_density
##    - coverage ~ shoot_density
##    - coverage ~ biomass
## ==========================================================
plot_and_fit <- function(df, title_prefix) {
  # Keep only complete rows
  valid <- complete.cases(df$shoot_density, df$biomass, df$coverage)
  sd  <- df$shoot_density[valid]
  bm  <- df$biomass[valid]
  cov <- df$coverage[valid]
  
  # In case everything is NA
  if (length(sd) == 0) {
    warning(paste("No valid rows for", title_prefix))
    return(list(fit1 = NULL, fit2 = NULL, fit3 = NULL))
  }
  
  # 3-panel layout
  par(mfrow = c(1, 3))
  
  ## 1) shoot_density vs biomass
  plot(sd, bm,
       pch = 19,
       xlab = "Shoot density",
       ylab = "Biomass",
       main = paste(title_prefix, "\nShoot density vs Biomass"))
  fit1 <- lm(bm ~ sd)  # biomass ~ shoot_density
  abline(fit1, col = "red", lwd = 2)
  
  ## 2) shoot_density vs coverage
  plot(sd, cov,
       pch = 19,
       xlab = "Shoot density",
       ylab = "Coverage",
       main = paste(title_prefix, "\nShoot density vs Coverage"))
  fit2 <- lm(cov ~ sd)  # coverage ~ shoot_density
  abline(fit2, col = "red", lwd = 2)
  
  ## 3) biomass vs coverage
  plot(bm, cov,
       pch = 19,
       xlab = "Biomass",
       ylab = "Coverage",
       main = paste(title_prefix, "\nBiomass vs Coverage"))
  fit3 <- lm(cov ~ bm)  # coverage ~ biomass
  abline(fit3, col = "red", lwd = 2)
  
  # Return all three models
  return(list(fit1 = fit1, fit2 = fit2, fit3 = fit3))
}

## ==========================================================
## 2. Read the four datasets
## ==========================================================
files <- c("Right_Behind.xlsx",
           "Right_Between.xlsx",
           "Left_Behind.xlsx",
           "Left_Between.xlsx")

all_data <- list()

for (f in files) {
  title <- tools::file_path_sans_ext(basename(f))
  all_data[[title]] <- read_excel(f)
}

## ==========================================================
## 3. INDIVIDUAL DATASETS (12 plots, 12 models)
## ==========================================================
fits_individual <- list()

for (name in names(all_data)) {
  cat("Plotting & fitting for dataset:", name, "\n")
  fits_individual[[name]] <- plot_and_fit(all_data[[name]], name)
}

## ==========================================================
## 4. COMBINED ALL (3 plots, 3 models)
## ==========================================================
combined_all <- do.call(rbind, all_data)
cat("Plotting & fitting for: COMBINED ALL\n")
fits_combined_all <- plot_and_fit(combined_all, "COMBINED ALL")

## ==========================================================
## 5. COMBINED RIGHT (Right_Behind + Right_Between)
##    (3 plots, 3 models)
## ==========================================================
combined_right <- rbind(all_data$Right_Behind,
                        all_data$Right_Between)
cat("Plotting & fitting for: COMBINED RIGHT\n")
fits_combined_right <- plot_and_fit(combined_right, "COMBINED RIGHT")

## ==========================================================
## 6. COMBINED LEFT (Left_Behind + Left_Between)
##    (3 plots, 3 models)
## ==========================================================
combined_left <- rbind(all_data$Left_Behind,
                       all_data$Left_Between)
cat("Plotting & fitting for: COMBINED LEFT\n")
fits_combined_left <- plot_and_fit(combined_left, "COMBINED LEFT")

## ==========================================================
## 7. PRINT REGRESSION SUMMARIES FOR ALL 21 MODELS
## ==========================================================

cat("\n========================================\n")
cat("REGRESSION SUMMARIES: INDIVIDUAL DATASETS\n")
cat("========================================\n\n")

for (name in names(fits_individual)) {
  fits <- fits_individual[[name]]
  cat("-----", name, "-----\n")
  
  cat("1) biomass ~ shoot_density\n")
  print(summary(fits$fit1))
  cat("\n")
  
  cat("2) coverage ~ shoot_density\n")
  print(summary(fits$fit2))
  cat("\n")
  
  cat("3) coverage ~ biomass\n")
  print(summary(fits$fit3))
  cat("\n\n")
}

cat("\n========================================\n")
cat("REGRESSION SUMMARIES: COMBINED ALL\n")
cat("========================================\n\n")

cat("COMBINED ALL: 1) biomass ~ shoot_density\n")
print(summary(fits_combined_all$fit1))
cat("\nCOMBINED ALL: 2) coverage ~ shoot_density\n")
print(summary(fits_combined_all$fit2))
cat("\nCOMBINED ALL: 3) coverage ~ biomass\n")
print(summary(fits_combined_all$fit3))
cat("\n\n")

cat("========================================\n")
cat("REGRESSION SUMMARIES: COMBINED RIGHT\n")
cat("========================================\n\n")

cat("COMBINED RIGHT: 1) biomass ~ shoot_density\n")
print(summary(fits_combined_right$fit1))
cat("\nCOMBINED RIGHT: 2) coverage ~ shoot_density\n")
print(summary(fits_combined_right$fit2))
cat("\nCOMBINED RIGHT: 3) coverage ~ biomass\n")
print(summary(fits_combined_right$fit3))
cat("\n\n")

cat("========================================\n")
cat("REGRESSION SUMMARIES: COMBINED LEFT\n")
cat("========================================\n\n")

cat("COMBINED LEFT: 1) biomass ~ shoot_density\n")
print(summary(fits_combined_left$fit1))
cat("\nCOMBINED LEFT: 2) coverage ~ shoot_density\n")
print(summary(fits_combined_left$fit2))
cat("\nCOMBINED LEFT: 3) coverage ~ biomass\n")
print(summary(fits_combined_left$fit3))
cat("\n\n")

cat("=== DONE: 21 plots + 21 regression summaries ===\n")
