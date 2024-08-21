####################### Setup #################################################
library(openxlsx)
library(tidyr)
library(igraph)
library(dplyr)
library(survival)
library(ggplot2)
library(kableExtra)
library(stargazer)

# NOTE: MINIMAL COMMENTS IN THIS - LOGIC OF CODE IS IDENTICAL TO PCC_MODEL.R
# PLEASE REFER TO PCC_MODEL FOR ANY COMMENTS NEEDED

data <- read.xlsx("allcovariates.xlsx")
data$predicted_ethnicitylass <- factor(data$predicted_ethnicity, levels = c("white","asian", "black", "hispanic"))
data$predicted_ethnicity <- as.factor(data$predicted_ethnicity)

colnames(data) <- gsub("Year_", "deg_", colnames(data))
colnames(data) <- gsub("year_", "ec_", colnames(data))

########################## Breaking into P-C and C-HC ############################
data_sorted <- data %>%
  arrange(name)

data_sorted <- data_sorted %>%
  group_by(name) %>%
  mutate(prev_title = lag(title)) %>%
  ungroup()

previous_keywords <- c("line", "backs", "backers", "secondary", "safeties")
current_keyword <- "coordinator"
data_sorted <- data_sorted %>%
  mutate(
    coord_prom = ifelse(
      grepl(paste(previous_keywords, collapse = "|"), tolower(prev_title)) &
        grepl(current_keyword, tolower(title)),
      1, 0
    )
  )

current_keyword_hc <- "head coach"
data_sorted <- data_sorted %>%
  mutate(
    hc_prom = ifelse(
      grepl(current_keyword_hc, tolower(title)) & 
        !grepl(current_keyword_hc, tolower(prev_title)),
      1, 0
    )
  )

total_promotion <- sum(data_sorted$promotion, na.rm = TRUE)
total_coord_prom <- sum(data_sorted$coord_prom, na.rm = TRUE)
total_hc_prom <- sum(data_sorted$hc_prom, na.rm = TRUE)

eligible_data <- data_sorted

######################## Kaplan-Meier #########################################

black_coaches <- subset(eligible_data, predicted_ethnicity == "black")
white_coaches <- subset(eligible_data, predicted_ethnicity == "white")
asian_coaches <- subset(eligible_data, predicted_ethnicity == "asian")
hispanic_coaches <- subset(eligible_data, predicted_ethnicity = "hispanic")

kmblack_obj <- Surv(time=black_coaches$season, event = black_coaches$hc_prom)
kmblack_fit <- survfit(kmblack_obj ~ 1, data = black_coaches)

kmwhite_obj <- Surv(time=white_coaches$season, event = white_coaches$hc_prom)
kmwhite_fit <- survfit(kmwhite_obj ~ 1, data = white_coaches)

kmasian_obj <- Surv(time=asian_coaches$season, event = asian_coaches$hc_prom)
kmasian_fit <- survfit(kmasian_obj ~ 1, data = asian_coaches)

kmhispanic_obj <- Surv(time=hispanic_coaches$season, event = hispanic_coaches$hc_prom)
kmhispanic_fit <- survfit(kmhispanic_obj ~ 1, data = hispanic_coaches)

surv_data <- bind_rows(
  tibble(time = kmblack_fit$time, surv = kmblack_fit$surv, ethnicity = "black"),
  tibble(time = kmwhite_fit$time, surv = kmwhite_fit$surv, ethnicity = "white"),
  tibble(time = kmasian_fit$time, surv = kmasian_fit$surv, ethnicity = "asian"),
  tibble(time = kmhispanic_fit$time, surv = kmhispanic_fit$surv, ethnicity = "hispanic")
)

# Plot
ggplot(surv_data, aes(x = time, y = surv, color = ethnicity)) +
  geom_step() +
  labs(title = "Kaplan-Meier Survival Curves by Ethnicity",
       x = "Time",
       y = "Survival Probability") +
  theme_minimal() +
  scale_color_manual(values = c("black" = "black", "white" = "blue", "asian" = "green", "hispanic" = "red"))

# running log-rank test for difference in means of curves
km_fit <- survfit(Surv(season, coord_prom) ~ predicted_ethnicity, data = eligible_data)
log_rank_test <- survdiff(Surv(season, promotion) ~ predicted_ethnicity, data = eligible_data)
log_rank_test

# Extract + Clean Results
results <- data.frame(
  N = log_rank_test$n,
  Observed = log_rank_test$obs,
  Expected = log_rank_test$exp
)

results <- results %>%
  mutate(
    `Difference` = Observed - Expected,
    `Normalized Difference` = (Observed - Expected)^2 / Expected
  )

# Add chi-squared + p-value for interpretation
chi_sq <- log_rank_test$chisq
p_value <- 1 - pchisq(chi_sq, length(log_rank_test$n) - 1)

# Print the table using knitr
results_table <- results %>%
  mutate(across(c(Observed, Expected, `Difference`, `Normalized Difference`), round, 2)) %>%
  kable(digits = 2, format = "latex", caption = paste("Log-Rank Test Results: Chi-squared =", round(chi_sq, 2), ", p-value =", format(p_value, digits = 3)))
print(results_table)

eligible_data$predicted_ethnicity = relevel(eligible_data$predicted_ethnicity, ref = "white")

###################################### Cox - race only #############################

surv_obj <- Surv(time = eligible_data$season, event = eligible_data$hc_prom)
r_cox_model <- coxph(surv_obj ~ predicted_ethnicity, data=eligible_data)
summary(r_cox_model)
stargazer(r_cox_model)

################################### Cox - race and social capital #################
data_long <- eligible_data %>%
  pivot_longer(
    cols = starts_with("ec_"),
    names_to = "year",
    values_to = "ec"
  ) %>%
  pivot_longer(
    cols = starts_with("deg_"),
    names_to = "deg_year",
    values_to = "deg"
  ) %>%
  filter(substr(year, 4, 7) == substr(deg_year, 5, 8)) %>%
  select(franchise, name, promotion, predicted_ethnicity, year, deg, ec) %>%
  arrange(name, year)

data_long <- data_long %>%
  group_by(name) %>%
  mutate(
    year_numeric = as.numeric(substr(year, 5, 8)),
    ec_cumulative = cumsum(ec)
  ) %>%
  ungroup()

data_long <- data_long %>%
  group_by(year_numeric) %>%
  mutate(
    ec_cumulative_standardized = (ec_cumulative - mean(ec_cumulative, na.rm = TRUE)) / sd(ec_cumulative, na.rm = TRUE)
  ) %>%
  ungroup()

data_long <- data_long %>%
  group_by(name) %>%
  mutate(
    start_time = year_numeric - 1,
    stop_time = year_numeric,
    event = promotion
  ) %>%
  ungroup()

data_long$predicted_ethnicity = relevel(data_long$predicted_ethnicity, ref = "white")

sc_surv_obj <- with(data_long, Surv(start_time, stop_time, event))

# Fit model
sc_cox_model <- coxph(
  sc_surv_obj ~ predicted_ethnicity + deg + ec_cumulative_standardized,
  data = data_long
)
summary(sc_cox_model)
stargazer(sc_cox_model) #export

########################### Cox - fixed effects ################################

# Convert dataset to long format
data_long <- eligible_data %>%
  pivot_longer(
    cols = starts_with("ec_"),
    names_to = "year",
    values_to = "ec"
  ) %>%
  pivot_longer(
    cols = starts_with("deg_"),
    names_to = "deg_year",
    values_to = "deg"
  ) %>%
  filter(substr(year, 4, 7) == substr(deg_year, 5, 8)) %>%
  select(name, promotion, predicted_ethnicity, franchise, year, deg, ec) %>%
  arrange(name, year)

# Calculate cumulative eigenvector centralities
data_long <- data_long %>%
  group_by(name) %>%
  mutate(
    year_numeric = as.numeric(substr(year, 5, 8)),
    ec_cumulative = cumsum(ec)
  ) %>%
  ungroup()

# Standardize cumulative eigenvector centralities within each year
data_long <- data_long %>%
  group_by(year_numeric) %>%
  mutate(
    ec_cumulative_standardized = (ec_cumulative - mean(ec_cumulative, na.rm = TRUE)) / sd(ec_cumulative, na.rm = TRUE)
  ) %>%
  ungroup()

# Create survival object
data_long <- data_long %>%
  group_by(name) %>%
  mutate(
    start_time = year_numeric - 1,
    stop_time = year_numeric,
    event = promotion
  ) %>%
  ungroup()

surv_obj <- with(data_long, Surv(start_time, stop_time, event))
data_long$predicted_ethnicity = relevel(data_long$predicted_ethnicity, ref = "white")

# Fit model with fixed effects (coefficients inc.)
fecox_model <- coxph(
  surv_obj ~ predicted_ethnicity + deg + ec_cumulative_standardized + franchise,
  data = data_long
)
summary(fecox_model)

# Fit model with fixed effects through strata
fe1cox_model <- coxph(
  surv_obj ~ predicted_ethnicity + deg + ec_cumulative_standardized + strata(franchise),
  data = data_long
)
summary(fe1cox_model)
stargazer(fe1cox_model)

### Moderation #######################################################################

reshape_data <- eligible_data %>%
  select(-title, -first_name, -last_name)

reshape_data_long <- reshape_data %>%
  gather(key = "year", value = "deg", deg_2013:deg_2023) %>%
  gather(key = "year2", value = "ec", ec_2013:ec_2023) %>%
  filter(substr(year, 5, 8) == substr(year2, 4, 7)) %>%
  mutate(season = as.numeric(substr(year, 5, 8))) %>%
  select(-year2)

# Fit the Cox model
mod_cox_model <- coxph(Surv(time = reshape_data_long$season, event = reshape_data_long$promotion) ~ 
                         deg * predicted_ethnicity + 
                         ec * predicted_ethnicity,
                       data = reshape_data_long)

summary(mod_cox_model)
stargazer(mod_cox_model)
