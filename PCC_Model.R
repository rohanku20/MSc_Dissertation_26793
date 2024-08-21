####################### Setup ##################################################
library(openxlsx)
library(tidyr)
library(igraph)
library(dplyr)
library(survival)
library(ggplot2)
library(kableExtra)
library(stargazer)

data <- read.xlsx("allcovariates.xlsx")
data$predicted_ethnicitylass <- factor(data$predicted_ethnicity, levels = c("white","asian", "black", "hispanic"))
data$predicted_ethnicity <- as.factor(data$predicted_ethnicity)

colnames(data) <- gsub("Year_", "deg_", colnames(data))
colnames(data) <- gsub("year_", "ec_", colnames(data))

########################## Breaking into P-C and C-HC ##########################
data_sorted <- data %>%
  arrange(name)

data_sorted <- data_sorted %>%
  group_by(name) %>%
  mutate(prev_title = lag(title)) %>%
  ungroup()

previous_keywords <- c("line", "backs", "backers", "secondary", "safeties")
current_keyword <- "coordinator"

# Create the coord_prom variable
data_sorted <- data_sorted %>%
  mutate(
    coord_prom = ifelse(
      grepl(paste(previous_keywords, collapse = "|"), tolower(prev_title)) &
        grepl(current_keyword, tolower(title)),
      1, 0
    )
  )

current_keyword_hc <- "head coach"

# Create the hc_prom variable
data_sorted <- data_sorted %>%
  mutate(
    hc_prom = ifelse(
      grepl(current_keyword_hc, tolower(title)) & 
        !grepl(current_keyword_hc, tolower(prev_title)),
      1, 0
    )
  )

# Testing sum to make sure no data is lost
total_promotion <- sum(data_sorted$promotion, na.rm = TRUE)
total_coord_prom <- sum(data_sorted$coord_prom, na.rm = TRUE)
total_hc_prom <- sum(data_sorted$hc_prom, na.rm = TRUE)

# Filter out head coach
data_no_head_coach <- data_sorted %>%
  group_by(name) %>%
  filter(!any(grepl("head coach", tolower(title)))) %>%
  ungroup()

# Identify position coaches
keywords <- c("line", "backs", "backers", "secondary", "safeties")

# Find the first title for each individual- in filtered set
data_first_title <- data_no_head_coach %>%
  group_by(name) %>%
  filter(season == min(season)) %>%
  ungroup()

# Filter those whose first title ends with one of the specified keywords
data_eligible_first_title <- data_first_title %>%
  filter(grepl(paste(keywords, collapse = "|"), tolower(title)))

# Subset to keep relevant individual
eligible_data <- data_no_head_coach %>%
  filter(name %in% data_eligible_first_title$name)

# Relevel so white is reference category
eligible_data$predicted_ethnicity = relevel(eligible_data$predicted_ethnicity, ref = "white")

######################### Kaplan-Meier #########################################

# kaplan-meier - separately run for visualization
black_coaches <- subset(eligible_data, predicted_ethnicity == "black")
white_coaches <- subset(eligible_data, predicted_ethnicity == "white")
asian_coaches <- subset(eligible_data, predicted_ethnicity == "asian")
hispanic_coaches <- subset(eligible_data, predicted_ethnicity = "hispanic")

kmblack_obj <- Surv(time=black_coaches$season, event = black_coaches$coord_prom)
kmblack_fit <- survfit(kmblack_obj ~ 1, data = black_coaches)

kmwhite_obj <- Surv(time=white_coaches$season, event = white_coaches$coord_prom)
kmwhite_fit <- survfit(kmwhite_obj ~ 1, data = white_coaches)

kmasian_obj <- Surv(time=asian_coaches$season, event = asian_coaches$coord_prom)
kmasian_fit <- survfit(kmasian_obj ~ 1, data = asian_coaches)

kmhispanic_obj <- Surv(time=hispanic_coaches$season, event = hispanic_coaches$coord_prom)
kmhispanic_fit <- survfit(kmhispanic_obj ~ 1, data = hispanic_coaches)

# re-binding for visualization
surv_data <- bind_rows(
  tibble(time = kmblack_fit$time, surv = kmblack_fit$surv, ethnicity = "black"),
  tibble(time = kmwhite_fit$time, surv = kmwhite_fit$surv, ethnicity = "white"),
  tibble(time = kmasian_fit$time, surv = kmasian_fit$surv, ethnicity = "asian"),
  tibble(time = kmhispanic_fit$time, surv = kmhispanic_fit$surv, ethnicity = "hispanic")
)

# Plot using ggplot2
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

######################## Cox - race only ########################################

surv_obj <- Surv(time = eligible_data$season, event = eligible_data$coord_prom)
r_cox_model <- coxph(surv_obj ~ predicted_ethnicity, data=eligible_data)
summary(r_cox_model)

stargazer(r_cox_model)

#################### Cox - social capital + race ################################

# reshape to long for time-varying covariates
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

# find cumulative eigenvector centralities
data_long <- data_long %>%
  group_by(name) %>%
  mutate(
    year_numeric = as.numeric(substr(year, 5, 8)),
    ec_cumulative = cumsum(ec)
  ) %>%
  ungroup()

# standardize eigenvector centralities within year for comparison
data_long <- data_long %>%
  group_by(year_numeric) %>%
  mutate(
    ec_cumulative_standardized = (ec_cumulative - mean(ec_cumulative, na.rm = TRUE)) / sd(ec_cumulative, na.rm = TRUE)
  ) %>%
  ungroup()

# create survival object to allow data to come from relevant years
data_long <- data_long %>%
  group_by(name) %>%
  mutate(
    start_time = year_numeric - 1,
    stop_time = year_numeric,
    event = promotion
  ) %>%
  ungroup()

# relevel
data_long$predicted_ethnicity = relevel(data_long$predicted_ethnicity, ref = "white")

# fit model
sc_surv_obj <- with(data_long, Surv(start_time, stop_time, event))
sc_cox_model <- coxph(
  sc_surv_obj ~ predicted_ethnicity + deg + ec_cumulative_standardized,
  data = data_long
)

summary(sc_cox_model)
stargazer(sc_cox_model)
####################### Cox - fixed effect ####################################

# Reconvert dataset to long format
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

# cumulative e-centrality
data_long <- data_long %>%
  group_by(name) %>%
  mutate(
    year_numeric = as.numeric(substr(year, 5, 8)),
    ec_cumulative = cumsum(ec)
  ) %>%
  ungroup()

# standardize
data_long <- data_long %>%
  group_by(year_numeric) %>%
  mutate(
    ec_cumulative_standardized = (ec_cumulative - mean(ec_cumulative, na.rm = TRUE)) / sd(ec_cumulative, na.rm = TRUE)
  ) %>%
  ungroup()

# survival object
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

# fit model - with franchise fixed effects
fecox_model <- coxph(
  surv_obj ~ predicted_ethnicity + deg + ec_cumulative_standardized + franchise,
  data = data_long
)
summary(fecox_model)

# fit model - with strata so fixed effects do not have coefficients
fe1cox_model <- coxph(
  surv_obj ~ predicted_ethnicity + deg + ec_cumulative_standardized + strata(franchise),
  data = data_long
)

summary(fe1cox_model)
stargazer(fe1cox_model)

############################## Moderation Analysis ################################

# relevant columns to be reshaped
reshape_data <- eligible_data %>%
  select(-title, -first_name, -last_name)

# reshape
reshape_data_long <- reshape_data %>%
  gather(key = "year", value = "deg", deg_2013:deg_2023) %>%
  gather(key = "year2", value = "ec", ec_2013:ec_2023) %>%
  filter(substr(year, 5, 8) == substr(year2, 4, 7)) %>%
  mutate(season = as.numeric(substr(year, 5, 8))) %>%
  select(-year2)

# Fit model with interaction terms
mod_cox_model <- coxph(Surv(time = reshape_data_long$season, event = reshape_data_long$promotion) ~ 
                         deg * predicted_ethnicity + 
                         ec * predicted_ethnicity,
                       data = reshape_data_long)

# print and export
summary(mod_cox_model)
stargazer(mod_cox_model)

