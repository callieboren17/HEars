# ============================================================================
# HEars Analysis (Public Use)
# ============================================================================
# Input: HEars_public.csv (already cleaned and scored; 92 observations).
# This script does not require raw item responses, dates, or identifiers.
# Run from a fresh R session with the CSV in the working directory.

# 1. Packages, import, and checks
library(dplyr)
library(ggplot2)
library(gtsummary)
library(gt)
library(flextable)

HEars <- read.csv("HEars_public.csv", stringsAsFactors = FALSE)

# For reproducibility, check that all required columns exist
required_cols <- c(
  "ha_group", "age", "sex", "race", "ethnicity", "education_years",
  "hl_duration", "be_4fpta", "empaq_score", "empaq_n_valid",
  "HH.comp", "sadl_global", "negA.comp", "socAx.comp", "ES.tscore"
)
missing_cols <- setdiff(required_cols, names(HEars))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Set HA users as the reference category for regression
HEars$ha_group <- factor(HEars$ha_group, levels = c("Yes", "No"))

# Verify the intended public-use analytic sample and group sizes
stopifnot(nrow(HEars) == 92)
stopifnot(sum(HEars$ha_group == "Yes", na.rm = TRUE) == 63)
stopifnot(sum(HEars$ha_group == "No", na.rm = TRUE) == 29)
stopifnot(sum(!is.na(HEars$sadl_global)) == 66)
stopifnot(all(!is.na(HEars$empaq_score)))

# Compare audiometric hearing loss by HA group (Welch's t-test)
bepta_ttest <- t.test(be_4fpta ~ ha_group, data = HEars)
bepta_ttest

# 2. Table 1: participant demographic and hearing characteristics
# Convert demographic fields to the types required by gtsummary
HEars <- HEars %>%
  mutate(
    age = as.numeric(age),
    be_4fpta = as.numeric(be_4fpta),
    education_years = as.numeric(education_years),
    hl_duration = as.numeric(hl_duration),
    sex = factor(sex),
    race = factor(race),
    ethnicity = factor(ethnicity)
  )

# Table 1 includes only participants with an assigned HA group
table_data <- HEars %>%
  filter(!is.na(ha_group))
# Audit HA group and total sample sizes
table(table_data$ha_group)
nrow(table_data)
# Summarize continuous variables as mean (SD) and categorical as n (%)
table1 <- table_data %>%
  select(
    ha_group,
    age,
    sex,
    be_4fpta,
    hl_duration,
    education_years,
    race,
    ethnicity
  ) %>%
  tbl_summary(
    by = ha_group,
    type = list(
      all_continuous() ~ "continuous"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    label = list(
      age ~ "Age (years)",
      sex ~ "Sex",
      be_4fpta ~ "Better-ear PTA (dB HL)",
      hl_duration ~ "Estimated hearing loss duration (years)",
      education_years ~ "Education (years)",
      race ~ "Race",
      ethnicity ~ "Ethnicity"
    ),
    # Remove individual missing-data rows
    missing = "no"
  ) %>%
  # Add overall column before HA groups
  add_overall(
    last = FALSE,
    col_label = "**Overall**"
  ) %>%
  # Format headers
  modify_header(
    label ~ "**Characteristic**",
    all_stat_cols() ~ "**{level}**\nN = {n}"
  ) %>%
  # Add table title
  modify_caption(
    "**Table 1. Participant Demographic and Hearing Characteristics**"
  ) %>%
  bold_labels()
# Construct a missing-data footnote for Table 1 variables
vars_to_check <- c(
  "age",
  "sex",
  "be_4fpta",
  "hl_duration",
  "education_years",
  "race",
  "ethnicity"
)
variable_labels <- c(
  age = "age",
  sex = "sex",
  be_4fpta = "better-ear PTA",
  hl_duration = "estimated hearing loss duration",
  education_years = "education",
  race = "race",
  ethnicity = "ethnicity"
)
missing_counts <- sapply(
  table_data[vars_to_check],
  function(x) sum(is.na(x))
)
missing_counts <- missing_counts[missing_counts > 0]
missing_note <- if (length(missing_counts) > 0) {
  paste0(
    "Missing data: ",
    paste(
      paste0(
        variable_labels[names(missing_counts)],
        " (n = ",
        missing_counts,
        ")"
      ),
      collapse = "; "
    ),
    "."
  )
} else {
  "No missing demographic data."
}
table_note <- paste(
  "Values are presented as mean (SD) or n (%).",
  "Percentages are calculated using nonmissing observations.",
  missing_note,
  paste(
    "Estimated hearing loss duration was calculated as",
    "current age minus self-reported age at hearing loss onset."
  ),
  "HA = hearing aid; PTA = pure-tone average."
)
# Display formatted table in R
table1_gt <- table1 %>%
  as_gt() %>%
  tab_source_note(
    source_note = table_note
  )
table1_gt
# Export Table 1 to Word in the current working directory
table1_word <- table1 %>%
  as_flex_table() %>%
  flextable::add_footer_lines(values = table_note) %>%
  flextable::autofit()
flextable::save_as_docx(
  table1_word,
  path = "Table1_Demographics.docx"
)

# 3. RQ1: HA use and empowerment, adjusted for better-ear PTA
# Center PTA so the intercept represents the reference group at mean PTA
HEars$be_4fpta_c <- HEars$be_4fpta - mean(
  HEars$be_4fpta,
  na.rm = TRUE
)
# Fit the additive linear model (reference group: HA users)
model_ha <- lm(
  empaq_score ~ ha_group + be_4fpta_c,
  data = HEars
)
summary(model_ha)

# Figure 1: EmpAQ versus better-ear PTA by HA group
my_colors <- c(
  "Yes" = "#003333",
  "No"  = "darkgoldenrod2"
)

p_ha <- ggplot(
  HEars,
  aes(x = be_4fpta, y = empaq_score)
) +
  geom_point(
    aes(color = ha_group),
    alpha = 0.8,
    size = 4,
    stroke = 0
  ) +
  scale_color_manual(
    values = my_colors,
    breaks = c("Yes", "No"),
    labels = c("Hearing aid users", "Non-users")
  ) +
  labs(
    x = "Better-Ear Pure-Tone Average (dB HL)",
    y = "Hearing Empowerment",
    color = NULL
  ) +
  theme_classic(base_size = 22) +
  theme(
    # Axis titles
    axis.title = element_text(
      size = 25,
      face = "bold",
      color = "black"
    ),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    # Axis tick labels
    axis.text = element_text(
      size = 20,
      color = "black"
    ),
    # Legend
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 21),
    legend.key.width = unit(1.2, "cm"),
    # Clean background
    panel.grid.major.y = element_line(
      color = "grey90",
      linewidth = 0.4
    ),
    panel.grid.major.x = element_blank(),
    # More prominent axes
    axis.line = element_line(
      linewidth = 0.8,
      color = "black"
    ),
    axis.ticks = element_line(linewidth = 0.8),
    # Presentation margins
    plot.margin = margin(20, 25, 20, 20)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 5, alpha = 1)
    )
  )
p_ha

# 4. RQ1 regression diagnostics and robust-SE sensitivity check
# Check linear regression assumptions visually
par(mfrow = c(2, 2))
plot(model_ha)
par(mfrow = c(1, 1))

# Calculate Cook's distances and externally studentized residuals.
diagnostics <- data.frame(
  cooks_d = cooks.distance(model_ha),
  studentized_residual = rstudent(model_ha)
)

# Display observations with the largest Cook's distances
diagnostics %>%
  arrange(desc(cooks_d)) %>%
  head(10)

# Compare standard inference with HC3 heteroskedasticity-robust SEs.
lmtest::coeftest(
  model_ha,
  vcov. = sandwich::vcovHC(model_ha, type = "HC3")
)

# 5. RQ2: EmpAQ correlations with hearing and psychosocial measures
# Six prespecified Pearson correlations; pairwise complete cases
predictors <- c(
  "be_4fpta",
  "HH.comp",
  "sadl_global",
  "negA.comp",
  "socAx.comp",
  "ES.tscore"
)
# Compute each Pearson r and p, then Holm-adjust across all six tests
corr_data <- lapply(predictors, function(x) {
  dat <- na.omit(HEars[, c("empaq_score", x)])
  test <- cor.test(
    dat$empaq_score,
    dat[[x]],
    method = "pearson"
  )
  data.frame(
    variable = x,
    n = nrow(dat),
    r = unname(test$estimate),
    p_raw = test$p.value
  )
}) |>
  bind_rows() |>
  mutate(
    p = p.adjust(p_raw, method = "holm"),
    variable = recode(
      variable,
      be_4fpta = "Better-ear PTA",
      HH.comp = "Auditory communication difficulties",
      sadl_global = "Hearing aid satisfaction",
      negA.comp = "Negative affect",
      socAx.comp = "Social anxiety",
      ES.tscore = "Emotional support"
    ),
    direction = if_else(r >= 0, "Positive", "Negative"),
    significant = p < .05,
    p_label = if_else(
      p < .001,
      "p < .001",
      paste0("p = ", sprintf("%.3f", p))
    ),
    label = paste0(
      "r = ", sprintf("%.2f", r),
      "    ", p_label
    )
  ) |>
  mutate(
    variable = reorder(variable, r)
  )
corr_data

# Plot Pearson r and list Holm-adjusted p for each comparison
my_colors <- c(
  "Positive" = "royalblue3",
  "Negative" = "red3"
)
p_corr <- ggplot(
  corr_data,
  aes(x = r, y = variable)
) +
  # Zero reference line
  geom_vline(
    xintercept = 0,
    color = "grey65",
    linewidth = 0.8
  ) +
  # Lines extending from zero to correlation
  geom_segment(
    aes(
      x = 0,
      xend = r,
      yend = variable,
      color = direction
    ),
    linewidth = 2,
    alpha = 0.7
  ) +
  # Correlation points
  geom_point(
    aes(color = direction),
    size = 6
  ) +
  # Correlation and p-value annotations
  geom_text(
    aes(
      x = 0.85,
      label = label,
      fontface = if_else(significant, "bold", "plain")
    ),
    hjust = 0,
    size = 5.5,
    color = "black",
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = my_colors,
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(-0.65, 1.65),
    breaks = seq(-0.6, 0.8, 0.2),
    labels = function(x) sprintf("%.1f", x)
  ) +
  labs(
    x = "Correlation with Hearing Empowerment (r)",
    y = NULL
  ) +
  theme_classic(base_size = 22) +
  theme(
    axis.title.x = element_text(
      size = 24,
      face = "bold",
      margin = margin(t = 18)
    ),
    axis.text.x = element_text(
      size = 19,
      color = "black"
    ),
    axis.text.y = element_text(
      size = 20,
      color = "black",
      margin = margin(r = 15)
    ),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey92",
      linewidth = 0.4
    ),
    plot.margin = margin(25, 30, 25, 25)
  )
p_corr

# 6. RQ3: incremental variance in SADL explained by EmpAQ
# Baseline model: ACD only
# Restrict both models to the same complete-case subset
sadl_data <- HEars %>%
  filter(!is.na(sadl_global), !is.na(HH.comp), !is.na(empaq_score))
model1 <- lm(sadl_global ~ HH.comp, data = sadl_data)

# Expanded model: add EmpAQ to test incremental explanatory value
model2 <- lm(sadl_global ~ HH.comp + empaq_score, data = sadl_data)
stopifnot(nobs(model1) == nobs(model2))

# Summaries
summary(model1)
summary(model2)

# Nested F test: both models must use identical observations.
anova(model1, model2)

# Inspect R-squared for the baseline and expanded models.
summary(model1)$r.squared
summary(model2)$r.squared

# Incremental proportion of variance explained by adding EmpAQ.
delta_r2 <- summary(model2)$r.squared - summary(model1)$r.squared
delta_r2

# 7. Reproducibility information
# Record R and package versions used to generate the published results.
sessionInfo()
