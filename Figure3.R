library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(patchwork)

# ------------------------------------------------------------
# 2. Set file path
# ------------------------------------------------------------

input_file <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/Red Tide/Mata Analysis/Figure3.xlsx"

output_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/Red Tide/Mata Analysis"

# Check whether file exists
if (!file.exists(input_file)) {
  stop("Excel file not found. Please check the file path and file name.")
}

# ------------------------------------------------------------
# 3. Read Figure 3 dataset
# ------------------------------------------------------------

df <- read_excel(input_file, sheet = "ForestPlot_Data")

# Check column names
print(names(df))

# ------------------------------------------------------------
# 4. Prepare data
# ------------------------------------------------------------

df <- df %>%
  mutate(
    Panel = factor(
      Panel,
      levels = c("Respiratory", "Gastrointestinal", "Neurologic")
    ),
    Study = as.character(Study),
    Outcome = as.character(Outcome),
    Effect_Measure = as.character(Effect_Measure),
    Effect_Scale = as.character(Effect_Scale),
    Estimate = as.numeric(Estimate),
    Lower_CI = as.numeric(Lower_CI),
    Upper_CI = as.numeric(Upper_CI),
    Study_Label = paste0(Study, "\n", Outcome),
    Effect_Label = case_when(
      Effect_Scale == "Ratio" ~ paste0(
        Effect_Measure, " = ", sprintf("%.3g", Estimate),
        " (", sprintf("%.3g", Lower_CI), "-", sprintf("%.3g", Upper_CI), ")"
      ),
      Effect_Scale == "Additive" ~ paste0(
        "\u03b2 = ", sprintf("%.2f", Estimate),
        " (", sprintf("%.2f", Lower_CI), "-", sprintf("%.2f", Upper_CI), ")"
      ),
      TRUE ~ ""
    )
  )

# ------------------------------------------------------------
# 5. Common figure theme
# Axis titles: Arial 18 bold
# All other text: Arial 18
# ------------------------------------------------------------

theme_fig3 <- theme_classic(base_family = "Arial", base_size = 18) +
  theme(
    plot.title = element_text(
      family = "Arial",
      face = "bold",
      size = 18,
      color = "black",
      hjust = 0
    ),
    axis.title.x = element_text(
      family = "Arial",
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.title.y = element_text(
      family = "Arial",
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.text.x = element_text(
      family = "Arial",
      face = "plain",
      size = 18,
      color = "black"
    ),
    axis.text.y = element_text(
      family = "Arial",
      face = "plain",
      size = 18,
      color = "black"
    ),
    legend.title = element_text(
      family = "Arial",
      face = "bold",
      size = 18,
      color = "black"
    ),
    legend.text = element_text(
      family = "Arial",
      face = "plain",
      size = 18,
      color = "black"
    ),
    strip.text = element_text(
      family = "Arial",
      face = "bold",
      size = 18,
      color = "black"
    ),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.grid = element_blank()
  )

# ------------------------------------------------------------
# 6. Panel A: Respiratory healthcare utilization
# ------------------------------------------------------------

resp_df <- df %>%
  filter(Panel == "Respiratory") %>%
  mutate(
    Study_Label = factor(Study_Label, levels = rev(unique(Study_Label))),
    Point_Type = ifelse(
      str_detect(tolower(Study), "pooled"),
      "Pooled",
      "Study"
    )
  )

p_resp <- ggplot(resp_df, aes(x = Estimate, y = Study_Label)) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.7,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = Lower_CI, xmax = Upper_CI),
    height = 0.15,
    linewidth = 0.8,
    color = "black"
  ) +
  geom_point(
    aes(shape = Point_Type),
    size = 4,
    color = "black"
  ) +
  scale_shape_manual(
    values = c("Study" = 16, "Pooled" = 18)
  ) +
  scale_x_log10(
    limits = c(0.8, 2.4),
    breaks = c(0.8, 1.0, 1.5, 2.0)
  ) +
  labs(
    title = "A. Respiratory healthcare utilization",
    x = "Relative effect estimate",
    y = NULL
  ) +
  theme_fig3 +
  theme(
    legend.position = "none"
  )

# ------------------------------------------------------------
# 7. Panel B: Gastrointestinal healthcare utilization
# ------------------------------------------------------------

gi_df <- df %>%
  filter(Panel == "Gastrointestinal") %>%
  mutate(
    Study_Label = factor(Study_Label, levels = rev(unique(Study_Label)))
  )

p_gi <- ggplot(gi_df, aes(x = Estimate, y = Study_Label)) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.7,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = Lower_CI, xmax = Upper_CI),
    height = 0.15,
    linewidth = 0.8,
    color = "black"
  ) +
  geom_point(
    size = 4,
    color = "black"
  ) +
  scale_x_log10(
    limits = c(0.85, 2.1),
    breaks = c(0.9, 1.0, 1.4, 2.0)
  ) +
  labs(
    title = "B. Gastrointestinal healthcare utilization",
    x = "Relative effect estimate",
    y = NULL
  ) +
  theme_fig3

# ------------------------------------------------------------
# 8. Panel C: Neurologic outcomes
# Diaz et al. is beta scale, so only this estimate is plotted here.
# Wang et al. neurologic aOR should remain supportive text/table only.
# ------------------------------------------------------------

neuro_df <- df %>%
  filter(
    Panel == "Neurologic",
    Effect_Measure == "Beta"
  ) %>%
  mutate(
    Study_Label = factor(Study_Label, levels = rev(unique(Study_Label)))
  )

p_neuro <- ggplot(neuro_df, aes(x = Estimate, y = Study_Label)) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.7,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = Lower_CI, xmax = Upper_CI),
    height = 0.15,
    linewidth = 0.8,
    color = "black"
  ) +
  geom_point(
    size = 4,
    color = "black"
  ) +
  scale_x_continuous(
    limits = c(-0.05, 0.45),
    breaks = c(0, 0.1, 0.2, 0.3, 0.4)
  ) +
  labs(
    title = "C. Neurologic outcomes",
    x = "Additional headache ED visits per county-month",
    y = NULL
  ) +
  theme_fig3

# ------------------------------------------------------------
# 9. Combine panels
# No main header and no footer/caption
# ------------------------------------------------------------

figure3 <- p_resp / p_gi / p_neuro +
  plot_layout(
    heights = c(1.25, 1.0, 1.0)
  )

# Display figure
print(figure3)

# ------------------------------------------------------------
# 10. Save outputs
# ------------------------------------------------------------

ggsave(
  filename = file.path(output_dir, "Figure3_forest_plot_no_header_footer.png"),
  plot = figure3,
  width = 12,
  height = 14,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "Figure3_forest_plot_no_header_footer.pdf"),
  plot = figure3,
  width = 12,
  height = 14
)

ggsave(
  filename = file.path(output_dir, "Figure3_forest_plot_no_header_footer.tiff"),
  plot = figure3,
  width = 12,
  height = 14,
  dpi = 600,
  compression = "lzw"
)

# ------------------------------------------------------------
# 11. Confirmation
# ------------------------------------------------------------

cat("Figure 3 saved successfully in:\n")
cat(output_dir, "\n")

