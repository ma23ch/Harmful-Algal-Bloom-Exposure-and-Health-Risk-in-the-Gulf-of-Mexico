library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(scales)

# ------------------------------------------------------------
# 2. Set file paths
# ------------------------------------------------------------

output_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/Red Tide/Mata Analysis"

input_file <- file.path(output_dir, "FigureML.xlsx")

if (!file.exists(input_file)) {
  input_file <- file.choose()
}

# ------------------------------------------------------------
# 3. Read FigureML dataset
# ------------------------------------------------------------

df <- read_excel(input_file, sheet = "FigureML_Data")

print(names(df))

# ------------------------------------------------------------
# 4. Prepare plotting dataset
# ------------------------------------------------------------

plot_df <- df %>%
  mutate(
    Panel = as.character(Panel),
    Plot_Group = as.character(Plot_Group),
    Row_ID = as.character(Row_ID),
    Study = as.character(Study),
    Target = as.character(Target),
    Model_Method = as.character(Model_Method),
    Lead_Time = as.character(Lead_Time),
    Metric = as.character(Metric),
    Primary_For_Figure = as.character(Primary_For_Figure),
    Figure_Label = as.character(Figure_Label),
    Metric_Percent = as.numeric(Metric_Percent),
    Metric_SD_Percent = as.numeric(Metric_SD_Percent),
    Row_Number = as.numeric(str_extract(Row_ID, "\\d+")),
    SD_Low = ifelse(
      is.na(Metric_SD_Percent),
      NA_real_,
      pmax(0, Metric_Percent - Metric_SD_Percent)
    ),
    SD_High = ifelse(
      is.na(Metric_SD_Percent),
      NA_real_,
      pmin(100, Metric_Percent + Metric_SD_Percent)
    )
  ) %>%
  filter(Primary_For_Figure == "Yes") %>%
  filter(!is.na(Metric_Percent))

# Export a clean label key for manuscript supplement
label_key <- plot_df %>%
  select(
    Row_ID,
    Panel,
    Study,
    Region_System,
    Target,
    Model_Method,
    Input_Data,
    Lead_Time,
    Metric,
    Metric_Percent,
    Additional_Metrics,
    Interpretation,
    Source_Table_or_Text,
    DOI_URL,
    Notes
  )

write.csv(
  label_key,
  file.path(output_dir, "FigureML_LabelKey.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 5. Theme: Arial 18
# Plot titles bold; all other text regular
# ------------------------------------------------------------

if (.Platform$OS.type == "windows") {
  windowsFonts(Arial = windowsFont("Arial"))
}

theme_figure_ml <- theme_classic(base_family = "Arial", base_size = 18) +
  theme(
    plot.title = element_text(
      family = "Arial",
      face = "bold",
      size = 18,
      color = "black",
      hjust = 0,
      margin = margin(b = 8)
    ),
    axis.title.x = element_text(
      family = "Arial",
      face = "plain",
      size = 18,
      color = "black",
      margin = margin(t = 8)
    ),
    axis.title.y = element_blank(),
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
      color = "black",
      lineheight = 0.95
    ),
    axis.line = element_line(
      color = "black",
      linewidth = 0.7
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 0.7
    ),
    axis.ticks.length = unit(2.2, "mm"),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(
      t = 10,
      r = 20,
      b = 10,
      l = 18
    )
  )

# ------------------------------------------------------------
# 6. Helper plotting function
# ------------------------------------------------------------

make_ml_panel <- function(dat, title_text) {

  dat <- dat %>%
    arrange(Row_Number) %>%
    mutate(
      Figure_Label = factor(
        Figure_Label,
        levels = rev(unique(Figure_Label))
      )
    )

  ggplot(dat, aes(x = Metric_Percent, y = Figure_Label)) +

    geom_segment(
      data = dat %>% filter(!is.na(Metric_SD_Percent)),
      aes(
        x = SD_Low,
        xend = SD_High,
        y = Figure_Label,
        yend = Figure_Label
      ),
      linewidth = 0.8,
      color = "black"
    ) +

    geom_point(
      size = 3.8,
      color = "black"
    ) +

    scale_x_continuous(
      limits = c(0, 105),
      breaks = c(0, 25, 50, 75, 100),
      labels = function(x) paste0(x)
    ) +

    scale_y_discrete(
      expand = expansion(add = 0.65)
    ) +

    labs(
      title = title_text,
      x = "Reported performance metric scaled to 0-100"
    ) +

    theme_figure_ml
}

# ------------------------------------------------------------
# 7. Panel A: Detection/classification performance
# ------------------------------------------------------------

class_df <- plot_df %>%
  filter(Plot_Group == "Detection/classification")

p_class <- make_ml_panel(
  dat = class_df,
  title_text = "A. Detection and classification performance"
)

# ------------------------------------------------------------
# 8. Panel B: Forecasting/regression/image-prediction performance
# ------------------------------------------------------------

forecast_df <- plot_df %>%
  filter(Plot_Group == "Forecasting/regression/image prediction")

p_forecast <- make_ml_panel(
  dat = forecast_df,
  title_text = "B. Forecasting, regression, and image-prediction performance"
)

# ------------------------------------------------------------
# 9. Combine panels
# No main title/header and no footer/caption
# ------------------------------------------------------------

figure_ml <- p_class / p_forecast +
  plot_layout(
    heights = c(
      max(nrow(class_df), 1),
      max(nrow(forecast_df), 1)
    )
  )

print(figure_ml)

# ------------------------------------------------------------
# 10. Save figure outputs
# ------------------------------------------------------------

ggsave(
  filename = file.path(output_dir, "FigureML.png"),
  plot = figure_ml,
  width = 14,
  height = 16,
  units = "in",
  dpi = 600,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "FigureML.pdf"),
  plot = figure_ml,
  width = 14,
  height = 16,
  units = "in",
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "FigureML.tiff"),
  plot = figure_ml,
  width = 14,
  height = 16,
  units = "in",
  dpi = 600,
  compression = "lzw",
  limitsize = FALSE
)

# ------------------------------------------------------------
# 11. Confirmation
# ------------------------------------------------------------

cat("FigureML saved successfully in:\n")
cat(output_dir, "\n")
cat("Files created:\n")
cat("FigureML.png\n")
cat("FigureML.pdf\n")
cat("FigureML.tiff\n")
cat("FigureML.csv\n")
