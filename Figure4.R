library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(scales)
library(tibble)

# ------------------------------------------------------------
# 2. File path
# ------------------------------------------------------------

output_dir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/Red Tide/Mata Analysis"

input_file <- file.path(output_dir, "Figure4_clean.xlsx")

if (!file.exists(input_file)) {
  input_file <- file.path(output_dir, "Figure4.xlsx")
}

if (!file.exists(input_file)) {
  stop("Figure4_clean.xlsx or Figure4.xlsx not found in the folder.")
}

# ------------------------------------------------------------
# 3. Read data
# ------------------------------------------------------------

df <- read_excel(input_file, sheet = "Figure_Data")

print(names(df))

# ------------------------------------------------------------
# 4. Clean and prepare data
# ------------------------------------------------------------

df <- df %>%
  mutate(
    Panel = as.character(Panel),
    Study = as.character(Study),
    Evidence_Stream = as.character(Evidence_Stream),
    Organism_Compartment = as.character(Organism_Compartment),
    Endpoint = as.character(Endpoint),
    Metric_Type = as.character(Metric_Type),
    Unit = as.character(Unit),
    Include_In_Figure = as.character(Include_In_Figure),
    Plot_Family = as.character(Plot_Family),
    Pooling_Decision = as.character(Pooling_Decision),
    Extraction_Note = as.character(Extraction_Note),
    Estimate = as.numeric(Estimate),
    Lower = as.numeric(Lower),
    Upper = as.numeric(Upper),
    Lower_plot = ifelse(is.na(Lower), Estimate, Lower),
    Upper_plot = ifelse(is.na(Upper), Estimate, Upper)
  ) %>%
  filter(Include_In_Figure == "Yes") %>%
  filter(!is.na(Estimate))

# ------------------------------------------------------------
# 5. Create short Nature-style labels
# ------------------------------------------------------------

df <- df %>%
  mutate(
    Study_short = Study %>%
      str_replace_all(" et al\\.? ", " ") %>%
      str_replace_all("\\.", ""),
    
    Nature_Label = case_when(
      
      # Panel A: ng/g
      Plot_Family == "ng_g" & str_detect(Study, "Fire") & str_detect(Endpoint, "Baseline") ~
        "Dolphin liver, baseline",
      
      Plot_Family == "ng_g" & str_detect(Study, "Fire") & str_detect(Endpoint, "exposed") ~
        "Dolphin liver, exposed",
      
      Plot_Family == "ng_g" & str_detect(Study, "Fire") & str_detect(Organism_Compartment, "non-bloom") ~
        "Prey fish, non-bloom",
      
      Plot_Family == "ng_g" & str_detect(Study, "Fire") & str_detect(Organism_Compartment, "Sarasota") ~
        "Prey fish, bloom",
      
      Plot_Family == "ng_g" & str_detect(Study, "Fire") & str_detect(Organism_Compartment, "Whole fish") ~
        "Whole fish burden",
      
      Plot_Family == "ng_g" & str_detect(Study, "Fire") & str_detect(Organism_Compartment, "dolphins") ~
        "Stranded dolphins",
      
      Plot_Family == "ng_g" & str_detect(Study, "Fire") & str_detect(Organism_Compartment, "Manatees") ~
        "Manatees",
      
      Plot_Family == "ng_g" & str_detect(Study, "Abraham") ~
        "Gastropods",
      
      Plot_Family == "ng_g" & str_detect(Study, "McFarland") & str_detect(Organism_Compartment, "Oyster") ~
        "Oyster",
      
      Plot_Family == "ng_g" & str_detect(Study, "McFarland") & str_detect(Endpoint, "comparison") ~
        "Green mussel vs oyster",
      
      Plot_Family == "ng_g" & str_detect(Study, "McFarland") ~
        "Green mussel",
      
      # Panel B: ng/mL
      Plot_Family == "ng_ml" & str_detect(Organism_Compartment, "plasma") ~
        "Loggerhead plasma",
      
      Plot_Family == "ng_ml" & str_detect(Organism_Compartment, "liver") ~
        "Hatchling liver",
      
      Plot_Family == "ng_ml" & str_detect(Organism_Compartment, "yolk") ~
        "Hatchling yolk sac",
      
      Plot_Family == "ng_ml" & str_detect(Organism_Compartment, "eggs") ~
        "Unhatched eggs",
      
      # Panel C: percent / probability
      Plot_Family == "percent_probability" &
        str_detect(Study, "Newstead") &
        str_detect(Endpoint, "Lowest") &
        str_detect(Organism_Compartment, "Florida") ~
        "Red knots, Florida fall survival",
      
      Plot_Family == "percent_probability" &
        str_detect(Study, "Newstead") &
        str_detect(Endpoint, "Lowest") &
        str_detect(Organism_Compartment, "Texas") ~
        "Red knots, Texas fall survival",
      
      Plot_Family == "percent_probability" &
        str_detect(Study, "Newstead") &
        str_detect(Organism_Compartment, "Florida") ~
        "Red knots, Florida annual survival",
      
      Plot_Family == "percent_probability" &
        str_detect(Study, "Newstead") &
        str_detect(Organism_Compartment, "Texas") ~
        "Red knots, Texas annual survival",
      
      Plot_Family == "percent_probability" &
        str_detect(Study, "Newstead") &
        str_detect(Organism_Compartment, "Louisiana") ~
        "Red knots, Louisiana annual survival",
      
      Plot_Family == "percent_probability" &
        str_detect(Study, "Fire") &
        str_detect(Organism_Compartment, "Manatees") ~
        "Manatees, PbTx-positive",
      
      Plot_Family == "percent_probability" &
        str_detect(Study, "Fire") ~
        "Dolphins, PbTx-positive",
      
      Plot_Family == "percent_probability" &
        str_detect(Study, "Wetzel") &
        str_detect(Endpoint, "Training") ~
        "Manatee biomarker, training",
      
      Plot_Family == "percent_probability" &
        str_detect(Study, "Wetzel") ~
        "Manatee biomarker, validation",
      
      Plot_Family == "percent_probability" &
        str_detect(Study, "Reich") ~
        "NSP cases, visitor status",
      
      # Panel D: counts / fold-change
      Plot_Family == "count_fold" & str_detect(Study, "Abraham") ~
        "Gastropod NSP patients",
      
      Plot_Family == "count_fold" & str_detect(Study, "Reich") ~
        "Confirmed NSP cases",
      
      Plot_Family == "count_fold" & str_detect(Endpoint, "Superoxide") ~
        "Turtle PBL, SOD/thioredoxin",
      
      Plot_Family == "count_fold" & str_detect(Endpoint, "Ubiquinol") ~
        "In vitro PBL, ubiquinol",
      
      Plot_Family == "count_fold" & str_detect(Endpoint, "Beta") ~
        "In vitro PBL, beta-tubulin",
      
      Plot_Family == "count_fold" & str_detect(Endpoint, "Thiopurine") ~
        "Turtle PBL, TSMT",
      
      TRUE ~ paste0(Study_short, ": ", str_trunc(Organism_Compartment, 35))
    ),
    
    Nature_Label = paste0(Study_short, ": ", Nature_Label)
  )

# ------------------------------------------------------------
# 6. Add row IDs and export full label key
# ------------------------------------------------------------

df <- df %>%
  group_by(Plot_Family) %>%
  arrange(Estimate, .by_group = TRUE) %>%
  mutate(
    Row_Number = row_number(),
    Panel_Letter = case_when(
      Plot_Family == "ng_g" ~ "A",
      Plot_Family == "ng_ml" ~ "B",
      Plot_Family == "percent_probability" ~ "C",
      Plot_Family == "count_fold" ~ "D",
      TRUE ~ "X"
    ),
    Row_ID = paste0(Panel_Letter, Row_Number),
    Figure_Label = paste0(Row_ID, ". ", Nature_Label)
  ) %>%
  ungroup()

label_key <- df %>%
  select(
    Row_ID,
    Panel,
    Plot_Family,
    Study,
    Evidence_Stream,
    Organism_Compartment,
    Endpoint,
    Metric_Type,
    Estimate,
    Lower,
    Upper,
    Unit,
    Pooling_Decision,
    Extraction_Note
  )

write.csv(
  label_key,
  file.path(output_dir, "Figure4_NatureStyle_LabelKey_Arial18.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 7. Theme: Arial 18
# Plot titles bold
# All other text regular
# ------------------------------------------------------------

windowsFonts(Arial = windowsFont("Arial"))

theme_nature_18 <- theme_classic(base_family = "Arial", base_size = 18) +
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
      r = 18,
      b = 10,
      l = 18
    )
  )

# ------------------------------------------------------------
# 8. Helper functions
# ------------------------------------------------------------

order_labels <- function(dat) {
  dat %>%
    mutate(
      Figure_Label = factor(
        Figure_Label,
        levels = rev(unique(Figure_Label))
      )
    )
}

make_panel <- function(dat,
                       title_text,
                       x_title,
                       x_type = "continuous",
                       x_limits = NULL,
                       x_breaks = NULL) {
  
  p <- ggplot(dat, aes(x = Estimate, y = Figure_Label)) +
    
    geom_segment(
      aes(
        x = Lower_plot,
        xend = Upper_plot,
        y = Figure_Label,
        yend = Figure_Label
      ),
      linewidth = 0.8,
      color = "black"
    ) +
    
    geom_point(
      size = 3.2,
      color = "black"
    ) +
    
    labs(
      title = title_text,
      x = x_title,
      y = NULL
    ) +
    
    scale_y_discrete(
      expand = expansion(add = 0.65)
    ) +
    
    theme_nature_18
  
  if (x_type == "log10") {
    p <- p +
      scale_x_log10(
        limits = x_limits,
        breaks = x_breaks,
        labels = comma
      )
  } else {
    p <- p +
      scale_x_continuous(
        limits = x_limits,
        breaks = x_breaks,
        labels = comma
      )
  }
  
  return(p)
}

# ------------------------------------------------------------
# 9. Panel A: Toxin burden and food-web transfer, ng/g
# ------------------------------------------------------------

ngg <- df %>%
  filter(Plot_Family == "ng_g") %>%
  filter(Estimate > 0) %>%
  mutate(
    Lower_plot = ifelse(Lower_plot <= 0, Estimate, Lower_plot),
    Upper_plot = ifelse(Upper_plot <= 0, Estimate, Upper_plot)
  ) %>%
  arrange(Estimate) %>%
  order_labels()

p_ngg <- make_panel(
  dat = ngg,
  title_text = "A. Toxin burden and food-web transfer",
  x_title = "Brevetoxin concentration (ng/g; log scale)",
  x_type = "log10",
  x_limits = c(0.1, 200000),
  x_breaks = c(0.1, 1, 10, 100, 1000, 10000, 100000)
)

# ------------------------------------------------------------
# 10. Panel B: Turtle maternal transfer and tissue exposure, ng/mL
# ------------------------------------------------------------

ngml <- df %>%
  filter(Plot_Family == "ng_ml") %>%
  filter(Estimate > 0) %>%
  mutate(
    Lower_plot = ifelse(Lower_plot <= 0, Estimate, Lower_plot),
    Upper_plot = ifelse(Upper_plot <= 0, Estimate, Upper_plot)
  ) %>%
  arrange(Estimate) %>%
  order_labels()

p_ngml <- make_panel(
  dat = ngml,
  title_text = "B. Turtle maternal transfer and tissue exposure",
  x_title = "Brevetoxin concentration (ng/mL; log scale)",
  x_type = "log10",
  x_limits = c(0.8, 30),
  x_breaks = c(1, 2, 5, 10, 20)
)

# ------------------------------------------------------------
# 11. Panel C: Sentinel detection, survival, and classification
# ------------------------------------------------------------

pct <- df %>%
  filter(Plot_Family == "percent_probability") %>%
  arrange(Estimate) %>%
  order_labels()

p_pct <- make_panel(
  dat = pct,
  title_text = "C. Sentinel detection, survival, and classification",
  x_title = "Percent or survival probability (%)",
  x_type = "continuous",
  x_limits = c(0, 105),
  x_breaks = c(0, 25, 50, 75, 100)
)

# ------------------------------------------------------------
# 12. Panel D: Biological effects and case evidence
# ------------------------------------------------------------

other <- df %>%
  filter(Plot_Family == "count_fold") %>%
  arrange(Estimate) %>%
  order_labels()

p_other <- make_panel(
  dat = other,
  title_text = "D. Biological effects and case evidence",
  x_title = "Count or minimum fold-change",
  x_type = "continuous",
  x_limits = c(0, 25),
  x_breaks = c(0, 5, 10, 15, 20, 25)
)

# ------------------------------------------------------------
# 13. Combine panels
# No main header and no footer
# ------------------------------------------------------------

figure4 <- p_ngg / p_ngml / p_pct / p_other +
  plot_layout(
    heights = c(
      nrow(ngg),
      nrow(ngml),
      nrow(pct),
      nrow(other)
    )
  )

print(figure4)

# ------------------------------------------------------------
# 14. Save outputs
# Larger size is needed for Arial 18 readability
# ------------------------------------------------------------

ggsave(
  filename = file.path(output_dir, "Figure4_NatureStyle_Arial18.png"),
  plot = figure4,
  width = 14,
  height = 20,
  units = "in",
  dpi = 600,
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "Figure4_NatureStyle_Arial18.pdf"),
  plot = figure4,
  width = 14,
  height = 20,
  units = "in",
  limitsize = FALSE
)

ggsave(
  filename = file.path(output_dir, "Figure4_NatureStyle_Arial18.tiff"),
  plot = figure4,
  width = 14,
  height = 20,
  units = "in",
  dpi = 600,
  compression = "lzw",
  limitsize = FALSE
)

# ------------------------------------------------------------
# 15. Confirmation
# ------------------------------------------------------------

cat("Figure 4 saved successfully in:\n")
cat(output_dir, "\n")
cat("Files created:\n")
cat("Figure4_NatureStyle_Arial18.png\n")
cat("Figure4_NatureStyle_Arial18.pdf\n")
cat("Figure4_NatureStyle_Arial18.tiff\n")
cat("Figure4_NatureStyle_LabelKey_Arial18.csv\n")
