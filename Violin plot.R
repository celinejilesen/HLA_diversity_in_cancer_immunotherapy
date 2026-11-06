library(ggplot2)
library(dplyr)
library(readxl)

#import HLA frequencies of personalized dataset
HLA_dataset_neo <- read_excel(
  "I:/Research projects/Celine J/9. Side project HLA in cancer vaccinations/Final map/2 digit all data + undefined personalized.xlsx",
  sheet = "Data analysis neo",
  range = "D5:E111",
  col_names = c("Header", "Frequency"))
  
#import HLA frequencies of shared dataset
HLA_dataset_TAA <- read_excel(
    "I:/Research projects/Celine J/9. Side project HLA in cancer vaccinations/Final map/2 digit all data + undefined personalized.xlsx",
    sheet = "Data analysis TAA-CTA",
    range = "D2:E20",
    col_names = c("Header", "Frequency"))

#combine datasets
plot_data <- bind_rows(
  HLA_dataset_neo %>%
    mutate(dataset = "Neoantigen"),
  HLA_dataset_TAA %>%
    mutate(dataset = "TAA")
)

#plot
ggplot(
  plot_data,
  aes(
    x = dataset,
    y = Frequency,
    fill = dataset
  )
) +
  geom_violin(
    alpha = 0.8,
    colour = NA
  ) +
  geom_jitter(
    aes(colour = dataset),
    width = 0.08,
    size = 2.5,
    alpha = 0.8
  ) +
  scale_x_discrete(
    labels = c(
      "Neoantigen" = "Personalized antigen",
      "TAA" = "Shared antigen",
      size = 2.5
    )
  ) +
  scale_fill_manual(
    values = c(
      "Neoantigen" = "#E9D8F3",
      "TAA" = "#CFE8CF"
    )
  ) +
  scale_colour_manual(
    values = c(
      "Neoantigen" = "#5E2A84",
      "TAA" = "#2E6F40"
    )
  ) +
  theme_classic() +
  labs(
    x = NULL,
    y = "Fraction of studies investating HLA allele"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

#save plot
ggsave(
  "Violinplot.png",
  width = 110,
  height = 120,
  units = "mm",
  dpi = 600
)