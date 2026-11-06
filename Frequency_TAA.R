#load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

#shared TAA data
plot_data_taa <- read_excel(
  "I:/Research projects/Celine J/9. Side project HLA in cancer vaccinations/Final map/2 digit all data + undefined personalized.xlsx",
  sheet = "Data analysis TAA-CTA",
  range = "D2:E20",
  col_names = c("Header", "Frequency")
)

#plot
ggplot(
  plot_data_taa,
  aes(x = reorder(Header, Frequency), y = Frequency, fill = Frequency)) +
  geom_col(fill = "#2E6F40") +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 11),
    plot.title = element_text(size = 11)
  ) +
  labs(
    title = "Frequency of HLA types in TAA literature",
    x = "",
    y = "Frequency (%)"
  )

ggsave(
  "Frequency_taa.png",
  width = 200,
  height = 100,
  units = "mm",
  dpi = 600
)