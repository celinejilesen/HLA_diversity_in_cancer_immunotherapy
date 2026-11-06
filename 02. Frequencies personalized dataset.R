#load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

#load data class I
plot_data_I <- read_excel(
  "I:/Research projects/Celine J/9. Side project HLA in cancer vaccinations/Final map/2 digit all data + undefined personalized.xlsx",
  sheet = "Data analysis neo",
  range = "A116:B173",
  col_names = c("Header", "Frequency")
)

#plot class I
classIplot <- ggplot(
  plot_data_I,
  aes(x = reorder(Header, -Frequency), y = Frequency, fill = Frequency)) +
  geom_col(fill = "#492050") +
  scale_y_continuous(labels = percent_format(),
                     breaks = seq(0, 1, 0.1)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  labs(
    title = "Frequency of HLA class I types in neoantigen literature",
    x = "",
    y = "Frequency (%)"
  )

classIplot

#save
ggsave(
  "Frequency_neoI.png",
  width = 180,
  height = 120,
  units = "mm",
  dpi = 600
)

#load data class II
plot_data_II <- read_excel(
  "I:/Research projects/Celine J/9. Side project HLA in cancer vaccinations/Final map/2 digit all data + undefined personalized.xlsx",
  sheet = "Data analysis neo",
  range = "C116:D163",
  col_names = c("Header", "Frequency")
)

#plot class II
classIIplot <- ggplot(
  plot_data_II,
  aes(x = reorder(Header, -Frequency), y = Frequency, fill = Frequency)) +
  geom_col(fill = "#492050") +
  scale_y_continuous(labels = percent_format()) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  labs(
    title = "Frequency of HLA class II types in neoantigen literature",
    x = "",
    y = "Frequency (%)"
  )

classIIplot

ggsave(
  "Frequency_neoII.png",
  width = 180,
  height = 120,
  units = "mm",
  dpi = 600
)


