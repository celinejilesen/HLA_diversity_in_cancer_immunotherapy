library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)

#import IEDB coverage data
coverage2 <- read_excel(
  "I:/Research projects/Celine J/9. Side project HLA in cancer vaccinations/Werkmap/Countries.xlsx",
  sheet = "Sheet1",
  range = "A21:C31")

#set order
coverage2$Region2 <- factor(
coverage2$Region2,
levels = rev(coverage2$Region2))

#convert data to long format
coverage_long2 <- coverage2 %>%
  pivot_longer(
    cols = c(Neo_Coverage2, TAA_Coverage2),
    names_to = "Type",
    values_to = "Coverage"
  ) %>%
  mutate(
    Type = factor(
      Type,
      levels = c("Neo_Coverage2", "TAA_Coverage2")
    )
  )

#plot color
my_colors2 <- diverging_hcl(
  10,
  palette = "Purple-Green"
)

#plot
ggplot(
  coverage_long2,
  aes(
    x = Coverage,
    y = Region2,
    fill = Type
  )
) +
  geom_col(
    position = position_dodge2(
      width = 0.8,
      reverse = TRUE
    ),
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      TAA_Coverage2 = "#2E6F40",
      Neo_Coverage2 = "#b98ccf"
    ),
    labels = c(
      Neo_Coverage2 = "Personalized antigen",
      TAA_Coverage2 = "Shared antigen"
    ),
    breaks = c(
      "Neo_Coverage2",
      "TAA_Coverage2"
    ),
    name = NULL
  ) +
  labs(
    x = "Regional coverage of studied alleles (%)",
    y = ""
  ) +
  theme_classic() +
  theme(
    legend.position = "right"
  )

#save plot
ggsave(
  "Coverage.png",
  width = 150,
  height = 120,
  units = "mm",
  dpi = 600
)

