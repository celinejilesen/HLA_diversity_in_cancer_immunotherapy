library(readxl)
library(ggplot2)
library(colorspace)

#import data
cumulativepopcov <- read_excel(
  "I:/Research projects/Celine J/9. Side project HLA in cancer vaccinations/Final map/2 digit all data + undefined personalized.xlsx",
  sheet = "Sheet1",
  range = "E2:F14"
)

#rename columns
colnames(cumulativepopcov) <- c("Category", "Percent")

#set order
cumulativepopcov$Category <- factor(
  cumulativepopcov$Category,
  levels = cumulativepopcov$Category
)

#plot color
pg <- diverging_hcl(
  50,
  palette = "Purple-Green"
)
my_colors2 <- pg[38:49]

#plot
ggplot(cumulativepopcov,
       aes(x = Category, y = Percent, fill = Category)) +
  geom_col(width = 0.8) +
  geom_hline(
    yintercept = 80,
    colour = "black",
    linetype = "dotted",
    linewidth = 0.8
  ) +
  scale_fill_manual(values = my_colors2) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = \(x) paste0(x, "%")
  ) +
  labs(
    x = NULL,
    y = "Cumulative population coverage of most common global HLA alleles (%)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#save plot
ggsave(
  "Cumulative coverage plot.png",
  width = 110,
  height = 150,
  units = "mm",
  dpi = 600
)


