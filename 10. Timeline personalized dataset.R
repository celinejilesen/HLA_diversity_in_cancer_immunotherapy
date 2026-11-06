library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

timeline_neo <- read_excel(
  "I:/Research projects/Celine J/9. Side project HLA in cancer vaccinations/Final map/2 digit all data + undefined personalized.xlsx",
  sheet = "Neoantigen papers",
  range = "K2:L39",
  col_names = c("Year", "n_hla")
)

timeline_neo <- timeline_neo %>%
  mutate(
    Year = as.numeric(Year),
    n_hla = as.numeric(n_hla)
  ) %>%
  filter(!is.na(Year), !is.na(n_hla))

timeline_neo <- timeline_neo %>%
  filter(Year != 2026)

ggplot(
  timeline_neo,
  aes(
    x = Year,
    y = n_hla
  )
) +
  geom_count(
    aes(colour = factor(after_stat(n))),
    size = 5
  ) +
  scale_colour_manual(
    values = c(
    "2" = "#C8A2E8",   # light purple
    "1" = "#5E2A84"),  # dark purple
  name = "Number of studies") + 
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1,
    linetype = "dotted",
    colour = "black"
  ) +
  scale_x_continuous(
    breaks = seq(
      min(timeline_neo$Year),
      max(timeline_neo$Year),
      by = 1
    )
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      max(timeline_neo$n_hla),
      by = 10
    )
  ) +
  theme_classic() +
  labs(
    x = "Publication year",
    y = "Number of unique HLA alleles per publication"
  )

model <- lm(n_hla ~ Year, data = timeline_neo)
summary(model)$r.squared


ggsave(
  "Timeline neo.png",
  width = 150,
  height = 130,
  units = "mm",
  dpi = 600
)
