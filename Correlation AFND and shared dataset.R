library(ggplot2)
library(ggh4x)
library(dplyr)
library(readxl)
library(ggrepel)
library(colorspace)
library(tidyr)

#load AFND data
load("I:/Research projects/Celine J/9. Side project HLA in cancer vaccinations/Rstudio/Allele.AllStudies_finalecht.RData")
afnd <- Allele.AllStudies

#convert sample size to numeric
afnd <- afnd %>%
  mutate(`Sample Size` = as.numeric(gsub(",", "", `Sample Size`)))

#calculate allele frequency only where carrier frequency data is available
afnd <- afnd %>%
  mutate(
    AlleleFrequency = if_else(
      is.na(AlleleFrequency) &
        !is.na(`% of individuals that have the allele`),
      
      1 - sqrt(
        1 - as.numeric(
          gsub("%", "", `% of individuals that have the allele`)
        ) / 100
      ),
      
      AlleleFrequency
    ),
    AlleleFrequency = replace_na(AlleleFrequency, 0)
  )

#HLA name harmonozation between AFND data and datasets created in this study
allele_group_taa <- list(
  "A2+"        = "^A\\*02:",
  "A24+"       = "^A\\*24:",
  "DRB1*4+"    = "^DRB1\\*04:",
  "DRB1*15+"   = "^DRB1\\*15:",
  "DPB1*05:01" = "^DPB1\\*05:01$",
  "DPB1*09:01" = "^DPB1\\*09:01$",
  "DRB1*08:03" = "^DRB1\\*08:03$",
  "A3+"        = "^A\\*03:",
  "A4+"        = "^A\\*04:",
  "A*11:01"    = "^A\\*11:01$",
  "B*07:02"    = "^B\\*07:02$",
  "B*08:01"    = "^B\\*08:01$",
  "B*44:03"    = "^B\\*44:03$",
  "DPB1*04:01" = "^DPB1\\*04:01$"
)

#calculate weighted mean of HLA frequencies per region
plot_data_taa <- bind_rows(
  lapply(names(allele_group_taa), function(group_name) {
    
    afnd %>%
      filter(
        grepl(
          allele_group_taa[[group_name]],
          Allele
        )
      ) %>%
      group_by(Region, Population) %>% 
      summarise(
        n = first(`Sample Size`),
        group_freq = sum(AlleleFrequency, na.rm = TRUE), #sum of HLA frequency in a population
        .groups = "drop"
      ) %>%
      group_by(Region) %>%
      summarise(
        total_n = sum(n, na.rm = TRUE),
        group_freq = weighted.mean( #weighted regional mean of HLA frequencies
          group_freq,
          w = n, na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      mutate(hla_group = group_name)
    
  })
)

plot_data_taa$hla_group <- factor(
  plot_data_taa$hla_group,
  levels = names(allele_group_taa)
)

#import HLA frequencies of personalized studies dataset
HLA_dataset_taa <- read_excel(
  "I:/Research projects/Celine J/9. Side project HLA in cancer vaccinations/Final map/2 digit all data + undefined personalized.xlsx",
  sheet = "Data analysis TAA-CTA",
  range = "D2:E20",
  col_names = c("Header", "Frequency")
)

study_freq_taa <- HLA_dataset_taa %>%
  select(
    hla_group = Header,
    fraction_studies_taa = Frequency
  ) %>%
  mutate(
    hla_group = sub("^HLA-", "", hla_group)
  )

#combine shared studies dataset and AFND data
correlation_data_taa <- plot_data_taa %>%
  left_join(
    study_freq_taa,
    by = "hla_group"
  ) 

#calculate Spearman correlation between AFND weighted population HLA frequencies en HLA frequencies from shared dataset per region
cor_labels_taa <- correlation_data_taa %>%
  group_by(Region) %>%
  summarise(
    r = cor(
      group_freq,
      fraction_studies_taa,
      method = "spearman",
      use = "complete.obs"
    ),
    p = cor.test(
      group_freq,
      fraction_studies_taa,
      method = "spearman"
    )$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(
      "ρ = ", round(r, 2),
      "\np = ", signif(p, 2)
    )
  )

#label six largest outliers
label_points_taa <- correlation_data_taa %>%
  group_by(Region) %>%
  group_modify(~{
    
    tmp <- .x
    
    fit <- lm(
      fraction_studies_taa ~ group_freq,
      data = tmp
    )
    
    tmp$residual <- resid(fit)
    
    bind_rows(
      
      tmp %>%
        mutate(abs_residual = abs(residual)) %>%
        slice_max(
          order_by = abs_residual,
          n = 6,
          with_ties = FALSE
        )
      
    ) %>%
      distinct(
        hla_group,
        .keep_all = TRUE
      )
    
  }) %>%
  ungroup()

#plot colors
hla_cols_taa <- diverging_hcl(
  n = length(unique(correlation_data_taa$hla_group)),
  palette = "Purple-Green")

names(hla_cols_taa) <- sort(
  unique(as.character(correlation_data_taa$hla_group))
)

#plot
ggplot(
  correlation_data_taa,
  aes(
    x = group_freq,
    y = fraction_studies_taa,
    colour = hla_group,
    size = fraction_studies_taa
  )
) +
  
  geom_point() +
  
  facet_wrap2(
    ~Region,
    axes = "all"
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "black",
    linetype = 2,
    linewidth = 0.7
  ) +
  
  geom_text(
    data = cor_labels_taa,
    aes(
      x = Inf,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 1.1,
    vjust = 1.1,
    size = 3.5
  ) +
  
  geom_text_repel(
    data = label_points_taa %>%
      filter(
        !is.na(group_freq),
        !is.na(fraction_studies_taa),
        !is.na(hla_group)
      ),
    aes(label = hla_group),
    colour = "black",
    size = 3,
    show.legend = FALSE,
    max.overlaps = Inf
  ) +
  
  scale_colour_manual(
    values = hla_cols_taa,
    name = "HLA group"
  ) +
  
  scale_size_continuous(
    name = "Fraction of studies",
    range = c(2, 8)
  ) +
  
  theme_classic() +
  
  labs(
    title = "Correlation between fraction of TAA studies and weighted mean of allele frequency",
    x = "Weighted mean of regional allele frequency",
    y = "Fraction of studies targeting allele"
  ) +
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 11
    ),
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    axis.title = element_text(
      size = 11
    ),
    legend.position = "right",
    legend.title = element_text(
      face = "bold",
      size = 11
    ),
    legend.text = element_text(
      size = 11)
    )

#save plot
ggsave(
  "Correlation taa.png",
  width = 280,
  height = 170,
  units = "mm",
  dpi = 600
)

#CSV file of weighted HLA frequencies 
excel_table <- correlation_data_taa %>%
  select(
    Region,
    Allele = hla_group,
    Weighted_Allele_Frequency = group_freq
  ) %>%
  arrange(Region, Allele)

write.csv(
  excel_table,
  "Weighted_allele_frequencies_by_region_taa.csv",
  row.names = FALSE
)
