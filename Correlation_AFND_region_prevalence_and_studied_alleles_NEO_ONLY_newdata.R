library(ggplot2)
library(ggh4x)
library(dplyr)
library(readxl)
library(ggrepel)
library(tidyr)
library(cowplot)
library(colorspace)

#load AFND data (obtained from script HLA_Alleles.qmd)
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
allele_group_neoantigen <- list(
  
  "A2+" = "^A\\*02(:|$)",
  "C7+" = "^C\\*07(:|$)",
  "A3+" = "^A\\*03(:|$)",
  "B*07:02" = "^B\\*07:02(:|$|[A-Z])",
  "A11+" = "^A\\*11(:|$)",
  "A24+" = "^A\\*24(:|$)",
  "A68+" = "^A\\*68(:|$)",
  "B*08:01" = "^B\\*08:01(:|$|[A-Z])",
  "B35+" = "^B\\*35(:|$)",
  "A1+" = "^A\\*01(:|$)",
  "B*18:01" = "^B\\*18:01(:|$|[A-Z])",
  "B40+" = "^B\\*40(:|$)",
  "B44+" = "^B\\*44(:|$)",
  "C3+" = "^C\\*03(:|$)",
  "C*05:01" = "^C\\*05:01(:|$|[A-Z])",
  "C*06:02" = "^C\\*06:02(:|$|[A-Z])",
  "B15+" = "^B\\*15(:|$)",
  "B27+" = "^B\\*27(:|$)",
  "B57+" = "^B\\*57(:|$)",
  "C4+" = "^C\\*04(:|$)",
  "A30+" = "^A\\*30(:|$)",
  "A*31:01" = "^A\\*31:01(:|$|[A-Z])",
  "A*32:01" = "^A\\*32:01(:|$|[A-Z])",
  "B51+" = "^B\\*51(:|$)",
  "C12+" = "^C\\*12(:|$)",
  "B38+" = "^B\\*38(:|$)",
  "DRB5*1+" = "^DRB5\\*1(:|$)",
  "A*25:01" = "^A\\*25:01(:|$|[A-Z])",
  "A*26:01" = "^A\\*26:01(:|$|[A-Z])",
  "B*37:01" = "^B\\*37:01(:|$|[A-Z])",
  "C2+" = "^C\\*02(:|$)",
  "C8+" = "^C\\*08(:|$)",
  "DRB1*4+" = "^DRB1\\*04(:|$)",
  "DRB1*11+" = "^DRB1\\*11(:|$)",
  "DRB1*13+" = "^DRB1\\*13(:|$)",
  "DRB3*02:02" = "^DRB3\\*02:02(:|$|[A-Z])",
  "A*23:01" = "^A\\*23:01(:|$|[A-Z])",
  "B*58:01" = "^B\\*58:01(:|$|[A-Z])",
  "C15+" = "^C\\*15(:|$)",
  "DRB1*3+" = "^DRB1\\*03(:|$)",
  "DRB1*15+" = "^DRB1\\*15(:|$)",
  "A33+" = "^A\\*33(:|$)",
  "B13+" = "^B\\*13(:|$)",
  "B39+" = "^B\\*39(:|$)",
  "B*52:01" = "^B\\*52:01(:|$|[A-Z])",
  "C1+" = "^C\\*01(:|$)",
  "C14+" = "^C\\*14(:|$)",
  "DRB1*1+" = "^DRB1\\*01(:|$)",
  "DRB1*07:01" = "^DRB1\\*07:01(:|$|[A-Z])",
  "DRB1*8+" = "^DRB1\\*08(:|$)",
  "DRB1*12+" = "^DRB1\\*12(:|$)",
  "DRB3*01:01" = "^DRB3\\*01:01(:|$|[A-Z])",
  "A29+" = "^A\\*29(:|$)",
  "B41+" = "^B\\*41(:|$)",
  "B*55:01" = "^B\\*55:01(:|$|[A-Z])",
  "C*16:01" = "^C\\*16:01(:|$|[A-Z])",
  "C*17:01" = "^C\\*17:01(:|$|[A-Z])",
  "DRB1*14+" = "^DRB1\\*14(:|$)",
  "DRB4*1+" = "^DRB4\\*1(:|$)",
  "DRB5*02:02" = "^DRB5\\*02:02(:|$|[A-Z])",
  "B14+" = "^B\\*14(:|$)",
  "B47+" = "^B\\*47(:|$)",
  "B*50:01" = "^B\\*50:01(:|$|[A-Z])",
  "DRB1*10:01" = "^DRB1\\*10:01(:|$|[A-Z])",
  "A*66:01" = "^A\\*66:01(:|$|[A-Z])",
  "A*69:01" = "^A\\*69:01(:|$|[A-Z])",
  "B*42:01" = "^B\\*42:01(:|$|[A-Z])",
  "B*46:01" = "^B\\*46:01(:|$|[A-Z])",
  "B*49:01" = "^B\\*49:01(:|$|[A-Z])",
  "B*53:01" = "^B\\*53:01(:|$|[A-Z])",
  "B*54:01" = "^B\\*54:01(:|$|[A-Z])",
  "B*56:01" = "^B\\*56:01(:|$|[A-Z])",
  "B*81:01" = "^B\\*81:01(:|$|[A-Z])",
  "C*35:01" = "^C\\*35:01(:|$|[A-Z])",
  "DRB1*09:01" = "^DRB1\\*09:01(:|$|[A-Z])",
  "DRB1*16:01" = "^DRB1\\*16:01(:|$|[A-Z])",
  "DRB3*03:01" = "^DRB3\\*03:01(:|$|[A-Z])")

#calculate weighted mean of HLA frequencies per region
plot_data_neo <- bind_rows(
  lapply(names(allele_group_neoantigen), function(group_name) {
    
    afnd %>%
      filter(
        grepl(
          allele_group_neoantigen[[group_name]],
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

plot_data_neo$hla_group <- factor(
  plot_data_neo$hla_group,
  levels = names(allele_group_neoantigen)
)

#import HLA frequencies of personalized studies dataset
HLA_dataset_neo <- read_excel(
  "I:/Research projects/Celine J/9. Side project HLA in cancer vaccinations/Final map/2 digit all data + undefined personalized.xlsx",
  sheet = "Data analysis neo",
  range = "D5:E111",
  col_names = c("Header", "Frequency")
)

study_freq_neo <- HLA_dataset_neo %>%
  select(
    hla_group = Header,
    fraction_studies_neo = Frequency
  ) %>%
  mutate(
    hla_group = sub("^HLA-", "", hla_group)
  )

#combine personalized studies dataset and AFND data
correlation_data_neo <- plot_data_neo %>%
  left_join(
    study_freq_neo,
    by = "hla_group"
  ) 

#calculate Spearman correlation between AFND weighted population HLA frequencies en HLA frequencies from personalized dataset per region
cor_labels_neo <- correlation_data_neo %>%
  group_by(Region) %>%
  summarise(
    r = cor(
      group_freq,
      fraction_studies_neo,
      method = "spearman",
      use = "complete.obs"
    ),
    p = cor.test(
      group_freq,
      fraction_studies_neo,
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
label_points_neo <- correlation_data_neo %>%
  group_by(Region) %>%
  group_modify(~{
    
    tmp <- .x
    
    fit <- lm(
      fraction_studies_neo ~ group_freq,
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
hla_cols_neo <- diverging_hcl(
  n = length(unique(correlation_data_neo$hla_group)),
  palette = "Purple-Green")

names(hla_cols_neo) <- sort(
  unique(as.character(correlation_data_neo$hla_group))
)

#plot
ggplot(
  correlation_data_neo,
  aes(
    x = group_freq,
    y = fraction_studies_neo,
    colour = hla_group,
    size = fraction_studies_neo
  )
) +
  
  geom_point() +
  
  facet_wrap2(
    ~Region,
    axes = "all") +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "black",
    linetype = 2,
    linewidth = 0.7
  ) +
  
  geom_text(
    data = cor_labels_neo,
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
  
  ggrepel::geom_text_repel(
    data = label_points_neo,
    aes(
      x = group_freq,
      y = fraction_studies_neo,
      label = hla_group
    ),
    inherit.aes = FALSE,
    colour = "black",
    size = 3,
    show.legend = FALSE
  ) +
  
  scale_colour_manual(
    values = hla_cols_neo,
    name = "HLA group"
  ) +
  
  scale_size_continuous(
    name = "Fraction of studies",
    range = c(2, 8)
  ) +
  
  theme_classic() +
  
  labs(
    title = "Correlation between fraction of neoantigen studies and weighted mean of allele frequency",
    x = "Weighted mean of regional allele frequency",
    y = "Fraction of studies targeting allele"
  ) +
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    axis.title = element_text(
      size = 12
    ),
    legend.position = "right",
    legend.title = element_text(
      face = "bold"
    )
  )

#save plot
ggsave(
  "Correlation neo.png",
  width = 250,
  height = 200,
  units = "mm",
  dpi = 600
)

#CSV file of weighted HLA frequencies 
excel_table <- correlation_data_neo %>%
  select(
    Region,
    Allele = hla_group,
    Weighted_Allele_Frequency = group_freq
  ) %>%
  arrange(Region, Allele)

write.csv(
  excel_table,
  "Weighted_allele_frequencies_by_region.csv",
  row.names = FALSE
)
