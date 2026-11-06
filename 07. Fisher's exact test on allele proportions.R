#counts manually imported from datasets
a_above10 <- 54   #personalized dataset: alleles investigated in >10% of studies
a_below10 <- 52   #personalized dataset: alleles investigated in <10% of studies

b_above10 <- 2    #shared dataset: alleles investigated in >10% of studies
b_below10 <- 16   #shared dataset: alleles investigated in <10% of studies

#create 2x2 contingency table
tbl <- matrix(
  c(a_above10, a_below10,
    b_above10, b_below10),
  nrow = 2,
  byrow = TRUE
)

rownames(tbl) <- c("Neoantigen", "TAA")
colnames(tbl) <- c(">10pct", "<=10pct")

tbl

# Fisher's exact test
fisher.test(tbl)
