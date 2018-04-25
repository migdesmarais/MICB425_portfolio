library(tidyverse)
library(phyloseq)
library(ggplot2)
library(dplyr)
library(stringr)
library(magrittr)
library(data.table)
library(kableExtra)

mothur.data <- as.data.table(mothur)

chlorom = subset_taxa(mothur, Phylum=="Chloroflexi")
OTU1 = as(otu_table(chlorom), "matrix")
if(taxa_are_rows(mothur)){OTU1 <- t(OTU1)}
OTUdf = as.data.frame(OTU1)
OTUs= as.data.frame(colnames(OTUdf))

chloroq = subset_taxa(qiime2, Phylum=="D_1__Chloroflexi")
ASV1 = as(otu_table(chloroq), "matrix")
if(taxa_are_rows(qiime2)){ASV1 <- t(ASV1)}
ASVdf = as.data.frame(ASV1)
ASVs= as.data.frame(colnames(ASVdf))

library(tidyverse)
library(phyloseq)
library(ggplot2)
library(dplyr)
library(stringr)
library(magrittr)
library(knitr)

phylum_name_mothur = "Chloroflexi"
phylum_name_qiime2 = "D_1__Chloroflexi"

m.tax_table = data.frame(m.norm@tax_table)
m.filtered = m.tax_table %>% 
  rownames_to_column('OTU') %>%
  filter(Phylum==phylum_name_mothur) %>%
  column_to_rownames('OTU')

otu_stats = data.frame("Estimate" = numeric(0), "Std. Error"= numeric(0),"t value"= numeric(0),"Pr(>|t|)"= numeric(0))
for (otu in row.names(m.filtered)){
  linear_fit = m.norm %>% 
    psmelt() %>% 
    filter(OTU==otu) %>% 
    
    lm(Abundance ~ Depth_m, .) %>% 
    summary()
  otu_data = linear_fit$fstatistic[,1]
  otu_stats <- rbind(otu_stats, otu_data)
}
colnames(otu_stats)<- (c("Estimate", "Std. Error","t value","Pr(>|t|)"))
row.names(otu_stats) <- row.names(m.filtered)
kable(otu_stats,caption="Correlation data of OTUs within Chloroflexi phylum across depth")

q.tax_table = data.frame(qiime2@tax_table)
q.filtered = q.tax_table %>% 
  rownames_to_column('OTU') %>%
  filter(Phylum==phylum_name_qiime2) %>%
  column_to_rownames('OTU')
