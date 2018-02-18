install.packages("tidyverse")
library(tidyverse)

source("https://bioconductor.org/biocLite.R")
biocLite("phyloseq")
library(phyloseq)

metadata <- read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t", na.strings="NAN")
OTU <- read.table(file="Saanich.OTU.txt", header=TRUE, row.names=1, sep="\t", na.strings="NAN")

load("phyloseq_object.RData")
ggplot(metadata, aes(x=NO3_uM, y=Depth_m)) + 
  geom_point(shape=17, size=2, colour="purple") +
  scale_y_reverse()

metadata %>% 
  mutate(Temperature_F = (Temperature_C)*(9/5)+32) %>% 
  ggplot() + geom_point(aes(x=Temperature_F, y=Depth_m)) +
  scale_y_reverse()

physeq_percent = transform_sample_counts(physeq, function(x) 100 * x/sum(x))
plot_bar(physeq_percent, fill="Domain") + 
  geom_bar(aes(fill=Domain), stat="identity") +
  xlab("Sample depth") +
  ylab("% relative abundance") +
  ggtitle("Domain from 10-200 m in Saanich Inlet", subtitle = NULL)

metadata %>%
  gather(key="variable", value="value") %>%
  geom_line(aes(), stat="value") +
  facet_wrap(~Value)

metadatag <- metadata %>%
  select(O2_uM, PO4_uM, SiO2_uM, NO3_uM, NH4_uM, NO2_uM, Depth_m) %>%
  gather(key="variable", value="value")

ggplot(metadatag, aes(value)) +
  facet_wrap(~variable) +
  geom_line(y=Depth_m)
