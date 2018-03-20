library(ggplot2)
library(tidyverse)
metadata <- read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t", na.strings="NAN")

metadatag <- metadata %>%
  gather(key="variable", value="value", O2_uM, PO4_uM, SiO2_uM, NO3_uM, NH4_uM, NO2_uM)

ggplot(metadatag) +
  facet_wrap(~variable, scales="free_x") +
  geom_path(aes(x=value, y=Depth_m)) +
  scale_y_reverse() +
  xlab("µM") +
  ylab("Depth (m)") +
  ggtitle("Nutrient concentration")

