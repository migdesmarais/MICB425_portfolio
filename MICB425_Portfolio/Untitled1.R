#Comments
install.packages("tidyverse")
library(tidyverse)

#Load data
read.table(file="Saanich.metadata.txt")

metadata <- read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t", na.strings="NAN")
OTU <- read.table(file="Saanich.OTU.txt", header=TRUE, row.names=1, sep="\t", na.strings="NAN")

#DAY2
library(tidyverse)
data %>% function
function(data)

oxygen = metadata %>% select(O2_uM)

metadata %>% select(matches("O2|oxygen"))

metadata %>% select(matches("CH4|methane"))
metadata %>% select(matches("temp"))
metadata %>% select(matches("depth"))
#CH$_nM
metadata %>% filter(CH4_nM > 100 & Temperature_C < 10) %>% select(Depth_m,CH4_nM,Temperature_C)





metadata %>% filter(O2_uM == 0)

metadata %>% 
  filter(O2_uM == 0) %>% 
  select(Depth_m)

metadata[metadata$O2_uM == 0, "Depth_m"]

metadata %>% 
  mutate(N2O_uM = N2O_nM/1000)

metadata %>% 
  transmute(N2O_uM = N2O_nM/1000)

metadata %>% 
  mutate(N2O_uM = N2O_nM/1000) %>% 
  ggplot() + geom_point(aes(x=Depth_m, y=N2O_uM))

