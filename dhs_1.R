### 453 initial code 
### Kieran Douglas

# package instllation and data read-in
install.packages("tidyverse")
install.packages("gt")
library(tidyverse)
library(gt)

dhs <- read_csv("~/Documents/GitHub/bios/453/bios453/IAIR7EFL.csv")

# data cleaning for relevancy 
dhs_clean <- dhs %>% 
  select(m60_1, v481, m18_1, m19_1, m17_1, v214, v457) %>% 
  na.omit(dhs) %>% 
  rename(
    paradrug = m60_1,
    insured = v481,
    sizechild = m18_1,
    bwkg = m19_1,
    csect = m17_1,
    duration = v214,
    anemia = v457
  ) %>% 
  filter(
    paradrug!=8,
    bwkg <5000,
    sizechild!=8
  ) %>% 
  mutate(
    paradrug = as.factor(paradrug),
  )

# missing data report 

# concept map 

# summary tables

# graphical representation

# summary/interpretation 

# data analysis
model1 <- lm(data = dhs_clean, sizechild~paradrug+insured+bwkg+csect+duration+anemia)
summary(model1)
#0.00017 pvalue for association between paradrug use during pregnancy and birthweight
#7.08e-09 pvalue for association between paradrug use during pregnancy and mothers subjective rating of childs birth size
# seems like mothers might who use parasitic drugs during pregnancy are very likely to rate their child as being smaller at birth
# seems like mothers who use parasitc drugs during pregnancy are much more likely to have smaller children (consistent findings)
ggplot(data = dhs_clean, mapping = aes(x = sizechild, fill = paradrug)) +
  geom_bar()
