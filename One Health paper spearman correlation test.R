########################################################
# Loading packages 
########################################################
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

1
library(ggpubr)

library(readxl)

#########################################################
# 1st WOHC
#########################################################



wohc <- read_excel("wohc1.xlsx")

a <- ggscatter(wohc, x = "UNDP", y = "corresponding", type = "robust", fill = "HDI",
               title = "1st WOHC, 2011",
               color = "HDI", 
               add = "loess", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "spearman",
               xlab = "Human Development Index", ylab = "corresponding") +
  theme_classic2()



b <- ggscatter(wohc, x = "UNDP", y = "collobration", type = "robust", fill = "HDI",
               color = "HDI",  
               add = "loess", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "spearman",
               xlab = "Human Development Index", ylab = "collaboration") +
  theme_classic2()

#######################################################
# 6th WOHC 
#######################################################
wohc6 <- read_excel("wohc6.xlsx")
wohc6



d <- ggscatter(wohc6, x = "UNDP", y = "corresponding",,type = "robust",fill = "HDI",
               title = "6th WOHC, 2020",
               color = "HDI", 
               add = "loess", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "spearman",
               xlab = "Human Development Index", ylab = "corresponding") +
  theme_classic2()

e <- ggscatter(wohc6, x = "UNDP", y = "collobration",type = "robust",fill = "HDI",
               color = "HDI", 
               add = "loess", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "spearman",
               xlab = "Human Development Index", ylab = "collobration") +
  theme_classic2()

#############################################################################
library(sf)
library(readxl)
library(leaflet)
library(countrycode)
library(tidyverse)
library(ggrepel)
library(ggspatial)

world_sf <- read_sf("C:/R/world-shapefiles")

wohc1 <- read_excel("C:/R/data/wohc1.xlsx")

# ==== Join ====
world_gdp <-world_sf %>%
  left_join(wohc1, 
            by=c("iso_a3"="iso_a3"))
View(world_gdp)

world_gdp1 <- subset(world_gdp, iso_a3!="ATA")

world_gdp1

wohc1_sf <- na.omit(world_gdp) 

View(wohc1_sf)


#######################################



c <- ggplot() +
  geom_sf(data = world_gdp1) +
  geom_sf(data = wohc1_sf,
          aes(fill= HDI)) +
  guides(fill = guide_legend(override.aes = list(size = 0.5)),
         size = guide_legend(override.aes = list(fill = "gray70"))) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) + 
  theme(legend.position = c(0.18, 0.02)) + 
  theme(legend.title = element_text(colour="black", size=16, 
                                    face="bold")) + 
  theme(legend.text = element_text(colour="black", size=12, 
                                   face="bold"))


wohc1 <- read_excel("C:/R/data/wohc6.xlsx")

# ==== Join ====
world_gdp <-world_sf %>%
  left_join(wohc6, 
            by=c("iso_a3"="iso_a3"))
View(world_gdp)

world_gdp1 <- subset(world_gdp, iso_a3!="ATA")

world_gdp1

wohc1_sf <- na.omit(world_gdp) 

View(wohc1_sf)



f <- ggplot() +
  geom_sf(data = world_gdp1) +
  geom_sf(data = wohc1_sf,
          aes(fill= HDI)) +
  guides(fill = guide_legend(override.aes = list(size = 0.5)),
         size = guide_legend(override.aes = list(fill = "gray70"))) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) + 
  theme(legend.position = c(0.18, 0.02)) + 
  theme(legend.title = element_text(colour="black", size=16, 
                                    face="bold")) + 
  theme(legend.text = element_text(colour="black", size=12, 
                                   face="bold"))
ggarrange(a, b,c,d,e,f, labels = c("a", "b","c","d","e","f"),
          common.legend = TRUE, legend = "bottom")



