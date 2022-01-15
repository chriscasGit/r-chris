#'''creates scatter plot and regression line'''
#'''useful for comparison of trend across several categories '''
#written by christian c

#import necessary options and libs
library(readxl)
library(tidyverse)
library(ggplot2)
require("ggrepel")
options(ggrepel.max.overlaps = Inf)

#assign data that goes to dataframe df1
df1 = slxl
#define name of pdf output
pdf("rplot8.pdf") 


#create list for loop over dep
varvec <- list("MB", "WB", "MT", "WT")

#create graph object p. adjust x, y, color, size, shape, title as needed
  p <- ggplot(df1, aes(x=drops, y=adds, color=department, size=changes, shape=department, ggtitle=department)) +
    geom_point()+
    geom_smooth(method = "lm", se = FALSE)+
    geom_text_repel(aes(label = mfp), size = 1, box.padding=0.5)
  
  p <- p + labs(title = "South Retail: SL FW22: Drops vs Adds by Cat/MFP",
                subtitle = varmfp,
                caption = "Christian C. 14/jan/2022")
  
  
  print(p)



dev.off() 

