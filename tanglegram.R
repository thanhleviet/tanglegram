rm(list = ls())

library(ape)
library(dendextend)
library(dplyr)
library(randomcoloR)
# source("https://bioconductor.org/biocLite.R")
# biocLite("DECIPHER")
library(DECIPHER)

den <- read.nexus.data("dengue.nex") %>% as.DNAbin()

#Estimate distance
den.dist <- dist.dna(den, as.matrix = TRUE) 

#bionj
den.tree.bionj <- bionj(den.dist)
#Remove neg branch length
den.tree.bionj$edge.length[den.tree.bionj$edge.length < 0] <- 0 
den.tree.bionj <- chronos(den.tree.bionj)
write.tree(den.tree.bionj, "den_bionj.nwk")


#nj
den.tree.nj <- nj(den.dist)
#Remove neg branch length
den.tree.nj$edge.length[den.tree.nj$edge.length < 0] <- 0 
den.tree.nj <- chronos(den.tree.nj)
write.tree(den.tree.nj, "den_nj.nwk")


#Tanglegram
den_nj <- ReadDendrogram("den_nj.nwk") %>% as.dendrogram()
den_bionj <- ReadDendrogram("den_bionj.nwk") %>% as.dendrogram()

labels <- den_nj %>% set("labels_to_char") %>% labels
labels <- as.data.frame(labels)
labels$country <- gsub("D4|[0-9]{2}","",labels$labels)

unique_country <- data.frame(country = unique(labels$country))

unique_country$color <- distinctColorPalette(nrow(unique_country))

labels_color <- left_join(labels, unique_country)
den_list <- dendlist(den_bionj, den_nj)

tanglegram(den_nj, den_bionj, 
           highlight_branches_lwd = FALSE,
           common_subtrees_color_lines = FALSE,
           highlight_distinct_edges = FALSE,
           hang = F,
           lwd = 1.5,
           edge.lwd = 2,
           color_lines = labels_color$color,
           sort = FALSE,
           columns_width = c(4,1,4),
           intersecting = FALSE,
           margin_outer = 1.6,
           dLeaf = 0.00000000005,
           main = "Colored by Country",
           cex_main = 1)

