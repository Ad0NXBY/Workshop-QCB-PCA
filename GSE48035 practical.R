#Load packages and data
library(tidyverse)
library(factoextra)

#Run PCA using mean centered normalized LCPM data from GSE48035, save results as "DS1.svd"
DS1.svd <-
  DS1_lcpm.filtered.norm %>%
  t() %>%
  prcomp(scale = F)
summary(DS1.svd)
#Generate Scree plot
fviz_eig(DS1.svd, addlabels = T)

#Plot Samples in 2d
fviz_pca_ind(DS1.svd, repel = F)

#Visualize metadata for sample ID
fviz_pca_ind(DS1.svd, habillage = DS1_sample.metaData$Sample, label = "none", addEllipses = T, invisible = "quali") +
  theme_pubr(base_size = 9)

#Visualize metadata for tissue type
fviz_pca_ind(DS1.svd, habillage = DS1_sample.metaData$Tissue.type, label = "none", addEllipses = T, invisible = "quali") +
  theme_pubr(base_size = 9)

#Visualize metadata for protocol used
fviz_pca_ind(DS1.svd, habillage = DS1_sample.metaData$Protocol, label = "none", addEllipses = T, invisible = "quali") +
  theme_pubr(base_size = 9)

#predominant source of variance within this dataset is Sample as there is no overlap 

#Change rownames of the rotation matrix  to gene names rather than the gene ID
gene.names.idx <- match(rownames(DS1_lcpm.filtered.norm),
                        DS1_gene.metaData$gene_id)
gene.names <- DS1_gene.metaData$gene_name[gene.names.idx]
gene.names.unique <- make.unique(gene.names)
rownames(DS1.svd$rotation) <- gene.names.unique

#plot feature coordinates
fviz_pca_var(DS1.svd, select.var = list(cos2 = 20), repel = T)

#print top 10 genes positively contributing to PC1
as.data.frame(DS1.svd$rotation[,1:2]) %>%
  slice_max(n=10, order_by = PC1)

#Create biplot
fviz_pca_biplot(DS1.svd, select.var = list(name = c("GFAP", "MKI67")),
                habillage = DS1_sample.metaData$Tissue.type,
                label = "var" , 
                invisible = "quali",
                addEllipses = T) + ggtitle("Tissue Type")
