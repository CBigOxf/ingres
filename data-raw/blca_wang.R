## code to prepare `blca_wang` dataset goes here
blca.data = Read10X_h5("data-raw/BLCA_GSE130001.h5")
seu = CreateSeuratObject(counts = blca.data, project = "ingres",
                         min.cells = 3, min.features = 200)

#QC
seu[["percent.mt"]] = PercentageFeatureSet(seu, pattern = "^MT-")
VlnPlot(seu, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)
FeatureScatter(seu, feature1 = "nCount_RNA", feature2 = "percent.mt")
FeatureScatter(seu, feature1 = "nCount_RNA", feature2 = "nFeature_RNA")

seu = subset(seu, subset = nFeature_RNA > 0 &
               nFeature_RNA < 3000 & percent.mt < 3)

#standard pipeline
seu %<>% NormalizeData() %>%
  FindVariableFeatures() %>%
  ScaleData(features = rownames(seu))

#PCA
seu %<>% RunPCA(features = VariableFeatures(.))
DimPlot(seu)

#clustering
ElbowPlot(seu)
seu %<>% FindNeighbors(dims = 1:10) %>% FindClusters(resolution = 0.5) %>%
  RunTSNE(dims = 1:10)

DimPlot(seu, reduction = "tsne", pt.size = 1)

#keep only 10 cells from each cluster and remove unneeded slots
reduced.seu = seu %>% subset(downsample = 10) %>% DietSeurat(dimreducs = 'tsne')

table(Idents(seu))
table(Idents(reduced.seu))
DimPlot(reduced.seu, reduction = "tsne", pt.size = 1)

small_blca_wang = reduced.seu
usethis::use_data(small_blca_wang, overwrite = TRUE, compress = 'xz')

