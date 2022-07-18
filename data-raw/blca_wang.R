## code to prepare `blca_wang` dataset goes here
blca.data = Seurat::Read10X_h5("data-raw/BLCA_GSE130001.h5")
seu = Seurat::CreateSeuratObject(counts = blca.data, project = "ingres",
                         min.cells = 3, min.features = 200)

#QC
seu[["percent.mt"]] = Seurat::PercentageFeatureSet(seu, pattern = "^MT-")
Seurat::VlnPlot(seu, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)
Seurat::FeatureScatter(seu, feature1 = "nCount_RNA", feature2 = "percent.mt")
Seurat::FeatureScatter(seu, feature1 = "nCount_RNA", feature2 = "nFeature_RNA")

seu = Seurat::subset(seu, subset = nFeature_RNA > 0 &
               nFeature_RNA < 3000 & percent.mt < 3)

#standard pipeline
seu %<>% Seurat::NormalizeData() %>%
  Seurat::FindVariableFeatures() %>%
  Seurat::ScaleData(features = rownames(seu))

#PCA
seu %<>% Seurat::RunPCA(features = VariableFeatures(.))
Seurat::DimPlot(seu)

#clustering
Seurat::ElbowPlot(seu)
seu %<>% Seurat::FindNeighbors(dims = 1:10) %>%
  Seurat::FindClusters(resolution = 0.5) %>%
  Seurat::RunTSNE(dims = 1:10)

Seurat::DimPlot(seu, reduction = "tsne", pt.size = 1)

#keep only 10 cells from each cluster and remove unneeded slots
reduced.seu = seu %>% Seurat::subset(downsample = 10) %>%
  Seurat::DietSeurat(dimreducs = 'tsne')

table(Seurat::Idents(seu))
table(Seurat::Idents(reduced.seu))
Seurat::DimPlot(reduced.seu, reduction = "tsne", pt.size = 1)

small_blca_wang = reduced.seu
usethis::use_data(small_blca_wang, overwrite = TRUE, compress = 'xz')

