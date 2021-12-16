
# Data in inst/extdata/

## Orthogroups.tsv

The gene family file was downloaded from [PLAZA Dicots
5.0](https://doi.org/10.1186/s13059-019-1650-2) and converted to a
OrthoFinder-like format with the following code:

``` r
library(dplyr)
fam <- readr::read_tsv("~/Downloads/genefamily_data.HOMFAM.csv.gz", skip = 2)
fam_brassicaceae <- fam %>%
    filter(species %in% c("ath", "bol")) %>%
    rename(Orthogroup = `#gf_id`) 

fam_ath <- fam_brassicaceae %>%
    filter(species == "ath") %>%
    group_by(Orthogroup) %>%
    mutate(Ath = paste(gene_id, collapse = ", ")) %>%
    ungroup() %>%
    distinct(Orthogroup, .keep_all = TRUE) %>%
    select(Orthogroup, Ath)

fam_bol <- fam_brassicaceae %>%
    filter(species == "bol") %>%
    group_by(Orthogroup) %>%
    mutate(Bol = paste(gene_id, collapse = ", ")) %>%
    ungroup() %>%
    distinct(Orthogroup, .keep_all = TRUE) %>%
    select(Orthogroup, Bol)

fam_final <- inner_join(fam_ath, fam_bol)

readr::write_tsv(fam_final[1:10, ],
                 file = here::here("inst", "extdata", "Orthogroups.tsv")
)
```

``` bash
cd inst/extdata
gzip Orthogroups.tsv
```

# Data in data/

## og.rda

``` r
og <- fam %>%
    dplyr::filter(species %in% c("ath", "bol")) %>%
    dplyr::rename(Orthogroup = `#gf_id`, Species = species, Gene = gene_id) %>%
    as.data.frame()
og$Species <- gsub("ath", "Ath", og$Species)
og$Species <- gsub("bol", "Bol", og$Species)

usethis::use_data(og, compress = "xz")
```

## interpro\_ath.rda

``` r
# Download and tidy the data set
download.file("https://ftp.psb.ugent.be/pub/plaza/plaza_public_dicots_05/InterPro/interpro.ath.csv.gz", destfile = "~/Downloads/interpro.ath.csv.gz")

interpro_ath <- read.csv("~/Downloads/interpro.ath.csv.gz", 
                         sep = "\t", skip = 8)[, c(1, 3, 4)]
names(interpro_ath) <- c("Gene", "Domain_ID", "Description")

# Keep only genes included in orthogroups
og <- system.file("extdata", "Orthogroups.tsv.gz", package = "cogeqc")
og <- read_orthogroups(og)
interpro_ath <- interpro_ath[interpro_ath$Gene %in% og$Gene, ]

# Save data
interpro_ath <- interpro_ath[, c(1,2)]
usethis::use_data(interpro_ath, compress = "xz", overwrite = TRUE)
```

## interpro\_bol.rda

``` r
# Download and tidy the data set
download.file("https://ftp.psb.ugent.be/pub/plaza/plaza_public_dicots_05/InterPro/interpro.bol.csv.gz", destfile = "~/Downloads/interpro.bol.csv.gz")

interpro_bol <- read.csv("~/Downloads/interpro.bol.csv.gz", 
                         sep = "\t", skip = 8)[, c(1, 3, 4)]
names(interpro_bol) <- c("Gene", "Domain_ID", "Description")

# Keep only genes included in orthogroups
og <- system.file("extdata", "Orthogroups.tsv.gz", package = "cogeqc")
og <- read_orthogroups(og)
interpro_bol <- interpro_bol[interpro_bol$Gene %in% og$Gene, ]

# Save data
interpro_bol <- interpro_bol[, c(1,2)]
usethis::use_data(interpro_bol, compress = "xz", overwrite = TRUE)
```
