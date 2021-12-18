
# Data in inst/extdata/

## Orthogroups.tsv.gz

The gene family file was downloaded from [PLAZA Dicots
5.0](https://doi.org/10.1186/s13059-019-1650-2) and converted to an
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

## short\_summary.txt (BUSCO output)

Here, we will run BUSCO in a Conda environment created with a temporary
installation of miniconda. This temporary miniconda installation is only
possible thanks to the `Herper` package.

``` r
# Download Ostreococcus tauri's genome
ota_genome <- file.path(tempdir(), "ota_genome.fasta.gz")
download.file("ftp://ftp.psb.ugent.be/pub/plaza/plaza_pico_03/Genomes/ota.fasta.gz", destfile = ota_genome)

system2("gunzip", args = ota_genome)
ota_genome <- gsub("\\.gz", "", ota_genome)

# Choose BUSCO dataset
dataset <- "chlorophyta_odb10"
```

``` r
# Install miniconda in a temporary directory
library(Herper)
miniconda_path <- file.path(tempdir(), "temp_miniconda")
env <- "busco_env"
install_CondaTools(tools = "busco", 
                   env = env, 
                   pathToMiniConda = miniconda_path)

# Test if it is working
with_CondaEnv(env,
              system2(command = "busco", args = "--list-datasets", stdout = TRUE),
              pathToMiniConda = miniconda_path)


# Run BUSCO on Ostreococcus tauri's genome
run_busco(sequence = ota_genome, outlabel = "ota", mode = "genome",
          lineage = dataset, threads = 2, outpath = "~/Documents", 
          download_path = "~/Documents/busco_datasets",
          envname = env, miniconda_path = miniconda_path, force = TRUE)

fs::file_copy("~/Documents/ota/run_chlorophyta_odb10/short_summary.txt", 
              here::here("inst", "extdata", "short_summary.txt"))
```

## ota\_subset.fa

This file contains the first 1,000 lines from the Herbaspirilllum
seropedicae SmR1 (GCA\_000143225) genome, and it was downloaded from
Ensembl Bacteria.

``` bash
# Bash
head -n 1001 Hse.fa > Hse_subset.fa
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
names(interpro_ath) <- c("Gene", "Annotation", "Description")

# Keep only genes included in orthogroups
data(og)
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
names(interpro_bol) <- c("Gene", "Annotation", "Description")

# Keep only genes included in orthogroups
data(og)
interpro_bol <- interpro_bol[interpro_bol$Gene %in% og$Gene, ]

# Save data
interpro_bol <- interpro_bol[, c(1,2)]
usethis::use_data(interpro_bol, compress = "xz", overwrite = TRUE)
```
