
## Orthogroups.tsv

The raw file was downloaded from [this paper](https://doi.org/10.1186/s13059-019-1650-2).

```{r eval=FALSE}
library(dplyr)
ortho <- readr::read_tsv("~/Downloads/Orthogroups.csv")

ortho_brassicaceae <- ortho %>%
    select(...1, Ath.pep.faa, Bol.pep.faa) %>%
    rename(c(OG = ...1, Ath = Ath.pep.faa, Bol = Bol.pep.faa))
    
readr::write_tsv(ortho_brassicaceae,
                 file = here::here("inst", "extdata", "Orthogroups.tsv")
)
```

```{bash eval=FALSE}
cd inst/extdata
gzip Orthogroups.tsv
```

