# List BUSCO data sets

List BUSCO data sets

## Usage

``` r
list_busco_datasets()
```

## Value

A hierarchically organized list of available data sets as returned by
`busco --list-datasets`.

## Examples

``` r
if(busco_is_installed()) {
    list_busco_datasets()
}
```
