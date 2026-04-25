# RosyREDCap

``` r
library(REDCapSync)
library(RosyREDCap)

TEST_CLASSIC <- load_project("TEST_CLASSIC")
#> ! No cached projects... use `setup_project(...)`
#> ✔ Loaded TEST project TEST_CLASSIC!
#> ! Does not actually communicate with any REDCap API
TEST_CLASSIC |> REDCap_diagram()
```
