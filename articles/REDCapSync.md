# REDCapSync

``` r
library(REDCapSync)
library(RosyREDCap)

TEST_CLASSIC <- load_test_project("TEST_CLASSIC", with_data = TRUE)
TEST_CLASSIC |> REDCap_diagram()
```
