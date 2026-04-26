# RosyREDCap

Load both REDCapSync and RosyREDCap.

``` r
library(REDCapSync)
library(RosyREDCap)
```

Launch the shiny app in your browser with …

``` r
 # by default will launch in your browser
run_RosyREDCap()
# launch in pop out RStudio window
run_RosyREDCap(options = NULL) 
# launch in RStudio Viewer
run_RosyREDCap(options = c(launch.browser = .rs.invokeShinyPaneViewer)) 
# launch in test_mode (built-in fake projects)
run_RosyREDCap(test_mode = FALSE)
```

You can also use plot functions from the shiny app to make visuals in
RStudio.

``` r

TEST_CLASSIC <- load_project("TEST_CLASSIC")
#> ! No cached projects... use `setup_project(...)`
#> ✔ Loaded TEST project TEST_CLASSIC!
#> ! Does not actually communicate with any REDCap API
TEST_CLASSIC |> REDCap_diagram()
```

``` r

load_project("TEST_REDCAPR_LONGITUDINAL") |> 
  REDCap_diagram(duplicate_forms = FALSE,
                 hierarchical = TRUE)
#> ! No cached projects... use `setup_project(...)`
#> ✔ Loaded TEST project TEST_REDCAPR_LONGITUDINAL!
#> ! Does not actually communicate with any REDCap API
```

As more functions are developed they will be shared. Please reach out if
you have ideas/suggestions
