# make_survival

make_survival

## Usage

``` r
make_survival(
  DF,
  start_col,
  end_col,
  time_col,
  status_col,
  strat_col,
  units = "months",
  allowed_levels = NULL,
  pval = TRUE,
  pval.coord = c(0L, 0.25),
  risk.table = TRUE,
  title = NULL,
  palette = NULL,
  conf.int = TRUE,
  xlim = NULL,
  legend.position = "top",
  tables.height = 0.25,
  show_stats = FALSE
)
```

## Arguments

- DF:

  data.frame

- start_col:

  character string of DF colname for start dates

- end_col:

  character string of DF colname for end dates

- time_col:

  character string of DF colname for time (will then ignore start and
  end)

- status_col:

  character string of DF colname for status (should be factor or 0/1 pr
  T/F)

- strat_col:

  character string of DF colname for stratification

- units:

  one of "weeks", "months", "years"

- allowed_levels:

  character string of allowed levels

- pval:

  logical for including pval

- pval.coord:

  coordinates of pval

- risk.table:

  logical

- title:

  character string for title

- palette:

  colors

- conf.int:

  logical for confidence interval shading

- xlim:

  limits for x axis

- legend.position:

  ong of top, bottom, left, right

- tables.height:

  table high as character

- show_stats:

  logcial for stat printing

## Value

survival curve plot
