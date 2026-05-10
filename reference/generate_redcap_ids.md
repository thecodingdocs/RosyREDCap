# generate_redcap_ids

**\[experimental\]** generate redcap record ids

## Usage

``` r
generate_redcap_ids(
  project,
  needed,
  random = TRUE,
  prefix = "",
  chosen_length = 6L,
  start_n = 1L
)
```

## Arguments

- project:

  REDCapSync project object

- needed:

  integer length of new IDs needed

- random:

  T/F for random

- prefix:

  character string of ID prefix such as "ID" for "ID001"

- chosen_length:

  integer length of padding for IDs

- start_n:

  integer 0 or 1

## Value

character string of new REDCap IDs not included in current object
