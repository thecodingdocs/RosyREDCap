# generate_redcap_ids

generate_redcap_ids

## Usage

``` r
generate_redcap_ids(project, needed, prefix = "", chosen_length = 6L)
```

## Arguments

- project:

  REDCapSync project object

- needed:

  integer length of new IDs needed

- prefix:

  character string of ID prefix such as "ID" for "ID001"

- chosen_length:

  integer length of padding for IDs

## Value

character string of new REDCap IDs not included in current object
