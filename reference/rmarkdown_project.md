# rmarkdown_project

\`r lifecycle::badge("experimental")\` Generate a rmarkdown PDF from a
project object

## Usage

``` r
rmarkdown_project(project, dir_other)
```

## Arguments

- project:

  project object from REDCapSync package

- dir_other:

  Character. The directory where the dataset file will be saved. Default
  is the \`output\` folder within the database directory.

## Details

You will need the \[tinytex\] and \[reticulate\] packages. Follow errors
during your first use and afterwards it should work
