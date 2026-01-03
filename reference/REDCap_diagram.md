# Generate REDCap Project Diagram

Generates a diagram of the REDCap project structure based on the
\`project\` object.

## Usage

``` r
REDCap_diagram(
  project,
  static = FALSE,
  render = TRUE,
  duplicate_forms = TRUE,
  clean_names = TRUE,
  include_fields = FALSE,
  include_choices = FALSE,
  hierarchical = FALSE,
  direction = "LR",
  zoomView = TRUE
)
```

## Arguments

- project:

  project object from REDCapSync package

- static:

  Logical (TRUE/FALSE). If TRUE, generates a static diagram with
  \`DiagrammeR\`. If FALSE, generates an interactive diagram with
  \`visnetwork\`. Default is \`FALSE\`.

- render:

  Logical (TRUE/FALSE). If TRUE, renders the diagram. Default is
  \`TRUE\`.

- duplicate_forms:

  Logical (TRUE/FALSE). If TRUE, includes duplicate form nodes in the
  diagram. Default is \`TRUE\`.

- clean_names:

  Logical (TRUE/FALSE). If TRUE, cleans the names of the forms and
  fields in the diagram. Default is \`TRUE\`.

- include_fields:

  Logical (TRUE/FALSE). If TRUE, includes fields in the diagram. Default
  is \`FALSE\`.

- include_choices:

  Logical (TRUE/FALSE). If TRUE, includes choices in the diagram.
  Default is \`FALSE\`.

- hierarchical:

  Logical (TRUE/FALSE). If TRUE, generates a hierarchical diagram.
  Default is \`FALSE\`.

- direction:

  Character string specifying the direction of the diagram. Options are
  "LR" (left to right), "TB" (top to bottom), "RL" (right to left), and
  "BT" (bottom to top). Default is "LR".

- zoomView:

  Logical (TRUE/FALSE). If TRUE, user can zoom. Default is \`TRUE\`.

## Value

A diagram object representing the REDCap project structure.

## Details

This function generates a visual diagram of the REDCap project
structure, including forms, fields, and choices. It supports various
options such as rendering the diagram, including fields and choices, and
specifying the direction of the diagram.
