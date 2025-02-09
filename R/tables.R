make_table1<-function(
    DF,
    group = "no_choice",
    variables,
    render.missing = F
){
  if(missing(variables))variables <- colnames(DF)
  has_group <- group!="no_choice"
  # if(any(!x))warning(paste0(x,collapse = ", ")," <- not in the form you specified")
  if(!is_something(variables))return(h3("Nothing to return!")) # stop("Provide variable names of at least length 1!")
  # caption  <- "Basic stats"
  # footnote <- "ᵃ Also known as Breslow thickness"
  BAD <- variables %>% vec1_not_in_vec2(colnames(DF))
  if(length(BAD)>0){
    variables <- variables[which(!variables%in%BAD)]
    warning("Following variables dropped (not included in DF): " %>% paste0(as_comma_string(BAD)),immediate. = T)
  }
  if(!is_something(variables))return(h3("Nothing to return!")) # stop("Provide variable names of at least length 1!")
  DF <- DF[,index_na(DF,invert = T),drop = F]
  BAD <- variables %>% vec1_not_in_vec2(colnames(DF))
  if(length(BAD)>0){
    variables <- variables[which(!variables%in%BAD)]
    warning("Following variables dropped (only NAs in DF): " %>% paste0(as_comma_string(BAD)),immediate. = T)
  }
  if(!is_something(variables))return(h3("Nothing to return!")) # stop("Provide variable names of at least length 1!")
  if(has_group){
    if(!group%in%colnames(DF))stop("`group` not included in DF colnames")
    if(!is.factor(DF[[group]]))DF$group <-  DF[[group]] %>% factor()
    DF <- DF[which(!is.na(DF[[group]])),] %>% clone_attr(from = DF)
    variables<-variables[which(variables!=group)]
  }
  if(!is_something(variables))return(h3("Nothing to return!")) # stop("Provide variable names of at least length 1!")
  forumla <- paste0(variables, collapse = " + ")
  if(has_group)forumla <- paste0(forumla, " | ",group)
  forumla <- stats::as.formula(paste0("~",forumla))
  if(render.missing){
    table1::table1(forumla,data=DF, big.mark=",")
  }else{
    table1::table1(forumla,data=DF,big.mark=",",render.missing=NULL)
  }
}
index_na <- function(DF, MARGIN = "col",invert = FALSE) {
  okcols <- c("cols","col")
  okrows <-  c("row","rows")
  allowed <- c(okcols,okrows,1,2)
  if(length(MARGIN)!=1)stop("MARGIN must be length 1")
  if(!tolower(MARGIN) %in% allowed)stop("MARGIN must be one of the following ... ",as_comma_string(allowed))
  if(tolower(MARGIN) %in% okcols) MARGIN <- 2
  if(tolower(MARGIN) %in% okrows) MARGIN <- 1
  x <- DF %>% apply(MARGIN = MARGIN,function(IN){
    all(is.na(IN))
  })
  if(invert)x <- !x
  x <- x %>% which() %>% unname()
  return(x)
}
save_table1 <- function(table1,filepath){
  table1 %>%
    table1::t1flex() %>%
    flextable::bg(bg="white",part = "all") %>%
    flextable::save_as_image(
      path = filepath
    )
}
clone_attr <- function(to,from){
  units_vec <- from %>% lapply(function(col){attr(col,"units")}) %>% unlist()
  label_vec <- from %>% lapply(function(col){attr(col,"label")}) %>% unlist()
  to_cols <- colnames(to)
  if(is_something(units_vec)){
    for(i in 1:length(units_vec)){
      x <- units_vec[i]
      col <- names(x)
      if(is_something(x)){
        if(col%in%to_cols){
          attr(to[[col]],"units") <- as.character(x)
        }
      }
    }
  }
  if(is_something(label_vec)){
    for(i in 1:length(label_vec)){
      x <- label_vec[i]
      col <- names(x)
      if(is_something(x)){
        if(col%in%to_cols){
          attr(to[[col]],"label") <- as.character(x)
        }
      }
    }
  }
  return(to)
}
get_labels <- function(DF){
  DF %>% names() %>% sapply(function(name){
    out <- attr(DF[[name]],"label")
    if(!is.null(out))return(out)
    return(name)
  }) %>% as.character()
}
make_DT_table<-function(DF,editable = F,selection="single",paging = TRUE,scrollY = F,searching = T){
  if(!is_something(DF)){
    return(
      DT::datatable(
        data.frame(x = " ")[0,,drop = F],
        options = list(
          dom = 't',        # Simplify the table appearance
          paging = FALSE,   # Disable pagination
          ordering = FALSE # Disable ordering
        ),
        rownames = FALSE,
        colnames = " "
      )
    )
  }
  DF %>% DT::datatable(
    selection = selection,
    editable = editable,
    rownames = F,
    # fillContainer = T,
    # extensions = 'Buttons',
    options = list(
      columnDefs = list(list(className = 'dt-center',targets = "_all")),
      paging = paging,
      pageLength = 20,
      fixedColumns = FALSE,
      ordering = TRUE,
      scrollY = scrollY,
      scrollX = T,
      # autoWidth = T,
      searching = searching,
      # dom = 'Bfrtip',
      # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      scrollCollapse = F,
      stateSave = F
    ),
    class = "cell-border",
    filter = 'top'
    # escape = F
  ) %>% DT::formatStyle(
    colnames(DF),
    color = "#000"
  ) %>% return()
}
make_DT_table_simple<-function(DF){
  if(!is_something(DF)){
    return(h3("No data available to display."))
  }
  DF %>% DT::datatable() %>% DT::formatStyle(
    colnames(DF),
    color = "#000"
  ) %>% return()
}
