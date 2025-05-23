#' @title Generate REDCap Project Diagram
#' @description
#' Generates a diagram of the REDCap project structure based on the `project` object.
#'
#' @details
#' This function generates a visual diagram of the REDCap project structure, including forms, fields, and choices. It supports various options such as rendering the diagram, including fields and choices, and specifying the direction of the diagram.
#'
#' @param project project object from REDCapSync package
#' @param static Logical (TRUE/FALSE). If TRUE, generates a static diagram with `DiagrammeR`. If FALSE, generates an interactive diagram with `visnetwork`. Default is `FALSE`.
#' @param render Logical (TRUE/FALSE). If TRUE, renders the diagram. Default is `TRUE`.
#' @param zoomView Logical (TRUE/FALSE). If TRUE, user can zoom. Default is `TRUE`.
#' @param duplicate_forms Logical (TRUE/FALSE). If TRUE, includes duplicate form nodes in the diagram. Default is `TRUE`.
#' @param clean_names Logical (TRUE/FALSE). If TRUE, cleans the names of the forms and fields in the diagram. Default is `TRUE`.
#' @param include_fields Logical (TRUE/FALSE). If TRUE, includes fields in the diagram. Default is `FALSE`.
#' @param include_choices Logical (TRUE/FALSE). If TRUE, includes choices in the diagram. Default is `FALSE`.
#' @param hierarchical Logical (TRUE/FALSE). If TRUE, generates a hierarchical diagram. Default is `FALSE`.
#' @param direction Character string specifying the direction of the diagram. Options are "LR" (left to right), "TB" (top to bottom), "RL" (right to left), and "BT" (bottom to top). Default is "LR".
#' @return A diagram object representing the REDCap project structure.
#' @seealso
#' \link{setup_project} for initializing the `project` object.
#' @family Visuals
#' @export
REDCap_diagram <- function(project,
                           static = FALSE,
                           render = TRUE,
                           duplicate_forms = TRUE,
                           clean_names = TRUE,
                           include_fields = FALSE,
                           include_choices = FALSE,
                           hierarchical = FALSE,
                           direction = "LR",
                           zoomView = TRUE) {
  if(is.null(project$redcap))return(NULL)
  OUT <- create_node_edge_REDCap(project,duplicate_forms = duplicate_forms,include_fields = include_fields,include_choices = include_choices)
  if (!clean_names) {
    OUT$node_df$label <- OUT$node_df$entity_name
  }
  OUT$node_df$physics <- TRUE
  OUT$node_df$physics[which(OUT$node_df$group =="project")] <- FALSE
  if(static){
    OUT$node_df$shape[which(OUT$node_df$shape=="box")] <- "rectangle"
    OUT$node_df$shape[which(OUT$node_df$shape=="ellipse")] <- "circle"
    colnames(OUT$node_df)[which(colnames(OUT$node_df)=="title")] <- "tooltip"
    colnames(OUT$node_df)[which(colnames(OUT$node_df)=="group")] <- "type"
    colnames(OUT$node_df)[which(colnames(OUT$node_df)=="color.border")] <- "color"
    colnames(OUT$node_df)[which(colnames(OUT$node_df)=="font.color")] <- "fontcolor"
    OUT$node_df$fillcolor <- OUT$node_df$color.background
    # node_df$color.highlight <- "gold"
    OUT$node_df$tooltip <-gsub("<br>","\\\n",OUT$node_df$tooltip) %>% remove_html_tags()
    if(is_something(OUT$edge_df))colnames(OUT$edge_df)[which(colnames(OUT$edge_df)=="width")] <- "penwidth"
    graph <- DiagrammeR::create_graph(
      nodes_df =  OUT$node_df,
      edges_df = OUT$edge_df
    )
    rendered_graph <- DiagrammeR::render_graph(
      graph,
      title = project$redcap$project_info$project_title,
      output = "graph"
    )
  }else{
    project_color <- "lightblue"
    arm_color <- "green"
    event_color <- "orange"
    form_color <- "#FF474C"
    field_color <- "yellow"
    attribute_color <- "green"
    choice_color <- "lightblue"
    bordercolor <- "black"
    OUT$node_df$type <- OUT$node_df$group
    rendered_graph <- visNetwork::visNetwork(
      nodes =  OUT$node_df,
      edges = OUT$edge_df,
      main = project$redcap$project_info$project_title,
      submain = project$redcap$project_info$project_notes %>%
        paste0("<br>Code by Brandon Rose, M.D., M.P.H. at <a href='https://www.thecodingdocs.com/home'>TheCodingDocs.com</a> using <a href='https://github.com/thecodingdocs/RosyREDCap'>RosyREDCap with REDCapSync</a> and <a href='https://github.com/datastorm-open/visNetwork'>VisNetwork</a>")
    ) %>%
      visNetwork::visInteraction(zoomView = zoomView) %>%
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visNetwork::visGroups(
        groupname = "project",
        color = list(
          background = project_color,
          border = bordercolor,
          highlight = project_color
        ),
        font = list(color = bordercolor)
      ) %>%
      visNetwork::visGroups(
        groupname = "arm",
        color = list(
          background = arm_color,
          border = bordercolor,
          highlight = arm_color
        ),
        font = list(color = bordercolor)
      ) %>%
      visNetwork::visGroups(
        groupname = "event",
        color = list(
          background = event_color,
          border = bordercolor,
          highlight = event_color
        ),
        font = list(color = bordercolor)
      ) %>%
      visNetwork::visGroups(
        groupname = "form",
        color = list(
          background = form_color,
          border = bordercolor,
          highlight = form_color
        ),
        font = list(color = bordercolor)
      ) %>%
      visNetwork::visGroups(
        groupname = "field",
        color = list(
          background = field_color,
          border = bordercolor,
          highlight = field_color
        ),
        font = list(color = bordercolor)
      ) %>%
      visNetwork::visGroups(
        groupname = "choice",
        color = list(
          background = choice_color,
          border = bordercolor,
          highlight = choice_color
        ),
        font = list(color = bordercolor)
      ) %>%
      visNetwork::visLegend(main = "Legend") %>%
      visNetwork::visLayout(hierarchical = hierarchical)
    if(hierarchical){
      rendered_graph <- rendered_graph %>% visNetwork::visHierarchicalLayout(direction = direction, levelSeparation = 300)
    }
    # if(include_fields){
    #   groups <- "field"
    #   if(include_choices) groups <- groups %>% append("choice")
    #   rendered_graph <- rendered_graph %>% visNetwork::visClusteringByGroup(groups = groups)
    # }
    # rendered_graph$x$options$groups <- rendered_graph$x$groups %>% lapply(function(group){
    #   list(
    #     shape=OUT$node_df$shape[which(OUT$node_df$group==group)[[1]]],
    #     font = list(
    #       color = OUT$node_df$font.color[which(OUT$node_df$group==group)[[1]]]
    #     ),
    #     color = list(
    #       background = OUT$node_df$color.background[which(OUT$node_df$group==group)[[1]]],
    #       border = OUT$node_df$color.border[which(OUT$node_df$group==group)[[1]]]
    #     )
    #   )
    # }) %>% unlist()
  }
  if (render) {
    return(rendered_graph)
  }
  graph
}
#' @noRd
create_node_edge_REDCap <- function(
    project,
    duplicate_forms = TRUE,
    include_fields = FALSE,
    include_choices = FALSE
){
  # setup ==========================
  node_df <- NULL
  edge_df <- NULL
  bordercolor <- font.color <- "black"
  project_color <- "lightblue"
  arm_color <- "green"
  event_color <- "orange"
  form_color <- "#FF474C"
  field_color <- "yellow"
  attribute_color <- "green"
  choice_color <- "lightblue"
  arrow_type <- "to"
  arms <- project$metadata$arms
  events <- project$metadata$events
  event_mapping <- project$metadata$event_mapping
  forms <- project$metadata$forms[order(project$metadata$forms$repeating),]
  fields <- project$metadata$fields
  choices <- project$metadata$choices
  # nodes ======================================================================
  # project ---------------------------------------------------------
  level <- 1
  node_df <- node_df %>% dplyr::bind_rows(
    data.frame(
      id = NA,
      group = "project",
      entity_name = project$short_name,
      entity_label = project$redcap$project_info$project_title,
      # label = forms$form_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
      level = level,
      shape = "box", # entity
      style = "filled",
      color.background = project_color,
      color.border = bordercolor,
      font.color = font.color
    )
  )
  # arms & events -------------------------
  if(project$redcap$is_longitudinal){
    level <- level + 1
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        group = "arm",
        entity_name = arms$arm_number %>% as.character(),
        entity_label = arms$arm_number %>% as.character(),
        # label = forms$form_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        level = level,
        shape = "box", # entity
        style = "filled",
        color.background = arm_color,
        color.border = bordercolor,
        font.color = font.color
      )
    )
    level <- level + 1
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        group = "event",
        entity_name = events$unique_event_name,
        entity_label = events$event_name,
        # label = forms$form_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        level = level,
        shape = "box", # entity
        style = "filled",
        color.background = event_color,
        color.border = bordercolor,
        font.color = font.color
      )
    )
  }
  # forms -----------
  # if(project$redcap$has_repeating_forms){
  #   level <- level + 1
  #   node_df <- node_df %>% dplyr::bind_rows(
  #     data.frame(
  #       id = NA,
  #       group = "structure",
  #       entity_name = c("Repeating","Not Repeating"),
  #       entity_label = c("Repeating","Not Repeating"),
  #       level = level,
  #       # label = forms$form_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
  #       shape = "circle", # attribute
  #       style = "filled",
  #       color.background = attribute_color,
  #       color.border = bordercolor,
  #       font.color = font.color
  #     )
  #   )
  # }
  if(duplicate_forms && project$redcap$is_longitudinal){
    level <- level + 1
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        group = "form",
        entity_name = event_mapping$form,
        entity_label = forms$form_label[match(event_mapping$form,forms$form_name)],# turn to function
        level = level,
        # label = forms$form_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        title = event_mapping$form %>% lapply(function(x){
          paste0("<p><b>",x,"</b><br>",paste0(REDCapSync:::form_names_to_field_names(x,project),collapse = "<br>"),"</p>")
        }) %>% unlist(),
        shape = "box", # entity
        style = "filled",
        color.background = form_color,
        color.border = bordercolor,
        font.color = font.color
      )
    )
  }else{
    level <- level + 1
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        group = "form",
        entity_name = forms$form_name,
        entity_label = forms$form_label,
        level = level,
        # label = forms$form_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        title = forms$form_name %>% lapply(function(x){
          paste0("<p><b>",x,"</b><br>",paste0(REDCapSync:::form_names_to_field_names(x,project),collapse = "<br>"),"</p>")
        }) %>% unlist(),
        shape = "box", # entity
        style = "filled",
        color.background = form_color,
        color.border = bordercolor,
        font.color = font.color
      )
    )
  }
  # fields --------------
  if(include_fields){
    level <- level + 1
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        group = "field",
        entity_name = fields$field_name,
        entity_label = fields$field_label,
        level = level,
        # label = project$fields$fields$field_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        title = paste0("<p><b>",fields$field_name,"</b><br>",paste0("<b>Field Label:</b> ",fields$field_label),"<br>",paste0("<b>Field Type:</b> ",fields$field_type),"</p>"),
        shape = "ellipse",
        style = "filled",
        color.background = field_color,
        color.border = bordercolor,
        font.color = font.color
      )
    )
    if(include_choices){
      level <- level + 1
      node_df <- node_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          group = "choice",
          entity_name = choices$name,
          entity_label = choices$name,
          level = level,
          # label = project$fields$fields$field_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
          title = NA,
          shape = "ellipse",
          style = "filled",
          color.background = choice_color,
          color.border = bordercolor,
          font.color = font.color
        )
      )
    }
  }
  # final nodes-------------------
  node_df$id <- seq_len(nrow(node_df))
  node_df$fixedsize <- FALSE
  # node_df$color.highlight <- "gold"
  node_df$label<-node_df$entity_label %>% lapply(function(text){
    wrap_text(text,25)
  }) %>% unlist()
  rownames(node_df) <- NULL
  # edges ======================
  # edges not longitudinal ---------------
  if( ! project$redcap$is_longitudinal){
    # project to forms-------------------
    edge_df <- edge_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        from = node_df$id[which(node_df$group=="project")],
        to = node_df$id[which(node_df$group=="form")],
        rel = NA,#"Belongs to",
        style = "filled",
        color = font.color,
        arrowhead = "none",
        arrows = arrow_type
      )
    )
    # if(project$redcap$has_repeating_forms){
    #   sub_node_df_structure <- node_df[which(node_df$group=="structure"),]
    #   edge_df <- edge_df %>% dplyr::bind_rows(
    #     data.frame(
    #       id = NA,
    #       from = sub_node_df_forms$id,
    #       to = sub_node_df_structure$id[match(ifelse(forms$repeating[match(sub_node_df_forms$entity_name,forms$form_name)],"Repeating","Not Repeating"),sub_node_df_structure$entity_name)],
    #       rel = NA,#"Belongs to",
    #       style = "filled",
    #       color = font.color,
    #       arrowhead = "none",
    #       arrows = arrow_type
    #     )
    #   )
    # }
  }
  # edges is longitudinal ---------------
  if(project$redcap$is_longitudinal){
    # project to arms-------------------
    edge_df <- edge_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        from = node_df$id[which(node_df$group=="project")],
        to = node_df$id[which(node_df$group=="arm")],
        rel = NA,#"Belongs to",
        style = "filled",
        color = font.color,
        arrowhead = "none",
        arrows = arrow_type
      )
    )
    # arms to events --------------------
    edge_df <- edge_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        from = events$arm_number %>% lapply(function(x){node_df$id[which(node_df$group=="arm"&node_df$entity_name==x)]}) %>% unlist(),
        to = events$unique_event_name %>% lapply(function(x){node_df$id[which(node_df$group=="event"&node_df$entity_name==x)]}) %>% unlist(),
        rel = NA,#"Belongs to",
        style = "filled",
        color = font.color,
        arrowhead = "none",
        arrows = arrow_type
      )
    )
    # events to forms ----------------------
    if(duplicate_forms){
      sub_node_df <- node_df[which(node_df$group=="form"),]
      if(any(!sub_node_df$entity_name %in% event_mapping$form))stop("event match error! check the diagram function. For now do not duplicate forms.")
      edge_df <- edge_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          from = event_mapping$unique_event_name %>% lapply(function(x){node_df$id[which(node_df$group=="event"&node_df$entity_name==x)]}) %>% unlist(),
          to = sub_node_df$id,
          rel = NA,#"Belongs to",
          style = "filled",
          color = font.color,
          arrowhead = "none",
          arrows = arrow_type
        )
      )
    }else{
      edge_df <- edge_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          from = event_mapping$unique_event_name %>% lapply(function(x){node_df$id[which(node_df$group=="event"&node_df$entity_name==x)]}) %>% unlist(),
          to = event_mapping$form %>% lapply(function(x){node_df$id[which(node_df$group=="form"&node_df$entity_name==x)]}) %>% unlist(),
          rel = NA,#"Belongs to",
          style = "filled",
          color = font.color,
          arrowhead = "none",
          arrows = arrow_type
        )
      )
    }
  }
  # forms to fields --------------
  sub_node_df_forms <- node_df[which(node_df$group=="form"),]
  if(include_fields){
    sub_node_df_fields <- node_df[which(node_df$group=="field"),]
    edge_df <- edge_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        from = sub_node_df_forms$id[match(fields$form_name[match(sub_node_df_fields$entity_name,fields$field_name)],sub_node_df_forms$entity_name)],
        to = sub_node_df_fields$id,
        rel = NA,#"Belongs to",
        style = "filled",
        color = font.color,
        arrowhead = "none",
        arrows = arrow_type
      )
    )
    if(include_choices){
      sub_node_df_choices <- node_df[which(node_df$group=="choice"),]
      edge_df <- edge_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          from = sub_node_df_fields$id[match(choices$field_name,sub_node_df_fields$entity_name)],
          to = sub_node_df_choices$id,
          rel = NA,#"Belongs to",
          style = "filled",
          color = font.color,
          arrowhead = "none",
          arrows = arrow_type
        )
      )
    }
  }
  # final edges -------------------
  edge_df$id <- seq_len(nrow(edge_df))
  OUT <- list(
    node_df = node_df,
    edge_df = edge_df
  )
  return(OUT)
}
