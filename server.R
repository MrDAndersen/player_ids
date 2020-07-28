function(input, output, session){
 
  table_updated <- reactiveVal(FALSE)
  
  output$id_table <- DT::renderDataTable({
    
    updates <- table_updated()
    
    data <- db_table
    
    # Filter position if selected
    if(!is.null(input$show_pos)){
      p <- input$show_pos
      if(any(p == "IDP"))
        p <- c(p, c("DE", "DT", "S", "CB", "LB"))
      
      data <- data %>% filter(position %in% p)
    }
    
    # Filter for missing id field
    if(!is.null(input$missing_id)){
      data <- data %>% collect() %>% filter_at(vars(one_of(input$missing_id)), any_vars(id_missing(.)))
    } else {
      data <- data %>% collect()
    }
    
    # Display data
    data %>% 
      mutate(player = paste0("<a href = '#' class = 'dbplayer' id = '", id, "'>", player, ", ", team, " ", position, "</a>")) %>% 
      select(-id, -team, -position) %>% 
      
      DT::datatable(
        class = "compact row-border hover",
        selection = "none",
        escape = FALSE,
        rownames = FALSE,
        colnames = c("Player", names(id_names)),
        options = list(
          pageLength = 25,
          lengthChange = FALSE,
          ordering = FALSE,
          # Callback function will set input$clicked_player to the player's MFL ID
          drawCallback =  DT::JS("function(settings, json) {$('.dbplayer').click(function() {
                                      var playerinfo = $(this).attr('id');
                                      Shiny.onInputChange('clicked_player', playerinfo);});}")
        )
    )
  })
  
  # Modal dialog to edit ids
  id_modal <- function(p){
    modalDialog(
      size = "m", easyClose = FALSE, 
      title = paste0(p$player, ", ", p$team, " ", p$position),
      fluidRow(
        column(4, imap(id_names[1:4], ~ textInput(.x, .y, value = p[[.x]]))),
        column(4, imap(id_names[5:8], ~ textInput(.x, .y, value = p[[.x]]))),
        column(4, imap(id_names[9:11], ~ textInput(.x, .y, value = p[[.x]])))
      ),
      footer = tagList(
        actionButton("cancel", "Cancel"),
        actionButton("update", "Update", class = "primary")
      )
      
    )
  }
  
  # Event handler for clicking a player
  observeEvent(input$clicked_player, {
    
    player_id <- input$clicked_player
    player_info <- db_table %>% filter(id == player_id) %>% collect()
    
    showModal(
      id_modal(player_info)
    )
  }, ignoreNULL = TRUE)
  
  # Event handler for cancelling the update
  observeEvent(input$cancel, {
    runjs("Shiny.onInputChange('clicked_player', null);")
    removeModal()
  })
  
  # Event handler for making an update
  observeEvent(input$update, {
    # Player ID of selected player
    current_player <- input$clicked_player
    
    # Curent IDs
    current_ids <- db_table %>% 
      filter(id == current_player) %>% collect() %>% as.list() %>% `[`(id_names)
    
    # Determining which IDs to update
    update_ids <- current_ids %>% 
      imap( ~ if(id_missing(.x)  || .x != input[[.y]]){input[[.y]]}) %>% 
      discard(is.null)
    
    # Creating and executing update statement
    if(length(update_ids) > 0){
      update_fields <- update_ids %>% 
        imap_chr( ~ {paste0(.y , " = '", .x, "'")}) %>% paste(collapse = ", ")
      
      update_stmt <- paste0("UPDATE id_table SET ", update_fields, " WHERE id ='", current_player, "'")

      dbExecute(db_con, update_stmt)
      
      #Signal that table was updated
      table_updated(!table_updated())
    }
    
    runjs("Shiny.onInputChange('clicked_player', null);")
    removeModal()
    
  })
  
  # Download data
  output$get_ids <- downloadHandler(
    filename = "player_ids.csv",
    content = function(con){
      
      db_table %>% select(id, unname(id_names)) %>% collect() %>%
        bind_rows(inactive_ids) %>% arrange(id) %>% write_csv(con, na = "")
    }
  )
}