fluidPage(
  tags$head(tags$title('Manage Player IDs'), tags$style("a.dbplayer {color: steelblue;} a.dbplayer:hover {color: darkblue}")),
  shinyjs::useShinyjs(),
  # Header
  fluidRow(column(12, span("Manage Player IDs"), style = "background-color: darkblue; color: white; font-size:130%; font-weight: bold; text-align: center")),
  
  #Body
  fluidRow(
    column(1, checkboxGroupInput("show_pos", "Show Positions", c("QB", "RB", "WR", "TE", "K", "DST", "IDP")),
           p(),
           downloadButton("get_ids", "Download IDs", class = "btn-primary")
           ),
    column(11, 
           checkboxGroupInput("missing_id", "Show Missing", choices = id_names, inline = TRUE ),
           DT::dataTableOutput("id_table")
    )
  )
)