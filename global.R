library(ffanalytics)
library(shiny)
library(shinyjs)
library(RSQLite)
library(dbplyr)
library(DT)

# Helper function
id_missing <- function(x)(is.na(x) | x == "")

# Create table
id_table <- player_table %>%
  select(id, first_name, last_name, team, position) %>%
  arrange(last_name) %>%
  unite("player", c(first_name, last_name), sep = " ") %>%
  left_join(ffanalytics:::player_ids, by = "id")

# Initialize SQL Lite in-memory database to handle updates
db_con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")

# Copy tale into database
copy_to(db_con, id_table)

# Establish connection to DB table
db_table <- tbl(db_con, "id_table")

id_names <- names(ffanalytics:::player_ids)[-1] 
names(id_names) <- c("Yahoo", "CBS", "Fleaflicker", "NFL", "ESPN", "FFToday", "Numberfire", "FantasyPros", "FantasyData", "FantasyNerd", "RTS")

# IDs for players no longer active in MFL. Will need those for historical purposes.
inactive_ids <- ffanalytics:::player_ids %>% filter(!(id %in% id_table$id))