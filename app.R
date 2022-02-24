library(shiny)
library(dplyr)
library(jsonlite)
library(glue)
library(stringr)

# Boilerplate and setup

addResourcePath(prefix = "static", directoryPath = "static")

# Functions ----

movement_details <- function(x) {
  movement_name <- unlist(x$content)
  video_slug <- unlist(x$tutorialVideo$slug)
  video_url <- create_video_url(video_slug)
  thumbnail_url <- create_thumbnail_url(unlist(x$tutorialVideo$thumbnail$url))
  
  card(movement_name, thumbnail_url, video_url)
}


get_random_movements <- function(x, n = 6) {
  selected_poses <- sample(x, n)
  selected_poses
}


create_google_search_query <- function(x) {
  # https://www.google.com/search?q=Double+Pigeon+w%2F+Archer
  query_string <- x %>% 
    str_replace_all(regex("\\W+"), " ") %>% 
    str_squish() %>% 
    str_replace_all(" ", "+")
  
  glue("https://www.google.com/search?q={query_string}")
}


create_video_url <- function(x) {
  if (is.null(x)) {
    return("#")
  } else {
    movement_name <- x %>% 
      str_to_lower() %>% 
      str_replace_all(" ", "-")
    
    return(glue("https://romwod.com/app/routine/{movement_name}"))
  }
}

create_thumbnail_url <- function(x) {
  if (is.null(x)) {
    return("static/imgs/placeholder-1920x1080.jpeg")
  } else {
    return(x)
  }
}


card <- function(movement_name, img_url, video_url) {
  google_url <- create_google_search_query(movement_name)
  
  message(str(list(
    "movement_name" = movement_name, 
    "img_url" = img_url, 
    "video_url" = video_url,
    "google_url" = google_url
  )))
  
  if (video_url == "#") {
    video_div <- 'No Video found'
  } else {
    video_div <- glue('<a href="{video_url}" class="card-link" target="_blank">Video</a>')
  }
  
  HTML(
    glue(
      '
      <div class="col">
        <div class="card">
          <img src="{img_url}" class="card-img-top" alt="{movement_name}">
          <div class="card-body">
            <p class="card-title"><b>{movement_name}</b></p>
            <div>👀 <a href="{google_url}" class="card-link" target="_blank">Google search movement</a></div>
            <div class="card-text">🍿 {video_div}</div>
          </div>
        </div>
      </div>
      '
    )
  )
}

card_group <- function(cards) {
  HTML(paste0(
    '<div class="row row-cols-1 row-cols-md-2 g-4">',
    paste(cards, collapse = ""),
    '</div>'
  ))
}


# UI ----

ui <- navbarPage(
  "Random ROM",
  theme = bslib::bs_theme(bootswatch = "flatly", version = 5),
  tabPanel(
    "Home",
    sidebarLayout(
      sidebarPanel(
        numericInput("n_movements", "Number of movements:", 3, min = 1, max = 20, step = 1),
        actionButton("generate_movements", "Lets Move!", class = "btn-block btn-primary")
      ),
      mainPanel(
        uiOutput("random_movements"),
      )
    )
  ),
  tabPanel(
    "About",
    markdown(glue(
      "
      Random ROM randomly selects stretches from Kelly Starrett's [ROM WOD](https://romwod.com/app/poses) 
      website. Please check out and support [ROM WOD](https://romwod.com). It is
      a fun and awesome way to improvement your fitness 💪.
      
      Random ROM is open source and built using [R](https://www.r-project.org/) 
      and [shiny](https://shiny.rstudio.com/):
      
      - Source code: [https://github.com/SamEdwardes/random-rom](https://github.com/SamEdwardes/random-rom)
      "
    ))
  )
)

# Server ----

server <- function(input, output, session) {
  
  poses <- read_json("data/poses.json")
  
  random_movements <- eventReactive(input$generate_movements, ignoreNULL = FALSE, {
    get_random_movements(poses, input$n_movements)
  })
  
  output$random_movements <- renderUI({
    movements <- random_movements()
    cards <- purrr::map(movements, ~ movement_details(.x))
    card_group(cards)
  })
  
}

shinyApp(ui, server)