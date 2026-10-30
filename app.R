# =========================================================================
# MOVIE RECOMMENDATION SYSTEM - SHINY WEB APP  (save this file as app.R)
# =========================================================================

packages <- c("shiny", "ggplot2")
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(shiny)
library(ggplot2)

# ---- LOAD AND CLEAN DATA -------------------------------------------------
movies <- read.csv("C:/Users/priya/Downloads/archive (2)/imdb_top_1000.csv",
                   stringsAsFactors = FALSE)

movies <- movies[, c("Series_Title", "Released_Year", "Genre", "IMDB_Rating",
                     "Director", "Star1", "Star2", "Star3", "Star4")]
movies$Released_Year <- suppressWarnings(as.numeric(movies$Released_Year))
movies <- movies[!is.na(movies$Released_Year) & !is.na(movies$IMDB_Rating), ]
movies$Decade <- floor(movies$Released_Year / 10) * 10

all_genres <- sort(unique(unlist(strsplit(movies$Genre, ",\\s*"))))

col_main <- "#2E86AB"; col_accent <- "#F24236"; col_dark <- "#33415C"

pretty_theme <- theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 17, colour = col_dark),
        plot.subtitle = element_text(colour = "grey40"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

# ---- RECOMMENDER FUNCTION ------------------------------------------------
# +1 for every shared genre, +2 if same director, +1 for every shared actor
recommend <- function(movie_title, n = 5) {
  idx <- which(movies$Series_Title == movie_title)
  chosen        <- movies[idx[1], ]
  chosen_genres <- strsplit(chosen$Genre, ",\\s*")[[1]]
  chosen_stars  <- c(chosen$Star1, chosen$Star2, chosen$Star3, chosen$Star4)
  
  scores <- numeric(nrow(movies))
  for (i in 1:nrow(movies)) {
    other_genres <- strsplit(movies$Genre[i], ",\\s*")[[1]]
    other_stars  <- c(movies$Star1[i], movies$Star2[i],
                      movies$Star3[i], movies$Star4[i])
    scores[i] <- length(intersect(chosen_genres, other_genres)) +
      length(intersect(chosen_stars, other_stars))
    if (movies$Director[i] == chosen$Director) scores[i] <- scores[i] + 2
  }
  
  result       <- movies
  result$Score <- scores
  result       <- result[result$Series_Title != chosen$Series_Title, ]
  result       <- result[order(-result$Score, -result$IMDB_Rating), ]
  head(result[, c("Series_Title", "Released_Year", "Genre",
                  "Director", "IMDB_Rating", "Score")], n)
}

# ---- USER INTERFACE (what the user sees) ---------------------------------
ui <- fluidPage(
  titlePanel("CineMatch"),
  sidebarLayout(
    sidebarPanel(
      h4("Get recommendations"),
      selectInput("movie", "Choose a movie you like:",
                  choices = sort(unique(movies$Series_Title)),
                  selected = "The Dark Knight"),
      sliderInput("n", "Number of recommendations:",
                  min = 3, max = 15, value = 6),
      hr(),
      h4("Browse by genre"),
      selectInput("genre", "Genre:", choices = c("All", all_genres)),
      sliderInput("minrating", "Minimum rating:",
                  min = 7.6, max = 9.3, value = 8.0, step = 0.1),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Recommendations", br(),
                 plotOutput("recPlot", height = "380px"),
                 br(), tableOutput("recTable")),
        tabPanel("Browse by genre", br(),
                 tableOutput("genreTable")),
        tabPanel("Insights", br(),
                 plotOutput("genrePlot", height = "350px"),
                 plotOutput("decadePlot", height = "350px"),
                 plotOutput("ratingPlot", height = "350px"))
      )
    )
  )
)

# ---- SERVER (the logic behind the screen) --------------------------------
server <- function(input, output) {
  
  rec <- reactive({ recommend(input$movie, input$n) })
  
  output$recPlot <- renderPlot({
    d <- rec()
    ggplot(d, aes(x = reorder(Series_Title, Score + IMDB_Rating / 100),
                  y = IMDB_Rating, fill = Score)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = IMDB_Rating), hjust = -0.2,
                colour = col_dark, fontface = "bold") +
      scale_fill_gradient(low = "#9ECAE1", high = "#08519C",
                          name = "Similarity\nscore") +
      coord_flip(ylim = c(0, 10)) +
      labs(title = paste("Because you liked", input$movie),
           subtitle = "Darker bar = more similar (genre, director, actors)",
           x = NULL, y = "IMDB Rating") +
      pretty_theme
  })
  
  output$recTable <- renderTable({
    d <- rec()
    data.frame(Title = d$Series_Title, Year = as.integer(d$Released_Year),
               Genre = d$Genre, Director = d$Director,
               Rating = d$IMDB_Rating, Match = d$Score)
  }, striped = TRUE, hover = TRUE, digits = 1)
  
  output$genreTable <- renderTable({
    d <- movies
    if (input$genre != "All") d <- d[grepl(input$genre, d$Genre), ]
    d <- d[d$IMDB_Rating >= input$minrating, ]
    d <- head(d[order(-d$IMDB_Rating), ], 20)
    data.frame(Title = d$Series_Title, Year = as.integer(d$Released_Year),
               Genre = d$Genre, Director = d$Director, Rating = d$IMDB_Rating)
  }, striped = TRUE, hover = TRUE, digits = 1)
  
  output$genrePlot <- renderPlot({
    gc <- sort(table(unlist(strsplit(movies$Genre, ",\\s*"))), decreasing = TRUE)
    df <- head(data.frame(Genre = names(gc), Count = as.numeric(gc)), 12)
    ggplot(df, aes(x = reorder(Genre, Count), y = Count, fill = Count)) +
      geom_col(show.legend = FALSE, width = 0.75) +
      geom_text(aes(label = Count), hjust = -0.2, colour = col_dark) +
      scale_fill_gradient(low = "#9ECAE1", high = "#08519C") +
      coord_flip(ylim = c(0, max(df$Count) * 1.12)) +
      labs(title = "Most Common Genres", x = NULL, y = "Number of movies") +
      pretty_theme
  })
  
  output$decadePlot <- renderPlot({
    d <- aggregate(IMDB_Rating ~ Decade, data = movies, FUN = mean)
    ggplot(d, aes(x = Decade, y = IMDB_Rating)) +
      geom_line(colour = col_main, linewidth = 1.3) +
      geom_point(size = 4, colour = col_accent) +
      geom_text(aes(label = round(IMDB_Rating, 2)), vjust = -1.2,
                colour = col_dark) +
      scale_x_continuous(breaks = d$Decade) +
      labs(title = "Average IMDB Rating by Decade",
           x = "Decade", y = "Average rating") +
      pretty_theme
  })
  
  output$ratingPlot <- renderPlot({
    ggplot(movies, aes(x = IMDB_Rating)) +
      geom_histogram(binwidth = 0.1, fill = col_main, colour = "white") +
      geom_vline(xintercept = mean(movies$IMDB_Rating),
                 colour = col_accent, linetype = "dashed", linewidth = 1) +
      labs(title = "Distribution of IMDB Ratings (dashed line = mean)",
           x = "IMDB Rating", y = "Number of movies") +
      pretty_theme
  })
}

# ---- RUN THE APP ---------------------------------------------------------
shinyApp(ui = ui, server = server)