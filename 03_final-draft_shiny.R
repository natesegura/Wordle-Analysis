# Load Packages

library(tidyverse)
library(shiny)
library(ggrepel)

# -------- SETUP --------

# Load data
answers1 <- read_csv("data/wordle_answers_clean.csv")
wordrankings <- read_csv("data/english_words_ranked.csv")

# Define ALL Functions Necessary

check_green <- function(df, word) {
  
  df |> 
    mutate(g1 = if_else(str_sub(answer, 1, 1) == str_sub(word, 1, 1), 1, 0),
           g2 = if_else(str_sub(answer, 2, 2) == str_sub(word, 2, 2), 1, 0),
           g3 = if_else(str_sub(answer, 3, 3) == str_sub(word, 3, 3), 1, 0),
           g4 = if_else(str_sub(answer, 4, 4) == str_sub(word, 4, 4), 1, 0),
           g5 = if_else(str_sub(answer, 5, 5) == str_sub(word, 5, 5), 1, 0)) |> 
    mutate(greens = rowSums(across(7:11)))
  
}

yellow_formula <- function(answer, word) {
  
  p_letters <- paste0(unlist(str_split(word, "")), "{1}")
  
  remaining <- 
    str_remove(
      str_remove(
        str_remove(
          str_remove(
            str_remove(
              answer, p_letters[1]), p_letters[2]), p_letters[3]), p_letters[4]), p_letters[5])
}

check_yellowgreen <- function(df, word) {
  
  df2 <- check_green(df, word)
  
  df3 <- df2 |> 
    select(-g1, -g2, -g3, -g4, -g5) |> 
    mutate(yellows = (5 - str_length(yellow_formula(df$answer, word))) - greens)
  
  return(df3)
}

plot_yellowgreen <- function(df, word) {
  
  stopifnot(is.data.frame(df))
  stopifnot(str_length(word) == 5)
  
  df2 <- check_yellowgreen(df, word)
  
  dfg <- df2 |> 
    count(greens) |> 
    complete(greens = 0:5, fill = list(n = 0)) |> 
    rename(count = greens) |> 
    mutate(color = "greens") |> 
    filter(count != 0)
  
  dfy <- df2 |> 
    count(yellows) |> 
    complete(yellows = 0:5, fill = list(n = 0)) |> 
    rename(count = yellows) |> 
    mutate(color = "yellows") |> 
    filter(count != 0)
  
  df3 <- full_join(dfg, dfy)
  
  df3 |> 
    ggplot(aes(count, n, fill = color)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("greens" = "#00BA42", "yellows" = "#FFDD33")) +
    geom_text_repel(aes(label = n), 
                    vjust = -1, 
                    ylim = c(0, NA),
                    position = position_dodge(width = 0.9)) +
    #scale_color_manual(values = c("greens" = "#00BA42", "yellows" = "#FFDD33")) +
    scale_x_continuous(breaks = c(1,2,3,4,5)) +
    ylim(0, max(df3$n) + 100) +
    labs(title = paste0("Distribution of Yellow and Green Letters in ", word),
         subtitle = "Wordle Answers from September 2021 to April 2026",
         caption = c(paste0("Instances of No Yellows: ", sum(df2$yellows == 0)),
                     paste0("Instances of No Greens: ", sum(df2$greens == 0))),
         x = "Number of Green or Yellows",
         y = "") +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = c(1, 0)))
  
}

# ---------- UI ----------

ui <- fluidPage(
  titlePanel("Wordle Analysis - Interactive Shiny App"),
  h4("Final Project for DSCI-250, Spring 2026"),
  h5("By: Nate Segura"),
  
  sidebarLayout(
    sidebarPanel(
      
      textInput(inputId = "word", 
                label = "Enter a Five-Letter Word:",
                value = "",
                placeholder = "input"),
      
      wellPanel(p("Thank you for using this interactive visualization!"),
                p("Inputting a five-letter word in the above text box returns one plot and one table. The table contains the word's rankings against a list of 3,103 guess words. The column plot details the overall results of the guess."),
                br(),
                p(strong("Vocabulary/Descriptions:")),
                p(strong("The Wordle Puzzle: "),
                  "Wordle is a daily puzzle game hosted by the New York Times. Each day, a five-letter word is chosen and your goal is to find what this word is. To do this, Wordle asks you to input any five-letter word. Afterwards, some letters may be highlighted either green or yellow to provide further hints towards the day's answer word."),
                p(strong("'Greens' "),
                  "refers to the amount of green letters returned when inputting the provided word as your first guess. Wordle highlights a letter as green when it is present in the answer word AND is in the same position."),
                p(strong("'Yellows' "),
                  "Similar to greens, this refers to the amount of yellow letters returned. Wordle highlights a letter as yellow when it is present in the answer word but is in the wrong position within the word."),
                p(strong("Plot: "),
                  "This plot depicts the total amount of green or yellow letters returned by a word over 1,755 Wordle puzzles.")
      )
    ),
    
    mainPanel(
      tableOutput("rankTable"),
      plotOutput("barPlot")
    )
  )
)

# -------- SERVER --------

server <- function(input, output, session) {
  
  output$rankTable <- renderTable({
    
    word <- str_to_upper(input$word)
    
    wordrankings |> 
      filter(guess == word) |> 
      summarize(Guess = guess,
                GreenRank = greenrank,
                YellowRank = yellowrank,
                TotalGreens = totalgreen,
                TotalYellows = totalyellow)
    
  })
  
  output$barPlot <- renderPlot({
    
    word <- str_to_upper(input$word)
    
    if(str_length(word) != 5) {
      word <- "     "
    }
    
    plot_yellowgreen(answers1, word)
    
  })
}

# -------- RUN APP --------

shinyApp(ui = ui, server = server)

