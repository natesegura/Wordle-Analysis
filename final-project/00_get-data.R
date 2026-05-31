# Loading Packages
  
library(tidyverse)
library(rvest)
library(chromote)


# Aquire Wordle Dataset

# Website: <https://wordfinder.yourdictionary.com/wordle/answers/>
  
wordle_html <- read_html_live("https://wordfinder.yourdictionary.com/wordle/answers/")

wordle_table <- wordle_html |> 
  html_elements("table") |> 
  html_table()

wordle_answers <- bind_rows(wordle_table)

write_csv(wordle_answers, "data/wordle_answers_raw.csv")



# Aquire Five-Letter Word Data

# Website: <https://gist.github.com/daemondevin/df09befaf533c380743bc2c378863f0c>

five_letter_words <- read_csv("https://gist.githubusercontent.com/daemondevin/df09befaf533c380743bc2c378863f0c/raw/b79b0628e79766326cdd61cc52a7222b8e5ad49a/5-letter-words.txt",
                              col_names = FALSE)

write_csv(five_letter_words, "data/english_words.csv")