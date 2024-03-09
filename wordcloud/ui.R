
library(shiny)
library(wordcloud2)
library(wordcloud)
library(dplyr)
library("stringr") 
library(quanteda)
library(jiebaR)

# Define UI for application that draws a histogram

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Word Cloud"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textAreaInput("wctext2","輸入文字",rows=20),
      textAreaInput("key_word","專有名詞, 用逗號(,)做分隔, 例如:九族文化村,櫻花祭潭",rows=1),
      textAreaInput("stop_word","停止詞(Stop Words), 用逗號(,)做分隔, 例如:的,了,也",rows=1),
      sliderInput("n.cut","最小字頻", value=1, min=0, max=10),
      actionButton("update","Create Word Cloud")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
#      plotOutput("wcplot"),
      wordcloud2Output("wordcloud2"),
      plotOutput("word_hist")
    )
  )
)
)
