#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(wordcloud2)
library(wordcloud)
library(dplyr)
library("stringr") 
library(quanteda)
library(jiebaR)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  wc_data <- reactive({
    
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        test <- input$wctext2

        #去掉數字、英文, 移除 NA 
        content <- str_remove_all(test, "[0-9a-zA-Z.]+?")
        content2 <- content[is.na(content)==F]
        #去掉冗詞贅字
        new_words <- c(unlist(strsplit(input$key_word,",")),c("壓力管理","時間管理"))
        writeLines(new_words, "new_words.txt")
        # 設定停止詞
        stop_words <- c(unlist(strsplit(input$stop_word,",")),c("在","的","下","個","來","至","座","亦","與","或","日","月","年","週"))
        writeLines(stop_words, "stop_words.txt")
        # 重新定義斷詞器，匯入停止詞
        cutter <- worker(user = "new_words.txt", stop_word = "stop_words.txt", bylines = FALSE)
        seg_words <- cutter[content2]
        freq(seg_words)
        
      })
    })
    
    
  })
  wordcloud_rep <-repeatable(wordcloud)
  
  output$wordcloud2 <- renderWordcloud2({
    withProgress({
      setProgress(message="Creating Wordcloud...")
      wc_corpus<-wc_data()
      wc_corpus.o<-wc_corpus[order(-wc_corpus$freq),]
      wordcloud2(filter(wc_corpus.o, freq >= input$n.cut),#minSize = 2, 
                 fontFamily = "Microsoft YaHei", size = 1, shape = "circle" ,
                 color='random-light', backgroundColor="black")
    })
  })
  output$word_hist <- renderPlot({
    wc_corpus<-wc_data()
    wc_corpus.o<-wc_corpus[order(-wc_corpus$freq),]
    barplot(wc_corpus.o$freq,names.arg=wc_corpus.o$char,main="",border=F,xlim=c(1,20))
  })
}
)