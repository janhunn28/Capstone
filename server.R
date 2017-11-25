#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#Preparations to get what is needed.  Libraries and data dictionaries.

library(shiny); 

readRDS(biC, file = "C:/Users/janhu/Documents/Data Science/Capstone/biC.rds")
readRDS(triC, file = "C:/Users/janhu/Documents/Data Science/Capstone/triC.rds")
readRDS(quadC, file = "C:/Users/janhu/Documents/Data Science/Capstone/quadC.rds")

##n-Gram Functions needed based on the input

#If one word is entered, call the bigram prediction dictionary first
bigram_predict = function(x){
  ret_word = biC[biC[1]==x,]$word2
  return(ret_word)
}

#If two words are entered, call the trigram prediction dictionary first.
trigram_predict = function(x,y) {
  ret_word <- triC[triC[1]==x & triC[2]==y,]$word3
  return(ret_word)
}

#If three words are entered, call the quadgram prediction dictionary first.
quadgram_predict = function(x,y,z) {
  ret_word <- quadC[quadC[1]==x & quadC[2]==y & quadC[3]==z,]$word4
  return(ret_word)
  
}

#If 4 words are entered, call the quintgram prediction dictionary first.
quintgram_predict = function(x,y,z,a) {
  #  ret_word <- quintC[quintC$word1==x & quintC$word2==y & quintC$word3==z & quintC$word4 == a,]$word5
  ret_word <- quintC[quintC[1]==x & quintC[2]==y & quintC[3]==z & quintC[4] == a,]$word5
  return(ret_word)
  
}



#################################################
#The algorithm is a fairly "Stupid" Backoff model where it is based on the number of 
#words entered.  If 4 words or more are entered, it will predict the fifth word from the 
#prior 4 words entered starting in the Quint Dictionary.  If not found, it continues in that sequence 
#by setting the number of words to 3 and looking in the Quad dictionary.  It then subtracts
#the number of words and searches the TriCorpus Dictionary and so on.
#If nothing is found, it returns the most frequent unigram word which is "the"
#
#
#For added speed and efficiency, if the user enters 1, 2, 3 or 4 words only, it goes directly
#to the appropriate function bypassing the need for the subtraction of words from the entered
#string of data.



## ShineServer code to call the function predictWord
shinyServer(function(input, output) {

pw <- reactive({  
  words <- strsplit(input$in_string," ")[[1]]
  long <- length(words)
 

  #clear out predict_word from any previous run
  predict_word = character(0)

  
  if (long >=3) {
    predict_word = quadgram_predict(words[length(words)-2], words[length(words)-1], words[length(words)])
    long = 2
  } 
  if (length(predict_word) == 0 & long == 2) {
    predict_word = trigram_predict(words[length(words)-1],words[length(words)])
    long = 1
    predict_word
  } 
  if (length(predict_word) == 0 & long == 1) {
    predict_word = bigram_predict(words[length(words)]) 
    predict_word
  } 
  if (length(predict_word) == 0) predict_word = 'the'
  
 predict_word
 
}) 
 
    output$out2 <- renderText(pw());
    output$out1 <- renderText({input$in_string});
    
    
 }
)
  
  


