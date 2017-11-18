library(dplyr)
library(ngram)
library(tm)
library(quanteda)
library(readtext)
library(lattice)
library(tidyr)


blogfile <- readLines('final/en_US/en_US.blogs.txt')
blog <- readChar("final/en_US/en_US.blogs.txt", file.info("final/en_US/en_US.blogs.txt")$size)
bloglen <- length(gregexpr("\n",blog)[[1L]])


#Read in the news file then determine its length by using end of line character.
newsfile <- readLines('final/en_US/en_US.news.txt')
news <- readChar("final/en_US/en_US.news.txt", file.info("final/en_US/en_US.news.txt")$size)
newslen <- length(gregexpr("\n",news)[[1L]])


#Read in twitter file then determine its length by using end of line character.
twitfile <- readLines('final/en_US/en_US.twitter.txt')
twit <- readChar("final/en_US/en_US.twitter.txt", file.info("final/en_US/en_US.twitter.txt")$size)
twitlen <- length(gregexpr("\n",twit)[[1L]])



#Create a random sample of 1% of each of the three files.  At first, 5% was attempted
#giving the same answer so for speed the sample was changed for .01% to be used.
bindex <- sample(seq_len(bloglen),bloglen*.01)
nindex <- sample(seq_len(newslen),bloglen*.01)
tindex <- sample(seq_len(twitlen),bloglen*.01)

#Index each of the blog, news and twitter files.
subblog <- blogfile[bindex[]]
subnews <- newsfile[nindex[]]
subtwit <- twitfile[tindex[]]

#Now combine the indexed files to create a first rough draft of the Corpus.
Corpus <- corpus(c(subblog, subnews, subtwit))


sentences <- tokenize(Corpus, what="sentence", remove_url = TRUE, remove_twitter = TRUE)
                     

corpus_words <- tokenize(tolower(sentences), what="word", remove_punct=TRUE, remove_numbers = TRUE,
                          remove_hyphens = TRUE, remove_symbols = TRUE)
     
uniCorpus <- dfm(corpus_words, ngrams = 1)  
uniCorpus <- topfeatures(uniCorpus, 10)
uniCorpus
                         
biCorpus <- dfm(corpus_words, ngrams = 2)    
biC <- textstat_frequency(biCorpus)
biC <- separate(biC, feature, c("word1", "word2"), sep = "_") %>% group_by(word1) %>% filter(rank==min(rank))
  
triCorpus <- dfm(corpus_words, ngrams = 3)    
triC <- textstat_frequency(triCorpus) 
triC <- separate(triC, feature, c("word1", "word2","word3"), sep = "_") %>% group_by(word1) %>% filter(rank==min(rank))

quadCorpus <- dfm(corpus_words, ngrams = 4)    
quadC <- textstat_frequency(quadCorpus) 
quadC <- separate(quadC, feature, c("word1", "word2","word3","word4"), sep = "_") %>% group_by(word1) %>% filter(rank==min(rank))

quintCorpus <- dfm(corpus_words, ngrams = 5)    
quintC <- textstat_frequency(quintCorpus) 
quintC <- separate(quintC, feature, c("word1", "word2","word3","word4","word5"), sep = "_") %>% group_by(word1) %>% filter(rank==min(rank))



#################################
### Grab the input string, determine the length, clean the data
# Probably call a function to clean up the input string


# For now, I'm just setting it to get the algorithm working
in_string = ' '
words <- strsplit(in_string," ")[[1]]
long <- length(words)



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


#clear out predict_word from any previous run
predict_word = ' '

if (long >= 4) {
   #always predict off of the last 4 words.
   predict_word = quintgram_predict(words[long-3], words[long-2], words[long-1], words[long])
   long = 3
}   
if (predict_word == ' ' & long == 3) {
   predict_word = quadgram_predict(words[long-2], words[long-1], words[long])
   long = 2
}

if (predict_word == ' ' & long == 2) {
   predict_word = trigram_predict(words[long-1],words[long])
   long = 1
}
if (predict_word == ' ' & long == 1) 
   predict_word = bigram_predict(words[long])  


if (predict_word == "") predict_word = 'the'



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


   
#Left to do
#
#1. Function to clean up the input word(s)
#2. See if there is a way to reduce the size of the dictionaries only pulling top choices
#3. Run with more than .01 of the blog, twitter and news feed until memory becomes an issue
#
# 
#Shiny Web Application
# 5 Page Presentation
#


