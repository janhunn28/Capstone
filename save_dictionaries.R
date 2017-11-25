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
bindex <- sample(seq_len(bloglen),bloglen*.20)
nindex <- sample(seq_len(newslen),newslen*.20)
tindex <- sample(seq_len(twitlen),twitlen*.20)

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
biC <- subset(biC, frequency > 1)

triCorpus <- dfm(corpus_words, ngrams = 3)    
triC <- textstat_frequency(triCorpus) 
triC <- separate(triC, feature, c("word1", "word2","word3"), sep = "_") %>% group_by(word1) %>% filter(rank==min(rank))
#triC <- subset(triC, frequency > 1)

quadCorpus <- dfm(corpus_words, ngrams = 4)    
quadC <- textstat_frequency(quadCorpus) 
quadC <- separate(quadC, feature, c("word1", "word2","word3","word4"), sep = "_") %>% group_by(word1) %>% filter(rank==min(rank))
#quadC <- subset(quadC, frequency > 1)

#quintCorpus <- dfm(corpus_words, ngrams = 5)    
#quintC <- textstat_frequency(quintCorpus) 
#quintC <- separate(quintC, feature, c("word1", "word2","word3","word4","word5"), sep = "_") %>% group_by(word1) %>% filter(rank==min(rank))
#quintC <- subset(quintC, frequency > 1)

saveRDS(biC, file = "biC.rds")
saveRDS(triC, file = "triC.rds")
saveRDS(quadC, file = "quadC.rds")
#saveRDS(quintC, file = "quintC.rds")


#try <- tbl_df(biC)
#try3 <- tbl_df(triC)
#try4 <- tbl_df(quadC)
#saveRDS(try, file = "try.rds")
#saveRDS(try3, file = "try3.rds")
#saveRDS(try4, file = "try4.rds")

