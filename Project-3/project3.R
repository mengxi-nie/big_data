install.packages("tm")
install.packages("textreuse")
install.packages("wordnet")
install.packages("zipfR")
install.packages("wordcloud")
install.packages("NLP")
install.packages("openNLP")
install.packages("formattable")

library(openNLP)
library(NLP)
library(tm)
library(textreuse)
library(wordnet)
library(zipfR)
library(wordcloud)
library(formattable)

#========== Try functions in lecture 9 =======#
data("acq")
acq
inspect(acq)
head(summary(acq), n=15)
dtm <- DocumentTermMatrix(acq)
dtm
termFreq(acq[[1]])
dm2 <- TermDocumentMatrix(acq, control = list(wordLengths = c(1, Inf)))
dm2
assoc <- findAssocs(dm2, "states", 0.25)
assoc
freq.terms <- findFreqTerms(dm2, lowfreq = 3)
freq.terms

#========= Get the 15 longest documents ==========#
temp <- tm_map(acq, content_transformer(tolower)) 
temp <- tm_map(temp, removeWords, stopwords("english")) 
temp <- tm_map(acq, removePunctuation) 
temp <- tm_map(temp, removeNumbers) 
temp <- tm_map(temp, stripWhitespace) 
dtm <- DocumentTermMatrix(temp)
word_count <- rowSums(as.matrix(dtm))
dtm_sort <- names(head(sort(word_count, decreasing = T), 15))
dtm_sort


#========= Show the dendrogram and wordcloud ========#
for(i in 1:50){
  test<-acq[26]
  test
  SATlow <- tm_map(test, content_transformer(tolower))
  SATlow
  removeNumPunct <-function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  SATcl <- tm_map(SATlow, content_transformer(removeNumPunct))
  myStopwords <- c(stopwords('english'))
  SATstop <- tm_map(SATcl, removeWords, myStopwords)
  SATtdm2<-TermDocumentMatrix(SATstop,control=list(wordLengths=c(1,Inf)))
  SATtdm2
  m1<-as.matrix(SATtdm2)
  word.freq<-sort(rowSums(m1),decreasing = T)
  word.freq
  pal <- brewer.pal(9, "BuGn")
  pal <- pal[-(1:4)]
  wordcloud(words = names(
    word.freq), freq = word.freq, min.freq = 2, random.order = F, colors = pal
  )
  testdf <- as.data.frame(word.freq)
  distMatrix <- dist(scale(testdf))
  fit <- hclust(distMatrix, method = "ward.D2")
  plot(fit)
  groups <- cutree(fit, k=4)
  rect.hclust(fit, k=4, border="red")
}


#======== Find the longest word and lonest sentence ==============#
sents <- tokenize_sentences(acq[[dtm_sort[1]]]$content)
word <- tokenize_words(acq[[dtm_sort[1]]]$content)
# longest word by chars
word[which.max(nchar(word))]
# longest sentence by chars
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[2]]]$content)
word <- tokenize_words(acq[[dtm_sort[2]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[3]]]$content)
word <- tokenize_words(acq[[dtm_sort[3]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[4]]]$content)
word <- tokenize_words(acq[[dtm_sort[4]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[5]]]$content)
word <- tokenize_words(acq[[dtm_sort[5]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[6]]]$content)
word <- tokenize_words(acq[[dtm_sort[6]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[7]]]$content)
word <- tokenize_words(acq[[dtm_sort[7]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[8]]]$content)
word <- tokenize_words(acq[[dtm_sort[8]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[9]]]$content)
word <- tokenize_words(acq[[dtm_sort[9]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[10]]]$content)
word <- tokenize_words(acq[[dtm_sort[10]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[11]]]$content)
word <- tokenize_words(acq[[dtm_sort[11]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[12]]]$content)
word <- tokenize_words(acq[[dtm_sort[12]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[13]]]$content)
word <- tokenize_words(acq[[dtm_sort[13]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[14]]]$content)
word <- tokenize_words(acq[[dtm_sort[14]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

sents <- tokenize_sentences(acq[[dtm_sort[15]]]$content)
word <- tokenize_words(acq[[dtm_sort[15]]]$content)
word[which.max(nchar(word))]
sents[which.max(nchar(sents))]

#========== Print the length of sentences of 10 largest files =====#
sents <- tokenize_sentences(acq[[dtm_sort[1]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[2]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[3]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[4]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[5]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[6]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[7]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[8]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[9]]]$content)
formattable(sapply(sents, wordcount))
sents <- tokenize_sentences(acq[[dtm_sort[10]]]$content)
formattable(sapply(sents, wordcount))
#======= Remove punctuation and display sentence ==========#
no_punct <- tm_map(acq, removePunctuation)
dataframe<-data.frame(text=unlist(sapply(no_punct, `[`, "content")), stringsAsFactors=F)
as.String(dataframe$text)

#================= Print part-of-speech =================#
require("NLP")
tokenized <- tokenize_words(as.String(dataframe$text))
s <-as.String(dataframe$text)
posTagger <- Maxent_POS_Tag_Annotator(language = "en", probs = F, model = NULL)
sent_token_annotator <- Maxent_Sent_Token_Annotator ()
word_token_annotator <- Maxent_Word_Token_Annotator ()
pos_tag_annotator <- Maxent_POS_Tag_Annotator ()
dir <- annotate(s, c(sent_token_annotator, word_token_annotator))
t <- annotate(s, posTagger, dir)
t
head(t, n = 50)

#============= Analyze word frequencey using zipfR =======#
library(zipfR)
?zipfR
data(acq)
summary(acq.spc)
N(acq.spc)
V(acq.spc)
#===Baayen's P
Vm(acq.spc, 1) / N(acq.spc)
plot(acq.spc)
plot(acq.spc, log = "x")

acq.fzm <- lnre("fzm",acq.spc)
summary(acq.fzm)
#===Looking at VGCs
summary(acq.vgc)
acq.vgc
N(acq.vgc)
plot(acq.vgc)


##ADD another one######
#=== Estimating LNRE models
library("tm")
data("acq")
acq
tdm <- TermDocumentMatrix(acq)
# for clarity we limit the size of this for the print our
temp <- inspect(tdm)
freq <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(freq) <- NULL
head(freq)
mytfl <- tfl(freq$Freq, freq$ST)
mytfl
acq.spc <- tfl2spc(mytfl)
acq.spc
# frequency spectrum
plot(acq.spc)
summary(acq.spc)

acq.fzm <- lnre("fzm", acq.spc)
summary(acq.fzm)

# Vocab Growth
Vm(acq.fzm,1)/N(acq.spc)
sample.sizes <- floor (N(acq.spc)/100*(1:100))
acq.vgc <- vgc.interp(acq.spc, sample.sizes)
plot(acq.vgc)



install.packages("quanteda")
install.packages("stats")
library(quanteda)
library(stats)

my_search <- function(data, search){
  mycorpus <- corpus(data)
  #mycorpus <- corpus(acq)
  
  #result <- kwic(mycorpus, "statement" , window = 3)
  result <- kwic(mycorpus, phrase(search) , window = 3)
  #result
  #result[[1]][[1]]
  # mycorpus[result[[1]][1]]
  #get the document number
  doc_number <- result[[1]]
  #doc_number_u <- unique(result[[1]])
  #doc_number_u
  #print the documents that contain the specific word
  docs <- list(unique(result[[1]]))
  #docs
  
  #out<-capture.output(cat(mycorpus[docs[[1]]]))
  #out
  #summary(out)
  #docs[[1]][1]
  
  line_index <- list()
  for (i in 1:length(unique(result[[1]]))){
    temp_doc <- capture.output(cat(mycorpus[docs[[1]][i]]))
    line <- grep(search, temp_doc, ignore.case = T)
    #line
    line_index <-  append(line_index, as.numeric(line))
    #line_index
  }
  #line_index
  
  
  # word index in document
  word_index <- result[[2]] 
  #word_index
  doc_index <- result[[1]]
  #doc_index
  
  line_index <- data.frame(matrix(unlist(line_index)))
  
  
  df <- data.frame(doc_index, line_index, word_index)
  colnames(df) <- c("doc_number", "line_number", "word_index")
  
  word_used <- cat("Searching documents for:", search, "\n")
  return(df)
}

my_search(acq, "statement")
my_search(acq, "Thursday")
my_search(acq, "employees")
my_search(acq, "as rumors") 

