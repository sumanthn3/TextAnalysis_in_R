install.packages('pacman')
library(pacman)
p_load('tm','stringr','tidyverse','tidytext','wordcloud','quanteda','syuzhet','topicmodels')
setwd('/Users/sumanthnandeti/Documents/Project3')
getwd()
PoMarsAll=VCorpus(DirSource("/Users/sumanthnandeti/Documents/Project3/GroupProject/",ignore.case = TRUE,mode = "text"))
str(PoMarsAll)
PoMarsAll

text=content(PoMarsAll[[1]])
text
ch_indices = str_which(text, pattern = "^CHAPTER\\s")
ch_indices=ch_indices[1:12]

#splitting the chapters
chapters = list()
for (i in 1:(length(ch_indices) - 1)) {
  chapters[[i]] = text[ch_indices[i]: (ch_indices[i + 1] - 1)]
}

#creating a directory of chapters I-XI
dir.create("chapters")
for (i in seq_along(chapters)) {
  chapter_file <- file.path("chapters/", paste0("Chapter_", i, ".txt"))
  writeLines(chapters[[i]], chapter_file)
}

#creating a vcorpus for I-XI chapters
PoMars=VCorpus(DirSource("/Users/sumanthnandeti/Documents/Project3/chapters",ignore.case = TRUE,mode = "text"))
str(PoMars)
PoMars
chaptersTidy=tidy(PoMars)
tidytext1=chaptersTidy$text[1]
number <- gsub("[^0-9]", "", chaptersTidy$id[1])

#finding 10largest words and sentences in first chapter

words <- str_extract_all(tidytext1, "\\w+") %>% unlist()
sortedWords <- words[order(nchar(words), decreasing = TRUE)]
longestUniqueWords <- sortedWords %>% unique() %>% head(10)

sentences <- str_split(tidytext1, "\\.\\s") %>% unlist()
sortedSentences <- sentences[order(nchar(sentences), decreasing = TRUE)]
longest10Sentences <- sortedSentences %>% unique() %>% head(10)


#defining a function to find 10 largest words and sentences for all XI chapters
find10longWordsSentences <- function(tidytext1,num1) {
  # Find the 10 longest words
  chapter_num <- gsub("[^0-9]", "", chaptersTidy$id[num1])
  words <- str_extract_all(tidytext1, "\\w+") %>% unlist()
  sortedWords <- words[order(nchar(words), decreasing = TRUE)]
  longest10Words <- sortedWords %>% unique() %>% head(10)
  
  sentences <- str_split(tidytext1, "\\.\\s") %>% unlist()
  sortedSentences <- sentences[order(nchar(sentences), decreasing = TRUE)]
  longest10Sentences <- sortedSentences %>% unique() %>% head(10)
  
  return(tibble(Chapter = chapter_num,
                ItemType = rep(c("Word", "Sentence"), each = 10),
                Item = c(longest10Words, longest10Sentences),
                Length = c(nchar(longest10Words), nchar(longest10Sentences))))
}

# print a table of first 10 longest words and sentences of each chapter
chapter_files <- list.files(path = "/Users/sumanthnandeti/Documents/Project3/chapters", pattern = "\\.txt$")
chapter_text <- character(length(chapter_files))

# Loop through each text file and read its contents into the vector
for (i in seq_along(chapter_files)) {
  chapter_text[i] <- get_text_as_string(file.path("/Users/sumanthnandeti/Documents/Project3/chapters", chapter_files[i]))
}
results_table <- map_dfr(seq_along(chapters), ~find10longWordsSentences(chapter_text[.],.))
print(results_table,n = 500)


# print a table of first 10 longest words and sentences of each chapter using another method
results_table <- map_dfr(seq_along(chapters), ~find10longWordsSentences(chaptersTidy$text[.],.))
print(results_table,n = 500)

#Document Term Matrix of chapters file
PoMarsDTM=DocumentTermMatrix(PoMars)
PoMarsDTM
inspect(PoMarsDTM)
str(PoMarsDTM)

#Term Document Matrix of entire file
PoMarsTDM=TermDocumentMatrix(PoMars)
PoMarsTDM
inspect(PoMarsTDM)
str(PoMarsTDM)

# creating a data frame of PoMars text file
PoMarsDF=data.frame(text)
PoMarsDF

#Remove numers and punctuations from the text file
PoMarsText_noNum=removeNumbers(text)
PoMarsText_noNum
PoMarsText_noNumPunc=removePunctuation(PoMarsText_noNum)
PoMarsText_noNumPunc

#Remove Num and Punc from a vcorpus file
removeNumPunc<-function(x) gsub("[^[:alpha:][:space:]]*","",x)
PoMarsCl=tm::tm_map(PoMars,content_transformer(removeNumPunc))
str(PoMarsCl)
content(PoMarsCl[[1]])
inspect(PoMars)
inspect(PoMarsCl)

#Converting text to lowercase characters
PoMarsCl_Low=tm::tm_map(PoMarsCl,content_transformer(tolower))
content(PoMarsCl_Low[[1]])

#Term Document Matrix of entire file
PoMarsTDM=TermDocumentMatrix(PoMarsCl_Low)
PoMarsTDM
inspect(PoMarsTDM)
str(PoMarsTDM)

#Document Term Matrix
PoMarsDTM=DocumentTermMatrix(PoMarsCl_Low)
PoMarsDTM
inspect(PoMarsDTM)
str(PoMarsDTM)

#Matrix
PoMarsMatrixTDM=as.matrix(PoMarsTDM)
PoMarsMatrixDTM=as.matrix(PoMarsDTM)

#Remove Stop words
myStopWords=(tm::stopwords(kind = 'en'))
PoMarsNoStopWords=tm::tm_map(PoMarsCl_Low,tm::removeWords,myStopWords)
str(PoMarsNoStopWords)
tm::inspect(PoMarsNoStopWords[[1]])
#TDM of PoMars without stop words
PoMarsNoStopTDM=TermDocumentMatrix(PoMarsNoStopWords)
PoMarsNoStopTDM
inspect(PoMarsNoStopTDM)
str(PoMarsNoStopTDM)

#Document Term Matrix of PoMars without stop words
PoMarsNoStopDTM=DocumentTermMatrix(PoMarsNoStopWords)
PoMarsNoStopDTM
inspect(PoMarsNoStopDTM)
str(PoMarsNoStopDTM)


#Find Frequency words
wordfreq=colSums(as.matrix(PoMarsNoStopDTM))
freqterms=tm::findFreqTerms(PoMarsNoStopTDM)
PoMarsTF=tm::termFreq(PoMarsNoStopWords[[1]])
PoMarsTF
#dendrogram
PoMarsDF1<-as.data.frame(PoMarsNoStopTDM[[1]])
PoMarsDist<-dist(PoMarsDF1)
PoMarsDG<-hclust(PoMarsDist,method="ward.D2")
str(PoMarsDG)
plot(PoMarsDG)

#dendogram by removing words with high sparsity
PoMarsNoStopTDM_Mat=as.matrix(PoMarsNoStopTDM)
# Set sparsity threshold (e.g., 0.85 means words that appear in greater than 85% of the documents)
sparsity_threshold <- 0.85

# Calculate sparsity for each word
sparsity <- rowSums(PoMarsNoStopTDM_Mat > 0) / ncol(PoMarsNoStopTDM_Mat)

# Select words with sparsity below the threshold
selected_words <- which(sparsity > sparsity_threshold)

# Create a new TDM with the selected words
PoMarsFilteredTDM <- PoMarsNoStopTDM_Mat[selected_words,]

# Convert the new TDM to a data frame
PoMarsDF1 <- as.data.frame(PoMarsFilteredTDM)

# Calculate the distance matrix
PoMarsDist <- dist(PoMarsDF1)

# Perform hierarchical clustering using Ward's method
PoMarsDG <- hclust(PoMarsDist, method = "ward.D2")

# Check the structure of the dendrogram
str(PoMarsDG)

# Plot the dendrogram
plot(PoMarsDG)


#WordCloud
words=names(PoMarsTF)
pal<-brewer.pal(9,"Spectral")
PoMarsWC=wordcloud(words,PoMarsTF,colors = pal)
#Quanteda
PoMarstext=PoMarsCl[[1]]
PoMarstext$content[1:10]
# tokens
PoMarsToken<-quanteda::tokens(PoMarstext$content)
PoMarsToken

#removing lines with no characters
PoMarsDFM=quanteda::dfm(PoMarsToken)
str(PoMarsDFM)
# calculate freq of words using quanteda
PoMarsfreq=quanteda::docfreq(PoMarsDFM)
PoMarsfreq
#weights
PoMarsWeights=quanteda::dfm_weight(PoMarsDFM)
PoMarsWeights
str(PoMarsWeights)
#Term Frequency-InverseDocumentFrequency
PoMarsTFIDF=quanteda::dfm_tfidf(PoMarsDFM, scheme_tf = "count" )

#syuzhet Package
PoMarstextDF=PoMars[[1]]$content
PoMarstextDF
# computing sentiment for only one chapter
PoMarsAsString=get_text_as_string("/Users/sumanthnandeti/Documents/Project3/chapters/Chapter_1.txt")
PoMarsAsString
PoMarsSentences=get_sentences(PoMarsAsString)
#Compute sentiment
PoMarsSentiment=get_sentiment(PoMarsSentences,"syuzhet")
#Sentiment Dictionary using syuzhet
SyuzhetDictionary=get_sentiment_dictionary("syuzhet")
SyuzhetDictionary
#finding overall sentiment in the text using syuzhet
SentimentSum=sum(PoMarsSentiment)
SentimentSum
SentimentMean=mean(PoMarsSentiment)
SentimentMean
summary(PoMarsSentiment)
plot(PoMarsSentiment, main = "PoMars Sentiment plot Trajectory using syuzhet", xlab = "Narrative", ylab = "Emotional Valence")

#Sentiment Dictionary Bing
BingDictionary=get_sentiment_dictionary("bing")
BingDictionary
PoMarsSentimentBing=get_sentiment(PoMarsSentences,"bing")
#finding overall sentiment in the text using Bing
SentimentSumBing=sum(PoMarsSentimentBing)
SentimentSumBing
SentimentMeanBing=mean(PoMarsSentimentBing)
SentimentMeanBing
summary(PoMarsSentimentBing)
plot(PoMarsSentimentBing, main = "PoMars Sentiment plot Trajectory using bing", xlab = "Narrative", ylab = "Emotional Valence")


#Sentiment Dictionary NRC of syuzhet
nrcDictionary=get_sentiment_dictionary("nrc")
nrcDictionary
PoMarsSentimentNrc=get_sentiment(PoMarsSentences,"nrc")
PoMarsSentimentNrc
#finding overall sentiment in the text using Bing
SentimentSumNRC=sum(PoMarsSentimentNrc)
SentimentSumNRC
SentimentMeanNrc=mean(PoMarsSentimentNrc)
SentimentMeanNrc
summary(PoMarsSentimentNrc)
plot(PoMarsSentimentBing, main = "PoMars Sentiment plot Trajectory using nrc of syuzhet", xlab = "Narrative", ylab = "Emotional Valence")

#Sentiment Dictionary NRC of tidytext
PoMarsSentimentNrc1=get_nrc_sentiment(PoMarsSentences)

# computing sentiment for each chapter

# Function to compute sentiment
compute_sentiment <- function(text) {
  sentences <- get_sentences(text)
  sentiment_values <- get_sentiment(sentences, "syuzhet")
  sentiment_sum <- sum(sentiment_values)
  sentiment_mean <- mean(sentiment_values)
  Sentiment_ValuesBing<-get_sentiment(sentences, "bing")
  sentiment_sum_bing <- sum(Sentiment_ValuesBing)
  sentiment_mean_bing <- mean(Sentiment_ValuesBing)
  Sentiment_ValuesNRC<-get_sentiment(sentences, "nrc")
  sentiment_sum_nrc <- sum(Sentiment_ValuesNRC)
  sentiment_mean_nrc <- mean(Sentiment_ValuesNRC)
  
  list(syuzhetSum = sentiment_sum, syuzhetMean = sentiment_mean,bingSum = sentiment_sum_bing, bingMean = sentiment_mean_bing,nrcSum = sentiment_sum_nrc, nrcMean = sentiment_mean_nrc )
}

# Get the list of chapter files
chapter_files <- list.files(path = "/Users/sumanthnandeti/Documents/Project3/chapters/", pattern = "Chapter_.*\\.txt$", full.names = TRUE)

# Compute sentiment for all chapters
sentiment_results <- lapply(chapter_files, function(chapter_file) {
  chapter_text <- get_text_as_string(chapter_file)
  compute_sentiment(chapter_text)
})

# Prepare table
chapter_names <- basename(chapter_files)
sentiment_table <- tibble(
  Chapter = chapter_names,
  SentimentSum_syuzhet = sapply(sentiment_results, function(res) res$syuzhetSum),
  SentimentMean_syuzhet = sapply(sentiment_results, function(res) res$syuzhetMean),
  SentimentSum_bing = sapply(sentiment_results, function(res) res$bingSum),
  SentimentMean_bing = sapply(sentiment_results, function(res) res$bingMean),
  SentimentSum_nrc = sapply(sentiment_results, function(res) res$nrcSum),
  SentimentMean_nrc = sapply(sentiment_results, function(res) res$nrcMean)
)

# Display table
print(sentiment_table)

#percentage value with 10 bins
PoMarsPCTValue=get_percentage_values(PoMarsSentiment, bins=10)
PoMarsPCTValue
structure(PoMarsPCTValue)
plot(PoMarsPCTValue, main = "PoMars percentage values 10 bins", xlab = "Narrative", ylab = "Emotional Valence",col='red')
#percentage value with 20 bins
PoMarsPCTValue=get_percentage_values(PoMarsSentiment, bins=20)
PoMarsPCTValue
structure(PoMarsPCTValue)
plot(PoMarsPCTValue, main = "PoMars percentage values 10 bins", xlab = "Narrative", ylab = "Emotional Valence",col='red')


#Topic models Package
#LDA:Latent Dirichlet Allocation (LDA) is a generative probabilistic model used for topic modeling in natural language processing and machine learning. The main goal of LDA is to discover the underlying latent topics in a collection of documents. LDA is an unsupervised technique, meaning it does not require labeled data to train the model.

lda_PoMars<-topicmodels::LDA(PoMarsNoStopDTM,k = 5)

# Print the topic-word distribution
topic_word_dist <- as.data.frame(lda_PoMars@beta)
print(topic_word_dist)

# Print the document-topic distribution
document_topic_dist <- as.data.frame(lda_PoMars@gamma)
print(document_topic_dist)

#CTM: CTM (Correlated Topic Model) is a generative probabilistic model used for topic modeling in natural language processing and machine learning. CTM is an extension of the Latent Dirichlet Allocation (LDA) model, which also aims to discover the underlying latent topics in a collection of documents. However, unlike LDA, CTM allows topics to be correlated, which can lead to a more accurate representation of the underlying structure of the text data.
# Estimate the CTM model
ctm_model <- CTM(PoMarsNoStopDTM, k = 5)

# Print the topic-word distribution
topic_word_dist <- as.data.frame(ctm_model@beta)
print(topic_word_dist)

# Print the document-topic distribution
document_topic_dist <- as.data.frame(ctm_model@gamma)
print(document_topic_dist)

#word cloud package
words=names(PoMarsTF)
pal<-brewer.pal(9,"Spectral")
PoMarsWC=wordcloud(words,PoMarsTF,colors = pal)

# Define the colors for each document group
colors <- brewer.pal(9, "Set1")

# Create a comparison cloud:Creates a word cloud that compares word frequencies across different texts or groups.
PoMarsNoStopTDM_Mat=as.matrix(PoMarsNoStopTDM)

comparison.cloud(PoMarsNoStopTDM_Mat, colors = colors,scale = c(3, 0.5),random.order = FALSE,title.size = 0.5)
#Commonality Cloud: The commonality.cloud() function is part of the wordcloud package in R and is used to create a word cloud that highlights the common words shared by multiple documents
commonality.cloud(PoMarsNoStopTDM_Mat, colors = colors,scale = c(3, 0.5),random.order = FALSE)

# extra methods for text mining package
#weightTf(), weightTfIdf(): Applies term frequency (TF) or term frequency-inverse document frequency (TF-IDF) weighting to a DTM or TDM. These functions can be used to transform the raw term frequencies in a matrix into weighted values that better represent the importance of terms in the documents
# DTM after performing preprocessing is given as the input to the functions.
PoMarsdtm_tf <- weightTf(PoMarsNoStopDTM)
inspect(PoMarsdtm_tf)
PoMarsdtm_tfidf <- weightTfIdf(PoMarsNoStopDTM)
inspect(PoMarsdtm_tfidf)
#stemDocument(): Performs word stemming using the Snowball stemmer.
PoMarsNoStopWords_stemmed <- tm_map(PoMarsNoStopWords, stemDocument)
inspect(PoMarsNoStopWords_stemmed)
lapply(PoMarsNoStopWords_stemmed, as.character)


