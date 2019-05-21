#############################################
### INITIAL EVALUTAION
##############################################

#######################
#DATA LOADING
setwd('C:\\Sritham\\sem2_classes\\businessAnalytics\\project3\\dataset')
data_ca <- read.csv('CAvideos.xlsx.csv') #Canada - English & French
data_de <- read.csv('DEvideos.csv') #Denamrk - Danish
data_fr <- read.csv('FRvideos.csv') #France - French
data_gb <- read.csv('GBvideos.csv') #Great Britain - English
data_in <- read.csv('INvideos.csv') #India - English
data_jp <- read.csv('JPvideos.csv') #Japan - Japanese
data_kr <- read.csv('KRvideos.csv') #Korea - Korean
data_mx <- read.csv('MXvideos.csv') #Mexico - spanish
data_ru <- read.csv('RUvideos.csv') #Russia - russian
data_us <- read.csv('USvideos.csv') #USA - English
#################

data_na <- rbind(data_ca,data_us,data_mx)
data_eu <- rbind(data_fr,data_gb,data_de)
data_in

###scoring
NROW(data_na[!complete.cases(data_na),])
data_na$score <- ((0.2 * data_na$likes) + (0.6 * data_na$views) + (0.2 * data_na$comment_count))
data_eu$score <- ((0.2 * data_eu$likes) + (0.6 * data_eu$views) + (0.2 * data_eu$comment_count))
data_in$score <- ((0.2 * data_in$likes) + (0.6 * data_in$views) + (0.2 * data_in$comment_count))

summary(data_na$score)
g1_na <- subset(data_na, score >= 172507)
g2_na <- subset(data_na, score < 172507)

summary(data_eu$score)
g1_eu <- subset(data_eu, score >=117643)
g2_eu <- subset(data_eu, score < 117643)

summary(data_in$score)
g1_in <- subset(data_in, score >=183949)
g2_in <- subset(data_in, score < 183949)


library("data.table")
fwrite(g1_eu, file = "g1_eu.csv")

##########
#LIBRARIES
library(tm)
library(quanteda)
library(ggplot2)
library(wordcloud)
library(stm)
library(igraph)
library(tm)
library(cluster)
##############

#####
#CANADA
#####
NROW(unique(data_ca$video_id))
#####
#corelation and regression
str(data_ca)
cor(data_ca$views,data_ca$likes)
cor(data_ca$views,data_ca$dislikes)
cor(data_ca$likes,data_ca$dislikes)
cor(data_ca$views, data_ca$comment_count)
cor(data_ca$comment_count, data_ca$likes)
cor(data_ca$comment_count, data_ca$dislikes)

model1 <- lm(data = data_ca,data_ca$views ~ data_ca$likes + 
               data_ca$dislikes + data_ca$comment_count + data_ca$comments_disabled + data_ca$ratings_disabled)
model1 <- lm(data = data_ca,data_ca$likes ~ data_ca$views + 
               data_ca$dislikes + data_ca$comment_count + data_ca$comments_disabled + data_ca$ratings_disabled)

plot(model1)
summary(model1)
######

########
#scoring
data_ca$score <- ((0.2 * data_ca$likes) + (0.6 * data_ca$views) + (0.2 * data_ca$comment_count))

temp <- sort(data_ca)
head(temp)
#######

##########
#DATA INSIGHTS

############

######
#DATA CONVERSION TO FACTORS

######










##########
#combining data
###for europe
data_eu <- rbind(data_mx, data_us, data_ca)
str(data_eu)

# removing duplicate video_id
library("dplyr")
data <- data_eu %>% distinct(video_id, .keep_all = T)

data$score <- (0.2*data$likes) + (0.2*data$comment_count) + (0.6*data$views)
data$score <- data$score/100
summary(data)

###############
# video title cleaning
###############

# Text analysis for video title
library(tm)
library(quanteda)

data$title <- gsub("'", "", data$title) # remove apostrophes
data$title <- gsub("[[:punct:]]", " ", data$title)  # replace punctuation with space
data$title <- gsub("[[:cntrl:]]", " ", data$title)  # replace control characters with space
data$title <- gsub("^[[:space:]]+", "", data$title) # remove whitespace at beginning of documents
data$title <- gsub("[[:space:]]+$", "", data$title) # remove whitespace at end of documents
data$title <- gsub("[^a-zA-Z -]", " ", data$title) # allows only letters
data$title <- tolower(data$title)  # force to lowercase

sum((data$title == ""))

data$title <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", data$title, perl=TRUE)
data <- data[!(data$title == ""), ]
summary(data$score)

# grouping it according to the quartile values
g1_t <- subset(data, score >= 557.70)
g2_t <- subset(data, score < 557.70)


###############
# tags cleaning
###############

# Text analysis for video title
library(tm)
library(quanteda)

data$tags <- gsub("'", "", data$tags) # remove apostrophes
data$tags <- gsub("[[:punct:]]", " ", data$tags)  # replace punctuation with space
data$tags <- gsub("[[:cntrl:]]", " ", data$tags)  # replace control characters with space
data$tags <- gsub("^[[:space:]]+", "", data$tags) # remove whitespace at beginning of documents
data$tags <- gsub("[[:space:]]+$", "", data$tags) # remove whitespace at end of documents
data$tags <- gsub("[^a-zA-Z -]", " ", data$tags) # allows only letters
data$tags <- tolower(data$tags)  # force to lowercase


data$tags <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", data$tags, perl=TRUE)
sum((data$tags == ""))
data <- data[!(data$tags == ""), ]
summary(data$score)

# grouping it according to the quartile values
g1_t <- subset(data, score > 302.67)
g2_t <- subset(data, score <= 302.67)

###############
# description cleaning
###############

# Text analysis for video title
library(tm)
library(quanteda)

data$description <- gsub("'", "", data$description) # remove apostrophes
data$description <- gsub("[[:punct:]]", " ", data$description)  # replace punctuation with space
data$description <- gsub("[[:cntrl:]]", " ", data$description)  # replace control characters with space
data$description <- gsub("^[[:space:]]+", "", data$description) # remove whitespace at beginning of documents
data$description <- gsub("[[:space:]]+$", "", data$description) # remove whitespace at end of documents
data$description <- gsub("[^a-zA-Z -]", " ", data$description) # allows only letters
data$description <- tolower(data$description)  # force to lowercase



data$description <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", data$description, perl=TRUE)
sum((data$description == ""))
data <- data[!(data$description == ""), ]
summary(data$score)

# grouping it according to the quartile values
g1_t <- subset(data, score > 1185.96)
g2_t <- subset(data, score <= 1185.96 & score >= 78.16)
g3_t <- subset(data, score < 78.1
               
########################################
## Description Analysis on Europe
##########################################
#ANALYSIS OF VIDEO description

##############
## GROUP 1 ###
##############

g1_t_description_corpus <- corpus(g1_t$description,
                                  docnames = g1_t$video_id,
                                  docvar = data.frame(views = g1_t$views, 
                                                      likes = g1_t$likes,
                                                      dislikes = g1_t$dislikes,
                                                      comment_count = g1_t$comment_count,
                                                      comment_disabled = g1_t$comments_disabled,
                                                      ratings_disabled = g1_t$ratings_disabled))

names(g1_t_description_corpus)
summary(g1_t_description_corpus)

# viewing the richness of the docs
doc.df_g1_t_description <- g1_t_description_corpus$documents
token.df_g1_t_description <- count.fields(textConnection(doc.df_g1_t_description$texts))
doc.df_g1_t_description$Tokens <- token.df_g1_t_description

library(ggplot2)
g1_t_description_tockenplot <- ggplot(data = doc.df_g1_t_description, aes(x = Tokens))
g1_t_description_tockenplot + geom_histogram(binwidth = 20) + ylab("Distribution of tokens")


# creating a document feature matrix
help(dfm)
g1_t_description_dfm <- dfm(g1_t_description_corpus, 
                            remove = c(stopwords("en"), stopwords("fr"), stopwords("da")), 
                            verbose=TRUE,
                            stem=FALSE)

topfeatures(g1_t_description_dfm, n = 50)

g1_t_swlist <- c('o','k','h')

g1_t_description_dfm <- dfm(g1_t_description_corpus, 
                            remove = c(g1_t_swlist, stopwords("en"), stopwords("fr"), stopwords("da")), 
                            verbose=TRUE, 
                            stem=FALSE)
topfeatures(g1_t_description_dfm, n = 100)

# evaluating sparcity and removing the words that does not appear in at least 2 docs
g1_t_description_dfm.tm <- convert(g1_t_description_dfm, to="tm")
g1_t_description_dfm.tm  
g1_t_description_dfm.sparse <- removeSparseTerms(g1_t_description_dfm.tm, 0.98)
g1_t_description_dfm.sparse


#exploration in context
tokens <- as.tokens()
kwic(g1_t_description_corpus, "b", 2)

kwic(g1_t_description_corpus , "b", window = 3)



# Sentiment Analysis for g1_t_descp
mydict <- dictionary(list(negative = c("detriment*", "bad*", "awful*", "terrib*", "horribl*",'sur','très','tout','contre','faire','temps','mort','peu','part','reste'),
                          postive = c("good", "great", "super*", "excellent", "yay",'plus','comme','premier','bien','groupe','sans','fin','non','notamment','grand','som','under','mod','store','stor','side','helt','ny','bedste','arbejde')))

g1_t_description_sentiment <- dfm(g1_t_description_corpus, 
                                  remove = c(g1_t_swlist, stopwords("en"), stopwords("fr"), stopwords("da")), 
                                  verbose=TRUE, 
                                  dictionary = mydict,
                                  stem=FALSE)
topfeatures(g1_t_description_sentiment)
View(g1_t_description_sentiment)

# forming the word cloud for g1_t_descp

install.packages("wordcloud")
library("wordcloud")
set.seed(420)   #keeps cloud' shape fixed
dark2 <- brewer.pal(8, "Set1")   
freq <- topfeatures(g1_t_description_dfm, n=500)

wordcloud(names(freq), 
          freq, max.words = 200, 
          scale = c(3, 0.1), 
          colors = brewer.pal(8, "Set1"))

#specifying a correlation limit of 0.5   
findAssocs(g1_t_description_dfm.tm, 
           c("trailer", "official", "b"), 
           corlimit = 0.4)


# Topic Modeling for g1_t_descp
#install.packages("stm")
library(stm)

#Process the data for analysis.
help("textProcessor")
textpro <- textProcessor(documents = g1_t$description, metadata = g1_t)
names(textpro)  # produces:  "documents", "vocab", "meta", "docs.removed" 
meta <- textpro$meta
vocab <- textpro$vocab
docs <- textpro$documents
out <- prepDocuments(docs, vocab, meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta


#running stm for top 20 topics
help("stm")
prevfit <- stm(docs, vocab, 
               K=20, 
               verbose = TRUE,
               data = meta, 
               max.em.its=50)

topics <- labelTopics(prevfit , topics=c(1:20))
topics   #shows topics with highest probability words

#exploring the topics in context.  Provides an example of the text 
help("findThoughts")

z <- g1_t$description
z <- z[1:13444]
length(z)

findThoughts(prevfit, texts = z,  topics = 10,  n = 2)

help("plot.STM")
plot.STM(prevfit, type="summary")
#plot.STM(prevfit, type="labels", topics=c())
plot.STM(prevfit, type="perspectives", topics = c(19, 2))

# to aid on assigment of labels & intepretation of topics
help(topicCorr)
mod.out.corr <- topicCorr(prevfit)  #Estimates a graph of topic correlations

#install.packages("igraph")
library(igraph)

plot(mod.out.corr)


##############################
###regular package  tag analysis
###############################
#ANALYSIS OF VIDEO title

##############
## GROUP 1 ###
##############

g1_t_title_corpus <- corpus(g1_t$title,
                            docnames = g1_t$video_id,
                            docvar = data.frame(views = g1_t$views, 
                                                likes = g1_t$likes,
                                                dislikes = g1_t$dislikes,
                                                comment_count = g1_t$comment_count,
                                                comment_disabled = g1_t$comments_disabled,
                                                ratings_disabled = g1_t$ratings_disabled))

names(g1_t_title_corpus)
summary(g1_t_title_corpus)

# viewing the richness of the docs
doc.df_g1_t_title <- g1_t_title_corpus$documents
token.df_g1_t_title <- count.fields(textConnection(doc.df_g1_t_title$texts))
doc.df_g1_t_title$Tokens <- token.df_g1_t_title

library(ggplot2)
g1_t_title_tockenplot <- ggplot(data = doc.df_g1_t_title, aes(x = Tokens))
g1_t_title_tockenplot + geom_histogram(binwidth = 20) + ylab("Distribution of tokens")


# creating a document feature matrix
help(dfm)
g1_t_title_dfm <- dfm(g1_t_title_corpus, 
                      remove = c(stopwords("en"), stopwords("fr"), stopwords("da")), 
                      verbose=TRUE,
                      stem=FALSE)

topfeatures(g1_t_title_dfm, n = 50)

g1_t_swlist <- c('r','die','f','e','k','das','im','b','h','re','g','o','ln','v','p','und','vom','ist','auf','ein','aus','zu','wie','zum')

g1_t_title_dfm <- dfm(g1_t_title_corpus, 
                      remove = c(g1_t_swlist, stopwords("en"), stopwords("fr"), stopwords("da")), 
                      verbose=TRUE, ngram = 2,
                      stem=FALSE)
topfeatures(g1_t_title_dfm, n = 50)

# evaluating sparcity and removing the words that does not appear in at least 2 docs
g1_t_title_dfm.tm <- convert(g1_t_title_dfm, to="tm")
g1_t_title_dfm.tm  
g1_t_title_dfm.sparse <- removeSparseTerms(g1_t_title_dfm.tm, 0.98)
g1_t_title_dfm.sparse


#exploration in context
tokens <- as.tokens()
kwic(g1_t_title_corpus, "r", 2)

kwic(g1_t_title_corpus , "b", window = 3)



# Sentiment Analysis for g1_t_descp
mydict <- dictionary(list(negative = c("detriment*", "bad*", "awful*", "terrib*", "horribl*",'sur','très','tout','contre','faire','temps','mort','peu','part','reste'),
                          postive = c("good", "great", "super*", "excellent", "yay",'plus','comme','premier','bien','groupe','sans','fin','non','notamment','grand','som','under','mod','store','stor','side','helt','ny','bedste','arbejde')))

g1_t_title_sentiment <- dfm(g1_t_title_corpus, 
                            remove = c(g1_t_swlist, stopwords("en"), stopwords("fr"), stopwords("da")), 
                            verbose=TRUE, 
                            dictionary = mydict,
                            stem=FALSE)
topfeatures(g1_t_title_sentiment)
View(g1_t_title_sentiment)

# forming the word cloud for g1_t_descp

install.packages("wordcloud")
library("wordcloud")
set.seed(420)   #keeps cloud' shape fixed
dark2 <- brewer.pal(8, "Set1")   
freq <- topfeatures(g1_t_title_dfm, n=500)

wordcloud(names(freq), 
          freq, max.words = 50, 
          scale = c(3, 0.1), 
          colors = brewer.pal(8, "Set1"))

#specifying a correlation limit of 0.5   
findAssocs(g1_t_title_dfm.tm, 
           c("de", "tr"), 
           corlimit = 0.4)


# Topic Modeling for g1_t_descp
#install.packages("stm")
library(stm)

#Process the data for analysis.
help("textProcessor")
textpro <- textProcessor(documents = g1_t$title, metadata = g1_t)
names(textpro)  # produces:  "documents", "vocab", "meta", "docs.removed" 
meta <- textpro$meta
vocab <- textpro$vocab
docs <- textpro$documents
out <- prepDocuments(docs, vocab, meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta


#running stm for top 20 topics
help("stm")
prevfit <- stm(docs, vocab, 
               K=20, 
               verbose = TRUE,
               data = meta, 
               max.em.its=50)

topics <- labelTopics(prevfit , topics=c(1:20))
topics   #shows topics with highest probability words

#exploring the topics in context.  Provides an example of the text 
help("findThoughts")

z <- g1_t$title
z <- z[1:27307]
length(z)

findThoughts(prevfit, texts = z,  topics = 10,  n = 2)

help("plot.STM")
plot.STM(prevfit, type="summary")
#plot.STM(prevfit, type="labels", topics=c())
plot.STM(prevfit, type="perspectives", topics = c(17, 16))

# to aid on assigment of labels & intepretation of topics
help(topicCorr)
mod.out.corr <- topicCorr(prevfit)  #Estimates a graph of topic correlations

#install.packages("igraph")
library(igraph)

plot(mod.out.corr)
plot(mod.out.corr,  
     topics = c(1:20),
     vlabels = c('topic 17: ', 'topic 2: probably noise', 'topic 3: noise or thinkthank`s report', 'topic 4: asian-pacific region', 'topic 5: northkorea and kimjongun', 'topic 6: security measure of US', 'topic 7: denuclearization of northkorea', 'topic 8: economic problem in US', 'topic 9: Israel problem', 'topic 10: humanright issue'))


#####################
#Premium video title analysis
######################
#ANALYSIS OF VIDEO TITLE

##############
## GROUP 1 ###
##############

g1_t_title_corpus <- corpus(g1_t$title,
                            docnames = g1_t$video_id,
                            docvar = data.frame(views = g1_t$views, 
                                                likes = g1_t$likes,
                                                dislikes = g1_t$dislikes,
                                                comment_count = g1_t$comment_count,
                                                comment_disabled = g1_t$comments_disabled,
                                                ratings_disabled = g1_t$ratings_disabled))

names(g1_t_title_corpus)
summary(g1_t_title_corpus)

# viewing the richness of the docs
doc.df_g1_t_title <- g1_t_title_corpus$documents
token.df_g1_t_title <- count.fields(textConnection(doc.df_g1_t_title$texts))
doc.df_g1_t_title$Tokens <- token.df_g1_t_title

library(ggplot2)
g1_t_title_tockenplot <- ggplot(data = doc.df_g1_t_title, aes(x = Tokens))
g1_t_title_tockenplot + geom_histogram(binwidth = 20) + ylab("Distribution of tokens")


# creating a document feature matrix
help(dfm)
g1_t_title_dfm <- dfm(g1_t_title_corpus, 
                      remove = c(stopwords("en"), stopwords("fr"), stopwords("da")), 
                      verbose=TRUE,
                      stem=FALSE)

topfeatures(g1_t_title_dfm, n = 50)

g1_t_swlist <- c('o','k','h')

g1_t_title_dfm <- dfm(g1_t_title_corpus, 
                      remove = c(g1_t_swlist, stopwords("en"), stopwords("fr"), stopwords("da")), 
                      verbose=TRUE, 
                      stem=FALSE)
topfeatures(g1_t_title_dfm, n = 100)

# evaluating sparcity and removing the words that does not appear in at least 2 docs
g1_t_title_dfm.tm <- convert(g1_t_title_dfm, to="tm")
g1_t_title_dfm.tm  
g1_t_title_dfm.sparse <- removeSparseTerms(g1_t_title_dfm.tm, 0.98)
g1_t_title_dfm.sparse


#exploration in context
tokens <- as.tokens()
kwic(g1_t_title_corpus, "b", 2)

kwic(g1_t_title_corpus , "b", window = 3)



# Sentiment Analysis for g1_t_descp
mydict <- dictionary(list(negative = c("detriment*", "bad*", "awful*", "terrib*", "horribl*",'sur','très','tout','contre','faire','temps','mort','peu','part','reste'),
                          postive = c("good", "great", "super*", "excellent", "yay",'plus','comme','premier','bien','groupe','sans','fin','non','notamment','grand','som','under','mod','store','stor','side','helt','ny','bedste','arbejde')))

g1_t_title_sentiment <- dfm(g1_t_title_corpus, 
                            remove = c(g1_t_swlist, stopwords("en"), stopwords("fr"), stopwords("da")), 
                            verbose=TRUE, 
                            dictionary = mydict,
                            stem=FALSE)
topfeatures(g1_t_title_sentiment)
View(g1_t_title_sentiment)

# forming the word cloud for g1_t_descp

install.packages("wordcloud")
library("wordcloud")
set.seed(420)   #keeps cloud' shape fixed
dark2 <- brewer.pal(8, "Set1")   
freq <- topfeatures(g1_t_title_dfm, n=500)

wordcloud(names(freq), 
          freq, max.words = 200, 
          scale = c(3, 0.1), 
          colors = brewer.pal(8, "Set1"))

#specifying a correlation limit of 0.5   
findAssocs(g1_t_title_dfm.tm, 
           c("trailer", "official", "b"), 
           corlimit = 0.4)


# Topic Modeling for g1_t_descp
#install.packages("stm")
library(stm)

#Process the data for analysis.
help("textProcessor")
textpro <- textProcessor(documents = g1_t$title, metadata = g1_t)
names(textpro)  # produces:  "documents", "vocab", "meta", "docs.removed" 
meta <- textpro$meta
vocab <- textpro$vocab
docs <- textpro$documents
out <- prepDocuments(docs, vocab, meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta


#running stm for top 20 topics
help("stm")
prevfit <- stm(docs, vocab, 
               K=20, 
               verbose = TRUE,
               data = meta, 
               max.em.its=50)

topics <- labelTopics(prevfit , topics=c(1:20))
topics   #shows topics with highest probability words

#exploring the topics in context.  Provides an example of the text 
help("findThoughts")

z <- g1_t$title
z <- z[1:13444]
length(z)

findThoughts(prevfit, texts = z,  topics = 10,  n = 2)

help("plot.STM")
plot.STM(prevfit, type="summary")
#plot.STM(prevfit, type="labels", topics=c())
plot.STM(prevfit, type="perspectives", topics = c(19, 2))

# to aid on assigment of labels & intepretation of topics
help(topicCorr)
mod.out.corr <- topicCorr(prevfit)  #Estimates a graph of topic correlations

#install.packages("igraph")
library(igraph)

plot(mod.out.corr)

############################
## Rest in other files
##########################
