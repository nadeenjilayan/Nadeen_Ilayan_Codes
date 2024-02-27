# Author: Nadeen Ilayan
# Date: Feb. 5, 2024
# Purpose: Analyzing Diversity and Bias in University Ambassador Profiles

#libraries
library(ggplot2)
library(ggthemes)
library(tm)
library(topicmodels)
library(wordcloud)
library(syuzhet)
library(dplyr)

# Working Directory
setwd("C:\\Users\\nadee\\OneDrive\\Desktop\\Hult_Visualizing_Analyzing_Data_with_R\\personalFiles")

# Custom function to convert text to lowercase safely
tryTolower <- function(x) {
  try_error <- tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error')) tolower(x) else NA
}

# Function to clean and preprocess a text corpus
cleanCorpus <- function(corpus, customStopwords) {
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus
}

# Define custom stopwords
customStopwords <- tolower(c(stopwords("en"), "hult", "university", "business"))

# Load student bios data
studentBios <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing_Analyzing_Data_with_R/main/Cases/A3_NLP/Student%20Ambassador%20Bios/final_student_data.csv')

# Create and clean the corpus
studentCorpus <- VCorpus(VectorSource(studentBios$bio))
studentCorpus <- cleanCorpus(studentCorpus, customStopwords)

# Examine a sample record from the corpus
content(studentCorpus[[1]])

# Create a Document Term Matrix (DTM) and convert to matrix format
studentDTM <- DocumentTermMatrix(studentCorpus)
studentDTM <- as.matrix(studentDTM)

# Display DTM dimensions and class
dim(studentDTM)
class(studentDTM)

# Generate and examine word frequency matrix
studentFreq <- colSums(studentDTM)
studentFreq <- data.frame(word = names(studentFreq), frequency = studentFreq, row.names = NULL)
head(studentFreq, 10)

# Bar Plot of Words: 

# Filter words with frequency >= 25 and sort
topWords <- subset(studentFreq, frequency >= 25)
topWords <- topWords[order(topWords$frequency, decreasing = TRUE),]

# Set word factor with specific order
topWords$word <- factor(topWords$word, levels = unique(topWords$word))

# Generate bar plot for top 10 words
ggplot(topWords[1:10, ], aes(x = word, y = frequency)) +  
  geom_bar(stat = "identity", fill = "darkgrey") + 
  coord_flip() +  
  theme_minimal() +  
  theme(text = element_text(color = "black"),  
        axis.text = element_text(color = "black")) +  
  geom_text(aes(label = frequency), hjust = 1.25, size = 5.0)  

# Generate word cloud
wordcloud(words = studentFreq$word, freq = studentFreq$frequency, min.freq = 1,
          max.words = 100, random.order = FALSE, rot.per = 0.35,  # 35% of words will be rotated
          colors = c("slategray", "darkblue", "steelblue", "lightgray"))  # Darker color palette

# Sentiment Analysis:

# Extract text content from the corpus
corpus_content <- lapply(studentCorpus, function(corpus) as.character(content(corpus)))

# Convert the list to a character vector
corpus_content_vector <- unlist(corpus_content, use.names = FALSE)

# Verify the match between the number of bios and sentiment scores
if(length(corpus_content_vector) != nrow(studentBios)) {
  stop("Mismatch between the number of bios and sentiment scores.")
}

# Obtain sentiment scores using NRC sentiment lexicon
sentiments <- get_nrc_sentiment(corpus_content_vector)

# Combine sentiment scores with the original student bios data
studentBiosSentiment <- cbind(studentBios, sentiments)

# Calculate net sentiment score (positive - negative)
studentBiosSentiment$net_sentiment <- studentBiosSentiment$positive - studentBiosSentiment$negative

# Convert relevant columns to factors for visualization
studentBiosSentiment$programTitle <- factor(studentBiosSentiment$programTitle)
studentBiosSentiment$namSorGender.likelyGender <- factor(studentBiosSentiment$namSorGender.likelyGender)
studentBiosSentiment$campus <- factor(studentBiosSentiment$campus)
studentBiosSentiment$namSorCountry.country <- factor(studentBiosSentiment$namSorCountry.country)

# Visualize Sentiment by Program 
ggplot(studentBiosSentiment, aes(x = programTitle, y = net_sentiment, fill = programTitle)) +
  geom_bar(stat = "identity") +  
  coord_flip() +  
  theme_minimal() +  
  theme(text = element_text(color = "black"),  
        axis.text = element_text(color = "black"),  
        legend.title = element_text(color = "black"),  
        legend.text = element_text(color = "black")) +  
  scale_fill_manual(values = c("darkblue", "darkslategray", "midnightblue", "navy", "darkslateblue",
                               "steelblue4", "royalblue4", "slategray", "cadetblue4", "lightsteelblue4")) +
  labs(x = "Program Title", y = "Net Positive Sentiment", title = "Sentiment by Program") 

# Visualize Sentiment by Gender 
ggplot(studentBiosSentiment, aes(x = namSorGender.likelyGender, y = net_sentiment, fill = namSorGender.likelyGender)) +
  geom_bar(stat = "identity") +  
  coord_flip() +  
  theme_minimal() +  
  theme(text = element_text(color = "black"),  
        axis.text = element_text(color = "black"),  
        legend.title = element_text(color = "black"),  
        legend.text = element_text(color = "black")) +  
  scale_fill_manual(values = c("darkgray", "midnightblue")) +  
  labs(x = "Likely Gender", y = "Net Positive Sentiment", title = "Sentiment by Gender")  

# Visualize Sentiment by Campus 
ggplot(studentBiosSentiment, aes(x = campus, y = net_sentiment, fill = campus)) +
  geom_bar(stat = "identity") +  
  coord_flip() +  
  theme_minimal() +  
  theme(text = element_text(color = "black"),  
        axis.text = element_text(color = "black"),  
        legend.title = element_text(color = "black"),  
        legend.text = element_text(color = "black")) +  
  scale_fill_manual(values = c("midnightblue", "darkslategray", "darkolivegreen", "darkgrey")) +  
  labs(x = "Campus", y = "Net Positive Sentiment", title = "Sentiment by Campus") 

# Visualize Sentiment by Country 
ggplot(studentBiosSentiment, aes(x = reorder(namSorCountry.country, net_sentiment, FUN = mean), y = net_sentiment, fill = namSorCountry.country)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = rep(c("darkslategray", "midnightblue", "navy", "darkolivegreen", "darkcyan", "darkred", "darkmagenta", "darkgoldenrod", "darkgreen", "darkorange"), length.out = length(unique(studentBiosSentiment$namSorCountry.country)))) +
  labs(x = "Country", y = "Net Positive Sentiment", title = "Sentiment by Country")

# Topic Analysis:

# Convert the Document-Term Matrix (DTM) to the format required by LDA
dtm <- DocumentTermMatrix(studentCorpus)

# Define the number of topics 
num_topics <- 5

# Set seed for reproducibility
lda_result <- LDA(dtm, k = num_topics, control = list(seed = 1234))

# Assign the most probable topic to each document
topics <- as.matrix(topics(lda_result))

# Extract the top terms for each topic
topic_terms <- terms(lda_result, 6)  # Retrieving 6 terms per topic

# Convert the matrix of top terms to a data frame for better readability
topic_terms_df <- as.data.frame(topic_terms)

# Rename columns to reflect topic labels
names(topic_terms_df) <- paste0("Topic ", 1:ncol(topic_terms_df))

# Display the top terms associated with each topic
print(topic_terms_df)

# Topic Analysis and Sentiment Correlation:

# Obtain topic probabilities for each document from the LDA model
topic_probabilities <- posterior(lda_result)$topics

# Assign the most probable topic to each document in the dataset
studentBiosSentiment$topic <- apply(topic_probabilities, 1, which.max)

# Extract top terms for each topic from the LDA model
top_terms <- terms(lda_result, 6)  # Getting 6 top terms per topic

# Convert top terms per topic into a single string for readability
topic_terms_df <- apply(top_terms, 2, function(x) paste(x, collapse = ", "))

# Include the top terms for the most probable topic in each document
studentBiosSentiment$topic_terms <- topic_terms_df[studentBiosSentiment$topic]

# Analyzing Net Sentiment by Topic:

# Calculate the average net sentiment for each topic
sentiment_by_topic <- aggregate(net_sentiment ~ topic, data = studentBiosSentiment, mean)

# Combine average net sentiment with corresponding top terms for each topic
topic_sentiment <- merge(sentiment_by_topic, as.data.frame(topic_terms_df), by.x = "topic", by.y = "row.names")

# Rename columns for enhanced clarity
names(topic_sentiment) <- c("Topic", "Average Net Sentiment", "Top Terms")

# Display the combined data frame of topic sentiment and top terms
print(topic_sentiment)

# Visualize Topic Distribution across Campuses 
ggplot(studentBiosSentiment, aes(x = factor(topic), fill = campus)) +
  geom_bar(position = "dodge") +  # Use dodge position for side-by-side bars
  theme_minimal(base_family = "Arial") +  # Minimal theme for a clean look
  theme(plot.background = element_blank(),  # Remove plot background color
        panel.background = element_blank(),  # Remove panel background color for a cleaner look
        text = element_text(color = "black"),  # Use black text for better contrast on a light background
        axis.text = element_text(color = "black"),  # Use black axis text for readability
        axis.title = element_text(color = "black"),  # Use black axis titles
        legend.background = element_blank(),  # Remove legend background for consistency
        legend.text = element_text(color = "black")) +  # Use black legend text for readability
  scale_fill_manual(values = c("darkslategray", "midnightblue", "darkgrey", "darkolivegreen")) +  # Custom dark colors for campuses
  labs(x = "Topic", y = "Count", fill = "Campus", title = "Topic Distribution by Campus")

# Sample Size and Distribution Analysis:

# Initial sample size assessment
total_samples <- nrow(studentBiosSentiment)
london_samples <- nrow(subset(studentBiosSentiment, campus == "London"))
boston_samples <- nrow(subset(studentBiosSentiment, campus == "Boston"))
dubai_samples <- nrow(subset(studentBiosSentiment, campus == "Dubai"))
sf_samples <- nrow(subset(studentBiosSentiment, campus == "San Francisco"))

# Visualizing Net Sentiment Distribution
hist(studentBiosSentiment$net_sentiment, breaks=30, col="darkgray", border="white",
     main="Histogram of Net Sentiment", xlab="Net Sentiment")
plot(density(studentBiosSentiment$net_sentiment), main="Density Plot of Net Sentiment",
     xlab="Net Sentiment", col="darkblue", lwd=2)

# END