

# Analyze customer reviews

reviews <- read_csv("Data Files/reviews.csv", 
                    col_types = cols(date = col_date(format = "%Y-%M-%d")))

# Check the structure of the file
str(reviews)

# Create Corpus to translate value labels to a format for text analysis
corpus <- iconv(reviews$title)
corpus <- Corpus(VectorSource(corpus))

# To see the corpus
inspect(corpus[1:5])

# Cleaning corpus
corpus <- tm_map(corpus, tolower)
#inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
#inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
#inspect(corpus[1:5])

corpus <- tm_map(corpus, removeWords, stopwords("english"))
# inspect(corpus[1:5])

corpus <- tm_map(corpus, removeWords, c("tracker", "leaf", "bellabeat"))

reviews_final <- corpus

# Term document
tdm <- TermDocumentMatrix(reviews_final)
tdm <- as.matrix(tdm)
tdm[1:20, 1:5]

# Bar plot of words
wordplot <- rowSums(tdm)
wordplot <- subset(wordplot, wordplot>=25)
barplot(wordplot, las = 2, col = "blue")

# Create wordcloud
wc <- sort(rowSums(tdm), decreasing = T)
set.seed(2000)
wordcloud(words = names(wc),
          freq = wc,
          max.words = 50,
          random.order = T,
          min.freq = 5,
          colors = brewer.pal(8, "Dark2"),
          scale = c(5, 0.5))

# Sentiment scores
sentiment_data <- iconv(reviews$title)
s <- get_nrc_sentiment(sentiment_data)
s[1:10,]

# Calculate review wise score
s$score <- s$positive - s$negative
s[1:10,]

# Save scores into csv file
write_csv(s, "final_score_title.csv")

# Check overall product sentiment

review_score <- colSums(s[,])
print(review_score)

barplot(colSums(s),
        las = 2,
        col = rainbow(11),
        ylab = "Count",
        main = "Sentiment")
