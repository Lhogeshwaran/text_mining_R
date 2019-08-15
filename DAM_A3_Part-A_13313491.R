# Author: Lhogeshwaran Purushothaman
# Student ID: 13313491
# Subject : 36106 Data, Algorithms and Meaning
# Assignment 3, Part A: Analysis and interpretation of unstructured data - Individual

# Clear environment and setup workspace
rm(list=ls())
dev.off()
setwd("/Users/loki/UTS_MDSI/Sem1/DAM/Assignment_3") 
getwd()
library(tm) 
library(SnowballC)

# Load corpus
docs <- VCorpus(DirSource("docs"))

# Check details of docs
print(docs)
class(docs)
# Volatile corpus consisting of 41 documents

# Examine contents
docs[1]
class(docs[1])

class(docs[[1]])
docs[[1]]$meta
docs[[1]]$content

## Data processing

# Inspect few documents manually
writeLines(as.character(docs[[1]]))
writeLines(as.character(docs[[4]]))
writeLines(as.character(docs[[15]]))

# Process items like : world-views, rational/specific 
# Create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "/")

getTransformations()
# Remove punctuation - replace punctuation marks with " "
docs <- tm_map(docs, removePunctuation)
# Remove non-standard punctuations
docs <- tm_map(docs, toSpace, "’")
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, " - ")

# Transform to lower case
docs <- tm_map(docs,content_transformer(tolower))
# Strip digits
docs <- tm_map(docs, removeNumbers)
# Remove stopwords from standard stopword list 
docs <- tm_map(docs, removeWords, stopwords("english"))
# Strip whitespace (cosmetic?)
docs <- tm_map(docs, stripWhitespace)
# Stem document
docs <- tm_map(docs,stemDocument)
# Inspect output
writeLines(as.character(docs[[1]]))

# Final cleaning
docs <- tm_map(docs, content_transformer(gsub), pattern = "organiz", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "organis", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "assumpt", replacement = "assum")
docs <- tm_map(docs, content_transformer(gsub), pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub), pattern = "analysi", replacement = "analys")
docs <- tm_map(docs, content_transformer(gsub), pattern = "analyst", replacement = "analys")
docs <- tm_map(docs, content_transformer(gsub), pattern = "analyt", replacement = "analys")
docs <- tm_map(docs, content_transformer(gsub), pattern = "analyz", replacement = "analys")
docs <- tm_map(docs, content_transformer(gsub), pattern = "argu", replacement = "argument")
docs <- tm_map(docs, content_transformer(gsub), pattern = "appli", replacement = "applic")
docs <- tm_map(docs, content_transformer(gsub), pattern = "document", replacement = "docs")
docs <- tm_map(docs, content_transformer(gsub), pattern = "document", replacement = "docs")
myStopwords <- c("will", "make", "well", "like", "best", "take", 
                 "need", "also", "often", "said", "look", "much", 
                 "within", "even", "although", "done", "anoth", "given",
                 "occur", "shown", "know", "made", "mention", "just", "want",
                 "consid", "jack", "other",
                 "also","howev","tell","will",
                 "much","need","take","tend","even",
                 "like","particular","rather","said",
                 "well","make","first","help","often",
                 "might","someth","thing","point",
                 "post","look","right","think",
                 "anoth","good",
                 "want","sure","kind","larg",
                 "quit","sinc","attempt","lack","seen","awar",
                 "littl","ever","moreov","though","found",
                 "enough","earli","away","achiev","draw",
                 "last","never","brief","bit","entir","brief")
docs <- tm_map(docs, removeWords, myStopwords)

# End of preprocessing

## Text analysis 
# Create DTM
# DTM is the starting point for quantitative text analysis
# Words with lesser than 4 and greater than 20 alphabets removed
dtm <- DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20)))
dtm
inspect(dtm[1:10,1000:1006]) # Inspect segment of DTM
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq,decreasing=TRUE) # Sort in ascending order of frequency of words
freq[head(ord, 10)] # Most frequently occuring words
freq[tail(ord, 100)] # Least frequently occuring words
#write.csv(file = "freq.csv",freq[ord]) # Write to disk and inspect file

# List most frequent terms
# Let’s get a list of terms that occur at least a 80 times in the entire corpus
findFreqTerms(dtm,lowfreq = 80)
# Check correlations and cooccurance
findAssocs(dtm,"argumentment",0.7)
findAssocs(dtm,"model",0.7)

# Graphs 
library(ggplot2)
wf = data.frame(term = names(freq), occurrences = freq)
# Ordered by frequency
p <- ggplot(subset(wf, occurrences>175), aes(reorder(term,occurrences), occurrences)) + 
  geom_bar(stat="identity", color = "steelblue", fill = "steelblue") +
  geom_text(aes(label=occurrences), vjust=1.6, color="white", size=3.5) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Frequent words") + xlab("") + ylab("Occurences")
p

# Wordcloud
library(wordcloud)
library(RColorBrewer)
set.seed(47)
wordcloud(names(freq), freq, min.freq = 80 , colors = brewer.pal(8,"Accent"))
wordcloud(names(freq), freq, max.words = 40 , colors = brewer.pal(8,"Accent"))

## Clustering
m<-as.matrix(dtm)
#write.csv(m,file="dtmAsMatrix.csv") # Write to disk and inspect file
#compute distance between document vectors
d <- dist(m)

# K-means
library(cluster)
# Calculate cosine distance
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
cd <- 1-cs
kfit <- kmeans(cd, 4, nstart=100)
clusplot(as.matrix(cd), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster
#write clusters to csv file
#write.csv(kfit$cluster,file="KMClustGroups2_cosine.csv") # Write to disk and inspect file
#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm attempts to minimise)
kfit$withinss
#kmeans - optimal number of clusters
#approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:(length(docs)-1)
for (i in 2:(length(docs)-1)) wss[i] <- sum(kmeans(cd,centers=i,nstart=25)$withinss)
plot(2:(length(docs)-1), wss[2:(length(docs)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 
# elbow at 4 -- change 'centers' value and rerun

# Hierarchical clustering
library(tm)
# cosine distance
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
cd <- 1-cs
#run hierarchical clustering using cosine distance
groups <- hclust(cd,method="ward.D")
#plot, use hang to ensure that labels fall below tree
plot(groups, hang=-1)
#cut into 2 subtrees.
rect.hclust(groups,4)
hclusters_cosine <- cutree(groups,4)
#write.csv(hclusters_cosine,"hclusters_cosine.csv") # Write to disk and inspect file

## Topic modelling
library(topicmodels)
# The burn-in period is used to ensure that we start from a representative point. 
burnin <- 1000
# perform 2000 iterations (after burn-in)...
iter <- 2000
# take every 500th one for further use. This "thinning" is done to ensure that samples are not correlated.
thin <- 500
# 5 different, randomly chosen as starting points
nstart <- 5
# random integers as seed. 
seed <- list(47,72,63,123,647)
# take the best run (the one with the highest probability) as the result
best <- TRUE
#Number of topics
k <- 4
ldaOut <- LDA(dtm,k, method="Gibbs", control=
                list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
topics(ldaOut)
ldaOut.topics <-as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))
terms(ldaOut,8)
ldaOut.terms <- as.matrix(terms(ldaOut,8))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))
topicProbabilities <- as.data.frame(ldaOut@gamma) 
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))

## Network graphs
# Map filenames to matrix row numbers these numbers will be used to reference files in the network graph
library(igraph)
filekey <- cbind(1:length(docs),rownames(m))
rownames(m) <- 1:length(docs)
#compute cosine similarity between document vectors
#converting to distance matrix sets diagonal elements to 0
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
#adjacency matrix: set entries below a certain threshold to 0.
#We choose half the magnitude of the largest element of the matrix as the cutoff. This is an arbitrary choice
cs[cs < max(cs)/2] <- 0
cs <- round(cs,3)
# build a graph from the above matrix
#mode is undirected because similarity is a bidirectional relationship
g <- graph.adjacency(as.matrix(cs), weighted=T, mode = "undirected")
# Plot a Graph
set.seed(47)
# Community detection - Fast/Greedy
comm_fg <- fastgreedy.community(g)
comm_fg$membership
V(g)$color <- comm_fg$membership
plot(g, layout=layout.kamada.kawai)
community_mapping <- cbind(as.data.frame(filekey, row.names = F),comm_fg$membership)
community_mapping

