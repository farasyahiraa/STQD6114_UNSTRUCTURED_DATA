## MONDAY 5/5/2025 ###

### Text Analysis I: LDA
library(topicmodels)
library(tidytext)
library(tidyr)
library(ggplot2)
library(dplyr)

data("AssociatedPress")

ap_lda<-LDA(AssociatedPress,k=2,control=list(seed=1234)) #create two-topic LDA model

ap_topics<-tidy(ap_lda,matrix="beta") #Extract the per-topic-per-word-probabilities

#Find terms that are most common within each topics 
ap_top_terms <- ap_topics %>% group_by(topic) %>% top_n(10,beta) %>% ungroup () %>% arrange (topic, -beta)

ap_top_terms%>% mutate(term=reorder(term,beta))%>%
  ggplot(aes(term,beta,fill=factor(topic)))+geom_col(show.legend=FALSE)+
  facet_wrap(~topic,scales="free")+coord_flip() #visualize the above

beta_spread <- ap_topics %>% mutate (topic=paste0("topic",topic)) %>% spread(topic,beta) %>%
  filter (topic1>0.002 | topic2 > 0.002) %>% mutate(log_ratio = log2(topic2/topic1))

beta_spread <- ap_topics %>% mutate (topic=paste0("topic",topic)) %>% spread(topic,beta) %>%
  filter (topic2>0.003 | topic3 > 0.003) %>% mutate(log_ratio = log2(topic3/topic2))
beta_spread%>% mutate(term=reorder(term,log_ratio))%>%
  ggplot(aes(term,log_ratio))+geom_col(show.legend=FALSE)+coord_flip()

ap_documents<-tidy(ap_lda,matrix="gamma") #Extract the per-document-per-topic-probabilities
ap_documents %>% subset(document=2000)

tidy(AssociatedPress)%>%filter(document==6)%>%arrange(desc(count)) #Check the most common words in the document, eg document 6

## 
ap_documents %>% slice

mytext<-DirSource("movies")
mycorpus<-VCorpus(mytext)
inspect(mycorpus)
as.character(mycorpus[[1]])

getwd()

#Create custom transformation
toSpace<-content_transformer(function(x,pattern){return(gsub(pattern," ",x))})

as.character(docs[[133]]) #check line 133 
docs<-tm_map(docs,toSpace,"-")
as.character(docs[[133]])

docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removeWords,stopwords("english")) #remove stop words

as.character(docs[[2]])

docs<-tm_map(docs,removeWords,"gp") #c("gp","sheet")
docs<-tm_map(docs,removeWords,c("gp","sheet")) #multiple words to remove
docs<-tm_map(docs,stripWhitespace) #whitespace ni buat last
as.character(docs[[2]])
             
### TEXT DATA ANALYSIS- PART2, CLUSTER ANALYSIS
# to group, to categorize. Element that are similar should be clustered together
# elements are evaluated on the similarity/differences index
# clustering group num depend on k value we set
library(tm)
mytext<-DirSource("TextMining")
docs<-VCorpus(mytext)
docs <- tm_map(docs,content_transformer(tolower)) 
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs1 <- tm_map(docs, toSpace, "-")
#docs2 <- tm_map(docs1, toSpace, ":")
#docs3 <- tm_map(docs2, toSpace, "'")
#docs4 <- tm_map(docs3, toSpace, ".")
#docs5 <- tm_map(docs4, toSpace, ". ")
#docs6 <- tm_map(docs5, toSpace, " -") ## STEP DOC2-6 are skipped 
docs2 <- tm_map(docs1, removePunctuation) #remove punctuation
docs3 <- tm_map(docs2, removeNumbers) #Strip digits
docs4 <- tm_map(docs3, removeWords, stopwords("english")) #remove stopwords
docs5 <- tm_map(docs4, stripWhitespace) #remove whitespace

tdm <- DocumentTermMatrix(docs5) #Create document term matrix
tdm

#Present text data numerically, weighted TF-IDF
tdm.tfidf <- weightTfIdf(tdm)
tdm.tfidf <- removeSparseTerms(tdm.tfidf, 0.999)
tfidf.matrix <- as.matrix(tdm.tfidf)

# Cosine distance matrix (useful for specific clustering algorithms) 
install.packages("proxy")
library(proxy)
# dist.matrix <- dist(tfidf.matrix, method = "cosine") #from 
dist.matrix <- proxy::dist(tfidf.matrix, method = "cosine")

truth.K=3 #k seed
#Perform clustering
install.packages("dbscan")
library(dbscan)
clustering.kmeans <- kmeans(tfidf.matrix, truth.K)
str(tfidf.matrix)
clustering.kmeans <- kmeans(tfidf.matrix, centers = truth.K)
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
clustering.dbscan <- hdbscan(dist.matrix, minPts= 10) #noise will be detected here

library(cluster)
clusplot(as.matrix(dist.matrix),clustering.kmeans$cluster,color=T,shade=T,labels=2,lines=0)
plot(clustering.hierarchical)
rect.hclust(clustering.hierarchical,3)
plot(as.matrix(dist.matrix),col=clustering.dbscan$cluster+1L)

#Combine results
master.cluster <- clustering.kmeans$cluster

#plotting results
library(colorspace)
points <- cmdscale(dist.matrix, k = 3)
palette <- diverge_hcl(truth.K) # Creating a color palette, need library(colorspace)
#layout(matrix(1:3,ncol=1))
slave.hierarchical <- cutree(clustering.hierarchical, k = truth.K)
slave.dbscan <- clustering.dbscan$cluster

plot(points, main = 'K-Means clustering', 
     col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
plot(points, main = 'Hierarchical clustering', col 
     = as.factor(slave.hierarchical), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Density-based clustering', 
     col = as.factor(slave.dbscan), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 

table(master.cluster)
table(slave.hierarchical)
table(slave.dbscan)

#Elbow plot
#accumulator for cost results
cost_df <- data.frame()

#run kmeans for all clusters up to 100
for(i in 1:20){#Run kmeans for each level of i, allowing up to 100 iterations for convergence
  kmeans<- kmeans(x=tfidf.matrix, centers=i, iter.max=100)
  #Combine cluster number and cost together, write to df
  cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
}
names(cost_df) <- c("cluster", "cost")
plot(cost_df$cluster, cost_df$cost)
lines(cost_df$cluster, cost_df$cost)



##### FINAL 30% open book
#text data analysis
#LDA, model data analysis, SENTIMENT analysis, clustering
#image & audio not included
#2 part (part A explanation) (part b only text data analysis)