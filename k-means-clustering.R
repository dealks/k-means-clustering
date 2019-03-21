library(ISLR)
library(ggplot2)
library(cluster)
library(dplyr)

############################FIRST PASS - CUSTOMERS WITH PURCHASE IN LAST YEAR#############################
df <- read.csv('cluster_input_last_year.csv', sep=',')
head(df)

df2 <- select(df, 'COMMON_CUST_NUM','SUM_SALES', 'NUM_TRANS', 'MAJOR_PURCH_METHOD',
              'RECENCY', 'PURCH_CYCLE', 'HDAY_TRANS_PER_ELIG_HDAY', 'SPEND_PER_TRANS', 'TOT_BOXES', 'SPEND_PER_BOX')

head(df2)

summary(df2)
str(df2)

#remove outliers

df2_cleaned <- subset(df2,c(NUM_TRANS<640 & SUM_SALES<156000 & SUM_SALES>0 
                            & PURCH_CYCLE<550 & HDAY_TRANS_PER_ELIG_HDAY<=2.25 & SPEND_PER_TRANS<570 & TOT_BOXES<2950
                            & SPEND_PER_BOX<120))

ggplot(df2_cleaned, aes(x=SUM_SALES)) + geom_histogram(color="black", fill="white", bins=20)
ggplot(df2_cleaned, aes(x=NUM_TRANS)) + geom_histogram(color="black", fill="white", bins=20)
ggplot(df2_cleaned, aes(x=MAJOR_PURCH_METHOD)) + geom_histogram(color="black", fill="white", bins=20)
ggplot(df2_cleaned, aes(x=RECENCY)) + geom_histogram(color="black", fill="white", bins=20)
ggplot(df2_cleaned, aes(x=PURCH_CYCLE)) + geom_histogram(color="black", fill="white", bins=20)
ggplot(df2_cleaned, aes(x=HDAY_TRANS_PER_ELIG_HDAY)) + geom_histogram(color="black", fill="white", bins=20)
ggplot(df2_cleaned, aes(x=SPEND_PER_TRANS)) + geom_histogram(color="black", fill="white", bins=20)
ggplot(df2_cleaned, aes(x=TOT_BOXES)) + geom_histogram(color="black", fill="white", bins=20)
ggplot(df2_cleaned, aes(x=SPEND_PER_BOX)) + geom_histogram(color="black", fill="white", bins=20)

str(df2_cleaned)

#elbow plot
set.seed(123)

# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
df2.scaled <- scale(df2_cleaned[, -8])
data <- df2.scaled
head(data)
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10 )$tot.withinss})

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)

#clusters
head(df2_cleaned)
clusters <- kmeans(df2_cleaned[,2:10],6, nstart=50)
#help(hclust)

print(clusters)

clusplot(df2_cleaned,clusters$cluster, color=T, shade=T, labels=0, lines=0)

df2_cleaned$cluster <- clusters$cluster
head(df2_cleaned)


#check cluster features
pl <- ggplot(df2_cleaned, aes(cluster,SUM_SALES))
print(pl + geom_point(size=4))

pl <- ggplot(df2_cleaned, aes(cluster,NUM_TRANS))
print(pl + geom_point(size=4))

pl <- ggplot(df2_cleaned, aes(cluster,MAJOR_PURCH_METHOD))
print(pl + geom_point(size=4))

pl <- ggplot(df2_cleaned, aes(cluster,RECENCY))
print(pl + geom_point(size=4))

pl <- ggplot(df2_cleaned, aes(cluster,PURCH_CYCLE))
print(pl + geom_point(size=4))

pl <- ggplot(df2_cleaned, aes(cluster,HDAY_TRANS_PER_ELIG_HDAY))
print(pl + geom_point(size=4))

pl <- ggplot(df2_cleaned, aes(cluster,SPEND_PER_TRANS))
print(pl + geom_point(size=4))

pl <- ggplot(df2_cleaned, aes(cluster,TOT_BOXES))
print(pl + geom_point(size=4))

pl <- ggplot(df2_cleaned, aes(cluster,SPEND_PER_BOX))
print(pl + geom_point(size=4))

#WRITE OUTPUT FILE
write.csv(df2_cleaned, file = "CRM_clusters.csv",row.names=FALSE)