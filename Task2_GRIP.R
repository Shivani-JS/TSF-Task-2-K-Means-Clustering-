


## Prediction using Unsurpervised ML.

## Submitted By:- Shivani Jadhav

## Aim is to predict the optimum number of Clusters using 
## KMeans Clustering and displaying the Clusters Graphically.

## Kmeans Clustering:-
## Partitioning the dataset of "n" objects in "K" clusters 
## is called Clustering.

## Kmeans algorithm computes "K" clusters with specific centroid 
## which is the mean point of that Cluster.

## The following codes makes use of WSS (Within group Sum of Squares)
## method to determine the optimum number of clusters.

#########################################################################################
## Installing Packages
install.packages("ggfortify")



## Loading Libraries
library(stats)        ##Library for statistical Functioning
library(ggplot2)      ##Library for plotting Graphs
library(ggfortify)


########################################################################################


## Loading the data into Dataframe object
data<-read.csv("Iris.csv") 
View(data)

## To display summary of the data
summary(data)




#Since this is Unsupervised Learning, we require Unlabelled Data 
#We therefore select only the independent Variables

New_data= data[,c(2,3,4,5)]
str(New_data)
View(New_data)



########################################################################################



#To choose optimum number of Clusters
#Using Elbow Method to determine the number of Clusters

#WSS (Within group Sum of Squares) Plot function

wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab ="Number of Clusters",
       ylab = "WSS")

}



# Applying the Function to our Dataset
wssplot(New_data)

## Spot knink (Elbow shape) at n=3
## Therefore we take number of clusters = 3

############################################################################################


## KMeans Model Implementation
Kmeans_model <- kmeans(New_data,3)


## Generating the Cluster Plot
#help("autoplot")
autoplot(Kmeans_model,New_data,frame=TRUE)

## Displaying Cluster centers
Kmeans_model$centers


## Therefore the optimum number of Clusters for Iris data 
## has been predicted using Elbow Method 
## and representing the clusters Graphically.


                  #### Task-2 Completed #### 

###############################################################################################
