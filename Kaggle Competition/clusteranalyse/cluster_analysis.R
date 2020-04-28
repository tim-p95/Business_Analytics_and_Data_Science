# find numeric variables
idx_numeric <- sapply(known1, is.numeric)
known1_numeric <- known1[,idx_numeric]

# Filter the variables using a temporary anonymous/"lambda" function
check_not_dummy <- function(x) !(min(x)==0 & max(x)==1) 
idx_continuous <- sapply(known1_numeric, function(x)!(min(x)==0 & max(x)==1))
known1_continuous <- known1_numeric[,idx_continuous]

#edit continuous variables
known1_continuous = known1_continuous[, c(1,4,5,6,7,8)]


## Standardize numeric variables
known1_norm <- lapply(known1_continuous, function(x) (x-mean(x))/sd(x)  )

#select relevant variables
known1_norm <- data.frame(known1_norm)

## k-means Clustering
set.seed(123)

# Cluster the observations based on their numeric features into 3 groups
# The maximum number of iterations is set to 50. The algorithm will stop after the 50th iteration, even if the solution is not stable
cluster.object <- kmeans(known1_norm, centers = 3, iter.max = 50, nstart = 25)

# The result is a list describing the cluster solution
str(cluster.object)
clusters <- cluster.object$cluster

# Let's give it a quick look, on two important variables
library(ggplot2)
temp <- cbind(known1_norm, "cluster"= factor(clusters))
cluster_plot <- ggplot(temp, aes(x=item_price, y=user_yob, color=cluster)) + geom_point(alpha=0.5)
plot(cluster_plot)

# We can also check what the 'typical' cluster member would look like
cluster_means <- data.frame(cluster.object$centers)
cluster_means
cluster_means$cluster <- factor(c(1,2,3))

# Add the cluster centroids to the exising plot
cluster_plot + geom_point(data = cluster_means, aes(x=item_price, y=user_yob, color=cluster), shape=18, size=4)

# Test different numbers of clusters ####
# Define a vector with candidate settings for k
k.settings = 2:15
# Define another variable to store the results. Here, we have two options:
# i)  only store the objective values corresponding to individual settings of k, or
# ii) store the complete cluster solutions corresponding to individual settings of k
# For option i) we need a vector that can store numeric data. For ii) we need to store a set of objects
obj.values = vector(mode="numeric", length = length(k.settings)) # results for i)
cluster.models = vector(mode = "list", length = length(k.settings)) # results for ii)

# Note that it is always a good practice to first initialize a vector, before assigning some data to it (although R does allow you to create variables on-the-fly)
for (i in 1:length(k.settings)) {
  # Create a cluster solution using the current setting of k
  clu.sol <- kmeans(known1_norm, centers=k.settings[i], iter.max = 50, nstart = 100)
  # Option i) we only store the objective value
  obj.values[i] <- clu.sol$tot.withinss
  # Option ii) we store the full cluster model (i.e., object)
  cluster.models[[i]] <- clu.sol 
  # Remark: it is not straightforward to see why we must use [[ ]] in the above line. 
  # It has to do with the fact that cluster.models stores objects of type list and that clu.sol is also of type list. So we are dealing with lists of lists. 
  # Using [ ] to index a list will return a list (in this case with only one element), using [[ ]] will return only the content of the indexed element
}


qplot(x= k.settings,y=obj.values,geom=c("line","point"),xlab = "k", ylab="Total within-cluster SS",
      main = "Elbow curve for k selection", color="red") + guides(color=FALSE)
#qplot (quickplot) is a wrapper for ggplot, an alternative plotting system for R. Its structure is very similar to plot(), but its running a ggplot() function instead. qplot also automatically adds a legend which is diabled by adding "+guides(color=False)".
#The following code represents how to plot the same graph with ggplot itself. ggplot requires its data to be present as a dataframe which is why there is the additional step of creating a dataframe.
df <- data.frame(k.settings, obj.values)
ggplot(df, aes(k.settings,obj.values))+geom_line(color = "red") + geom_point(color="red")  + xlab("k") + ylab("Total within-cluster SS") + ggtitle("Elbow curve for k selection")



# The above approach goes beyond what is needed to solve the exercise. The advantage is that you store all information about a cluster solution, not
# only the objective; as in option i)

## Last, here is an "elegant R-like way" to avoid the loop using apply. ####
# It is an alternative to the above approach with for and gives the same result. 

# This is the alternative to option i) 
my.kMeans <- function(k) {  
  clu.sol <- kmeans(loans_norm, centers=k) 
  return(clu.sol$tot.withinss)
}
obj.values <- sapply(k.settings, my.kMeans)

# This is the alternative to option ii) 
cluster.models <- lapply(k.settings, function (k) kmeans(loans_norm, centers=k))
# Note that many R programmers do often not bother to first create a user-defined
# function and put it directly into the call to apply, as is shown above. This works
# well if the user-defined function consists of a single statement (also as above).
# It is clearly not a very readable way to write code, but, as said, is very common
# in the R community

#cumpute final cluster solution
clu.solution <- kmeans(known1_norm, centers=7, iter.max = 50, nstart = 100)

str(clu.solution)
clusters_known <- clu.solution$cluster

# Let's give it a quick look, on two important variables
library(ggplot2)
temp <- cbind(known1_norm, "cluster"= factor(clusters_known))
cluster_plot <- ggplot(temp, aes(x=item_price, y=user_yob, color=cluster)) + geom_point(alpha=0.5)
plot(cluster_plot)

# We can also check what the 'typical' cluster member would look like
cluster_means <- data.frame(cluster.object$centers)
cluster_means
cluster_means$cluster <- factor(c(1:3))

# Add the cluster centroids to the exising plot
cluster_plot + geom_point(data = cluster_means, aes(x=item_price, y=user_yob, color=cluster), shape=18, size=4)


###same for unknown
idx_numeric <- sapply(unknown1, is.numeric)
unknown1_numeric <- unknown1[,idx_numeric]

# Filter the variables using a temporary anonymous/"lambda" function
check_not_dummy <- function(x) !(min(x)==0 & max(x)==1) 
idx_continuous <- sapply(unknown1_numeric, function(x)!(min(x)==0 & max(x)==1))
unknown1_continuous <- unknown1_numeric[,idx_continuous]

#edit continuous variables
unknown1_continuous = unknown1_continuous[, c(1,4,5,6,7,8)]


## Standardize numeric variables
unknown1_norm <- lapply(unknown1_continuous, function(x) (x-mean(x))/sd(x)  )

#select relevant variables
unknown1_norm <- data.frame(unknown1_norm)


#check best k setting
k.settings_unknown = 2:15

for (i in 1:length(k.settings_unknown)) {
  # Create a cluster solution using the current setting of k
  clu.sol <- kmeans(unknown1_norm, centers=k.settings_unknown[i], iter.max = 50, nstart = 100)
  # Option i) we only store the objective value
  obj.values[i] <- clu.sol$tot.withinss
  # Option ii) we store the full cluster model (i.e., object)
  cluster.models[[i]] <- clu.sol 
  # Remark: it is not straightforward to see why we must use [[ ]] in the above line. 
  # It has to do with the fact that cluster.models stores objects of type list and that clu.sol is also of type list. So we are dealing with lists of lists. 
  # Using [ ] to index a list will return a list (in this case with only one element), using [[ ]] will return only the content of the indexed element
}

qplot(x= k.settings,y=obj.values,geom=c("line","point"),xlab = "k", ylab="Total within-cluster SS",
      main = "Elbow curve for k selection", color="red") + guides(color=FALSE)
#qplot (quickplot) is a wrapper for ggplot, an alternative plotting system for R. Its structure is very similar to plot(), but its running a ggplot() function instead. qplot also automatically adds a legend which is diabled by adding "+guides(color=False)".
#The following code represents how to plot the same graph with ggplot itself. ggplot requires its data to be present as a dataframe which is why there is the additional step of creating a dataframe.
df <- data.frame(k.settings, obj.values)
ggplot(df, aes(k.settings,obj.values))+geom_line(color = "red") + geom_point(color="red")  + xlab("k") + ylab("Total within-cluster SS") + ggtitle("Elbow curve for k selection")


## k-means Clustering
set.seed(123)

clu.solution <- kmeans(unknown1_norm, centers=7, iter.max = 50, nstart = 100)

str(clu.solution)
clusters_unknown <- clu.solution$cluster

# Let's give it a quick look, on two important variables
library(ggplot2)
temp <- cbind(unknown1_norm, "cluster"= factor(clusters_unknown))
cluster_plot <- ggplot(temp, aes(x=item_price, y=user_yob, color=cluster)) + geom_point(alpha=0.5)
plot(cluster_plot)

# Add the cluster centroids to the exising plot
cluster_plot + geom_point(data = cluster_means, aes(x=item_price, y=user_yob, color=cluster), shape=18, size=4)


#put normalized data together
known_unknown_norm = rbind(known1_norm, unknown1_norm)

#make cluster solution for both data sets together
set.seed(123)

clu.solution <- kmeans(known_unknown_norm, centers=7, iter.max = 50, nstart = 100)

str(clu.solution)
clusters_known_unknown <- clu.solution$cluster

# Let's give it a quick look, on two important variables
library(ggplot2)
temp <- cbind(known_unknown_norm, "cluster"= factor(clusters_known_unknown))
cluster_plot <- ggplot(temp, aes(x=item_price, y=user_yob, color=cluster)) + geom_point(alpha=0.5)
plot(cluster_plot)

# Add the cluster centroids to the exising plot
cluster_plot + geom_point(data = cluster_means, aes(x=item_price, y=user_yob, color=cluster), shape=18, size=4)


#add clusters to known1
known1$cluster = clusters_known_unknown[1:100000]
unknown1$cluster = clusters_known_unknown[100001:150000]

head(known1)
head(clusters_known_unknown)
