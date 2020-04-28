#combine all observations
colnames(known1)
colnames(unknown1)
all = rbind(known1[, 2:25], unknown1)

# find numeric variables
idx_numeric <- sapply(all, is.numeric)
all_numeric <- all[,idx_numeric]

all_numeric$delivery_date_missing = NULL

# Filter the variables using a temporary anonymous/"lambda" function
check_not_dummy <- function(x) !(min(x)==0 & max(x)==1) 
idx_continuous <- sapply(all_numeric, function(x)!(min(x)==0 & max(x)==1))
all_continuous <- all_numeric[,idx_continuous]

all_continuous$brand_id = NULL
all_continuous$item_id = NULL

## Standardize numeric variables
all_norm <- lapply(all_continuous, function(x) (x-mean(x))/sd(x)  )

#select relevant variables
all_norm <- data.frame(all_norm)

item_norm = all_norm[, c(1, 3:5)]
user_norm = all_norm[, c(1, 2, 6)]

## k-means Clustering
set.seed(123)

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
  clu.sol <- kmeans(item_norm, centers=k.settings[i], iter.max = 50, nstart = 100)
  # Option i) we only store the objective value
  obj.values[i] <- clu.sol$tot.withinss
  # Option ii) we store the full cluster model (i.e., object)
  cluster.models[[i]] <- clu.sol 
  # Remark: it is not straightforward to see why we must use [[ ]] in the above line. 
  # It has to do with the fact that cluster.models stores objects of type list and that clu.sol is also of type list. So we are dealing with lists of lists. 
  # Using [ ] to index a list will return a list (in this case with only one element), using [[ ]] will return only the content of the indexed element
}

library(ggplot2)
qplot(x= k.settings,y=obj.values,geom=c("line","point"),xlab = "k", ylab="Total within-cluster SS",
      main = "Elbow curve for k selection", color="red") + guides(color=FALSE)
#qplot (quickplot) is a wrapper for ggplot, an alternative plotting system for R. Its structure is very similar to plot(), but its running a ggplot() function instead. qplot also automatically adds a legend which is diabled by adding "+guides(color=False)".
#The following code represents how to plot the same graph with ggplot itself. ggplot requires its data to be present as a dataframe which is why there is the additional step of creating a dataframe.
df <- data.frame(k.settings, obj.values)
ggplot(df, aes(k.settings,obj.values))+geom_line(color = "red") + geom_point(color="red")  + xlab("k") + ylab("Total within-cluster SS") + ggtitle("Elbow curve for k selection")

#cumpute final cluster solution
clu.solution <- kmeans(item_norm, centers=4, iter.max = 50, nstart = 100)

str(clu.solution)
item_cluster = clu.solution$cluster

# Let's give it a quick look, on two important variables
library(ggplot2)
temp <- cbind(item_norm, "cluster"= factor(item_cluster))
cluster_plot <- ggplot(temp, aes(x=item_price, y=item_id_freq, color=cluster)) + geom_point(alpha=0.5)
plot(cluster_plot)

known1$item_cluster = item_cluster[1:100000]
unknown1$item_cluster = item_cluster[100001:150000]


##########################################
#same for user cluster
##########################################
## k-means Clustering
set.seed(123)

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
  clu.sol <- kmeans(user_norm, centers=k.settings[i], iter.max = 50, nstart = 100)
  # Option i) we only store the objective value
  obj.values[i] <- clu.sol$tot.withinss
  # Option ii) we store the full cluster model (i.e., object)
  cluster.models[[i]] <- clu.sol 
  # Remark: it is not straightforward to see why we must use [[ ]] in the above line. 
  # It has to do with the fact that cluster.models stores objects of type list and that clu.sol is also of type list. So we are dealing with lists of lists. 
  # Using [ ] to index a list will return a list (in this case with only one element), using [[ ]] will return only the content of the indexed element
}

library(ggplot2)

qplot(x= k.settings,y=obj.values,geom=c("line","point"),xlab = "k", ylab="Total within-cluster SS",
      main = "Elbow curve for k selection", color="red") + guides(color=FALSE)
#qplot (quickplot) is a wrapper for ggplot, an alternative plotting system for R. Its structure is very similar to plot(), but its running a ggplot() function instead. qplot also automatically adds a legend which is diabled by adding "+guides(color=False)".
#The following code represents how to plot the same graph with ggplot itself. ggplot requires its data to be present as a dataframe which is why there is the additional step of creating a dataframe.
df <- data.frame(k.settings, obj.values)
ggplot(df, aes(k.settings,obj.values))+geom_line(color = "red") + geom_point(color="red")  + xlab("k") + ylab("Total within-cluster SS") + ggtitle("Elbow curve for k selection")

#cumpute final cluster solution
clu.solution <- kmeans(user_norm, centers=3, iter.max = 50, nstart = 100)

str(clu.solution)
user_cluster <- clu.solution$cluster

# Let's give it a quick look, on two important variables
library(ggplot2)
temp <- cbind(item_norm, "cluster"= factor(item_cluster))
cluster_plot <- ggplot(temp, aes(x=item_price, y=item_id_freq, color=cluster)) + geom_point(alpha=0.5)
plot(cluster_plot)

# We can also check what the 'typical' cluster member would look like
cluster_means <- data.frame(cluster.object$centers)
cluster_means
cluster_means$cluster <- factor(c(1:3))

# Add the cluster centroids to the exising plot
cluster_plot + geom_point(data = cluster_means, aes(x=item_price, y=user_yob, color=cluster), shape=18, size=4)


known1$user_cluster = user_cluster[1:100000]
unknown1$user_cluster = user_cluster[100001:150000]
