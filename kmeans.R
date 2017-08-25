rm(list=ls())
setwd("~/Desktop/EdmontonRealEstate/")
data_raw = read.csv("EdmontonRealEstateData.dropDup.subset.processed.filter.csv", header = TRUE)
boundary = read.csv("fdaPDEBoundary.subset.filter.csv")
# only kept variables relevant for house pricing
# removed 1. tax id 6. assessed value 8. lot size 11. site_coverage = gross/lot_size
# 12. tot_gross_area (correlation with net_area is 1)

boundary_points = data_raw[boundary[,1], c("lon","lat")]
data = scale(data_raw[,c("net_area","lot_size")])


# Determine number of clusters, "scree" plot
# wss = (nrow(data)-1)*sum(apply(data,2,var))
# for (i in 2:15) wss[i] <- sum(kmeans(data, centers=i)$withinss)
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")

n_clusters = 4 # 2 clusters give a nice separation
# K-Means Cluster Analysis
fit <- kmeans(data, n_clusters, nstart = 100)
# get cluster means 
# aggregate(data,by=list(fit$cluster),FUN=mean)
# append cluster assignment
data_clustered = data.frame(data, fit$cluster)

# plot clusters
n_colors = length(colors(distinct = TRUE))
col_index = sample(1:n_colors, n_clusters, replace = FALSE)
col = colors(distinct = TRUE)[col_index]
#dev.new()
plot(data_raw[, c("lon","lat")], col = col[data_clustered[,"fit.cluster"]])


mean_value = c(1:n_clusters)
mean_lot_size = c(1:n_clusters)
cluster_size = c(1:n_clusters)
wss = c(1:n_clusters)
y = 53.50
for(i in 1:n_clusters){
  mean_value[i] = round(mean(data_raw[fit$cluster == i,"assessed_value"]))
  mean_lot_size[] = round(mean(data_raw[fit$cluster == i,"lot_size"]))
  cluster_size[i] = sum(data_clustered[,"fit.cluster"] == i)
  wss[i] =fit$withinss[i]
    
  y = y - 0.003
  text(-113.60, y, labels = paste("cluster ", i, "wss/size ", 
                                  round(wss[i]/cluster_size[i])), col = col[i])
  cat("\n cluster ", i, " size: ", cluster_size[i])
  cat("\n mean value: ", mean_value[i])
  cat("\n mean lot size: ", mean_lot_size[i])
  
  
}

# PCA
# data = data_raw[,-1]
# R = prcomp(data, scale = TRUE)
# sum(R$sdev^2/ncol(data))
# cumsum(R$sdev^2)/ncol(data)

t(fit$centers)

for(i in 1:n_clusters){
  plot(data_raw[fit$cluster == i, c("lon","lat")], col = "blue")
  lines(boundary_points, col = "black")
  line <- readline()
}




