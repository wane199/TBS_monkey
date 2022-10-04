# Rtsne
iris_unique <- unique(iris) # Remove duplicates
iris_matrix <- as.matrix(iris_unique[,1:4])

X <- normalize_input(iris_matrix)
colMeans(X)
range(X)
# Set a seed if you want reproducible results
set.seed(123)
tsne_out <- Rtsne(iris_matrix,pca=FALSE,perplexity=30,theta=0.0) # Run TSNE

# Show the objects in the 2D tsne representation
plot(tsne_out$Y,col=iris_unique$Species, asp=1)

# data.frame as input
tsne_out <- Rtsne(iris_unique,pca=FALSE, theta=0.0)

# Using a dist object
set.seed(123)
tsne_out <- Rtsne(dist(normalize_input(iris_matrix)), theta=0.0)
plot(tsne_out$Y,col=iris_unique$Species, asp=1)

set.seed(123)
tsne_out <- Rtsne(as.matrix(dist(normalize_input(iris_matrix))),theta=0.0)
plot(tsne_out$Y,col=iris_unique$Species, asp=1)

# Supplying starting positions (example: continue from earlier embedding)
set.seed(123)
tsne_part1 <- Rtsne(iris_unique[,1:4], theta=0.0, pca=FALSE, max_iter=350)
tsne_part2 <- Rtsne(iris_unique[,1:4], theta=0.0, pca=FALSE, max_iter=650, Y_init=tsne_part1$Y)
plot(tsne_part2$Y,col=iris_unique$Species, asp=1)

## Not run: 
# Fast PCA and multicore
tsne_out <- Rtsne(iris_matrix, theta=0.1, partial_pca = TRUE, initial_dims=3)
tsne_out <- Rtsne(iris_matrix, theta=0.1, num_threads = 2)
## End(Not run)

