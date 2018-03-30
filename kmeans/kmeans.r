kmeans <- function(dataset, centers=2, threshold=1e-3){
	dataset = as.matrix(dataset)
	ids = sample(1:nrow(dataset), centers)
	centers = dataset[ids,]
	closest = rep(0, nrow(dataset));

	div = 2*threshold
	while(div > threshold){
		div = 0

		for(i in 1:nrow(dataset)){
			row = dataset[i,]

			distance = apply(centers, 1, function(ci) {
					sqrt(sum((row - ci)^2))
				})

			closest[i] = sort.list(distance)[1]
		}

		old = centers
		for(i in 1:nrow(centers)){
			id = which(closest == i)
			centers[i,] = colMeans(dataset[id,])
		}

		div = sum((old-centers)^2)
	}

	return (centers)
}