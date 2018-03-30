knn <- function(dataset, query, k=1){
	classCol = ncol(dataset)
	
	D = apply(dataset, 1, function(row){
		sqrt(sum((as.numeric(row[1:classCol-1]) - query)^2))
		})

	ids = sort.list(D)[1:k]
	classes = dataset[ids, classCol]
	U = unique(classes)
	C = rep(0, length(U))

	for(i in 1:length(U)){
		#how many times the class U[i] occurs in the k-nearest neighbors
		C[i] = sum(U[i] == classes)
	}

	id = which.max(C)

	return (U[id])
}