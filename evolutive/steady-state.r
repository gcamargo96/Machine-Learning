# An evolutionary approach to the Traveling Salesman Problem

fitness <- function(candidate, distances){
	sum = 0
	for(i in 1:(length(candidate)-1)){
		sum = sum + distances[candidate[i], candidate[i+1]]
	}
	sum = sum + distances[candidate[length(candidate)], candidate[1]]

	return (-sum)
}

swap <- function(candidate, i, j){
	aux = candidate[i]
	candidate[i] = candidate[j]
	candidate[j] = aux
	return (candidate)
}

mutation <- function(candidate){
	ids = sample(1:length(candidate), size=2)
	return (swap(candidate, ids[1], ids[2]))
}

generateDistances <- function(ncities){
	D = matrix(runif(min=0.1, max=5, n=ncities*ncities),
		nrow=ncities, ncol=ncities)
	for(i in 1:ncities){
		D[i,i] = 0
	}
	return (D)
}

steady.state <- function(distances, pop.size=100, ngenerations=1000){
	ncities = nrow(distances)
	population = matrix(0, nrow=pop.size, ncol=ncities)
	fitness = rep(0, pop.size)

	# generating the population and calculating the fitness
	for(i in 1:pop.size){
		population[i, ] = sample(1:ncities)
		fitness[i] = fitness(population[i, ], distances)
	}

	statistics = NULL
	for(i in 1:ngenerations){
		parentId = sample(1:pop.size, size=1)
		child = population[parentId, ]
		child = mutation(child)

		anotherId = sample(1:pop.size, size=1)
		if(fitness(child, distances) > fitness(population[anotherId, ], distances)){
			population[anotherId, ] = child
			fitness[anotherId] = fitness(child, distances)
		}

		statistics = rbind(statistics, c(i, mean(fitness), sd(fitness)))
	}

	ret = list()
	ret$population = population
	ret$fitness = fitness
	ret$statistics = statistics

	return (ret)
}