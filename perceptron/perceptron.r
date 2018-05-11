f <- function(net){
	if(net >= 0.5){
		return (1)
	}
	return (0)
}

perceptron.test <- function(x, weights){
	net = c(x, 1) %*% weights
	return (f(net))
}

perceptron.train <- function(dataset, eta=0.1, threshold=1e-3){
	dataset = as.matrix(dataset)
	classId = ncol(dataset)
	X = dataset[, 1:classId-1]
	Y = dataset[, classId]

	#Equações de adaptação dos pesos w_1, w_2 e theta
	#w_i(t+1) = w_i(t) - eta*dE2/dw_i
	#theta(t+1) = theta(t) - eta*dE2/theta

	#weights = [ w_1 w_2 theta ]
	weights = runif(min=-0.5, max=0.5, n=ncol(X)+1)

	sqerror = 2*threshold

	while(sqerror > threshold){
		sqerror = 0

		for(i in 1:nrow(X)){
			x = X[i,]
			y = Y[i]

			#aplicar exemplo no perceptron
			net = c(x, 1) %*% weights
			y.o = f(net)

			error = y - y.o
			sqerror = sqerror + error^2

			#dE2/d_w1 = 2*(error)*-x[1]
			#dE2/d_w2 = 2*(error)*-x[2]
			#dE2/d_theta = 2*(error)*-1
			dE2 = 2*error*-c(x,1)

			weights = weights - eta * dE2
		}

		sqerror = sqerror/nrow(X)
		cat("sqerror = ", sqerror, "\n")
	}

	return (weights)
}