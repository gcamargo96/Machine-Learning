# para cada query:
# para cada yi, calcular yi * w(xi, xj)
# somar todos os valores obtidos e dividir pela soma dos pesos (média ponderada)

weight <- function(xi, xj, sigma){
	dist = sum((xi-xj)^2)
	w = exp((-dist^2)/(2*sigma))
}

dwnn <- function(dataset, query, sigma=0.5){
	dataset = as.matrix(dataset)

	classId = ncol(dataset)
	X = as.matrix(dataset[, 1:(classId-1)])
	Y = as.matrix(dataset[, classId])

	w = apply(X, 1, function(xj){
		weight(query, xj, sigma)
		})

	y = sum(Y*w)/sum(w)

	return (y)
}