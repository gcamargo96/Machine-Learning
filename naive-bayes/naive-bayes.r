naive.bayes <- function(dataset, query, min_prob=1e-7){
	classId = ncol(dataset)
	classes = unique(dataset[,classId])

	probabilities = rep(0, length(classes))

	for(i in 1:length(classes)){
		p_class = sum(dataset[,classId] == classes[i])/nrow(dataset)
		probabilities[i] = probabilities[i] + log(p_class)

		for(j in 1:length(query)){
			attribute = query[j]
			if(attribute != "?"){
				p_attr_class = sum(dataset[,classId] == classes[i] & dataset[,j] == attribute)/sum(dataset[,classId] == classes[i])
				if(p_attr_class == 0){
					p_attr_class = min_prob
				}
				probabilities[i] = probabilities[i] + log(p_attr_class)
			}
		}
	}

	probabilities = exp(probabilities)

	ret = list()
	ret$classes = classes
	ret$probabilities = probabilities/sum(probabilities)

	return (ret)
}