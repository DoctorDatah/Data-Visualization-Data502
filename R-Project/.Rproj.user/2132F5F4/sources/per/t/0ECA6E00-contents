# install.packages("psych")
library(psych)
library(datasets)

iris
describe(iris)
summary(iris)
?summary()
?describe()


# Describe gives the descriptive statistics of the variables. i.e mean, sd. max, min etc. 
# Summary gives the quantiles of the dataset.


# Spliting for individual stats

Category  = split(iris, iris$Species)

describe(iris[1])
describe(iris[2])
describe(iris[3])
describe(iris[4])

describe(Category$setosa)
describe(Category$virginica)
describe(Category$versicolor)
# Boxplot
boxplot(iris)


Category  = split(iris, iris$Species)

boxplot(Category$setosa)
boxplot(Category$virginica)
boxplot(Category$versicolor)



