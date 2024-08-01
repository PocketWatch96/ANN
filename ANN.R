install.packages(c('neuralnet', 'kera','tensorflow'), dependencies = T)
install.packages("tidyverse")

library(neuralnet)
library(tidyverse)

iris<-iris%>%mutate_if(is.character, as.factor)
iris_sample <- iris[sample(nrow(iris), 10), ]
summary(iris)

set.seed(254)
data_rows<-floor(0.80*nrow(iris))
data_rows

train_indices<-sample(c(1:nrow(iris)), data_rows)
train_indices

train_data<-iris[train_indices, ]
head(train_data)

test_data<-iris[-train_indices, ]
head(test_data)

model<-neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = train_data, hidden = c(4), linear.output = FALSE)
plot(model, rep='best')

pred<-predict(model, test_data)
labels<-c("setosa", "versicolor", "virginica")
prediction_label <- data.frame(max.col(pred)) %>% mutate(pred=labels[max.col.pred.]) %>% select(2) %>% unlist()
table(test_data$Species, prediction_label)

check = as.numeric(test_data$Species) == max.col(pred)
accuracy = (sum(check)/nrow(test_data))*100
print(accuracy)