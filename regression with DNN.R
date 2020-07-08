library(keras)
library(ggplot2)
dataset<-dataset_boston_housing()
c(c(train_data, train_target), c(test_data, test_target))%<-%dataset

mean<-apply(train_data, MARGIN = 2, mean)
std<-apply(train_data, MARGIN = 2, sd)
train_data<-scale(train_data, center = mean, scale = std)
test_data<-scale(test_data, center = mean, scale=std)

build_model<-function(){
  model<-keras_model_sequential()%>%
    layer_dense(units = 64, activation = 'relu', input_shape = dim(train_data)[2])%>%
    layer_dense(units=64, activation = 'relu')%>%
    layer_dense(units=1)
  model%>%compile('rmsprop', loss='mse', metrics=c('mae'))
}

k<-4
indices<-sample(1:nrow(train_data))
folds<-cut(indices, breaks = 4, labels = FALSE)
num_epoch<-500
all_mae_histories<-NULL
for(i in 1:k){
  cat("processing fold #", i, "\n")
  val_indices<-which(folds==i, arr.ind = T)
  val_data<-train_data[val_indices,]
  val_targets<-train_target[val_indices]
  partial_train_data<-train_data[-val_indices,]
  partial_train_targets<-train_target[-val_indices]
  model<-build_model()
  history<-model%>%fit(partial_train_data, partial_train_targets, validation_data=list(val_data, val_targets), epochs=num_epoch, batch_size=1, verbose=0)
  mae_history<-history$metrics$val_mean_absolute_error
  all_mae_histories<-rbind(all_mae_histories, mae_history)
}
str(all_mae_histories)
averagee_mae_history<-data.frame(
  epoch=seq(1:ncol(all_mae_histories)),
  validation_mae=apply(all_mae_histories, 2, mean)
)

ggplot(averagee_mae_history, aes(x=epoch, y=validation_mae))+geom_line()
ggplot(averagee_mae_history, aes(x=epoch, y=validation_mae))+geom_smooth()

model<-build_model()
model%>%fit(train_data, train_target, epochs=125, 
            batch_size=16, verbose=0)
result<-model%>%evaluate(test_data, test_target)
result
