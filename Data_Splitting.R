#Data Splitting

x <- as.matrix(imputed_data[, -15])
y <- imputed_data[, 15]

size <- floor(0.8 * nrow(imputed_data))
training_index <- sample(seq_len(nrow(imputed_data)), size = size)

train <- imputed_data[training_index, ]
test <- imputed_data[-training_index, ]

xtrain <- as.matrix(train[, -15])
ytrain <- train[, 15]

xtest <- as.matrix(test[, -15])
ytest <- test[, 15]