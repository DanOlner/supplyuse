#Mess with PCA

#data <- data.frame(ttwa = c(1:5), coal = c(1,2,3,3.5,4.5), elec = c(1:5), agri = c(1:5))
data <- data.frame(ttwa = c(1:5), coal = c(1,2,3,3.5,4.5), elec = c(1:5))

#data <- data.frame(ttwa = c(1:5), coal = rep(1,times=5), elec = rep(1,times=5), agri = rep(1,times=5))


#plot(data$coal, data$elec)

data <- as.matrix(data)

plot(data[,2], data[,3])

data.pca <- prcomp(data[,2:3])
                   
summary(data.pca)

print(data.pca)

biplot(data.pca, expand = 1)

data.pca$rotation
data.pca$x

results <- data.pca$x
resultsr <- data.pca$rotation

plot(results)
plot(resultsr)
