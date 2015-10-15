###########################################################
### ISLR - looking at some KNN implementations - C2
###########################################################

library(ISLR)
library(ggplot2)
library(class) # for knn

### First, let's being with a 2D feature space for two classes
# Class A training object instances (cases)
A1 <- c(0,0)
A2 <- c(1,1)
A3 <- c(2,2)
# Class B training objects instances (cases)
B1 <- c(6,6)
B2 <- c(5.5,7)
B3 <- c(6.5,5)

train1 <- data.frame(rbind(A1,A2,A3, B1,B2,B3))
names(train1) <- c("X", "Y")
test1 <- c(4,4)
cl <- factor(c(rep("A",3),rep("B",3))) # class labels
knn(train1, test1, cl, k = 1) # knn with 1 neighbour
summary(knn(train1, test1, cl, k = 1))
knn(train1, test1, cl, k = 3) # knn with 3 neighbours
summary(knn(train1, test1, cl, k = 3))

# lets plot out the datapoints to see them
plot(train1$X, train1$Y, main = "Features for KNN", xlab = "X - axis", ylab = "Y - axis")

# more complex plot also showing test data point. Since there is only one test point, need
# to make a data.frame of one pair
test_point <- data.frame("X" = 4.0, "Y" = 4.0)
ggplot(data = train1) +
    aes(X, Y, colour = cl) +
    geom_point(size = 3) +
    geom_point(data = test_point, aes(X, Y, colour = "Test Data"), size = 4) +
    labs(size= "1", x = "X coords", y = "Y coords",
         title = "Features for KNN", vjust=-10,
         colour = "Class Labels") +  # change the label for legend by variable name in aes()
    theme(axis.text=element_text(size=16),
          axis.text.x = element_text(angle=0, vjust=1),
          axis.title=element_text(size=16),
          legend.position="bottom", legend.direction = "vertical", #change location and direction of legend
          legend.text = element_text(colour="blue", size = 16, face = "bold")) +  #change style for legend text
    theme(plot.title = element_text(size = 18))

# that results in correct classification and one can see that the test point is closer to
# A class with k = 1, closer to B class with k = 3 (euclidean metric).

# now expand the number of test points
# as a matrix. This will need to be cast as a df when using ggplot
test_points = matrix(c(4,4,3,3,5,6,7,7), ncol=2, byrow=TRUE)
ggplot(data = train1) +
    aes(X, Y, colour = cl) +
    geom_point(size = 3) +
    geom_point(data = as.data.frame(test_points), aes(V1, V2, colour = "Test Data"), size = 4) +
    labs(size= "1", x = "X coords", y = "Y coords",
         title = "Features for KNN", vjust=-10,
         colour = "Class Labels") +  # change the label for legend by variable name in aes()
    theme(axis.text=element_text(size=16),
          axis.text.x = element_text(angle=0, vjust=1),
          axis.title=element_text(size=16),
          legend.position="bottom", legend.direction = "vertical", #change location and direction of legend
          legend.text = element_text(colour="blue", size = 16, face = "bold")) +  #change style for legend text
    theme(plot.title = element_text(size = 18))

knn(train1, test_points, cl, k = 1) # knn with 1 neighbour
summary(knn(train1, test_points, cl, k = 1))



### Sample data for exploratory KNN with 3 features in a 2 class problem
F1 <- c(0,2,0,0,-1,1)
F2 <- c(3,0,1,1,0,1)
F3 <- c(0,0,3,2,1,1)
Y  <- c("Red","Red","Red","Green","Green","Red") # class labels
train2 <- data.frame(X1 = F1,
                    X2 = F2,
                    X3 = F3)

test2 <- c(0,0,0)
knn(train2, test2, k=3, cl = Y) # perform the knn with k neighbors
summary(knn(train2, test2, k=3, cl = Y))


