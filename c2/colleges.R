###########################################################
### ISLR -  - C2
###########################################################

library(ISLR)

# read in college data set.
collegeDF <- College

summary(collegeDF)

# produce a matrix of scatterplots, first 10 vars.
pairs(collegeDF[,1:10])

# boxplots of Outstate and Private
boxplot(Outstate ~ Private, # Out of state pop modeled by factor(Private)
        data = collegeDF,
        horizontal = TRUE,
        names = c("Public", "Private"),
        col = (c("Blue", "Red")),
        notch = TRUE,
        border = "darkgreen",
        main = "Out of state students \nat Public - Private Universities")

boxplot(S.F.Ratio ~ Private, # S.F. Ratio broken modeled by factor(Private)
        data = collegeDF,
        horizontal = TRUE,
        # color = (c("Blue", "Red")),
        notch = TRUE,
        varwidth = TRUE,
        names = c("Public", "Private"),
        col = c("turquoise", "tomato"),
        xlab = "S/F Ratio",
        border = "darkgreen",
        main = "Student-Faculty ratio \n Public vs Private Universities")

plot(as.factor(collegeDF$Private), collegeDF$Outstate,
     horizontal = TRUE,
     main = "Number of Private Students \nand Out of State")
