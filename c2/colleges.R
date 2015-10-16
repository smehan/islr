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
        col = c("thistle", "wheat"),
        xlab = "S/F Ratio",
        border = "darkgreen",
        main = "Student-Faculty ratio \n Public vs Private Universities")

plot(as.factor(collegeDF$Private), collegeDF$Outstate,
     horizontal = TRUE,
     main = "Number of Private Students \nand Out of State")

# ggplot boxplots
ggplot(collegeDF) +
    aes(x = as.factor(Private), y = S.F.Ratio, fill = as.factor(Private)) +
    geom_boxplot() +
    coord_flip() +
    labs(size= "1", x = "Is Private", y = "S/F ratio",
         title = "S/F Ratio by University status", vjust=-10) +
    guides(fill = FALSE) + # remove redundant legend
    stat_summary(fun.y=mean, geom="point", shape=5, size=4) # add mean to each plot

ggplot(collegeDF) +
    aes(x=S.F.Ratio) +
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=.5,
                   colour="black",
                   fill="white") +
    geom_density(alpha=.2, fill="turquoise")

ggplot(collegeDF) +
    aes(x=S.F.Ratio) +
    geom_histogram(binwidth=.5,
                   colour="black",
                   fill="royalBlue") +
    geom_vline(aes(xintercept = collegeDF["California Polytechnic-San Luis","S.F.Ratio"]),
               color = "limegreen",
               linetype = "dashed",
               size = 2) +
    geom_vline(aes(xintercept = mean(collegeDF$S.F.Ratio, na.rm = TRUE)),
               color = "red",
               linetype = "dotted",
               size = 1) +
    labs(size= "1", x = "S/F Ratio", y = "How many universities?",
         title = "Cal Poly S/F Ratio amongst US Universities\n 1995 data", vjust=-10)

ggplot(collegeDF) +
    aes(x=S.F.Ratio, fill = Private) +
    geom_histogram(binwidth=.5,
                   alpha = 0.5,
                   position = "identity") +
    geom_vline(aes(xintercept = collegeDF["California Polytechnic-San Luis","S.F.Ratio"]),
               color = "limegreen",
               linetype = "dashed",
               size = 2) +
    geom_vline(aes(xintercept = mean(collegeDF$S.F.Ratio, na.rm = TRUE)),
               color = "red",
               linetype = "dotted",
               size = 1) +
    labs(size= "1", x = "S/F Ratio", y = "How many universities?",
         title = "Cal Poly S/F Ratio amongst US Universities\n 1995 data", vjust=-10)

ggplot(collegeDF) +
    aes(x=S.F.Ratio, fill = Private) +
    scale_fill_manual(values = (c("gold", "rosybrown"))) +
    geom_histogram(binwidth=.5,
                   alpha = 0.8,
                   position = "dodge") +
    geom_vline(aes(xintercept = collegeDF["California Polytechnic-San Luis","S.F.Ratio"]),
               color = "limegreen",
               linetype = "dashed",
               size = 2) +
    geom_vline(aes(xintercept = mean(collegeDF$S.F.Ratio, na.rm = TRUE)),
               color = "red",
               linetype = "dotted",
               size = 1) +
    labs(size= "1", x = "S/F Ratio", y = "How many universities?",
         title = "Cal Poly S/F Ratio amongst US Universities\n 1995 data", vjust=-10)

ggplot(collegeDF) +
    aes(x=S.F.Ratio, fill = Private) +
    scale_fill_manual(values = (c("gold", "rosybrown"))) +
    geom_density(alpha = 0.3) +
    geom_vline(aes(xintercept = collegeDF["California Polytechnic-San Luis","S.F.Ratio"]),
           color = "limegreen",
           linetype = "dashed",
           size = 2) +
    geom_vline(aes(xintercept = mean(collegeDF$S.F.Ratio, na.rm = TRUE)),
               color = "red",
               linetype = "dotted",
               size = 1) +
    labs(size= "1", x = "S/F Ratio", y = "University Kernel Density",
         title = "Cal Poly S/F Ratio amongst US Universities\n 1995 data", vjust=-10)

# can't get this to work to calculate subset means
# ddply(collegeDF, "S.F.Ratio", summarise, rating.mean=mean(rating))

# end of listing
