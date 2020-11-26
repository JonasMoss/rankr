#install.packages("clusterGeneration")

mat = clusterGeneration::rcorrmatrix(5, alphad = 1)

eigen(2 * diag(5) - mat)

sum((mat - (2 * diag(5) - mat))^2)/2

dat = psychTools::bfi[, 1:25]
dat = dat[complete.cases(dat), ]

plot(table(dat$A1, dat$A2))

myTable <- table(dat$N1, dat$N2)
myFrame <- as.data.frame(myTable)


library("ggplot2")
ggplot(myFrame, aes(x = Var1, y = Var2, size = Freq)) + 
  geom_point()


ggplot(dat, aes(x = N2, y = N1)) + 
  geom_jitter(shape=21, position=position_jitter(0.2))

tau6(dat$N2, dat$N1)
psych::polychoric(table(dat$N1, dat$N2))


table(dat$C3)

