
source("https://raw.githubusercontent.com/julbautista/Startup/master/julian_startup.R")
setwd("C:/Users/Julian Bautista/Documents/Portfolio/Dogs")


#Read in data
dogs <- read.table("https://raw.githubusercontent.com/julbautista/Dogs/master/dogs.txt", 
                   skip = 2)
names(dogs) <- c("dog", 0:24)

#Convert S and dots into 1's and 0's with trial 0 remaining
dogs1 <- dogs[,2:26] #set 2:26 without drop, 3:26 with dropping
doggy <- ifelse(dogs1 == "S", 1, 0)

#creating vector of "previous shocks" (inclusive of initial shock)
prevshocks <- matrix(data = NA, nrow = dim(doggy)[1], ncol = dim(doggy)[2])

for(i in 1:dim(doggy)[1]){
  for(j in 1:dim(doggy)[2]){
    prevshocks[i, j] <- sum(doggy[i, 1:(j - 1)])
  }
}
prevshocks[,1] <- rep(0, 30) #meant for if we don't drop trial 0

#dropping trial 0 from prevshocks and dog1
#prevshocks <- prevshocks[1:30,2:25] #drops trial 0
#doggy <- doggy[,2:25] #dropped trial 0


#creating vector of dog id's
id <- matrix(data = NA, nrow = dim(doggy)[1], ncol = dim(doggy)[2])

for(i in 1:dim(doggy)[1]){
    id[i,] <- rep(i,dim(doggy)[2])
  }

#creating a matrix of all of the vectors
snoop<- as.data.frame(
      cbind(
        rep(seq(from = 0, to = 24),30),
        c(t(id)),
        c(t(doggy)),
        c(t(prevshocks))
        ))
snoop <- cbind(snoop, (snoop[1]-snoop[4])) #adds avoid column
      
names(snoop) <- c("trial", "dog_id", "shocks", "prevshocks", "avoidances")

#avoid <- ifelse(snoop$avoid == 1,0,1) #whether or not dog avoided shock, 1 if avoided
shocks <- snoop$shocks #whether or not dog avoided shock, 1 if shocked
trial <- snoop$trial #trial number
dog_id <- snoop$dog_id #which dog is it
prevshocks <- snoop$prevshocks #number of shocks experienced by dog
avoidances <- snoop$avoidances #number of avoidances by the dog
N <- 750

#mu(prevshocks, avoidances) logit(-0.12,-0.31) log(-0.09,-0.29)
#fitting the stan model for the logistic regression without dropped variables
stanc("dogs.stan")$status
fit <- stan("dogs.stan",
            data = c("shocks","dog_id","avoidances","N","prevshocks"),
            iter = 1000, chains = 3)


#extracting the stan model
logit<-extract(fit)
summary(logit)
plot(density(logit$b_prevshocks))
plot(density(logit$b_avoidances))

#fitting the stan model for the logarithmic regression without dropped variables
stanc("dogslog.stan")$status
fitlog <- stan("dogslog.stan",
            data = c("shocks","dog_id","avoidances","N","prevshocks"),
            iter = 1000, chains = 3)

#extract from stan model
log<-extract(fitlog)
summary(log$mu_prevshocks)
summary(log$mu_avoidances)
sd(log$mu_prevshocks)
sd(log$mu_avoidances)
summary(log$tau_prevshocks)
summary(log$tau_avoidances)
sd(log$tau_prevshocks)
sd(log$tau_avoidances)

#ploting densities in a histogram
sim_avoid<-log$sim_avoid
sim_avoid_mean<-c()
sim_avoid_sd<-c()
#mean and sd of each dog
for(i in 1:dim(sim_avoid)[2]){
  sim_avoid_mean[i]<- mean(sim_avoid[i,])
  sim_avoid_sd[i]<- sd(sim_avoid[i,])
}
hist(sim_avoid_mean,breaks =25, col ="light gray", 
     xlab = "Mean", ylab = NULL, main = "Mean of Shocks")
abline(v = mean(shocks), col = "red", lwd = 2.5)

hist(sim_avoid_mean,breaks =15, col ="light gray", xlim = (c(0.2,0.5)),
     xlab = "Mean", ylab = NULL, main = "SD of Shocks")
abline(v = sd(shocks), col = "red", lwd = 2.5)

#prediction plot
dog_avoid <- 1-apply(as.matrix(doggy), 2, mean)
pred_avoid <- matrix(colMeans(sim_avoid),ncol = 25, nrow = 30, byrow = TRUE)

plot(x =NULL, y=NULL, ylim = c(0,1.0), xlim = c(0,25), 
     ylab = "Avoidance Propportion", xlab = "Trials", 
     main = "Model Predicting Avoidance")
for(i in 1:30){
  lines(1:25, (1-pred_avoid[i, c(1:25)]), 
        type = "l", col = "light grey", cex = 0.2)
}
lines(1:25, dog_avoid, type ="l", cex =1.5)

#simulate data and reorder
sim_avoid_sam <- matrix(sim_avoid[64,], ncol = 25, nrow = 30, byrow = TRUE)
names(sim_avoid_sam) <- 0:24

last_sim <- c()
for(n in 1:30){
  last_sim[n]<-max((1:25)[y[n,]==1])
}
order_sim <- sim_avoid_sam[order(last_sim),]
row.names(order_sim) <- NULL
order_sim

#reorder real data
y<-data.frame(doggy)
names(y) <- 0:24

last_y <- c()
for(n in 1:30){
  last_y[n]<-max((1:25)[y[n,]==1])
}
order_y <- y[order(last_y),]
row.names(order_y) <- NULL
order_y


#heatmap
library(pheatmap)

pheatmap(order_sim, cluster_row = FALSE , cluster_col = FALSE , legend = FALSE ,
show_colnames = T,color=gray.colors(2,start=1,end=0),main="Fake Data",
show_rownames = T, labels_row = "Dogs", labels_col = "Trials")

pheatmap(order_y, cluster_row = FALSE , cluster_col = FALSE , legend = FALSE ,
show_colnames = T,color=gray.colors(2,start=1,end=0),main="Real Data",
show_rownames = T, labels_row = "Dogs", labels_col = "Trials")

