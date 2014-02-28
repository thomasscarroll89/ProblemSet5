## Class Activity Feb. 20 ## 
## Simulation Activity
library(MASS)
install.packages("pdist")
library(pdist)

####Problems 1-2####
#The function "voters" generates an nx2 matrix of voter preferences, where each row corresponds to a different voter
#and each column corresponds to a specific policy dimension. The "option" argument determines how the preferences are 
#to be drawn (i.e. whether they should be drawn from a normal distribution, uniform distribution, etc.). For this problem,
#"option" can be set to any integer from 1 to 5. If option=1, then the voter preferences are drawn from a standard normal 
#distribution. If option=2, then the preferences are drawn from 2 separate normal distributions with  standard deviations 
#equal to sig1 and sig2, respectively. If option=3, preferences are drawn from a uniform distribution. If option=4, preferences
#are drawn from a multivariate normal distribution with variance-covariance matrix equal to argument "Sigma". If option=5,
#then 3 separate multivariate normal distributions with random parameters are used to generate preferences, and from these 3 
#distributions a random sample of size n is selected and used as the matrix of voter preferences. 
voters <- function(n, option=1, sig1=runif(1), sig2=runif(1), Sigma=matrix(c(10,3,3,2),2,2)){
  if (option==1){
    voter <- matrix(rnorm(n*2), nrow=n, ncol=2)
    return(voter)
  }
  if (option==2){
    voter <- matrix(c(rnorm(n, mean=0, sd=sig1), rnorm(n, mean=0, sd=sig2)),
                      nrow=n, ncol=2)
    return(voter)
  }
  if (option==3){
    voter <- matrix(runif(n*2), nrow=n, ncol=2)
    return(voter)
  }
  if (option==4){
    voter <- matrix(mvrnorm(n, mu=c(0,0), Sigma=Sigma),nrow=n, ncol=2)
    return(voter)
  }
  if (option==5){
    mvr.1 <- mvrnorm(n, mu=c(0,0), Sigma=Sigma)
    mvr.2 <- mvrnorm(n, mu=runif(2), Sigma=Sigma+runif(1))
    mvr.3 <- mvrnorm(n, mu=runif(2), Sigma=Sigma+runif(1))
    total <- rbind(mvr.1, mvr.2, mvr.3)
    voter <- total[sample(1:(3*n), size=n),]
    return(voter)
  }
}


####Problem 3: Function such that voters affiliate with the closest of the two parties####
#The affiliation function takes the same arguments as the voters function above; thus when running affiliation, you can 
#decide which kind of distribution will be used to generate voter preferences, etc. 
affiliation <- function(n, option=1, sig1=runif(1), sig2=runif(1), Sigma=matrix(c(10,3,3,2),2,2)){
  outcome <- vector("list")

  #Generate the voters' and parties' positions 
  voter.position <-  voters(n=n, option=option, sig1=sig1, sig2=sig2, Sigma=Sigma)  #run the voters function from Problems 1-2 above to generate some random initial voter preferences
  party.1 <- rnorm(2) #the original positions taken by Party 1 on two dimensions
  party.2 <- rnorm(2) #the original positions taken by Party 2 on two dimensions
  party.position <- rbind(party.1, party.2)
  
  #Calculate a vector showing each individual's total distance from party 1; call it distance.1
  distance.temp.1.1 <- voter.position[,1] - party.1[1] 
  distance.temp.1.2 <- voter.position[,2] - party.1[2] 
  distance.temp.1 <- cbind(distance.temp.1.1, distance.temp.1.2)^2 #matrix; first column is a^2, second column is b^2
  distance.1 <- sqrt(apply(distance.temp.1, 1, sum)) #Pythagorean Theorem: c=sqrt(a^2 + b^2)
  
  #Next calculate a vector showing each individual's total distance from party 2; call it distance.2
  distance.temp.2.1 <- voter.position[,1] - party.2[1]
  distance.temp.2.2 <- voter.position[,2] - party.2[2]
  distance.temp.2 <- cbind(distance.temp.2.1, distance.temp.2.2)^2 #matrix; first column is a^2, second column is b^2
  distance.2 <- sqrt(apply(distance.temp.2, 1, sum)) #Pythagorean Theorem: c=sqrt(a^2 + b^2)
  
  final.distance <- distance.1 - distance.2
  voter.position <- data.frame(voter.position)
  voter.position$final.distance <- final.distance
  voter.position$affiliation <- ifelse(final.distance > 0, "2", "1") 
    #create affiliation variable. Coded as "2" if voter is closer to party 2, and "1" otherwise
  outcome$voter.position.data <- voter.position
  outcome$party.positions <- party.position
  return(outcome)
}


####Problem 4: Visualizing the Affiliations####
first.draw <- affiliation(n=1000, option=5) #I use a sample size of 100 to demonstrate
#First we plot only those observations who have an affiliation score of 1 (colored blue)
plot(x=first.draw$voter.position.data[which(first.draw$voter.position.data$affiliation==1),1], 
     y=first.draw$voter.position.data[which(first.draw$voter.position.data$affiliation==1),2],
     col="blue", pch=16, cex=0.25, xlab="First Dimension", ylab="Second Dimension", 
     xlim=c(min(first.draw$voter.position.data[,1]), max(first.draw$voter.position.data[,1])), 
     ylim=c(min(first.draw$voter.position.data[,2]), max(first.draw$voter.position.data[,2])))
#Second we add the points with affiliation scores of 2 (colored red)
points(x=first.draw$voter.position.data[which(first.draw$voter.position.data$affiliation==2),1], 
       y=first.draw$voter.position.data[which(first.draw$voter.position.data$affiliation==2),2],
       col="red", pch=16, cex=0.25)
#Third we add the locations of the parties themselves, graphed as orange triangles
points(x=first.draw$party.position[,1], y=first.draw$party.position[,2], col="orange", pch=17)


# Subsequent code; didn't work because of the changes I made to problem 3. I didn't end up using ggplot(), but let me 
#know if you want me to go back and use ggplot for any reason. I'm just gonna comment it for now in case we want to 
#go back and include it. 
# colnames(party.position) <- c("issue1", "issue2")
# library(ggplot2)
# plot1 <- ggplot(voter.position, aes(x=X1, y=X2, colour=affiliation)) + geom_point() 
# plot1 <- plot1 + geom_point(aes(x=party.position[1,1], y=party.position[1,2], colour="party1")) # Party 1
# plot1 <- plot1 + geom_point(aes(x=party.position[2,1], y=party.position[2,2], colour="party2")) # Party 2
# plot1 
# ?ggplot
