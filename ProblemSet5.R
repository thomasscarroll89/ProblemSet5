## Class Activity Feb. 20 ## 
## Simulation Activity
library(MASS)
install.packages("pdist")
library(pdist)
# Simulation set up
# Create a matrix of voters

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
    voter <- total[sample(1:(3*100), size=100),]
    return(voter)
  }
}

#changed the number of samples from "n*2" to "n" 
#changed the way we sampled for option=5; the old way might have been mixing up results from different columns of the multivariate distribution



# 3. Function such that voters affiliate with the closes of the two parties. 
  party.1 <- rnorm(2)
  party.2 <- rnorm(2)
party.position <- rbind(party.1, party.2)
party.position
  voter.position <-  voters(n=100, option=1)  
  distance.1 <- apply((voter.position - party.1)^2, 1, sum)
  distance.1 <- sqrt(distance.1)
  distance.2 <- apply((voter.position - party.2)^2, 1, sum)
  distance.2 <- sqrt(distance.2)
  final.distance <- distance.1 - distance.2
  voter.position <- data.frame(voter.position)
  voter.position$final.distance <- final.distance
  voter.position$affiliation <- ifelse(final.distance > 0, "2", "1")
 
 # 4. 
colnames(party.position) <- c("issue1", "issue2")
library(ggplot2)
plot1 <- ggplot(voter.position, aes(x=X1, y=X2, colour=affiliation)) + geom_point() 
plot1 <- plot1 + geom_point(aes(x=party.position[1,1], y=party.position[1,2], colour="party1")) # Party 1
plot1 <- plot1 + geom_point(aes(x=party.position[2,1], y=party.position[2,2], colour="party2")) # Party 2
plot1 
?ggplot
