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
visualizing <- function(n, option=1, sig1=runif(1), sig2=runif(1), Sigma=matrix(c(10,3,3,2),2,2)){
  first.draw <- affiliation(n=n, option=option, sig1=sig1, sig2=sig2, Sigma=Sigma)
  #First we plot only those observations who have an affiliation score of 1 (colored blue)
  plot(x=first.draw$voter.position.data[which(first.draw$voter.position.data$affiliation==1),1], 
       y=first.draw$voter.position.data[which(first.draw$voter.position.data$affiliation==1),2],
       col="blue", pch=16, cex=0.25, xlab="First Dimension", ylab="Second Dimension", 
       xlim=c(min(c(first.draw$voter.position.data[,1], first.draw$party.position[,1])), 
              max(c(first.draw$voter.position.data[,1], first.draw$party.position[,1]))), 
       ylim=c(min(c(first.draw$voter.position.data[,2], first.draw$party.position[,2])),
              max(c(first.draw$voter.position.data[,2], first.draw$party.position[,2]))))
  #Second we add the points with affiliation scores of 2 (colored red)
  points(x=first.draw$voter.position.data[which(first.draw$voter.position.data$affiliation==2),1], 
         y=first.draw$voter.position.data[which(first.draw$voter.position.data$affiliation==2),2],
         col="red", pch=16, cex=0.25)
  #Third we add the locations of the parties themselves, graphed as orange triangles
  points(x=first.draw$party.position[,1], y=first.draw$party.position[,2], col=c("orange", "green"), pch=17)
  legend(x="topright", legend=c("Vote for Party 1", "Vote for Party 2", "Party 1", "Party 2"), 
         pch=c(16, 16, 17, 17), col=c("blue", "red", "orange", "green"), cex=0.5)
  return(first.draw)
}

visualizing(n=1000, option=1)
visualizing(n=1000, option=2)
visualizing(n=1000, option=3)
visualizing(n=1000, option=4)
visualizing(n=1000, option=5)

# Subsequent code for ggplot. I didn't end up using ggplot(), but let me know if you want me to go back 
#and use ggplot for any reason. I'm just gonna comment it for now in case we want to go back and include it. 
# library(ggplot2)
# plot1 <- ggplot(first.draw$voter.position.data, aes(x=X1, y=X2, colour=affiliation)) + geom_point() 
# plot1 <- plot1 + geom_point(aes(x=party.position[1,1], y=party.position[1,2], colour="party1")) # Party 1
# plot1 <- plot1 + geom_point(aes(x=party.position[2,1], y=party.position[2,2], colour="party2")) # Party 2
# plot1 

####"Get Things Moving" Section####
####Problem 1-2####
#This function is identical to the affiliation function above, which calculates both voter preferences and the 
#party locations at random. 

####Problem 3####
#The relocate function below takes one argument, voter.pref, which is the first element of the list that is output by the 
#affiliation() or visualizing() functions above.
relocate <- function(voter.pref){
  new.party.1 <- c(mean(voter.pref[which(voter.pref$affiliation==1),1]), 
                   mean(voter.pref[which(voter.pref$affiliation==1),2]))
  new.party.2 <- c(mean(voter.pref[which(voter.pref$affiliation==2),1]), 
                   mean(voter.pref[which(voter.pref$affiliation==2),2]))
  new.party.positions <- rbind(new.party.1, new.party.2)
  return(new.party.positions)
}


####Problem 4####
#Affiliation2 function allows the user to set the values of the party positions, as well as the voters' positions, instead
#of generating them randomly. It then runs code similar to the affiliation function above to calculate which voters will vote
#which parties. The voter.position argument should be a 2 column matrix, the first column containing all voters' positions on 
#the first dimension while the second column contains all voters' positions on the 2nd dimension. Party.1 and Party.2 are vectors
#of length 2 containing the positions of each party on a specific issue. Essentially I use this function to recalculate who the
#voters will vote for in each iteration of the master() function below. 
affiliation2 <- function(voter.position, party.1, party.2){  
  #Rbind the party positions
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
  output <- vector("list")
  output$voter.position <- voter.position
  output$party.position <- party.position
  return(output)
}

#The function below is the master functions, which randomly creates a set of initial values for the voters and parties
#and then has the parties move to the mean of their supporters for a certain number of iterations. 
master <- function(sims=10, n=1000, option=1, sig1=runif(1), sig2=runif(1), Sigma=matrix(c(10,3,3,2),2,2)){
  draw <- visualizing(n=n, option=option, sig1=sig1, sig2=sig2, Sigma=Sigma)
#  xlimit <- c(min(c(draw$voter.position.data[,1], draw$party.position[,1])), 
#              max(c(draw$voter.position.data[,1], draw$party.position[,1])))
#  ylimit <- c(min(c(draw$voter.position.data[,2], draw$party.position[,2])), 
#              max(c(draw$voter.position.data[,2], draw$party.position[,2])))
  for(i in 2:sims){
    party.positions.temp <- relocate(draw[[1]])
    draw <- affiliation2(voter.position=draw[[1]][,1:2], party.1=party.positions.temp[1,], 
                         party.2=party.positions.temp[2,])
  }  
}
  
####Problem 5####  
#First I create the visualizing2 function, which is like the visualizing function above except that it takes its own arguments
#for the values of the voters' and parties' ideal points. The voter.position argument should be a matrix containing
#voters' positions on issues as well as their party affiliation. The party.position argument should be a 2 by 2 matrix containing 
#the 2 parties' ideal point locations. Each row corresponds to a different party; each column corresponds to a different issue. 
#Finally xlimit and ylimit are both vectors of length 2 giving the x and y axis limits
visualizing2 <- function(voter.positions, party.position, xlimit, ylimit){
  plot(x=voter.positions[which(voter.positions$affiliation==1),1], y=voter.positions[which(voter.positions$affiliation==1),2],
       col="blue", pch=16, cex=0.25, xlab="First Dimension", ylab="Second Dimension", 
       xlim=xlimit, ylim=ylimit)
  #Second we add the points with affiliation scores of 2 (colored red)
  points(x=voter.positions[which(voter.positions$affiliation==2),1], y=voter.positions[which(voter.positions$affiliation==2),2],
         col="red", pch=16, cex=0.25)
  #Third we add the locations of the parties themselves, graphed as orange triangles
  points(x=party.position[,1], y=party.position[,2], col=c("orange", "green"), pch=17)
  legend(x="topright", legend=c("Vote for Party 1", "Vote for Party 2", "Party 1", "Party 2"), 
         pch=c(16, 16, 17, 17), col=c("blue", "red", "orange", "green"), cex=0.5)
}

#Now I rewrite the master() function to allow me to plot the positions of the voters and parties:
Master <- function(sims=10, n=1000, option=1, sig1=runif(1), sig2=runif(1), Sigma=matrix(c(10,3,3,2),2,2)){
  draw <- visualizing(n=n, option=option, sig1=sig1, sig2=sig2, Sigma=Sigma)
   xlimit <- c(min(c(draw$voter.position.data[,1], draw$party.position[,1])), 
               max(c(draw$voter.position.data[,1], draw$party.position[,1])))
   ylimit <- c(min(c(draw$voter.position.data[,2], draw$party.position[,2])), 
               max(c(draw$voter.position.data[,2], draw$party.position[,2])))
  for(i in 2:sims){
    party.positions.temp <- relocate(draw[[1]])
    draw <- affiliation2(voter.position=draw[[1]][,1:2], party.1=party.positions.temp[1,], 
                         party.2=party.positions.temp[2,])
    visualizing2(voter.positions=draw[[1]], party.position=draw[[2]], xlimit=xlimit, ylimit=ylimit)
  }  
}

#Note: the function windows() below opens a new window which will record the plots as they are generated. So running the master()
#function after this will save each plot that is generated by the master function. To access it, simply open the window by running 
#windows() function and use the page up/down keys to scroll through the different plots (or click on the History tab and click on 
#Previous or Next to scroll through the plots)
windows(record=TRUE)
Master()