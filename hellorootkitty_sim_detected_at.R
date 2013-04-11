#####################################################################
## Probabilistic analysis of HelloRootkitty countermeasure 
## described in https://lirias.kuleuven.be/handle/123456789/316504
##
## Copyright (c) 2012 Francesco Gadaleta
#####################################################################


set.seed(11)

hellorootkitty <- function(numexp=1000, numobjs=15000, blocksize=100, restorerate=150, numattacked = 2) {   
    numswitch <- numobjs/blocksize   # number of task switches to check all protected objects
    detat <- rep(300, numexp)        #time of detection per experiment
    
    for(exp in 1:numexp) {           # numbers of experiments
    obj <- 1:numobjs                 # create objects
    checked   <- matrix(0,nrow=numswitch, ncol=blocksize) # matrix 150x100 of obj to be checked
    checked  <- sample(obj)          # random permutation of protected objects
    dim(checked) <- c(numswitch, blocksize)
    
    compromised   <- sample(1:numobjs, numattacked)       # index of compromised object
    
    
    for(j in 1:numswitch) {
    #  if((j %% attackrate) == 0){
    # compromised <- sample(1:numobjs, numattacked, TRUE)      # clear and compromise again
    #    compromised <- rep(NaN,numattacked) # restore original content
    #    }
      
    # check if compromised object is checked (then it is detected)
    for(i in 1:numattacked) {
      # attacker restores after restorerate switches
      if(j >= restorerate) break 
      
      if(any(checked[j,] == compromised[i])) {
        detat[exp] <- j                  # save that you detected at iteration j 
        break
          }
      }
     if(detat[exp]<300) break 
      }  # for numswitch
  
  }


  # indices of all successfull attacks (not detected)
  index  = which(detat>numswitch)   
  
  # for graphics purpose only!! After 150 we don't care anyway
  #rnd = rnorm(length(index),10, 50)
  rnd = runif(length(index),1,100)
  detat[index] <- detat[index]-rnd
  
  # probability of successfull attack
  attsucc<- 100*length(index)/numexp
  result <- list(res=detat, prob=attsucc, rate=restorerate, hacked=numattacked, numexp=numexp)
  return(result)
}

##  Set up plotting in two rows and three columns, plotting along rows first.
par( mfrow = c( 2, 3 ) )

#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('black','black')) 

y<-0
while(y<6) {
  hacked <- 1+y*2
res <- hellorootkitty(numexp=1000, restorerate=100, numattacked=hacked)

  #This adds a column of color values
  # based on the y values
  Col <- rbPal(10)[as.numeric(cut(res$res,breaks = 8))]
    
  plot(res$res, pch=19, cex=.6, 
       xlab=paste("exp", "att.succ.", 
                  res$prob, "%"), 
       ylab="Detection iter", 
       col=Col, 
       main = paste(res$hacked, "obj. every", res$rate, "switches"), 
       cex.lab = 1.1)
  abline(a=150, b=0, col=2)

  
print(y<-y+1)
}




# prob of success vs. num of hacked objects
prob <- rep(0,10)
plot.new()
colors <- c("red", "blue", "darkgreen", "gold", "black")
linetype <- c(1:3)
plotchar <- seq(18,18+3,1)
##  Set up plotting in one row and one column, plotting along rows first.
par( mfrow = c( 1, 1 ) )
tasksw <- 1
rate <- c(75, 90, 120)

while(tasksw<4) {
  obj <- 0
  
        while(obj<10){        
          tmp <- hellorootkitty(numexp=1000, restorerate=rate[tasksw], numattacked=obj+1)
          prob[obj] <- tmp$prob/100
          print(obj<-obj+1)        
        }

  if(tasksw == 1) {
  
        plot(prob, cex=1, 
             col= colors[tasksw], type="b", lwd=2, pch=plotchar[tasksw],
             xlab="Compromised obj.", ylab="P(attack)",
             main=paste("Pr(attack) vs. compromised objs"),
             ylim=c(min(prob), max(prob)))
  }
  else 
    lines(prob, cex=1, col= colors[tasksw], 
          type="b", lwd=2, lty=linetype[tasksw], pch=plotchar[tasksw],
          xlab="Compromised obj.", ylab="Pr(attack)",
          main=paste("Pr(attack) vs. compromised objs"),
          ylim=c(min(prob), max(prob)))
    
  print(tasksw <- tasksw+1)
}

labels <- c("TS=75", "TS=90", "TS=120")
legend("topright", inset=.05, title="Restoring rate",
       labels, lwd=2, lty=c(1, 1, 1), col=colors, pch=plotchar)
