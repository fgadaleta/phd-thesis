#####################################################################
## Probabilistic analysis of BUBBLE countermeasure 
## described in https://lirias.kuleuven.be/handle/123456789/316504
##
## Copyright (c) 2012 Francesco Gadaleta
#####################################################################
set.seed(11)


bubble <- function(numexp=100, sprayedmem = 1, every = 26, codesize=25) {   
  hits <- 0                              
  membytes <- sprayedmem *1024*1024       # sprayed memory in bytes
  n <- round(membytes/every)              # numbers of blocks  
    
  interrupts <- sample(1:every, n, replace=TRUE)        # number of interruptions
  space <- rep(0,membytes)                # zero memory
  space[(1:n-1)*every + interrupts] <- 1  # interrupted memory space 
    
    
    for(exp in 1:numexp) {              # numbers of experiments
      #at each experiment reset memory and re-randomise 
      #interrupts <- sample(1:every, n, replace=TRUE)        # number of interruptions
      #space <- rep(0,membytes)                # zero memory
      #space[(1:n-1)*every + interrupts] <- 1  # interrupted memory space 
      
      # generate position
      start <- sample(1:(membytes-codesize), 1)
      end <- start+codesize      
      # inject at position x size 25 with a jump in between 
      
      #check if hit interruption from beginning to end of injected code
      if(sum(space[start:end])>0) {
          hits <- hits+1
      }
  }
    
  # indices of all successfull attacks (not detected)
  #index  = which(detat>numswitch)   
  # for graphics purpose only!! After 150 we don't care anyway
  #rnd = rnorm(length(index),10, 50)
  #rnd = runif(length(index),1,100)
  #detat[index] <- detat[index]-rnd
  # probability of successfull attack
  #attsucc<- 100*length(index)/numexp
  detrate <- hits/numexp   # rate of detection
  #result <- list(res=detat, prob=attsucc, rate=restorerate, hacked=numattacked, numexp=numexp)
  result <- list(res=hits, prob=detrate)
  return(result)
}


##  Set up plotting in two rows and three columns, plotting along rows first.
#par( mfrow = c( 2, 3 ) )

#Create a function to generate a continuous color palette
#rbPal <- colorRampPalette(c('black','black')) 

####################################################
#    plot Prob(detect) vs Size of injected code    #
####################################################
i <- 0        # for plotting 
len <- 0      # code size from len[1] to len[2]
len[1] <- 1
len[2] <- 25

prob <- rep(0,len[2]-len[1]-1)
sz <- rep(0,len[2]-len[1]-1)

while(len[1]<len[2]) {
  size = len[1]
  interr <- 25
  res <- bubble(numexp=500, sprayedmem=1, every=interr, codesize=size)
  #This adds a column of color values
  # based on the y values
  #Col <- rbPal(10)[as.numeric(cut(res$res, breaks = 8))]
  prob[i] <- res$prob
  sz[i] <- size
  i <- i+1
  len[1]<-len[1]+1
  #print(len[1]<-len[1]+1)
}

  plot(sz, prob, pch=19, cex=.6, 
       xlab=paste("Code size", "(interrupted every ", interr, " bytes)"), 
       ylab="P(det)", 
       main = paste("Prob(detection) vs codesize"), 
       cex.lab = 1.1)
  #abline(a=150, b=0, col=2)
  
smooth = smooth.spline(sz,prob,spar=0.5)
lines(smooth, lty=1, col="red", lwd=1.8)

axis(1, seq(0, 25, by=1), FALSE)
###########################################################


plot.new()


####################################################
#     prob of detection vs interruption rate       #
####################################################
i <- 0        # for plotting 
len <- 0      # interruption rates from len[1] to len[2]
len[1] <- 24
len[2] <- 60
codesize <- 15
col <- c("red", "blue", "green", "black", "yellow")
prob <- rep(0,len[2]-len[1]-1)
x <- rep(0,len[2]-len[1]-1)

# plot skeleton 
plot(1, ann=FALSE,type="n",
     pch=19, cex=.7, 
     xlim=c(24,60),
     ylim=c(0,1))

title("Prob(detection) vs Interr. rate", xlab="Interr. rate", ylab="P(det)")

j=0
for(j in 1:5) {
  size <- codesize*j
  print(paste("Codesize ", size))
  from <- len[1]
  to   <- len[2]
  
  
  while(from<to) {
          interr = from
          
          res <- bubble(numexp=100, sprayedmem=1, every=interr, codesize=size)
          #This adds a column of color values
          # based on the y values
          #Col <- rbPal(10)[as.numeric(cut(res$res, breaks = 8))]
          prob[i] <- res$prob
          x[i] <- interr
          i <- i+1
          from<-from+1
          #print(from<-from+1)
        }
        
        #plot(x, prob, pch=19, cex=.6, 
        #     xlab=paste("Int rate"), 
        #     ylab="P(det)", 
        #     main = paste("Prob(detection) vs Interr. rate"), 
        #     cex.lab = 1.1)
        
        smooth = smooth.spline(x,prob,spar=0.3)
        lines(smooth, lty=j, col=col[j], lwd=2)
        #lines(x, prob, col="red", lwd=2.5)
        #axis(1, seq(0, 250, by=5), FALSE)
}

labels <- c("CS=15", "CS=30", "CS=45","CS=60","CS=75")

legend("bottomleft", title="Size of injected code",
       labels, lwd=2, lty=1:5, col=col, xpd=NA, 
       plot=TRUE, cex=0.7, xjust=0, yjust=1, ncol=2)

axis(2, seq(0,1,by=0.1))


#####################################################################

















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
          tmp <- bubble(numexp=1000, restorerate=rate[tasksw], numattacked=obj+1)
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

