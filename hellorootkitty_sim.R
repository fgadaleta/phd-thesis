numiter <- 3000
numobjs <- 15000        #number of protected objects
blocksize <- 100        #number of objects checked at each iteration (task switch)
numswitch <- numobjs/blocksize   # number of task switches to check all protected objects
freq <- 0
attackrate <- 10     # compromise objects every attackrate taskswitches
detnum <- 0
detected <- 0
detections<-0       # number of detected objects per block 
prob <- rep(0, numiter)
#protected <- rep(0, numobjs)                             #indices of objects to be protected 
checked   <- matrix(0,nrow=numswitch, ncol=blocksize)     # matrix 150x100 of obj to be checked
obj <- 1:numobjs
checked <- sample(obj)    # random permutation of protected objects
dim(checked) <- c(numswitch, blocksize) 
#plot(1,1,log="xy", pch=20, col="blue", xlab="Value", ylab="Frequency")
# Make an empty chart
plot(1, 1, pch=20, col="blue", xlim=c(1, numiter), ylim=c(0,1), type="n", main="Title", xlab="Switches", ylab="Frequency")
axis(1, at=seq(0,numiter,50))
abline(v=(seq(0,numiter,50)), col="lightgray", lty="dotted")
#compromise first time
compromised   <- sample(1:numobjs, 1)      # index of compromised object
cum <- 0
#j<-0
###################################################################################
for(t in 1:numiter) {                                             # simulation time
      #detected <- j*blocksize/numobjs
      #j <- j+1
      #detected <- 0
      
      # TODO randomly compromise 
      # TODO visualize compromised object
      if(sample(0:1,1,replace=T)==1) {
        if((t %% attackrate) == 0) {
          compromised <- sample(1:numobjs, 1, TRUE)      # clear and compromise again
        }
      }
      
      #for(block in 1:numswitch) {
        #sample 100 objects to be checked every 150 task switches
        if((t %% numswitch)==0)
        {
          #compromised <- sample(1:numobjs, 1, TRUE)      # compromise again
          #j <- 0
          cum <- 1
      
          #store and reset counter 
          detections <- append(detections, detnum)
          detnum<-0
        }
          
        # check if compromised object is actually checked
        if(any(checked[j,] == compromised)) {
          detected <- 1
          detnum <- detnum+1
          freq <- c(freq, detnum)
          }
          
      #points(t, cum, pch=23, col="red")
      ###points(t, detected, pch=20, col="blue")
      #points(t, density, pch=20)
      
}