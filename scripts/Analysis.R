IndependentT<- function(alldata,task, exp1 = "M", exp2 = "F") {
  data<-alldata[alldata$Experiment == task,]
  print(sprintf('this is the between subjects comparison of Males and females in %s Data', task))
  print('Aligned')
  print(t.test(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2])) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of 1st rotation')
  print(t.test(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2])) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2], na.rm = TRUE))
  print('End of 1st rotation')
  print(t.test(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2])) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of 2nd rotation')
  print(t.test(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2])) # not sig  A vs. NC
  print(cohen.d(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of Error Clamp')
  print(t.test(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2])) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2], na.rm = TRUE))
  print('End of Error Clamp (32 trials)')
  print(t.test(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2]))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2], na.rm = TRUE))
  sprintf("There are %.f Males out of %.f subjects", sum(data$Sex == "M"),sum(nrow(data)))
  
}

Overallttestofsex<- function(data){
print(t.test(data$Aligned[data$Sex == "M"],data$Aligned[data$Sex == "F"]))
print(t.test(data$R1_Early[data$Sex == "M"],data$R1_Early[data$Sex == "F"]))
print(t.test(data$R1_Late[data$Sex == "M"],data$R1_Late[data$Sex == "F"]))
print(t.test(data$EC_Late[data$Sex == "M"],data$EC_Late[data$Sex == "F"]))
sprintf("There are %.f Males out of %.f subjects", sum(data$Sex == "M"),sum(nrow(data)))
}


exposuresexcomparison<- function(){
  EC_early<-colMeans(exposure_reaches[1:8,2:33], na.rm = TRUE)
  EC_Late<-colMeans(exposure_reaches[41:48,2:33], na.rm = TRUE)
  E_RM<- cbind(EC_early, EC_Late, Demos[Demos$Experiment == "Exposure",1:2])
  print(t.test(E_RM$EC_early[E_RM$Sex == "M"],E_RM$EC_early[E_RM$Sex == "F"]))
  print(t.test(E_RM$EC_Late[E_RM$Sex == "M"],E_RM$EC_Late[E_RM$Sex == "F"]))
  sprintf("There are %.f Males out of %.f subjects", sum(E_RM$Sex == "M"),sum(nrow(E_RM)))
}

VariationTcombine<- function(data, give = "RM"){
  
  start<- seq(from = 50, to = 480, by = 12)
  start<- start+8 #this means i am taking the last 4 trials from each rotation, making this smaller means i take more trials.
  stop<- seq(from = 61, to = 481, by = 12)
  stop[36]<- stop[36]-2
  
  V_RM<- data.frame(rep(NA, times = 33))
  
  for (i in 1:36){
    V_RM[,i]<-colMeans(data[start[i]:stop[i],], na.rm = TRUE)
  }

  
  aligned<- colMeans(data[46:49,2:33], na.rm = TRUE)
  rotation<-c(0,as.numeric(V_RM[1,]))
  colnames(V_RM)<- c(as.character(V_RM[1,]))
  V_RM<- V_RM[-1,]
  V_RM<- cbind(aligned, V_RM, Demos[Demos$Experiment == "Variation",1:2])
  
  print(t.test(V_RM$aligned[V_RM$Sex == "M"],V_RM$aligned[V_RM$Sex == "F"]))

  sprintf("There are %.f Males out of %.f subjects", sum(data$Sex == "M"),sum(nrow(data)))
  
  
  if (give == "RM") {
  return(V_RM)
  }else {
    
    return(rotation)
  }
  
}


Plotlearningovertime<- function(){

  
  ##pull in the necessary data to make the plots
Vrot<-VariationTcombine(variation_reaches,1)
V_RM<-VariationTcombine(variation_reaches)
V_PM<-VariationTcombine(variation_localization)
locs<-colMeans(V_PM[,1:37], na.rm = TRUE)
reachs<-colMeans(V_RM[,1:37], na.rm = TRUE)


#Make all the data go in the same direction, some rotations are positive

for (i in 1:37){
if (reachs[i] <0){
  
  reachs[i]<-reachs[i]*-1
} else{
 #print("Done")
}
  
}

for (i in 1:37){
  if (locs[i] <0){
    
    locs[i]<-locs[i]*-1
  } else{
   # print("Done")
  }
  
}

for (i in 1:37){
  if (Vrot[i] <0){
    
    Vrot[i]<-Vrot[i]*-1
  } else{
    #print("Done")
  }
  
}

#Remove "INF" and O compensation trials, this wy it only includes trials that had a rotation

for (i in 1:37) locs[i]<-round((locs[i]/Vrot[i])*100)
percents<-as.numeric(unlist(locs[locs>5]))
percents<-as.numeric(unlist(percents[percents<100]))
for (i in 1:37) reachs[i]<-round((reachs[i]/Vrot[i])*100)
REpercents<-as.numeric(unlist(reachs[reachs>5]))
REpercents<-as.numeric(unlist(REpercents[REpercents<100]))



#Plot the two separate lines that show how much they compensate over time. 

plot(x = seq(from = 1, to = length(percents)), y = percents, type = "l", 
     xlab = "Time", ylab = "Percent Compensation", ylim = c(0,100), col = "Red",
     axes = FALSE, cex.lab = 1.5)
axis(2, at = c(0, 20, 40, 60, 80, 100), cex.axis = 1.5,
     las = 2)
#axis(1,labels = c(50, 265, 480) ,at = c(1,12,24), cex.axis = 1.5 )
lines(x = seq(from = 1, to = length(percents)), y = REpercents, col= "Blue")
legend(
  1,
  17,
  legend = c(
    'Reaches',
    'Localizations'),
  col = c('blue', 'red'),
  lty = c(1),
  
  
  lwd = c(2),
  bty = 'n', 
  cex = 1.2
)


#Run regression comparing the percent compensation to the values 1:24. 
reachreg<-lm(REpercents~x)
summary(reachreg)
locreg<-lm(percents~x)
summary(locreg)

##adding lines of best fit to plot
abline(lm(REpercents~x), col = "Blue", lty = 3)
abline(lm(percents~x), col = "Red", lty = 3 )
}




Sexcomparisonexp1<- function(alldata, exp1 = "M", exp2 = "F") {
  data<-alldata[alldata$Experiment == "Active" | alldata$Experiment == "Passive" | alldata$Experiment == "No-Cursor"| alldata$Experiment == "No-CursorI",]
  
  print(sprintf('this is the between subjects comparison of Males and females in %s Data', task))
  print('Aligned')
  print(t.test(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2])) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of 1st rotation')
  print(t.test(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2])) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2], na.rm = TRUE))
  print('End of 1st rotation')
  print(t.test(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2])) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of 2nd rotation')
  print(t.test(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2])) # not sig  A vs. NC
  print(cohen.d(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of Error Clamp')
  print(t.test(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2])) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2], na.rm = TRUE))
  print('End of Error Clamp (32 trials)')
  print(t.test(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2]))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2], na.rm = TRUE))
  sprintf("There are %.f Males out of %.f subjects", sum(data$Sex == "M"),sum(nrow(data)))
  
}

Sexcomparisonexp2Reach<- function(alldata, exp1 = "M", exp2 = "F") {
  data<-alldata[alldata$Experiment == "Terminal" | alldata$Experiment == "Passive" | alldata$Experiment == "Exposure"| alldata$Experiment == "Active" ,]
  
  print(sprintf('this is the between subjects comparison of Males and females in %s Data', task))
  print('Aligned')
  print(t.test(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2])) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of 1st rotation')
  print(t.test(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2])) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2], na.rm = TRUE))
  print('End of 1st rotation')
  print(t.test(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2])) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of 2nd rotation')
  print(t.test(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2])) # not sig  A vs. NC
  print(cohen.d(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of Error Clamp')
  print(t.test(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2])) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2], na.rm = TRUE))
  print('End of Error Clamp (32 trials)')
  print(t.test(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2]))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2], na.rm = TRUE))
  
  sprintf("There are %.f Males out of %.f subjects", sum(data$Sex == "M"),sum(nrow(data)))
}

Sexcomparisonexp2prop<- function(alldata, exp1 = "M", exp2 = "F") {
  data<-alldata[alldata$Experiment == "Exposure" | alldata$Experiment == "Passive" | alldata$Experiment == "Terminal",]
  
  print(sprintf('this is the between subjects comparison of Males and females in %s Data', task))
  print('Aligned')
  print(t.test(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2])) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of 1st rotation')
  print(t.test(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2])) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2], na.rm = TRUE))
  print('End of 1st rotation')
  print(t.test(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2])) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of 2nd rotation')
  print(t.test(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2])) # not sig  A vs. NC
  print(cohen.d(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of Error Clamp')
  print(t.test(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2])) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2], na.rm = TRUE))
  print('End of Error Clamp (32 trials)')
  print(t.test(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2]))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2], na.rm = TRUE))
  sprintf("There are %.f Males out of %.f subjects", sum(data$Sex == "M"),sum(nrow(data)))
  
}


Sexcomparisonexp1prop<- function(alldata, exp1 = "M", exp2 = "F") {
  data<-alldata[alldata$Experiment == "Active" | alldata$Experiment == "Passive" ,]
  
  print(sprintf('this is the between subjects comparison of Males and females in %s Data', task))
  print('Aligned')
  print(t.test(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2])) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of 1st rotation')
  print(t.test(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2])) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2], na.rm = TRUE))
  print('End of 1st rotation')
  print(t.test(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2])) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of 2nd rotation')
  print(t.test(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2])) # not sig  A vs. NC
  print(cohen.d(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of Error Clamp')
  print(t.test(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2])) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2], na.rm = TRUE))
  print('End of Error Clamp (32 trials)')
  print(t.test(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2]))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2], na.rm = TRUE))
  
  sprintf("There are %.f Males out of %.f subjects", sum(data$Sex == "M"),sum(nrow(data)))
}


Sexcomparisonexp1REA<- function(alldata, exp1 = "M", exp2 = "F") {
  data<-alldata[alldata$Experiment == "No-Cursor" | alldata$Experiment == "No-CursorI" ,]
  
  print(sprintf('this is the between subjects comparison of Males and females in %s Data', task))
  print('Aligned')
  print(t.test(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2])) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Sex == exp1],data$Aligned[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of 1st rotation')
  print(t.test(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2])) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Sex == exp1],data$R1_Early[data$Sex == exp2], na.rm = TRUE))
  print('End of 1st rotation')
  print(t.test(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2])) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Late[data$Sex == exp1],data$R1_Late[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of 2nd rotation')
  print(t.test(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2])) # not sig  A vs. NC
  print(cohen.d(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R2[data$Sex == exp1],data$R2[data$Sex == exp2], na.rm = TRUE))
  print('Beginning of Error Clamp')
  print(t.test(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2])) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC[data$Sex == exp1],data$EC[data$Sex == exp2], na.rm = TRUE))
  print('End of Error Clamp (32 trials)')
  print(t.test(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2]))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC_Late[data$Sex == exp1],data$EC_Late[data$Sex == exp2], na.rm = TRUE))
  sprintf("There are %.f Males out of %.f subjects", sum(data$Sex == "M"),sum(nrow(data)))
  
}
