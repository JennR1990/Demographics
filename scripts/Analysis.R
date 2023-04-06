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
  print('End of 2nd rotation')
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
print(sprintf("There are %.f Males out of %.f subjects", sum(data$Sex == "M"),sum(nrow(data))))
}


exposuresexcomparison<- function(){
  EC_early<-colMeans(exposure_reaches[1:8,2:33], na.rm = TRUE)
  EC_Late<-colMeans(exposure_reaches[41:48,2:33], na.rm = TRUE)
  E_RM<- cbind(EC_early, EC_Late, Demos[Demos$Experiment == "Exposure",1:2])
  print(t.test(E_RM$EC_early[E_RM$Sex == "M"],E_RM$EC_early[E_RM$Sex == "F"]))
  print(t.test(E_RM$EC_Late[E_RM$Sex == "M"],E_RM$EC_Late[E_RM$Sex == "F"]))
  print(sprintf("There are %.f Males out of %.f subjects", sum(E_RM$Sex == "M"),sum(nrow(E_RM))))
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


Plotlearningovertimebyblock<- function(){

  
  ##pull in the necessary data to make the plots
Vrot<-VariationTcombine(variation_reaches,1)
V_RM<-VariationTcombine(variation_reaches)
V_PM<-VariationTcombine(variation_localization)
locs<-colMeans(V_PM[,1:37], na.rm = TRUE)
reachs<-colMeans(V_RM[,1:37], na.rm = TRUE)

#plots reaches ~ prop for last four trials of each rotation
plot(reachs, locs, main = "Last four trials of each rotation", xlab = "Reaches", ylab = "Localizations")



#Make all the data go in the same direction, some rotations are positive

for (i in 1:length(reachs)){
if (reachs[i] <0){
  
  reachs[i]<-reachs[i]*-1
} else{
 #print("Done")
}
  
}

for (i in 1:length(reachs)){
  if (locs[i] <0){
    
    locs[i]<-locs[i]*-1
  } else{
   # print("Done")
  }
  
}

for (i in 1:length(reachs)){
  if (Vrot[i] <0){
    
    Vrot[i]<-Vrot[i]*-1
  } else{
    #print("Done")
  }
  
}

#plots reaches ~ prop for last four trials of each rotation, but everything is positive
plot(reachs, locs, main = "Last four trials of each rotation", xlab = "Reaches", ylab = "Localizations")

#Remove "INF" and O compensation trials, this wy it only includes trials that had a rotation

for (i in 1:length(reachs)) locs[i]<-round((locs[i]/Vrot[i])*100)
percents<-as.numeric(unlist(locs[locs>5]))
percents<-as.numeric(unlist(percents[percents<100]))
for (i in 1:length(reachs)) reachs[i]<-round((reachs[i]/Vrot[i])*100)
REpercents<-as.numeric(unlist(reachs[reachs>5]))
REpercents<-as.numeric(unlist(REpercents[REpercents<100]))



#Plot the two separate lines that show how much they compensate over time. 

plot(x = seq(from = 1, to = length(percents)), y = percents, type = "l", 
     xlab = "Time", ylab = "Compensation [%]", ylim = c(0,100), col = "Red",
     axes = FALSE, cex.lab = 1.5, main = "Last four trials of each rotation")
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
reachreg<-lm(REpercents~seq(from = 1, to = length(percents)))
print(summary(reachreg))
locreg<-lm(percents~seq(from = 1, to = length(percents)))
print(summary(locreg))

##adding lines of best fit to plot
abline(lm(REpercents~seq(from = 1, to = length(percents))), col = "Blue", lty = 3)
abline(lm(percents~seq(from = 1, to = length(percents))), col = "Red", lty = 3 )
}



Plotlearningovertimealltrials<- function(){
  
  
  ##pull in the necessary data to make the plots
  Vrot<- variation_reaches[,1]
  reachs<-rowMeans(variation_reaches[,2:33], na.rm = TRUE)
  locs<-rowMeans(variation_localization[,2:33], na.rm = TRUE)

  
  #plots reaches ~ prop for last four trials of each rotation
  plot(reachs, locs, main = "All trials", xlab = "Reaches", ylab = "Localizations")
  
  
  
  #Make all the data go in the same direction, some rotations are positive
  
  for (i in 1:length(reachs)){
    if (reachs[i] <0){
      
      reachs[i]<-reachs[i]*-1
    } else{
      #print("Done")
    }
    
  }
  
  for (i in 1:length(reachs)){
    if (locs[i] <0){
      
      locs[i]<-locs[i]*-1
    } else{
      # print("Done")
    }
    
  }
  
  for (i in 1:length(reachs)){
    if (Vrot[i] <0){
      
      Vrot[i]<-Vrot[i]*-1
    } else{
      #print("Done")
    }
    
  }
  
  #plots reaches ~ prop for last four trials of each rotation, but everything is positive
  plot(reachs, locs, main = "Last four trials of each rotation", xlab = "Reaches", ylab = "Localizations")
  
  #Remove 360 and O compensation trials, this way it only includes trials that had a rotation
  
  rot<- c()
  reach<- c()
  loc<- c()
  for (i in 1:length(reachs)){
    
    if (Vrot[i] == 360 || Vrot[i] == 0){
      
      print("bad") 
  } else {
   rot[i]<- Vrot[i]
   reach[i]<- reachs[i]
   loc[i]<- locs[i]
  }
  }
  rot<- rot[!is.na(rot)]
  loc<- loc[!is.na(loc)]
  reach<- reach[!is.na(reach)]
  
  for (i in 1:length(reach)) loc[i]<-round((loc[i]/rot[i])*100)
  percents<-as.numeric(unlist(loc[!is.na(loc)]))
  percents<-loc
  #percents<-as.numeric(unlist(percents[percents<100]))
  for (i in 1:length(reach)) reach[i]<-round((reach[i]/rot[i])*100)
  REpercents<- reach
  REpercents<-as.numeric(unlist(reach[!is.na(reach)]))
  #REpercents<-as.numeric(unlist(REpercents[REpercents<100]))
  
  
  
  #Plot the two separate lines that show how much they compensate over time. 
  
  plot(x = seq(from = 1, to = length(percents)), y = percents, type = "l", 
       xlab = "Time", ylab = "Percent Compensation", ylim = c(0,100), col = "Red",
       axes = FALSE, cex.lab = 1.5)
  
  axis(2, at = c(0, 20, 40, 60, 80, 100), cex.axis = 1.5,
       las = 2)
  #axis(1,labels = c(50, 265, 480) ,at = c(1,12,24), cex.axis = 1.5 )
  lines(x = seq(from = 1, to = length(REpercents)), y = REpercents, col= "Blue")
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
  reachreg<-lm(REpercents~seq(from = 1, to = length(percents)))
  print(summary(reachreg))
  locreg<-lm(percents~seq(from = 1, to = length(percents)))
  print(summary(locreg))
  
  ##adding lines of best fit to plot
  abline(lm(REpercents~seq(from = 1, to = length(percents))), col = "Blue", lty = 3)
  abline(lm(percents~seq(from = 1, to = length(percents))), col = "Red", lty = 3 )
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



PrepreachdataforT<- function(pasdata, termdata){
  #
  
  Pas_RM<-TCombine(pasdata)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  
  Term_RM<-TCombine(termdata)
  Term_RM$Experiment <- rep('Terminal', nrow(Term_RM))
  
  Pas_RM<-TCombine(pausedata)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  
  Term_RM<-TCombine(nocursordata)
  Term_RM$Experiment <- rep('Terminal', nrow(Term_RM))
  
  
  
  
  
  
  AllDataRM<- rbind(Pas_RM, Term_RM)
  # 
  return(AllDataRM)
}


PrepdataforANOVA <- function(pasdata, termdata) {
  
  
  Pas_RM<-ANOVAcombine(pasdata)
  Pas_RM$ID <- sprintf('PasLoc.%s',Pas_RM$ID)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  
  Term_RM<-ANOVAcombine(termdata)
  Term_RM$ID <- sprintf('Terminal.%s',Term_RM$ID)
  Term_RM$Experiment <- rep('Terminal', nrow(Term_RM))
  
  
  AllDataRM<- rbind(Pas_RM, Term_RM)
  #
  return(AllDataRM)
  
}


PrepANOVARebounds<- function (pas, term, expo){
  
  
  rebounds<- data.frame()
  Experiment<- c(rep("Passive", times = 32), rep("Terminal", times = 32), rep("Exposure", times = 32))
  ID<- c(rep(1:32,times = 3))
  EC_Late<- colMeans(pas[273:288,2:33], na.rm = TRUE)
  EC_Late<- c(EC_Late,colMeans(term[273:288,2:33], na.rm = TRUE))
  EC_Late<- c(EC_Late,(colMeans(expo[33:48,2:33], na.rm = TRUE)*-1))
  begins<- c(rep(1:32,times = 3))
  ends<-c(rep("p", times = 32), rep("t", times = 32), rep("e", times = 32))
  ID<-paste(begins, ends, sep = ".")
  
  rebounds<-data.frame(cbind(EC_Late, Experiment, ID))
  rebounds$EC_Late<- as.numeric(rebounds$EC_Late)
  
  return(rebounds)
}


TCombine<- function(data) {
  ParticipantRM<- data.frame()
  participants <- c(2:ncol(data))
  for (participant in participants){
    Aligned<- mean(unlist(data[61:64,participant]), na.rm = TRUE)
    r1<- unlist(data[65,participant])
    r2<- unlist(data[66,participant])
    r3<- unlist(data[67,participant])
    r4<- unlist(data[68,participant])
    R1_Early<- mean(unlist(data[65:68,participant]), na.rm = TRUE)
    R1_second<-mean(unlist(data[69:72,participant]), na.rm = TRUE) 
    R1_Late<- mean(unlist(data[221:224,participant]), na.rm = TRUE)
    R2<- mean(unlist(data[237:240,participant]), na.rm = TRUE)
    EC<- mean(unlist(data[241:244,participant]), na.rm = TRUE)
    EC1<- mean(unlist(data[241,participant]), na.rm = TRUE)
    EC2<- mean(unlist(data[242,participant]), na.rm = TRUE)
    EC3<- mean(unlist(data[243,participant]), na.rm = TRUE)
    EC4<- mean(unlist(data[244,participant]), na.rm = TRUE)
    EC_Late<- mean(unlist(data[273:288,participant]), na.rm = TRUE)
    RM<- data.frame(Aligned, r1, r2,r3,r4, R1_Early, R1_second, R1_Late, R2, EC, EC_Late, EC1, EC2, EC3, EC4)
    if (prod(dim(ParticipantRM)) == 0) {
      ParticipantRM <- RM
    } else {
      ParticipantRM <- rbind(ParticipantRM, RM)
    }
  }
  return(ParticipantRM)
}

##repeated measures combine for ANOVAs

ANOVAcombine<- function(data) {
  ParticipantARM<- data.frame()
  participants <- names(data)[2:dim(data)[2]]
  epochs <- list('R1_early'=c(65,4), 'R1_late'=c(221,4), 'R2D2'=c(237,4), 'EC'=c(273,16))
  Deviations<- c()
  Time<- c()
  ID<- c()
  
  for (participant in participants){
    
    participant_reaches <- unlist(data[,participant])
    
    for (epoch in names(epochs)) {
      
      start <- epochs[[epoch]][1]
      finish <- start -1 + epochs[[epoch]][2]
      Deviations <- c(Deviations, mean(participant_reaches[start:finish], na.rm=TRUE))
      Time <- c(Time, epoch)
      ID <- c(ID, participant)
      ANOVARM<- data.frame(Deviations, Time, ID)
    }
  }
  #b<- !is.nan(ANOVARM$Reaches)
  # ANOVARM<- ANOVARM[c(b),]
  return(ANOVARM)
}



etaSquaredTtest <- function(g1,g2=NA,mu=0,na.rm=TRUE) {
  
  doOneSample <- FALSE
  doTwoSample <- FALSE
  
  if (length(g2) == 1) {
    if (is.na(g2)) {
      doOneSample <- TRUE
    } else {
      # set mu to the single value in g2 and do a one sample one anyway?
    }
  } else {
    doTwoSample <- TRUE
  }
  
  if (doOneSample) {
    
    # compare group 1 mean with mu as explanation
    SStotal <- sum((g1-mean(g1,na.rm=na.rm))^2)
    SSeffect <- sum(((mean(g1, na.rm=na.rm) - mu)^2)*length(g1))
    # 
    # 
    return(SSeffect / SStotal)
    
  }
  
  if (doTwoSample) {
    
    overallmean <- mean(c(g1,g2),na.rm=na.rm)
    # compare overall mean with group means as explanation
    SStotal <- sum((c(g1,g2) - overallmean)^2, na.rm=na.rm)
    SSeffect <- sum(length(g1)*(mean(g1,na.rm=na.rm)-overallmean)^2, length(g2)*(mean(g2,na.rm=na.rm)-overallmean)^2)
    return(SSeffect / SStotal)
    
  }
  
}



ANOVAanalysis<- function(AllDataANOVA){
  AllDataANOVA$ID<- as.factor(AllDataANOVA$ID)
  AllDataANOVA$Experiment<- as.factor(AllDataANOVA$Experiment)
  AllDataANOVA$Time<- as.factor(AllDataANOVA$Time)
  AllDataANOVA$Sex<- as.factor(AllDataANOVA$Sex)
  fullmodel <- ezANOVA(data=AllDataANOVA,
                       dv=Deviations,
                       wid=ID,
                       within=Time,
                       between = .(Experiment,Sex),
                       type=3,
                       return_aov=TRUE)
  return(fullmodel)
}


NoCursorsTCombine<- function(data) {
  ParticipantRM<- data.frame()
  participants <- c(2:ncol(data))
  for (participant in participants){
    Aligned<- mean(unlist(data[29:32,participant]), na.rm = TRUE)
    r1<- unlist(data[33,participant])
    r2<- unlist(data[34,participant])
    r3<- unlist(data[35,participant])
    r4<- unlist(data[36,participant])
    R1_Early<- mean(unlist(data[33:36,participant]), na.rm = TRUE)
    R1_second<- mean(unlist(data[37:40,participant]), na.rm = TRUE)
    R1_third<- mean(unlist(data[41:44,participant]), na.rm = TRUE)
    R1_forth<-mean(unlist(data[45:48,participant]), na.rm = TRUE) 
    R1_fifth<- mean(unlist(data[49:52,participant]), na.rm = TRUE)
    R1_sixth<-mean(unlist(data[53:56,participant]), na.rm = TRUE) 
    R1_Late<- mean(unlist(data[189:192,participant]), na.rm = TRUE)
    R2<- mean(unlist(data[205:208,participant]), na.rm = TRUE)
    EC<- mean(unlist(data[209:212,participant]), na.rm = TRUE)
    EC_Late<- mean(unlist(data[241:256,participant]), na.rm = TRUE)
    EC1<- mean(unlist(data[209,participant]), na.rm = TRUE)
    EC2<- mean(unlist(data[210,participant]), na.rm = TRUE)
    EC3<- mean(unlist(data[211,participant]), na.rm = TRUE)
    EC4<- mean(unlist(data[212,participant]), na.rm = TRUE)
    RM<- data.frame(Aligned, r1, r2,r3,r4, R1_Early, R1_second, R1_third, R1_forth, R1_fifth, R1_sixth, R1_Late, R2, EC, EC_Late, EC1, EC2, EC3, EC4)
    if (prod(dim(ParticipantRM)) == 0) {
      ParticipantRM <- RM
    } else {
      ParticipantRM <- rbind(ParticipantRM, RM)
    }
  }
  return(ParticipantRM)
}

NoCursorACombine<- function(data) {
  ParticipantARM<- data.frame()
  participants <- names(data)[2:dim(data)[2]]
  epochs <- list('R1_early'=c(33,4), 'R1_late'=c(189,4), 'R2D2'=c(205,4), 'EC'=c(241,16))
  Deviations<- c()
  Time<- c()
  ID<- c()
  
  for (participant in participants){
    
    participant_reaches <- unlist(data[,participant])
    
    for (epoch in names(epochs)) {
      
      start <- epochs[[epoch]][1]
      finish <- start -1 + epochs[[epoch]][2]
      Deviations <- c(Deviations, mean(participant_reaches[start:finish], na.rm=TRUE))
      Time <- c(Time, epoch)
      ID <- c(ID, participant)
      ANOVARM<- data.frame(Deviations, Time, ID)
    }
  }
  #  b<- !is.nan(ANOVARM$Reaches)
  #  ANOVARM<- ANOVARM[c(b),]
  return(ANOVARM)
}
