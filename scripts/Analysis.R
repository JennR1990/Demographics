IndependentT<- function(data, exp1, exp2, task) {
  print(sprintf('this is the between subjects comparison of Males and females in %s Data', task))
  print('Aligned')
  print(t.test(data$Aligned[data$Experiment == exp1],data$Aligned[data$Experiment == exp2])) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$Aligned[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$Aligned[data$Experiment == exp2], na.rm = TRUE))
  print('Beginning of 1st rotation')
  print(t.test(data$R1_Early[data$Experiment == exp1],data$R1_Early[data$Experiment == exp2])) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Experiment == exp1],data$R1_Early[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Experiment == exp1],data$R1_Early[data$Experiment == exp2], na.rm = TRUE))
  print('End of 1st rotation')
  print(t.test(data$R1_Late[data$Experiment == exp1],data$R1_Late[data$Experiment == exp2])) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Experiment == exp1],data$R1_Late[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Late[data$Experiment == exp1],data$R1_Late[data$Experiment == exp2], na.rm = TRUE))
  print('Beginning of 2nd rotation')
  print(t.test(data$R2[data$Experiment == exp1],data$R2[data$Experiment == exp2])) # not sig  A vs. NC
  print(cohen.d(data$R2[data$Experiment == exp1],data$R2[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R2[data$Experiment == exp1],data$R2[data$Experiment == exp2], na.rm = TRUE))
  print('Beginning of Error Clamp')
  print(t.test(data$EC[data$Experiment == exp1],data$EC[data$Experiment == exp2])) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Experiment == exp1],data$EC[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC[data$Experiment == exp1],data$EC[data$Experiment == exp2], na.rm = TRUE))
  print('End of Error Clamp (32 trials)')
  print(t.test(data$EC_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp2]))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$EC_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp2], na.rm = TRUE))
  
  
}

t.test(TtestPdata$Aligned[TtestPdata$Sex == "M"],TtestPdata$Aligned[TtestPdata$Sex == "F"])
t.test(TtestPdata$R1_Early[TtestPdata$Sex == "M"],TtestPdata$R1_Early[TtestPdata$Sex == "F"])
t.test(TtestPdata$R1_Late[TtestPdata$Sex == "M"],TtestPdata$R1_Late[TtestPdata$Sex == "F"])
t.test(TtestPdata$EC_Late[TtestPdata$Sex == "M"],TtestPdata$EC_Late[TtestPdata$Sex == "F"])

