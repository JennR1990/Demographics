

first <- function(x) {
  return(x[1])
}

getGroupTargetAngles <- function(group='TimeModel1_Passive') {
  
  # this character sometimes shows up in other parts of the filename (not just the main folder)
  variant <- substr(group, 10,10)
  
  
  phases <- c(1,2,3,4)
  if (variant == '2') {
    phases <- c(1,2,3,4,5)
  }
  
  # to make processeing the data a little easier, we assign column names:
  columns <- c('phase','trialpair','trial','targetangle_deg','rotation_deg','time_ms','X1','Y1','X2','Y2','homex_cm','homey_cm','targetx_cm','targety_cm','a','b','c','d','e')
  
  # this data frame will have all the target angles in the end
  allTargetAngles <- NA
  
  # there have to be 32 participants!
  for (participant in c(1:32)) {
    
    print(participant)
    
    # we construct the folder for the participant (not relative to the project, so absolute path)
    folder <- sprintf('/home/marius/Science/TimeModel/%s/time_model%s_%d', group, variant, participant)
    if (variant == '5') {
      folder <- sprintf('/home/marius/Science/TimeModel/%s/time_model_activeloc_%d', group, participant)
    }
    
    # this will have all the target angles for the participant
    pTAdf <- NA
    
    ####################################
    ## NO CURSOR DOES NOT HAVE 4 PHASES!
    
    # we load the files for each phase of the task
    for (phase in phases) {
      
      # we construct the file name:
      filename <- sprintf('%d_%d__time_model_reach_selected.txt',participant,phase)
      if (variant == '5') {
        filename <- sprintf('%d_%d__time_model4_reach_selected.txt',participant,phase)
      }
      if (variant == '2') {
        filename <- sprintf('%d_%d__time_model2_reach_selected.txt',participant,phase)
      }
      
      # we load the file (from the folder) into a data frame:
      #pdf <- read.table(file=sprintf('%s/%s',folder,filename))
      #print(dim(pdf))
      pdf <- read.table(file=sprintf('%s/%s',folder,filename))
      
      if (dim(pdf)[2] == 19) {
        colnames(pdf) <-  columns
      }
      if (dim(pdf)[2] == 20) {
        colnames(pdf) <- c(columns, 'f')
      }
      
      #print(dim(pdf))
      
      # we get the target angles for each trial in the file:
      targetangles <- aggregate(targetangle_deg ~ trialpair, data=pdf, FUN=first)
      
      # concatenate all the data
      if (is.data.frame(pTAdf)) {
        pTAdf <- rbind(pTAdf, targetangles)
      } else {
        pTAdf <- targetangles
      }
      
    }
    
    pcol <- sprintf('p%d', participant)
    
    if (is.data.frame(allTargetAngles)) {
      allTargetAngles[,pcol] <- pTAdf$targetangle_deg
    } else {
      trial <- pTAdf$trialpair
      allTargetAngles <- data.frame(trial)
      allTargetAngles[,pcol] <- pTAdf$targetangle_deg
    }
    
  }
  
  return(allTargetAngles)
  
}

saveAllTargetAngles <- function() {
  
  for (groupno in c(1,2,3)) {
    
    df <- getGroupTargetAngles(group=c('TimeModel1_Passive','TimeModel2_Pause','TimeModel5_Active')[groupno])
    
    write.csv(df, 
              file=c('data/passive_reachtargets.csv','data/pause_reachtargets.csv','data/active_reachtargets.csv')[groupno],
              quote=F,
              row.names=F)
    
  }
  
}