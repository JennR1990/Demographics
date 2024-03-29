Loaddata<- function (group='passive', task='reaches') {
  # filename <- sprintf('data/%s_%s.csv',group,task)
  # df <- read.csv(filename, stringsAsFactors=F)
  return(read.csv(sprintf('data/%s_%s.csv',group,task), stringsAsFactors=F))
}

loadalldata<- function () {
  pause_reaches<<- removeReachOutliers(Loaddata(group='pause'))
  active_reaches<<- removeReachOutliers(Loaddata(group='active'))
  passive_reaches<<- removeReachOutliers(Loaddata())
  nocursor_reaches<<- removeReachOutliers(Loaddata(group='nocursor'))
  nocursorI_reaches<<- removeReachOutliers(Loaddata(group='nocursor', task = 'NI_reaches'))
  nocursorI_reaches<<-nocursorI_reaches[,-9]
  newnocursor_reaches<<- cbind(nocursor_reaches, nocursorI_reaches[2:ncol(nocursorI_reaches)])
  terminal_reaches<<- removeReachOutliers(Loaddata(group='terminal'))
  exposure_reaches<<- removeReachOutliers(Loaddata(group='exposure'))
  variation_reaches<<- removeReachOutliers(Loaddata(group='variation'))
  
  
  passive_localization<<- removeReachOutliers(Loaddata(task = 'localization'))
  active_localization<<- removeReachOutliers(Loaddata(group='active', task = 'localization'))
  terminal_localization<<- removeReachOutliers(Loaddata(group='terminal', task = 'localization'))
  exposure_localization<<- removeReachOutliers(Loaddata(group='exposure', task = 'localization'))
  variation_localization<<- removeReachOutliers(Loaddata(group='variation', task = 'localizations'))
  
  nocursor_nocursors<<- removeReachOutliers(Loaddata(group='nocursor', task = 'nocursors'))
  nocursorI_nocursors<<- removeReachOutliers(Loaddata(group='nocursor', task = 'NI_nocursors'))
  nocursorI_nocursors<<-nocursorI_nocursors[,-9]
  newnocursor_nocursors<<- cbind(nocursor_nocursors, nocursorI_nocursors[2:ncol(nocursorI_nocursors)])
 
  
  
  
  pause_angles<<- Loaddata(group='Pause', task = "Angles")
  active_angles<<- Loaddata(group='Active', task = "Angles")
  passive_angles<<- Loaddata(group = "Pause", task = "Angles")
  nocursor_angles<<- Loaddata(group='No-Cursor', task = "Angles")
  
  Instructed<<- Loaddata(group = "Instructed_No-Cursors", task = "MovementTimes")
  uninstructed<<- Loaddata(group="Uninstructed_No-Cursors", task = "MovementTimes")
  
  passive_prop_angles<<- Loaddata(group='Passive_Tap', task = "Angles")
  active_prop_angles<<- Loaddata(group='Active_Tap', task = "Angles")
  
  no_cursorm<<-  removeReachOutliers(Loaddata(group = "no-cursor_maxvel", task = "uninstructed"))
  no_cursormI<<- removeReachOutliers(Loaddata(group="no-cursor_maxvel", task = "instructed"))
  no_cursormI<- no_cursormI[,-9]
  
  Demos<<- read.csv("data/All_demographics.csv", header = TRUE)
  
}

fixnocursorcolnames<- function () {
  
  names<-colnames(newnocursor_reaches)
  newnames<- c('p33','p34','p35','p36','p37','p38','p39','p40','p41','p42','p43','p44','p45','p46','p47')
  names<- c(names[1:33], newnames)
  colnames(newnocursor_reaches)<<- names
  colnames(newnocursor_nocursors)<<- names
}

downloadOSFdata <- function(update=FALSE) {
  
  # this pulls data from the OSF repo:
  files <- c('active_localization.csv'  = 'https://osf.io/mc523/?action=download',
             'active_reaches.csv'       = 'https://osf.io/ejxy9/download',
             'nocursor_nocursors.csv'   = 'https://osf.io/5b8s9/download',
             'nocursor_reaches.csv'     = 'https://osf.io/vmnx7/download',
             'nocursor_NI_nocursors.csv'   = 'https://osf.io/y4k2x/download',
             'nocursor_NI_reaches.csv'     = 'https://osf.io/grnxh/download',
             'passive_localization.csv' = 'https://osf.io/27v54/download',
             'passive_reaches.csv'      = 'https://osf.io/mq5av/download',
             'pause_reaches.csv'        = 'https://osf.io/q59b3/download',
             'terminal_reaches.csv'     = 'https://osf.io/qdk9y/download',
             'terminal_localization.csv'= 'https://osf.io/a9sx5/download',
             'Active_Angles.csv'        = 'https://osf.io/ubdv8/?action=download',
             'Passive_Angles.csv'       = 'https://osf.io/3nsqm/download',
             'Pause_Angles.csv'         = 'https://osf.io/36cqd/download',
             'No-Cursor_Angles.csv'     = 'https://osf.io/jyz2n/download',
             'Instructed_No-Cursors_MovementTimes.csv'   = 'https://osf.io/8n3c6/download',
             'Uninstructed_No-Cursors_MovementTimes.csv'     = 'https://osf.io/k4pze/download',
             'Active_Tap_Angles.csv'    = 'https://osf.io/vkrs6/?action=download',
             'Passive_Tap_Angles.csv'   = 'https://osf.io/f67m5/download',
             'no-cursor_maxvel_instructed.csv'   = 'https://osf.io/62jbk/download',
             'no-cursor_maxvel_uninstructed.csv'     = 'https://osf.io/zmcpf/download',
             'exposure_reaches.csv'     = 'https://osf.io/6cmns/download',
             'exposure_localization.csv'= 'https://osf.io/gbkhm/download',
             'variation_reaches.csv'    = 'https://osf.io/pk5fy/download',
             'variation_localizations.csv'= 'https://osf.io/txgwj/download')
  
  
  # check if data directory exists and create if necessary:
  # (data should come from OSF, so is not on github)
  if (!dir.exists('data')) {
    dir.create('data')
  }
  
  # check if each file exists and copy it if necessary: 
  for (filename in names(files)) {
    
    filepath <- sprintf('data/%s',filename)
    
    if (!file.exists(filepath) | update) {
      
      df <- read.csv(url(files[filename]), stringsAsFactors=F)
      write.csv(df, filepath, quote=FALSE, row.names=FALSE)
      
    }
    
  }
  
}


percentNAs <- function (df) {
  return((sum(is.na(df))/prod(dim(df)))*100)
}

# OUTLIER REMOVAL ---------------------------------------------------------

removeSDoutliers <- function(values, sds=3) {
  
  avg <- mean(values, na.rm=TRUE)
  std <- sd(values, na.rm=TRUE) * sds
  
  values[values > avg + std] <- NA
  values[values < avg - std] <- NA
  
  return(values)
  
}

removeIQRoutliers <- function(values, range=3) {
  
  bp <- boxplot(values, range=3, plot=FALSE)
  
  values[values %in% bp$out] <- NA
  
  return(values)
  
}


removeReachOutliers <- function(data) {
  
  ntrials <- nrow(data)
  
  for (trialn in c(1:ntrials)) {
    
    data[trialn,2:ncol(data)] <- removeSDoutliers(as.numeric(data[trialn,2:ncol(data)]))
    
  }
  
  return(data)
  
}


loadcolors <- function() {
  ##Active
  colorA       <<- rgb(1.0, 0.4, 0.0)         # orange
  colorA_trans <<- rgb(1.0, 0.4, 0.0, 0.2)     # transparent orange
  
  
  ## Passive
  colorPA       <<- rgb(0.7, 0.0, 0.7)          # purple
  colorPA_trans <<- rgb(0.7, 0.0, 0.7, 0.2)     # transparent purple
  
  
  ## Pause
  colorNL       <<- rgb(0.63, 0.71, 0.81)      # blue-gray
  colorNL_trans <<- rgb(0.63, 0.71, 0.81, 0.2)  # transparent blue-gray
  
  
  ##No-Cursor
  colorNC       <<- rgb(0.0, 0.7, 0.0)         # green
  colorNC_trans <<- rgb(0.0, 0.7, 0.0, 0.2)     # transparent green
  
  ##New No-Cursor
  colorNNC       <<- rgb(0.1, 0.3, 0.5)         # purple
  colorNNC_trans <<- rgb(0.1, 0.3, 0.5, 0.2)     # transparent purple
  
  #Terminal
  colorT       <<- rgb(1, 0.0, 0.0)         # Red
  colorT_trans <<- rgb(1, 0.0, 0., 0.2)     # transparent Red
  
  ##Exposure
  colorE       <<- rgb(0.85, 0.65, 0.12)         # Yellow
  colorE_trans <<- rgb(0.85, 0.65, 0.12, 0.2)     # transparent Yellow
}

getreachesformodel<- function(data) {
  meanreaches<-rowMeans(data[,2:ncol(data)], na.rm=TRUE)
  distortion<- data$distortion
  return(data.frame(meanreaches,distortion))
}

combinereachdata<- function() {
  
  allreaches<- rbind(variation_reaches, exposure_reaches,terminal_reaches,passive_reaches, active_reaches,pause_reaches, nocursor_reaches) 
  
  
}



addsexforANOVA<- function() {
  source('scripts/Analysis.R')
  
  pauseRRM<<- ANOVAcombine(pause_reaches)
  activeRRM<<- ANOVAcombine(active_reaches)
  terminalRRM<<- ANOVAcombine(terminal_reaches)
  passiveRRM<<- ANOVAcombine(passive_reaches)
  nocursorRRM<<- ANOVAcombine(newnocursor_reaches)
  
  
    participant<- c()
  for (num in 1:32)
  participant<- c(participant,sprintf('p%s', num))
  
  
  for (p in participant) {
    
    pauseRRM$Sex[pauseRRM$ID == p]<<- Demos$Sex[Demos$Experiment == 'Pause' & Demos$Participant == p]
  }
    
    pauseRRM$Experiment<- rep("pause", times = nrow(pauseRRM))
  
  for (p in participant) {
    
    activeRRM$Sex[activeRRM$ID == p]<<- Demos$Sex[Demos$Experiment == 'Active' & Demos$Participant == p]
  }
    
    activeRRM$Experiment<- rep("active", times = nrow(activeRRM))
  
  for (p in participant) {
    
    terminalRRM$Sex[terminalRRM$ID == p]<<- Demos$Sex[Demos$Experiment == 'Terminal' & Demos$Participant == p]
  }
  
    terminalRRM$Experiment<- rep("terminal", times = nrow(terminalRRM))
    
  for (p in participant) {
    
    passiveRRM$Sex[passiveRRM$ID == p]<<- Demos$Sex[Demos$Experiment == 'Passive' & Demos$Participant == p]
  }
    passiveRRM$Experiment<- rep("passive", times = nrow(passiveRRM))
  
  
  participant<- c()
  for (num in 1:47)
    participant<- c(participant,sprintf('p%s', num))
  
  
  
  for (p in participant) {
    
    nocursorRRM$Sex[nocursorRRM$ID == p]<<- Demos$Sex[Demos$Experiment == 'No_Cursor' & Demos$Participant == p]
  }
  nocursorRRM$Experiment<- rep("no-cursor", times = nrow(nocursorRRM))
  
  
  allreachdata<<- rbind(pauseRRM,activeRRM,passiveRRM, terminalRRM, nocursorRRM)
  
}


addsexforTtest<- function () {
  source('scripts/Analysis.R')
  
  pauseTRRM<<- TCombine(pause_reaches)
  activeTRRM<<- TCombine(active_reaches)
  terminalTRRM<<- TCombine(terminal_reaches)
  passiveTRRM<<- TCombine(passive_reaches)
  nocursorTRRM<<- TCombine(newnocursor_reaches)
  
  pauseTRRM$Sex<<- Demos$Sex[Demos$Experiment == 'Pause']
  pauseTRRM$Experiment<- rep("pause", times = nrow(pauseTRRM))
  
  activeTRRM$Sex<<- Demos$Sex[Demos$Experiment == 'Active']
  activeTRRM$Experiment<- rep("active", times = nrow(activeTRRM))
  
  terminalTRRM$Sex<<- Demos$Sex[Demos$Experiment == 'Terminal']
  terminalTRRM$Experiment<- rep("terminal", times = nrow(terminalTRRM))
  
  passiveTRRM$Sex<<- Demos$Sex[Demos$Experiment == 'Passive']
  passiveTRRM$Experiment<- rep("passive", times = nrow(passiveTRRM))
  
  nocursorTRRM$Sex<<- Demos$Sex[Demos$Experiment == 'No_Cursor']
  nocursorTRRM$Experiment<- rep("no-cursor", times = nrow(nocursorTRRM))
  
  
  
  allreachTdata<<- rbind(pauseTRRM,activeTRRM,passiveTRRM, terminalTRRM, nocursorTRRM)
  
  
}



addsexforANOVAImplicit<- function() {
  source('scripts/Analysis.R')
  
  exposureLRM<<- ANOVAcombine(exposure_localization)
  activeLRM<<- ANOVAcombine(active_localization)
  terminalLRM<<- ANOVAcombine(terminal_localization)
  passiveLRM<<- ANOVAcombine(passive_localization)
  nocursorNRM<<- NoCursorACombine(newnocursor_nocursors)
  
  
  participant<- c()
  for (num in 1:32)
    participant<- c(participant,sprintf('p%s', num))
  
  
  for (p in participant) {
    
    exposureLRM$Sex[exposureLRM$ID == p]<<- Demos$Sex[Demos$Experiment == 'Exposure' & Demos$Participant == p]
  }
  
  exposureLRM$Experiment<- rep("exposure", times = nrow(exposureLRM))
  
  for (p in participant) {
    
    activeLRM$Sex[activeLRM$ID == p]<<- Demos$Sex[Demos$Experiment == 'Active' & Demos$Participant == p]
  }
  
  activeLRM$Experiment<- rep("active", times = nrow(activeLRM))
  
  for (p in participant) {
    
    terminalLRM$Sex[terminalLRM$ID == p]<<- Demos$Sex[Demos$Experiment == 'Terminal' & Demos$Participant == p]
  }
  
  terminalLRM$Experiment<- rep("terminal", times = nrow(terminalLRM))
  
  for (p in participant) {
    
    passiveLRM$Sex[passiveLRM$ID == p]<<- Demos$Sex[Demos$Experiment == 'Passive' & Demos$Participant == p]
  }
  passiveLRM$Experiment<- rep("passive", times = nrow(passiveLRM))
  
  
  participant<- c()
  for (num in 1:47)
    participant<- c(participant,sprintf('p%s', num))
  
  
  
  for (p in participant) {
    
    nocursorNRM$Sex[nocursorNRM$ID == p]<<- Demos$Sex[Demos$Experiment == 'No_Cursor' & Demos$Participant == p]
  }
  nocursorNRM$Experiment<- rep("no-cursor", times = nrow(nocursorNRM))
  
  
  allreachdataimplicit<<- rbind(exposureLRM,activeLRM,passiveLRM, terminalLRM, nocursorNRM)
  
}