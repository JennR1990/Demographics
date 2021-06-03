##repeated Measures Combine function for T-Tests

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
    R1_third<- mean(unlist(data[73:76,participant]), na.rm = TRUE)
    R1_forth<-mean(unlist(data[77:80,participant]), na.rm = TRUE) 
    R1_fifth<- mean(unlist(data[81:84,participant]), na.rm = TRUE)
    R1_sixth<-mean(unlist(data[85:88,participant]), na.rm = TRUE) 
    R1_Late<- mean(unlist(data[221:224,participant]), na.rm = TRUE)
    R2<- mean(unlist(data[237:240,participant]), na.rm = TRUE)
    EC<- mean(unlist(data[241:244,participant]), na.rm = TRUE)
    EC1<- mean(unlist(data[241,participant]), na.rm = TRUE)
    EC2<- mean(unlist(data[242,participant]), na.rm = TRUE)
    EC3<- mean(unlist(data[243,participant]), na.rm = TRUE)
    EC4<- mean(unlist(data[244,participant]), na.rm = TRUE)
    EC_Late<- mean(unlist(data[273:288,participant]), na.rm = TRUE)
    RM<- data.frame(Aligned, r1, r2,r3,r4, R1_Early, R1_second, R1_third, R1_forth, R1_fifth, R1_sixth, R1_Late, R2, EC, EC_Late, EC1, EC2, EC3, EC4)
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
  Reaches<- c()
  Time<- c()
  ID<- c()
  
  for (participant in participants){
    
    participant_reaches <- unlist(data[,participant])
    
    for (epoch in names(epochs)) {
      
      start <- epochs[[epoch]][1]
      finish <- start -1 + epochs[[epoch]][2]
      Reaches <- c(Reaches, mean(participant_reaches[start:finish], na.rm=TRUE))
      Time <- c(Time, epoch)
      ID <- c(ID, participant)
      ANOVARM<- data.frame(Reaches, Time, ID)
    }
  }
  #b<- !is.nan(ANOVARM$Reaches)
  # ANOVARM<- ANOVARM[c(b),]
  return(ANOVARM)
}

##repeated measures combine for No-Cursor Data

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
  Reaches<- c()
  Time<- c()
  ID<- c()
  
  for (participant in participants){
    
    participant_reaches <- unlist(data[,participant])
    
    for (epoch in names(epochs)) {
      
      start <- epochs[[epoch]][1]
      finish <- start -1 + epochs[[epoch]][2]
      Reaches <- c(Reaches, mean(participant_reaches[start:finish], na.rm=TRUE))
      Time <- c(Time, epoch)
      ID <- c(ID, participant)
      ANOVARM<- data.frame(Reaches, Time, ID)
    }
  }
  #  b<- !is.nan(ANOVARM$Reaches)
  #  ANOVARM<- ANOVARM[c(b),]
  return(ANOVARM)
}


PrepdataforANOVA1 <- function(adata, pasdata, paudata, ncdata, ncIdata, tdata ) {
  
  #Reaches for Active, Passive, Pause, Terminal, No-Cursor
  A_RM<-ANOVAcombine(adata)
  A_RM$ID <- sprintf('ActLoc.%s',A_RM$ID)
  A_RM$Experiment <- rep('Active', nrow(A_RM))
  
  Pas_RM<-ANOVAcombine(pasdata)
  Pas_RM$ID <- sprintf('PasLoc.%s',Pas_RM$ID)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  
  Pau_RM<-ANOVAcombine(paudata)
  Pau_RM$ID <- sprintf('Pause.%s',Pau_RM$ID)
  Pau_RM$Experiment <- rep('Pause', nrow(Pau_RM))
  
  nc_RM<-ANOVAcombine(ncdata)
  nc_RM$ID <- sprintf('NoCursor.%s',nc_RM$ID)
  nc_RM$Experiment <- rep('No-Cursor', nrow(nc_RM))
  
  ncI_RM<-ANOVAcombine(ncIdata)
  ncI_RM$ID <- sprintf('NoCursor.%s',ncI_RM$ID)
  ncI_RM$Experiment <- rep('No-CursorI', nrow(ncI_RM))
  
  T_RM<-ANOVAcombine(tdata)
  T_RM$ID <- sprintf('Term.%s',T_RM$ID)
  T_RM$Experiment <- rep('Terminal', nrow(T_RM))
  
  
  
  
  AllDataRM<- rbind(A_RM, Pas_RM, Pau_RM, nc_RM, ncI_RM,T_RM)
  #
  return(AllDataRM)
  
}

PrepdataforPropANOVA <- function(adata, pasdata, Tdata, Edata, NCdata, NCIdata) {
  
  A_RM<-ANOVAcombine(adata)
  A_RM$ID <- sprintf('ActLoc.%s',A_RM$ID)
  A_RM$Experiment <- rep('Active', nrow(A_RM))
  
  Pas_RM<-ANOVAcombine(pasdata)
  Pas_RM$ID <- sprintf('PasLoc.%s',Pas_RM$ID)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  
  
  T_RM<-ANOVAcombine(Tdata)
  T_RM$ID <- sprintf('Term.%s',T_RM$ID)
  T_RM$Experiment <- rep('Terminal', nrow(T_RM))
  
  E_RM<-ANOVAcombine(Edata)
  E_RM$ID <- sprintf('Expo.%s',E_RM$ID)
  E_RM$Experiment <- rep('Exposure', nrow(E_RM))
  
  NC_RM<-NoCursorACombine(NCdata)
  NC_RM$ID <- sprintf('NoC.%s',NC_RM$ID)
  NC_RM$Experiment <- rep('No-Cursor', nrow(NC_RM))
  
  NCI_RM<-NoCursorACombine(NCIdata)
  NCI_RM$ID <- sprintf('NoCI.%s',NCI_RM$ID)
  NCI_RM$Experiment <- rep('No-CursorI', nrow(NCI_RM))
  
  AllDataRM<- rbind(A_RM, Pas_RM,T_RM, E_RM, NC_RM, NCI_RM)
  return(AllDataRM)
  
}


PrepdataforT<- function(adata, pasdata, paudata, ncdata, ncncdata, Tdata){
  #
  
  A_RM<-TCombine(adata)
  A_RM$Experiment <- rep('Active', nrow(A_RM))
  A_RM[,21:22]<-Demos[Demos$Experiment == "Active",1:2]
  
  
  Pas_RM<-TCombine(pasdata)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  Pas_RM[,21:22]<-Demos[Demos$Experiment == "Passive",1:2]
  
  Pau_RM<-TCombine(paudata)
  Pau_RM$Experiment <- rep('Pause', nrow(Pau_RM))
  Pau_RM[,21:22]<-Demos[Demos$Experiment == "Pause",1:2]
  
  nc_RM<-TCombine(ncdata)
  nc_RM$Experiment <- rep('No-Cursor', nrow(nc_RM))
  nc_RM[,21:22]<-Demos[Demos$Experiment == "No_Cursor",1:2]
  
  ncnc_RM<-TCombine(ncncdata)
  ncnc_RM$Experiment <- rep('No-CursorI', nrow(ncnc_RM))
  ncnc_RM[,21:22]<-Demos[Demos$Experiment == "No_Cursor_I",1:2]
  
  T_RM<-TCombine(Tdata)
  T_RM$Experiment <- rep('Terminal', nrow(T_RM))
  T_RM[,21:22]<-Demos[Demos$Experiment == "Terminal",1:2]
  
  
  AllDataRM<- rbind(A_RM, Pas_RM, Pau_RM, nc_RM, ncnc_RM, T_RM)
  # 
  return(AllDataRM)
}

PrepdataforPropT<- function(adata, pasdata, ncdata, ncncdata, tdata, edata){
  A_RM<-TCombine(adata)
  A_RM$Experiment <- rep('Active', nrow(A_RM))
  A_RM[,21:22]<-Demos[Demos$Experiment == "Active",1:2]
  
  Pas_RM<-TCombine(pasdata)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  Pas_RM[,21:22]<-Demos[Demos$Experiment == "Passive",1:2]

  
  NC_RM<-TCombine(ncdata)
  NC_RM$Experiment <- rep('No-Cursor', nrow(NC_RM))
  NC_RM[,21:22]<-Demos[Demos$Experiment == "No_Cursor",1:2]
  
  NCI_RM<-TCombine(ncncdata)
  NCI_RM$Experiment <- rep('No-CursorI', nrow(NCI_RM))
  NCI_RM[,21:22]<-Demos[Demos$Experiment == "No_Cursor_I",1:2]
  
  T_RM<-TCombine(tdata)
  T_RM$Experiment <- rep('Terminal', nrow(T_RM))
  T_RM[,21:22]<-Demos[Demos$Experiment == "Terminal",1:2]
  
  E_RM<-TCombine(edata)
  E_RM$Experiment <- rep('Exposure', nrow(E_RM))
  E_RM[,21:22]<-Demos[Demos$Experiment == "Exposure",1:2]
  
  AllDataRM<- rbind(A_RM, Pas_RM,T_RM, E_RM, NC_RM, NCI_RM)
  return(AllDataRM)
}

