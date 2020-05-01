import2018 <- function() {
  library(xts)
  mergeData <- function(list.x, sort.by = 1, average = FALSE) {
    all.data <- do.call(merge, list.x)
    all.data.index <- which(!is.na(all.data[,sort.by]))
    for(i in 1:(length(all.data.index)-1)) {
      row.start <- all.data.index[i]
      row.stop <- all.data.index[i+1]
      if((row.stop-row.start) == 1) {
        next
      }
      if(!average) data.locf <- na.locf(all.data[(row.start+1):row.stop,])
      if(average) {
        data.avg <- t(data.frame(sapply(all.data[(row.start+1):row.stop,], function(x) mean(na.omit(x)))))
        rownames(data.avg) <- as.character(index(all.data)[row.stop])
        
      }
      if (!exists("new.data")) {
        if(!average) new.data <- data.frame(data.locf[nrow(data.locf),])
        if(average) new.data <- data.avg
      }
      if (exists("new.data")) {
        if(!average) new.data <- rbind(new.data, data.frame(data.locf[nrow(data.locf),]))
        if(average) new.data <- rbind(new.data, data.avg)
      }
    }
    new.data <- na.omit(new.data)
    na.fix <- which(!is.na(as.POSIXct(rownames(new.data), format = "%Y-%m-%d %H:%M:%S")))
    new.data.xts <- xts(new.data[na.fix,], order.by = as.POSIXct(rownames(new.data)[na.fix], format = "%Y-%m-%d %H:%M:%S"))
    
    return(new.data.xts)
  }
  
  # ##### PAA Process Data - Grab #####
  # delta <- intToUtf8(0x0394)
  # # Daily data
  # process.data <- readxl::read_excel("./data/paa/PAA PROFILE DATA_08-12-18.xlsx", 
  #                                    sheet = "Process Data", skip = 1)
  # process.data <- process.data[-1,]
  # n.paa.grab <- xts::xts(apply(process.data[,c(12:17,19:27)], 2, function(x) as.numeric(x)), order.by =  as.POSIXct(as.data.frame(process.data[,18])[,1], format = "%Y-%m-%d %H:%M:%S"))
  # colnames(n.paa.grab) <- c("PAA Dosing Pump Total Flow (gpm)", #1 
  #                           "PAA Dose (mg/L)", #2
  #                           "PAA Setpoint (mg/L)", #3 
  #                           "Upstream  Residual (mg/L)", #4 
  #                           # paste0(delta,"PAA (mg/L)"),	#5
  #                           "deltaPAA (mg/L)", #5
  #                           "Pre-Disinfection E. coli (MPN/100 mL)",  #6
  #                           "Effluent Discharge (MGD)", #7
  #                           "Contact Tank Volume (MG)", #8
  #                           "Detention Time (min)", #9
  #                           "Time to Upstream Sample Point (min)", #10
  #                           "Log Removal (N0/N)", #11
  #                           "Effluent E. coli (MPN/100 mL)", #12
  #                           "CT (mg/L*min)", #13
  #                           "CuT (mg/L*min)", #14
  #                           "Ambient Temperature")#15
  # colnames(n.paa.grab) <- stringr::str_replace_all(colnames(n.paa.grab), c(" " = ".", "/" = "." , "-" = "","[(]" = "", "[)]" = "", "[*]"="."))
  # rm(process.data)
  # 
  # 
  ##### October PAA Process Data - Grab #####
  oct.paa <- readxl::read_excel("data/paa/PAA PROFILE DATA_08-12-18.xlsx", 
                                sheet = "Oct 2 to 15, 2018", range = "A1:V170")[-1,]
  n.datetime <- which(colnames(oct.paa) == "Date and Time")
  oct.paa.index <- oct.paa[,n.datetime]
  oct.paa <- sapply(oct.paa[,-n.datetime], function(x) as.numeric(x))
  colnames(oct.paa) <- stringr::str_replace_all(colnames(oct.paa), c(" " = "." , "-" = "" ))
  oct.paa <- xts(oct.paa, order.by = oct.paa.index[[1]])
  oct.paa <- oct.paa[,4:5]
  
  # ###### North Carbovis Data #####
  # vis.data <- readxl::read_excel("data/paa/NNE Carbovis Data 2018.xlsx",
  #                                sheet = "Inst DL Data", col_types = c("date",
  #                                                                      "text", "numeric", "skip", "skip",
  #                                                                      "skip", "numeric", "skip", "skip",
  #                                                                      "skip", "numeric", "skip", "skip",
  #                                                                      "skip", "numeric", "skip", "skip",
  #                                                                      "skip", "numeric", "skip", "skip",
  #                                                                      "skip", "numeric", "skip", "skip",
  #                                                                      "skip", "skip", "numeric", "skip",
  #                                                                      "skip", "skip", "numeric", "skip",
  #                                                                      "skip", "skip", "skip", "numeric",
  #                                                                      "skip", "skip", "skip", "numeric",
  #                                                                      "skip"), skip = 6)
  # vis.data <- vis.data[which(vis.data[,2] == "Valid"),-2]
  # colnames(vis.data) <- c("Time", "CODto (mg/L)", "CODto (V)",
  #                         "TSS (mg/L)", "TSS (V)",
  #                         "UVT (%)", "UVT (V)",
  #                         "CODds (mg/L)", "CODds (V)",
  #                         "SACto (1/m)", "SACto (V)")
  # vis.data <- xts(vis.data[,-1], order.by = as.POSIXct(as.data.frame(vis.data[,1])[,1], format = "%Y-%m-%d %H-%M-%S"))
  
  
  ##### North Secondary - Online #####
  # ## North secondary online
  # nsec.online <- as.data.frame(suppressWarnings(readxl::read_excel("data/paa/North Secondary and Disinfection Process Data_2018.xlsx", sheet = "NSEC Online Data", col_names = FALSE,
  #                                                                  col_types = c("date", "numeric", "numeric", 
  #                                                                                "numeric", "numeric", "numeric", 
  #                                                                                "numeric", "numeric", "numeric", 
  #                                                                                "numeric", "numeric", "numeric", 
  #                                                                                "numeric", "numeric", "numeric", 
  #                                                                                "numeric", "numeric", "numeric", 
  #                                                                                "numeric", "numeric", "numeric", 
  #                                                                                "numeric", "numeric", "numeric", 
  #                                                                                "numeric", "numeric", "numeric", 
  #                                                                                "numeric", "numeric", "numeric", 
  #                                                                                "numeric", "numeric", "numeric", 
  #                                                                                "numeric", "numeric", "numeric"), 
  #                                                                  skip = 4)))
  # nsec.online <- xts(nsec.online[,-1], order.by = nsec.online[,1])
  # colnames(nsec.online) <- c("NSEC Influent Flow", "NSEC Influent Temp","NSEC Influent NH3","NSEC Influent TSS","NSEC Influent COD",
  #                            "NSEC CaRRB-1 Centrate Flow","NSEC CaRRB-1 NH3","NSEC CaRRB-3 Centrate Flow","NSEC CaRRB-3 NH3",
  #                            "GTE Flow","GTE to SSEC Flow","GTE to NSEC Flow",
  #                            "AB-10 Influent Flow","AB-10 A-Pass Temp","AB-10 A-Pass pH","AB-10 A-Pass DO","AB-10 A-Pass NH3","AB-10 A-Pass NO3","AB-10 B-Pass DO","AB-10 C-Pass pH	AB-10","C-Pass DO","AB-10 C-Pass NH3","AB-10 C-Pass NO3","AB-10 MLSS","AB-10 MLR Flow","Quad 4 RAS Flow","Quad 4 Basins in Service","AB-10 RAS Flow","NSEC Aerobic SRT",
  #                            "NSEC Effluent NH3","NSEC Effluent NO3","NSEC Effluent OP","NSEC Effluent TSS","NSEC Effluent NO5","NSEC Effluent Flow")
  # 
  # 
  # # nsec.online <- nsec.online["2018-11-04/2018-12-01"]
  # cols2remove <- c("NSEC CaRRB-1 Centrate Flow","NSEC CaRRB-1 NH3","NSEC CaRRB-3 Centrate Flow","NSEC CaRRB-3 NH3","GTE Flow","GTE to SSEC Flow","GTE to NSEC Flow")
  # 
  # nsec.online <- nsec.online[,-sapply(cols2remove, function(x) which(colnames(nsec.online) == x))]
  # 
  # more.nsec <- readxl::read_excel("data/paa/PAA-Ecoli.xlsx", 
  #                                 sheet = "Sheet1", col_types = c("date", 
  #                                                                 "numeric", "numeric", "numeric", 
  #                                                                 "numeric", "numeric", "numeric", 
  #                                                                 "numeric", "numeric", "numeric", 
  #                                                                 "numeric", "numeric"))
  # more.nsec <- more.nsec[which(!is.na(more.nsec[,1])),]
  # more.nsec <- xts(more.nsec[,-1], order.by = more.nsec[,1][[1]])
  # colnames(more.nsec) <- c("PAA Upstream Residual", "PAA Total Flow", "Dis North Flow", 
  #                          "Temperature NSEC Inf", "ASRT", "NSEC Effluent NH3",
  #                          "NSEC Effluent NO3","NSEC Effluent OP","NSEC Effluent TSS",
  #                          "NSEC Effluent NO5","NSEC Effluent Flow")
  
  # all.data <- oct.paa
  # r <- paste0(range(index(all.data))[1],"/",range(index(all.data))[2])
  # # all.data <- mergeData(list.x = list(all.data,vis.data[r],nsec.online[r,22:28]))
  # all.data <- mergeData(list.x = list(all.data,vis.data[r]))
  # all.data <- na.omit(all.data)
  # remove.cols.names <- c("Initial.PAA.Demand.or.Decay", "DPAA.Samples",
  #                        "Sample.Time..1_min.","Detention.Time",
  #                        "PAA.Pump.Total.Flow", "PAA.Set.Point.Dose.Algorithm", 
  #                        "...10" , "Volume.to.1.min..Sample" , 
  #                        "Time.to.1.min..Sample", "Total.Basin.Volume", 
  #                        "DT.of.1.2.Basin", "SPBased.Disinfection.CT",     
  #                        "CalcBased.Disinfection.CT", "CODto..V.", "CODds..V.", 
  #                        "TSS..V.", "UVT..V.", "UVT..V.", "SACto..V.",
  #                        "N..Eff..TSS.Conc.", "Temp..of.NSEC.Main.Ch.", 
  #                        "N..Basin.Outfall", "Temp..of.the.Atmos.", 
  #                        "Secondary.Effluent.Flow", "Pump.Flow.Based.PAA.Dose")
  # remove.cols <- sapply(remove.cols.names, function(x) which(colnames(all.data) == x))
  # all.data <- all.data[,-remove.cols]
  # 
  # flow <- all.data$N..Basin.Outfall
  # hrt.1 <- 135.11*flow^-.959
  # hrt.half <- 1551.2*flow^-.971
  # all.data <- cbind(all.data, hrt.1, hrt.half)
  # colnames(all.data)[(ncol(all.data)-1):ncol(all.data)] <- c("HRT 1", "HRT half")
  # 
  return(oct.paa)
}