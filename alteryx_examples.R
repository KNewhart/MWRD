# Initialize R-PI connection
library(piwebapi)

# Login information
useKerberos <- TRUE
username <- "knewhart"
password <- "Lunabear2@"
validateSSL <- TRUE
debug <- TRUE
piWebApiService <- piwebapi$new("https://pivision/piwebapi", useKerberos, username, password, validateSSL, debug)

pull.and.save <- function(tag, label=NULL) {
  pi.points <- piWebApiService$attribute$getByPath(path=tag)
  pi.data <- piWebApiService$stream$getRecorded(webId = pi.points$WebId, startTime='*-365d', endTime='*')[[2]]
  pi.data <- do.call("rbind", lapply(pi.data, function(x) {
    time.obj <- paste(strsplit(as.character(x[1]), "T")[[1]][1], 
                      strsplit(strsplit(as.character(x[1]), "T")[[1]][2], "Z")[[1]][1])
    time.obj <- lubridate::with_tz(as.POSIXct(time.obj, tz="UTC"), tzone = Sys.timezone())
    value.obj <- x[[2]]
    if(length(value.obj) > 1) value.obj <- NA
    return(data.frame("Timestamp"=time.obj, 
                      "Value" = value.obj))
  })
  )
  pi.data <- na.omit(pi.data)
  file.name <- tail(strsplit(tail(strsplit(tag,"[\\]")[[1]],n=1),"[|]")[[1]],n=1)
  if(is.null(label)) write.csv(pi.data, file=paste0(file.name,".csv"), row.names = FALSE)
  if(!is.null(label)) write.csv(pi.data, file=paste0(label,".csv"), row.names = FALSE)
}


# Calculation 1: Daily E.coli + Flow

## Pull E.coli from N
tag <- "\\\\APPLEPI_AF\\MWRD_Production\\Hite Treatment Plant\\19-Permit_Compliance\\Permit Analytics|North Ecoli"
pull.and.save(tag)

## Pull E.coli from S
tag <- "\\\\APPLEPI_AF\\MWRD_Production\\Hite Treatment Plant\\19-Permit_Compliance\\Permit Analytics|South Ecoli"
pull.and.save(tag)

## Pull Flow from N
tag <- "\\\\APPLEPI_AF\\MWRD_Production\\Hite Treatment Plant\\19-Permit_Compliance\\Permit Analytics|North Flow"
pull.and.save(tag)

## Pull Flow from S
tag <- "\\\\APPLEPI_AF\\MWRD_Production\\Hite Treatment Plant\\19-Permit_Compliance\\Permit Analytics|South Flow"
pull.and.save(tag)




# Calculation 2: TSS removal + SOR

## Pull NPRI 3 Inf Flow
tag <- "\\\\APPLEPI_AF\\MWRD_Production\\Hite Treatment Plant\\04-Primaries|NPRI 3 Influent Flow"
pull.and.save(tag)
 

## Pull NPRI 3 IS tags
tag <- "\\\\applepi\\HC_P517"
label <- "NPRI3-IS-1"
pull.and.save(tag, label)

tag <- "\\\\applepi\\HC_P518"
label <- "NPRI3-IS-2"
pull.and.save(tag, label)

tag <- "\\\\applepi\\HC_P519"
label <- "NPRI3-IS-3"
pull.and.save(tag, label)

tag <- "\\\\applepi\\HC_P520"
label <- "NPRI3-IS-4"
pull.and.save(tag, label)

## Pull NPRI 3 TSS tags
tag <- "\\\\APPLEPI_AF\\MWRD_Production\\Hite Treatment Plant\\04-Primaries\\NPRI-3\\Optimization\\Influent TSS Load|Concentration"
label <- "NPRI3-Inf-TSS"
pull.and.save(tag, label)

tag <- "\\\\APPLEPI_AF\\MWRD_Production\\Hite Treatment Plant\\04-Primaries\\NPRI-3\\Optimization\\Effluent TSS Load|Concentration"
label <- "NPRI3-Eff-TSS"
pull.and.save(tag, label)

# PAA + Upstream



