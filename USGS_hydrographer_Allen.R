#USGS_hydrographer <- function(site_num, start_date, end_date){
  # USGS_hydrographer.R
  
  # by George Allen
  
  # inputs:
  # USGS Gauge Site Number(s)
  # A string or a vector of strings of USGS gauge number(s) to plot
  
  # Hydrograph start date(s)
  # A string or a vector of strings of start dates with the following format:
  # YYYY-MM-DD
  
  # Hydrograph end date(s)
  # A string or a vector of strings of end dates with the following format:
  # YYYY-MM-DD
  
  # note: if multiple hydrographs are given, gauge site numbers, start dates, and end dates
  # given in vectors must be in the same order. 
  
  # sample data:
  site_num = "15896000" #"15200280" 
  start_date = "2018-08-01"
  end_date = "2019-08-01"
  
  # Catch:
  if (!exists("site_num")){ message("site_num is undefined") }
  if (!exists("start_date")){ message("start_date input is undefined") }
  if (!exists("end_date")){ message("end_date input is undefined") }
  
  # add a zero to the site number if there are only 7 characters:
  site_num[nchar(site_num)==7] = paste0('0', site_num[nchar(site_num)==7])
  
  # # create download status table:
  # names = c("index", "gauge", "status", "url")
  # status = data.frame(array(NA, c(length(site_num), length(names))))
  # names(status) = names
  # status$index = 1:length(site_num)
  
  # create URL string:
  Discharge = T
  Stage = T
  Temperature = T
  
  # convert to USGS parameter codes:
  # full list of codes: https://help.waterdata.usgs.gov/codes-and-parameters/parameters 
  if (Discharge == T){ flowStr = "cb_00065=on&" }else{ flowStr = "" }
  if (Stage == T){ stageStr = "cb_00060=on&" }else{ stageStr = "" }
  if (Temperature == T){ tempStr = "cb_00010=on&" }else{ tempStr = "" }
  
  url = paste0("https://nwis.waterdata.usgs.gov/usa/nwis/uv/?", 
               tempStr, stageStr, flowStr,
               "format=rdb&site_no=", site_num, 
               "&period=&begin_date=", start_date, 
               "&end_date=", end_date)
  
  # try to download USGS gauge table: 
  tab = try(read.table(url, fill=T, header=T, row.names=NULL)[-1,], silent=T)
  
  # if table isn't formatted as expected, try adding a zero in front of site number:
  if ((length(grep("Error in read.table", tab)) > 0) | !is.data.frame(tab)){
    url0 = sub(site_num, paste0("0", site_num), url) 
    tab = try(read.table(url0, fill=T, header=T, row.names=NULL)[-1,], silent=T)
  }
  
  # if table still isn't formatted as expected, give an error message:
  if ((length(grep("Error in read.table", tab)) > 0) | !is.data.frame(tab)){
    message(paste("No daily gauge record found for site number:", site_num))
  }
  
  # get names of columns:
  names = names(tab)
  
  # date:
  dCol = grep("site_no|datetime", names)
  dateTab = tab[, dCol]
  date = as.POSIXct(paste(dateTab$site_no, dateTab$datetime), format="%Y-%m-%d %H:%M")
  #date= as.Date(date, format="%Y-%m-%d %H:%M")
  # Extract discharge from tab:
  if (Discharge == T){ 
    qCol = grep("00060$", names)
    if (length(qCol)==0){
      message("USGS gauge record does not contain discharge measurements")
    }else{
      Q_raw = as.vector(tab[, qCol])
      Q_ice = grep("ice", Q_raw, ignore.case=T)
      if (length(Q_ice)==0){
        Q_cfs = as.numeric(Q_raw)
      }else{
        Q_cfs = as.numeric(gsub("ice", "0", Q_raw, ignore.case=T))
      }
      # convert cfs to cms:
      Q_cms = Q_cfs * 0.02832
      
      # temp: set readings of zero to NA 
      # (in this gauge, there were obviously erroroneous zero readings)
      Q_cms[Q_cms<5] = NA
    }
  }
  
  
  # Extract stage from tab:
  if (Stage == T){ 
    hCol = grep("00065$", names)
    if (length(hCol)==0){
      message("USGS gauge record does not contain stage measurements")
    }else{
      h_raw = as.vector(tab[, hCol])
      h_ice = grep("ice", h_raw, ignore.case=T)
      if (length(h_ice)==0){
        h_f = as.numeric(h_raw)
      }else{
        h_f = as.numeric(gsub("ice", "0", h_raw, ignore.case=T))
      }
      # convert feet to meters:
      h_m = h_f * 0.3048
    }
  }
  
  # Extract temperature from tab:
  if (Temperature == T){ 
    tCol = grep("00010$", names)
    if (length(tCol)==0){
      message("USGS gauge record does not contain temperature measurements")
    }else{
      t_raw = as.vector(tab[, tCol])
      t_c = as.numeric(t_raw)
    }
  }
  
  
  
  ####
  # Discharge value qualification codes:
  qCodeCol = grep("00060_cd$", names)
  qCodeStatus = as.vector(tab[, qCodeCol])
  qUniqCodes = unique(qCodeStatus)
  
  # translate codes to descriptions based on:
  # https://help.waterdata.usgs.gov/codes-and-parameters/daily-value-qualification-qCode-dv_rmk_cd 
  qCode = c("e", ":", "E", "A", "P", "<", ">", "1", "2", " ")
  qDesc = c("Estimated", "&", "Estimated", "Approved", 
           "Provisional", "Underestimated", "Overestimated", "Write protected",
           "Write protected", "Blank")
  
  legTab = as.data.frame(cbind(qCode, qDesc))
  uniqCodesSplt = strsplit(qUniqCodes, "")
  uniqCodeMatch = lapply(uniqCodesSplt, match, legTab$qCode)
  uniqDescSplit = lapply(uniqCodeMatch, function(x) legTab$qDesc[x])
  legendTextList = lapply(uniqDescSplit, paste, collapse=" ")
  legendText = unlist(legendTextList)
  
  
  ####
  # plot:
  legendCols = 1:length(qUniqCodes)
  lineCols = match(qCodeStatus, qUniqCodes)
  
  plot(date , Q_cms, type="n",
       main = paste("Site", site_num),
       xlab = "",
       ylab = "Discharge (cms)",
       xaxt="n")
  axis.POSIXct(1, date, format="%b")
  axis.POSIXct(1, date, format="%Y", padj=1.8)
  
  # plot hydrograph colored by qualification code:
  segments(date[-length(date)], Q_cms[-length(Q_cms)], date[-1], Q_cms[-1], col=lineCols, lwd=0.8)
  
  legend("topleft", legendText, lty=1, col=legendCols)

#}
