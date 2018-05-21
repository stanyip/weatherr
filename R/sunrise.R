sunrise = function(lat=0,lon=0,from=Sys.time(),to=NULL,location=NULL,tz=Sys.timezone()) {
  if (!is.null(location)) {
    latlon = as.numeric(rev(geocode(location=location,source = "google")))
    if (any(is.null(latlon))) stop('Error: no match location')
    lat = latlon[1]; lon = latlon[2]
  }
  if ((length(lat) != length(lon)) | (any(abs(lat)>90)) | (any(abs(lon)>180))) stop('Longitude and latitude should have equal length and within the valid range')
  inpar = paste0('lat=',lat,'&lon=', lon,'&',ifelse(length(to)==0,paste0('date=',format(from,'%Y-%m-%d')),paste0('from=',format(from,'%Y-%m-%d'),'&to=',format(to,'%Y-%m-%d'))))
  url = paste0("http://api.met.no/weatherapi/sunrise/1.1/?", inpar)
  u = paste0(paste0(readLines(url), collapse = "\n"), "\n")
  tmp = try(xmlRoot(xmlParse(u)))
  astz = function(time,tz) {if (length(time)==0) return(NA) else with_tz(strptime(time,'%Y-%m-%dT%H:%M:%S',tz='GMT'),tz)}
  x = Reduce('rbind',lapply(xpathSApply(tmp,'//time'),function(x) data.frame(date=xpathSApply(x,'@date'),sunrise=astz(xpathSApply(x,'location/sun/@rise'),tz),sunset=astz(xpathSApply(x,'location/sun/@set'),tz),noon=as.numeric(xpathSApply(x,'location/sun/noon/@altitude')),moonrise=astz(xpathSApply(x,'location/moon/@rise'),tz),moonset=astz(xpathSApply(x,'location/moon/@set'),tz), moon=xpathSApply(x,'location/moon/@phase'))))
  row.names(x) = NULL
  x
}