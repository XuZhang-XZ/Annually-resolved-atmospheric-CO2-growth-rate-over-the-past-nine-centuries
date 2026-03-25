

# This uses the ‘haversine’ formula to calculate the great-circle distance between two points – that is, the shortest distance over the earth’s surface – giving an ‘as-the-crow-flies’ distance between the points (ignoring any hills they fly over, of course!).
# 
# Haversine
# formula:	a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
# c = 2 ⋅ atan2( √a, √(1−a) )
# d = R ⋅ c
# where	φ is latitude, λ is longitude, R is earth’s radius (mean radius = 6,371km);
# note that angles need to be in radians to pass to trig functions!

distanceme=function(lon1,lat1,lon2,lat2){
  
  R=6371 ## kilometres
  lon1=lon1*pi/180
  lon2=lon2*pi/180
  lat1=lat1*pi/180
  lat2=lat2*pi/180
  
  del.lon=lon2-lon1
  del.lat=lat2-lat1
  
  a = sin(del.lat/2)^2 + cos(lat1)*cos(lat2)*sin(del.lon/2)^2
  c = 2*atan2(a^0.5,(1-a)^0.5)
  d = R* c
  d
}


# distanceme(lon1=-6,lat1 = 50,lon2 = -3,lat2 = 58.5)
