match_2d=function(x1,y1,x2,y2){

sd_x<-sd(x2[which(is.finite(x2)==T)])
sd_y<-sd(y2[which(is.finite(y2)==T)])
    
plane_dist <- sqrt(((x1-x2)/sd_x)^2+((y1-y2)/sd_y)^2)

match <- which(plane_dist==min(plane_dist[which(is.finite(plane_dist)==T)]))
return=match

}
