#' Hot spot and hot moment barplot with variance
#'
#' This function allows you to create 'hot spot' and 'hot moment categories from a dataframe
#' @param df the dataframe
#' @param mean in quotations is the column name you want to use to create the bars
#' @param variance in quotations is the column name you want to use to create the error bars
#' @param categories is a list of number or characters.  The categories create the HSHM varaibles.
#' @param col default is white, optional color for the barplots
#' @param main default is none, creates a header
#' @param xlab default is none, creates an x-axis label
#' @param ylab default is none, creates a y-axis label
#'
#' @examples
#' x<-c(1,2,3,4,5,6,7,8,9,10,11)
#' y<-c(12,13,14,15,16,17,18,19,20,21,22)
#' z<-c(1.3245234,2.2354256,3.34543,4.34634,5.43643,6.3463,7.34643,8.57345,9.65857,10.68454357,11.67986543)
#' df<-data.frame(x,y,z)
#' df<-HSHMcategories(df,"y",brkpts=c(11,16.5,23),categories=c("low","middle","high"))
#' HSHMIndex_df<-HSHMindex(df,"z","HSHM.cat")
#' baplotfnctvar(HSHMIndex_df,"mean.z","se.z","HSHM.cat",col=c("red","green","blue"))

barplotfnctvar<-function(df,mean,variance,categories,col="white",main="",xlab="",ylab=""){
  plotm<-df[,c(which(colnames(df)==mean),
               which(colnames(df)==variance),
               which(colnames(df)==categories))]
  names(plotm)[1]<-paste("mean")
  names(plotm)[2]<-paste("var")
  names(plotm)[3]<-paste("cat")
  cat<-unique(plotm$cat)
  narmvplotm<-na.exclude(plotm)
  ymin<-if(min(narmvplotm$mean)>0){0}else{min(narmvplotm$mean)+0.25*min(narmvplotm$mean)}
  ymax<-max(narmvplotm$mean)+max(narmvplotm$var)+(0.25*max(narmvplotm$var))

  #Plotting
  barplotlocation<-barplot(plotm$mean, col=col, ylim=c(ymin,ymax), las=1,xaxt="n")
  abline(h=0)
  segments(barplotlocation,plotm$mean+plotm$var,barplotlocation,plotm$mean-plotm$var)
  mtext(cat, at=barplotlocation, side=1, line=0.25)
  title(main)
  mtext(ylab,side=2,line=2.25)
  mtext(xlab,side=1,line=2.25)
  box()
}

#' Hot spot and hot moment barplot
#'
#' This function allows you to create 'hot spot' and 'hot moment categories from a dataframe
#' @param df the dataframe
#' @param mean in quotations is the column name you want to use to create the bars
#' @param categories is a list of number or characters.  The categories create the HSHM varaibles.
#' @param col default is white, optional color for the barplots
#' @param main default is none, creates a header
#' @param xlab default is none, creates an x-axis label
#' @param ylab default is none, creates a y-axis label
#'
#' @examples
#' x<-c(1,2,3,4,5,6,7,8,9,10,11)
#' y<-c(12,13,14,15,16,17,18,19,20,21,22)
#' z<-c(1.3245234,2.2354256,3.34543,4.34634,5.43643,6.3463,7.34643,8.57345,9.65857,10.68454357,11.67986543)
#' df<-data.frame(x,y,z)
#' df<-HSHMcategories(df,"y",brkpts=c(11,16.5,23),categories=c("low","middle","high"))
#' HSHMIndex_df<-HSHMindex(df,"z","HSHM.cat")
#' baplotfnct(HSHMIndex_df,"mean.z","HSHM.cat",col=c("red","green","blue"))

barplotfnct<-function(df,value,categories,col="white",main="",xlab="",ylab=""){
  #Creating the density functions
  plotm<-df[,c(which(colnames(df)==value),
               which(colnames(df)==categories))]
  names(plotm)[1]<-paste("mean")
  names(plotm)[2]<-paste("cat")
  cat<-unique(plotm$cat)
  narmvplotm<-na.exclude(plotm)
  ymin<-if(min(narmvplotm$mean)>0){0}else{min(narmvplotm$mean)+(0.25*min(narmvplotm$mean))}
  ymax<-max(narmvplotm$mean)+(max(narmvplotm$mean)*0.25)

  #Plotting
  barplotlocation<-barplot(plotm$mean, col=col, ylim=c(ymin,ymax),las=1,xaxt="n")
  mtext(cat, at=barplotlocation, side=1, line=0.25)
  abline(h=0)
  title(main)
  mtext(ylab,side=2,line=2.25)
  mtext(xlab,side=1,line=2.25)
  box()
}

