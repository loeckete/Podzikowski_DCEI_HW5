#' Hot spot and hot moment category
#'
#' This function allows you to create 'hot spot' and 'hot moment categories from a dataframe
#' @param df the dataframe
#' @param cutvariable in quotations is the column name you want to use to break the dataframe into categories
#' @param brkpts is a list of numbers to cut the dataset into variables.  The brkpts must be equal the number of categories plus one.  The numbers should bracket the values you want within a category, rather than equal those values.
#' @param categories is a list of number or characters.  The categories create the HSHM varaibles.
#' @param scenario default is "cat", will create a new category if you would like to create more than one HSHM category from a dataframe (e.g. scenario="spatial" will create a column name "HSHM.spatial)
#'
#' @examples
#' x<-c(1,2,3,4,5,6,7,8,9,10,11)
#' y<-c(12,13,14,15,16,17,18,19,20,21,22)
#' df<-data.frame(x,y)
#' df<-HSHMcategories(df,"y",brkpts=c(11,16.5,23),categories=c("low","middle","high"))

HSHMcategories<-function(df,cutvariable,brkpts,categories,scenario="cat"){
  fullm<-df[,which(colnames(df)==cutvariable)]
  fullm<-data.frame(fullm)
  fullm$HSHM.cat<-cut(fullm[,1],
                      brkpts,
                      labels=categories)
  fullm$HSHM.cat<-as.factor(fullm$HSHM.cat)
  df[paste("HSHM.",scenario,sep="")]<-fullm$HSHM.cat
  summary(df)
  return(df)
}


#' Hot spot and hot moment index
#'
#' This function allows you to calculate a mean, proportion, and index for a HSHM
#' @param df1 the dataframe
#' @param response in quotations, the response variable
#' @param category in quotations, the HSHM category variable
#' @param dfspatial default is df1, Allows you to use a different spatial dataframe
#'
#' @examples
#' x<-c(1,2,3,4,5,6,7,8,9,10,11)
#' y<-c(12,13,14,15,16,17,18,19,20,21,22)
#' z<-c(1.3245234,2.2354256,3.34543,4.34634,5.43643,6.3463,7.34643,8.57345,9.65857,10.68454357,11.67986543)
#' df<-data.frame(x,y,z)
#' df<-HSHMcategories(df,"y",brkpts=c(11,16.5,23),categories=c("low","middle","high"))
#' HSHMIndex_df<-HSHMindex(df,"z","HSHM.cat")

HSHMindex<-function(df1,response,category,dfspatial=df1){
  m1<-df1[,c(which( colnames(df1)==response),which( colnames(df1)==category))]
  HSHMcatmean<-tapply(m1[,1],m1[,2],mean)
  HSHMcatse<-tapply(m1[,1],m1[,2],se)
  HSHMcatN<-tapply(m1[,1],m1[,2],N)
  Group.1<-levels(m1[,2])
  HSHMcat<-data.frame(Group.1,HSHMcatmean,HSHMcatse,HSHMcatN)
  names(HSHMcat)[1]<-paste("cat")
  names(HSHMcat)[2]<-paste("mean",response,sep="")
  names(HSHMcat)[3]<-paste("se",response,sep="")
  names(HSHMcat)[4]<-paste("n",response,sep="")

  m2<-data.frame(dfspatial[,which(colnames(dfspatial)==category)])
  Percent<-c(c(summary(m2[,1])[1:NROW(summary(m2[,1]))])/NROW(m2))
  Percent.1<-data.frame(Percent, levels(m2[,1]))
  names(Percent.1)[2]<-paste("cat")

  m3<-df1[,c(which(colnames(df1)==response),which(colnames(df1)==category))]
  names(m3)[1]<-paste("response")
  names(m3)[2]<-paste("cat")
  total<-sum(m3$response)
  SumCat<-tapply(m3$response,m3$cat,sum)
  cat<-levels(m3$cat)
  Proportion<-data.frame(SumCat,total,cat)

  Proportion<-merge(HSHMcat,Proportion,by="cat")
  Proportion<-merge(Proportion,Percent.1,by="cat")
  Proportion["Prop"]<-Proportion$SumCat/Proportion$total
  Proportion["Index"]<-Proportion$Prop/Proportion$Percent
  Proportion$Index<-Proportion$Index-1
  return(Proportion)
}

