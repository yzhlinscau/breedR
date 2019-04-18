#' @title read file
#'
#' @description
#' \code{read.file} This function read file similar to asreml.read.table().
#'
#' @details
#' This function read file similar to asreml.read.table().
#' @aliases read.file
#' @param file	 File name.
#' @param header	 Whether file has header for varialbes, TRUE(default).
#' @param sep	 Field separator character, ','(default).
#' @param dec	  Decimal points'  character, '.'(default).
#' @param ...	 Further arguments to be passed to read.table.
#' @return this returned a data.frame.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016
#' AAfun website:https://github.com/yzhlinscau/AAfun
#' @seealso See Also as \code{\link{read.example}}, \code{\link{fdata}}
#' @examples
#' library(breedR)
#' breedR::read.example(package = "breedR", setpath = TRUE)
#' df<-breedR::read.file(file="fm.csv", header=TRUE)
#' names(df)
#'
#' @export
read.file<-function(file,header=TRUE,sep=',',dec='.',...){
  df<-read.table(file=file,header=header,sep=sep,dec=dec,...)
  aa<-names(df)

  sn<-grep('^[A-Z]{1}',aa)
  for(i in sn) df[,i]<-factor(df[,i])

  return(df)
}


#' @title read file list
#'
#' @description
#' \code{read.example} This function read file list under one package, or
#' sets working directory under package.
#'
#' @aliases read.example
#' @param package	 package name.
#' @param setpath	 Whether set working directory under package, FALSE(default).
#' @return this returned a path or file list.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016
#' AAfun website:https://github.com/yzhlinscau/AAfun
#' @seealso See Also as \code{\link{read.file}}, \code{\link{fdata}}
#' @examples
#' library(breedR)
#' # read file list under a package
#' breedR::read.example(package = "breedR")
#'
#' # set working directory under a package
#' breedR::read.example(package = "breedR", setpath = TRUE)
#' getwd()
#'
#' @export
read.example <- function(package,setpath = FALSE) {
  if (setpath== FALSE) {
    dir(system.file("extdata", package = package))
  } else {
    path<-system.file("extdata", package = package)
    setwd(path)
  }
}


#' @title format dataset
#'
#' @description
#' \code{fdata} This function will format dataset for varialbes to factors.
#'
#' @details
#' This function reads file list under one package, or sets working directory under package.
#' @aliases fdata
#' @param package	 package name.
#' @param setpath	 Whether set working directory under package, FALSE(default).
#' @return this returned a path or file list.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016
#' AAfun website:https://github.com/yzhlinscau/AAfun
#' @seealso See Also as \code{\link{read.file}}, \code{\link{read.example}}
#' @examples
#' library(datasets)
#' names(mtcars)
#'
#' mtcars1<-fdata(mtcars,faS=c(2,8,9))
#'
#' @export
fdata<-function(data,faS=NULL){
  data<-as.data.frame(data)
  if(is.null(faS)){
    aa<-names(data)

    sn<-grep('^[A-Z]{1}',aa)
  } else sn<-faS

  for(i in sn) data[,i]<-factor(data[,i])

  return(data)
}



