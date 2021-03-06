% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.file.R
\name{read.file}
\alias{read.file}
\title{read file}
\usage{
read.file(file, header = TRUE, sep = ",", dec = ".", ...)
}
\arguments{
\item{file}{File name.}

\item{header}{Whether file has header for varialbes, TRUE(default).}

\item{sep}{Field separator character, ','(default).}

\item{dec}{Decimal points'  character, '.'(default).}

\item{...}{Further arguments to be passed to read.table.}
}
\value{
this returned a data.frame.
}
\description{
\code{read.file} This function read file similar to asreml.read.table().
}
\details{
Count error for h2 and corr value, also outputs significent level.
}
\examples{
library(breedR)
breedR::read.example(package = "breedR", setpath = TRUE)
df<-breedR::read.file(file="fm.csv", header=TRUE)
names(df)
str(df)

df1<-read.csv(file="fm.csv", header=TRUE)
names(df1)
str(df1)
}
\references{
Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016
AAfun website:https://github.com/yzhlinscau/AAfun
}
\seealso{
See Also as \code{\link{read.example}}, \code{\link{fdata}}
}
\author{
Yuanzhen Lin <yzhlinscau@163.com>
}
