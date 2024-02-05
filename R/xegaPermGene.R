#
# (c) 2021 Andreas Geyer-Schulz
#     Simple Genetic Algorithm in R. V0.1
#     Layer: Gene-Level Functions 
#            For a binary gene representation.
#     Package: xegaPermGene
#

# require(xegaSelectGene)

#' Returns elements of 
#' vector \code{x} without elements in \code{y}. 
#'
#' @param x   Vector.
#' @param y   Vector.
#'
#' @return Vector.
#'
#' @family Utility
#'
#' @examples
#' a<-sample(1:15,15, replace=FALSE)
#' b<-c(1, 3, 5)
#' without(a, b)
#' @export
without<-function(x, y)
{
        x[!unlist(lapply(x, function(z) is.element(z, y)))]
}

#' Exponential decay. 
#'
#' @param t        Number of objects.
#' @param lambda   Exponential decay constant.
#' 
#' @return Vector with t elements with values of exponential decay.
#'         
#' @family Utility
#'
#' @examples
#' Decay(5, 0.4)
#' Decay(10, 0.4)
#'
#' @export
Decay<-function(t, lambda=0.05) { exp(1)^(-lambda*(1:t))}

