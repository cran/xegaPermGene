#
# (c) 2021 Andreas Geyer-Schulz
#     Simple Genetic Algorithm in R. V0.1
#     Layer: Gene-Level Functions 
#            For a binary gene representation.
#     Package: xegaPermGene
#

#' Initialize a gene with a permutation of integers
#'
#' @description \code{xegaPermInitGene} generates a random permutation 
#'              with a given length n.
#'
#'
#' @details In the permutation representation of 
#'          package \code{xegaPerm}, a \emph{gene} is a list with 
#'          \enumerate{
#'          \item \code{$evaluated}: Boolean: TRUE if the fitness is known.
#'          \item \code{$fit}:       The fitness of the genotype of 
#'                                  \code{$gene1}.         
#'          \item \code{$gene1}:     The permutation (the genotype).
#'          }
#'
#' @param lF   Local configuration of the genetic algorithm.
#'
#' @return A permutation gene.
#'
#' @family Initialization
#'
#' @examples
#' xegaPermInitGene(lFxegaPermGene)
#'
#' @export
xegaPermInitGene<-function(lF)
{
gene1<-sample(1:lF$penv$genelength(), lF$penv$genelength(), replace=FALSE)
return(list(evaluated=FALSE, evalFail=FALSE, fit=0, gene1=gene1))
}

