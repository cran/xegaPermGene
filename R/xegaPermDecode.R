#
# (c) 2021 Andreas Geyer-Schulz
#     Simple Genetic Algorithm in R. V0.1
#     Layer: Gene-Level Functions 
#            For a binary gene representation.
#     Package: xegaPermGene
#

#' Decode a permutation. 
#' 
#' @description \code{xegaPermDecodeGene} decodes a permutation gene.
#' @details      \code{xegaPermDecodeGene} is the identy function.
#'
#' @param gene  Permutation.
#' @param lF    Local configuration of the genetic algorithm.
#' 
#' @return A permutation gene.
#'
#' @family Decoder
#'
#' @examples
#' g<-xegaPermInitGene(lFxegaPermGene)
#' xegaPermDecodeGene(g)
#' @export
xegaPermDecodeGene<-function(gene, lF)
{
  gene$gene1
}

