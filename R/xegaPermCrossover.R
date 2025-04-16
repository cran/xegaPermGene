#
# (c) 2021 Andreas Geyer-Schulz
#     Simple Genetic Algorithm in R. V0.1
#     Layer: Gene-Level Functions 
#            For a binary gene representation.
#     Package: xegaPermGene
#

#' Position-based crossover of 2 genes. 
#'
#' @description \code{xegaPermCross2Gene} determines 
#'              a random subschedule of random length.
#'              
#'              It copies the random subschedule into a new gene.
#'              The rest of the positions of the new scheme is filled 
#'              with the elements of the other gene to complete the
#'              permutation. This is done for each gene.
#'
#' @param gg1    Permutation.
#' @param gg2    Permutation.
#' @param lF     Local configuration of the genetic algorithm.
#'
#' @return List of 2 permutations.
#'
#' @references Syswerda, G. (1991): 
#'             Schedule Optimization Using Genetic Algorithms.
#'             In: Davis, L. (Ed.): 
#'             Handbook of Genetic Algorithms, Chapter 21, p. 343.
#'             Van Nostrand Reinhold, New York.
#'             (ISBN:0-442-00173-8)
#'
#' @family Crossover
#'
#' @examples
#' gene1<-xegaPermInitGene(lFxegaPermGene)
#' gene2<-xegaPermInitGene(lFxegaPermGene)
#' xegaPermDecodeGene(gene1, lFxegaPermGene)
#' xegaPermDecodeGene(gene2, lFxegaPermGene)
#' newgenes<-xegaPermCross2Gene(gene1, gene2)
#' xegaPermDecodeGene(newgenes[[1]], lFxegaPermGene)
#' xegaPermDecodeGene(newgenes[[2]], lFxegaPermGene)
#' @export
xegaPermCross2Gene<-function(gg1, gg2, lF)
{
    newg1<-gg1; newg2<-gg2
    ng1<-g1<-gg1$gene1; ng2<-g2<-gg2$gene1
    l<-length(g1)	
    index<-1:l
    cut<-sl<-sample(index, 1); ss1<-sample(index, sl, replace=FALSE)
    ng2[ss1]<-g1[ss1]
    ng1[ss1]<-g2[ss1]
    ss2<-without(index, ss1)
    ng2[ss2]<-without(g2, g1[ss1])
    ng1[ss2]<-without(g1, g2[ss1])
    newg1$evaluated<-FALSE
    newg1$gene1<-ng1
    newg2$evaluated<-FALSE
    newg2$gene1<-ng2
     return(list(newg1, newg2))   
}


#' Position-based crossover of 2 genes. 
#'
#' @description \code{xegaPermCrossGene} determines 
#'              a random subschedule of random length.
#'              
#'              It copies the random subschedule into a new gene.
#'              The rest of the positions of the new scheme is filled 
#'              with the elements of the other gene to complete the
#'              permutation. 
#'
#' @param gg1   Permutation.
#' @param gg2   Permutation.
#' @param lF    Local configuration of the genetic algorithm.
#'
#' @return A list of 2 permutations.
#'
#' @references Syswerda, G. (1991): 
#'             Schedule Optimization Using Genetic Algorithms.
#'             In: Davis, L. (Ed.): 
#'             Handbook of Genetic Algorithms, Chapter 21, p. 343.
#'             Van Nostrand Reinhold, New York.
#'             (ISBN:0-442-00173-8)
#'
#' @family Crossover
#'
#' @examples
#' gene1<-xegaPermInitGene(lFxegaPermGene)
#' gene2<-xegaPermInitGene(lFxegaPermGene)
#' xegaPermDecodeGene(gene1, lFxegaPermGene)
#' xegaPermDecodeGene(gene2, lFxegaPermGene)
#' newgenes<-xegaPermCrossGene(gene1, gene2)
#' xegaPermDecodeGene(newgenes[[1]], lFxegaPermGene)
#' @export
xegaPermCrossGene<-function(gg1, gg2, lF)
{
    newg1<-gg1
    ng1<-g1<-gg1$gene1; g2<-gg2$gene1
    l<-length(g1)	
    index<-1:l
    cut<-sl<-sample(index, 1); ss1<-sample(index, sl, replace=FALSE)
    ng1[ss1]<-g2[ss1]
    ss2<-without(index, ss1)
    ng1[ss2]<-without(g1, g2[ss1])
    newg1$evaluated<-FALSE
    newg1$gene1<-ng1
     return(list(newg1))   
}

#' Configure the crossover function of a genetic algorithm.
#'
#' @description \code{xegaPermCrossoverFactory} implements the selection
#'              of one of the crossover functions in this
#'              package by specifying a text string.
#'              The selection fails ungracefully (produces
#'              a runtime error) if the label does not match.
#'              The functions are specified locally.
#'
#'              Current support:
#'
#'              \enumerate{
#'              \item Crossover functions with two kids:
#'              \enumerate{
#'              \item "Cross2Gene" returns \code{xegaPermCross2Gene}.
#'              }
#'              \item Crossover functions with one kid:
#'              \enumerate{
#'              \item "CrossGene" returns \code{xegaPermCrossGene}.
#'              }
#'              }
#'
#' @param method     A string specifying the crossover function.
#'
#' @return A crossover function for genes.
#'
#' @family Configuration
#'
#' @examples
#' XGene<-xegaPermCrossoverFactory("Cross2Gene")
#' gene1<-xegaPermInitGene(lFxegaPermGene)
#' gene2<-xegaPermInitGene(lFxegaPermGene)
#' XGene(gene1, gene2, lFxegaPermGene)
#' @export
xegaPermCrossoverFactory<-function(method="Cross2Gene") {
if (method=="Cross2Gene") {f<- xegaPermCross2Gene}
if (method=="CrossGene") {f<- xegaPermCrossGene}
if (!exists("f", inherits=FALSE))
        {stop("xegaPerm Crossover label ", method, " does not exist")}
return(f)
}

