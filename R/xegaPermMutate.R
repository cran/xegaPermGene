#
# (c) 2021 Andreas Geyer-Schulz
#     Simple Genetic Algorithm in R. V0.1
#     Layer: Gene-Level Functions 
#            For a binary gene representation.
#     Package: xegaPermGene
#

#' Mutate a gene (generalized order based mutation).
#'
#' @description \code{xegaPermMutateGene} mutates a permutation.
#'               The per-position mutation rate is given by 
#'               \code{lF$BitMutationRate1()}.
#'
#' @details This operator implements a generalized 
#'          order based mutation operator (Syswerda, 1991).
#'
#'          \enumerate{
#'          \item The indices of a random subschedule are extracted.
#'          \item The subschedule is extracted, permuted, and reinserted.
#'          }
#'
#' @references Syswerda, G. (1991): 
#'             Schedule Optimization Using Genetic Algorithms.
#'             In: Davis, L. (Ed.): 
#'             Handbook of Genetic Algorithms, Chapter 21, pp. 332-349.
#'             Van Nostrand Reinhold, New York.
#'             (ISBN:0-442-00173-8) 
#'
#' @param gene     A Permutation.
#' @param lF       Local configuration of the genetic algorithm.
#'
#' @return A Permutation.
#' 
#' @family Mutation
#'
#' @examples
#' gene1<-xegaPermInitGene(lFxegaPermGene)
#' xegaPermDecodeGene(gene1, lFxegaPermGene)
#' gene<-xegaPermMutateGeneOrderBased(gene1, lFxegaPermGene)
#' xegaPermDecodeGene(gene, lFxegaPermGene)
#' @importFrom stats runif
#' @export
xegaPermMutateGeneOrderBased<-function(gene, lF)
{
	ng<-gene
	newgene1<-gene1<-gene$gene1
	l<-length(gene1)
	pos<-(1:l)[stats::runif(l, 0, 1)<lF$BitMutationRate1()]
	p<-length(pos)
        gene1[pos]<-gene1[pos[sample(1:p, p, replace=FALSE)]]
        ng$evaluated<-FALSE 
        ng$gene1<-gene1
        return(ng)
}

#' Mutate a gene (k random inversions).
#'
#' @description \code{xegaPermMutateGenekInversion} performs k random inversions.
#'              The number of inversions is exponentially decaying
#'              with exponential decay constant \code{lambda}.
#'
#' @details The only difference to the order-based mutation 
#'          operator (Syswerda, 1991) is the exponential decay 
#'          in the number of inversions. 
#'
#'          \enumerate{
#'          \item The indices of a random subschedule are extracted.
#'          \item The subschedule is extracted, permuted, and reinserted.
#'          }
#'
#' @references Syswerda, G. (1991): 
#'             Schedule Optimization Using Genetic Algorithms.
#'             In: Davis, L. (Ed.): 
#'             Handbook of Genetic Algorithms, Chapter 21, pp. 332-349.
#'             Van Nostrand Reinhold, New York.
#' 
#' @param gene   A Permutation.
#' @param lF     Local configuration of the genetic algorithm.
#'
#' @return A Permutation.
#' 
#' @family Mutation
#'
#' @examples
#' gene1<-xegaPermInitGene(lFxegaPermGene)
#' xegaPermDecodeGene(gene1, lFxegaPermGene)
#' gene<-xegaPermMutateGenekInversion(gene1, lFxegaPermGene)
#' xegaPermDecodeGene(gene, lFxegaPermGene)
#'
#' @importFrom xegaSelectGene SelectPropFitDiff
#' @export
xegaPermMutateGenekInversion<-function(gene, lF)
{
	ng<-gene
	newgene1<-gene1<-gene$gene1
	len<-length(gene1)
        l<-xegaSelectGene::SelectPropFitDiff(Decay(len-1, lF$Lambda()), lF)
	pos<-sample((1:len), l, replace=FALSE)
	p<-length(pos)
        gene1[pos]<-gene1[pos[sample(1:p, p, replace=FALSE)]]
        ng$evaluated<-FALSE
        ng$gene1<-gene1
        return(ng)
}

#' Mutate a gene (by a random 2-Opt move).
#'
#' @description \code{xegaPermMutateGene2Opt} mutates a permutation.
#'
#' @details This operator is an implementation of the 2-Opt move
#'          due to Croes (1958).
#'          
#'          Two edges are exchanged, if the exchange improves the result.
#'
#' @references Croes, G. A. (1958): 
#'             A Method for Solving Traveling-Salesman Problems.
#'             Operations Research, 6(6), pp. 791-812.
#'             <doi:10.1287/opre.6.6.791>
#' 
#' @param gene   A Permutation.
#' @param lF     Local configuration of the genetic algorithm.
#'
#' @return A Permutation.
#' 
#' @family Mutation
#'
#' @examples
#' gene1<-xegaPermInitGene(lFxegaPermGene)
#' xegaPermDecodeGene(gene1, lFxegaPermGene)
#' gene<-xegaPermMutateGene2Opt(gene1, lFxegaPermGene)
#' xegaPermDecodeGene(gene, lFxegaPermGene)
#' @importFrom stats runif
#' @export
xegaPermMutateGene2Opt<-function(gene, lF)
{
	newgene<-gene
	tour<-lF$penv$rnd2Opt(gene$gene1, lF$Max2Opt())
	newgene$evaluated<-FALSE
	newgene$gene1<-tour
	newgene<-lF$EvalGene(newgene, lF)  
	return(newgene)
}

#' Mutate a gene (by a random Lin-Kernighan k-OPT move).
#'
#' @description \code{xegaPermMutateGenekOptLK} mutates a permutation.
#'
#' @details This operator implements a random k-Opt move
#'          version of the Lin-Kernighan heuristic.
#'          
#'          A sequence of random 2-Opt moves, all of which improve 
#'          the result is executed.
#'
#' @references Lin, S. and Kernighan. B. W. (1973): 
#'             An Effective Heuristic Algorithm 
#'             for the Traveling-Salesman Problem.
#'             Operations Research, 21(2), pp. 791-812.
#'             <doi:10.1287/opre.21.2.498>
#' 
#' @param gene   A Permutation.
#' @param lF     Local configuration of the genetic algorithm.
#'
#' @return A Permutation.
#' 
#' @family Mutation
#'
#' @examples
#' gene1<-xegaPermInitGene(lFxegaPermGene)
#' xegaPermDecodeGene(gene1, lFxegaPermGene)
#' gene<-xegaPermMutateGenekOptLK(gene1, lFxegaPermGene)
#' xegaPermDecodeGene(gene, lFxegaPermGene)
#' @importFrom stats runif
#' @export
xegaPermMutateGenekOptLK<-function(gene, lF)
{
 newgene<-gene
 tour<-lF$penv$LinKernighan(gene$gene1, lF$Max2Opt())
 newgene$evaluated<-FALSE
 newgene$gene1<-tour
 newgene<-lF$EvalGene(newgene, lF)
 return(newgene)
}

#' Mutate a gene (by inserting a greedy path at a random start position with a random length of k).
#'
#' @description \code{xegaPermMutateGeneGreedy} mutates a permutation 
#'              by inserting a greedy path of length \code{k} 
#'              at a random position \code{start}.
#'
#' @details    The path length \code{k} is exponentially decaying
#'              with exponential decay constant \code{lambda}.
#'
#' @param gene  A Permutation. 
#' @param lF    Local configuration of the genetic algorithm.
#'
#' @return A Permutation.
#' 
#' @family Mutation
#'
#' @examples
#' gene1<-xegaPermInitGene(lFxegaPermGene)
#' xegaPermDecodeGene(gene1, lFxegaPermGene)
#' gene<-xegaPermMutateGeneGreedy(gene1, lFxegaPermGene)
#' xegaPermDecodeGene(gene, lFxegaPermGene)
#' @importFrom xegaSelectGene SelectPropFit
#' @export
xegaPermMutateGeneGreedy<-function(gene, lF)
{ newgene<-gene
  ng1<-newgene$gene1
  l<-length(ng1)
  start<-sample(1:l, 1)
  k<-xegaSelectGene::SelectPropFit(Decay(l-1, lF$Lambda()), lF)
  path<-lF$penv$greedy(ng1[start], k)
    if (length(path)==length(ng1))
            { newgene$evaluated<-TRUE
              newgene$gene1<-path
              newgene<-lF$EvalGene(newgene, lF)
              return(newgene)}
  ng<-without(ng1,path) 
  index<-(1:length(ng))<sample(1:length(ng),1)
  new<-c(ng[index], path, ng[!index])
  newgene$evaluated<-TRUE
  newgene$gene1<-new
  newgene<-lF$EvalGene(newgene, lF)
  return(newgene)}

#' Mutate a gene (by inserting the best greedy path at a random start position with a random length of k).
#'
#' @description \code{xegaPermMutateGeneBestGreedy} mutates a permutation 
#'              by inserting the best greedy path of length \code{k} 
#'              at a random position \code{start}.
#'
#' @details    The path length \code{k} is exponentially decaying
#'              with exponential decay constant \code{lF$lambda()}.
#'
#' @param gene     A Permutation.
#' @param lF       Local configuration of the genetic algorithm.
#'
#' @return         A Permutation
#' 
#' @family Mutation
#'
#' @examples
#' gene1<-xegaPermInitGene(lFxegaPermGene)
#' xegaPermDecodeGene(gene1, lFxegaPermGene)
#' gene<-xegaPermMutateGeneGreedy(gene1, lFxegaPermGene)
#' xegaPermDecodeGene(gene, lFxegaPermGene)
#' @importFrom xegaSelectGene SelectPropFit
#' @export
xegaPermMutateGeneBestGreedy<-function(gene, lF)
{ newgene<-gene
  ng1<-newgene$gene1
  l<-length(ng1)
  k<-xegaSelectGene::SelectPropFit(Decay(l-1, lF$Lambda()), lF)
  path<-lF$penv$kBestGreedy(k)
    if (length(path)==length(ng1))
            { newgene$evaluated<-TRUE
              newgene$gene1<-path
              newgene<-lF$EvalGene(newgene, lF)
              return(newgene)}
  ng<-without(ng1,path) 
  index<-(1:length(ng))<sample(1:length(ng),1)
  new<-c(ng[index], path, ng[!index])
  newgene$evaluated<-TRUE
  newgene$gene1<-new
  newgene<-lF$EvalGene(newgene, lF)
  return(newgene)}

#' Mutation by a random mutation function.
#'
#'  A mutation function is randomly selected from the following list:
#'  xegaPermMutateGeneOrderBased, xegaPermMutateGenekInversion,
#'  xegaPermMutateGene2Opt, xegaPermMutateGenekOptLK, xegaPermMutateGeneGreedy,
#'  xegaPermMutateGeneBestGreedy.
#'
#' @param gene   A permutation. 
#' @param lF     Local configuration.
#'
#' @return  A permutation.
#' 
#' @family Mutation
#'
#' @examples
#' gene1<-xegaPermInitGene(lFxegaPermGene)
#' xegaPermDecodeGene(gene1, lFxegaPermGene)
#' gene<-xegaPermMutateMix(gene1, lFxegaPermGene)
#' xegaPermDecodeGene(gene, lFxegaPermGene)
#'@export
xegaPermMutateMix<-function(gene, lF) {
ML<-list(xegaPermMutateGeneOrderBased, xegaPermMutateGenekInversion,
    xegaPermMutateGene2Opt, xegaPermMutateGenekOptLK, xegaPermMutateGeneGreedy,
    xegaPermMutateGeneBestGreedy)
ng<-ML[[sample(1:length(ML),1)]](gene, lF)
return(ng)
}	

#' Configure the mutation function of a genetic algorithm.
#'
#' @description \code{xegaPermMutationFactory} implements the selection
#'              of one of the gene mutation functions in this 
#'              package by specifying a text string.
#'              The selection fails ungracefully (produces
#'              a runtime error) if the label does not match.
#'              The functions are specified locally.             
#'         
#' Current Support:
#' 
#' \enumerate{
#' \item "MutateGene" returns \code{xegaPermMutateGeneOrderBased}.
#' \item "MutateGeneOrderBased" returns \code{xegaPermMutateGeneOrderBased}.
#' \item "MutateGenekInversion" returns \code{xegaPermMutateGenekInversion}.
#' \item "MutateGene2Opt" returns \code{xegaPermMutateGene2Opt}.
#' \item "MutateGenekOptLK" returns \code{xegaPermMutateGenekOptLK}.
#' \item "MutateGeneGreedy" returns \code{xegaPermMutateGeneGreedy}.
#' \item "MutateGeneBestGreedy" returns \code{xegaPermMutateGeneBestGreedy}.
#' \item "MutateGeneMix" returns \code{xegaPermMutateMix}.
#' }
#'
#' @param method   The name of the mutation method.
#'
#' @return A permutation based mutation function.
#'  
#' @family Configuration
#' 
#' @examples
#' xegaPermMutationFactory(method="MutateGene")
#' @export
xegaPermMutationFactory<-function(method="MutateGene") {
if (method=="MutateGene") {f<- xegaPermMutateGeneOrderBased}
if (method=="MutateGeneOrderBased") {f<- xegaPermMutateGeneOrderBased}
if (method=="MutateGenekInversion") {f<- xegaPermMutateGenekInversion}
if (method=="MutateGene2Opt") {f<- xegaPermMutateGene2Opt}
if (method=="MutateGenekOptLK") {f<- xegaPermMutateGenekOptLK}
if (method=="MutateGeneGreedy") {f<- xegaPermMutateGeneGreedy}
if (method=="MutateGeneBestGreedy") {f<- xegaPermMutateGeneBestGreedy}
if (method=="MutateGeneMix") {f<- xegaPermMutateMix}
if (!exists("f", inherits=FALSE))
        {stop("xegaPerm Mutation Factory label ", method, " does not exist")}

return(f)
}

