% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xegaPermMutate.R
\name{xegaPermMutateGene2Opt}
\alias{xegaPermMutateGene2Opt}
\title{Mutate a gene (by a random 2-Opt move).}
\usage{
xegaPermMutateGene2Opt(gene, lF)
}
\arguments{
\item{gene}{A Permutation.}

\item{lF}{Local configuration of the genetic algorithm.}
}
\value{
A Permutation.
}
\description{
\code{xegaPermMutateGene2Opt} mutates a permutation.
}
\details{
This operator is an implementation of the 2-Opt move
         due to Croes (1958).
         
         Two edges are exchanged, if the exchange improves the result.
}
\examples{
gene1<-xegaPermInitGene(lFxegaPermGene)
xegaPermDecodeGene(gene1, lFxegaPermGene)
gene<-xegaPermMutateGene2Opt(gene1, lFxegaPermGene)
xegaPermDecodeGene(gene, lFxegaPermGene)
}
\references{
Croes, G. A. (1958): 
            A Method for Solving Traveling-Salesman Problems.
            Operations Research, 6(6), pp. 791-812.
            <doi:10.1287/opre.6.6.791>
}
\seealso{
Other Mutation: 
\code{\link{xegaPermMutateGeneBestGreedy}()},
\code{\link{xegaPermMutateGeneGreedy}()},
\code{\link{xegaPermMutateGeneOrderBased}()},
\code{\link{xegaPermMutateGenekInversion}()},
\code{\link{xegaPermMutateGenekOptLK}()},
\code{\link{xegaPermMutateMix}()}
}
\concept{Mutation}
