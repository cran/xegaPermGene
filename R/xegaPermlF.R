#
# (c) 2021 Andreas Geyer-Schulz
#     Simple Genetic Algorithm in R. V0.1
#     Layer: Gene-Level Functions 
#            For a binary gene representation.
#     Package: xegaPermGene
#

#' Generate local functions and objects. 
#'
#' @description
#' \code{lFxegaPermGene} is a list of functions 
#' which contains a definition of all local objects 
#' required for the use of genetic operators with the 
#  permutation representation. 
#' We refer to this object as local configuration. 
#'
#' @details
#'    We use the local function list (the local configuration) for 
#'    \enumerate{
#'    \item
#'       replacing all constants with constant functions.
#'       
#'       Rationale: We need one formal argument (the local function list lF)
#'       and we can dispatch multiple functions. E.g.  \code{lF$verbose()}
#'   \item    
#'       for dynamically binding a local function with a definition from a
#'       proper function factory. E.g. the selection methods 
#'       \code{lf$SelectGene} and \code{SelectMate}.
#'       
#'  \item for gene representation specific special functions:
#'        \code{lf$InitGene}, \code{lF$DecodeGene}, \code{lf$EvalGene}
#'        \code{lf$ReplicateGene}, ...
#'       
#'  } 
#'
#' @family Configuration
#'
#' @importFrom xegaSelectGene parm
#' @importFrom xegaSelectGene lau15
#' @export 
lFxegaPermGene<-list(
penv=xegaSelectGene::lau15,
replay=xegaSelectGene::parm(0),
verbose=xegaSelectGene::parm(4),
MutationRate=xegaSelectGene::parm(0.2),
BitMutationRate1=xegaSelectGene::parm(0.2),
Max2Opt=xegaSelectGene::parm(100),
Lambda=xegaSelectGene::parm(0.05),
CrossRate=xegaSelectGene::parm(0.8),
Max=xegaSelectGene::parm(1),
Offset=xegaSelectGene::parm(1),
Eps=xegaSelectGene::parm(0.01),
Elitist=xegaSelectGene::parm(TRUE),
TournamentSize=xegaSelectGene::parm(2),
SelectGene=xegaSelectGene::SelectGeneFactory(method="Proportional"),
SelectMate=xegaSelectGene::SelectGeneFactory(method="Proportional"),
MutateGene=xegaPermMutationFactory(method="MutateGene2Opt"),
CrossGene=xegaPermCrossGene,
InitGene=xegaPermInitGene,
DecodeGene=xegaPermDecodeGene,
EvalGene=xegaSelectGene::EvalGeneFactory(method="EvalGeneU"),
lapply=base::lapply
)
