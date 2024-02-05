

library(testthat)
library(xegaSelectGene)
library(xegaPermGene)

test_that("xegaPermMutateGeneOrderBased OK",
 {
gene1<-xegaPermInitGene(lFxegaPermGene)
newgene<-xegaPermMutateGeneOrderBased(gene1, lFxegaPermGene)
expect_identical(identical(gene1, newgene), FALSE)
}
)

test_that("xegaPermMutateGenekInversion OK",
 {
gene1<-xegaPermInitGene(lFxegaPermGene)
newgene<-xegaPermMutateGenekInversion(gene1, lFxegaPermGene)
expect_identical(identical(gene1, newgene), FALSE)
}
)

test_that("xegaPermMutateGene2Opt OK",
 {
gene1<-xegaPermInitGene(lFxegaPermGene)
newgene<-xegaPermMutateGene2Opt(gene1, lFxegaPermGene)
expect_identical(identical(gene1, newgene), FALSE)
}
)

test_that("xegaPermMutateGenekOptLK OK",
 {
gene1<-xegaPermInitGene(lFxegaPermGene)
newgene<-xegaPermMutateGenekOptLK(gene1, lFxegaPermGene)
expect_identical(identical(gene1, newgene), FALSE)
}
)

test_that("xegaPermMutateGeneGreedy OK",
 {
gene1<-xegaPermInitGene(lFxegaPermGene)
newgene<-xegaPermMutateGeneGreedy(gene1, lFxegaPermGene)
expect_identical(identical(gene1, newgene), FALSE)
}
)

test_that("xegaPermMutateGeneGreedy Repeat TSP4 OK",
 {
 a<-lau15$dist()[1:4, 1:4]
 TSP4<-newTSP(a, "TSP4")
 lFxegaPermGene$penv<-TSP4
 gene1<-xegaPermInitGene(lFxegaPermGene)
 set.seed(1)
 for (i in 1:10)
{ newgene<-xegaPermMutateGeneGreedy(gene1, lFxegaPermGene)
expect_identical(identical(gene1, newgene), FALSE) }
}
)

test_that("xegaPermMutateGeneBestGreedy OK",
 {
gene1<-xegaPermInitGene(lFxegaPermGene)
newgene<-xegaPermMutateGeneBestGreedy(gene1, lFxegaPermGene)
expect_identical(identical(gene1, newgene), FALSE)
}
)

test_that("xegaPermMutateGeneBestGreedy Repeat TSP4 OK",
 {
 a<-lau15$dist()[1:4, 1:4]
 TSP4<-newTSP(a, "TSP4")
 lFxegaPermGene$penv<-TSP4
 gene1<-xegaPermInitGene(lFxegaPermGene)
 set.seed(1)
 for (i in 1:10)
{ newgene<-xegaPermMutateGeneBestGreedy(gene1, lFxegaPermGene)
expect_identical(identical(gene1, newgene), FALSE) }
}
)

test_that("xegaPermMutateMix OK",
 {
gene1<-xegaPermInitGene(lFxegaPermGene)
set.seed(5)
newgene<-xegaPermMutateMix(gene1, lFxegaPermGene)
expect_identical(identical(gene1, newgene), FALSE)
}
)

test_that("xegaPermMutationFactory MutateGene OK",
 {
 f<-xegaPermMutationFactory(method="MutateGene")
 expect_identical(body(f), body(xegaPermGene::xegaPermMutateGeneOrderBased))
}
)

test_that("xegaPermMutationFactory MutateGeneOrderBased OK",
 {
 f<-xegaPermMutationFactory(method="MutateGeneOrderBased")
 expect_identical(body(f), body(xegaPermGene::xegaPermMutateGeneOrderBased))
}
)

test_that("xegaPermMutationFactory MutateGenekInversion OK",
 {
 f<-xegaPermMutationFactory(method="MutateGenekInversion")
 expect_identical(body(f), body(xegaPermGene::xegaPermMutateGenekInversion))
}
)

test_that("xegaPermMutationFactory MutateGene2Opt OK",
 {
 f<-xegaPermMutationFactory(method="MutateGene2Opt")
 expect_identical(body(f), body(xegaPermGene::xegaPermMutateGene2Opt))
}
)

test_that("xegaPermMutationFactory MutateGenekOptLK OK",
 {
 f<-xegaPermMutationFactory(method="MutateGenekOptLK")
 expect_identical(body(f), body(xegaPermGene::xegaPermMutateGenekOptLK))
}
)

test_that("xegaPermMutationFactory MutateGeneGreedy OK",
 {
 f<-xegaPermMutationFactory(method="MutateGeneGreedy")
 expect_identical(body(f), body(xegaPermGene::xegaPermMutateGeneGreedy))
}
)

test_that("xegaPermMutationFactory MutateGeneBestGreedy OK",
 {
 f<-xegaPermMutationFactory(method="MutateGeneBestGreedy")
 expect_identical(body(f), body(xegaPermGene::xegaPermMutateGeneBestGreedy))
}
)

test_that("xegaPermMutationFactory MutateGeneMix OK",
 {
 f<-xegaPermMutationFactory(method="MutateGeneMix")
 expect_identical(body(f), body(xegaPermGene::xegaPermMutateMix))
}
)

test_that("xegaPermMutationFactory sgunknown OK",
 {
 expect_error(
 xegaPermMutationFactory(method="sgunknown"),
 "sgunknown")
}
)

