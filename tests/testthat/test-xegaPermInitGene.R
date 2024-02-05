

library(testthat)
library(xegaSelectGene)
library(xegaPermGene)

test_that("xegaPermInitGene OK",
{
g<-xegaPermInitGene(lFxegaPermGene)
len<-lFxegaPermGene$penv$genelength()
expect_identical(g$evaluated, FALSE)
expect_identical(g$evalFail, FALSE)
expect_identical(g$fit, 0)
expect_equal(length(g$gene1), len)
expect_identical(without(g$gene1, (1:len)), integer(0))
}
)

