

library(testthat)
library(xegaSelectGene)
library(xegaPermGene)

test_that("xegaPermDecodeGene OK",
{
g<-xegaPermInitGene(lFxegaPermGene)
expect_identical(xegaPermDecodeGene(g), g$gene1)
}
)

