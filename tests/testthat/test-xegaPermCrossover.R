

library(testthat)
library(xegaSelectGene)
library(xegaPermGene)

test_that("xegaPermCross2Gene OK",
 {
gene1<-xegaPermInitGene(lFxegaPermGene)
gene2<-xegaPermInitGene(lFxegaPermGene)
newgenes<-xegaPermCross2Gene(gene1, gene2)
expect_identical(identical(gene1, newgenes[[1]]), FALSE)
expect_identical(identical(gene1, newgenes[[2]]), FALSE)
expect_identical(identical(gene2, newgenes[[1]]), FALSE)
expect_identical(identical(gene2, newgenes[[2]]), FALSE)
}
)

test_that("xegaPermCrossGene OK",
 {
gene1<-xegaPermInitGene(lFxegaPermGene)
gene2<-xegaPermInitGene(lFxegaPermGene)
newgenes<-xegaPermCrossGene(gene1, gene2)
expect_identical(identical(gene1, newgenes[[1]]), FALSE)
}
)

test_that("xegaPermCrossoverFactory Cross2Gene OK",
 {
 f<-xegaPermCrossoverFactory(method="Cross2Gene")
 expect_identical(body(f), body(xegaPermGene::xegaPermCross2Gene))
}
)

test_that("xegaPermCrossoverFactory CrossGene OK",
 {
 f<-xegaPermCrossoverFactory(method="CrossGene")
 expect_identical(body(f), body(xegaPermGene::xegaPermCrossGene))
}
)

test_that("xegaPermCrossoverFactory sgunknown OK",
 {
 expect_error(
 xegaPermCrossoverFactory(method="sgunknown"),
 "sgunknown")
}
)

