

library(testthat)
library(xegaSelectGene)
library(xegaPermGene)

test_that("without OK",
{
a<-1:10
b<-2:8
c<-11:12
expect_equal(without(a, b), c(1, 9, 10))
expect_equal(without(a, c), a)
expect_equal(without(b, a), integer(0))
}
)

test_that("Decay OK",
{
a<-Decay(5, 0.5)
expect_equal(length(a), 5)
expect_equal(a[1], exp(1)^(-0.5))
expect_equal(a[5], exp(1)^(-0.5*5))
}
)

