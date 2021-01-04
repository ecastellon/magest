# -*- coding: utf-8 -*-

## tests
context("procesamiento")

test_that("crea-excel",
{
    dd <- data.frame(x = 1:3, y = 3:1)
    ff <- RZ$find_file("data/tst.xlsx")
    expect_error(save_cel_xcl(aa))
    expect_error(save_cel_xcl(dd, file = ff, col = 0))
    expect_true(save_cel_xcl(dd, file = ff))
})

test_that("leer-excel",
{
    ff <- RZ$find_file("data/tst.xlsx")
    expect_error(read_cel_xcl("aa.xlsx"))
    expect_error(read_cel_xcl(ff, crf2=""))
    expect_error(read_cel_xcl(ff, 2))
    expect_s3_class(read_cel_xcl(ff, crf2 = "B$3"), "data.frame")
})


test_that("xsql",
{
    expect_error(xsql())
    expect_error(xsql(list("a")))
    expect_error(xsql(list("a","b")))
    expect_error(xsql("a", c("x", "y")))
    expect_error(xsql(c("a", "b"), joi = "in"))
    expect_error(xsql(list(list("a", c("a", "b")),
                           list("b", c("d", "e"))), joi = "ni"))
    expect_error(xsql(list(list("a", c("a", "b")),
                           list("b", c("d", "e"))), joi = "in"))
    expect_equal(xsql(list(list("a", "b"))),
                 "select b from a")
    expect_equal(xsql(list(list("a", c("b", "c")))),
                 "select b,c from a")
    expect_equal(xsql(list(list(a="tab", c(x = "cm1", y = "cm2")))),
                 "select a.cm1 as x,a.cm2 as y from tab a")

    aa <- xsql(list(list(a = "pria", c(a = "c1", b = "c2")),
                    list(b = "prib", c(a2 = "c1", b2 = "c2"))),
               whr="a.c1=b.c1", joi="le")
    bb <- paste("select a.c1 as a,a.c2 as b,b.c1 as a2,b.c2 as b2",
                "from pria a,prib b left join on a.c1=b.c1")
    expect_equal(aa, bb)
})
