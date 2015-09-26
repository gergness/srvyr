context("match_dot_args is behaving")

f1 <- function(...) {
  args <- srvyr:::match_dot_args(lazyeval::lazy_dots(...), c("x", "y", "z"))

  paste(lazyeval::lazy_eval(args$x),
        lazyeval::lazy_eval(args$y),
        lazyeval::lazy_eval(args$z), sep = "-")
}

test_that("All unnamed arguments",
          expect_equal(f1(1, 2, 3), "1-2-3")
)

test_that("Named out-of-order arguments",
          expect_equal(f1(x = 1, z = 3, y = 2), "1-2-3")
)

test_that("Mixed out-of-order arguments",
          expect_equal(f1(1, z = 3, 2), "1-2-3")
)
