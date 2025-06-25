x <- generate_interactions(10, n_int = 1000, steep = 0.9, shuffle_dates = TRUE, use_letters = TRUE)
x <- x[sort(sample(nrow(x), 15)), ]
rownames(x) <- NULL
drange <- range(x$date)

test_that("multiplication works", {
  expect_message(r <- find_extract_index(x$date, min(drange) - 1))
  expect_true(length(r <- find_extract_index(x$date, edates = NULL)) == 2)
  expect_error(r <- find_extract_index(rev(x$date), drange))

  r <- find_extract_index(x$date, c(drange, drange))
  expect_true(length(unique(r)) == length(unique(drange)))
})
