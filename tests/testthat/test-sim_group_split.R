test_that("sim_group_split works", {
  # all ids end up in two groups (ini group (0) and their destination after the split)
  test_vec1 <- logical(20)
  test_vec2 <- logical(length(test_vec1))
  for (i in seq_along(test_vec1)) {
    nind <- sample(5:20, 1)
    nint <- ceiling((nind * (nind-1) / 2) * runif(1, 1.5, 5))
    x <- sim_group_split(n_ind = nind, n_int = nint)

    xx <- rbind(x$g1data, x$g2data)
    test_vec1[i] <- all(rowSums(table(xx$winner, xx$group) > 0) < 3)
    test_vec2[i] <- all(rowSums(table(xx$loser, xx$group) > 0) < 3)
  }
  expect_true(all(test_vec1))
  expect_true(all(test_vec2))
})
