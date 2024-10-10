


test_that("each individual is present at least for one day", {
  testvec <- logical(20)
  for (i in 1:20) {
    n_ind <- sample(5:15, 1)
    n_grp <- sample(1:3, 1)
    if (n_grp > n_ind/2) next
    n_mig <- sample(0:10, 1)
    if (n_grp == 1) n_mig <- 0
    n_days=100
    n_abs <- sample(0:(n_ind + n_mig), 1)
    x <- presence_with_migrations(n_ind = n_ind, n_mig = n_mig, n_grp = n_grp, n_days = n_days, n_abs = n_abs)
    z <- x$out != 0
    testvec[i] <- all(colSums(z) > 0)
  }
  expect_true(all(testvec))
})


test_that("all groups contain at least one individual at all times", {
  testvec <- logical(20)
  for (i in 1:20) {
    n_ind <- sample(4:10, 1)
    n_grp <- sample(1:3, 1)
    if (n_grp > n_ind/2) next
    n_mig <- sample(0:10, 1)
    if (n_grp == 1) n_mig <- 0
    n_days=100
    n_abs <- sample(0:(n_ind + n_mig), 1)
    x <- presence_with_migrations(n_ind = n_ind, n_mig = n_mig, n_grp = n_grp, n_days = n_days, n_abs = n_abs)
    z <- x$out
    z[z == 0] <- NA
    # all groups are represented at least once
    test1 <- apply(z, 1, function(x)length(na.omit(unique(x)))) == n_grp
    test2 <- apply(z, 1, function(x)all(table(x) >= 1))
    testvec[i] <- all(test1) && all(test2)
  }
  expect_true(all(testvec))
})


test_that("error is thrown if there are too many absences", {
  for (i in 1:5) {
    n_ind <- sample(4:10, 1)
    n_mig <- sample(0:10, 1)
    n_grp <- sample(1:3, 1)
    n_days <- 50
    n_abs <- n_ind + n_mig + 1
    expect_error(presence_with_migrations(n_ind = n_ind, n_mig = n_mig, n_grp = n_grp, n_days = n_days, n_abs = n_abs))
  }
})
