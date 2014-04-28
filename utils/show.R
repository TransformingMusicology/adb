#! /usr/bin/Rscript --vanilla

library(lattice)

read.feature <- function(path) {
  info <- file.info(path)
  con <- file(path, "rb")
  dim <- readBin(con, "integer", n = 1, size = 4)
  ndoubles <- (info$size-4)/8
  nvectors <- ndoubles/dim
  vectors <- readBin(con, "double", n=ndoubles)
  close(con)
  list(dim=dim, vectors=t(matrix(vectors, nrow=dim, ncol=nvectors)))
}

show <- function(feature) {
  x11()
  dp <- feature$vectors %*% t(feature$vectors)
  d <- matrix(rep(diag(dp), length(diag(dp))), nrow=length(diag(dp)), ncol=length(diag(dp)))
  print(levelplot(d + t(d) - 2*dp, aspect="iso"))
  writeLines("press <return> when finished")
  invisible(readLines(file("stdin"), n=1))
}

content <- commandArgs(TRUE)[[1]]
f <- read.feature(content)
show(f)
