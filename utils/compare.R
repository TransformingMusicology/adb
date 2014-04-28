#! /usr/bin/Rscript --vanilla

library(lattice)
library(RColorBrewer)

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

compare <- function(feature1, feature2, output) {
  png(filename=output, width=1366, height=768)
  dp <- feature1$vectors %*% t(feature2$vectors)
  l2norm1 <- matrix(rep(rowSums(feature1$vectors^2), nrow(feature2$vectors)), nrow=nrow(feature1$vectors))
  l2norm2 <- matrix(rep(rowSums(feature2$vectors^2), nrow(feature1$vectors)), nrow=nrow(feature2$vectors))
  print(levelplot(l2norm1 + t(l2norm2) - 2*dp, aspect="iso", at=seq(0,1,length.out=11)^2*2.055-0.005, col.regions=c(brewer.pal(9, "Blues"), "#000000")))
}

content1 <- commandArgs(TRUE)[[1]]
f1 <- read.feature(content1)
content2 <- commandArgs(TRUE)[[2]]
f2 <- read.feature(content2)
compare(f1, f2, commandArgs(TRUE)[[3]])
