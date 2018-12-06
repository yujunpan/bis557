#' Sparse matrix class
#'
#' @description creating sparse matrix class with addition, multiplication and transpose operations
#' @param i row index of non-zero elements 
#' @param j cololumn index of a non-zero elements
#' @param x numeric value of the element corresponding position i, j
#' @param dims dimensions of the sparse matrix
#' @return A sparse.matrix object
#' @export

sparse.matrix <- function(i, j, x, dims = c(max(i), max(j))) {
  s <- data.frame(i, j, x)
  res <- list(sparse_matrix = s, 
              dim = dims)
  class(res) <- "sparse.matrix"
  return(res)
}


# addition

`+.sparse.matrix` <-function(a, b) {
  if (!identical(a$dim, b$dim)){
    stop("Different matrix dimension")
  }
  c <-merge(a$sparse_matrix, b$sparse_matrix, by =c("i", "j"), all = TRUE, suffixes =c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1+c$x2
  c <- c[,c("i", "j", "x")]
  c <- c[order(c$j),]
  sparse.matrix(c$i, c$j, c$x, dims = a$dim)
}

# multiplication

`%*%.default` = .Primitive("%*%")  # keep defalut
`%*%` = function(x,...){ 
  UseMethod("%*%",x)
}
`%*%` <- function(x, y) {
  UseMethod("%*%", x)
}

`%*%.sparse.matrix` <- function(a, b) {
  
  if (!inherits(a, "sparse.matrix"))
    stop ("a is not a sparse.matrix class!")
  
  if (!inherits(b, "sparse.matrix"))
    stop ("b is not a sparse.matrix class!")
  
  if (a$dim[1] != b$dim[2])
    stop ("incorrect dimensions for multiplication!")
  
  # below is the function from textbook
  colnames(b$sparse_matrix) <- c("i2", "j2", "x2")
  c <- merge(a$sparse_matrix, b$sparse_matrix, by.x = "j", by.y = "i2", all = FALSE, 
             suffixes = c("1", "2"))
  c$x <- c$x * c$x2
  c$key <- paste(c$i, c$j2, sep = "-")
  x <- tapply(c$x, c$key, sum)
  key <- strsplit(names(x), "-")
  d <- data.frame(i = sapply(key, getElement, 1),
                  j = sapply(key, getElement, 2),
                  x = as.numeric(x))
  
  # add numeric type is it won't past test file
  d$i <- as.numeric(d$i)
  d$j <- as.numeric(d$j)
  
  # have to change row order and names because it won't match test example
  res <- sparse.matrix(d$i, d$j, d$x, dims= c(a$dim[1], b$dim[2]))
  res$sparse_matrix <- res$sparse_matrix[order(res$sparse_matrix$j), ]
  row.names(res$sparse_matrix) <- NULL
  
  # return
  res
}

# transpose

t <- function (x, ...) {
  UseMethod("t", x)
}

`t.sparse.matrix` <- function(a){
  temp <- a$sparse_matrix$i
  a$sparse_matrix$i <- a$sparse_matrix$j
  a$sparse_matrix$j <- temp
  a$sparse_matrix <- rev(a$sparse_matrix)
  a$dim <- c(a$dim[2], a$dim[1])
  return(a)
}
