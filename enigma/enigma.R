library(Matrix)
library(igraph)

getMatrix <- function(x, ...) UseMethod('getMatrix', x)
setStatus <- function(x, ...) UseMethod('setStatus', x)
getStatus <- function(x) UseMethod('getStatus', x)
rotateOne <- function(x) UseMethod('rotateOne', x)


Rotor <- function(n) {
  j <- sample(1:n, n)
  mat <- sparseMatrix(i=1:n, j=j, x=1, dims=c(n, n))
  structure(list(mat=mat, size=n), class='Rotor')
}

is.Rotor <- function(x) {
  inherits(x, 'Rotor')
}

print.Rotor <- function(x) {
  left <- 1:x$size
  right <- x$mat %*% (1:x$size)
  res <- paste(left, ' -> ', right)
  cat(res, sep='\n')
  invisible(NULL)
}

length.Rotor <- function(x) {
  x$size
}

getMatrix.Rotor <- function(rotor, pos) {
# check 'pos' outside, 1 <= pos <= rotor$size
  n <- rotor$size
  up <- rotor$mat[pos:n, , drop=FALSE]
  lo <- rotor$mat[0:(pos - 1), , drop=FALSE]
  rBind(up, lo)
}


Reflector <- function(n) {
  if (n %% 2 != 0) {
    stop("reflector needs a even 'n'.")
  }
  n2 <- n %/% 2
  idx <- rep(1:n2, each=2)
  idx <- sample(idx, n)
  idx <- lapply(1:n2, function(x) which(idx == x))
  idx <- do.call(rbind, idx)
  mat <- sparseMatrix(i=idx[, 1], j=idx[, 2], x=1, dims=c(n, n), symmetric=TRUE)
  structure(list(mat=mat, size=n), class=c('Reflector', 'Rotor'))
}

is.Reflector <- function(x) {
  inherits(x, 'Reflector')
}

print.Reflector <- function(x) {
  NextMethod('print', x)
}

length.Reflector <- function(x) {
  x$size
}

getMatrix.Reflector <- function(reflector) {
  reflector$mat
}


Enigma <- function(dict, rotors, reflector) {
  if (!is.character(dict)) {
    stop("'dict' must be character.")
  }
  if (!is.list(rotors)) {
    stop("'rotors' must be a list.")
  }
  if (length(rotors) == 0) {
    stop("at least 1 rotor needed.")
  }
  if (any(!sapply(rotors, is.Rotor))) {
    stop("every element of 'rotors' must be Rotor.")
  }
  if (!is.Reflector(reflector)) {
    stop("'reflector' must be Reflector.")
  }
  dict.len <- length(dict)
  rotors.len <- sapply(rotors, length)
  reflector.len <- length(reflector)
  if (length(unique(c(dict.len, rotors.len, reflector.len))) != 1L) {
    stop("size mismatch.")
  }
  rotor.num <- length(rotors)
  status <- new.env()
  assign('status', rep(1L, rotor.num), envir=status)
  l <- list(dict=dict, rotors=rotors, reflector=reflector,
            dict.len=dict.len,
            rotors.len=rotors.len, rotor.num=rotor.num,
            status=status)
  structure(l, class='Enigma')
}

is.Enigma <- function(x) {
  inherits(x, 'Enigma')
}

setStatus.Enigma <- function(x, status=NULL) {
  if (missing(status)) {
    status <- rep(1L, x$rotor.num)
  }
  assign('status', status, x$status)
  invisible(NULL)
}

getStatus.Enigma <- function(x) {
  get('status', x$status)
}

rotateOne.Enigma <- function(x) {
  status <- getStatus(x)
  rotor.num <- x$rotor.num
  rotors.len <- x$rotors.len
  for (i in 1:rotor.num) {
    status[i] <- status[i] + 1
    if (status[i] > rotors.len[i]) {
      # need to increase next rotor
      status[i] <- 1
    } else {
      break
    }
  }
  setStatus(x, status)
  invisible(NULL)
}

getMatrix.Enigma <- function(x) {
  status <- getStatus(x)
  rotors.mat <- lapply(1:x$rotor.num, function(i) getMatrix(x$rotors[[i]], status[i]))
  reflector.mat <- getMatrix(x$reflector)
  mat <- Diagonal(x$dict.len)
  for (m in rotors.mat) {
    mat <- mat %*% m
  }
  mat <- mat %*% reflector.mat
  for (m in rev(rotors.mat)) {
    mat <- mat %*% t(m)
  }
  mat
}

encode.single.Enigma <- function(x, char) {
  dict <- x$dict
  char.idx <- match(char, dict)
  char.vec <- sparseVector(1L, char.idx, length(dict))
  trans.mat <- getMatrix(x)
  enc.vec <- char.vec %*% trans.mat
  rotateOne(x)
  dict[which(enc.vec == 1L)]
}

encode.Enigma <- function(x, words) {
  w <- strsplit(words, '')
  enc.w <- lapply(w, function(sw) {
                  paste(sapply(sw, function(char) encode.single.Enigma(x, char)), collapse='')
            })
  if (is.vector(words)) {
    enc.w <- enc.w[[1]]
  }
  enc.w
}

