#!/usr/bin/env Rscript
source('enigma.R')

test.Enigma <- function(enigma, words, print=FALSE) {
  setStatus(enigma)
  enc <- encode.Enigma(enigma, words)
  setStatus(enigma)
  dec <- encode.Enigma(enigma, enc)
  if (print) {
    cat(sprintf('original: %s\n', words))
    cat(sprintf(' encrypt: %s\n', enc))
    cat(sprintf(' decrypt: %s\n', dec))
  }
  stopifnot(words == dec)
}

N <- 6

a <- Rotor(N)
stopifnot(is.Rotor(a))
#print(a)
#print(getMatrix(a, 1))
#print(getMatrix(a, 3))

b <- Reflector(N)
stopifnot(is.Reflector(b))
stopifnot(is.Rotor(b))
#print(b)
#print(getMatrix(b))

dict <- LETTERS[1:N]
e <- Enigma(dict, list(a), b)
stopifnot(is.Enigma(e))
setStatus(e, 3)
stopifnot(3L == getStatus(e))
rotateOne(e)
stopifnot(4L == getStatus(e))
setStatus(e)
stopifnot(1L == getStatus(e))

original.text <- paste(dict[sample(1:length(dict), 10, TRUE)], collapse='')
test.Enigma(e, original.text)

dict2 <- c(LETTERS, tolower(LETTERS), ' ', ',', '.', '!')
rotors <- replicate(3, Rotor(length(dict2)))
reflector <- Reflector(length(dict2))
enigma <- Enigma(dict2, rotors, reflector)
test.Enigma(enigma, paste(sample(dict2, 10, TRUE), collapse=''), print=TRUE)

test.Enigma(enigma, 'This is the original text.', print=TRUE)
