cmyk <- function(C, M, Y, K) {

  C <- C / 100.0
  M <- M / 100.0
  Y <- Y / 100.0
  K <- K / 100.0

  n.c <- (C * (1-K) + K)
  n.m <- (M * (1-K) + K)
  n.y <- (Y * (1-K) + K)

  r.col <- ceiling(255 * (1-n.c))
  g.col <- ceiling(255 * (1-n.m))
  b.col <- ceiling(255 * (1-n.y))

  return(sprintf("#%02s%02s%02s", as.hexmode(r.col),
                 as.hexmode(g.col), as.hexmode(b.col)))

}

b2i <- function(two_bytes) {
  as.numeric(unpack("v", two_bytes[2:1]))
}

b2li <- function(four_bytes) {
  as.numeric(unpack("V", four_bytes[c(4,3,2,1)]))
}

b2f <- function(four_bytes) {
  floatraw2numeric(four_bytes[c(4,3,2,1)])
  # unpack("f", four_bytes[c(4,3,2,1)])
}

is_url <-function(x) { grepl("www.|http:|https:", x) }

#' @useDynLib adobecolor r_floatraw2numeric
#' @export
floatraw2numeric <- function(x) {
  stopifnot(is.raw(x))
  stopifnot(length(x) >= 4)
  .Call(r_floatraw2numeric, x)
}