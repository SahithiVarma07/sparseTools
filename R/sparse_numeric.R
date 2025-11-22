#' Core sparse_numeric infrastructure
#'
#' Defines the \code{sparse_numeric} S4 class and the core arithmetic
#' and utility methods that operate on it.
#'
#' @keywords internal
#' @import methods
#' @importFrom methods as new show
NULL


#' Sparse numeric vector S4 class
#'
#' Represents a sparse numeric vector by storing only the non-zero values,
#' their positions, and the full length of the underlying dense vector.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of positions of non-zero values (1-based).
#' @slot length Single integer giving the full length of the vector.
#'
#' @name sparse_numeric-class
#' @rdname sparse_numeric-class
#'
#' @export
setClass(
  "sparse_numeric",
  slots = c(
    value  = "numeric",
    pos    = "integer",
    length = "integer"
  )
)

#' @rdname sparse_numeric-class
setValidity("sparse_numeric", function(object) {
  if (length(object@value) != length(object@pos))
    return("value and pos lengths must match")
  if (length(object@length) != 1L || object@length <= 0)
    return("length must be a single positive integer")
  if (any(object@pos <= 0) || any(object@pos > object@length))
    return("pos contains invalid indices")
  TRUE
})

#' Coerce numeric to sparse_numeric
#'
#' @param from A numeric vector.
#'
#' @return A \code{sparse_numeric} object storing only the non-zero entries.
#' @export
setAs("numeric", "sparse_numeric", function(from) {
  nz <- which(from != 0)
  new("sparse_numeric",
      value  = from[nz],
      pos    = as.integer(nz),
      length = as.integer(length(from)))
})

#' Coerce sparse_numeric to dense numeric
#'
#' @param from A \code{sparse_numeric} object.
#'
#' @return A regular numeric vector of length \code{from@length}.
#' @export
setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  out[from@pos] <- from@value
  out
})

#' Coerce sparse_numeric to numeric via as.numeric()
#'
#' @param x A \code{sparse_numeric} object.
#'
#' @return A numeric vector of length \code{x@length}.
#' @export
setMethod("as.numeric", "sparse_numeric", function(x) {
  out <- numeric(x@length)
  out[x@pos] <- x@value
  out
})


combine_sparse <- function(x, y, op) {
  if (x@length != y@length)
    stop("Lengths must match")

  all_pos <- sort(unique(c(x@pos, y@pos)))
  vals <- numeric(length(all_pos))

  for (i in seq_along(all_pos)) {
    p <- all_pos[i]
    vx <- if (p %in% x@pos) x@value[x@pos == p] else 0
    vy <- if (p %in% y@pos) y@value[y@pos == p] else 0
    vals[i] <- op(vx, vy)
  }

  nz <- which(vals != 0)
  new("sparse_numeric",
      value  = vals[nz],
      pos    = as.integer(all_pos[nz]),
      length = x@length)
}


#' Add two sparse_numeric vectors
#'
#' Elementwise addition of two sparse vectors of the same length.
#'
#' @param x,y \code{sparse_numeric} objects.
#' @param ... Ignored; included for method compatibility.
#'
#' @return A \code{sparse_numeric} representing \code{x + y}.
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' @rdname sparse_add
#' @export
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) combine_sparse(x, y, `+`))

#' Subtract two sparse_numeric vectors
#'
#' Elementwise subtraction of two sparse vectors of the same length.
#'
#' @inheritParams sparse_add
#'
#' @return A \code{sparse_numeric} representing \code{x - y}.
#' @export
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' @rdname sparse_sub
#' @export
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) combine_sparse(x, y, `-`))

#' Elementwise product of two sparse_numeric vectors
#'
#' Multiplies corresponding elements of two sparse vectors.
#'
#' @inheritParams sparse_add
#'
#' @return A \code{sparse_numeric} representing the elementwise product.
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' @rdname sparse_mult
#' @export
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) combine_sparse(x, y, `*`))

#' Dot product (cross product) of two sparse_numeric vectors
#'
#' Computes the inner product \eqn{\sum_i x_i y_i}.
#'
#' @inheritParams sparse_add
#'
#' @return A numeric scalar equal to the dot product.
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

#' @rdname sparse_crossprod
#' @export
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("Lengths must match")
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0L) return(0)
            sum(x@value[match(common, x@pos)] * y@value[match(common, y@pos)])
          })



#' Arithmetic operators for sparse_numeric
#'
#' Elementwise addition, subtraction, and multiplication using the
#' standard \code{+}, \code{-}, and \code{*} operators.
#'
#' @param e1,e2 \code{sparse_numeric} objects.
#'
#' @return A \code{sparse_numeric} object.
#' @name sparse_arith_ops
#' @rdname sparse_arith_ops
NULL

#' @rdname sparse_arith_ops
#' @export
setMethod("+", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' @rdname sparse_arith_ops
#' @export
setMethod("-", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' @rdname sparse_arith_ops
#' @export
setMethod("*", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))




#' Mean of a sparse_numeric vector
#'
#' Computes the mean of the full dense vector, treating all missing
#' positions as zeros.
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Ignored.
#'
#' @return Numeric scalar mean.
#' @export
setMethod("mean", "sparse_numeric", function(x, ...) {
  sum(x@value) / x@length
})

#' Euclidean norm of a sparse_numeric vector
#'
#' Computes the Euclidean (L2) norm
#' \deqn{\sqrt{\sum_i x_i^2}}.
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Ignored; included for compatibility with the generic.
#'
#' @return Numeric scalar norm.
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' @rdname norm
#' @export
setMethod("norm", "sparse_numeric", function(x, ...) {
  sqrt(sum(x@value^2))
})

#' Standardize a sparse_numeric vector
#'
#'
#' @inheritParams norm
#'
#' @return A \code{sparse_numeric} object containing standardized values.
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#' @rdname standardize
#' @export
setMethod("standardize", "sparse_numeric", function(x, ...) {
  dense  <- as.numeric(x)
  scaled <- as.numeric(scale(dense))
  as(scaled, "sparse_numeric")
})



#' Show method for sparse_numeric
#'
#' Pretty-prints a \code{sparse_numeric} object by listing its non-zero
#' positions and values.
#'
#' @param object A \code{sparse_numeric} object.
#' @export
setMethod("show", "sparse_numeric", function(object) {
  cat("sparse_numeric vector of length", object@length, "\n")
  if (length(object@pos) == 0L) {
    cat("  (all zeros)\n")
  } else {
    df <- data.frame(pos = object@pos, value = object@value)
    print(df, row.names = FALSE)
  }
})

#' Length of a sparse_numeric vector
#'
#' Returns the full length of the sparse_numeric vector.
#'
#' @param x A sparse_numeric object
#'
#' @return Integer length of the underlying dense vector
#' @export
setMethod("length", "sparse_numeric", function(x) {
  x@length
})
