# Dot product (cross product) of two sparse_numeric vectors

Computes the inner product \\\sum_i x_i y_i\\.

## Usage

``` r
sparse_crossprod(x, y, ...)

# S4 method for class 'sparse_numeric,sparse_numeric'
sparse_crossprod(x, y, ...)
```

## Arguments

- x, y:

  `sparse_numeric` objects.

- ...:

  Ignored; included for method compatibility.

## Value

A numeric scalar equal to the dot product.
