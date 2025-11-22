# Sparse numeric vector S4 class

Represents a sparse numeric vector by storing only the non-zero values,
their positions, and the full length of the underlying dense vector.

## Slots

- `value`:

  Numeric vector of non-zero values.

- `pos`:

  Integer vector of positions of non-zero values (1-based).

- `length`:

  Single integer giving the full length of the vector.
