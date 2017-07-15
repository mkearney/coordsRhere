#' gcoords class
#'
#' @name gcoords
#' @importFrom methods new
gcoords <- setClass(
  "gcoords",
  slots = c(
    point = "matrix",
    box = "matrix")
)

as_gcoords <- function(point, box) {
  ## point
  if (!all(is.matrix(point), is.numeric(point))) {
    if (is.atomic(point)) {
      point <- strsplit(point, "\\s", perl = TRUE)
    }
    if (is.list(point)) {
      stopifnot(all(lengths(point) < 3L))
      if (!all(lengths(point) == 2L)) {
        point[is.na(point)] <- rep(list(rep(NA, 2)), sum(is.na(point)))
      }
      point <- matrix(
        unlist(point), length(point), 2L, byrow = TRUE)
    }
    if (is.data.frame(point)) {
      point <- as.matrix(point)
    }
    point <- apply(point, 2, as.double)
  }

  ## box
  if (!all(is.matrix(box), is.numeric(box))) {
    if (is.atomic(box)) {
      box <- strsplit(box, "\\s", perl = TRUE)
    }
    if (is.list(box)) {
      stopifnot(lengths(box) < 9L)
      if (!all(lengths(box) == 8L)) {
        box[is.na(box)] <- rep(list(rep(NA, 8)), sum(is.na(box)))
      }
      box <- matrix(
        unlist(box), length(box), 8L, byrow = TRUE
      )
    }
    if (is.data.frame(box)) {
      box <- as.matrix(box)
    }
    box <- apply(box, 2, as.double)
  }
  gcoords(point = point, box = box)
}


setMethod("show", "gcoords", function(object) print(object))

#' print gcoords
#'
#' @param x Object of class coords.
#' @param n Number of obs to return
#' @param digits Number of digits to print.
print.gcoords <- function(x, n = 10, digits = 2) {
  message("An object of class \"gcoords\"")
  message("\nBounding box:")
  b <- apply(
    x@box[1:n, ], 2,
    function(.) as.double(round(., digits = digits))
  )
  print(b)
  message("\nPoint:")
  p <- apply(
    x@point[1:n, ], 2,
    function(.) as.double(round(., digits = digits))
  )
  print(p)
}

#' @importFrom methods slot
setMethod("$", "gcoords", function(x, name) slot(x, name))

