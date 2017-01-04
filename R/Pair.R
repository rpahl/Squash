#' A pair container similar to C++ std::pair
#'
#' @field first (ANY), the first value of the pair
#' @field second (ANY), the second value of the pair
#'
#' @author Roman Pahl
#' @aliases Pair
#'
#' @examples
#' p1 <- Pair("foo", 1)
#' p2 <- Pair(2, "bar")
#' p1$first # "foo"
#' p1$swap(p2)
#' p1$first # 2
#'
#' @importFrom methods setRefClass
#' @note Date of last modification: 09.09.2015
#' @note Change history: none
Pair <- methods::setRefClass("Pair"
    , fields = list(first = "ANY", second = "ANY")
    , methods = list(
        initialize = function(first = NA, second = NA) {
            # Enables positional args for object construction
            first <<- first
            second <<- second
        }
        , copy = function() {
            Pair(first = .self$first, second = .self$second)
        }
        , swap = function(pr) {
            "Exchanges the contents of two pairs"
            if (!inherits(pr, "Pair")) {
                stop("in Pair: argument must be a Pair")
            }
            tmp <- first
            first <<- pr$first
            pr$first <- tmp
            tmp <- second
            second <<- pr$second
            pr$second <- tmp
            invisible()
        }
    )
)

#' A typed pair container similar to C++ std::pair extending the Pair class
#'
#' @field T1 (character) string specifying the type of the first element
#' @field T2 (character) string specifying the type of the second element
#'
#' @author Roman Pahl
#' @aliases TypedPair
#'
#' @examples
#' x0 <- TypedPair(T1 = "character", first = "x", T2 = "numeric", second = 0)
#' Analog: Short (implicit) construction
#' x0 <- TypedPair("character", "x", "numeric", 0)
#' x0$first # "x"
#' x0$second # "0"
#' \dontrun{
#' x0$setFirst(1) # Throws "bad type - expected character but got numeric"
#' }
#' x0$setFirst("z") # ok
#' x0$first # "z"
#'
#' y1 <- TypedPair("character", "y", "numeric", 1)
#' x0$swap(y1)
#' x0$first # "y"
#' x0$second # "1"
#'
#' @importFrom methods setRefClass
#' @note Date of last modification: 09.09.2015
#' @note Change history: none
TypedPair <- methods::setRefClass("TypedPair" , contains = "Pair"
    , fields = list(T1 = "character", T2 = "character")
    , methods = list(
        initialize = function(T1, first = NA, T2, second = NA, ...) {
            T1 <<- T1
            T2 <<- T2
            callSuper(first = first, second = second, ...)

            # Ensure correct value types by using the own set functions
            .self$setFirst(first)
            .self$setSecond(second)
        }
        , copy = function() {
            TypedPair(T1 = .self$T1, first  = .self$first, 
                      T2 = .self$T2, second = .self$second)
        }
        , setFirst = function(x) {
            if (!inherits(x, what = T1)) {
                stop(paste("in TypedPair: bad type - expected", T1, "but got",
                           data.class(x)))
            }
            first <<- x
        }
        , setSecond = function(x) {
            if (!inherits(x, what = T2)) {
                stop(paste("in TypedPair: bad type - expected", T2, "but got",
                           data.class(x)))
            }
            second <<- x
        }
    )
)
TypedPair$lock("T1", "T2")

