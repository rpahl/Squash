#' @title Squash match simulator
#'
#' @description 
#' A package to simulate and visualize Squash matches. At this point, a very
#' simple probability model is used for this purpose. In particular, a Squash
#' match consists of two players, for which the winning odds of one player over
#' the other are specified. Then, a match is simulated point by point, and 
#' for each point, the winner is decided by sampling from a binomial 
#' distribution with probability weights specified by the odds. In addition, it
#' is possible to set a so-called momentum shift, which increases the odds in 
#' favor of the player that just has won a game.
#'
#' All functionality is provided in a GUI implemented as a shiny application
#' where parameters are easily set. The resulting match score in particular is 
#' provided both as a score table and graphs showing the process of each game.
#'
#' @author Roman Pahl, \email{roman.pahl@gmail.com}
#' @usage \code{start()}
#' @docType package
#' @name Squash
NULL

#' @title Starts the Squash match simulator
#' @description 
#' Starts the application in a web browser, which should open automatically.
#'
#' @param debugMode (logical) if TRUE, the server function will call
#' \code{\link[devtools]{load_all()}} each time the browser is refreshed, which
#' allows to test code-changes without having to abort and restart the function.
#'
#' @author Roman Pahl, \email{roman.pahl@gmail.com}
#' @examples
#' 
#' \dontrun{
#' start()
#' }
#' @export
start <- function(debugMode = FALSE) {
    if (debugMode) {
        old <- getOption("squashsim.debug")
        on.exit(options(squashsim.debug = old))
        options(squashsim.debug = TRUE)
    }
    shiny::runApp(appDir = system.file('gui', package = 'Squash'))
    invisible()
}

