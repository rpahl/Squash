#' @title Shiny server function 
#'
#' @description The server-side logic of the Shiny application is 
#' defined/started via the function \code{\link[shiny]{shinyServer}}, which 
#' itself has a function as its argument, which, in turn, is defined here.
#'
#' @details Note that this function does not return anything, but rather the
#' work is done in the function body, using and modifying the \code{input},
#' \code{output}, and \code{session} parameters as well as calling other shiny
#' functions such as \code{\link[shiny]{observe}} and
#' \code{\link[shiny]{observeEvent}}, in order to control elements of the GUI.
#'
#' @param input shiny input parameters
#' @param output shiny output parameters
#' @param session shiny session parameter
#'
#' @author Roman Pahl, \email{roman.pahl@gmail.com}
#' @seealso For more details see \code{\link[shiny]{shinyServer}}.
server_function <- function(input, output, session) {
    isDebugging <- getOption("squashsim.debug", default = FALSE)
    if (isDebugging) {
        devtools::load_all()
    }

    shiny::observe({
        update_input_widgets(session, input)
    })
    shiny::observeEvent(input$player1, {
                        if (nchar(input$player1) == 0) {
                            shinyjs::js$backgroundCol("player1", "black")
                        } else {
                            shinyjs::js$backgroundCol("player1", "white")
                        }
    })
    shiny::observeEvent(input$player2, {
                        if (nchar(input$player2) == 0) {
                            shinyjs::js$backgroundCol("player2", "red")
                        } else {
                            shinyjs::js$backgroundCol("player2", "white")
                        }
    })

    shiny::observeEvent(input$play, {
        p1 <- Player(input$player1, skill = input$skill)
        p2 <- Player(input$player2, skill = 1 - input$skill)
        m <- Match(game = Game(p1, p2), bestOf = as.numeric(input$bestOf))

        random.seed <- if (nchar(input$seed) > 0) as.numeric(input$seed) else NULL
        m$play(momentum = input$momentum, seed = random.seed)
        output[["scoreLabel"]] <- renderText("Final score:")
        output[["score"]] <- renderTable({
            m$get_match_table()
        }, digits = 0)

        games <- m$played_games
        len <- length(games)
        output[["graph1"]] <- renderPlot(plot(games[[1]]$graph("Game 1")))
        output[["graph2"]] <- renderPlot(plot(games[[2]]$graph("Game 2")))

        df0 <- data.frame(Point = numeric(0), Score = numeric(0))
        plot0 <- ggplot2::ggplot(data = df0, aes(x = Point, y = Score)) +
            geom_blank() + theme_bw()
        if(len >= 3) {
            output[["graph3"]] <- renderPlot(plot(games[[3]]$graph("Game 3")))
        }
        else {
            output[["graph3"]] <- renderPlot(plot(plot0))
        }
        if(len >= 4) {
            output[["graph4"]] <- renderPlot(plot(games[[4]]$graph("Game 4")))
        }
        else {
            output[["graph4"]] <- renderPlot(plot(plot0))
        }
        if(len >= 5) {
            output[["graph5"]] <- renderPlot(plot(games[[5]]$graph("Game 5")))
        }
        else {
            output[["graph5"]] <- renderPlot(plot(plot0))
        }
    })
    invisible()
}

