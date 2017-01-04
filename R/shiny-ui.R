#' @title Shiny theme setup
#' @description
#' Sets a specified shiny theme, but does not fail, if the theme is not
#' installed. In the latter case, the default theme will be in effect.
#'
#' @param theme_name (character) specifying the theme name
#' @return If the theme is available (i.e., installed) the theme name is
#' returned as is, else NULL is returned.
#'
#' @author Roman Pahl, \email{roman.x.pahl@@gsk.com}
set_shiny_theme_if_installed <- function(theme_name = "united") {
    hasThemes <- "shinythemes" %in% installed.packages()[, "Package"]
    if (hasThemes) {
        return(shinythemes::shinytheme(theme_name))
    } else {
        return(NULL)
    }
}


#' @title Layout specification of the user interface (UI)
#'
#' @return A \code{shiny} user interface definition
#' @author Roman Pahl, \email{roman.pahl@gmail.com}
#' @seealso \code{\link[shiny]{shinyUI}}
#' @import shiny V8
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs extendShinyjs
build_user_interface <- function() {
    fluidPage(title = "Squash simulator",
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(script = system.file("gui", "color.js",
                                                package = "Squash")),
        titlePanel(paste0("Squash match simulator (v",
                          packageVersion("Squash"), ")")),

        sidebarLayout(
            sidebarPanel(
                #img(src = "images/gsk-logo.png", height=72, width=203), 
                #br(), br(), br(), 
                column(6, textInput("player1", label = "Player 1:", value = "A")),
                column(6, textInput("player2", label = "Player 2:", value = "B")),
                sliderInput("skill", label = "Skill allocation", min = 0, max = 1, 
                            value = 0.5, step = 0.05),
                selectInput("bestOf", label = h3("Modus"),
                            choices = list("Best of 5" = "5",
                                           "Best of 3" = "3")),
                #checkboxInput("momentum", label = "Consider momentum", value = TRUE),
                sliderInput("momentum", label = "Momentum shift after game won (1 = none)", 
                            min = 1, max = 2, value = 1, step = 0.1),
                textInput("seed", label = "Random seed (optional)", value = ""),
                actionButton("play", label = "Play"), br(), br(), br(),
                textOutput("scoreLabel"), br(),
                tableOutput("score"),
                width = 3
            ),

            mainPanel(
                column(4, plotOutput("graph1"), height = "200px"),
                column(4, plotOutput("graph2"), height = "200px"),
                column(4, plotOutput("graph3"), height = "200px"),
                br(),
                column(4, plotOutput("graph4"), height = "80%"),
                column(4, plotOutput("graph5"), height = "80%")
                #tabsetPanel(tabPanel("Graphs", htmlOutput("Match graphs")))
            )
        ),
        
        theme = set_shiny_theme_if_installed("united")
    )
}
