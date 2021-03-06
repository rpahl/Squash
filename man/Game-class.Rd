% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Player.R
\docType{class}
\name{Game-class}
\alias{Game}
\alias{Game-class}
\title{A Squash Game class}
\description{
Models a Squash game where points a played by sampling from a
 binomial distribution weighted by the skill of both contenders. Furthermore,
 a graphical visualization of the game result is provided (see \code{graph}).
}
\section{Fields}{

\describe{
\item{\code{players}}{(list) contains both players/opponents of the game}

\item{\code{game_length}}{(numeric) length of the game}

\item{\code{winByTwo}}{(logical) if TRUE, players must win by two clear points}

\item{\code{odds}}{(numeric) a vector of length two representing the winning odds of 
player1 vs player2}

\item{\code{score_lines}}{(list) played points are kept internally in two score
lines separately for each player}
}}
\examples{
set.seed(123)
g1 <- Game(Player("A"), Player("B"))
g1$get_winner()                      # NULL
g1$play()
g1$get_winner()$name                 # "A"
g1$get_score()                       # 11 9
\dontrun{
plot(g1$graph())                      
}

}
\author{
Roman Pahl, \email{roman.pahl@gmail.com}
}

