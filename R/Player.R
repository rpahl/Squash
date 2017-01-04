#' @title A Squash Player class
#' @description Wraps name and skill of a Squash player 
#' @field name (character) the player's name
#' @field name (numeric) the player's skill (number between 0 and 1)
#' 
#' @author Roman Pahl, \email{roman.pahl@gmail.com}
#' @examples
#' p1 <- Player("John", skill = 0.7)
#' p1$isPlayer()    # TRUE
#' print(p1)
#'
#' @importFrom methods setRefClass
#' @export
Player <- methods::setRefClass("Player",
    fields = list(name = "character", skill = "numeric"),
    methods = list(
        initialize = function(name = "Ashour", skill = 1) {
            if(length(name) > 1) stop("name must be a single character string")
            if(skill < 0 || skill > 1) stop("skill must be in [0, 1]")
            name <<- name
            skill <<- skill
            invisible(.self)
        },
        isPlayer = function() TRUE
    )
)


#' @title A Squash Game class
#' @description Models a Squash game where points a played by sampling from a
#'  binomial distribution weighted by the skill of both contenders. Furthermore,
#'  a graphical visualization of the game result is provided (see \code{graph}).
#' @field players (list) contains both players/opponents of the game
#' @field game_length (numeric) length of the game
#' @field winByTwo (logical) if TRUE, players must win by two clear points
#' @field odds (numeric) a vector of length two representing the winning odds of 
#'  player1 vs player2
#' @field score_lines (list) played points are kept internally in two score
#'  lines separately for each player
#' 
#' @author Roman Pahl, \email{roman.pahl@gmail.com}
#' @examples
#' set.seed(123)
#' g1 <- Game(Player("A"), Player("B"))
#' g1$get_winner()                      # NULL
#' g1$play()
#' g1$get_winner()$name                 # "A"
#' g1$get_score()                       # 11 9
#' \dontrun{
#' plot(g1$graph())                      
#' }
#'
#' @import ggplot2
#' @importFrom methods setRefClass
#' @export
Game <- methods::setRefClass("Game",
    fields = list(players = "list", game_length = "numeric", 
                  winByTwo = "logical", odds = "numeric", score_lines = "list"),
    methods = list(
        initialize = function(p1 = Player("A"), p2 = Player("B"), 
                              game_length = 11, winByTwo = TRUE) {
            if (p1$name == p2$name) stop("Players must have different names")
            stopifnot(isTRUE(p1$isPlayer()), isTRUE(p2$isPlayer()))
            players <<- list(p1, p2)
            odds <<- c(p1$skill, p2$skill)
            game_length <<- game_length
            winByTwo <<- winByTwo
            score_lines <<- list(0, 0)
            invisible(.self)
        },
        get_score = function() unlist(lapply(score_lines, FUN = tail, 1)),
        get_detailed_score = function() {
            len <- length(score_lines[[1]])
            points <- c(1:len, 1:len)
            pp <- as.factor(c(rep(.self$players[[1]]$name, times = len),
                              rep(.self$players[[2]]$name, times = len)))
            scores <- Reduce(x = .self$score_lines, f = c)
            data.frame(Point = points, Score = scores, Player = pp)
        },
        get_winner = function() {
            if (.self$hasFinished()) {
                .self$players[[which.max(.self$get_score())]]
            } else {
                NULL
            }
        },
        hasFinished = function() {
            score <- .self$get_score()
            max(score) >= game_length && (!winByTwo || abs(diff(score)) >= 2)
        },
        play = function() {
            odds <<- c(.self$players[[1]]$skill, .self$players[[2]]$skill)
            while (!.self$hasFinished()) .self$.play_point()
            invisible(.self)
        },
        reset = function() {
            score_lines <<- list(0, 0)
            invisible(.self)
        },
        graph = function(title = "") {
            ylims <- c(0, max(.self$get_score()) + 0.5)
            ggplot(data = .self$get_detailed_score(), 
                   mapping = ggplot2::aes(x = Point, y = Score, 
                                          group = Player)) +
                   scale_colour_manual(values = c("black", "red")) +
                   geom_line(ggplot2::aes(colour = Player)) +
                   geom_point(ggplot2::aes(colour = Player)) +
                   geom_hline(ggplot2::aes(yintercept = .self$game_length)) +
                   ylim(ylims) + ggplot2::ggtitle(title) + theme_bw()
        },
        
        # Private
        .play_point = function() {
            winner <- sample(1:2, size = 1, prob = .self$odds)
            looser <- if (winner == 1) 2 else 1

            last <- tail(score_lines[[winner]], n = 1)
            score_lines[[winner]] <<- c(score_lines[[winner]], last + 1)
            
            last <- tail(score_lines[[looser]], n = 1)
            score_lines[[looser]] <<- c(score_lines[[looser]], last)
            invisible(.self)
        }
    )
)


#' @title A Squash Match class
#' @description Models a Squash match consisting of several \code{\link{Game}}s.
#'  For match play, a "game momentum" can be specified, which increases the odds
#'  in favor of the player that just has won a game.
#'
#' @field game (Game) contains all necessary game parameters
#' @field bestOf (numeric) the maximum match length
#' @field played_games (list) all played games are saved internally in this list
#' 
#' @author Roman Pahl, \email{roman.pahl@gmail.com}
#' @seealso \code{\link{Game}},
#' @examples
#' m1 <- Match(Game())
#' m1$hasFinished()             # FALSE
#' m1$get_match_table()         #   1 2 3
#'                              # A 0 0 0
#'                              # B 0 0 0
#' set.seed(123)
#' m1$play()
#' m1$hasFinished()             # TRUE
#' m1$get_match_table()         #    1  2  3  4  5
#'                              # A 11 11  2  6  9
#'                              # B  9  2 11 11 11
#'
#' m1$get_winner()$name         # "B"
#'
#' @importFrom methods setRefClass
#' @export
Match <- methods::setRefClass("Match",
    fields = list(game = "Game", bestOf = "numeric", played_games = "list"),
    methods = list(
        initialize = function(game, bestOf = 5) {
            bestOf <<- bestOf
            game <<- game
            invisible(.self)
        },
        get_match_table = function() {
            if (.self$hasFinished()) {
                scores <- lapply(.self$played_games, 
                                 FUN = function(game) game$get_score())
                tab <- Reduce(x = scores, f = cbind)
            } else {
                tab <- matrix(0, nrow = 2, ncol = (.self$bestOf + 1) / 2)
            }
            rownames(tab) <- unlist(lapply(.self$game$players, 
                                           FUN = function(p) p$name))
            colnames(tab) <- as.character(1:ncol(tab))
            tab
        },
        get_winner = function() {
            winners <- lapply(.self$played_games, 
                              FUN = function(game) game$get_winner()$name)
            game_count <- table(unlist(winners))
            .self$game$players[[which.max(game_count)]]
        },
        hasFinished = function() {
            if (length(.self$played_games) == 0) return(FALSE)
            winners <- lapply(.self$played_games, 
                              FUN = function(game) game$get_winner()$name)
            game_count <- table(unlist(winners))
            max(game_count) > .self$bestOf / 2
        },
        play = function(momentum = 1, seed = NULL) {
            if (.self$hasFinished()) {
                warning("Match was already played - will be returned unchanged")
                return(invisible(.self))
            }
            if (!is.null(seed)) {
                set.seed(seed)
            }
            .self$game$reset()
            .self$game$play()
            played_games <<- c(.self$played_games, list(.self$game$copy()))
            winner <- which.max(game$get_score())
            s <- .self$game$players[[winner]]$skill
            game$players[[winner]]$skill <<- s * momentum
            if (!.self$hasFinished()) .self$play(momentum) 
            invisible(.self)
        },
        reset = function() {
            .self$game$reset()
            .self$played_games <- list()
            invisble(.self)
        }
    )
)

