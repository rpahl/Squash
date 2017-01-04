context("Squashsim")

test_that("Player", {
    expect_error(Player("A", skill = 2), 
                 regexp = "skill must be in [0, 1]", fixed = TRUE)
    p1 <- Player("John", skill = 0.7)
    expect_equal(p1$name, expected = "John")
    expect_equal(p1$skill, expected = 0.7)
})


test_that("Game", {
    expect_error(Game(Player("A"), Player("A")),
                      regexp = "Players must have different names")
    g1 <- Game(Player("A"), Player("B"))
    expect_true(is.null(g1$get_winner()))
    g1$play()
    expect_false(is.null(g1$get_winner()))
    expect_is(g1$get_winner(), class = "Player")
    
    g2 <- g1$copy()
    expect_equal(g2$get_score(), expected = g1$get_score())
    g1$reset()
    expect_true(is.null(g1$get_winner()))
    expect_equal(g1$get_score(), expected = c(0,0))
})

test_that("Match", {
    m1 <- Match(Game())
    expect_false(m1$hasFinished())
    expect_true(all(m1$get_match_table() == 0))
    
    set.seed(123)
    m1$play()
    expect_true(m1$hasFinished())
    tab <- m1$get_match_table()
    expect_equal(nrow(m1$tab), expected = 2)
    expect_true(ncol(m1$tab) > m1$bestOf / 2)
    expect_true(ncol(m1$tab) <= m1$bestOf)
    
    expect_is(m1$get_winner(), class = "Player")
    expect_warning(m1$play(), regexp = "Match was already played")
})
