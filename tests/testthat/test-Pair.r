context("Pair")

test_that("Pair initialize", {
    expect_error(Pair(1, 2, 3), regexp = "*unused argument (3)*")
    p1 <- Pair("foo", 1)
    expect_equal(p1$first,  expected = "foo")
    expect_equal(p1$second, expected = 1)
})

test_that("Pair swapping and copy", {
    p1 <- Pair(first = "foo", second = 1)
    p2 <- Pair(first = 2, "bar")
    p1$swap(p2)

    # After swap, p1 should have all values from p2
    expect_equal(p1$first, expected = 2)
    expect_equal(p1$second, expected = "bar")

    # Swapping with itself should not alter anything
    p1_copy <- p1$copy()
    p1$swap(p1)
    expect_equal(p1, expected = p1_copy)

    # Finally show that we truly work on a copy (and not a reference to p1)
    p1_copy$first <- 999
    expect_false(p1_copy$first == p1$first)
})

test_that("TypedPair initialize and type safety", {
    expect_error(TypedPair(T1 = 0),
                 regexp = paste("should be from class", dQuote("character")))
    expect_error(TypedPair(first = 0, second = 1),
                 regexp = "argument \"T1\" is missing, with no default")
    expect_error(TypedPair("character", first = 0, second = 1),
                 regexp = "argument \"T2\" is missing, with no default")

    # You can construct it by explicitly specifying types and values, ...
    x0 <- TypedPair(T1 = "character", first = "x",
                    T2 = "numeric", second = 0)
    # ... or just implicitly pass all arguments (order matters)
    expect_true(all.equal(target  = x0, 
                          current = TypedPair("character", "x", "numeric", 0)))

    # Changing the types of the elements should not be possible
    expect_error(x0$T1 <- "numeric", regexp = "is read-only")
    expect_error(x0$T2 <- "integer", regexp = "is read-only")

    # A bad type also should throw right at initialization
    expect_error(TypedPair("character", 0, "numeric", 0),
                 regexp = "expected character but got numeric")
})

test_that("TypedPair copy and that it behaves like a Pair", {
    x0 <- TypedPair("character", "x", "numeric", 0)

    # Changing the value to some other type, would be possible, but not, if you
    # use the corresponding setter functions.
    expect_error(x0$setFirst(1), 
                 regexp = "bad type - expected character but got numeric")
    x0$setFirst("z") # ok
    expect_equal(x0$first, expected = "z")
    expect_error(x0$setSecond("z"), 
                 regexp = "bad type - expected numeric but got character")
    x0$setSecond(0 + 10)
    expect_equal(x0$second, expected = 10)

    y1 <- TypedPair("character", "y", "numeric", 1)
    y1y1 <- y1$copy()
    expect_true(all.equal(y1, y1y1))
    x0$swap(y1)
    expect_true(all.equal(x0, y1y1))
})

