context("strings")

test_that("strtrim works", {
    expect_equal(strTrim("      Hello World!    "), "Hello World!")
    expect_equal(strTrim("Hello World!    "), "Hello World!")
    expect_equal(strTrim("      Hello World!"), "Hello World!")
    expect_equal(strTrim("Hello World!"), "Hello World!")
    expect_equal(strTrim("   "), "")
    expect_equal(strTrim(""), "")
    expect_equal(strTrim(" ěšččřžýůú  "), "ěšččřžýůú")
    expect_output(strTrim(NA), NA)
    expect_equal(strTrim(1:5), as.character(1:5))
})

test_that("str_contains works", {
    expect_equal(str_contains("Hello world", "wor"), TRUE)
    expect_equal(str_contains("Hello world", "WOR"), FALSE)
    expect_equal(str_contains(tolower("Hello world"), tolower("wor")), TRUE)
    expect_equal(str_contains("Hello world", ""), TRUE)
})

test_that("str_find works", {
    expect_equal(str_find("Hello, hello, hello world", "ell"), c(2, 9, 16))
    expect_identical(str_find("Hello, hello, hello world", "q"), integer(0))
})

test_that("str_find1 works", {
    expect_equal(str_find1("Hello, hello, hello world", "ell"), 2)
    expect_identical(str_find1("Hello, hello, hello world", "q"), integer(0))
})


context("sequence")

test_that("strtrim works", {
    expect_equal(seqM(1, 3, 0.5), c(1, 1.5, 2, 2.5, 3))
    expect_equal(seqM(1, 3), 1:3)
    expect_equal(seqM(1, 3, by=.8), c(1, 1.8, 2.6))
    expect_equal(seqM(1, 3, by=5), 1)
    expect_equal(seqM(3, 1), integer(0))
    expect_equal(seqM(3, 1, by=+1), integer(0))
    expect_equal(seqM(3, 1, by=-1), c(3, 2, 1))
    expect_equal(seqM(3, 1, by=-3), 3)
    expect_equal(seqM(1, 3, len=5), c(1, 1.5, 2, 2.5, 3))
    expect_equal(seqM(1, 3, len=3), c(1, 2, 3))
    expect_equal(seqM(1, 3, len=2), c(1, 3))
    expect_equal(seqM(1, 3, len=1), 3)
    expect_warning(seqM(1, 3, len=0), "length.out == 0, return empty vector")
    expect_warning(seqM(1, 3, len=-2))
    expect_equal(seqM(3, 1, len=3), c(3, 2, 1))

})


context("isSomething")

test_that("isInt works", {
    expect_equal(isInt(2), TRUE)
    expect_equal(isInt(2L), TRUE)
    expect_equal(isInt(-2), TRUE)
    expect_equal(isInt(-2L), TRUE)
    expect_equal(isInt(2.1), FALSE)
    expect_equal(isInt(-2.1), FALSE)
    expect_equal(isInt(1:5), FALSE)
    expect_equal(isInt(NA_integer_), FALSE)
    expect_equal(isInt(NA), FALSE)
    expect_equal(isInt(integer(0)), FALSE)
    expect_equal(isInt(mtcars), FALSE)
})



test_that("isString works", {
    expect_equal(isString("hello"), TRUE)
    expect_equal(isString(""), TRUE)
    expect_equal(isString(2), FALSE)
    expect_equal(isString(c("hello", "world")), FALSE)
    expect_equal(isString(NA_character_), FALSE)
    expect_equal(isString(NA), FALSE)
    expect_equal(isString(character(0)), FALSE)
    expect_equal(isString(mtcars), FALSE)
})

test_that("isNum works", {
    expect_equal(isNum(2), TRUE)
    expect_equal(isNum(2L), TRUE)
    expect_equal(isNum(-2), TRUE)
    expect_equal(isNum(-2L), TRUE)
    expect_equal(isNum(2.1), TRUE)
    expect_equal(isNum(-2.1), TRUE)
    expect_equal(isNum(1:5), FALSE)
    expect_equal(isNum(NA_integer_), FALSE)
    expect_equal(isNum(NA_real_), FALSE)
    expect_equal(isInt(NA), FALSE)
    expect_equal(isInt(integer(0)), FALSE)
    expect_equal(isInt(numeric(0)), FALSE)
    expect_equal(isInt(mtcars), FALSE)
})



context("round2")

test_that("round2 works", {
    expect_equal(round2(23.5), 24)
    expect_equal(round2(23.4), 23)
    expect_equal(round2(24.5), 25)
    expect_equal(round2(-23.5), -24)
    expect_equal(round2(-23.4), -23)
    expect_equal(round2(-24.5), -25)
    expect_equal(round2(123.456, -1), 123.5)
    expect_equal(round2(123.456, -2), 123.46)
    expect_equal(round2(123.456, 1), 120)
    expect_equal(round2(123.456, 2), 100)
    expect_equal(round2(123.456, 3), 0)
    expect_equal(round2(-123.456, -1), -123.5)
    expect_equal(round2(-123.456, -2), -123.46)
    expect_equal(round2(-123.456, 1), -120)
    expect_equal(round2(-123.456, 2), -100)
    expect_equal(round2(-123.456, 3), 0)
    expect_output(round2(NA), NA)
    expect_equal(round2(c(0.3, 2, pi)), c(0, 2, 3))
})


context("ifft")

test_that("ifft works", {
    expect_equal(Re(ifft(3)), 3)
    expect_equal(Im(ifft(3)), 0)
    expect_equal(ifft(fft(1:5)), as.complex(1:5))
})
