#' strTrim
#'
#' Trim leading and trailing whitespace in character string.
#'
#' Like str_trim() in stringr package or trimws() in R3.2.0 but way faster.
#'
#' Source: Hadley Wickham comment at http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
#'
#' @param string character string
#'
#' @return returns a character string with removed leading and trailing whitespace characters.
#' @export
#' @seealso \code{\link{isString}} for testing whether it is 1 character vector, \code{\link{str_contains}} for finding string in string without regexp, \code{\link{str_find}} for all indices without regexp, \code{\link{str_find1}} for the first index withoud regexp.
#' @examples
#' strTrim("      Hello World!    ")
strTrim <- function (string) {
    gsub("^\\s+|\\s+$", "", string)
}



# cyklus <- function(vektorCh) {
#     print(paste0("Cyklus pro argument ", vektorCh))
#     vektor <- eval(parse(text = vektorCh))
#     vystup <- "  "
#     for (i in vektor)
#         vystup <- paste(vystup, i, sep = "  ")
#
#     print(paste0(vystup, "."))
# }

# cyklus("1:3")   # jako Matlab
# cyklus("3:1")   # !!! Matlab by dal Empty matrix: 1-by-0
# cyklus("seq(1, 3, 0.5)")   # jako Matlab
# cyklus("seq(3, 1, -1)")   # jako Matlab
# #cyklus("seq(3, 1, 1)")    #  !!! skončí s chybou, Matlab by dal Empty matrix: 1-by-0
# cyklus("integer(0)")  # prázdný vektor, cyklus neproběhne ani jednou - jako Matlab
# # takže teď potřebujeme jenom vytvořit "Matlab-like" generátor sekvencí
#
# cyklus("seqM(1, 3)")   # jako Matlab
# cyklus("seqM(3, 1)")   # jako Matlab
# cyklus("seqM(1, 3, 0.5)")   # jako Matlab
# cyklus("seqM(3, 1, -1)")   # jako Matlab
# cyklus("seqM(3, 1, 1)")    #  jako Matlab
#
# x <- seq(0, 2*pi, by = 0.2)
# Stem(x, sin(x))


#' seqM
#'
#' Matlab-like behaviour of colon operator or linspace for creating sequances, for-loop friendly.
#'
#' Like \code{seq()} but with Matlab-like behavior ([: operator] with \code{by} or [linspace] with \code{length.out}).
#'
#' If I create a for-loop, I would like to get an empty vector for 3:1 (I want a default step +1)
#' and also an empty vector for seq(3, 1, by = 1) (not an error). This is solved by this \code{seqM} function.
#'
#' @section Comparison:
#' \tabular{lllll}{
#'   R: seqM  \tab    \tab                        Matlab  \tab \tab                          R: seq  \cr
#'   seqM(1, 3)  \tab       [1] 1 2 3      \tab           1:3           \tab  the same           \tab        the same \cr
#'   seqM(1, 3, by=.8) \tab [1] 1.0 1.8 2.6 \tab          1:.8:3        \tab  the same            \tab       the same \cr
#'   seqM(1, 3, by=5)  \tab [1] 1          \tab           1:5:3         \tab  the same             \tab      the same \cr
#'   seqM(3, 1)     \tab    integer(0)    \tab            3:1           \tab  the same            \tab       [1] 3 2 1 \cr
#'   seqM(3, 1, by=+1) \tab integer(0)    \tab            3:1:1         \tab  the same            \tab       Error: wrong 'by' \cr
#'   seqM(3, 1, by=-1) \tab [1] 3 2 1     \tab            3:-1:1        \tab  the same            \tab       the same \cr
#'   seqM(3, 1, by=-3) \tab [1] 3        \tab             3:-3:1        \tab  the same            \tab       the same \cr
#'   seqM(1, 3, len=5) \tab [1] 1.0 1.5 2.0 2.5 3.0  \tab linspace(1,3,5) \tab the same           \tab        the same \cr
#'   seqM(1, 3, len=3) \tab [1] 1 2 3       \tab          linspace(1,3,3) \tab the same           \tab        the same \cr
#'   seqM(1, 3, len=2) \tab [1] 1 3        \tab           linspace(1,3,2) \tab the same           \tab        the same \cr
#'   seqM(1, 3, len=1) \tab [1] 3          \tab          linspace(1,3,1) \tab the same             \tab      [1] 1 \cr
#'   seqM(1, 3, len=0) \tab integer(0) + warning \tab     linspace(1,3,0) \tab the same without warning \tab  the same without warning \cr
#'   seqM(3, 1, len=3) \tab [1] 3 2 1          \tab       linspace(3,1,3) \tab the same                \tab   the same  \cr
#' }
#'
#'
#' @param from starting value of the sequence (the first number)
#' @param to end value of the sequence (the last number or the boundary number)
#' @param by increment of the sequence (if specified, do not use the \code{length.out} parameter). If both \code{by} and \code{length.out} are not specified, then \code{by = +1}.
#' @param length.out desired length of the sequence (if specified, do not use the \code{by} parameter)
#'
#' @return returns a vector of type "integer" or "double"
#' @export
#' @seealso \code{\link{round2}}, \code{\link{isNum}}, \code{\link{isInt}}, \code{\link{ifft}}.
#'
#' @examples
#' seqM(1, 3)
#' seqM(1, 3, by=.8)
#' seqM(1, 3, by=5)
#' seqM(3, 1)
#' seqM(3, 1, by=+1)
#' seqM(3, 1, by=-1)
#' seqM(3, 1, by=-3)
#' seqM(1, 3, len=5)
#' seqM(1, 3, len=3)
#' seqM(1, 3, len=2)
#' seqM(1, 3, len=1)
#' seqM(1, 3, len=0)
#' seqM(3, 1, len=3)
#'
#'
#'
seqM <- function(from, to, by=NA, length.out=NA) {
    # nonsense or default parameters

    if (class(from) != "numeric" & class(from) != "integer")
        stop("'from' must be numeric or integer")
    if (length(from) != 1)
        stop("'from' must be 1 number")

    if (class(to) != "numeric" & class(to) != "integer")
        stop("'to' must be numeric or integer")
    if (length(to) != 1)
        stop("'to' must be 1 number")

    if (length(by) != 1)
        stop("'by' must be 1 number")
    if (!is.na(by)) {
        if (class(by) != "numeric" & class(by) != "integer")
            stop("'by' must be numeric or integer")
    }

    if (length(length.out) != 1)
        stop("'length.out' must be 1 number")
    if (!is.na(length.out)) {
        if (class(length.out) != "numeric" & class(length.out) != "integer")
            stop("'length.out' must be numeric or integer")
    }

    if (!is.na(by) & !is.na(length.out))
        stop("too many arguments, cannot set 'by' and 'length.out' together")

    if (is.na(by) & is.na(length.out))
        by <- 1


    # VAR 1) length.out
    if (!is.na(length.out)) {
        if (!isInt(length.out)) {
            len <- trunc(length.out)
            warning(paste0("length.out is not integer (length.out=", length.out, "), truncating it to: ", len))
            length.out <- len
        }
        if (length.out == 0) {
            warning("length.out == 0, return empty vector")
            return(integer(0))
        }
        if (length.out < 0) {
            warning(paste0("length.out < 0 (length.out=", length.out, "), return empty vector"))
            return(integer(0))
        }
        if (length.out == 1) {
            return(to)  # Matlab behavior
        }
        return(seq(from, to, length.out = length.out))
    }


    # VAR 2) by
    # integer or numeric?
    if (isInt(from) & isInt(to) & isInt(by)) {
        outInt <- TRUE
        from <- as.integer(from)
        to <- as.integer(to)
        by <- as.integer(by)
    }
    else {
        outInt <- FALSE
    }

    if (by == 0) {
        warning("by == 0, return empty vector")
        return(integer(0))
    }

    if (by > 0) {
        if (from > to)
            return(integer(0))

        if (outInt)
            return(seq.int(from, to, by))
        else
            return(seq(from, to, by))

    } else {
        if (from < to)
            return(integer(0))

        if (outInt)
            return(seq.int(from, to, by))
        else
            return(seq(from, to, by))
    }

}


#' Stem
#'
#' Matlab-like stem plotting function for discrete series.
#'
#' Discrete plots using base plotting system.
#'
#' Author: Matti Pastell, Sep 11 2009
#' http://mpastell.com/2009/09/11/matlab-style-stem-plot-with-r/
#'
#' @param x horizontal-axis values
#' @param y vertical-axis values
#' @param pch integer value, style of points (\code{pch = 21}: circle without fill, see \code{plot} \code{pch} parameter)
#' @param linecol color of the plot
#' @param clinecol zero axis color
#' @param ... other parameters passed to \code{plot} function
#'
#' @return creates a plot in base plotting system.
#' @export
#' @seealso For interactive time-series plots, see package \code{dygraphs}.
#'
#' @examples
#' t <- seqM(from = 0, to = 2*pi, length.out = 20)
#' Stem(t, sin(t))
#' Stem(t, sin(t), pch=21)
#' Stem(t, sin(t), pch=21, line="blue")
#' Stem(t, sin(t), main = "Default style")
Stem <- function(x,y,pch=16,linecol=1,clinecol=1,...){
    if (missing(y)) {
        y = x
        x = 1:length(x) }
    plot(x,y,pch=pch,col=linecol,...)
    for (i in 1:length(x)){
        lines(c(x[i],x[i]), c(0,y[i]),col=linecol)
    }
    lines(c(x[1]-2,x[length(x)]+2), c(0,0),col=clinecol)
}


#' isInt
#'
#' Returns TRUE / FALSE whether it is exactly 1 integer number (in fact, the class can be numeric but the number must be integer), non-missing
#'
#' @param num variable to be tested
#'
#' @return TRUE / FALSE
#' @export
#' @seealso \code{\link{isNum}}, \code{\link{isString}}
#'
#' @examples
#' isInt(2)
#' isInt(2L)
#' isInt(-2)
#' isInt(-2L)
#' isInt(2.1)
#' isInt(-2.1)
#' isInt(1:5)
#' isInt(NA_integer_)
#' isInt(integer(0))
isInt <- function(num) {
    if (!("numeric" %in% class(num))  &  !("integer" %in% class(num)))
        return(FALSE)

    if (length(num) != 1)
        return(FALSE)

    if (is.na(num))
        return(FALSE)

    if (trunc(num) == num)
        return(TRUE)
    else
        return(FALSE)
}

#' isString
#'
#' Returns TRUE / FALSE whether it is exactly 1 character string (character vector of length 1, non-missing)
#'
#' @param string variable to be tested
#'
#' @return TRUE / FALSE
#' @export
#' @seealso \code{\link{isInt}}, \code{\link{isNum}}
#'
#' @examples
#' isString("hello")
#' isString(2)
#' isString(c("hello", "world"))
#' isString(NA_character_)
isString <- function(string) {
    if (!("character" %in% class(string)))
        return(FALSE)

    if (length(string) != 1)
        return(FALSE)

    if (is.na(string))
        return(FALSE)

    return(TRUE)
}

#' isNum
#'
#' Returns TRUE / FALSE whether it is exactly 1 number (numeric or integer vector of length 1, non-missing)
#'
#' @param num variable to be tested
#'
#' @return TRUE / FALSE
#' @export
#' @seealso \code{\link{isInt}}, \code{\link{isString}}
#'
#' @examples
#' isNum(2)
#' isNum(2L)
#' isNum(-2)
#' isNum(-2L)
#' isNum(2.1)
#' isNum(-2.1)
#' isNum(1:5)
#' isNum(NA_real_)
#' isNum(numeric(0))
isNum <- function(num) {
    if (!("numeric" %in% class(num))  &  !("integer" %in% class(num)))
        return(FALSE)

    if (length(num) != 1)
        return(FALSE)

    if (is.na(num))
        return(FALSE)

    return(TRUE)
}


# Zaoukrouhlí na daný počet desetinných míst (order 1: desítky, 0:
#                                                 jednotky, -1 desetiny, -2 setiny apod.)
#
# Př. round2(pi*100, -2), round2(pi*100, 2)


#' round2
#'
#' Rounds a number to the specified order. Round half away from zero (this is the difference from built-in \code{round} function.)
#'
#' @param x number to be rounded
#' @param order 0 (default) = units, -1 = 0.1, +1 = 10
#'
#' @return rounded number to the specified order
#' @export
#' @seealso \code{\link{round}}, \code{\link{trunc}}, \code{\link{ceiling}}, \code{\link{floor}}
#'
#' @examples
#' round2(23.5)   # = 24, compare: round(23.5) = 24
#' round2(23.4)   # = 23
#' round2(24.5)   # = 25, compare: round(24.5) = 24
#' round2(-23.5)   # = -24, compare: round(-23.5) = -24
#' round2(-23.4)   # = -23
#' round2(-24.5)   # = -25, compare: round(-24.5) = -24
#' round2(123.456, -1)   # 123.5
#' round2(123.456, -2)   # 123.46
#' round2(123.456, 1)  # 120
#' round2(123.456, 2)  # 100
#' round2(123.456, 3)  # 0
#' round2(-123.456, -1)   # -123.5
#' round2(-123.456, -2)   # -123.46
#' round2(-123.456, 1)  # -120
#' round2(-123.456, 2)  # -100
#' round2(-123.456, 3)  # 0
round2 <- function(x, order = 0) {
    zaokrouhli <- function(cislo) {
        return(trunc(cislo + sign(cislo)*0.5))
    }

    return(zaokrouhli(x / 10^order) * 10^order)
}


#' str_contains
#'
#' @param string string in which we try to find something
#' @param patternNoRegex string we want to find, "as it is" - no regular exprressions
#'
#' @return TRUE / FALSE
#' @export
#' @seealso \code{\link{str_find}}, \code{\link{str_find1}}, \code{\link{isString}}
#'
#' @examples
#' str_contains("Hello world", "wor")  # TRUE
#' str_contains("Hello world", "WOR")  # FALSE
#' str_contains(tolower("Hello world"), tolower("wor"))  # TRUE
#' str_contains("Hello world", "")  # TRUE
str_contains <- function(string, patternNoRegex) {
    return(regexpr(patternNoRegex, string, fixed = TRUE)[1] != -1)
}


#' str_find
#'
#' @param string string in which we try to find something
#' @param patternNoRegex string we want to find, "as it is" - no regular exprressions
#'
#' @return indices of all occurences (1 = 1st character)
#' @export
#' @seealso \code{\link{str_find1}}, \code{\link{str_contains}}, \code{\link{isString}}
#'
#' @examples
#' str_find("Hello, hello, hello world", "ell")   # 2 9 16
#' str_find("Hello, hello, hello world", "q")     # integer(0)
str_find <- function(string, patternNoRegex) {
    indexy <- as.integer(gregexpr(patternNoRegex, string, fixed = TRUE)[[1]])
    if (length(indexy) == 1 & indexy[1] == -1)
        indexy <- integer(0)
    return(indexy)
}

# Vrátí index první nalezené pozice podřetězce patternNoRegex ve string.
# prádný vektor = nenalezeno, 1 ... první znak apod.

#' str_find1
#'
#' @param string string in which we try to find something
#' @param patternNoRegex string we want to find, "as it is" - no regular exprressions
#'
#' @return index of the first occurence only (1 = 1st character)
#' @export
#' @seealso \code{\link{str_find}}, \code{\link{str_contains}}, \code{\link{isString}}
#'
#' @examples
#' str_find1("Hello, hello, hello world", "ell")   # 2
#' str_find1("Hello, hello, hello world", "q")     # integer(0)
str_find1 <- function(string, patternNoRegex) {
    index <- regexpr(patternNoRegex, string, fixed = TRUE)[1]
    if (index == -1)
        index <- integer(0)
    return(index)
}

#' ifft
#'
#' Inverse Fast Fourier Transform (discrete FT), Matlab-like behavior.
#'
#' This is really the inverse of the fft function, so ifft(fft(x)) == x.
#'
#' @param sig input vector
#'
#' @return output vector of the same length as the input vector
#' @export
#' @seealso \code{\link{fft}}, \code{\link{Re}}, \code{\link{Im}}, \code{\link{Mod}}, \code{\link{Conj}}
#'
#' @examples
#' ifft(fft(1:5))
ifft <- function(sig) {
    return(fft(sig, inverse = TRUE) / length(sig))
}
