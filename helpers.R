# helper functions

library(ggplot2)
library("highcharter")

sanitize <- function(str, keyl=1, let=1) {
  # input: raw text string
  # output: relative frequencies
  str <- substr(str, 1, 30000)
  newstr <- gsub("[^[:alpha:]]", "", str)
  newstr <- tolower(newstr)
  chars <- strsplit(newstr, NULL)[[1]]
  nl <- length(chars)
  seqind <- seq(let, nl, keyl)
  freqs <- summary(factor(chars[seqind]))
  rfreqs <- freqs/sum(freqs)
  return(freqs)
}

fill_freqs <- function(freqs, pos = 0) {
  # input: frequencies (not necessarily for all 26 letters)
  # output: frequencies for all 26 letters
  # moves over the frequencies by pos shift
  
  # make 0-count vector
  alphabet <- rep(1, 26)
  names(alphabet) <- letters
  
  # alphabet_filled <- sapply(1:26, function(x) {freqs[names(alphabet)[x]]})
  alphabet_filled <- freqs[names(alphabet)]
  alphabet_filled[is.na(alphabet_filled)] <- 0
  
  names(alphabet_filled) <- letters
  # move over the frequencies by pos
  shiftedind <- ((1:26) + pos - 1) %% 26 + 1 
  alphabet_filled <- alphabet_filled[shiftedind]
  names(alphabet_filled) <- letters
  alphabet_filled <- as.data.frame(alphabet_filled)
  names(alphabet_filled) <- "rfreqs"
  return(alphabet_filled)
} 

mod1 <- function(v, n) {
  # mod1(1:20, 6) => 1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 5 6 1 2
  ((v - 1) %% n) + 1
}

str2ints <- function(s) {
  # input: character string (s)
  # ouput: integer vector representing letters (position) from character string
  as.integer(Filter(Negate(is.na),
    factor(levels = LETTERS, strsplit(toupper(s), "")[[1]])))
}

vigen <- function(input, key, decrypt = F) {
  input <- str2ints(input)
  key <- rep(str2ints(key), len = length(input)) - 1
  paste(collapse = "",
        LETTERS[mod1(input + (if (decrypt) -1 else 1)*key, length(LETTERS))]
  )
}

# function to build chart -- set and edit options here
build_highchart <- function(str, ref, keyl, pos, let) {

  freqs <- sanitize(str,keyl,let)
  rfreqs <- freqs/sum(freqs)
  alphabet_filled_freqs <- fill_freqs(freqs,pos)
  alphabet_filled_rfreqs <- fill_freqs(rfreqs,pos)

  alphabet_filled_ref <- fill_freqs(ref,0)

  hc <- highchart() %>%
    hc_chart(type = "column",
             animation = "false",
             marginRight = "0"
    ) %>%
    hc_plotOptions(
      column = list(
        grouping = FALSE,
        shadow = FALSE,
        borderWidth = 0,
        animation = FALSE
    )) %>%
    hc_yAxis(
      visible = FALSE,
      lineWidth = 0,
      gridLineWidth = 0,
      minorGridLineWidth = 0,
      tickLength = 0,
      tickWidth = 0
    ) %>%
    hc_legend(
      enabled = T,
      align = "right",
      verticalAlign = "top",
      layout = "vertical"
    ) %>%
    hc_tooltip(
      enabled = T,
      valueDecimals = 3
    ) %>%
    hc_xAxis(categories = LETTERS) %>%
    hc_add_series(name = "Reference Distribution", color = "rgba(223, 83, 83, .5)", data = alphabet_filled_ref$rfreqs, pointPadding = -0.1, pointPlacement = 0) %>%
    hc_add_series(name = "Input Frequencies", color = "#3182bd", data = alphabet_filled_rfreqs$rfreqs, pointPadding = 0.2, pointPlacement = 0)
  
  # Use the color codes below to set the chart color for each series
  # color options = {light blue: #9ecae1, dark blue: #3182bd, light red: rgba(223, 83, 83, .5), red: #9A3334, other red: #CC6666, purple: rgba(165,170,217,1), 
  # dark purple: rgba(126,86,134,.9), orange: rgba(248,161,63,1), dark orange: rgba(186,60,61,.9)
  
  return(hc)
}




















