#example_frequencies_processing.R

#English classics
#source: http://www.data-compression.com/english.html
#classics_raw includes the space character
classics_raw <- c(0.0651738,0.0124248,0.0217339,0.0349835,0.1041442,0.0197881,0.0158610,0.0492888,0.0558094,0.0009033,0.0050529,0.0331490,0.0202124,0.0564513,0.0596302,0.0137645,0.0008606,0.0497563,0.0515760,0.0729357,0.0225134,0.0082903,0.0171272,0.0013692,0.0145984,0.0007836,0.1918182)

#recompute freq to exclude space
classics <- classics_raw[1:26] / sum(classics_raw[1:26])
names(classics) <- letters


#Oxford
#source: http://www.oxforddictionaries.com/us/words/what-is-the-frequency-of-the-letters-of-the-alphabet-in-english
oxford_raw <- c(0.084966,0.020720,0.045388,0.033844,0.111607,0.018121,0.024705,0.030034,0.075448,0.001965,0.011016,0.054893,0.030129,0.066544,0.071635,0.031671,0.001962,0.075809,0.057351,0.069509,0.036308,0.010074,0.012899,0.002902,0.017779,0.002722)

#recompute
oxford <- oxford_raw / sum(oxford_raw)
names(oxford) <- letters
