# benchmark
source("llr_functions.R")
microbenchmark::microbenchmark(llr(z = z, x = x, y = y, omega = 1))
