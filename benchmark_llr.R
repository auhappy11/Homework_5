# benchmark
source("llr_functions.R")
microbenchmark::microbenchmark(llr(x = x, y = y, z = z, omega = 1))
