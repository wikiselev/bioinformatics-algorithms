library(ggplot2)

# length of each string
N = 1000
# number of letters in alphabet (A, C, G, T)
A = 4

# P(N, A, Pattern, t) denote the probability that a string Pattern appears t or
# more times in a random string of length N formed from an alphabet of A letters.
P <- function(N, A, k, t) {
	return(choose(N - t*(k - 1), t)/A^(t*k))
}

# function that constructs a data frame of P(N, A, Pattern, t) for given N, n and t,
# and varying k (here k = seq(1,12) - I am interested in k-mers with lengths 1..12)
get_P_data_frame <- function(N, n, t) {
	d <- data.frame(N = N, A = A, k = seq(1,12), t = rep(t, each = 12), n = n)
	d$P <- P(N, A, d$k, d$t)*n
	# id is a unique line identifier - it is need for grouping in plotting with geom_line
	d$id <- paste0(d$t, "_", d$n)
	return(d)
}

# construct a total data frame by varying n and t
d <- NULL
for (n in c(1, 500)) {
	for (t in c(1, 2, 3)) {
		d <- rbind(d, get_P_data_frame(N, n, t))
	}
}

# plot the total data frame
p <- ggplot(d, aes(x = k, y = log10(P), group = id)) +
	 geom_line(aes(color = as.factor(t), linetype = as.factor(n)), size = 1) +
	 geom_hline(yintercept = 0) +
	 coord_cartesian(ylim = c(-1, 9)) +
	 scale_x_continuous(breaks=seq(1, 12, 1)) +
	 scale_y_continuous(breaks=seq(-1, 9, 1)) +
	 labs(x = "k", y = "log10(P*n)", 
	 	title = paste0("Expected k-mer occurrences (P*n)\nin n strings of length ", N),
	 	color = "t", linetype = "n") +
	 theme(text = element_text(size = 16))
png(file = "figures/kmer-expect-occur.png", width = 600, height = 400)
print(p)
dev.off()
