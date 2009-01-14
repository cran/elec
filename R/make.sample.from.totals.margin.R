`make.sample.from.totals.margin` <-
function( M,  totals, per.winner=NULL ) {
	GT = sum(totals)
	vote.W = round( GT* (0.5 + M/2) )
	vote.L = round( GT * (0.5 - M/2) )
	make.sample.from.totals( vote.W, vote.L, totals )
}

