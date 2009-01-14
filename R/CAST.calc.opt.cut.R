`CAST.calc.opt.cut` <-
function( Z, beta = 0.9, stages=2, t=3, plot=FALSE, ... ) {

	cuts = t:max(2*Z$V$tot.votes)
	ns = rep( NA, length(cuts) )
	qs = rep( NA, length(cuts) )
	cnt = 1
	done = FALSE
	while( !done ) {
		
		s = CAST.calc.sample( Z, beta, stages, t, small.cut=cuts[[cnt]], ... )
		if ( s$q < 0 ) {
			done = TRUE
		} else {
			ns[cnt] = s$n
			qs[cnt] = s$q
		#	cat( "on ", cuts[[cnt]], " - ", ns[cnt], "\n" )
			cnt = cnt+1
		}
	}
	names(ns) = cuts
	ns = ns[1:(cnt-1)]
	qs = qs[1:(cnt-1)]
	cuts = cuts[1:(cnt-1)]
	
	if ( plot ) {
		plot( names(ns), ns, ylim=c(0,max(ns)), type="s", main="Sample Size by Small Cut", pch=19,
		ylab="sample size needed", xlab="cut-off for cutting out small precincts" )
		scale = max(qs) / max(ns) 
		qsU = unique( qs )
		points( names(ns), qs / scale, type="s", col="red" )
		axis( side=4, at=qsU / scale, labels=qsU )
	}
	cuts = data.frame( cut=cuts, n=ns, q=qs )
	v = which.min( cuts$n )
	cuts[v, ]
}

