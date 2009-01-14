`tri.sample.stats` <-
function( samp ) {
	
	c( sampled = nrow( samp ),
		votes = sum( samp$tot.votes ),
		mean.e.max = mean( samp$e.max ) )
		
}

