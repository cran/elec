`make.ok.truth` <-
function( Z, num.off = 8, amount.off = 5 ) {
	num.off = floor(num.off / 2)
	off = sample( 1:nrow(Z$V), num.off * 2 )
	offUp = off[1:num.off]
	offDown = off[num.off + 1:num.off]
	CW = Z$winners[Z$f]
	CL = Z$losers[1]
	Z$V[ offUp, CW ] = Z$V[ offUp, CW ] + amount.off
	Z$V[ offUp, CL ] = pmax( Z$V[ offUp, CL ] - amount.off, 0 )
	Z$V[ offDown, CW ] = pmax( Z$V[ offDown, CW ] - amount.off, 0 )
	Z$V[ offDown, CL ] = Z$V[ offDown, CL ] + amount.off
	
	Z = countVotes(Z)
	
	Z
}

