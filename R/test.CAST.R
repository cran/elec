`test.CAST` <-
function() {
	Z = make.cartoon()
	
	samp.info = CAST.calc.sample( Z )
	samp.info
	
	samp = CAST.sample( Z, samp.info )
	
	list( samp.info, samp )
}

