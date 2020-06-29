def loc_to_ulf(loc):
	x = ":x " + str(loc[0])
	y = ":y " + str(loc[1])
	z = ":z " + str(loc[2])
	return '($ loc' + x + ' ' + y + ' ' + z + ')'