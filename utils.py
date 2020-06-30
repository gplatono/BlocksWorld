def loc_to_ulf(location):
	x = ":x " + str(location[0])
	y = ":y " + str(location[1])
	z = ":z " + str(location[2])
	return '($ loc ' + x + ' ' + y + ' ' + z + ')'