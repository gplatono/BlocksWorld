def loc_to_ulf(location):
	x = ":x " + str(location[0])
	y = ":y " + str(location[1])
	z = ":z " + str(location[2])
	return '($ loc ' + x + ' ' + y + ' ' + z + ')'


def lisp_to_pylist(lisp_list):
	# ret_list = []
	# token = ""
	# while index < len(lisp_list) and lisp_list != ')':
	# 	if lisp_list[index] == '(':
	# 		token = lisp_to_pylist(lisp_list)
	stack = []
	current = []
	token = ""
	for char in lisp_list:
		if char == '(':
			stack.append(current)
			current = []
		elif char == ')':
			if token != "":
				current += [token]
				token = ""                
			if (len(stack) > 0):
				stack[-1].append(current)
				current = stack[-1]
				stack.pop()                    
		elif char == ' ':
			if token != "":
				current += [token]
				token = ""
		else:
			token += char
	return current[0]