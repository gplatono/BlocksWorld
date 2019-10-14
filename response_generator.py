

class ResponseGenerator(object):
	''' Generates plain english responses from query and answer '''

	def __init__(self):
		self.cert_threashold = 0.7
		self.debugging = True

	'''
	For Confirm:
	"Is the McDonalds block near the Burger King block?":
		(yes)   [<[McDonalds], "near" [Burger_King], 1>]
		(maybe) [<[McDonalds], "near" [Burger_King], {0 < x < 1}>]
		(no)    [<[McDonalds], "near" [Burger_King], 0>]

	For Color:
	"What color is the SRI block?":
		[<[SRI], None, [], 1>]
	"What color are the blocks to the left of the Twitter block?":
		[<[A], "to the left of", [Twitter], 0.95>
		 <[B], "to the left of", [Twitter], 0.90>
		 <[C], "to the left of", [Twitter], 0.80>]
		-> flag arg0_inquired = true
	"The Toyota block is touching what color blocks?":
		[<[Toyota], "touching", [A], 0.9>
		 <[Toyota], "touching", [B], 0.8>
		 <[Toyota], "touching", [C], 0.7>]
		 -> flag arg1_inquired = true

	For Exist:
	"Is there a blue block next to the Mercedes block?"
		(no)    [<[], "next to", [Mercedes], 1>]
		(yes)   [<[B], "next to", [Mercedes], {x > cert_threashold}>]
		(maybe) [<[B], "next to", Mercedes], {x < cert_threashold}>]
		-> flag arg0_inquired = true
	"Is the Toyota block next to any blocks?"
		(no)	[<[Toyota], "next to", [], 1>]
		(yes)	[<[Toyota], "next to", [A], {x > cert_threashold}]
		(maybe) [<[Toyota], "next to", [B], {x < cert_threashold}]
		-> flag arg1_inquired = true


	For Ident & Desc do what's natural, and for count just make the respose_object
	contain x 4-tuples

	TODO: add subject type (the noun that is the subject) to query object
		  add the flags for which part of the relation is the inquired part (arg0_inquired and arg1_inquired)


	'''
	def generate_response(self, query_object, response_object):
		# Process the response_object -----------------------------------------
		arg0_uniform = all(response_object[i][0] == response_object[0][0] for i in range(len(response_object)))
		rel_uniform = all(response_object[i][1] == response_object[0][1] for i in range(len(response_object)))
		arg1_uniform = all(response_object[i][2] == response_object[0][2] for i in range(len(response_object)))
		all_cert = all(response_object[i][3] > self.cert_threashold for i in range(len(response_object)))
		all_uncert = all(response_object[i][3] <= self.cert_threashold for i in range(len(response_object)))
		mixed_cert = (not all_cert) and (not all_uncert)
		singleton_response = (1 == len(response_object))

		subj_adjs = query_object.extract_subject_adj_modifiers()
		subj_type = query_object.subject_type
		subj_type_plur = self.pluralize(subj_type)

		arg0_inq = query_object.arg0_inquired
		arg1_inq = query_object.arg1_inquired

		if self.debugging:
			print(response_object)
			print(arg0_uniform, rel_uniform, arg1_uniform, all_cert, all_uncert, mixed_cert)

		# Decide on how to "dress" the response -------------------------------
		# will we include the relation in the response?
		grounding = True
		# does the question's wording have a false preconcieved notion?
		# i.e. "Which blocks are blue" -> [only one answer] "JUST A is blue"
		# or "Which block is blue" -> [multiple answers] "A, B, and C are ALL blue"
		number_correction = query_object.is_subject_plural == (1 != len(response_object))

		# First deal with the special case ERROR
		if query_object.query_type == QueryFrame.QueryType.ERROR:
			return "Sorry, I was a bit confused by that.  Can you rephrase that in a more simple way?"

		# Now handle Confirmation questions
		if query_object.query_type == QueryFrame.QueryType.CONFIRM:
			arg0_list = self.entities_to_english_list(respose_object[0][0])
			rel = respose_object[0][1]
			arg1_list = self.entities_to_english_list(respose_object[0][2])
			if respose_object[0][3] == 0:
				if subj_adjs != "":
					return "No."
				return "No, " + arg0_list + " " + rel + " " + arg1_list + "."
			if respose_object[0][3] == 1:
				if subj_adjs != "":
					return "Yes."
				return "Yes, " + arg0_list + " " + rel + " " + arg1_list + "."
			else:
				if subj_adjs != "":
					return "Maybe."
				return "Maybe, " + arg0_list + " " + rel + " " + arg1_list + "."

		# More Processing, condense the response object as much as possible
		if len(response_object) > 1:
			if arg0_uniform and rel_uniform:
				if all_cert or all_uncert:
					# merge the arg1s, use a single new response object
					arg1s = []
					for tup in response_object:
						arg1s.append(tup[2])
					response_object = [([response_object[0][0]], response_object[0][1], arg1s, response_object[0][3])]
				else:
					# merge the arg1s, use two new response object
					arg1sC = []
					arg1sUC = []
					for tup in response_object:
						if tup[3] > self.cert_threashold:
							arg1sC.append(tup[2])
						else:
							arg1sUC.append(tup[2])
					response_object = [([response_object[0][0]], response_object[0][1], arg1sC, 1),
									   ([response_object[0][0]], response_object[0][1], arg1sUC, -1)]

			if arg1_uniform and rel_uniform:
				if all_cert or all_uncert:
					# merge the arg0s, use a single new response object
					arg0s = []
					for tup in response_object:
						arg0s.append(tup[0])
					response_object = [(arg0s, response_object[0][1], response_object[0][2], response_object[0][3])]
				else:
					# merge the arg0s, use two new response object
					arg0sC = []
					arg0sUC = []
					for tup in response_object:
						if tup[3] > self.cert_threashold:
							arg0sC.append(tup[0])
						else:
							arg0sUC.append(tup[0])
					response_object = [(arg0sC, response_object[0][1], response_object[0][2], 1),
									   (arg0sUC, response_object[0][1], response_object[0][2], -1)]

			if self.debugging:
				print(response_object)

		# Next handle special case ATTR_COLOR
		if query_object.query_type == QueryFrame.QueryType.ATTR_COLOR:
			# if there is no relation, output only the colors
			if response_object[0][1] == None:
				colsList = self.entities_to_color_list(response_object[0][0])
				ans_list = self.entities_to_english_list(response_object[0][0], 'name')
				if len(response_object[0][0]) == 1:
					return ans_list.capitalize() + " is " + colsList + "."
				else:
					return "They are " + colsList + "."


			if (arg0_inq and response_object[0][0] == []) or (arg1_inq and response_object[0][2] == []):
				return "There is no such " + subj_type + "."

			resp = respose_object[0]

			if arg0_inq: # what color is the block touching the Toyota block?
				colsList = self.entities_to_color_list(resp[0])
				ans_list = self.entities_to_english_list(resp[0], 'name')
				if all_cert:
					if len(resp[0]) == 1:
						if number_correction:
							return "Only a " + colsList + " " + subj_type + " is " + resp[1] + " " + self.entities_to_english_list(resp[0][2], 'name') + "."
						return "A " + colsList + " " + subj_type + " is " + resp[1] + " " + self.entities_to_english_list(resp[0][2], 'name') + "."
					else:
						if number_correction:
							return colsList.capitalize() + " " + subj_type_plur + " are all " + resp[1] + " " + self.entities_to_english_list(resp[0][2], 'name') + "."
						return colsList.capitalize() + " " + subj_type_plur + " are " + resp[1] + " " + self.entities_to_english_list(resp[0][2], 'name') + "."
				else:
					if len(resp[0]) == 1:
						if number_correction:
							return "Perhaps only a " + colsList + " " + subj_type + " is " + resp[1] + " " + self.entities_to_english_list(resp[0][2], 'name') + "."
						return "A " + colsList + " " + subj_type + " might be " + resp[1] + " " + self.entities_to_english_list(resp[0][2], 'name') + "."
					else:
						if number_correction:
							return colsList.capitalize() + " " + subj_type_plur + " all may be " + resp[1] + " " + self.entities_to_english_list(resp[0][2], 'name') + "."
						return colsList.capitalize() + " " + subj_type_plur + " are perhaps " + resp[1] + " " + self.entities_to_english_list(resp[0][2], 'name') + "."

			if arg1_inq: # the Toyota block is touching what color block?
				colsList = self.entities_to_color_list(resp[2])
				ans_list = self.entities_to_english_list(resp[2], 'name')
				arg0_list = self.entities_to_english_list(resp[0], 'name')
				aux = " is "
				if len(resp[0]) > 1:
					aux = " are "
				if all_cert:
					if len(resp[2]) == 1:
						if number_correction:
							return arg0_list.capitalize() + aux + resp[1] + " only a " + colsList + " " + subj_type + "."
						return arg0_list.capitalize() + aux + resp[1] + " a " + colsList + " " + subj_type + "."
					else:
						return arg0_list.capitalize() + aux + resp[1] + " " + colsList + " " + subj_type_plur + "."
				else:
					if len(resp[2]) == 1:
						if number_correction:
							return arg0_list.capitalize() + aux + "probably " + resp[1] + " only a " + colsList + " " + subj_type + "."
						return arg0_list.capitalize() + aux + "probably " + resp[1] + " a " + colsList + " " + subj_type + "."
					else:
						return "Maybe " + arg0_list + aux + resp[1] + " " + colsList + " " + subj_type_plur + "."

		# Now handle Existential questions
		if query_object.query_type == QueryFrame.QueryType.EXIST:
			resp = respose_object[0]
			if arg0_inq:
				if resp[0] == [] or resp[0] == None:
					if subj_adjs == "":
						return "There is no " + subj_type + " that is " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + "."
					return "There is no " + subj_adjs + " " + subj_type + " that is " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + "."
				if all_cert:
					if len(resp[0]) == 1:
						return "Yes, " + entities_to_english_list(resp[0], 'name') + " is " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + "."
					else:
						return "Yes, " + entities_to_english_list(resp[0], 'name') + " are " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + "."
				elif all_uncert:
					if len(resp[0]) == 1:
						return "Maybe, " + entities_to_english_list(resp[0], 'name') + " seems to be " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + "."
					else:
						return "Maybe, " + entities_to_english_list(resp[0], 'name') + " seem to be " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + "."
				elif mixed_cert:
					resp2 = respose_object[1]
					if len(resp[0]) == 1:
						if len(resp2[0] == 1):
							return "Yes, " + entities_to_english_list(resp[0], 'name') + " is " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + ", and maybe " + entities_to_english_list(resp2[0]) + " is too."
						else:
							return "Yes, " + entities_to_english_list(resp[0], 'name') + " is " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + ", and maybe " + entities_to_english_list(resp2[0]) + " are too."
					else:
						if len(resp2[0] == 1):
							return "Yes, " + entities_to_english_list(resp[0], 'name') + " are " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + ", and maybe " + entities_to_english_list(resp2[0]) + " is too."
						else:
							return "Yes, " + entities_to_english_list(resp[0], 'name') + " are " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + ", and maybe " + entities_to_english_list(resp2[0]) + " are too."
			if arg1_inq:
				if resp[2] == [] or resp[2] == None:
					if subj_adjs == "":
						subj_adjs = "such"
					if len(resp[0]) == 1:
						return entities_to_english_list(resp[0], 'name').capitalize() + " is " + resp[1] + " no " + subj_adjs + " " + subj_type + "."
					else:
						return entities_to_english_list(resp[0], 'name').capitalize() + " are " + resp[1] + " no " + subj_adjs + " " + subj_type + "."

				if all_cert:
					if len(resp[0]) == 1:
						return "Yes, " + entities_to_english_list(resp[0], 'name') + " is " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + "."
					else:
						return "Yes, " + entities_to_english_list(resp[0], 'name') + " are " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + "."
				elif all_uncert:
					if len(resp[0]) == 1:
						return "Maybe, " + entities_to_english_list(resp[0], 'name') + " seems to be " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + "."
					else:
						return "Maybe, " + entities_to_english_list(resp[0], 'name') + " seem to be " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + "."
				elif mixed_cert:
					resp2 = respose_object[1]
					if len(resp[0]) == 1:
						return "Yes, " + entities_to_english_list(resp[0], 'name') + " is " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + ", and maybe " + entities_to_english_list(resp2[0]) + " too."
					else:
						return "Yes, " + entities_to_english_list(resp[0], 'name') + " are " + resp[1] + " " + entities_to_english_list(resp[2], 'name') + ", and maybe " + entities_to_english_list(resp2[0]) + " too."
			else:
				return "I'm confused."

		# Next deal with counting questions
		if query_object.query_type == QueryFrame.QueryType.COUNT:
			resp = response_object[0][0]
			arg0_list = self.entities_to_english_list(resp[0], 'name')
			arg1_list = self.entities_to_english_list(resp[2], 'name')

			if arg0_inq: # how many blocks are near the Toyota block?
				count = len(resp[0])

				if resp[1] == None:
					if count == 1:
						if query_object.is_subject_plural:
							return "There is only 1."
						else:
							return "There is 1."
					else:
						return "There are " + str(count) + "."

				if subj_adjs != "":
					if count == 1:
						if query_object.is_subject_plural:
							return "There is only 1 " + subj_adjs + " " + subj_type + " " + resp[1] + " " + arg1_list + "."
						else:
							return "There is 1 " + subj_adjs + " " + subj_type + " " + resp[1] + " " + arg1_list + "."
					else:
						return "There are " + str(count) + " " + subj_adjs + " " + subj_type_plur + " that are " + resp[1] + " " + arg1_list + "."

				if count == 1:
					if query_object.is_subject_plural:
						return "There is only 1 " + subj_type + " " + resp[1] + " " + arg1_list + "."
					else:
						return "There is 1 " + subj_type + " " + resp[1] + " " + arg1_list + "."
				else:
					return "There are " + str(count) + " " + subj_type_plur + " " + resp[1] + " " + arg1_list + "."



			else:		 # the Toyota block is near how many blocks?
				count = len(resp[2])
				aux = " is "
				if len(resp[0]) > 1:
					aux = " are "

				if resp[1] == None:
					if count == 1:
						if query_object.is_subject_plural:
							return "There is only 1."
						else:
							return "There is 1."
					else:
						return "There are " + str(count) + "."

				if subj_adjs != "":
					if count == 1:
						if query_object.is_subject_plural:
							return arg0_list + aux +  ""   "There is only 1 " + subj_adjs + " " + subj_type + " " + resp[1] + " " + arg1_list + "."
						else:
							return "There is 1 " + subj_adjs + " " + subj_type + " " + resp[1] + " " + arg1_list + "."
					else:
						return "There are " + str(count) + " " + subj_adjs + " " + subj_type_plur + " that are " + resp[1] + " " + arg1_list + "."

				if count == 1:
					if query_object.is_subject_plural:
						return "There is only 1 " + subj_type + " " + resp[1] + " " + arg1_list + "."
					else:
						return "There is 1 " + subj_type + " " + resp[1] + " " + arg1_list + "."
				else:
					return "There are " + str(count) + " " + subj_type_plur + " " + resp[1] + " " + arg1_list + "."

		# Finally deal with Identification and Descriptive questions
		if ((query_object.query_type == QueryFrame.QueryType.IDENT) or (query_object.query_type == QueryFrame.QueryType.DESCR)):
			# for these types just change the response object to english
			if len(response_object) == 1:
			   # simply change the single response into english
			   resp = response_object[0]
			   arg0_list = entities_to_english_list(resp[0], 'name')
			   arg1_list = entities_to_english_list(resp[2], 'name')
			   if len(resp[0]) == 0:

			   if resp[3] > self.cert_threashold:
				   if len(resp[0]) == 1:
					   if number_correction:
						   return ("Just " + arg0_list + " is " + resp[1] + " " + arg1_list + ".")
					   else:
						   return (arg0_list.capitalize() + " is " + resp[1] + " " + arg1_list + ".")
				   else:
					   if number_correction:
						   return (arg0_list.capitalize() + " are all " + resp[1] + " " + arg1_list + ".")
					   else:
						   return (arg0_list.capitalize() + " are " + resp[1] + " " + arg1_list + ".")
			   else:
				   if len(resp[0]) == 1:
					   if number_correction:
						   return ("Maybe just " + arg0_list + " is " + resp[1] + " " + arg1_list + ".")
					   else:
						   return (arg0_list.capitalize() + " may be " + resp[1] + " " + arg1_list + ".")
				   else:
					   if number_correction:
						   return ("Maybe " + arg0_list + " are all " + resp[1] + " " + arg1_list + ".")
					   else:
						   return (arg0_list.capitalize() + " all may be " + resp[1] + " " + arg1_list + ".")

			if len(response_object) == 2 and response_object[1][3] == -1:
			   # here we have one that was mixed certainty... Note that we know the relation is the same for both
			   resp0 = response_object[0]
			   resp1 = response_object[1]
			   arg0_list0 = entities_to_english_list(resp0[0], 'name')
			   arg1_list0 = entities_to_english_list(resp0[2], 'name')
			   arg0_list1 = entities_to_english_list(resp1[0], 'name')
			   if len(resp[0]) == 1:
				   if number_correction:
					   return ("Just " + arg0_list0 + " is " + resp[1] + " " + arg1_list0 + " for sure, but " + arg0_list1 + " may be as well.")
				   else:
					   return (arg0_list.capitalize() + " is " + resp[1] + " " + arg1_list0 + " for sure, but " + arg0_list1 + " might be too.")
			   else:
				   if number_correction:
					   return (arg0_list.capitalize() + " are all " + resp[1] + " " + arg1_list + " certainly, and " + arg0_list1 + " might be too")
				   else:
					   return (arg0_list.capitalize() + " are " + resp[1] + " " + arg1_list + " certainly, and " + arg0_list1 + " might be too")

			else:
			   # Here the best we can do is list them out
			   out = ""
			   for resp in response_object:
				   arg0_list = entities_to_english_list(resp[0], 'name')
				   arg1_list = entities_to_english_list(resp[2], 'name')
				   if resp[3] > self.cert_threashold:
					   if len(resp[0]) == 1:
						   if number_correction:
							   out += ("Just " + arg0_list + " is " + resp[1] + " " + arg1_list + ".")
						   else:
							   out += (arg0_list.capitalize() + " is " + resp[1] + " " + arg1_list + ".")
					   else:
						   if number_correction:
							   out += (arg0_list.capitalize() + " are all " + resp[1] + " " + arg1_list + ".")
						   else:
							   out += (arg0_list.capitalize() + " are " + resp[1] + " " + arg1_list + ".")
				   else:
					   if len(resp[0]) == 1:
						   if number_correction:
							   out += ("Maybe just " + arg0_list + " is " + resp[1] + " " + arg1_list + ".")
						   else:
							   out += (arg0_list.capitalize() + " may be " + resp[1] + " " + arg1_list + ".")
					   else:
						   if number_correction:
							   out += ("Maybe " + arg0_list + " are all " + resp[1] + " " + arg1_list + ".")
						   else:
							   out += (arg0_list.capitalize() + " all may be " + resp[1] + " " + arg1_list + ".")
				   out += "  "
			   return out.strip()




	# takes an entity list and returns an english list with no excess whitespace
	# the list must be of length at least 1
	# ents is the answer set, attribute is the attribute that should be listed
	def entities_to_english_list(self, ents, attribute):
		types = list(map(lambda x: x.type_structure[:-1][-1], ents))
		attribs = list(map(operator.attrgetter(attribute), ents))
		uniform_type = all(x == types[0] for x in types)

		if len(ents) == 1:
			if types[0].lower() == "table" and attribute == "name":
				return 'the ' + attribs[0]
			return 'the ' + attribs[0] + ' ' + types[0]
		elif len(ents) == 2:
			return 'the ' + attribs[0] + ' ' + types[0] + ' and the ' + attribs[1] + ' ' + types[1]
		else:
			if uniform_type:
				out = 'the ' + attribs[0]
				for i in range(1, len(ents)-1):
					out += ', ' + attribs[i]
				out += ', and ' + attribs[-1] + ' ' + types[-1] + 's'
				return out
			else:
				out = 'the ' + attribs[0] + ' ' + types[0]
				for i in range(1, len(ents)-1):
					out += ', ' + attribs[i] + ' ' + types[i]
				out += ', and ' + attribs[-1] + ' ' + types[-1]
				return out

	# use this when we only want colors, without types or determiners
	def entities_to_color_list(self, ents):
		# similar to entities_to_english_list here
		types = map(lambda x: x.type_structure[:-1][0], ents)
		cols = map(operator.attrgetter('color'), ents)
		cols = list(dict.fromkeys(cols))
		uniform_type = all(x == type[0] for x in type)

		# there's only one answer
		if len(cols) == 1:
			return cols[0]

		if len(cols) == 2:
			return cols[0] + " and " + cols[1]

		else:
			out = ""
			for i in range(0, (len(cols) - 1)):
				out += cols[i] + ", "
			out += " and " + cols[-1]
			return out

	def pluralize(self, type):
		return type += "s"

rg = ResponseGenerator()

#rg.generate_response(None, [(["Twitter"], "below", ["Toyota"], 0.8)])

#rg.generate_response(None, [(["Twitter"], "below", ["Toyota"], 0.8), (["McDonalds"], "below", ["Toyota"], 0.6), (["Chucky's"], "below", ["Toyota"], 0.6)])

#rg.generate_response(None, [(["Twitter"], "below", ["Toyota"], 0.8), (["McDonalds"], "below", ["Toyota"], 0.8)])

#rg.generate_response(None, [(["Twitter"], "below", ["Toyota"], 0.8), (["Twitter"], "above", ["Toyota"], 0.8)])

#rg.generate_response(None, [(["Twitter"], "below", ["Toyota"], 0.8), (["Twitter"], "below", ["McDonalds"], 0.8)])









