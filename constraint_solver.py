import spatial
import itertools
#from ulf_parser import *
from ulf_grammar import *
import numpy as np
import math

global_entities = []
world = None

def ident(arg1, arg2):
	return int(arg1 is arg2)

def exist(arg):
	return int(arg is not None)

def color_pred(entity):
	if hasattr(entity, 'color_mod'):
		return entity.color_mod
	elif hasattr(entity, 'color'):
		return entity.color
	return None


def blue(arg):
	return hasattr(arg, 'color_mod') and arg.color_mod.lower() == "blue"

def red(arg):
	return hasattr(arg, 'color_mod') and arg.color_mod.lower() == "red"

def green(arg):
	return hasattr(arg, 'color_mod') and arg.color_mod.lower() == "green"

def yellow(arg):
	return hasattr(arg, 'color_mod') and arg.color_mod.lower() == "yellow"

def black(arg):
	return hasattr(arg, 'color_mod') and arg.color_mod.lower() == "black"

def orange(arg):
	return hasattr(arg, 'color_mod') and arg.color_mod.lower() == "orange"
# def leftmost(args):
# 	return process_predicate(spatial.to_the_left_of_deic, args, None)

# def rightmost(args):
# 	return process_predicate(spatial.to_the_right_of_deic, args, None)

# def topmost(args):
# 	return process_predicate(spatial.on, args, None)

#Dictionary that maps the relation names to the names of the functions that implement them
func_to_rel_map = {
	spatial.on: 'on',	
	spatial.to_the_left_of_deic: 'to the left of',	
	spatial.to_the_right_of_deic: 'to the right of',
	spatial.near: 'near',
	spatial.above: 'above',
    spatial.below: 'below',
    spatial.over: 'over',
    spatial.under: 'under',
    spatial.inside: 'in',    
    spatial.touching: 'touching',
    spatial.at: 'next to',        
    spatial.higher_than: 'high',
    spatial.lower_than: 'low',
    spatial.in_front_of_deic: 'in front of',
    spatial.behind: 'behind',
    spatial.clear: 'clear',
    spatial.where: 'where',
    spatial.supporting: 'supporting',
    exist: 'exist',
    spatial.facing: 'facing',
    color_pred: 'color',
    blue: 'blue',
    red: 'red',
    green: 'green',
    ident: 'is',
    spatial.between: 'between',
    spatial.in_front_of: 'in front of',
}

rel_to_func_map = {
	'on.p': spatial.on,
	'on': spatial.on,	

	'to_the_left_of.p': spatial.to_the_left_of_deic,
	'left.a': spatial.to_the_left_of_deic,
	'leftmost.a': spatial.to_the_left_of_deic,
	'to_the_right_of.p': spatial.to_the_right_of_deic,
	'right.a': spatial.to_the_right_of_deic,
	'rightmost.a': spatial.to_the_right_of_deic,
	'right.p': spatial.to_the_right_of_deic,
    'left.p': spatial.to_the_left_of_deic,
    
	'near.p': spatial.near,
	'near_to.p': spatial.near,
	'close_to.p': spatial.near,
	'close.a': spatial.near,
	'on.p': spatial.on,
	'on_top_of.p': spatial.on,
	'above.p': spatial.above,
    'below.p': spatial.below,
    'over.p': spatial.over,
    'under.p': spatial.under,    
    'underneath.p': spatial.under,    
    'supporting.p': spatial.supporting,

    'in.p': spatial.inside,
    'in': spatial.inside,    
    'inside.p': spatial.inside,

    'touching.p': spatial.touching,
    'touch.v': spatial.touching,
    'adjacent_to.p': spatial.touching,
    
    'at.p': spatial.at,    
    'next_to.p': spatial.at,
    
    'high.a': spatial.higher_than,
    'upper.a': spatial.higher_than,
    'highest.a': spatial.higher_than,
    'topmost.a': spatial.higher_than,
    'top.a': spatial.higher_than,
    'low.a': spatial.lower_than,
    'lowest.a': spatial.lower_than,    

    'in_front_of.p': spatial.in_front_of,
    'front.a': spatial.in_front_of,
    'frontmost.a': spatial.in_front_of,
    
    'behind.p': spatial.behind,
    'backmost.a': spatial.behind,
    'back.a': spatial.behind,
    'farthest.a': spatial.behind,
    'far.a': spatial.behind,
    'between.p': spatial.between,
    'clear.a': spatial.clear,
    'where.a': spatial.where,
    'exist.pred': exist,

    'face.v': spatial.facing,
    'facing.p': spatial.facing,

    'color.pred': color_pred,

    'blue.a': blue,
    'red.a': red,
    'green.a': green,

    'blue': blue,
    'red': red,
    'green': green
}

#Dictionary mapping the predicates to the number of their arguments
arity = {
	spatial.on: 2,
	spatial.to_the_left_of_deic: 2,
	spatial.to_the_right_of_deic: 2,
	spatial.near: 2,
	spatial.above: 2,
    spatial.below: 2,
    spatial.over: 2,
    spatial.under: 2,
    spatial.inside: 2,
    spatial.touching: 2,    
    spatial.at: 2,
    spatial.in_front_of: 2,    
    spatial.behind: 2,
    spatial.between: 3,
    spatial.clear: 1,	
	spatial.higher_than: 2,
	spatial.lower_than: 2,
	spatial.where: 1,
	spatial.facing: 2,
	spatial.supporting: 2,
	ident: 2,
	exist: 1,
	color_pred: 1,

	blue:1,
	red:1,
	green:1
    }

#Returns the sublist of the entity list having the specified color
def filter_by_color(entities, color):
	ret_val = [] if entities == [] or entities is None \
			else [entity for entity in entities if entity.color_mod == color]
	return ret_val

#Returns the list of entities having the specified type
def filter_by_type(entities, type_id):	
	print ("TYPE PROCESSING: ", type_id)
	ret_val = [] if entities == [] or entities is None \
			else [entity for entity in entities if type_id in entity.type_structure or type_id == 'one']
	return ret_val	

#Returns the list of entities having the specified type
def filter_by_name(entities, name):
	ret_val = [] if entities == [] or entities is None \
			else [entity for entity in entities if entity.type_structure[-1].lower().replace("'", "") == name.lower().replace("'", "")]
	return ret_val
	
#Returns the list of pairs (relatum, referent) such that the given relation holds
#between them (above the threshold)
def filter_relation_by_threshold(relatums, relation, referents, threshold):
        ret_val = []
        for rel in relatums:
                for ref in referents:
                        if relation(rel, ref) >= threshold:
                                ret_val += [(rel, ref)]
        return ret_val

def filter_by_relation(relatums, relation, referents, modifier=None):
	ret_val = []
	if modifier in ['fully.adv-a', 'directly.adv-a', 'very.adv-a', 'fully.mod-a', 'directly.mod-a', 'very.mod-a']:
		return filter_relation_by_threshold(relatums, relation, referents, 0.9)
	elif modifier in ['slightly.adv-a', 'slightly.mod-a', 'marginally.adv-a']:
		return filter_relation_by_threshold(relatums, relation, referents, 0.5)
	elif modifier in ['halfway.adv-a', 'halfway.mod-a']:
		return filter_relation_by_threshold(relatums, relation, referents, 0.7)
	else:
		return filter_relation_by_threshold(relatums, relation, referents, 0.5)

def form_arg_tuples(args, arity):	
	cert_dict = {}
	for item in args:
		cert_dict[item[0]] = item[1]
	bare_args = [item[0] for item in args]	
	tuples = list(itertools.combinations(bare_args, arity))
	ret_val = [(tup, np.average([cert_dict[item] for item in tup])) for tup in tuples]
	# print ("TUPLES: ", arity)
	# print ("BARE ARGS: ", bare_args)
	# print ("TUPLE LIST: ", tuples)
	# print ("ARGS: ", args)
	# print ("PROC ARGS: ", ret_val)
	return ret_val

def filter_by_numeral(numeral, entities):
	"""
	Return the list of tuples of entities
	determined by the numeral.

	"""
	print ("NUMERAL: ", numeral)
	num = 1
	if numeral.content == "two.d" or numeral.content == "two.a":
		num = 2
	elif numeral.content == "three.d" or numeral.content == "three.a":
		num = 3
	elif numeral.content == "four.d" or numeral.content == "four.a":
		num = 4

	# ret_val = list(itertools.combinations(entities, num))
	# ret_val = [(arg, 1.0) for arg in ret_val]
	ret_val = form_arg_tuples(entities, num)

	return ret_val

def filter_by_determiner(entities, modifier):
	ret_val = entities
	if modifier.content == "every.d" or modifier.content == "each.d":
		arg = tuple([arg[0] for arg in entities])
		arg_val = np.average([arg[1] for arg in entities])
		ret_val = [(arg, arg_val)]
	print ("DET PROCESSING: ", ret_val)
	return ret_val

# def compute_predicate(predicate, *arglists):	
# 	arg_combinations = list(itertools.product(*arglists))	
# 	print ("ARG_LISTS: ", arglists, *arglists, arg_combinations)
# 	if arg_combinations is None or arg_combinations == [] or len(arg_combinations[0]) != arity[predicate]:
# 		return []
# 	predicate_values = [(arg, predicate(*arg)) for arg in arg_combinations]	
# 	return predicate_values

def compute_predicate(predicate, relata, referents):
	"""
	Compute the values of the specified predicate for each combination of 
	arg0, arg1.

	Return:
	The list of tuples of the form ((arg0, arg1), predicate_value),
	where predicate_value is the value for that pair od arguments

	"""
	predicate_values = []
	#print("\n\n\n")
	#print ("RELATA: ", relata)
	#print ("\nREFS: ", referents)

	for rel in relata:
		if type(rel[0]) != tuple:
			rel = ((rel[0],), rel[1])
		rel_tuples = list(itertools.combinations(rel[0], r = 1))
		rel_cert = rel[1]
		if referents is not None and referents != []:
			for ref in referents:				
				if type(ref[0]) != tuple:
					ref = ((ref[0],), ref[1])

				if predicate != ident and len(set(rel[0]).intersection(set(ref[0]))) > 0:					
					print ("INTERSECT: ", rel, ref)
					if len(ref[0]) < 5:
						continue
					else:
						filtered = tuple([item for item in ref[0] if item not in rel[0]])
						print ("FILTERED: ", filtered)
						ref = (filtered, ref[1])
				ref_tuples = list(itertools.combinations(ref[0], r = arity[predicate] - 1))
				ref_cert = ref[1]
				print ("REL TUPLES: ", rel_tuples)
				print ("REF TUPLES: ", ref_tuples)
				arg_combinations = list(itertools.product(rel_tuples, ref_tuples))
				#print ("ARG_COMBINATIONS: ", arg_combinations)
				if predicate != ident:
					arg_combinations = [(*arg0, *arg1) for (arg0, arg1) in arg_combinations if arg0[0] not in arg1]
				else:
					arg_combinations = [(*arg0, *arg1) for (arg0, arg1) in arg_combinations]
				print ("ARG_COMBINATIONS1: ", arg_combinations)
				pred_value = np.average([predicate(*arg) for arg in arg_combinations])
				if math.isnan(pred_value):
					pred_value = 0
				predicate_values.append(((*rel[0], *ref[0]), pred_value))
		else:
			arg_combinations = rel_tuples
			pred_value = np.average([predicate(*arg) for arg in arg_combinations])
			predicate_values.append(((*rel[0], None), pred_value))

	#predicate_values = [(arg, predicate(*arg)) for arg in arg_combinations]
	predicate_values.sort(key = lambda x: x[1], reverse=True)
	
	pred_name = func_to_rel_map[predicate]
	ret_val = [([item[0][0]], pred_name, list(item[0][1:]), item[1]) for item in predicate_values]
	return predicate_values, ret_val

def filter_by_predicate_modifier(predicate_values, modifier):
	"""Return the subset of entities that satisfy the given predicate modifier."""
	if modifier.content is not None and modifier.content != "":
		modifier = modifier.content

	print ("MODIFIER CONTENT: ", modifier)

	if modifier in ['fully.adv-a', 'directly.adv-a', 'very.adv-a', 'fully.mod-a', 'directly.mod-a', 'very.mod-a']:
		return [(arg, val) for (arg, val) in predicate_values if val >= 0.9]
	elif modifier in ['slightly.adv-a', 'slightly.mod-a', 'marginally.adv-a']:
		return [(arg, val) for (arg, val) in predicate_values if val >= 0.5]
	elif modifier in ['halfway.adv-a', 'halfway.mod-a']:
		return [(arg, val) for (arg, val) in predicate_values if val >= 0.8]
	elif type(modifier) == TNeg or modifier in ['not.adv-s', 'not.adv-a', 'not.mod-a']:
		return [(arg, 1 - val*0.3/0.7) for (arg, val) in predicate_values if val < 0.7]
	elif type(modifier) == TSuperMarker:
		#predicate_values.sort(key = lambda x: x[1])
		arg, val = predicate_values[0]
		if val > 0.7 or (len(predicate_values) > 1 and val > 1.1 * predicate_values[1][1]):
			return [(arg, 1.0)]
		else:
			return []
	else:
		return [(arg, val) for (arg, val) in predicate_values if val >= 0.7]

def filter_by_mod(entities, modifier, entity_list):
	if type(modifier) == NColor:
		items = [entity[0] for entity in entities]
		ret_val = filter_by_color(items, modifier.content)
		return [(item, 1.0) for item in ret_val]
	elif type(modifier) == TNumber:		
		ret_val = filter_by_numeral(modifier, entities)
		return ret_val
	elif type(modifier) == TDet:
		return filter_by_determiner(entities, modifier)
	elif type(modifier) == TAdj or type(modifier) == NPred:
		print ("ADJ PROCESSING... PRED = ", modifier, " RELATA = ", entities)
		#pred = NPred(content = modifier.content, mods = modifier.mods)
		predicate_values = process_predicate(modifier, relata=entities, entity_list=entity_list)

		#Now extract the arg0
		answer_set = {}
		for (item, val) in predicate_values:
			if item[0] not in answer_set.keys():
				answer_set[item[0]] = 0
			answer_set[item[0]] = max(answer_set[item[0]], val)
		
		answer_set = [(key, answer_set[key]) for key in answer_set.keys()]
		return answer_set				

def process_predicate(predicate, relata=None, referents=None, entity_list=None):
	"""
	Processes the predicate by computing its values over all the combinations
	of arguments and then appyling each modifiers to obtain more and more
	restricted list of argument tuples that satisfy the constraints of the
	predicate.

	"""
	#print ("ENTERING PREDICATE PROCESSING: ", predicate)
	predicate_func = resolve_predicate(predicate)
	pred_arity = arity[predicate_func]
	modifiers = predicate.mods
	print ("\nPREDICATE COMPONENTS: ", predicate, modifiers)
	unique = False
	unary = False

	#Resolve arguments
	print ("ARGS BEFORE RESOLVING: ", relata, referents)
	if relata is None:
		relata = resolve_argument(predicate.children[0], entity_list) if len(predicate.children) > 0 else None
		if referents is None:
			referents = resolve_argument(predicate.children[1], entity_list) if len(predicate.children) > 1 else None
	elif referents is None:
		referents = resolve_argument(predicate.children[0], entity_list) if predicate.children is not None and len(predicate.children) > 0 else None
	# print ("RESOLVED RELATA:", relata)
	# print ("RESOLVED REFERENTS:", referents)
	
	#For handling "Where" and color requests
	if predicate_func == spatial.where:		
		predicate_values = [spatial.where(relatum[0]) for relatum in relata]
		print ("WHERE PRED VALUES: ", predicate_values)
		return predicate_values
	elif predicate_func == color_pred:
		predicate_values = [color_pred(relatum[0]) for relatum in relata]
		return predicate_values

	#For superlatives	
	if (referents is None or referents == []) and pred_arity == 2:
		unique = True
		unary = True
		referents = [(tuple(world.active_context), 1.0)]
		
	print ("FINAL_RELATA: ", relata)
	print ("FINAL_REFERENTS: ", referents)
	predicate_values, ret_val = compute_predicate(predicate_func, relata, referents)

	if unary:
		predicate_values = [((arg[0],), val) for (arg, val) in predicate_values]
	
	print ("PREDICATE VALUES: ", predicate_values)	
	if modifiers is not None and modifiers != []:
		for modifier in modifiers:
			predicate_values = filter_by_predicate_modifier(predicate_values, modifier)
	elif predicate_values is not None and predicate_values != []:
		if unique == False:
			predicate_values = [(arg, val) for (arg, val) in predicate_values if val >= 0.7]
		else:
			if len(predicate_values) > 1:
				if predicate_values[0][1] > predicate_values[1][1] + 0.2:
					predicate_values = [predicate_values[0]]
				else:
					predicate_values = []

			#predicate_values = [predicate_values[0]]
		
	print ("FINAL ARGLISTS RETURNED FROM PRED: ",  predicate_values)

	
	#from response_generator import ResponseGenerator
	#resp = ResponseGenerator()
	#resp = resp.generate_response(ret_val)
	#print ("RET_VAL", ret_val)
	#print ("RESP", resp)
	return predicate_values

def resolve_argument(arg_object, entities):
	ret_args = entities

	if type(arg_object) == NArg and (arg_object.obj_type is None or arg_object.obj_id is None or arg_object.obj_type.lower() != "table" or arg_object.obj_id.lower() != "table"):
		entities = [item for item in entities if "table" not in item.type_structure]

	print ("RESOLVING THE ARGUMENT...", arg_object)

	if type(arg_object) == NConjArg:
		args1 = resolve_argument(arg_object.children[0], entities)
		args2 = resolve_argument(arg_object.children[1], entities)
		ret_args = []
		for arg1 in args1:
			for arg2 in args2:
				ret_args.append(((arg1[0], arg2[0]), (arg1[1]+arg2[1]) / 2))
		return ret_args

	arg_type = arg_object.obj_type
	arg_id = arg_object.obj_id
	arg_det = arg_object.det
	arg_plur = arg_object.plur
	arg_mods = arg_object.mods
	#arg_mods.reverse()

	#print (arg_object)
	#print ("ARG CANDIDATES: ", entities)
	if arg_type is not None:
		ret_args = filter_by_type(ret_args, arg_type)

	#print ("AFTER TYPE RESOLUTION:", ret_args)

	if arg_id is not None:
		ret_args = filter_by_name(ret_args, arg_id)

	#print ("AFTER NAME RESOLUTION:", ret_args)

	if ret_args is not None and ret_args != []:
		if type(ret_args[0]) != tuple:
			ret_args = [(item, 1.0) for item in ret_args]

	print ("\nARGS BEFORE MODIFIERS: ", ret_args, arg_mods)
	if arg_mods is not None and arg_mods != []:
		for modifier in arg_mods:
			print ("CURRENT MOD: ", modifier)
			ret_args = filter_by_mod(ret_args, modifier, entities)
			print ("CURRENT ARGS: ", ret_args)

	print ("\nAFTER MOD APPLICATION:", ret_args)

	if arg_det is not None:
		ret_args = filter_by_determiner(ret_args, arg_det)
	print ("\nFINAL RESOLVED ARGS: ", ret_args)	
	return ret_args

def resolve_predicate(predicate_object):	
	if type(predicate_object) == str:
		pred = rel_to_func_map[predicate_object]
	elif predicate_object.content is not None and type(predicate_object.content) == str:
		pred = rel_to_func_map[predicate_object.content]
	else:
		if type(predicate_object.content) == TCopulaBe:
			pred = ident
		else:			
			pred = rel_to_func_map[predicate_object.content.content]
	print ("RESOLVING PREDICATE...", pred)
	return pred

def process_query(query, entities):
	#if type(query) != NSentence or (not query.is_question) or query.content == None:
	#	return None	
	relata = []
	referents = []
	if query.predicate is not None:#type(arg) == NRel or type(arg) == NPred:
		print ("\nENTERING NPRED PROCESSING...")
		pred = query.predicate

		predicate_values = process_predicate(pred, entity_list=entities)
		if query.query_type == query.QueryType.DESCR or query.query_type == query.QueryType.ATTR_COLOR:
			return predicate_values		
		if predicate_values is not None and predicate_values != []:
			relata = [(arg[0], val) for (arg, val) in predicate_values]
		
			if len(predicate_values[0][0]) > 1:
				referents = [(arg[1:], val) for (arg, val) in predicate_values]
				if referents is not None and len(referents) > 0:
					referents.sort(key = lambda x: x[1], reverse = True)
					if query.arg1_singular:
						referents = [referents[0]]
			
	elif query.arg is not None:#type(arg) == NArg:
		print ("\nENTERING TOP LEVEL ARG PROCESSING...")
		arg = query.arg
		relata = resolve_argument(arg, entities)
		print ("RESOLVED RELATA:", relata)		
		if relata != None and relata != []:
			if type(relata[0]) != tuple:
				relata = [(item, 1.0) for item in relata]
	else:
		return "FAIL"

	relata.sort(key = lambda x: x[1], reverse = True)

	#Remove duplicates
	encountered_relata = []
	filtered_relata = []
	for (arg, val) in relata:
		if arg not in encountered_relata:
			encountered_relata.append(arg)
			filtered_relata.append((arg, val))
	relata = filtered_relata

	print ("IS ARG0 SINGULAR? ", query.arg0_singular)
	if len(relata) > 0 and query.arg0_singular:		
		relata = [relata[0]]

	print ("RETURNED RESOLVED ARG0, ARG1 ", relata, referents)
	print ("ACTIVE CONTEXT: ", world.active_context)

	# if query.arg is not None and query.arg.obj_type is not None and query.arg.obj_type.lower() != "table" or\
	# 	query.relatum is not None and query.relatum.obj_type is not None and query.relatum.obj_type.lower() != "table":
	relata = [item for item in relata if "table" not in item[0].type_structure]
	return relata, referents