import bpy
import os
import sys
import numpy as np
import geometry_utils

filepath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, filepath)

from entity import Entity

class Constructor(object):

	def __init__(self):
		pass

	def construct(self, args, rel_tuples):
		"""
		Combines the elements of args using a set of relations.

		Input: 
		args - list of entities, e.g., [Toyota, McDonalds, ...]
		rel_tuples - list of tuples of the form (arg0, relation, (arg1, {arg2})), where 
		arg0, arg, arg2 are elements of args and arg2 is optional, e.g., 
		[(Toyota, "on_top", (Texaco,)), (Texaco, "between", (Starbucks, McDonalds)), ...]

		"""

		pass

	def sample(self, relatum, relation, referent1=None, referent2=None):
		position = np.array([0, 0, 0])
		val = 0
		region_size = 5		
		iter = 0
		while val < 0.9:
			new_pos = np.random.multivariate_normal(mean = position, cov = (1 - val) * region_size * np.eye(3), size=1)[0]			
			relatum.move_to(new_pos)
			while (referent1 is not None and geometry_utils.intersection_entities(relatum, referent1)) or\
						(referent2 is not None and geometry_utils.intersection_entities(relatum, referent2)):
				new_pos = np.random.multivariate_normal(mean = position, cov = (1 - val) * region_size * np.eye(3), size=1)[0]			
				relatum.move_to(new_pos)
			curr_val = relation(relatum) if referent1 is None else relation(relatum, referent1) if referent2 is None else relation(relatum, referent1, referent2)
			if curr_val > val:
				val = curr_val
				position = new_pos
			iter += 1
			if iter == 500:
				return False
		print ("CONVERGED AFTER " + str(iter) + " ITERATIONS.")
		return True