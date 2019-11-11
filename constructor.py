import bpy
import os
import sys
import numpy as np
import geometry_utils
import math
from sklearn.metrics.pairwise import cosine_similarity

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

	#returns magnitude of a 3d vector a.
	def get_magnitude(self, a):
		return math.sqrt((a[0] ** 2)+(a[1] ** 2)+(a[2] ** 2))

	#returns the dot products of two 3d vectors a and b.
	def get_dot_product(self, a, b):
		return ((a[0]*b[0])+(a[1]*b[1])+(a[2]*b[2]))

	#function that inputs two vectors a,b and returns "similarity" between the two.
  	#a1, a2, and c are scalers that have yet to be determined.
	def vectorSimilarity(self, a, b, a1, a2, c):
		return (a1 * math.e ** (-c * (get_magnitude(a)- get_magnitude(b)))) + (a2 * cosine_similarity(a,b))

	# returns a value between 0 - 1 for the structure similarity 
	def structureSimilarity(self, a, b):
		z = 0
		for vectora in a:
			x = 0
			for vectorb in b:
				y = vectorSimilarity(vectora , vectorb)
				if(y > x):
					x = y
			z += x
		return z/len(a)