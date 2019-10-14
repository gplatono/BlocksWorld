import bpy
import os
import sys

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