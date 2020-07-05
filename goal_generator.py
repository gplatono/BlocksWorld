import utils

class GoalGenerator:
	def __init__(self, schema):
		self.goal_schema = self.generate(schema)

		print (self.goal_schema)

	def generate(self, schema):
		schema = utils.lisp_to_pylist(schema)
		self.header = []
		self.types = []
		self.rigid_conds = []
		self.prototypes = []
		for item in schema:
			if item == ':types':
				pass
		return schema

gen = GoalGenerator('(obj-schema (?x BW-row.n) \
 :types \
   !t0 (?x row-of.n \'BW-block.n) \
 :skeletal-prototype \
   bw-row1.obj \
   bw-row2.obj \
   bw-row3.obj)')