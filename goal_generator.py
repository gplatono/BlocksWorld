import utils

class GoalGenerator:
	def __init__(self, schema):
		self.goal_schema = self.generate(schema)

		#print (self.goal_schema)

	def generate(self, schema):
		schema = utils.lisp_to_pylist(schema)
		self.header = []
		self.types = []
		self.rigid_conds = []
		self.prototypes = []
		marker = None
		for item in schema:
			if item == ':header' or item == ':types':
				marker = item
			elif marker == ':header':
				pass
		schema = "(rel-schema (bw-pyramid.n ?s) \
  					:vars ?x ?y ?z ?u ?v \
  					:types (?x block) (?y block) (?z block) (?u block) (?v block)  \
  					:rigid-conds \
  					(on ?x table) (on ?y table) (on ?z table) (on ?u ?x) (on ?u ?y) (on ?v ?y) (on ?v ?z) :end)"
		return schema

	def get_goal(self):
		return self.goal_schema

gen = GoalGenerator('(obj-schema (?x BW-row.n) \
 :types \
   !t0 (?x row-of.n \'BW-block.n) \
 :skeletal-prototype \
   bw-row1.obj \
   bw-row2.obj \
   bw-row3.obj)')