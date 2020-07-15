from goal_generator import GoalGenerator
import utils
import constraint_solver

class Planner:
	def __init__(self, world):
		self.world = world
		self.plan = []

	def init(self, obj_schema):
		self.schema = obj_schema
		self.goal_generator = GoalGenerator(self.schema)
		self.goal_schema = self.goal_generator.get_goal()
		self.generate_plan()

		print (self.goal_schema)

	def get_goal_schema(self):
		return self.goal_schema

	def generate_plan(self):
		move = [self.world.find_entity_by_name('Toyota'), "on.p", self.world.find_entity_by_name('Table')]
		self.plan = [move]

	def next(self):				
		return utils.rel_to_ulf(self.plan[0])

	def execute(self):
		self.plan.pop(0)

	def update(self):
		pass

	def process_move(self, move):
		arg0 = self.world.find_entity_by_name(move[0])
		rel = self.plan[0][1]
		arg1 = self.plan[0][2]
		question = "(" + arg0.get_ulf() + "((pres be.v) " + rel + " " + arg1.get_ulf() + ")"
		print (question)


# planner = Planner('($ obj-schema \
#  :header (?x BW-row.n) \
#  :types \
#    !t0 (?x row-of.n \'BW-block.n) \
#  :skeletal-prototype \
#    bw-row1.obj \
#    bw-row2.obj \
#    bw-row3.obj)')