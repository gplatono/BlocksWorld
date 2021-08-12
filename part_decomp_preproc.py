import bpy

filepath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, filepath)

from part_decomposition import Decomposer

scene = bpy.context.scene
mesh = []

for obj in bpy.data.objects:
	vertices = [obj.matrix_world @ v.co for v in obj.data.vertices]
	mesh += vertices

decomp = Decomposer(mesh)