import numpy as np
import heapq

class Decomposer:
	def __init__(self, mesh):
		self.mesh = mesh
		self.curvatures = {}

		self.mesh_curvature()
		# vects = {}
		# for p1 in mesh:

		# 	for p2 in mesh:
		# 		if p1 != p2:


	def curvature(self, vertex, neighborhood):
		vects = [neighbor - vertex for neighbor in neighborhood]
		return np.average(vects)

	def get_neighbors(self, vertex):
		neighbors = []
		for v in self.mesh:
			if v != vertex:
				heapq.heappush((np.linalg.norm(v - vertex), v))

		return heapq.nsmallest(5, neighbors)

	def mesh_curvature(self):		
		for vertex in self.mesh:
			self.curvatures[vertex] = self.curvature(vertex, self.get_neighbors(vertex))