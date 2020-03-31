import bpy
import bpy_types
import bpy_extras
import numpy as np
import math
from math import e, pi
import itertools
import os
import sys
import random
import bmesh
from functools import reduce
from mathutils import Vector
import importlib
from threading import Thread
import multiprocessing
import time

#The path to the this source file
filepath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, filepath)

from entity import Entity
from geometry_utils import *
#from annot_parser import *
import spatial
from ulf_parser import ULFParser
import constraint_solver
from world import World
from hci_manager import HCIManager
from query_frame import QueryFrame
#from bw_tracker import Tracker
#from query_proc import *

link = False
#The current scene
scene = bpy.context.scene

conf_list = open("config").readlines()
settings = {}
for line in conf_list:
	setting = line.split("=")[0]
	val = line.split("=")[1]
	if setting == "DEBUG_MODE" or setting == "SIMULATION_MODE":
		val = (val == "1")
	settings[setting] = val

#List of  possible color modifiers
color_mods = ['black', 'red', 'blue', 'brown', 'green', 'yellow']

relations = ['on', 'to the left of', 'to the right of', 'in front of', 'behind', 'above', 'below', 'over', 'under', 'near', 'touching', 'at', 'between']

types_ids = {
	'chair':  'props.item.furniture.chair',
	'table':  'props.item.furniture.table',    
	'bed':     'props.item.furniture.bed',
	'sofa':  'props.item.furniture.sofa',
	'bookshelf':  'props.item.furniture.bookshelf',
	'desk':  'props.item.furniture.desk',
	'book': 'props.item.portable.book',
	'laptop': 'props.item.portable.laptop',
	'pencil': 'props.item.portable.pencil',
	'pencil holder': 'props.item.portable.pencil holder',
	'note': 'props.item.portable.note',
	'rose': 'props.item.portable.rose',
	'vase': 'props.item.portable.vase',
	'cardbox': 'props.item.portable.cardbox',
	'box': 'props.item.portable.box',
	'ceiling light': 'props.item.stationary.ceiling light',
	'lamp': 'props.item.portable.lamp',
	'apple': 'props.item.food.apple',
	'banana': 'props.item.food.banana',
	'plate': 'props.item.portable.plate',
	'bowl': 'props.item.portable.bowl',
	'trash bin': 'props.item.portable.trash can',
	'trash can': 'props.item.portable.trash can',
	'tv': 'props.item.appliances.tv',
	'poster': 'props.item.stationary.poster',
	'picture': 'props.item.stationary.picture',
	'fridge' : 'props.item.appliances.fridge',
	'ceiling fan': 'props.item.stationary.ceiling fan',
	'block': 'props.item.block',
	'floor': 'world.plane.floor',
	'ceiling': 'world.plane.ceiling',
	'wall': 'world.plane.wall'
}

#Checks if the entity "collides" (overlaps) with some other entity along any coordinate axis
#Inputs: a - entity
#Return value: Boolean value                
def check_collisions_for_entity(a):
	for entity in entities:
		if entity != a and check_collision(a, entity):            
			return True
	return False            

#Checks if two entities "collide" (overlap) along some coordinate axis
#Inputs: a,b - entities
#Return value: Boolean value                
def check_collision(a, b):
	span_a = a.get_span()
	span_b = b.get_span()
	ax_1 = span_a[0][0]
	ax_2 = span_a[0][1]
	ay_1 = span_a[1][0]
	ay_2 = span_a[1][1]
	az_1 = span_a[2][0]
	az_2 = span_a[2][1]
	bx_1 = span_b[0][0]
	bx_2 = span_b[0][1]
	by_1 = span_b[1][0]
	by_2 = span_b[1][1]
	bz_1 = span_b[2][0]
	bz_2 = span_b[2][1]
	return (ax_1 <= bx_1 and bx_1 < ax_2 or bx_1 <= ax_1 and ax_1 < bx_2) or \
	(ay_1 <= by_1 and by_1 < ay_2 or by_1 <= ay_1 and ay_1 < by_2) or \
	(az_1 <= bz_1 and bz_1 < az_2 or bz_1 <= az_1 and az_1 < bz_2)
	#return axis_collision((span_a[0], span_a[1]), (span_b[0], span_b[1])) and \
	#                      axis_collision((span_a[2], span_a[3]), (span_b[2], span_b[3])) and \
	#                      axis_collision((span_a[4], span_a[5]), (span_b[4], span_b[5]))


#Render and save the current scene screenshot
#Inputs: none
#Return value: none
def save_screenshot():
	add_props()
	scene.render.resolution_x = 1920
	scene.render.resolution_y = 1080
	scene.render.resolution_percentage = 100
	scene.render.use_border = False
	scene.render.image_settings.file_format = 'JPEG'
	current_scene = bpy.data.filepath.split("/")[-1].split(".")[0]
	scene.render.filepath = filepath + current_scene + ".jpg"
	bpy.ops.render.render(write_still=True)




def get_weighted_measure(a, b, observer):
	a_bbox = get_2d_bbox(vp_project(a, observer))
	b_bbox = get_2d_bbox(vp_project(b, observer))
	axial_dist = scaled_axial_distance(a_bbox, b_bbox)
	if axial_dist[0] <= 0:
		return 0
	horizontal_component = sigmoid(axial_dist[0], 1.0, 0.5) - 0.5
	vertical_component = gaussian(axial_dist[1], 0, 2.0)
	distance_factor = math.exp(-0.01 * axial_dist[0])
	return 0.5 * horizontal_component + 0.3 * vertical_component + 0.2 * distance_factor


def fix_ids():
	for ob in scene.objects:
		if ob.get('main') is not None:# and ob.get('id') is None:
			for key in types_ids.keys():
				if key in ob.name.lower():
					ob['id'] = types_ids[key] + "." + ob.name
			if ob.get('color_mod') is None:
				for color in color_mods:
					if color in ob.name.lower():
						ob['color_mod'] = color
						break

def get_similar_entities(relatum):
	ret_val = []
	relatum = get_entity_by_name(relatum, entities)
	for entity in entities:
		if relatum.type_structure[-2] == entity.type_structure[-2] and (relatum.color_mod is None \
		   and entity.color_mod is None or relatum.color_mod == entity.color_mod):
			ret_val += [entity]
	return ret_val

def pick_descriptions(relatum):
	candidates = get_similar_entities(relatum)
	relatum = get_entity_by_name(relatum, entities)
	max_vals = []
	for relation in relations:
		max_val = 0
		if relation != 'between':            
			for ref in entities:
				if ref != relatum:
					max_val = max([(globals()[rf_mapping[relation]](cand, ref), cand) for cand in candidates], key=lambda arg: arg[0])
		else:
			for pair in itertools.combinations(entities, r = 2):
				if relatum != pair[0] and relatum != pair[1]:
					max_val = max([(globals()[rf_mapping[relation]](cand, pair[0], pair[1]), cand) for cand in candidates], key=lambda arg: arg[0])
		if max_val[1] == relatum:
			max_vals += [(relation, max_val[0])]
	max_vals.sort(key=lambda arg: arg[1])
	print ("MAX_VALS: {}".format(max_vals))
	max_vals = [item[0] for item in max_vals]
	return tuple(max_vals[0:3])

def run_debug(world, hci_manager):
	surface_forms = open("sqa_dev_surface.bw").readlines()
	ulfs = open("sqa_dev_ulf.bw").readlines()
	min_len = min(len(ulfs), len(surface_forms))
	surface_forms = surface_forms[:min_len]

	ulf_parser = ULFParser()
 
	global observer
	observer = world.observer
	
	toy = world.find_entity_by_name("Toyota")
	tex = world.find_entity_by_name("Texaco")
	mcd = world.find_entity_by_name("McDonald's")
	sri = world.find_entity_by_name("SRI")
	nvd = world.find_entity_by_name("NVidia")
	stb = world.find_entity_by_name("Starbucks")
	mrc = world.find_entity_by_name("Mercedes")
	bgk = world.find_entity_by_name("Burger King")
	tar = world.find_entity_by_name("Target")
	tbl = world.find_entity_by_name("Table")
	ent = [toy, tex, mcd, bgk, tar, stb, tbl,mrc]         

	for ulf in ulfs:
		idx = ulfs.index(ulf)
		print ("\n" + str(1 + ulfs.index(ulf)) + " out of " + str(len(ulfs)))
		ulf = ulf.lower().strip().replace("{", "").replace("}", "")
		if "row" not in ulf and "stack" not in ulf and "face" not in ulf and "-of" not in ulf and "right.a (red.a block.n)" not in ulf and "and.cc" not in ulf and "most-n" not in ulf:
			query_frame = QueryFrame(surface_forms[idx], ulf, ulf_parser.parse(ulf))
			print (query_frame.raw)
			print ("\n" + ulf + "\n")
			answer_set_rel, answer_set_ref = constraint_solver.process_query(query_frame, world.entities)
			response_surface = hci_manager.generate_response(query_frame, [item[0] for item in answer_set_rel], [item[1] for item in answer_set_rel])
			print ("ANSWER SET: ", answer_set_rel)
			print ("RESPONSE: ", response_surface)
			print ([(bl, spatial.on(bl, mrc)) for bl in ent if bl != tbl])
			#print ([(bl, touching(bl, tbl)) for bl in ent if bl != tbl])           
			#print ([(bl, clear(bl)) for bl in ent])
			#print (extract_contiguous([entity for entity in ent if entity != tbl]))

			print (spatial.get_normal((0, 0, 0), (1,0,0), (0, 1, 0)))
			input("Press Enter to continue...")


#Entry point
def main():
	my_areas = bpy.context.workspace.screens[0].areas
	my_shading = 'MATERIAL'  # 'WIREFRAME' 'SOLID' 'MATERIAL' 'RENDERED'


	for area in my_areas:
		for space in area.spaces:
			if space.type == 'VIEW_3D':
				space.shading.type = my_shading

	print ("Entering the main loop...")
	if '-s' in sys.argv:
		settings['SIMULATION_MODE'] = True

	if '-d' in sys.argv:
		settings['DEVELOPMENT_MODE'] = True

	if '-bo' in sys.argv:
		settings['DEVELOPMENT_MODE'] = True
		settings['SIMULATION_MODE'] = True
		
	 
	world = World(bpy.context.scene, simulation_mode=settings['SIMULATION_MODE'])
	spatial.entities = world.entities
	spatial.world = world
	constraint_solver.world = world
	# world.history.append(world.State(world.entities))
	# for item in world.history:
	#     print (item.state_facts)

	# Toy = world.find_entity_by_name('toyota')
	# Star = world.find_entity_by_name('starbucks')
	# Tex = world.find_entity_by_name('texaco')
	# Tar = world.find_entity_by_name('target')
	# Mrc = world.find_entity_by_name('mercedes')

	#print (Toy.type_structure)

	#from constructor import Constructor
	#cons = Constructor()

	#struct = Entity([Toy, Star])

	#print (spatial.touching(struct, Mrc))
	#cons.sample(Toy, spatial.touching, Star)

	#print (Toy.vertex_set)

	#Toy.move_to(np.array([0, 0, 0]))

	#print ([v.co for v in Toy.components[0].data.vertices])


	if '-bo' not in sys.argv:        
		hci_manager = HCIManager(world, debug_mode = settings['DEBUG_MODE'])

		if hci_manager.debug_mode == True:
			run_debug(world, hci_manager)
			return
	
		hci_thread = Thread(target = hci_manager.start)
		hci_thread.setDaemon(True)
		hci_thread.start()

if __name__ == "__main__":
	#fix_ids()
	#bpy.ops.wm.save_mainfile(filepath=bpy.data.filepath)
	main()
