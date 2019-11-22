import bpy
import json
import requests
import random
import time
import bmesh
import math
import numpy as np
#import geometry_utils
from mathutils import Vector
from mathutils import Quaternion
import os
from threading import *
from entity import Entity

print (os.getcwd())

class ModalTimerOp(bpy.types.Operator):    
        #metatags for Blender internal machinery
        bl_idname = "wm.modal_timer_operator"
        bl_label = "Modal Timer Op"
        
        #internal timer
        _timer = None
        tracker = None
        
        #execution step (fires at every timer tick)
        def modal(self, context, event):
            if event.type == "ESC":
                return self.cancel(context)
            elif event.type == "TIMER":
                ModalTimerOp.tracker.moved_blocks = \
                ModalTimerOp.tracker.update(ModalTimerOp.tracker.get_block_data())
                #bpy.context.scene.update()
                bpy.context.evaluated_depsgraph_get().update() 
                #dg.update()
                #print (ModalTimerOp.tracker.moved_blocks)

                time.sleep(0.1)
                for name in ModalTimerOp.tracker.moved_blocks:                    
                    ent = ModalTimerOp.tracker.world.find_entity_by_name(name)
                    # if np.linalg.norm(ent.location - bpy.data.objects[name].location) <= 0.1:
                    #     continue                        
                    old_loc = ent.location                    
                    ModalTimerOp.tracker.world.entities.remove(ent)
                    #ent.update()
                    ent = Entity(bpy.data.objects[name])
                    ModalTimerOp.tracker.world.entities.append(ent)
                    #bpy.context.scene.update()
                    bpy.context.evaluated_depsgraph_get().update()
                    if self.tracker.verbose:
                        print ("ENTITY RELOCATED: ", name, ent.name, np.linalg.norm(old_loc - ent.location))
                        print ("OLD LOCATION: ", old_loc)
                        print ("NEW LOCATION: ", ent.location)
                    
                #world = ModalTimerOp.tracker.world
                #toy = world.find_entity_by_name("Toyota")
                #print (toy.location, toy.size)

            return {"PASS_THROUGH"}
        
        #Setup code (fires at the start)
        def execute(self, context):
            self._timer = context.window_manager.event_timer_add(0.5, window=context.window)
            context.window_manager.modal_handler_add(self)
            #self.flag = False
            return {"RUNNING_MODAL"}
        
        #Timer termination and cleanup
        def cancel(self, context):
            context.window_manager.event_timer_remove(self._timer)
            return {"CANCELLED"}

class Tracker(object):
    
    def __init__(self, world):
        #Sizes of BW objects in meters
        self.block_edge = 1.0#0.155
        self.table_edge = 1.53
        self.bw_multiplier = 1.0 / 0.155

        self.kinectLeft = (-0.75, 0.27, 0.6)
        self.kinectRight = (0.75, 0.27, 0.6)

        bpy.utils.register_class(ModalTimerOp)
        ModalTimerOp.tracker = self

        self.scene_setup()

        self.verbose = False
        self.verbose_rotation = False
        
        block_data = self.get_block_data()
        block_data.sort(key = lambda x : x[1][0])
        for idx in range(len(block_data)):
            id, location, rotation = block_data[idx]
            self.block_to_ids[self.blocks[idx]] = id
            self.block_by_ids[id] = self.blocks[idx]
            self.blocks[idx].location = location
            self.blocks[idx].rotation_euler = rotation

        self.moved_blocks = []
        self.world = world        
        dg = bpy.context.evaluated_depsgraph_get().update()
        #dg.update()
        bpy.ops.wm.modal_timer_operator()
          
    def create_block(self, name="", location=(0,0,0), rotation=(0,0,0), material=None):
        if bpy.data.objects.get(name) is not None:
            bl = bpy.data.objects[name]
            bl.rotation_euler = rotation
            print (bl.rotation_euler)
            return bl
        block_mesh = bpy.data.meshes.new('Block_mesh')
        block = bpy.data.objects.new(name, block_mesh)
        bpy.context.collection.objects.link(block)
        #bpy.context.collection.objects.active = block
        #block.select = True
        bm = bmesh.new()
        bmesh.ops.create_cube(bm, size=self.block_edge)
        bm.to_mesh(block_mesh)
        bm.free()
        block.data.materials.append(material)
        block.location = location
        block.rotation_euler = rotation
        block['id'] = "bw.item.block." + name
        block['color_mod'] = material.name
        block['main'] = 1.0
        #bpy.context.scene.update()
        bpy.context.evaluated_depsgraph_get().update()
        return block
    
    #Reset the scene by removing all the meshes
    def clear_scene(self):
        #iterate over the objects in the scene
        for ob in bpy.data.objects:
            #If it's a mesh select it
            if ob.type == "MESH":
                ob.select = True
        #remove all selected objects
        bpy.ops.object.delete()

    #Adds objects to the scene    
    def scene_setup(self):        
        bpy.data.materials.new(name="red")
        bpy.data.materials.new(name="blue")
        bpy.data.materials.new(name="green")
        bpy.data.materials['red'].diffuse_color = (1, 0, 0, 0)
        bpy.data.materials['green'].diffuse_color = (0, 1, 0, 0)
        bpy.data.materials['blue'].diffuse_color = (0, 0, 1, 0)

        self.block_names = ['Target', 'Starbucks', 'Twitter', 'Texaco', 'McDonald\'s', 'Mercedes', 'Toyota', 'Burger King']
        materials = [bpy.data.materials['blue'], bpy.data.materials['green'], bpy.data.materials['red']]
        # for ob in bpy.data.objects:
        #     print (ob.name, bpy.data.objects.get(ob.name))

        self.blocks = [self.create_block(name, Vector((0, 0, self.block_edge / 2)), (0,0,0), materials[self.block_names.index(name) % 3]) for name in self.block_names]
        self.block_by_ids = {}
        self.block_to_ids = {}
        #self.clear_scene()
        #Creating the materials        
        #bpy.ops.mesh.primitive_plane_add(location=(0,0,0), radius=self.table_edge/2)

    def unoccluded(self, block):
        LeftBlocked = False
        RightBlocked = False
        for key in self.block_dict:
            if self.block_dict[key] != block:
                dist_left = get_distance_from_line(block.location, self.kinectLeft, self.block_dict[key].location)
                dist_right = get_distance_from_line(block.location, self.kinectRight, self.block_dict[key].location)
                if dist_left <= 0.05:
                    LeftBlocked = True
                if dist_right <= 0.05:
                    RightBlocked = True
        return LeftBlocked and RightBlocked
    
    def update(self, block_data):
        #print (block_data)
        moved_blocks = []

        updated_blocks = {}
        unpaired = []

        for block in self.blocks:
            updated_blocks[block] = 0

        for id, location, rotation in block_data:
            if id in self.block_by_ids:
                block = self.block_by_ids[id]              
                rot1 = np.array([item for item in rotation])
                rot2 = np.array([item for item in block.rotation_euler])
                if np.linalg.norm(location - block.location) >= 0.05 or np.linalg.norm(rot1 - rot2) >= 0.05:
                    if self.verbose or self.verbose_rotation:
                        if np.linalg.norm(location - block.location) >= 0.1:
                            print ("MOVED BLOCK: ", block.name, location, block.location, np.linalg.norm(location - block.location))
                        else:
                            print ("ROTATED BLOCK: ", block.name, rotation, block.rotation_euler)
                    moved_blocks.append(block.name)
                    block.location = location
                    block.rotation_euler = rotation
                updated_blocks[block] = 1
            else:
                id_assigned = False
                for block in self.blocks:
                    if np.linalg.norm(location - block.location) < 0.05:
                        if self.verbose:
                            print ("NOISE: ", block.name, location, block.location, np.linalg.norm(location - block.location))
                        self.block_by_ids.pop(self.block_to_ids[block], None)
                        self.block_by_ids[id] = block
                        self.block_to_ids[block] = id
                        block.location = location
                        block.rotation_euler = rotation
                        id_assigned = True
                        updated_blocks[block] = 1
                        moved_blocks.append(block.name)
                        break
                if id_assigned == False:
                    unpaired.append((id, location, rotation))

        for id, location, rotation in unpaired:
            min_dist = 10e9
            cand = None
            for block in self.blocks:
                if updated_blocks[block] == 0:
                    cur_dist = np.linalg.norm(location - block.location)
                    if min_dist > cur_dist:
                        min_dist = cur_dist
                        cand = block
            if cand != None:
                if self.verbose or self.verbose_rotation:
                    if np.linalg.norm(location - cand.location) >= 0.05:
                        print ("MOVED BLOCK: ", cand.name, location, cand.location, np.linalg.norm(location - cand.location))                
                    else:
                        print ("ROTATED BLOCK: ", block.name, rotation, block.rotation_euler)
                self.block_by_ids.pop(self.block_to_ids[cand], None)
                self.block_by_ids[id] = cand
                self.block_to_ids[cand] = id
                updated_blocks[cand] = 1
                if np.linalg.norm(location - cand.location) >= 0.05 or np.linalg.norm(rot1 - rot2) >= 0.05:
                    cand.location = location
                    cand.rotation_euler = rotation
                    moved_blocks.append(cand.name)
        return moved_blocks

    # def update_scene(self, block_ids, block_locations):
    #     remove_queue = []
    #     for key in block_ids:
    #         if key in self.block_dict:
    #             idx = block_ids.index(key)
    #             self.block_dict[key].location = block_locations[idx]
    #         else:
    #             min_dist = 10e9
    #             cand = None
    #             for key1 in self.block_dict:
    #                 if key1 not in block_ids:
    #                     print (block_locations[block_ids.index(key)], self.block_dict[key1].location)                    
    #                     cur_dist = bl_dist(block_locations[block_ids.index(key)], self.block_dict[key1].location)
    #                     if min_dist > cur_dist:
    #                         min_dist = cur_dist
    #                         cand = key1
    #             #print (cand, key)
    #             if cand != None:
    #                 self.block_dict[cand].location = block_locations[block_ids.index(key)]
    #                 self.block_dict[key] = block_dict[cand]
    #                 del self.block_dict[cand]
    #             else:
    #                 self.add_block(key)
    #                 self.block_dict[key].location = block_locations[block_ids.index(key)]
               
            
    def get_block_data(self):
        url = "http://127.0.0.1:1236/world-api/block-state.json"
        response = requests.get(url, data="")
        #print (response.text)
        json_data = json.loads(response.text)
        #block_ids = []
        #block_locations = []

        block_data = []        

        for segment in json_data['BlockStates']:
            #print (segment)
            #block_ids += [segment['ID']]
            #str_loc = segment['Position']
            #print (segment['Position'])
            #block_locations += [np.array([float(x) for x in str_loc.split(",")])]
            position = self.bw_multiplier * np.array([float(x) for x in segment['Position'].split(",")])
            rotation = Quaternion([float(x) for x in segment['Rotation'].split(",")]).to_euler()
            # tmp = rotation[0]
            # rotation[0] = rotation[1]
            # rotation[1] = tmp
            block_data.append((segment['ID'], position, rotation))            

        #print (json_data['BlockStates'][0]['ID'])
        #print (json_data['BlockStates'][0]['Position'])
        #print (json_data['BlockStates'][1]['ID'])
        #print (json_data['BlockStates'][1]['Position'])
        #str_loc = json_data['BlockStates'][0]['Position']
        #loc = [float(x) for x in str_loc.split(",")]
        #print (block_ids)
        #print (block_locations)
        #url = "http://127.0.0.1:1236/world-api/block-metadata.json"
        #response = requests.get(url, data="")
        #print (response.text)
        #json_data = json.loads(response.text)
        #print ("\n")
        #for bl in json_data["Blocks"]:
        #    print (bl)
        #print ("RETURNED VALS:", block_ids, block_locations, block_data)
        #print ("\n".join([str(bl) for bl in block_data]))
        return block_data

def main():
    Tracker()

if __name__== "__main__":
    main()