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
                ModalTimerOp.tracker.world.update(self.get_block_data())
                bpy.context.evaluated_depsgraph_get().update() 
                
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
                                    
            return {"PASS_THROUGH"}
        
        #Setup code (fires at the start)
        def execute(self, context):
            self._timer = context.window_manager.event_timer_add(0.5, window=context.window)
            context.window_manager.modal_handler_add(self)
            return {"RUNNING_MODAL"}
        
        #Timer termination and cleanup
        def cancel(self, context):
            context.window_manager.event_timer_remove(self._timer)
            return {"CANCELLED"}

class Tracker(object):
    
    def __init__(self, world):       
        bpy.utils.register_class(ModalTimerOp)
        ModalTimerOp.tracker = self
        # self.block_by_ids = {}
        # self.block_to_ids = {}
        
        self.verbose = False
        self.verbose_rotation = False
        
        

        self.moved_blocks = []
        self.world = world        
        bpy.ops.wm.modal_timer_operator()

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
               
            
    

def main():
    Tracker()

if __name__== "__main__":
    main()