import glob
import subprocess


scenes = glob.glob("*.blend")
tests = glob.glob("*.data")
tests = []
for scene in scenes:	
	command = ['blender', scene, '-P', 'test_scene.py', '--', scene.split('.')[0] + '.data']
	subprocess.call(command)