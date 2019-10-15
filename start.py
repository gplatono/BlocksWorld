import subprocess
import os
import time
import sys
import platform

system = platform.system()

eta_path = os.path.normpath("../eta-blocksworld")
bw_path = os.path.dirname(os.path.abspath(__file__))

command = ['blender', 'bw_scene.blend', '-P', 'main.py', '--']
bgcommand = ['blender', '--background', 'bw_scene.blend', '-P', 'main.py', '--']

os.chdir(eta_path)
if system == 'Windows':
	os.system("start /B sbcl --load start.lisp")
else:
	os.system("sbcl --load start.lisp &")

time.sleep(2.0)
os.chdir(bw_path)

if '-s' in sys.argv:
	command.append('-s')

if '-d' in sys.argv:
	command.append('-d')

if '-b' not in sys.argv:
	subprocess.call(command)
else:
	subprocess.call(bgcommand)