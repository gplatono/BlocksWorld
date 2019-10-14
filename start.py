import subprocess
import os
import time


os.chdir("..\\eta-blocksworld\\")
os.system("start /B sbcl --load start.lisp")
#subprocess.call(['sbcl', '--load', 'start.lisp'])

time.sleep(2.0)

os.chdir("..\\BlocksWorld")
subprocess.call(['blender', 'bw_scene.blend', '-P', 'main.py'])