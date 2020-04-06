import subprocess
import os
import time
import sys
import platform

system = platform.system()

eta_path = os.path.normpath("../eta")
bw_path = os.path.dirname(os.path.abspath(__file__))

params = []

if '-bo' not in sys.argv:
	os.chdir(eta_path)
	if system == 'Windows':
		os.system("start cmd.exe /c sbcl --load start.lisp")
		os.system("start cmd.exe /c python C:\\Users\\user\\quicklisp\\local-projects\\ulf2english\\python-repl-server.py 8080 \"g:g\"")
	elif system == 'Linux':
		#os.system("")
		cmd = ['gnome-terminal', '--']
		process = subprocess.Popen(cmd + ['sbcl',  '--load', 'start.lisp'])#, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
		process = subprocess.Popen(cmd + ['python2', '../quicklisp/local-projects/ulf2english/python-repl-server.py', '8080', '\"g:g\"'])#, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
		#os.system("gnome-terminal -x sh -c \"sbcl --load start.lisp &; bash\"")
		#os.system("gnome-terminal -x sh -c \"python ../quicklisp/local-projects/ulf2english/python-repl-server.py 8080 \"g:g\"")	
	elif system == 'Darwin':
		pass

	time.sleep(5.0)
	os.chdir(bw_path)
else:
	params.append('-bo')

if '-s' in sys.argv:
	params.append('-s')

if '-d' in sys.argv:
	params.append('-d')

if '-t' in sys.argv:
	params.append('-t')

if '-bg' not in sys.argv:
	command = ['blender', 'bw_scene.blend', '-P', 'main.py', '--'] + params	
else:
	command = ['blender', '--background', 'bw_scene.blend', '-P', 'main.py', '--'] + params

subprocess.call(command)