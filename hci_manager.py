import enum
import os
import sys
import re
import requests
from threading import Thread, RLock, Event
import time
import operator
import collections
import datetime
import traceback

from query_frame import QueryFrame
from ulf_parser import ULFParser
from constraint_solver import *
import spatial
from spatial import near, touching
from geometry_utils import get_planar_distance_scaled
import utils
from bw_planner import Planner

class HCIManager(object):
	"""Manages the high-level interaction loop between the user and the system."""

	class STATE(enum.Enum):
		"""Enumeration of all the states that the system can assume."""
		INIT = 0
		SYSTEM_GREET = 1
		USER_GREET = 2
		QUESTION_PENDING = 3		
		USER_BYE = 4
		END = 5
		SUSPEND = 6
		TUTORING_BEGIN = 7

	def __init__(self, world, debug_mode = False, speech_mode = True, avatar_mode = True):

		#Stores the context of the conversation. For future use.
		self.context = None

		self.debug_mode = debug_mode
		self.speech_mode = speech_mode
		self.avatar_mode = avatar_mode

		#Current state = initial state
		self.state = self.STATE.INIT

		#User's latest speech piece
		self.current_input = ""

		#The address of the avatar's speech processing servlet
		self.avatar_speech_servlet = 'http://ec2-35-162-37-211.us-west-2.compute.amazonaws.com:8080/avatar_control/'

		self.speech_lock = RLock()
		self.asr_active = False

		self.eta_path = ".." + os.sep + "eta" + os.sep
		self.eta_io = ".." + os.sep + "eta" + os.sep + "io" + os.sep + "david-qa" + os.sep
		self.eta_input = self.eta_path + "input.lisp"		
		self.eta_ulf = self.eta_path + "ulf.lisp"
		self.eta_answer = self.eta_path + "answer.lisp"
		self.eta_output = self.eta_io + "output.txt"
		self.eta_perceptions = self.eta_path + "perceptions.lisp"
		self.eta_goal_req = self.eta_path + "goal-request.lisp"
		self.eta_goal_resp = self.eta_path + "goal-rep.lisp"
		self.eta_planner_input = self.eta_path + "planner-input.lisp"
		self.eta_user_try_success = self.eta_path + "user-try-ka-success.lisp"

		self.eta_perceptions_in = self.eta_io + "in" + os.sep + "Blocks-World-System.lisp"
		self.eta_audio_in = self.eta_io + "in" + os.sep + "Audio.lisp"
		self.eta_text_in = self.eta_io + "in" + os.sep + "Text.lisp"
		self.eta_spatial_in = self.eta_io + "in" + os.sep + "Spatial-Reasoning-System.lisp"

		self.eta_perceptions_out = self.eta_io + "out" + os.sep + "Blocks-World-System.lisp"		
		self.eta_text_out = self.eta_io + "out" + os.sep + "Text.lisp"
		self.eta_spatial_out = self.eta_io + "out" + os.sep + "Spatial-Reasoning-System.lisp"

		self.coords_log_path = "logs" + os.sep + "session_coords_data"
		self.dialog_log_path = "logs" + os.sep + "dialog_log"
		self.log_file = None

		self.world = world
		self.ulf_parser = ULFParser()
		self.planner = Planner(self.world)
		
		self.last_mentioned = []
		self.state = self.STATE.INIT

		self.dialog_counter = 0

		if self.debug_mode:
			self.state = self.STATE.QUESTION_PENDING

	def get_speech_ulf(self, speech_text):
		return "(setq *input* '((^you say-to.v ^me \"" + speech_text + "\")))"
	
	def send_to_eta(self, mode, text):
		if mode == "INPUT":
			filename = self.eta_audio_in
			formatted_msg = self.get_speech_ulf(text)
		elif mode == "GOAL_RESP":
			filename = self.eta_spatial_in
			formatted_msg = "(setq *goal-rep* '" + text + ")"
		elif mode == "PLAN_STEP":
			filename = self.eta_spatial_in
			formatted_msg = "(setq planner-input '" + text + ")"
		else:
			filename = self.eta_spatial_in
			if text != "\'None":
				formatted_msg = "(setq *input* \'(" + text + "))"
			else: 
				formatted_msg = "(setq *input* " + text + ")"
		
	# def send_to_eta(self, mode, text):
	# 	if mode == "INPUT":
	# 		filename = self.eta_input
	# 		formatted_msg = "(setq *next-input* \"" + text + "\")"
	# 	elif mode == "GOAL_RESP":
	# 		filename = self.eta_goal_resp
	# 		formatted_msg = "(setq *goal-rep* '" + text + ")"
	# 	elif mode == "PLAN_STEP":
	# 		filename = self.eta_planner_input
	# 		formatted_msg = "(setq planner-input '" + text + ")"
	# 	else:
	# 		filename = self.eta_answer
	# 		if text != "\'None":
	# 			formatted_msg = "(setq *next-answer* \'(" + text + "))"
	# 		else: 
	# 			formatted_msg = "(setq *next-answer* " + text + ")"

	# 	# filename = self.eta_input if mode == "INPUT" else self.eta_answer
		# formatted_msg = "(setq *next-input* \"" + text + "\")" if mode == "INPUT" \
		# 							else "(setq *next-answer* \'(" + text + "))" if text != "\'None" \
		# 								else "(setq *next-answer* " + text + ")"
									#else "(setq *next-answer* \'(" + text + " NIL))"
		print("SENT TO ETA: ", formatted_msg)
		with open(filename, 'w') as file:
			file.write(formatted_msg)

	def read_from_eta(self, mode):
		filename = ""
		if mode == "ULF":
			filename = self.eta_spatial_out
		elif mode == "GOAL_REQ":
			filename = self.eta_spatial_out
		else:			
		 	filename = self.eta_output

		attempt_counter = 0
		with open(filename, "r+") as file:
			msg = ""
			while msg is None or msg == "":
				time.sleep(0.3)
				msg = file.readlines()
				attempt_counter += 1
				if attempt_counter == 40:
					break
			file.truncate(0)

		if msg == "" or msg is None or msg == []:
			return None
		elif mode == "ULF":
			result = "".join([line for line in msg])
			result = re.sub(" +", " ", result.replace("\n", ""))
			result = (result.split("* '")[1])[:-1]
			self.log("ULF", result)
		elif mode == "GOAL_REQ":
			msg = "".join(msg)
			msg = msg.split("*chosen-obj-schema*")[1].strip()[1:-1]
			return msg
		else:
			result = ""
			responses = [r.strip() for r in msg if r.strip() != ""]
			for resp in responses:
				if "*" not in resp and ": ANSWER" in resp:
					result += resp.split(":")[2]
				elif "*" not in resp and ": " in resp and "DO YOU HAVE A SPATIAL QUESTION" not in resp and "NIL" not in resp:
					result += resp.split(":")[1]			

		return result

	# def read_from_eta(self, mode):
	# 	filename = ""
	# 	if mode == "ULF":
	# 		filename = self.eta_ulf
	# 	elif mode == "GOAL_REQ":
	# 		filename = self.eta_goal_req
	# 	else:			
	# 	 	filename = self.eta_output

	# 	attempt_counter = 0
	# 	with open(filename, "r+") as file:
	# 		msg = ""
	# 		while msg is None or msg == "":
	# 			time.sleep(0.3)
	# 			msg = file.readlines()
	# 			attempt_counter += 1
	# 			if attempt_counter == 40:
	# 				break
	# 		file.truncate(0)

	# 	if msg == "" or msg is None or msg == []:
	# 		return None
	# 	elif mode == "ULF":
	# 		result = "".join([line for line in msg])
	# 		result = re.sub(" +", " ", result.replace("\n", ""))
	# 		result = (result.split("* '")[1])[:-1]
	# 		self.log("ULF", result)
	# 	elif mode == "GOAL_REQ":
	# 		msg = "".join(msg)
	# 		msg = msg.split("*chosen-obj-schema*")[1].strip()[1:-1]
	# 		return msg
	# 	else:
	# 		result = ""
	# 		responses = [r.strip() for r in msg if r.strip() != ""]
	# 		#print ("RESP RAW: ", responses)
	# 		for resp in responses:
	# 			if "*" not in resp and ": ANSWER" in resp:
	# 				result += resp.split(":")[2]
	# 			elif "*" not in resp and ": " in resp and "DO YOU HAVE A SPATIAL QUESTION" not in resp and "NIL" not in resp:
	# 				result += resp.split(":")[1]
	# 			# 	if "#: ANSWER" in resp:
	# 			# 	result += resp.split(":")[2]
	# 			# elif "#: " in resp and "DO YOU HAVE A SPATIAL QUESTION" not in resp and "NIL" not in resp:
	# 			# 	result += resp.split(":")[1]

	# 	return result

	def clear_file(self, filename):
		open(filename, 'w').close()

	def read_and_vocalize_from_eta(self):
		response = self.read_from_eta(mode = "OUTPUT")		
		if response != "" and response is not None:
			print ("\n\033[1;34;40mDAVID: " + str(response) + "\033[0;37;40m\n")
			#response = response.lower().replace(' you was', ' i was')
			if self.avatar_mode == True:
				self.send_to_avatar('SAY', response)
			# else:
			# 	print ("Go on...")
			self.log("DAVID", response)		
		return response

	def is_talking(self):
		req = requests.get(self.avatar_speech_servlet + "?is_talking")
		#print ("REQ RESULT: ", req.text)
		return req.text == "true"

	def log(self, mode, text):
		dt = datetime.datetime.now()
		dt_str = dt.strftime("%H:%M:%S")
		self.world.log_event(mode, text)
		with open(self.dialog_log_path, "a+") as logf:
			logf.write(dt_str + " " + str(mode) + ": " + str(text) + "\n")		

	def log_coords(self, coords):
		with open(self.coords_log_path, "a+") as logf:
			logf.write("\n" + coords + "\n")

	def init_log(self):
		with open(self.dialog_log_path, "a+") as logf:
			logf.write("\nSESSION: " + datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S") + "\n")
			logf.write("=============================================================\n")		

		with open(self.coords_log_path, "a+") as logf:
			logf.write("\nSESSION: " + datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S") + "\n")
			logf.write("=============================================================\n")		

	def send_perceptions(self):
		"""
		(
		(|Toyota| at-loc.p ($ loc 0 0 0))
		(|Twitter|) ((past move.v) (from.p-arg ($ loc 0 0 0)) (to.p-arg ($ loc 1 1 1))))
		)

		(setq next-answer 'None) for no relation satisfying the query """
		
		assert self.world.history is not None and len(self.world.history) > 0

		loc_dict = self.world.history[-1].locations
		locations = ['(' + self.world.find_entity_by_name(name).get_ulf() + ' at-loc.p ' + \
				utils.loc_to_ulf(loc_dict[name]) + ')' for name in loc_dict]
		moves = self.world.get_move_ulfs_after_checkpoint()
		
		perceptions = "(setq *next-perceptions* \'(" + " ".join(locations + moves) + "))"
		print ("PERCEPTIONS: ", perceptions)
		self.log_coords(perceptions)
		with open(self.eta_perceptions_in, 'w') as f:
			f.write(perceptions)
		self.world.make_checkpoint()
	
	def get_ulf(self, query_frame, subj_list, obj_list):
		if subj_list is None or len(subj_list) == 0:
			return '\'None'
		ret_val = ''
		rel = None
		if query_frame.predicate is not None:
			rel = query_frame.predicate.content
			is_neg = query_frame.predicate.neg
			for mod in query_frame.predicate.mods:
				is_neg |= type(mod) == TNeg
			if is_neg:
				rel = 'not ' + rel
			#print ("ANS DATA: ", subj_list, rel, obj_list)
		print ("ANSWER SET DATA: ", subj_list, obj_list)
		if rel is not None and type(rel) != TCopulaBe and obj_list != None and len(obj_list) > 0 and type(obj_list[0]) == tuple and query_frame.query_type != query_frame.QueryType.ATTR_COLOR and query_frame.query_type != query_frame.QueryType.DESCR and query_frame.query_type != query_frame.QueryType.COUNT:
			self.last_mentioned = []
			for subj in subj_list:				
				if type(subj[0]) == Entity:
					self.last_mentioned.append(subj[0])
					for obj in obj_list:
						if type(obj[0][0]) == Entity:					
							ret_val += '((that (' + subj[0].get_ulf() + ' ' + rel + ' ' + obj[0][0].get_ulf()+ ')) certain-to-degree ' + str(subj[1] * obj[1]) + ') '
						else:
							ret_val += '(' + subj[0].get_ulf() + ' ' + str(subj[1]) + ') '
		elif query_frame.query_type == query_frame.QueryType.DESCR: 
			for item in obj_list:
				item = item[0][0]
				#print ("WHERE: ", item)
				self.last_mentioned = [item[1][0][0]]
				if len(item[1][0]) == 2:
					ret_val += '((that (' + item[1][0][0].get_ulf() + ' ' + item[0] + ' ' + item[1][0][1].get_ulf() + ')) certain-to-degree ' + str(item[1][1]) + ') '
				elif len(item[1][0]) == 3:
					ret_val += '((that (' + item[1][0][0].get_ulf() + ' ' + item[0] + ' ' + item[1][0][1].get_ulf() + ' ' + item[1][0][2].get_ulf() + ')) certain-to-degree ' + str(item[1][1]) + ') '
		else:
			self.last_mentioned = [subj[0] for subj in subj_list]
			for subj in subj_list:
				ret_val += '((' + subj[0].get_ulf() + ') ' + str(subj[1]) + ')'
		ret_val = ret_val.replace('McDonald\'s', 'McDonalds')
		return ret_val

	def preprocess(self, input):
		input = input.lower()
		misspells = [(' book', ' block'), (' blog', ' block'), (' black', ' block'), (' walk', ' block'), (' wok', ' block'), \
					(' lock', ' block'), (' vlog', ' block'), (' blocked', ' block'), (' glock', ' block'), (' look', ' block'),\
					(' talk', ' block'), (' cook', ' block'), (' clock', ' block'), (' plug', ' block'), (' logo', ' block'), (' boxer', ' blocks are'), \
					(' blonde', ' block'), (' blow', ' block'), (' bloke', ' block'), (' dog', ' block'), \
					(' involved', ' above'), (' about', ' above'), (' patching', ' touching'), (' catching', ' touching'),\
					(' cashing', ' touching'), (' flashing', ' touching'), (' flushing', ' touching'),(' fashion', ' touching'), (' patch', ' touch'), \
					(' thatching', ' touching'), (' trash in', ' touching'), (' tracking', ' touching'),
					(' in a cup', ' on top'), (' after the right', ' are to the right'), 
					(' merced us', ' mercedes'), (' messages', ' mercedes'), (' mercer does', ' mercedes'), (' merced is', ' mercedes'), \
					(' critter', ' twitter'), (' butcher', ' twitter'), (' treetop', ' twitter'), (' toilet', ' twitter'), (' trader', ' twitter'),
					(' front mount', ' frontmost'),
					(' grimlock', ' green block'),
					(' where\'s', ' where is'),
					(' attaching', ' touching'),
					(' talking block', ' target block'), (' chopping', ' target'), (' testicle', ' texaco'),\
					(' merciless', ' mercedes'), \
					(' mug', ' block'), (' the find', ' behind'),
					(' touches', ' touch'), (' at top', ' on top'), (' stretching', ' is touching'),
					(' to be right', ' to the right'), (' for the rights', ' to the right'), 
					(' in the table', ' on the table'), \
					(' top most', ' topmost'), (' right most', ' rightmost'), (' left most', ' leftmost'), (' front most', ' frontmost'),
					(' back most', ' backmost'), (' back-most', ' backmost'),
					(' top-most', ' topmost'), (' right-most', ' rightmost'), (' right move', ' rightmost'), (' left-most', ' leftmost'), (' front-most', ' frontmost'), \
					(' song', ' some'), (' sound', ' some'), (' sun', ' some'), \
					(' hyatt', ' highest'), (' hyve', ' highest'), (' hive', ' highest'), (' louis', ' lowest'), 
					(' father\'s', ' farthest'), (' father', ' farthest'),\
					(' gridlock', ' green block'), (' rim ', ' green '), (' siri', ' david'), (' date it', ' david'),
					(' grimblock', ' green block'), (' redlock', ' red block'), (' rap ', ' red '), (' ramp ', ' red '),
					(' ram block', ' red block'), (' to other', ' two other'), (' lava the', ' above the'), (' watch', ' touch'),
					(' phase', ' face'), (' passing', ' touching'), (' 3 ', ' three '), 
					(' to bl', ' two bl'), (' to red', ' two red'), (' to gre', ' two gre'), ('what colors', 'what color'),
					(' rad', ' red'), (' rand', ' red'), 
					('what\'s ', 'what is '),
					(' sims', ' since'), (' sings', ' since'), (' love', ' block'), (' globe', ' block'), (' is starting', ' is touching'), 
					(' passed', ' has'), (' pass', ' has'), (' paused', ' has'),
					(' did they', ' did i'), (' have they', ' have i'),
					(' 2 ', ' two '), (' 3 ', ' three '), (' to moves', ' two moves')]
		for misspell, fix in misspells:
			input = input.replace(misspell, fix)
		return input

	def start(self):
		"""Initiate the listening loop."""

		#self.debug_mode = True
		if self.speech_mode:
			print ("Starting the listening thread...")
			mic_thread = Thread(target = self.mic_loop)
			#mic_thread.setDaemon(True)
			self.asr_active = True
			mic_thread.start()
		

		self.clear_file(self.eta_ulf)
		self.clear_file(self.eta_answer)
		self.clear_file(self.eta_input)
		#self.clear_file(self.coords_log_path)
		self.init_log()

		print ("\n==========================================================")
		print ("Starting the dialog loop...")
		print ("==========================================================\n")
		
		while True:
			
			if self.state == self.STATE.INIT:				
				response = self.read_and_vocalize_from_eta()				
				if response is None:
					continue

				self.state = self.STATE.SYSTEM_GREET
				if "TEACH YOU THE CONCEPT" in response:
					self.state = self.STATE.TUTORING_BEGIN
					obj_schema = self.read_from_eta(mode = "GOAL_REQ")
					print ("GOAL: ", obj_schema)					
					#print ("OBJ_SCHEMA: ", obj_schema)
					self.planner.init(obj_schema)
					goal_schema = self.planner.get_goal_schema()
					print ("GOAL SCHEMA: ", goal_schema)
					self.send_to_eta(mode="GOAL_RESP", text=goal_schema)
					next_step = self.planner.next()
					print ("NEXT STEP:", next_step)
					#self.send_to_eta(mode="PLAN_STEP", text=next_step)

				else:
					print ("Go ahead, ask a question...")
				continue

			if not self.speech_mode:
				self.current_input = input ("\033[1;34;40mYOU: ")
				print ("\033[0;37;40m")

			self.speech_lock.acquire()
			if self.current_input != "":
				
				if self.state != self.STATE.SUSPEND and self.state != self.STATE.TUTORING_BEGIN:
					self.state = self.STATE.QUESTION_PENDING
				if self.speech_mode:
					print ("\033[1;34;40mYOU: " + self.current_input + "\033[0;37;40m")
				self.current_input = self.preprocess(self.current_input)
				#self.current_input = self.current_input.replace("is it ", "is that block ")
				
				if self.debug_mode == False and self.state != self.STATE.SUSPEND:

					self.qa()
					
					if response is not None and ("good bye" in response.lower() or "take a break" in response.lower()):
						print ("ENDING THE SESSION...")
						exit()

					time.sleep(0.5)					
				self.current_input = ""
			self.speech_lock.release()
			time.sleep(0.1)

	def qa(self):
		time.sleep(0.2)										
		self.send_to_eta("INPUT", self.current_input)
		self.send_to_avatar('USER_SPEECH', self.current_input)
		self.send_perceptions()
		time.sleep(1.0)

		#print ("WAITING FOR ULF...")
		ulf = self.read_from_eta(mode = "ULF")
		
		#print ("RETURNED ULF FROM ETA: ", ulf)
		if ulf is not None and ulf != "":
			self.send_to_avatar('ULF', ulf)
			if 'NON-QUERY' not in ulf.upper():
				response_surface = self.process_spatial_request(ulf)							
				print ("Sending Response to ETA: " + response_surface)										
				self.send_to_eta("ANSWER", response_surface)
		time.sleep(4.0)
		
		response = str(self.read_and_vocalize_from_eta())
		open(self.eta_answer, 'w').close()
		self.dialog_counter += 1

		# if not self.speech_mode:
		# 	print ("\033[1;32;40mPlease judge the system's answer, report an error or retract the question by typing one of the following",
		# 		"\033[1;32;40mc - correct",
		# 		"\033[1;32;40mp - partially correct",
		# 		"\033[1;32;40mi - incorrect",
		# 		"\033[1;32;40me - error has occurred",
		# 		"\033[1;32;40mr - retract question (system failed because of user input error, e.g., a typo or ungrammatical construction)", sep = '\n')
		# 	feedback = input ("\033[1;32;40mFEEDBACK (QUESTION " + str(self.dialog_counter) + "): ")
		# 	print ("\033[0;37;40m")
			
		# 	self.world.log_event('USER_FEEDBACK', feedback)
		# 	if feedback.lower() == 'e':
		# 		print ("Thanks, your feedback has been recorded. Please manually close the SBCL window with Eta and restart the program. Press any key to continue...")
		# 		input()
		# 		exit()

	def process_spatial_request(self, ulf):
		response_surface = "\'None"
		if ulf is not None and ulf != "" and ulf != "NIL":
			#self.send_to_avatar('ULF', ulf)
			if re.search(r"^\((\:OUT|OUT|OUT:)", ulf):
				if "(OUT " in ulf:
					ulf = (ulf.split("(OUT ")[1])[:-1]
				else:
					ulf = (ulf.split("(:OUT ")[1])[:-1]
				response_surface = ulf
			else:
				self.state = self.STATE.QUESTION_PENDING
				try:
					print ("ULF: ", ulf, self.last_mentioned)
					if "IT.PRO" in ulf and self.last_mentioned is not None and len(self.last_mentioned) > 0:
						ulf = ulf.replace("IT.PRO", self.last_mentioned[0].get_ulf())
					POSS_FLAG = False
					if "POSS-QUES" in ulf:
						POSS_FLAG = True
						ulf = (ulf.split("POSS-QUES ")[1])[:-1]
					query_tree = self.ulf_parser.parse(ulf)					
					query_frame = QueryFrame(self.current_input, ulf, query_tree)
					
					answer_set_rel, answer_set_ref = process_query(query_frame, self.world.entities)
					answer_set_rel = [item for item in answer_set_rel if item[1] > 0.1]

					if answer_set_ref is not None:
						answer_set_ref = [item for item in answer_set_ref if item[1] > 0.1]
					response_surface = self.get_ulf(query_frame, answer_set_rel, answer_set_ref)
				
					if POSS_FLAG:
						response_surface = "POSS-ANS " + response_surface
				except Exception as e:
					query_frame = QueryFrame(None, None, None)					
					response_surface = "\'None"#self.generate_response(query_frame, [], [])
					#print (str(e))
					traceback.print_exc()

		return response_surface


	def send_to_avatar(self, mode, text):
		#print ("Avatar's response: " + text)
		#print ("SENDING TO AVATAR " + mode + " " + text)
		if mode == 'SAY':
			self.asr_active = False			
			print ("WAIT, DAVID IS TALKING...")
			#time.sleep(1.0)
			req = requests.get(self.avatar_speech_servlet + "?say=" + text)
			time.sleep(1.0)
			while self.is_talking():
				time.sleep(1.0)
			time.sleep(0.5)
			self.asr_active = True
			print ("DAVID HAS FINISHED, GO ON...")
		elif mode == 'ULF':
			req = requests.get(self.avatar_speech_servlet + "?ulf=" + text)
		elif mode == 'USER_SPEECH':
			self.log("USER", self.current_input)
			req = requests.get(self.avatar_speech_servlet + "?user_speech=" + text)
		avatar_status = str(req.status_code)
		#print ("STATUS: " + avatar_status)

	def load_as_text(self, text):
		def load_loop():
			for utterance in text:
				while True:
					self.speech_lock.acquire()
					if self.current_input == "":
						self.current_input = utterance
						self.speech_lock.release()
						break
					self.speech_lock.release()

		ll_th = Thread(target = load_loop)
		ll_th.start()

	def mic_loop(self):
		"""The mic listening loop."""

		from gcs_micstream import ResumableMicrophoneStream
		from google.cloud import speech

		sample_rate = 16000
		chunk_size = int(sample_rate / 10)  # 100ms

		client = speech.SpeechClient()
		config = speech.types.RecognitionConfig(encoding=speech.enums.RecognitionConfig.AudioEncoding.LINEAR16,
			sample_rate_hertz=sample_rate, language_code='en-US', max_alternatives=1, enable_word_time_offsets=True,
			 enable_automatic_punctuation=True)
		streaming_config = speech.types.StreamingRecognitionConfig(config=config, interim_results=True)

		mic_manager = ResumableMicrophoneStream(sample_rate, chunk_size)

		with mic_manager as stream:
			while not stream.closed:
				audio_generator = stream.generator()
				requests = (speech.types.StreamingRecognizeRequest(audio_content=content)
					for content in audio_generator)

				responses = client.streaming_recognize(streaming_config, requests)

				responses = (r for r in responses if (r.results and r.results[0].alternatives))
				num_chars_printed = 0

				for response in responses:
					if not response.results:
						continue

					"""The `results` list is consecutive. For streaming, we only care about
					the first result being considered, since once it's `is_final`, it
					moves on to considering the next utterance."""
					result = response.results[0]
					if not result.alternatives:
						continue

					transcript = result.alternatives[0].transcript
					# Display interim results, but with a carriage return at the end of the
					# line, so subsequent lines will overwrite them.
					# If the previous result was longer than this one, we need to print
					# some extra spaces to overwrite the previous result
					overwrite_chars = ' ' * (num_chars_printed - len(transcript))

					if self.asr_active is False:
						# print ("ASR :", self.asr_active)						
						time.sleep(1.0)
						break

				

					if not result.is_final:
						sys.stdout.write(transcript + overwrite_chars + '\r')
						sys.stdout.flush()
						num_chars_printed = len(transcript)
					else:
						self.speech_lock.acquire()
						self.current_input = transcript.lower()
						self.speech_lock.release()
						num_chars_printed = 0

	'''
Here is a (nonexhaustive) list of questions that I think I can answer in a very natural way:
1: Existential Questions -
	Q: Is there a block at height 3?
	A: No, there is not a block that is at height 3.

	Q: Are there blocks to the left of the SRI block?
	A: Yes, a block that is to the left of the SRI block is the Toyota block.

	Q: Does there exist a red block touching a blue block?
	A: Yes, there probably does.
2: Identification Questions -
	Q: What block is above the Toyota block?
	A: There is no block that is above the Toyota block.

	Q: Which blocks are below the SRI block?
	A: The Toyota block and the Nvidia block are below the SRI block.

	Q: Which three blocks are below the SRI block?
	A: The Toyota block and the Nvidia block are below the SRI block,
				but it is less certain for the McDonald's block.
3. Counting Questions -
	Q: How many blocks are to the left of the Nvidia block?
	A: There are 7 blocks that are to the left of the Nvidia block.

	Q: How many blocks touch the Nvidia block?
	A: There are 3 blocks that touch the Nvidia block: the Toyota, Starbucks, and Mercedes blocks.

	Q: What is the number of blocks at height 5:
	A: There are 2: the SRI and Nvidia blocks
4. Questions about Color -
	Q: What is the color is the leftmost block?
	A: It is yellow.

	Q: What color are the blocks touching the McDonald's block?
	A: There is a red block and a green block that are touching the McDonald's block
5. Confirmation / Error:
	Q: What is the meaning of life?
	A: There is no object that satisfies those parameters, please rephrase and ask again.

	Q: Is there a yellow block touching a green block?
	A: No.

	Q: Can a block fit between the SRI and Nvidia blocks?
	A: Yes.

	'''
	# def generate_response(self, query_object, answer_set, certainty):
	# 	#answer_set = response_object.answer_set
	# 	#certainty = response_object.certainty
	# 	#relation = response_object.relation

	# 	#print ("ADJ_MODS: ", query_object.extract_subject_adj_modifiers())
	# 	#print ("SUBJ_PLUR: ", query_object.is_subject_plural)
	# 	# Check which state we're in...
	# 	if self.state == self.STATE.INIT:
	# 		pass

	# 	elif self.state == self.STATE.USER_GREET:
	# 		pass

	# 	elif self.state == self.STATE.QUESTION_PENDING:
	# 		# Here, branch on the question type
	# 		# (mostly) completed types:  EXIST, ERROR, IDENT, ATTR_COLOR
	# 		#       EXIST: perhaps use query object to generate desired property
	# 		#       ERROR: perhaps alter boilerplate
	# 		#       IDENT: perhaps use query object to generate desired property
	# 		#       ATTR_COLOR: perhaps use query object to generate desired property
	# 		#       COUNT: perhaps use query object to generate desired property
	# 		#       CONFIRM: consider making more verbose
	# 		# incomplete types: ATTR_ORIENT,
	# 		# empty types: DESCR
	# 		# Need to work on initial pre-processing as well

	# 		# handle the error case then begin pre-processing
	# 		if query_object.query_type == QueryFrame.QueryType.ERROR:
	# 			return "Sorry, I was unable to find an object that satisfies given constraints, please rephrase in a simpler way."

	# 		# pre-processing
	# 		user_input_surface = query_object.surface.lower()
	# 		user_input_list = re.findall("[a-zA-Z'-]+", user_input_surface)#self.english_sentence_to_list(user_input_surface)
	# 		# print ("USER INPUT LIST:", user_input_list)
	# 		while not user_input_surface[-1].isalpha():
	# 			user_input_surface = user_input_surface[:-1]

	# 		type_surf = "block"
	# 		pos_types = ["block", "row", "stack", "group"]#, "blocks", "rows", "stacks", "groups"]

	# 		for t in pos_types:
	# 			if (t + ".n") in query_object.ulf:
	# 				type_surf = t
	# 				break

	# 		# check for adjectives
	# 		subj_adjs = query_object.extract_subject_adj_modifiers()
	# 		if subj_adjs != []:
	# 			type_surf = " ".join(subj_adjs).strip() + " " + type_surf

	# 		plural_type_surf = type_surf + "s"

	# 		threashold = 0.7

	# 		index = 0

	# 		#ADDED BY GEORGIY
	# 		grounding = True

	# 		if not type_surf in user_input_list and not plural_type_surf in user_input_list:
	# 			grounding = False
	# 		elif not type_surf in user_input_list:
	# 			index = user_input_list.index(plural_type_surf) + 1
	# 		elif not plural_type_surf in user_input_list:
	# 			index = user_input_list.index(type_surf) + 1
	# 		else:
	# 			index = min(user_input_list.index(plural_type_surf), user_input_list.index(type_surf)) + 1

	# 		if index < len(user_input_list):
	# 			while user_input_list[index] == "are" or user_input_list[index] == "is" or user_input_list[index] == "that":
	# 				index += 1

	# 		if index >= len(user_input_list)-1:
	# 			grounding = False
	# 		else:
	# 			des_prop = " ".join(user_input_list[index:])

	# 		if query_object.query_type == QueryFrame.QueryType.IDENT:
	# 		#question_type == question_type.IDENT:
	# 			# These are the questions like "Which blocks are touching the SRI
	# 			# block?" and "What block is above the Toyota block?"...  This
	# 			# presuposes that the answer exists, but this will be treated very
	# 			# similarly to the exists question, only with more emphasis on the
	# 			# identification and less on the existance

	# 			# PREPROCESSING: --------------------------------------------------
	# 			# it is helpful to know how the question was posed so that it can
	# 			# be answered more naturally
	# 			surf_aux = "*"
	# 			if "is" in user_input_list:
	# 				surf_aux = "is"
	# 			elif "are" in user_input_list:
	# 				surf_aux = "are"

	# 			# decide when it will sound natural to ground our response
	# 			use_grounding = grounding and not surf_aux == "*"
	# 			# this is kinda an easy way out and somewhat arbitrary but
	# 			# it probably won't sound unnatural

	# 			if len(answer_set) == 0:
	# 			# give an "it doesn't exist" answer
	# 				if len(certainty) == 0 or certainty[0] > threashold:
	# 				# give a certain "it doesn't exist" answer
	# 					if use_grounding:
	# 					# give a certain "it doesn't exist" answer with grounding
	# 						if surf_aux == "is":
	# 							return "There is no " + type_surf + " that is " + des_prop + "."
	# 						elif surf_aux == "are":
	# 							return "There are no " + plural_type_surf + " that are " + des_prop + "."
	# 					else:
	# 					# give a certain "it doesn't exist" answer without grounding
	# 						return "There is no such " + type_surf + "."
	# 				else: # note this is probably not reachable
	# 				# give an uncertain "it doesn't exist answer"
	# 					return "That probably doesn't exist."

	# 			ans_list = self.entities_to_english_list(answer_set, 'name')

	# 			if len(answer_set) == 1:
	# 			# identify the only answer
	# 				if certainty[0] > threashold:
	# 				# identify the only answer with certainty
	# 					if use_grounding:
	# 					# identify the only answer with certainty and grounding
	# 						if query_object.is_subject_plural:
	# 						# identify the only answer with certainty, grounding, and corrective phrasing
	# 							return "Just " + ans_list + " is " + des_prop + "."
	# 						else:
	# 						# identify the only answer with certainty and grounding, but not corrective phrasing
	# 							return ans_list + " is " + des_prop + "."
	# 					else:
	# 					# identify the only answer with certainty and without grounding
	# 						return "Only " + ans_list + "."
	# 				else:
	# 				# identify the only answer with uncertainty
	# 					if use_grounding:
	# 					# identify the only answer with uncertainty and grounding
	# 						return "I think only " + ans_list + " is " + des_prop + "."
	# 					else:
	# 					# identify the only answer with uncertainty and without grounding
	# 						return "Probably just " + ans_list + " is."

	# 			elif len(answer_set) > 5 and not self.contains_a_number(query_object.ulf):
	# 			# identify the listable answers
	# 				ans_list = self.entities_to_english_list(answer_set[0:5], 'name')
	# 				if min(certainty) < threashold:
	# 					if max(certainty) > threashold:
	# 					# identify the multiple answers with mixed certainty

	# 						# split the answers by certainty
	# 						cert_lists = self.entities_split_by_certainty(answer_set, certainty, threashold)
	# 						cert_items = self.entities_to_english_list(cert_lists[0])
	# 						uncert_items = self.entities_to_english_list(cert_lists[1])

	# 						if use_grounding:
	# 						# identify the multiple answers with mixed certainty and grounding
	# 							if len(cert_lists[0]) == 1:
	# 								return cert_items + " is " + des_prop + ", but it is less certain for " + uncert_items + "."
	# 							else:
	# 								return cert_items + " are " + des_prop + ", but it is less certain for " + uncert_items + "."
	# 						else:
	# 						# identify the multiple answers with mixed certainty and without grounding
	# 							if len(cert_lists[0]) == 1:
	# 								return cert_items + " does, but it is less certain for " + uncert_items + "."
	# 							else:
	# 								return cert_items + " do, but it is less certain for " + uncert_items + "."
	# 					else:
	# 					# identify the multiple answers uncertainly
	# 						if use_grounding:
	# 						# identify the multiple answers uncertainly with grounding
	# 							return "There probably are " + str(len(answer_set)) + " " + plural_type_surf + " that are " + des_prop + ", possibly including " + ans_list + " are " + des_prop + "."
	# 						else:
	# 						# identify the multiple answers uncertainly without grounding
	# 							return "There probably are " + str(len(answer_set)) + " " + plural_type_surf + ", possibly including " + ans_list + "."
	# 				else:
	# 				# identify the multiple answers with certainty
	# 					if use_grounding:
	# 					# identify the multiple answers with certainty with grounding
	# 						return "There are " + str(len(answer_set)) + " that are " + des_prop + ", including " + ans_list + "."
	# 					else:
	# 					# identify the multiple answers with certainty without grounding
	# 						return ans_list.capitalize() + " do, as well as " + str(len(answer_set) - 4) + " others."

	# 			else:
	# 			# identify the multiple answers
	# 				if min(certainty) < threashold:
	# 					if max(certainty) > threashold:
	# 					# identify the multiple answers with mixed certainty

	# 						# split the answers by certainty
	# 						cert_lists = self.entities_split_by_certainty(answer_set, certainty, threashold)
	# 						cert_items = self.entities_to_english_list(cert_lists[0])
	# 						uncert_items = self.entities_to_english_list(cert_lists[1])

	# 						if use_grounding:
	# 						# identify the multiple answers with mixed certainty and grounding
	# 							if len(cert_lists[0]) == 1:
	# 								return cert_items + " is " + des_prop + ", but it is less certain for " + uncert_items + "."
	# 							else:
	# 								return cert_items + " are " + des_prop + ", but it is less certain for " + uncert_items + "."
	# 						else:
	# 						# identify the multiple answers with mixed certainty and without grounding
	# 							if len(cert_lists[0]) == 1:
	# 								return cert_items + " does, but it is less certain for " + uncert_items + "."
	# 							else:
	# 								return cert_items + " do, but it is less certain for " + uncert_items + "."
	# 					else:
	# 					# identify the multiple answers uncertainly
	# 						if use_grounding:
	# 						# identify the multiple answers uncertainly with grounding
	# 							return "Possibly " + ans_list + " are " + des_prop + "."
	# 						else:
	# 						# identify the multiple answers uncertainly without grounding
	# 							return "Perhaps " + ans_list + " do."
	# 				else:
	# 				# identify the multiple answers with certainty
	# 					if use_grounding:
	# 					# identify the multiple answers with certainty with grounding
	# 						return ans_list + " are " + des_prop + "."
	# 					else:
	# 					# identify the multiple answers with certainty without grounding
	# 						return ans_list.capitalize() + " do."

	# 		elif query_object.query_type == QueryFrame.QueryType.CONFIRM:
	# 			# These are the questions like "Is the SRI block near the Toyota
	# 			# block?" and "The Toyota block is red, right?"
	# 			# How these will be handled depends very much on what the answer set
	# 			# will give in these cases, I'm not sure. I will assume for now
	# 			# that it just contains a boolean value in these cases

	# 			if len(answer_set) == 0:
	# 			# return certain no
	# 				return "No."
	# 			else:
	# 			# return yes
	# 				if certainty[0] > threashold:
	# 				# return certain yes
	# 					return "Yes."
	# 				else:
	# 				# return uncerain yes
	# 					return "Probably."
	# 		elif query_object.query_type == QueryFrame.QueryType.EXIST:
	# 			# These are the questions like "Is there a block at height 3?"
	# 			# or "Is there a block to the left of the SRI block?"
	# 			# In these cases, the answer set will be empty if the answer is no,
	# 			# and contain the applicable items if it is yes

	# 			# PREPROCESSING: --------------------------------------------------
	# 			# decide if we will ground the answer
	# 			use_grounding = grounding and (user_input_surface.startswith("is there") or user_input_surface.startswith("are there"))
	# 			# I think this is a good policy, if the user asks "does there exist
	# 			# a block at height 3" we can respond simply "Yes.", and if there
	# 			# is only one we can name it

	# 			# it is helpful to know how the question was posed so that it can
	# 			# be answered more naturally
	# 			surf_aux = "*"
	# 			if "is" in user_input_list:
	# 				surf_aux = "is"
	# 			elif "are" in user_input_list:
	# 				surf_aux = "are"
	# 			elif "does" in user_input_list:
	# 				surf_aux = "does"
	# 			elif "do" in user_input_list:
	# 				surf_aux = "do"
	# 			# use the list to ensure it only considers full words, i.e.
	# 			# "McDonald's" contains do

	# 			if len(answer_set) == 0:
	# 			# give a negative answer
	# 				if len(certainty) == 0 or certainty[0] > threashold:
	# 				# give a certain negative answer
	# 					if use_grounding:
	# 					# give a certain negative answer with grounding
	# 						if surf_aux == "is":
	# 							return "No, there is not a " + type_surf + " that is " + des_prop + "."
	# 						elif surf_aux == "are":
	# 							return "No, there are no " + plural_type_surf + " that are " + des_prop + "."
	# 					else:
	# 					# give a certain negative answer without grounding
	# 						return "No."
	# 				else:
	# 				# give an uncertain negative answer
	# 					return "Probably not."

	# 			elif len(answer_set) == 1:
	# 			# give a positive answer and identify it

	# 				# identify the entity
	# 				ans_list = self.entities_to_english_list(answer_set, 'name')
	# 				# ex. "the Nvidia block", "the afsk32313 stack"

	# 				if certainty[0] > threashold:
	# 				# give a certain positive answer and identify it
	# 					if use_grounding:
	# 					# give a certain positive answer w/ grounding and identify it
	# 						if surf_aux == "is":
	# 							return "Yes, the " + type_surf + " that is " + des_prop + " is " + ans_list + "."
	# 						elif surf_aux == "are":
	# 							return "Yes, a " + type_surf + " that is " + des_prop + " is " + ans_list + "."
	# 						# I think this is a good distinction, i.e. for the qs:
	# 						# "Is there a block on the SRI block?" should be answered:
	# 						#       "Yes, the block that is on the SRI block is the Toyota block."
	# 						# "Are there any blocks on the SRI block?" sba:
	# 						#       "Yes, a block that is on the SRI block is the Toyota block."

	# 					else:
	# 					# give a certain positive answer w/out grounding and identify it
	# 						if surf_aux == "do":
	# 							return "Yes, " + ans_list + " is one."
	# 						return     "Yes, it is " + ans_list + "."
	# 						# Similar to above, i.e. for the qs:
	# 						# "Do there exist blocks on the SRI block?" should be answered:
	# 						#       "Yes, the Toyota block is one."
	# 						# "Does there exist a block on the SRI block?" should be answered:
	# 						#       "Yes, it is the Toyota block."
	# 				else:
	# 				# give an uncertain positive answer and identify it
	# 					if use_grounding:
	# 					# give an uncertain positive answer w/ grounding and identify it
	# 						return "Perhaps, the " + ans_list + " may be " + des_prop + "."
	# 					else:
	# 					# give an uncertain positive answer w/out grounding and identify it
	# 						return "Perhaps, the " + ans_list + " might be."

	# 			# there is more than one answer
	# 			else:
	# 			# give a positive answer
	# 				if min(certainty) < threashold:
	# 					if max(certainty) > threashold:
	# 						# give a mixed certainty positive answer

	# 						# split the answers by certainty
	# 						cert_lists = self.entities_split_by_certainty(answer_set, certainty, threashold)
	# 						cert_items = self.entities_to_english_list(cert_lists[0])
	# 						uncert_items = self.entities_to_english_list(cert_lists[1])

	# 						if use_grounding:
	# 						# give a mixed certainty postive answer with grounding
	# 							if len(cert_items) == 1:
	# 								return "It is uncertain.  " + cert_items + " is " + des_prop + ", but it is less sure for " + uncert_items + "."
	# 							else:
	# 								return "It is uncertain.  " + cert_items + " are " + des_prop + ", but it is less sure for " + uncert_items + "."
	# 						else:
	# 						# give a mixed certainty postive answer without grounding
	# 							if len(cert_items) == 1:
	# 								return "It is uncertain.  " + cert_items + " is definately, but it is less sure for " + uncert_items + "."
	# 							else:
	# 								return "It is uncertain.  " + cert_items + " are definately, but it is less sure for " + uncert_items + "."
	# 					else:
	# 					# give an uncertain positive answer
	# 						if use_grounding:
	# 						# give an uncertain postive answer with grounding
	# 							return "I think there are " + plural_type_surf + " that are " + des_prop + "."
	# 						else:
	# 						# give an uncertain postive answer without grounding
	# 							return "Yes, there probably " + surf_aux + "."
	# 							# when being this vague we want to echo back the question's language, even though there is no grounding
	# 				else:
	# 				# give a certain positive answer
	# 					if use_grounding:
	# 					# give a certain positive answer with grounding
	# 						return "Yes, there are " + plural_type_surf + " that are " + des_prop + "."
	# 					else:
	# 					# give a certain positive answer without grounding
	# 						return "Yes."
	# 		elif query_object.query_type == QueryFrame.QueryType.ATTR_COLOR:
	# 			# These are the questions like "What is the color of the leftmost
	# 			# block?" or "Which color blocks are touching the Nvidia block"

	# 			# PREPROCESSING: --------------------------------------------------
	# 			# decide if we will ground the answer
	# 			use_grounding = grounding and user_input_surface.startswith("what color")
	# 			# I'm not sure what a good policy is here, I'll write code even if it's
	# 			# unreachable in case it sounds unnatural and this is changed

	# 			if len(answer_set) == 0:
	# 				# there is no such object
	# 				return "There is no such " + type_surf + "."

	# 			# a simple list of colors
	# 			ans_list_simple = self.entities_to_color_list(answer_set)
	# 			ans_list_complex = self.entities_to_english_list(answer_set, 'color')

	# 			if len(answer_set) == 1:
	# 				if certainty[0] > threashold:
	# 				# give a certain answer
	# 					if use_grounding:
	# 					# give a certain answer with grounding
	# 						return "There is a " + ans_list_complex.split()[1] + " that is " + des_prop + "."
	# 					else:
	# 					# give a certain answer without grounding
	# 						return "It is " + ans_list_simple + "."
	# 				else:
	# 					# give an uncertain answer
	# 					if use_grounding:
	# 						return "Perhaps there is a " + ans_list_complex.split()[1] + " that is " + des_prop + "."
	# 					else:
	# 						return "It may be " + ans_list_simple + "."

	# 			else:
	# 				# ans_list looks like "red, blue, and green" or "yellow and orange"
	# 				if all(cert > threashold for cert in certainty):
	# 				# give a certain answer
	# 					if use_grounding:
	# 					# give a certain answer with grounding
	# 						return "There is a " + ans_list_complex.split()[1] + " that are " + des_prop + "."
	# 					else:
	# 					# give a certain answer without grounding
	# 						return "They are " + ans_list_simple + "."
	# 				else:
	# 				# give an uncertain answer
	# 					if use_grounding:
	# 					# give an uncertain answer w/ grounding
	# 						return "Perhaps there are a " + ans_list_complex.split()[1] + " that are " + des_prop + "."
	# 					else:
	# 					# give an uncertain answer w/out grounding
	# 						return "They may be " + ans_list_simple + "."
	# 		elif query_object.query_type == QueryFrame.QueryType.ATTR_ORIENT:
	# 			# These are the questions like "What is the orientation of the SRI
	# 			# block?"
	# 			pass
	# 		elif query_object.query_type == QueryFrame.QueryType.COUNT:
	# 			print ("ENTERING COUNTING RESPONSE GENERATION...")
	# 			# These are questions like "How many blocks are to the left of the
	# 			# NVidia block" and "How many blocks touch the McDonals block?"
	# 			# If it is just one, we want to say that and name it, otherwise
	# 			# just give a number based on the size of the answer set and
	# 			# adjust for certainty

	# 			# PREPROCESSING: --------------------------------------------------
	# 			# decide if we will ground the answer
	# 			use_grounding = grounding and not user_input_surface.startswith("what is")
	# 			use_grounding = use_grounding and not des_prop == "*"
	# 			# I think this is workable...  if they ask for what is the number
	# 			# we simply want to give the number, if how many we should elaborate

	# 			# it is helpful to know how the question was posed so that it can
	# 			# be answered more naturally
	# 			surf_aux = "*"
	# 			if "is" in user_input_list:
	# 				surf_aux = "is"
	# 			elif "are" in user_input_list:
	# 				surf_aux = "are"

	# 			if len(answer_set) == 0:
	# 			# say there are none
	# 				if use_grounding:
	# 				# say with grounding there are none
	# 					if surf_aux == "is" or surf_aux == "are":
	# 						return "There are no " + plural_type_surf + " that are " + des_prop + "."
	# 					else:
	# 						return "There are no " + plural_type_surf + " that " + des_prop + "."
	# 				else:
	# 				# say flatly there are none
	# 					return "There are 0."

	# 			ans_list = self.entities_to_english_list(answer_set, 'name')

	# 			# This part is very much like the identification
	# 			if len(answer_set) == 1:
	# 			# identify the only answer
	# 				if certainty[0] > threashold:
	# 				# identify the only answer with certainty
	# 					if use_grounding:
	# 					# identify the only answer with certainty and grounding
	# 						if surf_aux == "is" or surf_aux == "are": # !!!! are we active or passive voice !!!!
	# 							return "Just " + ans_list + " is " + des_prop + "."
	# 						else:
	# 							return "Just " + ans_list + " " + des_prop + "."
	# 					else:
	# 					# identify the only answer with certainty and without grounding
	# 						return "Only one, " + ans_list + "."
	# 				else:
	# 				# identify the only answer with uncertainty
	# 					if use_grounding:
	# 					# identify the only answer with uncertainty and grounding
	# 						if surf_aux == "is" or surf_aux == "are":
	# 							return "Probably only " + ans_list + " is " + des_prop + "."
	# 						else:
	# 							return "Probably only " + ans_list + " " + des_prop + "."
	# 					else:
	# 					# identify the only answer with uncertainty and without grounding
	# 						return "Probably " + ans_list + " is the only one."

	# 			# give the number AND list them out, also very much like identification
	# 			elif len(answer_set) < 4:
	# 			# identify the reasonably enumerable answers
	# 				print ("HERE")
	# 				if certainty[0] > threashold:
	# 				# identify the reasonably enumerable answers with certainty
	# 					if use_grounding:
	# 					# identify the reasonably enumerable answers with certainty and grounding
	# 						if surf_aux == "is" or surf_aux == "are":
	# 							return "There are " + str(len(answer_set)) + " that are " + des_prop + ". " + ans_list + "."
	# 						else:
	# 							return "There are " + str(len(answer_set)) + " that " + des_prop + ". " + ans_list + "."
	# 					else:
	# 					# identify the reasonably enumerable answers with certainty and without grounding
	# 						return "There are " + str(len(answer_set)) + ": " + ans_list + "."
	# 				else:
	# 				# identify the reasonably enumerable answers with uncertainty
	# 					if use_grounding:
	# 					# identify the reasonably enumerable answers with uncertainty and grounding
	# 						if surf_aux == "is" or surf_aux == "are":
	# 							return "There are probably " + str(len(answer_set)) + " that are " + des_prop + ". " + ans_list + "."
	# 						else:
	# 							return "There are probably " + str(len(answer_set)) + " that " + des_prop + ". " + ans_list + "."
	# 					else:
	# 					# identify the reasonably enumerable answers with uncertainty and without grounding
	# 						return "Probably " + answer_list + " is the only one."

	# 			else:
	# 			# give the number
	# 				if all(cert > threashold for cert in certainty):
	# 				# give the number, with certainty
	# 					if use_grounding:
	# 					# give the number certaintly with grounding
	# 						if surf_aux == "is" or surf_aux == "are":
	# 							return "There are " + str(len(answer_set)) + " " + plural_type_surf + " that are " + des_prop + "."
	# 						else:
	# 							return "There are " + str(len(answer_set)) + " " + plural_type_surf + " that " + des_prop + "."
	# 					else:
	# 					# give the number certaintly without grounding
	# 						return "There are " + str(len(answer_set)) + "."
	# 				else:
	# 				# give the number, with uncertainty
	# 					if use_grounding:
	# 					# give the number uncertaintly with grounding
	# 						if surf_aux == "is" or surf_aux == "are":
	# 							return "There may be " + str(len(answer_set)) + " " + plural_type_surf + " that are " + des_prop + "."
	# 						else:
	# 							return "There may be " + str(len(answer_set)) + " " + plural_type_surf + " that " + des_prop + "."
	# 					else:
	# 					# give the number uncertaintly without grounding
	# 						return "Perhaps there are " + str(len(answer_set)) + "."

	# 	elif self.state == self.STATE.USER_BYE:
	# 		pass

	# 	elif self.state == self.STATE.END:
	# 		pass


	# # The following are functions used in generate_response -------------------------------------------

	# # takes an entity list and returns an english list with no excess whitespace
	# # the list must be of length at least 1
	# # ents is the answer set, attribute is the attribute that should be listed
	# def entities_to_english_list(self, ents, attribute):
	# 	types = list(map(lambda x: x.type_structure[:-1][-1], ents))
	# 	attribs = list(map(operator.attrgetter(attribute), ents))
	# 	uniform_type = all(x == types[0] for x in types)

	# 	#Entities? [yes, deciding to change varible names is troublesome]
	# 	if len(ents) == 1:#if len(list) == 1:
	# 		return 'the ' + attribs[0] + ' ' + types[0]
	# 	elif len(ents) == 2:
	# 		return 'the ' + attribs[0] + ' ' + types[0] + ' and the ' + attribs[1] + ' ' + types[1]
	# 	else:
	# 		if uniform_type:
	# 			out = 'the ' + attribs[0]
	# 			for i in range(1, len(ents)-1):
	# 				out += ', ' + attribs[i]
	# 			out += ', and ' + attribs[-1] + ' ' + types[-1] + 's'
	# 			return out
	# 		else:
	# 			out = 'the ' + attribs[0] + ' ' + types[0]
	# 			for i in range(1, len(ents)-1):
	# 				out += ', ' + attribs[i] + ' ' + types[i]
	# 			out += ', and ' + attribs[-1] + ' ' + types[-1]
	# 			return out
	# # Sample Usage:
	# # [_Nvidia_] -> (is...) "the Nvidia block" (is...)
	# # [_Toyota_, _McDonalds_] ->
	# #                  (...are) "the Toyota block and the McDonalds block" (are...)
	# # [_SRI_, _Mercedes_, _Target_] ->
	# #                      (...are) "the SRI, Mercedes, and Target blocks" (are...)
	# # [_SRI_, _Mercedes_, _Support_] ->
	# #  (...are) "the SRI block, the Mercedes block, and the Support stack" (are...)

	# # use this when we only want colors, without types or specifier
	# def entities_to_color_list(self, ents):
	# 	# similar to entities_to_english_list here
	# 	types = map(lambda x: x.type_structure[:-1][0], ents)
	# 	cols = map(operator.attrgetter('color'), ents)
	# 	cols = list(dict.fromkeys(cols))
	# 	uniform_type = all(x == type[0] for x in type)

	# 	# there's only one answer
	# 	if len(cols) == 1:
	# 		return cols[0]

	# 	if len(cols) == 2:
	# 		return cols[0] + " and " + cols[1]

	# 	else:
	# 		out = ""
	# 		for i in range(0, (len(cols) - 1)):
	# 			out += cols[i] + ", "
	# 		out += " and " + cols[-1]
	# 		return out

	# # returns two lists divided by the certainty about each entity
	# # used in mixed certainty answers
	# # assumes answers are already sorted by certainty
	# def entities_split_by_certainty(self, ents, certainty, threashold):
	# 	out = []
	# 	for i in range(0, len(ents)):
	# 		if certainty[i] < threashold:
	# 			out.append(ents[:i])
	# 			out.append(ents[i:])
	# 			return out
	# 	out.append(ents)
	# 	return out

	# # returns true if the ULF contains a number, false it is doesn't
	# def contains_a_number(self, ulf):
	# 	return 'one' in ulf or 'two' in ulf or 'three' in ulf or 'four' in ulf or 'five' in ulf or 'six' in ulf or 'seven' in ulf or 'eight' in ulf or 'nine' in ulf or 'ten' in ulf

	# def english_sentence_to_list(self, sentence):
	# 	# Divide the sentence into words
	# 	list = sentence.split()

	# 	# Next, we need to split the punctuation marks from the words
	# 	# ie. ['Is', 'this...', 'pig-latin?'] -> ['Is', 'this', '...', 'pig-latin', '?']
	# 	diff = 1
	# 	i = 0
	# 	while i < len(list):
	# 		# if the word ends in a punctuation mark
	# 		if not list[i][-1].isalpha():
	# 			cutoff_len = 1;
	# 			for j in range(2, len(list[i])):
	# 				if list[i][-j].isalpha():
	# 					cutoff_len = j-1
	# 					break
	# 			else:
	# 				i += 1
	# 				continue

	# 			list.insert(i+diff, list[i][-cutoff_len:])
	# 			list[i] = list[i][:-cutoff_len]

	# 			diff += 1
	# 			i -= 1
	# 		i += 1
	# 	return list

	# # There needs to be a better way to do this...  To cover cases where it is
	# # non-obvious
	# def get_verb_phrase(self, user_input_surface, is_are):
	# 	if not is_are == "*":
	# 		return is_are + user_input_surface.split(is_are)[1]
	# 	else:
	# 		return add(user_input_list.split()[3:])

	# -------------------------------------------------------------------------------------------------

def main():
	manager = HCIManager(debug_mode=False)
	#manager.load_as_text(["Test message 1", "Test message 2", "Test message 3", "Test message 4", "Test message 5"])
	manager.start()

if __name__== "__main__":
    main()