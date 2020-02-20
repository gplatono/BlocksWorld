import enum
import os
import sys
import re
import requests
from threading import Thread, RLock, Event
import time
import operator
import collections
from query_frame import QueryFrame
from ulf_parser import ULFParser
from constraint_solver import *
import spatial
from spatial import near, touching
from geometry_utils import get_planar_distance_scaled
import datetime
import traceback

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

	def __init__(self, world, debug_mode = False):

		#Stores the context of the conversation. For future use.
		self.context = None

		self.debug_mode = debug_mode

		#Current state = initial state
		self.state = self.STATE.INIT

		#User's latest speech piece
		self.current_input = ""

		#The address of the avatar's speech processing servlet
		self.avatar_speech_servlet = 'http://ec2-35-162-37-211.us-west-2.compute.amazonaws.com:8080/avatar_control/'

		self.speech_lock = RLock()
		self.asr_active = False

		self.eta_path = ".." + os.sep + "eta" + os.sep + "io" + os.sep
		self.eta_input = self.eta_path + "input.lisp"
		self.eta_ulf = self.eta_path + "ulf.lisp"
		self.eta_answer = self.eta_path + "answer.lisp"
		self.eta_output = self.eta_path + "output.txt"
		self.eta_perceptions = self.eta_path + "perceptions.lisp"

		self.dialog_log_path = "dialog_log"
		self.log_file = None

		self.ulf_parser = ULFParser()
		self.world = world

		self.state = self.STATE.INIT

		if self.debug_mode:
			self.state = self.STATE.QUESTION_PENDING
		
	def send_to_eta(self, mode, text):
		filename = self.eta_input if mode == "INPUT" else self.eta_answer
		formatted_msg = "(setq *next-input* \"" + text + "\")" if mode == "INPUT" \
									else "(setq *next-answer* \'(" + text + "))" if text != "\'None" \
										else "(setq *next-answer* " + text + ")"
									#else "(setq *next-answer* \'(" + text + " NIL))"
		print("SENT TO ETA: ", formatted_msg)
		with open(filename, 'w') as file:
			file.write(formatted_msg)

	def read_from_eta(self, mode):
		filename = self.eta_ulf if mode == "ULF" else self.eta_output
		attempt_counter = 0
		with open(filename, "r+") as file:
			msg = ""
			while msg is None or msg == "":
				time.sleep(0.5)
				msg = file.readlines()
				attempt_counter += 1
				if attempt_counter == 10:
					break
			file.truncate(0)

		if msg == "" or msg is None or msg == []:
			return None
		elif mode == "ULF":
			result = "".join([line for line in msg])
			result = re.sub(" +", " ", result.replace("\n", ""))
			result = (result.split("* '")[1])[:-1]
			self.log("ULF", result)
		else:
			result = ""
			responses = [r.strip() for r in msg if r.strip() != ""]
			print ("RESP RAW: ", responses)
			for resp in responses:
				if "#: ANSWER" in resp:
					result += resp.split(":")[2]
				elif "#: " in resp and "DO YOU HAVE A SPATIAL QUESTION" not in resp and "NIL" not in resp:
					result += resp.split(":")[1]

		return result

	def clear_file(self, filename):
		open(filename, 'w').close()

	def read_and_vocalize_from_eta(self):
		response = self.read_from_eta(mode = "OUTPUT")		
		if response != "" and response is not None:
			print ("\nFINAL RESPONSE: " + str(response))
			response = response.lower().replace(' you was', ' i was')
			self.send_to_avatar('SAY', response)
			self.log("DAVID", response)		
		return response

	def is_talking(self):
		req = requests.get(self.avatar_speech_servlet + "?is_talking")
		#print ("REQ RESULT: ", req.text)
		return req.text == "true"

	def log(self, mode, text):
		dt = datetime.datetime.now()
		dt_str = dt.strftime("%H:%M:%S")
		with open(self.dialog_log_path, "a+") as logf:
			logf.write(dt_str + " " + str(mode) + ": " + str(text) + "\n")

	def init_log(self):
		with open(self.dialog_log_path, "a+") as logf:
			logf.write("\nSESSION: " + datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S") + "\n")
			logf.write("=============================================================\n")		

	def send_perceptions(self):
		"""
		(
		(|Toyota| at-loc.p ($ loc 0 0 0))
		(|Twitter|) ((past move.v) (from.p-arg ($ loc 0 0 0)) (to.p-arg ($ loc 1 1 1))))
		)

		(setq next-answer 'None) for no relation satisfying the query """
		#last_moved = self.world.get_last_moved()
		def loc_to_str(loc):
			return ' '.join([str(coord) for coord in loc])

		assert self.world.history is not None and len(self.world.history) > 0

		loc_dict = self.world.history[-1].locations
		locations = ['(|' + name + '| at-loc.p ($ loc ' + loc_to_str(loc_dict[name]) + '))' for name in loc_dict]
		moves = self.world.get_moves_after_checkpoint()
		print ("moved blocks: " + ', '.join([item[0] for item in moves]))
		moves = ['(|' + item[0] + '| ((past move.v) (from.p-arg ($ loc ' + loc_to_str(item[1]) + ')) (to.p-arg ($ loc ' +\
							loc_to_str(item[2]) + '))))' for item in moves]

		perceptions = "(setq *next-perceptions* \'(" + " ".join(locations + moves) + "))"
		#print ("PERCEPTIONS: ", perceptions)
		with open(self.eta_perceptions, 'w') as f:
			f.write(perceptions)
		self.world.make_checkpoint()
		#if last_moved is not None and len(last_moved) > 0:			
			#print ("LAST_MOVED: ", last_moved)		
			#coord_list = ['(|' + item[0] + '| at-loc.p ($ loc ' + str(int(item[1][0])) + ' ' \
			#+ str(int(item[1][1])) + ' ' + str(int(item[1][2])) + '))' for item in last_moved]
		

	def get_ulf(self, query_frame, subj_list, obj_list):
		ret_val = ''
		rel = query_frame.predicate.content
		is_neg = query_frame.predicate.neg
		for mod in query_frame.predicate.mods:
			is_neg |= type(mod) == TNeg
		if is_neg:
			rel = 'not ' + rel
		print ("ANS DATA: ", subj_list, rel, obj_list)
		if obj_list != None:
			for subj in subj_list:
				for obj in obj_list:
					ret_val += '((|' + subj[0].name + '| ' + rel + ' |' + obj[0][0].name + '|) ' + str(subj[1] * obj[1]) + ') '
		else: 
			for subj in subj_list:
				ret_val += '(|' + subj[0].name + '|)'
		return ret_val

	def preprocess(self, input):
		input = input.lower()
		misspells = [(' book', ' block'), (' blog', ' block'), (' black', ' block'), (' walk', ' block'), (' wok', ' block'), \
					(' lock', ' block'), (' vlog', ' block'), (' blocked', ' block'), (' glock', ' block'), (' look', ' block'),\
					(' talk', ' block'), (' cook', ' block'), (' clock', ' block'), (' plug', ' block'), (' logo', ' block'), (' boxer', ' blocks are'), \
					(' blonde', ' block'), (' blow', ' block'), (' bloke', ' block'), (' dog', ' block'), \
					(' involved', ' above'), (' about', ' above'), (' patching', ' touching'), (' catching', ' touching'),\
					(' cashing', ' touching'), (' flashing', ' touching'), (' flushing', ' touching'),(' fashion', ' touching'), (' patch', ' touch'), \
					(' thatching', ' touching'), (' trash in', ' touching'),
					(' in a cup', ' on top'), (' after the right', ' are to the right'), \
					(' merced us', ' mercedes'), (' messages', ' mercedes'), (' mercer does', ' mercedes'), (' merced is', ' mercedes'), \
					(' critter', ' twitter'), (' butcher', ' twitter'), (' treetop', ' twitter'), \
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
					(' sims', ' since'), (' sings', ' since'), (' love', ' block'), (' is starting', ' is touching'), 
					(' passed', ' has'), (' pass', ' has'), (' paused', ' has')]
		for misspell, fix in misspells:
			input = input.replace(misspell, fix)
		return input

	def start(self):
		"""Initiate the listening loop."""
		if self.debug_mode == False:
			print ("Starting the listening thread...")
			mic_thread = Thread(target = self.mic_loop)
			#mic_thread.setDaemon(True)
			self.asr_active = True
			mic_thread.start()
		

		self.clear_file(self.eta_ulf)
		self.clear_file(self.eta_answer)
		self.clear_file(self.eta_input)
		self.init_log()

		print ("Starting the processing loop...")
		
		while True:
			
			if self.state == self.STATE.INIT:
				print ("WAITING...")
				response = self.read_and_vocalize_from_eta()				
				self.state = self.STATE.SYSTEM_GREET
				continue

			self.speech_lock.acquire()
			if self.current_input != "":
				#if re.search(r'\b(exit|quit)\b', self.current_input, re.I):
				#	self.speech_lock.release()
				#	break
				if self.state != self.STATE.SUSPEND:
					self.state = self.STATE.QUESTION_PENDING

				print ("you said: " + self.current_input)
				#print ("SIZE", self.world.find_entity_by_name("Toyota").size)
				self.current_input = self.preprocess(self.current_input)
				self.current_input = self.current_input.replace("is it ", "is that block ")
				
				if self.debug_mode == False and self.state != self.STATE.SUSPEND:
					print ("ENTERING ETA DIALOG EXCHANGE BLOCK...")

					input = self.current_input
					#self.write_coords()
					time.sleep(0.1)										
					self.send_to_eta("INPUT", self.current_input)
					self.send_to_avatar('USER_SPEECH', self.current_input)
					self.send_perceptions()
					#self.log("USER", self.current_input)				
					#print ("SLEEPING...")
					time.sleep(0.5)

					print ("WAITING FOR ULF...")
					ulf = self.read_from_eta(mode = "ULF")
					
					print ("RETURNED ULF FROM ETA: ", ulf)
					response_surface = self.process_spatial_request(ulf)
					#bkg = self.world.find_entity_by_name('Burger King')
					#tbl = self.world.find_entity_by_name('Table')
					#tar = self.world.find_entity_by_name('Target')
					#stb = self.world.find_entity_by_name('Starbucks')
					#print ("HIGHER THAN ", spatial.higher_than(tar, stb), spatial.higher_than(stb, tar), spatial.at_same_height(stb, tar))
					#print ("TOUCH:")
					#print ([(bl, touching(bl, tbl)) for bl in self.world.entities if bl != tbl])

					print ("RESPONSE SURFACE: " + response_surface)
					print ("SENDING REACTION AND WAITING FOR RESPONSE...")
					
					self.send_to_eta("ANSWER", response_surface)					
					time.sleep(2.0)
					response = str(self.read_and_vocalize_from_eta())
					open(self.eta_answer, 'w').close()					
					
					if response is not None and ("good bye" in response.lower() or "take a break" in response.lower()):
						print ("ENDING THE SESSION...")
						exit()

					time.sleep(0.5)					
				self.current_input = ""
			self.speech_lock.release()
			time.sleep(0.1)


	def process_spatial_request(self, ulf):
		response_surface = "\'None"
		if ulf is not None and ulf != "" and ulf != "NIL":
			self.send_to_avatar('ULF', ulf)
			if re.search(r"^\((\:OUT|OUT|OUT:)", ulf):
				if "(OUT " in ulf:
					ulf = (ulf.split("(OUT ")[1])[:-1]
				else:
					ulf = (ulf.split("(:OUT ")[1])[:-1]
				response_surface = ulf
			else:
				self.state = self.STATE.QUESTION_PENDING
				try:
					print ("ULF: ", ulf)
					POSS_FLAG = False
					if "POSS-QUES" in ulf:
						POSS_FLAG = True
						ulf = (ulf.split("POSS-QUES ")[1])[:-1]
					query_tree = self.ulf_parser.parse(ulf)					
					query_frame = QueryFrame(self.current_input, ulf, query_tree)
					print ("QUERY TYPE: ", query_frame.query_type)
					if query_frame.query_type == query_frame.QueryType.ATTR_COLOR:
						pred_vals = process_query(query_frame, self.world.entities)
						#print ("ANSWER SET: ", pred_vals)
						answer_set_rel = list(set([arg[0] for (arg, val) in pred_vals]))
						response_surface = self.get_ulf(query_frame, answer_set_rel, None)
					else:
						answer_set_rel, answer_set_ref = process_query(query_frame, self.world.entities)
						answer_set_rel = [item for item in answer_set_rel if item[1] > 0.1]
						if answer_set_ref is not None:
							answer_set_ref = [item for item in answer_set_ref if item[1] > 0.1]
						response_surface = self.get_ulf(query_frame, answer_set_rel, answer_set_ref)

					#print ("ANSWER: ", response_surface)

					# if query_frame.query_type != query_frame.QueryType.DESCR and query_frame.query_type != query_frame.QueryType.ATTR_COLOR:
					# 	answer_set_rel, answer_set_ref = process_query(query_frame, self.world.entities)
					# 	answer_set_rel = [item for item in answer_set_rel if item[1] > 0.1]
					# 	if answer_set_ref is not None:
					# 		answer_set_ref = [item for item in answer_set_ref if item[1] > 0.1]
					# 	ans = self.get_ulf(answer_set_rel, query_frame.predicate.content, answer_set_ref)
					# 	print ("ANSWER: ", ans)	
					# 	return ans


					# 	print ("ANSWER SET: ", answer_set_rel)
					# 	if query_frame.query_type == query_frame.QueryType.CONFIRM:
					# 		response_surface = "Yes" if len(answer_set_rel) > 0 else "No"						
					# 	elif query_frame.query_type == query_frame.QueryType.EXIST:
					# 		if len(answer_set_rel) > 0:
					# 			response_surface = "Yes, the " + answer_set_rel[0][0].name + " block."
					# 		else:
					# 			response_surface = "No, there is no such block."
					# 	else:
					# 		#If asking about the arg0
					# 		print ("IDENTIFYING BLOCK: ", query_frame.resolve_relatum)						

					# 		if query_frame.resolve_relatum:
					# 			response_surface = self.generate_response(query_frame, [item[0] for item in answer_set_rel], [item[1] for item in answer_set_rel])
					# 		else:
					# 			response_surface = self.generate_response(query_frame, [item[0] for item in answer_set_ref], [item[1] for item in answer_set_ref])
					# elif query_frame.query_type == query_frame.QueryType.DESCR:																		
					# 	pred_vals = process_query(query_frame, self.world.entities)
					# 	print ("ANSWER SET: ", pred_vals)								
					# 	response_surface = ""
					# 	for item in pred_vals:
					# 		if len(item[1][0]) == 2:
					# 			response_surface += "The " + item[1][0][0].name + " block is " + item[0] + " the " + item[1][0][1].name + " block."
					# 		else:
					# 			response_surface += "The " + item[1][0][0].name + " block is " + item[0] + " the " + item[1][0][1].name + " block and the "\
					# 			+ item[1][0][2].name + " block."
					# 	response_surface = response_surface.replace("Table block", "Table")
					# elif query_frame.query_type == query_frame.QueryType.ATTR_COLOR:
					# 	pred_vals = process_query(query_frame, self.world.entities)
					# 	print ("ANSWER SET: ", pred_vals)								
					# 	pred_vals = list(set(pred_vals))
					# 	response_surface = ""
					# 	if pred_vals == [] or (len(pred_vals) == 1 and pred_vals[0] == None):
					# 		response_surface = "There is no such block."
					# 	elif len(pred_vals) == 1:
					# 		response_surface = "It is " + pred_vals[0]
					# 	else:
					# 		response_surface = "They are "
					# 		for item in pred_vals[:-1]:	
					# 			response_surface += item + ", "
					# 		response_surface += "and " + pred_vals[-1]							
					if POSS_FLAG:
						response_surface = "POSS-ANS " + response_surface
				except Exception as e:
					query_frame = QueryFrame(None, None, None)					
					response_surface = self.generate_response(query_frame, [], [])
					#print (str(e))
					traceback.print_exc()

		return response_surface


	def send_to_avatar(self, mode, text):
		#print ("Avatar's response: " + text)
		#print ("SENDING TO AVATAR " + mode + " " + text)
		if mode == 'SAY':
			self.asr_active = False			
			print ("DAVID IS TALKING...")
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
	def generate_response(self, query_object, answer_set, certainty):
		#answer_set = response_object.answer_set
		#certainty = response_object.certainty
		#relation = response_object.relation

		#print ("ADJ_MODS: ", query_object.extract_subject_adj_modifiers())
		#print ("SUBJ_PLUR: ", query_object.is_subject_plural)
		# Check which state we're in...
		if self.state == self.STATE.INIT:
			pass

		elif self.state == self.STATE.USER_GREET:
			pass

		elif self.state == self.STATE.QUESTION_PENDING:
			# Here, branch on the question type
			# (mostly) completed types:  EXIST, ERROR, IDENT, ATTR_COLOR
			#       EXIST: perhaps use query object to generate desired property
			#       ERROR: perhaps alter boilerplate
			#       IDENT: perhaps use query object to generate desired property
			#       ATTR_COLOR: perhaps use query object to generate desired property
			#       COUNT: perhaps use query object to generate desired property
			#       CONFIRM: consider making more verbose
			# incomplete types: ATTR_ORIENT,
			# empty types: DESCR
			# Need to work on initial pre-processing as well

			# handle the error case then begin pre-processing
			if query_object.query_type == QueryFrame.QueryType.ERROR:
				return "Sorry, I was unable to find an object that satisfies given constraints, please rephrase in a simpler way."

			# pre-processing
			user_input_surface = query_object.surface.lower()
			user_input_list = re.findall("[a-zA-Z'-]+", user_input_surface)#self.english_sentence_to_list(user_input_surface)
			# print ("USER INPUT LIST:", user_input_list)
			while not user_input_surface[-1].isalpha():
				user_input_surface = user_input_surface[:-1]

			type_surf = "block"
			pos_types = ["block", "row", "stack", "group"]#, "blocks", "rows", "stacks", "groups"]

			for t in pos_types:
				if (t + ".n") in query_object.ulf:
					type_surf = t
					break

			# check for adjectives
			subj_adjs = query_object.extract_subject_adj_modifiers()
			if subj_adjs != []:
				type_surf = " ".join(subj_adjs).strip() + " " + type_surf

			plural_type_surf = type_surf + "s"

			threashold = 0.7

			index = 0

			#ADDED BY GEORGIY
			grounding = True

			if not type_surf in user_input_list and not plural_type_surf in user_input_list:
				grounding = False
			elif not type_surf in user_input_list:
				index = user_input_list.index(plural_type_surf) + 1
			elif not plural_type_surf in user_input_list:
				index = user_input_list.index(type_surf) + 1
			else:
				index = min(user_input_list.index(plural_type_surf), user_input_list.index(type_surf)) + 1

			if index < len(user_input_list):
				while user_input_list[index] == "are" or user_input_list[index] == "is" or user_input_list[index] == "that":
					index += 1

			if index >= len(user_input_list)-1:
				grounding = False
			else:
				des_prop = " ".join(user_input_list[index:])

			if query_object.query_type == QueryFrame.QueryType.IDENT:
			#question_type == question_type.IDENT:
				# These are the questions like "Which blocks are touching the SRI
				# block?" and "What block is above the Toyota block?"...  This
				# presuposes that the answer exists, but this will be treated very
				# similarly to the exists question, only with more emphasis on the
				# identification and less on the existance

				# PREPROCESSING: --------------------------------------------------
				# it is helpful to know how the question was posed so that it can
				# be answered more naturally
				surf_aux = "*"
				if "is" in user_input_list:
					surf_aux = "is"
				elif "are" in user_input_list:
					surf_aux = "are"

				# decide when it will sound natural to ground our response
				use_grounding = grounding and not surf_aux == "*"
				# this is kinda an easy way out and somewhat arbitrary but
				# it probably won't sound unnatural

				if len(answer_set) == 0:
				# give an "it doesn't exist" answer
					if len(certainty) == 0 or certainty[0] > threashold:
					# give a certain "it doesn't exist" answer
						if use_grounding:
						# give a certain "it doesn't exist" answer with grounding
							if surf_aux == "is":
								return "There is no " + type_surf + " that is " + des_prop + "."
							elif surf_aux == "are":
								return "There are no " + plural_type_surf + " that are " + des_prop + "."
						else:
						# give a certain "it doesn't exist" answer without grounding
							return "There is no such " + type_surf + "."
					else: # note this is probably not reachable
					# give an uncertain "it doesn't exist answer"
						return "That probably doesn't exist."

				ans_list = self.entities_to_english_list(answer_set, 'name')

				if len(answer_set) == 1:
				# identify the only answer
					if certainty[0] > threashold:
					# identify the only answer with certainty
						if use_grounding:
						# identify the only answer with certainty and grounding
							if query_object.is_subject_plural:
							# identify the only answer with certainty, grounding, and corrective phrasing
								return "Just " + ans_list + " is " + des_prop + "."
							else:
							# identify the only answer with certainty and grounding, but not corrective phrasing
								return ans_list + " is " + des_prop + "."
						else:
						# identify the only answer with certainty and without grounding
							return "Only " + ans_list + "."
					else:
					# identify the only answer with uncertainty
						if use_grounding:
						# identify the only answer with uncertainty and grounding
							return "I think only " + ans_list + " is " + des_prop + "."
						else:
						# identify the only answer with uncertainty and without grounding
							return "Probably just " + ans_list + " is."

				elif len(answer_set) > 5 and not self.contains_a_number(query_object.ulf):
				# identify the listable answers
					ans_list = self.entities_to_english_list(answer_set[0:5], 'name')
					if min(certainty) < threashold:
						if max(certainty) > threashold:
						# identify the multiple answers with mixed certainty

							# split the answers by certainty
							cert_lists = self.entities_split_by_certainty(answer_set, certainty, threashold)
							cert_items = self.entities_to_english_list(cert_lists[0])
							uncert_items = self.entities_to_english_list(cert_lists[1])

							if use_grounding:
							# identify the multiple answers with mixed certainty and grounding
								if len(cert_lists[0]) == 1:
									return cert_items + " is " + des_prop + ", but it is less certain for " + uncert_items + "."
								else:
									return cert_items + " are " + des_prop + ", but it is less certain for " + uncert_items + "."
							else:
							# identify the multiple answers with mixed certainty and without grounding
								if len(cert_lists[0]) == 1:
									return cert_items + " does, but it is less certain for " + uncert_items + "."
								else:
									return cert_items + " do, but it is less certain for " + uncert_items + "."
						else:
						# identify the multiple answers uncertainly
							if use_grounding:
							# identify the multiple answers uncertainly with grounding
								return "There probably are " + str(len(answer_set)) + " " + plural_type_surf + " that are " + des_prop + ", possibly including " + ans_list + " are " + des_prop + "."
							else:
							# identify the multiple answers uncertainly without grounding
								return "There probably are " + str(len(answer_set)) + " " + plural_type_surf + ", possibly including " + ans_list + "."
					else:
					# identify the multiple answers with certainty
						if use_grounding:
						# identify the multiple answers with certainty with grounding
							return "There are " + str(len(answer_set)) + " that are " + des_prop + ", including " + ans_list + "."
						else:
						# identify the multiple answers with certainty without grounding
							return ans_list.capitalize() + " do, as well as " + str(len(answer_set) - 4) + " others."

				else:
				# identify the multiple answers
					if min(certainty) < threashold:
						if max(certainty) > threashold:
						# identify the multiple answers with mixed certainty

							# split the answers by certainty
							cert_lists = self.entities_split_by_certainty(answer_set, certainty, threashold)
							cert_items = self.entities_to_english_list(cert_lists[0])
							uncert_items = self.entities_to_english_list(cert_lists[1])

							if use_grounding:
							# identify the multiple answers with mixed certainty and grounding
								if len(cert_lists[0]) == 1:
									return cert_items + " is " + des_prop + ", but it is less certain for " + uncert_items + "."
								else:
									return cert_items + " are " + des_prop + ", but it is less certain for " + uncert_items + "."
							else:
							# identify the multiple answers with mixed certainty and without grounding
								if len(cert_lists[0]) == 1:
									return cert_items + " does, but it is less certain for " + uncert_items + "."
								else:
									return cert_items + " do, but it is less certain for " + uncert_items + "."
						else:
						# identify the multiple answers uncertainly
							if use_grounding:
							# identify the multiple answers uncertainly with grounding
								return "Possibly " + ans_list + " are " + des_prop + "."
							else:
							# identify the multiple answers uncertainly without grounding
								return "Perhaps " + ans_list + " do."
					else:
					# identify the multiple answers with certainty
						if use_grounding:
						# identify the multiple answers with certainty with grounding
							return ans_list + " are " + des_prop + "."
						else:
						# identify the multiple answers with certainty without grounding
							return ans_list.capitalize() + " do."

			elif query_object.query_type == QueryFrame.QueryType.CONFIRM:
				# These are the questions like "Is the SRI block near the Toyota
				# block?" and "The Toyota block is red, right?"
				# How these will be handled depends very much on what the answer set
				# will give in these cases, I'm not sure. I will assume for now
				# that it just contains a boolean value in these cases

				if len(answer_set) == 0:
				# return certain no
					return "No."
				else:
				# return yes
					if certainty[0] > threashold:
					# return certain yes
						return "Yes."
					else:
					# return uncerain yes
						return "Probably."
			elif query_object.query_type == QueryFrame.QueryType.EXIST:
				# These are the questions like "Is there a block at height 3?"
				# or "Is there a block to the left of the SRI block?"
				# In these cases, the answer set will be empty if the answer is no,
				# and contain the applicable items if it is yes

				# PREPROCESSING: --------------------------------------------------
				# decide if we will ground the answer
				use_grounding = grounding and (user_input_surface.startswith("is there") or user_input_surface.startswith("are there"))
				# I think this is a good policy, if the user asks "does there exist
				# a block at height 3" we can respond simply "Yes.", and if there
				# is only one we can name it

				# it is helpful to know how the question was posed so that it can
				# be answered more naturally
				surf_aux = "*"
				if "is" in user_input_list:
					surf_aux = "is"
				elif "are" in user_input_list:
					surf_aux = "are"
				elif "does" in user_input_list:
					surf_aux = "does"
				elif "do" in user_input_list:
					surf_aux = "do"
				# use the list to ensure it only considers full words, i.e.
				# "McDonald's" contains do

				if len(answer_set) == 0:
				# give a negative answer
					if len(certainty) == 0 or certainty[0] > threashold:
					# give a certain negative answer
						if use_grounding:
						# give a certain negative answer with grounding
							if surf_aux == "is":
								return "No, there is not a " + type_surf + " that is " + des_prop + "."
							elif surf_aux == "are":
								return "No, there are no " + plural_type_surf + " that are " + des_prop + "."
						else:
						# give a certain negative answer without grounding
							return "No."
					else:
					# give an uncertain negative answer
						return "Probably not."

				elif len(answer_set) == 1:
				# give a positive answer and identify it

					# identify the entity
					ans_list = self.entities_to_english_list(answer_set, 'name')
					# ex. "the Nvidia block", "the afsk32313 stack"

					if certainty[0] > threashold:
					# give a certain positive answer and identify it
						if use_grounding:
						# give a certain positive answer w/ grounding and identify it
							if surf_aux == "is":
								return "Yes, the " + type_surf + " that is " + des_prop + " is " + ans_list + "."
							elif surf_aux == "are":
								return "Yes, a " + type_surf + " that is " + des_prop + " is " + ans_list + "."
							# I think this is a good distinction, i.e. for the qs:
							# "Is there a block on the SRI block?" should be answered:
							#       "Yes, the block that is on the SRI block is the Toyota block."
							# "Are there any blocks on the SRI block?" sba:
							#       "Yes, a block that is on the SRI block is the Toyota block."

						else:
						# give a certain positive answer w/out grounding and identify it
							if surf_aux == "do":
								return "Yes, " + ans_list + " is one."
							return     "Yes, it is " + ans_list + "."
							# Similar to above, i.e. for the qs:
							# "Do there exist blocks on the SRI block?" should be answered:
							#       "Yes, the Toyota block is one."
							# "Does there exist a block on the SRI block?" should be answered:
							#       "Yes, it is the Toyota block."
					else:
					# give an uncertain positive answer and identify it
						if use_grounding:
						# give an uncertain positive answer w/ grounding and identify it
							return "Perhaps, the " + ans_list + " may be " + des_prop + "."
						else:
						# give an uncertain positive answer w/out grounding and identify it
							return "Perhaps, the " + ans_list + " might be."

				# there is more than one answer
				else:
				# give a positive answer
					if min(certainty) < threashold:
						if max(certainty) > threashold:
							# give a mixed certainty positive answer

							# split the answers by certainty
							cert_lists = self.entities_split_by_certainty(answer_set, certainty, threashold)
							cert_items = self.entities_to_english_list(cert_lists[0])
							uncert_items = self.entities_to_english_list(cert_lists[1])

							if use_grounding:
							# give a mixed certainty postive answer with grounding
								if len(cert_items) == 1:
									return "It is uncertain.  " + cert_items + " is " + des_prop + ", but it is less sure for " + uncert_items + "."
								else:
									return "It is uncertain.  " + cert_items + " are " + des_prop + ", but it is less sure for " + uncert_items + "."
							else:
							# give a mixed certainty postive answer without grounding
								if len(cert_items) == 1:
									return "It is uncertain.  " + cert_items + " is definately, but it is less sure for " + uncert_items + "."
								else:
									return "It is uncertain.  " + cert_items + " are definately, but it is less sure for " + uncert_items + "."
						else:
						# give an uncertain positive answer
							if use_grounding:
							# give an uncertain postive answer with grounding
								return "I think there are " + plural_type_surf + " that are " + des_prop + "."
							else:
							# give an uncertain postive answer without grounding
								return "Yes, there probably " + surf_aux + "."
								# when being this vague we want to echo back the question's language, even though there is no grounding
					else:
					# give a certain positive answer
						if use_grounding:
						# give a certain positive answer with grounding
							return "Yes, there are " + plural_type_surf + " that are " + des_prop + "."
						else:
						# give a certain positive answer without grounding
							return "Yes."
			elif query_object.query_type == QueryFrame.QueryType.ATTR_COLOR:
				# These are the questions like "What is the color of the leftmost
				# block?" or "Which color blocks are touching the Nvidia block"

				# PREPROCESSING: --------------------------------------------------
				# decide if we will ground the answer
				use_grounding = grounding and user_input_surface.startswith("what color")
				# I'm not sure what a good policy is here, I'll write code even if it's
				# unreachable in case it sounds unnatural and this is changed

				if len(answer_set) == 0:
					# there is no such object
					return "There is no such " + type_surf + "."

				# a simple list of colors
				ans_list_simple = self.entities_to_color_list(answer_set)
				ans_list_complex = self.entities_to_english_list(answer_set, 'color')

				if len(answer_set) == 1:
					if certainty[0] > threashold:
					# give a certain answer
						if use_grounding:
						# give a certain answer with grounding
							return "There is a " + ans_list_complex.split()[1] + " that is " + des_prop + "."
						else:
						# give a certain answer without grounding
							return "It is " + ans_list_simple + "."
					else:
						# give an uncertain answer
						if use_grounding:
							return "Perhaps there is a " + ans_list_complex.split()[1] + " that is " + des_prop + "."
						else:
							return "It may be " + ans_list_simple + "."

				else:
					# ans_list looks like "red, blue, and green" or "yellow and orange"
					if all(cert > threashold for cert in certainty):
					# give a certain answer
						if use_grounding:
						# give a certain answer with grounding
							return "There is a " + ans_list_complex.split()[1] + " that are " + des_prop + "."
						else:
						# give a certain answer without grounding
							return "They are " + ans_list_simple + "."
					else:
					# give an uncertain answer
						if use_grounding:
						# give an uncertain answer w/ grounding
							return "Perhaps there are a " + ans_list_complex.split()[1] + " that are " + des_prop + "."
						else:
						# give an uncertain answer w/out grounding
							return "They may be " + ans_list_simple + "."
			elif query_object.query_type == QueryFrame.QueryType.ATTR_ORIENT:
				# These are the questions like "What is the orientation of the SRI
				# block?"
				pass
			elif query_object.query_type == QueryFrame.QueryType.COUNT:
				print ("ENTERING COUNTING RESPONSE GENERATION...")
				# These are questions like "How many blocks are to the left of the
				# NVidia block" and "How many blocks touch the McDonals block?"
				# If it is just one, we want to say that and name it, otherwise
				# just give a number based on the size of the answer set and
				# adjust for certainty

				# PREPROCESSING: --------------------------------------------------
				# decide if we will ground the answer
				use_grounding = grounding and not user_input_surface.startswith("what is")
				use_grounding = use_grounding and not des_prop == "*"
				# I think this is workable...  if they ask for what is the number
				# we simply want to give the number, if how many we should elaborate

				# it is helpful to know how the question was posed so that it can
				# be answered more naturally
				surf_aux = "*"
				if "is" in user_input_list:
					surf_aux = "is"
				elif "are" in user_input_list:
					surf_aux = "are"

				if len(answer_set) == 0:
				# say there are none
					if use_grounding:
					# say with grounding there are none
						if surf_aux == "is" or surf_aux == "are":
							return "There are no " + plural_type_surf + " that are " + des_prop + "."
						else:
							return "There are no " + plural_type_surf + " that " + des_prop + "."
					else:
					# say flatly there are none
						return "There are 0."

				ans_list = self.entities_to_english_list(answer_set, 'name')

				# This part is very much like the identification
				if len(answer_set) == 1:
				# identify the only answer
					if certainty[0] > threashold:
					# identify the only answer with certainty
						if use_grounding:
						# identify the only answer with certainty and grounding
							if surf_aux == "is" or surf_aux == "are": # !!!! are we active or passive voice !!!!
								return "Just " + ans_list + " is " + des_prop + "."
							else:
								return "Just " + ans_list + " " + des_prop + "."
						else:
						# identify the only answer with certainty and without grounding
							return "Only one, " + ans_list + "."
					else:
					# identify the only answer with uncertainty
						if use_grounding:
						# identify the only answer with uncertainty and grounding
							if surf_aux == "is" or surf_aux == "are":
								return "Probably only " + ans_list + " is " + des_prop + "."
							else:
								return "Probably only " + ans_list + " " + des_prop + "."
						else:
						# identify the only answer with uncertainty and without grounding
							return "Probably " + ans_list + " is the only one."

				# give the number AND list them out, also very much like identification
				elif len(answer_set) < 4:
				# identify the reasonably enumerable answers
					print ("HERE")
					if certainty[0] > threashold:
					# identify the reasonably enumerable answers with certainty
						if use_grounding:
						# identify the reasonably enumerable answers with certainty and grounding
							if surf_aux == "is" or surf_aux == "are":
								return "There are " + str(len(answer_set)) + " that are " + des_prop + ". " + ans_list + "."
							else:
								return "There are " + str(len(answer_set)) + " that " + des_prop + ". " + ans_list + "."
						else:
						# identify the reasonably enumerable answers with certainty and without grounding
							return "There are " + str(len(answer_set)) + ": " + ans_list + "."
					else:
					# identify the reasonably enumerable answers with uncertainty
						if use_grounding:
						# identify the reasonably enumerable answers with uncertainty and grounding
							if surf_aux == "is" or surf_aux == "are":
								return "There are probably " + str(len(answer_set)) + " that are " + des_prop + ". " + ans_list + "."
							else:
								return "There are probably " + str(len(answer_set)) + " that " + des_prop + ". " + ans_list + "."
						else:
						# identify the reasonably enumerable answers with uncertainty and without grounding
							return "Probably " + answer_list + " is the only one."

				else:
				# give the number
					if all(cert > threashold for cert in certainty):
					# give the number, with certainty
						if use_grounding:
						# give the number certaintly with grounding
							if surf_aux == "is" or surf_aux == "are":
								return "There are " + str(len(answer_set)) + " " + plural_type_surf + " that are " + des_prop + "."
							else:
								return "There are " + str(len(answer_set)) + " " + plural_type_surf + " that " + des_prop + "."
						else:
						# give the number certaintly without grounding
							return "There are " + str(len(answer_set)) + "."
					else:
					# give the number, with uncertainty
						if use_grounding:
						# give the number uncertaintly with grounding
							if surf_aux == "is" or surf_aux == "are":
								return "There may be " + str(len(answer_set)) + " " + plural_type_surf + " that are " + des_prop + "."
							else:
								return "There may be " + str(len(answer_set)) + " " + plural_type_surf + " that " + des_prop + "."
						else:
						# give the number uncertaintly without grounding
							return "Perhaps there are " + str(len(answer_set)) + "."

		elif self.state == self.STATE.USER_BYE:
			pass

		elif self.state == self.STATE.END:
			pass


	# The following are functions used in generate_response -------------------------------------------

	# takes an entity list and returns an english list with no excess whitespace
	# the list must be of length at least 1
	# ents is the answer set, attribute is the attribute that should be listed
	def entities_to_english_list(self, ents, attribute):
		types = list(map(lambda x: x.type_structure[:-1][-1], ents))
		attribs = list(map(operator.attrgetter(attribute), ents))
		uniform_type = all(x == types[0] for x in types)

		#Entities? [yes, deciding to change varible names is troublesome]
		if len(ents) == 1:#if len(list) == 1:
			return 'the ' + attribs[0] + ' ' + types[0]
		elif len(ents) == 2:
			return 'the ' + attribs[0] + ' ' + types[0] + ' and the ' + attribs[1] + ' ' + types[1]
		else:
			if uniform_type:
				out = 'the ' + attribs[0]
				for i in range(1, len(ents)-1):
					out += ', ' + attribs[i]
				out += ', and ' + attribs[-1] + ' ' + types[-1] + 's'
				return out
			else:
				out = 'the ' + attribs[0] + ' ' + types[0]
				for i in range(1, len(ents)-1):
					out += ', ' + attribs[i] + ' ' + types[i]
				out += ', and ' + attribs[-1] + ' ' + types[-1]
				return out
	# Sample Usage:
	# [_Nvidia_] -> (is...) "the Nvidia block" (is...)
	# [_Toyota_, _McDonalds_] ->
	#                  (...are) "the Toyota block and the McDonalds block" (are...)
	# [_SRI_, _Mercedes_, _Target_] ->
	#                      (...are) "the SRI, Mercedes, and Target blocks" (are...)
	# [_SRI_, _Mercedes_, _Support_] ->
	#  (...are) "the SRI block, the Mercedes block, and the Support stack" (are...)

	# use this when we only want colors, without types or specifier
	def entities_to_color_list(self, ents):
		# similar to entities_to_english_list here
		types = map(lambda x: x.type_structure[:-1][0], ents)
		cols = map(operator.attrgetter('color'), ents)
		cols = list(dict.fromkeys(cols))
		uniform_type = all(x == type[0] for x in type)

		# there's only one answer
		if len(cols) == 1:
			return cols[0]

		if len(cols) == 2:
			return cols[0] + " and " + cols[1]

		else:
			out = ""
			for i in range(0, (len(cols) - 1)):
				out += cols[i] + ", "
			out += " and " + cols[-1]
			return out

	# returns two lists divided by the certainty about each entity
	# used in mixed certainty answers
	# assumes answers are already sorted by certainty
	def entities_split_by_certainty(self, ents, certainty, threashold):
		out = []
		for i in range(0, len(ents)):
			if certainty[i] < threashold:
				out.append(ents[:i])
				out.append(ents[i:])
				return out
		out.append(ents)
		return out

	# returns true if the ULF contains a number, false it is doesn't
	def contains_a_number(self, ulf):
		return 'one' in ulf or 'two' in ulf or 'three' in ulf or 'four' in ulf or 'five' in ulf or 'six' in ulf or 'seven' in ulf or 'eight' in ulf or 'nine' in ulf or 'ten' in ulf

	def english_sentence_to_list(self, sentence):
		# Divide the sentence into words
		list = sentence.split()

		# Next, we need to split the punctuation marks from the words
		# ie. ['Is', 'this...', 'pig-latin?'] -> ['Is', 'this', '...', 'pig-latin', '?']
		diff = 1
		i = 0
		while i < len(list):
			# if the word ends in a punctuation mark
			if not list[i][-1].isalpha():
				cutoff_len = 1;
				for j in range(2, len(list[i])):
					if list[i][-j].isalpha():
						cutoff_len = j-1
						break
				else:
					i += 1
					continue

				list.insert(i+diff, list[i][-cutoff_len:])
				list[i] = list[i][:-cutoff_len]

				diff += 1
				i -= 1
			i += 1
		return list

	# There needs to be a better way to do this...  To cover cases where it is
	# non-obvious
	def get_verb_phrase(self, user_input_surface, is_are):
		if not is_are == "*":
			return is_are + user_input_surface.split(is_are)[1]
		else:
			return add(user_input_list.split()[3:])

	# -------------------------------------------------------------------------------------------------

def main():
	manager = HCIManager(debug_mode=False)
	#manager.load_as_text(["Test message 1", "Test message 2", "Test message 3", "Test message 4", "Test message 5"])
	manager.start()

if __name__== "__main__":
    main()