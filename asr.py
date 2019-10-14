from gcs_micstream import ResumableMicrophoneStream
from google.cloud import speech
import sys
import os

class ASR(object):
	def __init__(self):
		self.sample_rate = 16000
		self.chunk_size = int(self.sample_rate / 10)  # 100ms
		open('speech_input', 'w').close()

	def send(self, text):
		with open('speech_input', 'w') as file:
			file.write(text + os.linesep)

	def mic_loop(self):
			"""The mic listening loop."""			
			client = speech.SpeechClient()
			config = speech.types.RecognitionConfig(encoding=speech.enums.RecognitionConfig.AudioEncoding.LINEAR16,
				sample_rate_hertz=self.sample_rate, language_code='en-US', max_alternatives=1, enable_word_time_offsets=True,
				 enable_automatic_punctuation=True)
			streaming_config = speech.types.StreamingRecognitionConfig(config=config, interim_results=True)

			mic_manager = ResumableMicrophoneStream(self.sample_rate, self.chunk_size)

			print ("Starting the listening loop...")

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

						if not result.is_final:
							sys.stdout.write(transcript + overwrite_chars + '\r')
							sys.stdout.flush()
							num_chars_printed = len(transcript)
						else:
							transcript = transcript.lower().strip()
							print (transcript)
							if transcript != "":
								self.send(transcript)
							#self.speech_lock.acquire()
							#self.current_input = transcript.lower()
							#self.speech_lock.release()
							num_chars_printed = 0
							# # Exit recognition if any of the transcribed phrases could be one of our keywords.
							# if re.search(r'\b(exit|quit)\b', transcript, re.I):
							# 	print('Exiting..')
							# 	stream.closed = True
							#break


def main():
	ASR().mic_loop()
	
if __name__== "__main__":
	main()