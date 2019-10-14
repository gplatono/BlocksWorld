# BlocksWorld
Main repository for the Blocks World project.

asr.py - externalized speech recognition code (basically a copy of the code from
HCI_manager put in a separate file for a better modularity, but currently unused).

bw_tracker.py - implementation of the Blocks World tracker.

constraint_solver.py - solver for spatial questions.

gcs_micstream.py - Google Cloud Speech microphone stream processing. It is a modified copy of 
https://github.com/GoogleCloudPlatform/python-docs-samples/blob/master/speech/microphone/transcribe_streaming_indefinite.py
See inside for the copyright notice, etc.

geoetric_utils.py - library of low-level geometric functions, e.g., computing various distances,
angles, vector products, etc.

hci_manager.py - control loop of the BW system, listen to the user, calls spatial 
reasoning component to answer questions, etc.

main.py - main BW executable, essentially a bootstrapper for other components.

spatial.py - implementation of spatial relations.

start.py - launches the entire BW system, by starting the ETA dialog manager and 
executing main.py from inside Blender.

ulf_grammar.py - grammar for the ULF parser.

ulf_parser.py - parser of the ULF into query frame representation.

world.py - general class that keeps track of the blocks world, that is creates entities 
from Blender objects and keep track of them by using the BW tracker class.
