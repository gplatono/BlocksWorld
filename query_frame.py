import enum
import re
from ulf_grammar import *

class QueryFrame(object):
	"""Represents and incapsulates the query data in a frame-like format."""

	class ContentType(enum.Enum):
		"""The nature of the qury top-level node,
		can predicate/relation or argument."""		
		PRED = 0
		ARG = 1

	class QueryType(enum.Enum):
		"""Possible categories of questions (subject to a change)."""
		EXIST = 0
		CONFIRM = 1
		IDENT = 2
		DESCR = 3
		COUNT = 4
		ATTR_COLOR = 5
		ATTR_ORIENT = 6
		ERROR = 7

	def __init__(self, query_surface=None, query_ulf=None, query_parse_tree=None):

		"""
		Assume that initially the query is erroneous
		and then proceed to check if that is true.
		"""
		self.query_type = self.QueryType.ERROR

		if query_surface is None or query_ulf is None or query_parse_tree is None:
			return
		
		self.surface = query_surface.lower()
		self.ulf = query_ulf.lower()
		self.raw = query_parse_tree.content

		print ("QUERY REPRESENTATIONS: ")
		print (self.surface)
		print (self.ulf)
		print (self.raw)

		self.is_question = query_parse_tree.is_question

		self.YN_FLAG = False
		self.COUNT_FLAG = False
		self.EXIST_FLAG = False
		self.IDENT_FLAG = False
		self.DESCR_FLAG = False

		self.arg = None
		
		self.predicate = None
		self.relatum = None
		self.referent = None
		self.resolve_relatum = False
		self.resolve_referent = False

		if type(self.raw) == NArg or type(self.raw) == NConjArg:
			self.content_type = self.ContentType.ARG
			self.arg = query_parse_tree.content			
		else:
			self.content_type = self.ContentType.PRED
			self.predicate = query_parse_tree.content
			self.relatum = self.predicate.children[0]
			if len(self.predicate.children) > 1:
				self.referent = self.predicate.children[1]

		if self.relatum is not None and type(self.relatum) == NArg:
			self.resolve_relatum = self.resolve_arg(self.relatum)
		if self.referent is not None and type(self.referent) == NArg:
			self.resolve_referent = self.resolve_arg(self.referent)

		#print ("RESOLVE WHICH ARGS: ", self.resolve_relatum, self.resolve_referent)
		#print ("BEFORE ENTERING QUERY TPYE:")
		self.scan_type()

		self.arg0_singular = self.is_singular(self.arg if self.arg is not None else self.relatum)
		self.arg1_singular = self.is_singular(self.referent)

		self.is_subject_plural = self.subj_plural()


		#print ("QUERY CONTENT:")
		#print ("PREDICATE: ", self.predicate)
		#print ("RELATUM: ", self.relatum)
		#print ("REFERENT: ", self.referent)
		#print ("RESOLVE RELATUM: ", self.resolve_relatum)
		#print ("RESOLVE REFERENT: ", self.resolve_referent)

	def resolve_arg(self, arg):
		print ("\n\nRESOLVING ARG: ", arg, "\n")
		if arg.obj_id is None:
			return True
		if arg.det is not None:
			if type(arg.det) == NCardDet or arg.det.content in ["which.d", "what.d", "HOWMANY", "how_many.d"]:
				return True
		return False

	def scan_type(self):
		self.YN_FLAG = True if re.search(r'^\(*(pres|past|pres perf\)|pres prog\)|prog) (be.v|do.aux|can.aux)', self.ulf, re.IGNORECASE) else False
		self.COUNT_FLAG = True if re.search(r'^\(*(how.adv-a many.a|how_many.d)', self.ulf, re.IGNORECASE) else False
		
		if re.search('^.*(how.mod-a many.a|how_many.d)', self.ulf, re.IGNORECASE):
			self.COUNT_FLAG = True
		
		self.IDENT_FLAG = True if re.search('^.*(what.d|which.d).*(block.n).*(be.v)', self.ulf, re.IGNORECASE) else False
		self.IDENT_FLAG = True if re.search('^.*(what.pro|which.pro).*(be.v)', self.ulf, re.IGNORECASE) else self.IDENT_FLAG
		self.IDENT_FLAG = True if re.search('(what.pro|which.pro|which.d|what.d)', self.ulf, re.IGNORECASE) else self.IDENT_FLAG

		if re.search(r'^\(*what.pro', self.ulf, re.IGNORECASE):
			self.IDENT_FLAG = True

		self.DESCR_FLAG = True if re.search(r'^\(*(where).*(be.v).*\|.*\|.* block.n', self.ulf, re.IGNORECASE) else False
		self.DESCR_FLAG = True if re.search(r'at.p \(what.d place.n\)', self.ulf, re.IGNORECASE) else self.DESCR_FLAG		

		if "does.v" in self.ulf or (self.predicate is not None and type(self.predicate.content) == TCopulaBe):
			self.YN_FLAG = True

		if "is there" in self.surface:
			self.EXIST_FLAG = True

		if self.predicate is not None and self.predicate.content == "exist.pred":
			self.EXIST_FLAG = True

		self.COLOR_FLAG = 'what.d color.n' in self.ulf
		
		if self.COLOR_FLAG:
			self.query_type = self.QueryType.ATTR_COLOR
		elif self.DESCR_FLAG:
			self.query_type = self.QueryType.DESCR
		elif self.IDENT_FLAG:
			self.query_type = self.QueryType.IDENT
		elif self.COUNT_FLAG:
			self.query_type = self.QueryType.COUNT		
		elif self.EXIST_FLAG:
			self.query_type = self.QueryType.EXIST	
		elif self.YN_FLAG:
			self.query_type = self.QueryType.CONFIRM		

	def extract_subject_adj_modifiers(self):
		if self.arg is not None:
			mods = self.arg.mods
		else:
			mods = self.predicate.children[0].mods
		adjectives = []
		if mods is not None:
			for mod in mods:
				if type(mod) == TAdj or type(mod) == NColor:
					adjectives.append(mod.content.replace(".a", ""))
		return adjectives

	def subj_plural(self):
		if self.arg is not None:
			return self.arg.plur
		else: 
			return self.predicate.children[0].plur

	def is_singular(self, arg):
		if type(arg) == NArg:
			print ("RESOLVING SINGULARITY OF: ", arg, (not (arg is not None) and (arg.plur == True or arg.obj_type is None)))
			return (arg is not None) and (not (arg.plur == True or arg.obj_type is None))
		elif type(arg) == NConjArg:
			sing = [int(self.is_singular(item)) for item in arg.children]
			sing = sum(sing)
			return sing > 0
		else:
			return False