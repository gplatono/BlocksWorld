class TreeNode(object):
    __name__ = "TreeNode"
    def __init__(self, content=None, children=None):
        self.content = content
        self.children = children

    def __str__(self):
        if self.content is None and self.__name__ != "NArg":
            return self.__name__
        output = self.__name__ + ": "
        if type(self.content) == str:
            output += self.content
        elif self.content is not None:
            output += self.content.__str__()

        if type(self.children) == list and len(self.children) > 0:
            output += "[" + ";".join([child.__str__() for child in self.children]) + "]"
        return output

relations = ['on.p', 'to_the_left_of.p', 'to_the_right_of.p', 'in_front_of.p', 'behind.p', 'above.p', 'below.p', 'over.p', 'under.p', 'near.p', 'touching.p', 'at.p', 'between.p',
             'side-by-side-with.p', 'on_top_of.p']

grammar = {}

grammar['on.p'] = lambda x: TPred(x)
grammar['to_the_left_of.p'] = lambda x: TPred(x)
grammar['to_the_right_of.p'] = lambda x: TPred(x)
grammar['in_front_of.p'] = lambda x: TPred(x)
grammar['behind.p'] = lambda x: TPred(x)
grammar['above.p'] = lambda x: TPred(x)
grammar['below.p'] = lambda x: TPred(x)
grammar['over.p'] = lambda x: TPred(x)
grammar['under.p'] = lambda x: TPred(x)
grammar['underneath.p'] = lambda x: TPred(x)
grammar['near.p'] = lambda x: TPred(x)
grammar['touching.p'] = lambda x: TPred(x)
grammar['at.p'] = lambda x: TPred(x)
grammar['in.p'] = lambda x: TPred(x)
grammar['between.p'] = lambda x: TPred(x)
grammar['side_by_side_with.p'] = lambda x: TPred(x)
grammar['on_top_of.p'] = lambda x: TPred(x)
grammar['close_to.p'] = lambda x: TPred(x)
grammar['near_to.p'] = lambda x: TPred(x)
grammar['next_to.p'] = lambda x: TPred(x)
grammar['far_from.p'] = lambda x: TPred(x)
grammar['touching.p'] = lambda x: TPred(x)
grammar['supporting.p'] = lambda x: TPred(x)
grammar['exist.pred'] = lambda x: TPred(x)
grammar['facing.p'] = lambda x: TPred(x)
grammar['touch.v'] = lambda x: TPred(x)
grammar['contain.v'] = lambda x: TPred(x)
grammar['consist_of.v'] = lambda x: TPred(x)
grammar['face.v'] = lambda x: TPred(x)
grammar['have.v'] = lambda x: TPred(x)
grammar['color.pred'] = lambda x: TPred(x)
grammar['put.v'] = lambda x: TPred(x)
grammar['move.v'] = lambda x: TPred(x)
grammar['of.p'] = lambda x: TPred(x)

grammar['side_by_side.a'] = lambda x: TPred(x)
grammar['where.a'] = lambda x: TPred(x)
grammar['clear.a'] = lambda x: TPred(x)
grammar['leftmost.a'] = lambda x: NPred(x, mods = [TSuperMarker()])
grammar['rightmost.a'] = lambda x: NPred(x, mods = [TSuperMarker()])
grammar['frontmost.a'] = lambda x: NPred(x, mods = [TSuperMarker()])
grammar['topmost.a'] = lambda x: NPred(x, mods = [TSuperMarker()])
grammar['highest.a'] = lambda x: NPred(x, mods = [TSuperMarker()])
grammar['lowest.a'] = lambda x: NPred(x, mods = [TSuperMarker()])
grammar['backmost.a'] = lambda x: NPred(x, mods = [TSuperMarker()])
grammar['farthest.a'] = lambda x: NPred(x, mods = [TSuperMarker()])
#grammar['nearest_to.a'] = lambda x: NPred(x, mods = [TSuperMarker()])

#grammar['block.n'] = lambda x: TNoun(x)
#grammar['{block}.n'] = lambda x: TNoun(x)
grammar['block.n'] = lambda x: NArg(obj_type = x)
grammar['one.n'] = lambda x: NArg(obj_type = x)
grammar['{block}.n'] = lambda x: NArg(obj_type = x)
grammar['table.n'] = lambda x: NArg(obj_type = x, obj_id = "TABLE")
grammar['stack.n'] = lambda x: NArg(obj_type = x, obj_id = "STACK")
grammar['row.n'] = lambda x: NArg(obj_type = x, obj_id = "ROW")
grammar['thing.n'] = lambda x: NArg(obj_type = x, obj_id = None)
grammar['{thing}.n'] = lambda x: NArg(obj_type = x, obj_id = None)
grammar['what.pro'] = lambda x: NArg()
grammar['which.pro'] = lambda x: NArg()
grammar['anything.pro'] = lambda x: NArg()
grammar['something.pro'] = lambda x: NArg()
grammar['each_other.pro'] = lambda x: NArg(obj_type="EACHOTHER")

grammar['color.n'] = lambda x: NArg(obj_type = "PROPERTY", obj_id = "color")
grammar['direction.n'] = lambda x: NArg(obj_type = "PROPERTY", obj_id = "direction")
grammar['*ref'] = lambda x: NArg(obj_type = "REF", obj_id = "*ref")
grammar['it.pro'] = lambda x: NArg(obj_type = "REF", obj_id = x)
grammar['there.pro'] = lambda x: TTherePro()

grammar['corner-of.n'] = lambda x: TRelNoun(x)
grammar['edge-of.n'] = lambda x: TRelNoun(x)
grammar['side-of.n'] = lambda x: TRelNoun(x)
grammar['center-of.n'] = lambda x: TRelNoun(x)
grammar['middle-of.n'] = lambda x: TRelNoun(x)
grammar['part-of.n'] = lambda x: TRelNoun(x)
grammar['height-of.n'] = lambda x: TRelNoun(x)
grammar['length-of.n'] = lambda x: TRelNoun(x)

grammar['halfway.adv-a'] = lambda x: TAdv(x)
grammar['slightly.adv-a'] = lambda x: TAdv(x)
grammar['directly.adv-a'] = lambda x: TAdv(x)
grammar['fully.adv-a'] = lambda x: TAdv(x)

grammar['how.mod-a'] = lambda x: TAdvAdjMod(x)
grammar['very.mod-a'] = lambda x: TAdvAdjMod(x)
grammar['halfway.mod-a'] = lambda x: TAdvAdjMod(x)
grammar['slightly.mod-a'] = lambda x: TAdvAdjMod(x)
grammar['directly.mod-a'] = lambda x: TAdvAdjMod(x)

grammar['red.a'] = lambda x: NColor(x)
grammar['green.a'] = lambda x: NColor(x)
grammar['blue.a'] = lambda x: NColor(x)
grammar['yellow.a'] = lambda x: NColor(x)
grammar['black.a'] = lambda x: NColor(x)
grammar['white.a'] = lambda x: NColor(x)
grammar['brown.a'] = lambda x: NColor(x)
grammar['gray.a'] = lambda x: NColor(x)



grammar['left.a'] = lambda x: TAdj(x)
grammar['right.a'] = lambda x: TAdj(x)
grammar['top.a'] = lambda x: TAdj(x)
grammar['front.a'] = lambda x: TAdj(x)
grammar['back.a'] = lambda x: TAdj(x)
grammar['high.a'] = lambda x: TAdj(x)
grammar['upper.a'] = lambda x: TAdj(x)
grammar['low.a'] = lambda x: TAdj(x)
grammar['lower.a'] = lambda x: TAdj(x)
grammar['last.a'] = lambda x: TAdj(x)
grammar['first.a'] = lambda x: TAdj(x)
grammar['short.a'] = lambda x: TAdj(x)
grammar['long.a'] = lambda x: TAdj(x)
grammar['middle.a'] = lambda x: TAdj(x)
grammar['tall.a'] = lambda x: TAdj(x)
grammar['many.a'] = lambda x: TAdj(x)
grammar['far.a'] = lambda x: TAdj(x)
grammar['same.a'] = lambda x: TAdj(x)

grammar['one.a'] = lambda x: TNumber(x)
grammar['one.d'] = lambda x: TNumber(x)
grammar['two.a'] = lambda x: TNumber(x)
grammar['two.d'] = lambda x: TNumber(x)
grammar['three.a'] = lambda x: TNumber(x)
grammar['three.d'] = lambda x: TNumber(x)
grammar['few.a'] = lambda x: TNumber(x)
grammar['several.a'] = lambda x: TNumber(x)

grammar['do.aux-s'] = lambda x: TAuxSDo()
grammar['be.v'] = lambda x: TCopulaBe()
grammar['plur'] = lambda x: TPlurMarker()
grammar['pres'] = lambda x: TTenseMarker()
grammar['nquan'] = lambda x: TQuanMarker()
grammar['fquan'] = lambda x: TQuanMarker()
grammar['most-n'] = lambda x: TSuperMarker()
grammar['most'] = lambda x: TSuperMarker()
grammar['sub'] = lambda x: TSubMarker()
grammar['?'] = lambda x: TQMarker()
grammar['n+preds'] = lambda x: TNModMarker()
grammar['prog'] = lambda x: TAspectMarker(prog=True, perf=False)
grammar['perf'] = lambda x: TAspectMarker(prog=False, perf=True)
grammar['k'] = lambda x: TNReifierMarker()
grammar['='] = lambda x: TEqualsMarker()
grammar['adv-a'] = lambda x: TAdvTransformMarker(x)
grammar['adv-s'] = lambda x: TAdvTransformMarker(x)
grammar['adv-e'] = lambda x: TAdvTransformMarker(x)
grammar['adv-f'] = lambda x: TAdvTransformMarker(x)


grammar['that.rel'] = lambda x: TRelativizer(content=x)

grammar['which.d'] = lambda x: TDet(x)
grammar['the.d'] = lambda x: TDet(x)
grammar['a.d'] = lambda x: TDet(x)
grammar['other.d'] = lambda x: TDet(x)
grammar['another.d'] = lambda x: TDet(x)
grammar['every.d'] = lambda x: TDet(x)
grammar['other.a'] = lambda x: TDet(x)
grammar['any.d'] = lambda x: TDet(x)
grammar['some.d'] = lambda x: TDet(x)
grammar['what.d'] = lambda x: TDet(x)
grammar['all.d'] = lambda x: TDet(x)
grammar['how_many.d'] = lambda x: TDet(x)

grammar['Nvidia|'] = lambda x: TName(x)
grammar['Toyota'] = lambda x: TName(x)
grammar['McDonalds'] = lambda x: TName(x)
grammar['SRI'] = lambda x: TName(x)
grammar['Starbucks'] = lambda x: TName(x)
grammar['Texaco'] = lambda x: TName(x)
grammar['Target'] = lambda x: TName(x)
grammar['Burger_King'] = lambda x: TName(x)
grammar['Mercedes'] = lambda x: TName(x)
grammar['Twitter'] = lambda x: TName(x)
grammar['HP'] = lambda x: TName(x)
grammar['Shell'] = lambda x: TName(x)
grammar['Heineken'] = lambda x: TName(x)

grammar['nvidia|'] = lambda x: TName(x)
grammar['toyota'] = lambda x: TName(x)
grammar['mcdonalds'] = lambda x: TName(x)
grammar['sri'] = lambda x: TName(x)
grammar['starbucks'] = lambda x: TName(x)
grammar['texaco'] = lambda x: TName(x)
grammar['target'] = lambda x: TName(x)
grammar['burger_king'] = lambda x: TName(x)
grammar['mercedes'] = lambda x: TName(x)
grammar['twitter'] = lambda x: TName(x)
grammar['hp'] = lambda x: TName(x)
grammar['shell'] = lambda x: TName(x)
grammar['heineken'] = lambda x: TName(x)
grammar['burger'] = lambda x: TName(x)
grammar['king'] = lambda x: TName(x)

grammar['|Nvidia|'] = lambda x: TName(x)
grammar['|Toyota|'] = lambda x: TName(x)
grammar['|McDonalds|'] = lambda x: TName(x)
grammar['|SRI|'] = lambda x: TName(x)
grammar['|Starbucks|'] = lambda x: TName(x)
grammar['|Texaco|'] = lambda x: TName(x)
grammar['|Target|'] = lambda x: TName(x)
grammar['|Burger_King|'] = lambda x: TName(x)
grammar['|Mercedes|'] = lambda x: TName(x)
grammar['|Twitter|'] = lambda x: TName(x)
grammar['|HP|'] = lambda x: TName(x)
grammar['|Shell|'] = lambda x: TName(x)
grammar['|Heineken|'] = lambda x: TName(x)

grammar['|nvidia|'] = lambda x: TName(x)
grammar['|toyota|'] = lambda x: TName(x)
grammar['|mcdonalds|'] = lambda x: TName(x)
grammar['|sri|'] = lambda x: TName(x)
grammar['|starbucks|'] = lambda x: TName(x)
grammar['|texaco|'] = lambda x: TName(x)
grammar['|target|'] = lambda x: TName(x)
grammar['|nvidia|.n'] = lambda x: TName(x)
grammar['|toyota|.n'] = lambda x: TName(x)
grammar['|mcdonalds|.n'] = lambda x: TName(x)
grammar['|sri|.n'] = lambda x: TName(x)
grammar['|starbucks|.n'] = lambda x: TName(x)
grammar['|texaco|.n'] = lambda x: TName(x)
grammar['|target|.n'] = lambda x: TName(x)

grammar['not.adv-s'] = lambda x: TNeg()
grammar['not'] = lambda x: TNeg()
grammar['not.adv-a'] = lambda x: TNeg()

grammar['or.cc'] = lambda x: TConj(x)
grammar['and.cc'] = lambda x: TConj(x)


grammar[("TName", "TName")] = lambda x, y: TName(content=x.content + " " + y.content)

#Verb + tense/aspect rules
grammar[("TTenseMarker", "TCopulaBe")] = lambda x, y: NVP(content=y, children=[NSentenceParams(tense=x)])
grammar[("NSentenceParams", "TCopulaBe")] = lambda x, y: NVP(content=y, children=[x])
grammar[("TTenseMarker", "TVerb")] = lambda x, y: NVP(content=y, children=[NSentenceParams(tense=x)] + y.children)
grammar[("NSentenceParams", "TVerb")] = lambda x, y: NVP(content=y, children=[x] + y.children)
grammar[("TTenseMarker", "TAuxSDo")] = lambda x, y: NVP(content=y, children=[NSentenceParams(tense=x)])
grammar[("NSentenceParams", "TAuxSDo")] = lambda x, y: NVP(content=y, children=[x] + y.children)

grammar[("TAspectMarker", "TAspectMarker")] = lambda x, y: TAspectMarker(prog = x.prog or y.prog, perf = x.perf or y.perf)
grammar[("TTenseMarker", "TAspectMarker")] = lambda x, y: NSentenceParams(tense = x, aspect = y)

#Adjective modifier rules
grammar[("TSuperMarker", "TAdj")] = lambda x, y: TAdj(content = y.content, mods = [x])
grammar[("TAdvAdjMod", "TAdj")] = lambda x, y: TAdj(content = y.content, mods = [x])
grammar[("TNeg", "TAdj")] = lambda x, y: TAdj(content = y.content, mods = [x])

#Determiner rules
grammar[("TQuanMarker", "TAdj")] = lambda x, y: NDet(y) if (y.content != "many.a" or y.mods == [] or y.mods[0].content != "how.mod-a") else NCardDet()

#Argument + modifier rules
grammar[("TName", "NArg")] = lambda x, y: NArg(obj_type = y.obj_type, obj_id = x.content)
grammar[("TDet", "NArg")] = lambda x, y: NArg(obj_type = y.obj_type, obj_id = y.obj_id, mods = y.mods, det = x, plur = y.plur)
grammar[("NDet", "NArg")] = lambda x, y: NArg(obj_type = y.obj_type, obj_id = y.obj_id, mods = y.mods, det = x, plur = y.plur)
grammar[("NColor", "NArg")] = lambda x, y: NArg(obj_type = y.obj_type, obj_id = y.obj_id, mods = [x] + y.mods, det = x, plur = y.plur)
grammar[("NCardDet", "NArg")] = lambda x, y: NArg(obj_type = y.obj_type, obj_id = y.obj_id, mods = y.mods, det = x, plur = y.plur)

grammar[("TAdj", "NArg")] = lambda x, y: NArg(obj_type = y.obj_type, obj_id = y.obj_id, mods = y.mods + [x], det = y.det, plur = y.plur)
grammar[("TPlurMarker", "NArg")] = lambda x, y: NArg(obj_type = y.obj_type, obj_id = y.obj_id, mods = y.mods, det = y.det, plur = True)
grammar[("TNReifierMarker", "NArg")] = lambda x, y: y
grammar[("TEmpty", "NArg")] = lambda x, y: y
grammar[("NArg", "TEmpty")] = lambda x, y: x

grammar[("TRelNoun", "NArg")] = lambda x, y: NArg(obj_type = x.content[:-5], obj_id = x.content[:-5].upper(), mods = [y])

grammar[("TNumber", "NArg")] = lambda x, y: NArg(obj_type = y.obj_type, obj_id = y.obj_id, mods = y.mods + [x], det = y.det, plur = y.plur)
grammar[("TNeg", "NArg")] = lambda x, y: NArg(obj_type = y.obj_type, obj_id = y.obj_id, mods = y.mods + [x], det = y.det, plur = y.plur)

grammar[("TConj", "NArg")] = lambda x, y: NConjArg(x, children = [y])
grammar[("NArg", "TConj")] = lambda x, y: NConjArg(y, children = [x])
grammar[("Narg", "NConjArg")] = lambda x, y: NConjArg(y.content, children = y.children + [x])
grammar[("NConjArg", "NArg")] = lambda x, y: NConjArg(x.content, children = x.children + [y])
grammar[("NArg", "NArg")] = lambda x, y: NConjArg(TConj(), children = [x, y])

grammar[("TEqualsMarker", "NArg")] = lambda x, y: y
grammar[("NRel", "NArg")] = lambda x, y: NArg(obj_type = y.obj_type, obj_id = y.obj_id, mods = y.mods + [x], det = y.det, plur = y.plur)

#Relational rules
grammar[("TPrep", "NArg")] = lambda x, y: NRel(x, children=[y])
grammar[("TPrep", "NConjArg")] = lambda x, y: NRel(x, children=[y])

grammar[("TPred", "NArg")] = lambda x, y: NPred(x.content, children=[y])
grammar[("NArg", "TPred")] = lambda x, y : NPred(content = y.content, children = [x])
grammar[("TPred", "NConjArg")] = lambda x, y: NPred(x.content, children=[y])
grammar[("NConjArg", "NPred")] = lambda x, y: NPred(content=y.content, children=[x]+y.children, mods = y.mods)
grammar[("NConjArg", "TPred")] = lambda x, y: NPred(content=y.content, children=[x])
grammar[("TNeg", "NPred")] = lambda x, y: NPred(content=y.content, children=y.children, mods=y.mods+[x])
grammar[("TNeg", "TPred")] = lambda x, y: NPred(content=y.content, mods=[x])
grammar[("NSentenceParams", "NPred")] = lambda x, y: y
grammar[("TCopulaBe", "TPred")] = lambda x, y: y
grammar[("TCopulaBe", "NPred")] = lambda x, y: y
grammar[("TCopulaBe", "TAdj")] = lambda x, y: NPred(content = y.content, mods=y.mods)
grammar[("TCopulaBe", "NColor")] = lambda x, y: NPred(content = y.content)
# grammar[("TCopulaBe", "NRel")] = lambda x, y: y
grammar[("TCopulaBe", "NArg")] = lambda x, y: NPred(content = x, children = [y])
grammar[("NPred", "TPred")] = lambda x, y: NPred(content = y, children = x.children, neg = x.neg, mods = x.mods)

#grammar[("NVP", "NRel")] = lambda x, y: y


grammar[("TNeg", "NRel")] = lambda x, y: NRel(y.content, y.children, neg=True)
grammar[("NArg", "NRel")] = lambda x, y: NRel(content=y.content, children=[x]+y.children, neg = y.neg)
grammar[("NConjArg", "NRel")] = lambda x, y: NRel(content=y.content, children=[x]+y.children, neg = y.neg)
grammar[("NConjArg", "TPrep")] = lambda x, y: NRel(content=y, children=[x])

grammar[("NVP", "NRel")] = lambda x, y: y
grammar[("NVP", "TTherePro")] = lambda x, y: NPred(content = "EXIST")

#grammar[("TTherePro", "NPred")] = lambda x, y: NPred(content = "EXIST", children=y.children, neg = y.neg, mods = y.mods)
grammar[("TTherePro", "NPred")] = lambda x, y: y.children[0]
#grammar[("TTherePro", "NPred")] = lambda x, y: NPred(content = y.children[0].mods[1], children = 

grammar[("NSentenceParams", "NRel")] = lambda x, y: y

grammar[("NVP", "TAdj")] = lambda x, y: NRel(y, children=[])

#grammar[("TAdv", "NRel")] = lambda x, y: NRel(y.content, y.children, y.neg, y.mods + [x])
grammar[("TAdv", "NPred")] = lambda x, y: NPred(y.content, y.children, y.neg, y.mods + [x])

grammar[("TDet", "TPrep")] = lambda x, y: y
grammar[("TAdj", "TPrep")] = lambda x, y: y

grammar[("NVP", "NArg")] = lambda x, y: NPred(content = x, children = [y])

#Changed This!!!
#grammar[("NArg", "NPred")] = lambda x, y: NArg(obj_type = x.obj_type, obj_id = x.obj_id, mods = x.mods + [y], det = x.det, plur = x.plur)
grammar[("NArg", "NPred")] = lambda x, y: NPred(content = y.content, children = [x] + y.children, neg = y.neg, mods = y.mods)
grammar[("TEmpty", "NPred")] = lambda x, y: y
grammar[("NPred", "TEmpty")] = lambda x, y: x

grammar[("NPred", "NColor")] = lambda x, y: NPred(content = y.content, children = x.children, neg = x.neg, mods = x.mods)

grammar[("NPred", "NArg")] = lambda x, y: NPred(content = x.content, children = x.children + [y])

grammar[("TRelativizer", "NPred")] = lambda x, y: y
#grammar[("TRelativizer", "NRel")] = lambda x, y: y

grammar[("TSuperMarker", "NRel")] = lambda x, y: NRel(content = y.content, children = y.children, neg = y.neg, mods = y.mods + [x])

grammar[("TAdvTransformMarker", "NRel")] = lambda x, y: y
grammar[("TAdvAdjMod", "NRel")] = lambda x, y: NRel(content = y.content, children = y.children, neg = y.neg, mods = y.mods + [x])

grammar[("NPred", "NRel")] = lambda x, y : NPred(content = y.content, children = x.children + y.children, neg = y.neg, mods = y.mods)
grammar[("NPred", "NPred")] = lambda x, y : NPred(content = y.content, children = x.children + y.children, neg = y.neg, mods = y.mods)
grammar[("NArg", "TPrep")] = lambda x, y : NPred(content = y, children = [x])

grammar[("NRel", "NRel")] = lambda x, y : NPred(content = x.content, children = x.children, neg = x.neg, mods = x.mods + [y])

grammar[("NVerbParams", "NRel")] = lambda x, y : NPred(content = y.content, children = y.children, neg = y.neg, mods = [x] + y.mods)

#Sentence-level rules
#grammar[("NVerbParams", "NRel")] = lambda x, y : NSentence(content = y, is_question = False, tense = NVerbParams)
grammar[("NRel", "TQMarker")] = lambda x, y: NSentence(x, True)
grammar[("NArg", "TQMarker")] = lambda x, y: NSentence(x, True)
grammar[("NPred", "TQMarker")] = lambda x, y: NSentence(x, True)
grammar[("NSentence", "TQMarker")] = lambda x, y: NSentence(content = x.content, is_question = True, tense = x.tense)
grammar[("NSentenceParams", "NSentence")] = lambda x, y: NSentence(content = y.content, is_question = y.is_question, mods = y.mods + [x])
grammar[("TEmpty", "NSentence")] = lambda x, y: y
grammar[("TTenseMarker", "NSentence")] = lambda x, y: NSentence(content = y.content, is_question = y.is_question, mods = y.mods + [x])

class TEmpty(TreeNode):
    __name__ = "TEmpty"

class TRelativizer(TreeNode):
    __name__ = "TRelativizer"
    def __init__(self, content=None):
        super().__init__(content, None)

class TCopulaBe(TreeNode):
    __name__ = "TCopulaBe"
    def __init__(self, content=None):
        super().__init__(content, None)

class TAuxSDo(TreeNode):
    __name__ = "TAuxSDo"
    def __init__(self):
        super().__init__(None, None)
        
class TPlurMarker(TreeNode):
    __name__ = "TPlurMarker"
    def __init__(self):
        super(TPlurMarker, self).__init__(None, None)

class TQuanMarker(TreeNode):
    __name__ = "TQuanMarker"
    def __init__(self):
        super(TQuanMarker, self).__init__(None, None)

class TSuperMarker(TreeNode):
    __name__ = "TSuperMarker"
    def __init__(self, content=None):
        super(TSuperMarker, self).__init__(None, None)

class TNReifierMarker(TreeNode):
    __name__ = "TNReifierMarker"
    def __init__(self):
        super(TNReifierMarker, self).__init__()

class TAspectMarker(TreeNode):
    __name__ = "TAspectMarker"
    def __init__(self, prog=False, perf=False):
        super(TAspectMarker, self).__init__(None, None)
        self.prog = prog
        self.perf = perf

    def __str__(self):
        return "PROG=" + str(self.prog) + ":PERF=" + str(self.perf)

class TSubMarker(TreeNode):
    __name__ = "TSubMarker"
    def __init__(self):
        super().__init__(content, None)

class TEqualsMarker(TreeNode):
    __name__ = "TEqualsMarker"
    def __init__(self):
        super().__init__(None, None)

class TTherePro(TreeNode):
    __name__ = "TTherePro"
    def __init__(self, content=None):
        super().__init__()

class TAdvTransformMarker(TreeNode):
    __name__ = "TAdvTransformMarker"
    def __init__(self, content):
        super().__init__(content, None)


class TNeg(TreeNode):
    __name__ = "TNeg"
    def __init__(self):
        super().__init__(None, None)

    def __str__(self):
        return "NOT"

class TTenseMarker(TreeNode):
    __name__ = "TTenseMarker"
    def __init__(self, content=None):
        super().__init__(content, None)

class TQMarker(TreeNode):
    __name__ = "TQMarker"
    def __init__(self):
        super().__init__()

    def __str__(self):
        return "?"

class TNModMarker(TreeNode):
    __name__ = "TNModMarker"
    def __init__(self, content=None):
        super(TNModMarker, self).__init__()
      
class TDet(TreeNode):
    __name__ = "TDet"
    def __init__(self, content=None):
        super(TDet, self).__init__(content, None)

class TPrep(TreeNode):
    __name__ = "TPrep"
    def __init__(self, content=None):
        super(TPrep, self).__init__(content, None)

class TPred(TreeNode):
    __name__ = "TPred"
    def __init__(self, content=None):
        super().__init__(content)

class TNoun(TreeNode):
    __name__ = "TNoun"
    def __init__(self, content=None):
        super(TNoun, self).__init__(content, None)

class TRelNoun(TreeNode):
    __name__ = "TRelNoun"
    def __init__(self, content=None):
        super(TRelNoun, self).__init__(content, None)

class TAdvAdjMod(TreeNode):
    __name__ = "TAdvAdjMod"
    def __init__(self, content=None):
        super(TAdvAdjMod, self).__init__(content, None)

class TName(TreeNode):
    __name__ = "TName"
    def __init__(self, content=None):
        super().__init__(content.replace("|", "").replace(".n", ""))        

class TPro(TreeNode):
    __name__ = "TPro"
    def __init__(self, content=None):
        super(TPro, self).__init__(content, None)

class TNumber(TreeNode):
    __name__ = "TNumber"
    def __init__(self, content=None):
        super(TNumber, self).__init__(content, None)

class TAdj(TreeNode):
    __name__ = "TAdj"
    def __init__(self, content, mods=[]):
        super().__init__(content, None)
        self.mods = mods

    def __str__(self):
        return self.content if self.mods is [] else self.content + "; MOD=" + str(self.mods)

class TAdv(TreeNode):
    __name__ = "TAdv"
    def __init__(self, content=None):
        super(TAdv, self).__init__(content, None)

class TConj(TreeNode):
    __name__ = "TConj"
    def __init__(self, content=None):
        super(TConj, self).__init__(content, None)

class TUnknown(TreeNode):
    __name__ = "TUnknown"
    def __init__(self, content=None):
        super(TUnknown, self).__init__(content, None)

class NSentenceParams(TreeNode):
    __name__ = "NSentenceParams"
    def __init__(self, tense=None, aspect=None):
        self.tense = tense
        self.aspect = aspect

    def __str__(self):
        return self.tense.__str__() + ":" + self.aspect.__str__()

class NDet(TreeNode):
    __name__ = "NDet"
    def __init__(self, content=None):
        super(NDet, self).__init__(content, None)

class NColor(TreeNode):
    __name__ = "NColor"
    def __init__(self, content=None):
        super().__init__(content.replace(".a", ""), None)

class NCardDet(TreeNode):
    __name__ = "NCardDet"
    def __init__(self):
        super(NCardDet, self).__init__(None, None)

    def __str__(self):
        return "HOWMANY"

class NVP(TreeNode):
    __name__ = "NVP"
    def __init__(self, content, children=[]):
        self.content = content
        self.children = children
        
class NRel(TreeNode):
    __name__ = "NRel"
    def __init__(self, content, children=None, neg=False, mods=[]):
        #super().__init__(content, children)
        self.content = content
        self.children = children
        self.neg = neg
        self.mods = mods

    def __str__(self):
        output = "RELATION={"
        if self.neg == True:
            output += "NOT "
        for mod in self.mods:
            output += ": " + mod.__str__()
        output += self.content.__str__()
        for idx in range(len(self.children)):
            output += "\nARG" + str(idx) + " " + self.children[idx].__str__()
        output += "\n}"
        return output

class NPred(TreeNode):
    __name__ = "NPred"
    def __init__(self, content, children=[], neg=False, mods=[]):
        #super().__init__(content, children)
        self.content = content
        self.children = children
        self.neg = neg
        self.mods = mods

    def __str__(self):
        output = "PRED={"
        if self.neg == True:
            output += "NOT "
        output += "MODS: {"
        for mod in self.mods:
            output += mod.__str__() + "; "
        output += "}\nCONTENT: " + self.content.__str__()
        for idx in range(len(self.children)):
            output += "\nARG" + str(idx) + " " + self.children[idx].__str__()
        output += "\n}"
        return output

class NConjArg(TreeNode):
    __name__ = "NConjArg"
    def __init__(self, content, children=None):
        super().__init__(content, None)
        self.content = content
        self.children = children

class NArg(TreeNode):
    __name__ = "NArg"
    def __init__(self, obj_type=None, obj_id=None, mods=[], det=None, plur=False):
        #super().__init__(None, None)        
        self.obj_type = obj_type if obj_type is None else obj_type.replace(".n", "").replace("{", "").replace("}", "")
        if obj_type == "stack.n":
            print (".N CHECK!!!: ", obj_type, self.obj_type)
        self.obj_id = obj_id
        self.mods = mods
        self.det = det
        self.plur = plur

    def update(self, narg=None, obj_type=None, obj_id=None, mods=None, det=None, plur=None):
        if obj_type is not None:
            self.obj_type = obj_type
        elif narg is not None:
            self.obj_type = narg.obj_type
        if obj_id is not None:
            self.obj_id = obj_id
        elif narg is not None:
            self.obj_id = narg.obj_id
        if mods is not None:
            for item in mods:
                self.mods.append(copy.copy(item))
        elif narg is not None:
            self.mods += narg.mods
        if det is not None:
            self.det = det
        elif narg is not None:
            self.det = narg.det
        if plur is not None:
            self.plur = plur
        elif narg is not None:
            self.plur = narg.plur
        return self

    def __str__(self):        
        if self.mods is not None:
            for item in self.mods:
                if item is not None and hasattr(item, "children") and item.children is not None and self in item.children:
                    print ("ERROR!!!: ", self.obj_type, item.content)
                    return ""
        output = "ARGUMENT={" + str(self.obj_type)+"; " + str(self.obj_id) + "; " + str(self.det) + "; " + str(self.plur) + "; ["
        for mod in self.mods:
            #output += mod.__str__() + ", "
            output += str(mod) + ", "
        output+= "]}"
        return output

class NSentence(TreeNode):
    __name__ = "NSentence"
    def __init__(self, content, is_question=True, tense=None, mods=[]):
        super().__init__(content, None)
        self.content = content
        self.is_question = is_question
        self.tense = tense
        self.mods = mods