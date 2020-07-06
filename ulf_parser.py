import re
from ulf_grammar import *

class ULFParser(object):
    def __init__(self):
        self.verbose = False

    def parse(self, ulf):        
        ulf = self.preprocess(ulf)
        self.query_tree = self.parse_tree(ulf)
        return self.query_tree

    # def add_pipes(self, ulf):
    #     ulf = ulf.replace("Target", "|Target|")
    #     ulf = ulf.replace("Toyota", "|Toyota|")
        
    def preprocess(self, ulf):
        ulf = ulf.lower()
        ulf = self.replace_expr(ulf)
        ulf = self.lispify(ulf)  
        if self.verbose:
            print ("\nQUERY: ", ulf)        
        ulf = self.process_sub_rep(ulf)
        if self.verbose:
            print ("PRELIFT QUERY: ", ulf)
        ulf = self.lift(ulf, ['pres', 'prog', 'pref'])
        #ulf = self.propagate_conj(ulf, [])[0]
        #print ("AFTER LIFTING: ", ulf)        
        ulf = self.add_brackets(ulf)
        if self.verbose:
            print ("PROCESSED QUERY: ", ulf, "\n")
        return ulf

    def replace_expr(self, ulf):
        ulf = ulf.replace("(at.p (what.d place.n))", "where.a")
        ulf = ulf.replace("{", "")
        ulf = ulf.replace("}", "")
        ulf = ulf.replace("(of.p (what.d color.n))", "color.pred")
        ulf = ulf.replace("does.v", "(pres be.v)")
        ulf = ulf.replace("(each.d other.n)", "each_other.pro")
        if re.search(r'^\(\(\(pres be.v\) there.pro', ulf) is not None:
            ulf = ulf.replace("(pres be.v) there.pro", "exist.pred")
        return ulf

    def parse_tree(self, tree):
        if tree == []:
            return TEmpty()
        if type(tree) == str:
            if tree in grammar:
                return grammar[tree](tree)
            else:
                print ("UNKNOWN TOKEN DETECTED IN ULF TREE: " + tree)
                return TUnknown(tree)

        #print ("INITIAL TREE: ", tree)
        tree = [self.parse_tree(node) for node in tree]
        if self.verbose:
            print ("TREE BEFORE COLLAPSING: ", tree, " \n" + "\n".join(["\t" + node.__str__() for node in tree]))
        
        if type(tree[0]) == TNModMarker:
            #print ("CURRENT TREE: ", tree)
            ret = NArg(obj_type=tree[1].obj_type, obj_id=tree[1].obj_id, mods = tree[1].mods + tree[2:], det=tree[1].det, plur=tree[1].plur)
            #ret = tree[1].update(mods=tree[2:])
            #print ("POSTNOM MODS: ", ret.__str__())
            return ret

        while len(tree) >= 2 and (tree[0].__name__, tree[1].__name__) in grammar:
           # if tree[0].__name__ == "NArg" and tree[1].__name__ == "NRel" and tree[1] in tree[0].mods:
           #     print ("ERROR!!!: ",tree[0],tree[1])
            substitute = grammar[(tree[0].__name__, tree[1].__name__)](tree[0], tree[1])
            tree = [substitute] + tree[2:]

        while len(tree) >= 2 and (tree[-2].__name__, tree[-1].__name__) in grammar:
            substitute = grammar[(tree[-2].__name__, tree[-1].__name__)](tree[-2], tree[-1])
            tree = tree[:-3] + [substitute]

        #print ("TREE AFTER COLLAPSING: ", tree)
        if self.verbose:
            print ("TREE AFTER COLLAPSING: ", tree, "\n" + "\n".join(["\t" + node.__str__() for node in tree]))
       
        return tree[0]

    def lift(self, ulf, lifted_tokens):
        """Lifts the tokens appearing in the 'lifted_tokens' list from the ulf."""

        """If the current level is terminal, i.e., leaf node"""
        if type(ulf) != list:
            if ulf in lifted_tokens:
                return [[ulf], []]
            else:
                return [[], ulf]

        """If the current level is non-terminal"""
        lifted = []
        ret_ulf = []
        for item in ulf:
            ret_val = self.lift(item, lifted_tokens)            
            lifted += ret_val[0]
            if ret_val[1] != []:
                ret_ulf += [ret_val[1]]

        if len(ret_ulf) == 1:
            ret_ulf = ret_ulf[0]

        return [lifted, ret_ulf]
    
    def add_brackets(self, ulf):
        '''Adds brackets in different places in ulf if missing

        '''
        if type(ulf) == list:
            if len(ulf) == 3 and ulf[0] == "most-n":
                #print ("MOSTNNN:", ulf)
                return [[ulf[0], ulf[1]], self.add_brackets(ulf[2])]
            else:
                return [self.add_brackets(item) for item in ulf]
        else:
            return ulf


    '''def process_sub(self, tree, sub_expr=None):
        if type(tree) == list:
            if tree[0] == 'sub':
                return self.process_sub(tree[2], tree[1])
            else:
                return [self.process_sub(branch, sub_expr) for branch in tree]
        else:
            return sub_expr if (tree == "*h" and sub_expr is not None) else tree'''
    
    def process_sub_rep(self, tree, sub_expr=None, rep_expr=None):
        #print ("SUB_REP:", tree, sub_expr, rep_expr)
        if type(tree) == list:
            if tree[0] == 'sub':
                return self.process_sub_rep(tree[2], self.process_sub_rep(tree[1], sub_expr, rep_expr), rep_expr)
            elif tree[0] == "rep":
                return self.process_sub_rep(tree[1], sub_expr, self.process_sub_rep(tree[2], sub_expr, rep_expr))
            else:
                return [self.process_sub_rep(branch, sub_expr, rep_expr) for branch in tree]
        else:
            return sub_expr if (tree == "*h" and sub_expr is not None) else \
                rep_expr if (tree == "*p" and rep_expr is not None) else tree

    def lispify(self, input):
        stack = []
        current = []
        token = ""
        for char in input:
            if char == '(':
                stack.append(current)
                current = []
            elif char == ')':
                if token != "":
                    current += [token]
                    token = ""                
                if (len(stack) > 0):
                    stack[-1].append(current)
                    current = stack[-1]
                    stack.pop()                    
            elif char == ' ':
                if token != "":
                    current += [token]
                    token = ""
            else:
                token += char
        return current[0]

    def propagate_conj(self, ulf, prefix=[]):
        if type(ulf) == str and ".cc" not in ulf:
            return (ulf, False)
        for item in ulf:
            if ".cc" in item:
                ulf.remove(item)
                ret_val = [item]
                for token in ulf:                    
                    for pref_item in prefix:
                        token = [pref_item, token]
                    ret_val += [token]
                return (ret_val, True)
        
        if (ulf[0][-2:] == ".a" or ulf[0][-2:] == ".d"):
            ret_val, val = self.propagate_conj(ulf[1], [ulf[0]] + prefix)
            if val == True:
                return (ret_val, True)
            else:
                return (ulf, False)

        return ([self.propagate_conj(item, prefix)[0] for item in ulf], False)

    def propagate_conj1(self, ulf):
        if type(ulf) == str:
            if ".cc" not in ulf:
                return (ulf, False)
            else:
                return (ulf, True)
        ulf = list(map(propagate_conj1, ulf))
        for item in ulf:
            content, is_conj = item
            if is_conj == True:
                ulf.remove(item)
                ret_val = [item[0]] + list(map(lambda x: [x[0]], ulf))
                return ([item] + list(map(lambda x: [x], ulf)), True)
        
        if (ulf[0] == "k" or ulf[0][-2:] == ".d"):
            ret_val, val = self.propagate_conj(ulf[1], [ulf[0]] + prefix)
            if val == True:
                return (ret_val, True)
            else:
                return (ulf, False)

        return ([self.propagate_conj(item, prefix)[0] for item in ulf], False)