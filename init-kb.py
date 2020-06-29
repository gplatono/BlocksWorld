def generate_facts():
	return ['(on b1 table)', '(on b2 table)', '(on b3 table)', '(on b4 table)',
	'(block b1)', '(block b2)', '(block b3)', '(block b4)', '(red b1)', '(red b2)', '(blue b3)', '(blue b4)',
	'(available b1)', '(available b2)', '(available b3)', '(available b4)', '(available table)']
# (on B2 table) (on B3 table) \
#    (on B5 B6) (on B6 table) (block b1) (block b2) (block b3) (block b4) (block b5) \
#    (block b6) (red b1) (red b2) (blue b3) (blue b4) (red b5) (red b6) \
#    (between B3 B2 B6) (between B3 B1 B6) (between B1 B2 B3) (between B5 B4 B6)'

kb = open('init1.lisp', 'w')

kb.write('(defparameter *kb-ht* (make-hash-table :test #\'equal))\n')

facts = generate_facts()
fact_str = '\'(' + ' '.join(facts) + ')'
kb.write('(store-facts ' + fact_str + ' *kb-ht*)')