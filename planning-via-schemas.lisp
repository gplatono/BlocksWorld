;; Started Sep 30/17

;; Originally I worked on methods of constructing arbitrary block
;; configurations described logically (allowing for open variables),
;; starting from any arrangement of blocks on a table. This required
;; finding suitable available blocks (satisfying constraints such as
;; color constraints), and potentially exploiting existing structures
;; on the table to build desired structures. For example, suppose we
;; wanted a stack of four red blocks, and there was already a stack
;; with three "available" red blocks on the table, with a blue block on
;; top of them; then the desired structure could be created by unstacking 
;; the blue block, finding another available red block, digging it up
;; if necessary (i.e., no "clear" red block currently available), and
;; putting it on the three red blocks.
;;
;; After much work on general methods "finding objects satisfying 
;; a given description", I came to realize that this is a complex
;; problem, subsuming finding all ways of instantiating operator 
;; preconditions in a given situation (as in gridworld), but requiring
;; more if it is to be done efficiently -- projection, intersection,
;; and join operations, plus ways of dealing with disjunction and
;; universal instantiation.
;;
;; So it looks like coding the algorithm for bottom-up building
;; (or at least finding blocks that can be placed next, for use
;; in a general block-structure building schema) is the most
;; practical thing to do in the short run. In the longer run,
;; finding blocks to place next really requires a combination
;; of internal modelling and visual search; e.g., just consider
;; something as simple as "finding a red block, preferably within
;; easy reach and with as little as possible stacked on it"...


;; The following notes are copied from "./planning-via-schemas"
;; (working notes from summer 2017 - spring 2018):
;;
;; Action find-a-block-to-place(curr-state,goal-struct-name,available-blocks)
;; `````````````````````````````````````````````````````````````````````````
;; We can break the problem down as follows. We assume
;; that we have a representation of the goal structure, separate from the
;; representation of the actual situation (but any names occurring in the
;; goal structure should also occur in the latter representation). We will
;; predicate blocks that are already positioned-as-intended as being 
;; in-place (and the table is predicated from the outset as being in-place)
;; {this is a bit tricky -- 'in-place' is about actual objects, but it is
;; meaningful only within a (partially realized goal description}:
;; 
;; 1. When we're starting, with 'in-place' having no instances, we may 
;;    want to predicate all available-blocks as being 'available-for'
;;    the task (supplied above by name).
;;   
;;    {If we're in the middle of a construction task, some 'in-place' 
;;    predications might already be in the goal description, and some 
;;    variables of the goal description, if any, might be bound to 
;;    (equated with) certain specific objects; for such variables, 
;;    reference to them is equivalent to reference to their bindings. 
;;    When an object is put in place, the 'available-for' predication 
;;    is removed. However, we might return in-place objects bound to 
;;    variables to 'available-for' status, in case of construction
;;    failure. Note that we can't in general treat all objects that 
;;    are at hand but not marked as 'in-place' as available, because 
;;    the task specification might disallow use of certain objects that 
;;    are at hand. E.g., they might be reserved for other tasks.}
;; 
;; 2. Let goal-domain := all the terms in the goal structure;
;;    {But perhaps a goal description specified as a (static) schema
;;    already enumerates everything it involves in its participants
;;    and/or variables}
;; 
;; 3. IF
;;      SOME (?x ?y) in goal-domain are such that 
;;        (on ?x ?y) holds in the goal structure AND
;;        NOT SOME ?z in goal-domain is such that 
;;              ((on ?x ?z) AND (NOT (in-place ?z)))
;;        holds in the goal structure
;;    THEN 
;;      IF ?x is an unbound variable 
;;      THEN
;;        (props-of ?x) := nonfluent properties of ?x in the goal structure;
;;        SOME ?b in available-blocks is s.t. it has all the (props-of ?x)
;;               {i.e., replacing ?x by ?b in (props-of ?x) yields a set of 
;;               true facts};
;;        IF ?b is undefined {not enough blocks -- unless we made a bad
;;               arbitrary choice earlier taking a block needed later 
;;               "out of service"}
;;        THEN declare an impasse and quit;
;;        ELSE
;;          (req-supports-of ?x) := ALL ?c s.t. (on ?x ?c) in the goal structure;
;;            {The ?c items should be names even if the goal structure
;;            originally specified a variable}
;;          ?x := ?b {bind ?x in the goal structure to the block we found}
;;    ELSE {no on-relation in the goal structure is still open}
;;      RETURN task-completion to the superordinate program/schema;
;; 
;; 4. Position ?x {which is now a name or bound to a name} on 
;;      (req-supports-of ?x);
;;    Assert the (actual) on-relation between ?b and each support of ?b;
;; 
;; 5. Remove ?b from available-blocks
;; 
;; 6. Assert (in-place ?b) within the goal structure.
;; 
;; NEXT: Formalize the search actions, and the above "algorithm"
;; for placing one more target block; if sensible incorporate the entity
;; domains into the description (actual & desired) that they occur in.
;; Also it seems to make sense to make the descriptions, as schemas,
;; arguments of the search/find actions
;; 
;; Then formulate the full schema, with a repeat loop, for building a
;; desired structure, with the above as the main subplan, of course.
;; 


(defun place-block (var+bind supports goal-schema-inst curr-state-ht)
;```````````````````````````````````````````````````````````````````
; ** This takes for granted that the physical block-placement is done
;    successfully, so that the knowledge state can be updated accordingly.
;    The program should theoretically start with a call to a (physical)
;    executor, followed by a visual check whether the placement succeeded,
;    followed by the model updates we implement in this routine.
;
; var+bind: a pair like (?x B3), i.e., a variable with the name of the
;    block chosen to instantiate it (as the next block to be placed).
; supports: a list of supporting objects (usually a single block or 2
;    blocks, or the table) that the chosen block (in the example above,
;    B3) is to be placed on.
; goal-schema-inst: The variant of the original goal schema (created 
;    in the context of the collaborative construction schema) that
;    represents the current status of the partially achieved goal
;    schema. It will be destructively altered to reflect the binding
;    of the variable in var+bind to the binding it is paired with.
;      Goal schemas should contain variables for all blocks that are
;    to be put in place. This includes blocks whose identity is
;    specified in the goal schema; therefore, rather than directly
;    using names of such blocks in the constraints specified by the
;    schema, we use the corresponding variables, but equate these to
;    the desired constants, e.g.,  (eq ?z B1), (a nonfluent condition).
;    The reason for doing this is that we distinguish still-unplaced
;    blocks in the goal schema by keeping them on the :vars list without
;    a binding, while pairing already-placed blocks with their binding.
;    E.g., if we have goal schema constraint (eq ?z B1), we will initially
;    have ?z on the :vars list, and once B1 has been put in its desired
;    place, this becomes (?z B1).
; curr-state-ht: The hash table of facts representing the current blocks
;    world state. This contains both facts like (on B2 B1), (on B1 table)
;    and ones like (available B3) and (in-use B1). Availability generally
;    remains true for a block during the construction, whereas 'in-use'
;    is initially false for available blocks, and becomes true when 
;    they are made part of the intended goal structure. (Falsity of 
;    predications is represented by their absence from the world state,
;    i.e., we're making a closed-world assumption.) Predications can
;    be added to curr-state-ht using 'store-fact', or removed using
;    'remove-fact'. Storage is done under multiple keys, so that facts
;    matching a pattern (a predication containing question-mark variables)
;    can be retrieved.
;
 (let ((obj (second var+bind)) old-support-rels)
      (format t "~%## In place of this printout, the action of placing")
      (format t "~%   ~a on ~a should happen here" (second var+bind) supports)
      (bind-var-of goal-schema-inst var+bind)
      (store-fact (list 'in-use obj) curr-state-ht)
      ; remove previous support relations for the obj being placed;
      ; ** NB: This currently does not take care of possible dependent
      ;        relations such as supporting blocks becoming clear (which
      ;        depends on how many blocks can rest on another)
      (setq old-support-rels (get-matching-facts `(on ,obj ?x) curr-state-ht))
      (dolist (ss old-support-rels) (remove-fact ss curr-state-ht))
      (dolist (s supports) (store-fact (list 'on obj s) curr-state-ht))
 )); end of place-block
      

(defun copy-schema (schema)
;``````````````````````````
; For initializing a schema instance from a generic schema
;
 (if (atom schema) 
     schema
     (cons (car schema) 
           (copy-schema (cdr schema)))
 )); end of copy-schema

(defun declare-as-available (objs facts-name curr-state-ht)
;``````````````````````````````````````````````````````````
; Apply predicate 'available' to the given objects, adding these to the
; facts list that is the value of 'facts-name', and storing them in 
; the hash table 'curr-state-ht'
;
 (let (new-facts)
      (setq new-facts
         (remove nil
            (mapcar
              #'(lambda (x) ; not stored as available yet?
                  (if (null (gethash '(available x) curr-state-ht))
                      `(available ,x) nil)) objs)))
     (set facts-name (append new-facts (eval facts-name)))
     (store-facts new-facts curr-state-ht)
 )); end of declare-as-available


(defun find-a-block-to-place (goal-schema-inst curr-state-ht)
;````````````````````````````````````````````````````````````
; Find a pair 
;     ((var obj) list-of supporting-objects)
; where 'obj' is available in the current state and can be used to add 
; another piece, corresponding to variable 'var' in 'goal-schema-inst', 
; by placing it on the listed supporting objects. (There can be more than
; one support, e.g., a block partially supported by each of two others.) 
;
; The facts of the current situation are assumed to be recorded in 
; 'curr-state-ht', which also includes 'available' and 'in-use' properties 
; of objects that are maintained by the calling program when it positions 
; an available block on the partial goal structure, hence marking it as 
; being 'in-use' (no longer usable for the current construction attempt, 
; though it remains available for possible later attempts, after a failure).
;
; 'Goal-schema-inst' is a schema-based representation of the goal structure, 
; separate from the representation of the actual situation, as recorded in
; 'curr-state-ht'. We use variables for all objects to be instantiated in 
; the goal structure, allowing for nonfluent constraints that may include 
; identities to specific objects (e.g., the table). (Any names occurring 
; in such identities should occur in the current state.) 
;
; This is a key program for building structures "bottom-up", and some
; portion of 'goal-schema-inst' may already have bindings for some of
; the schema variables. Since bindings are added by the calling program
; only when an object is successfully added to the structure, the presence 
; of a binding indicates a properly placed block. Such properly placed
; blocks can only have other properly placed blocks on them -- there cannot
; be obstructing objects in the partially instantiated goal structure.
; (A separate program may be used by the calling program to identify
; partial (or even complete) structures that satisfy a goal schema for 
; which an instantiation is required. A large enough partial structure
; might be cleared off and then the present program might be used to find
; further placement actions.)

 (let (vv vvv vvvv var props objs descr obj bbb)
; 
;  1. Find an unbound goal-schema variable 'var' all of whose supports are bound
      (setq vv (unbound-vars-of goal-schema-inst))
      (if (null vv); schema instance is complete
          (return-from find-a-block-to-place T)); use T for "already complete" 
      (dolist (v vv)
          ; Find the supports of v required by the goal schema
          (setq vvvv (req-supports-of v goal-schema-inst))
          ; Note: these supports are generally obtained as variables,
          ;       except for supports permanently in-place (i.e., table)
          (setq vvv (set-difference vvvv vv :test #'equal))
          ; we've removed unbound vars; vvv could still contain constants;
          ; NB: We could get vvv = vvvv = nil, i.e., v needs no supports
          (when (= (length vvv) (length vvvv)) ; are all supports bound?
                (setq var v)
                (return nil)) ); exit dolist
      (if (null var); something's wrong -- no unbound var has its supports
                    ; (if any) in place.
          (return-from find-a-block-to-place '|***No vars with supports in place|))

;  2. Collect the (nonfluent) properties required for 'var', and add
;     the fluent properties of being currently available and not in-use:
      (setq props (req-properties var goal-schema-inst))
      (setq props (append `((available ,var) (not (in-use ,var))) props))

;  3. Find all available, not-yet-in-use objects that have the required 
;     properies (use a conjunctive lambda-abstract, coding lambda as ':l')
      (setq descr `(:l ,(list var) ,(cons 'and props)))
      (setq objs (find-all-instances descr curr-state-ht))
      (if (null objs); have we run out of suitable building blocks?
          (return-from find-a-block-to-place 
             (intern (format nil "***No suitable blocks available for ~a" var))))

;  4. Make a convenient choice from 'objs'; prefer clear blocks, & nearby ones;
      ; Assume *robot-posn* is the position (= (0 0)??) of the human director
      (setq obj (choose-easily-reached objs curr-state-ht))
      ; find the bindings of the support variables
      (setq bbb (bindings-of vvv goal-schema-inst))
      ; bbb will contain bindings for variables in vvv, while retaining in
      ;     place any constants (like 'table') in vvv;
      ; bbb can't contain a variable (because vvv can't contain an unbound var)
      (return-from find-a-block-to-place `((,var ,obj) ,bbb))
;     Note: The calling program will presumably bind 'var' to 'obj' iff
;           the block placement succeeds.
 )); end of find-a-block-to-place


;; BTW: At some point it may be possible to increase construction efficiency
;; by maintaining entity domains for the variables of a goal schema.

;; A program for finding a structure that instantiates a sufficient portion
;; of a goals schema that it makes sense to use it as a starting point for
;; a full instantiation; [but exploitation of existing partial structures is
;; a level of sophistication above what I would aim for initially, so I've
;; set aside implementation of various functions required for implementing
;; this high-level draft; instead, the initial goal is "building from
;; scratch"]:

(defun find-extant-partial-structures (goal-schema curr-state-ht)
;```````````````````````````````````````````````````````````````
; ** THIS PROGRAM IS NOT BEING USED -- IT'S A "PLACEBO" FOR POSSIBLE
;    FUTURE DEVELOPMENT.
;
; Find all structures in the current state that best "approximate" a lower
; portion of the desired goal-schema, returning these partial (or complete)
; instances, each accompanied by a list of obstructing blocks. (The calling 
; program will be reponsible for clearing these off.) So the result should
; be of form 
;   ((schema-inst1 obstr-list1) ... (schema-instk obstr-listk)).
;
; A "good enough" approximation is a set of n blocks that instantiates 
; a lower portion of the goal schema and has m < n obstructing blocks
; piled on top; i.e., time will be saved by clearing off the obstructing 
; blocks and building on top of the n good blocks. We want to find the
; partial instances with the largest n-m.
;
; We build instances much as we actually build structures, except that
; we just verify for each unbound variable whose supports are already
; bound whether a suitable object already sits on those supporting objects;
; if so, we bind the variable accordingly and continue; if not, then if
; we can't find another variable to bind, we find the set of obstructing
; objects, and if the partial structure is "good enough" and at least as 
; good as the best found so far, we add it to the output candidates,
; throwing out any worse ones among the current candidates.
;
; TBC -- THE CODE BELOW IS CURRENTLY MOSTLY A COPY OF 'find-a-block-to-place'
;       (SEE SOMEWHERE ABOVE), WITH ADDED STEPS. WE NEED TO ITERATE
;       THROUGH ALL AVAILABLE BLOCKS AS STARTING POINTS FOR IDENTIFYING A
;       PARTIAL INSTANCE. AS OF OCT 13/17, I THINK FINISHING THIS PROGRAM 
;       IS FEASIBLE, BUT NOT AS URGENT AS THE SUBROUTINES REQUIRED FOR 
;       'find-a-block-to-place'. SO I PUT IT ON "HOLD".

 (let (vv vvv vvvv var props objs descr obj )
; 
;  1. Find an unbound goal-schema variable 'var' all of whose supports are bound
      (setq vv (unbound-vars-of goal-schema-inst))
      (if (null vv); schema instance is complete
          (return-from find-a-block-to-place T)); use T for trivial completion 
      (dolist (v vv)
          (setq vvvv (req-supports-of v goal-schema-inst))
          (setq vvv (remove-if-not
                        #'(lambda (x) (bound-in x goal-schema-inst)) vvvv))
          ; NB: We could get vvv = vvvv = nil, i.e., v needs no supports
          (when (= (length vvv) (length vvvv)) ; are all supports bound?
                (setq var v)
                (return nil)) ); exit dolist
      (if (null var); something's wrong -- no unbound var has its supports
                    ; (if any) in place.
          (return-from find-a-block-to-place '|***No vars with supports in place|))

;  2. Collect the (nonfluent) properties required for 'var', and add
;     the fluent properties of being currently available and not in-use:
      (setq props (req-properties var goal-schema-inst))
      (setq props (append `((available ,var) (not (in-use ,var))) props))

;  3. Find all available, not-yet-in-use objects that have the required 
;     properies (use a conjunctive lambda-abstract, coding lambda as ':l')
      (setq descr `(:l ,var ,(cons 'and props)))
      (setq objs (find-all-instances descr curr-state-ht))
      (if (null objs); have we run out of suitable building blocks?
          (return-from find-a-block-to-place 
             (intern (format nil "***No suitable blocks available for ~a" var))))

;  4. Check if one of 'objs' is already on all of its supports, and if so,
;     just bind that object to 'var':
      (dolist (ob objs)
          (dolist (v vvv)
              (if (not (supports v ob curr-state-ht))
                  (return nil))); exit support-checks
          ; 'ob' is already on the required supporting objects
          (bind-var var ob goal-schema-inst)
          (return-from find-a-block-to-place `(already-on ,ob ,vvv)))

;  5. There is no object with the required properties already on supporting 
;     objects 'vvv'. Make a convenient choice from 'objs'; prefer an object 
;     that is already on *some* of the supports of 'var' (though it will 
;     have to be repositioned), and beyond that prefer clear blocks, & 
;     nearby ones;
      (setq obj (choose-easily-reached objs curr-state-ht))
      (return-from find-a-block-to-place `(puton-and-bind (,var ,obj) ,vvv))
;     Note: The calling program will presumably bind 'var' to 'obj' iff
;           the block placement succeeds.
 )); end of find-extant-partial-structures

;; Then formulate the full schema, with a repeat loop, for building a
;; desired structure, with the above as the main subplan, of course.

(defun unbound-vars-of (goal-schema-inst)
;````````````````````````````````````````
; Find the variables of the given goal schema instance that have no
; specified bindings. We assume that a schema instance has a :vars
; field consisting initially of all the schema variables, which become
; paired with names of objects when we decide on variable values. 
; (In LISSA we used destructive substitution, but this wouldn't serve 
; us here.) We could use a hash table, but for the moment schema instances 
; are not expected to be large enough to warrant this.
;
 (let (vars )
      (setq vars (retrieve-schema-entries :vars goal-schema-inst))
      ; Note: vars may lack a binding but nonfluent-conds can equate
      ;       a avariable to a constant, e.g., (= ?x B1)
      (remove-if #'listp vars); remove variables with bindings
 )); end of unbound-vars-of


(defun bindings-of (vars goal-schema-inst)
;`````````````````````````````````````````
; Find the bindings of the variables in vars in the goal-schema-inst, i.e.,
; the values they are paired with on the :vars list of goal-schema-inst.
; If vars constains constants, these are retained in the output as-is.
; If not all variables of vars are bound in goal-schema-inst, the list of
; bindings returned will contain the unbound variables (really an error
; condition)
;
 (let (vvv vv bbb)
      (setq vvv (retrieve-schema-entries :vars goal-schema-inst))
      (dolist (v vars); usually variables, but may contain constants
         (setq vv 
           (member-if #'(lambda (x) (and (listp x) (eq (car x) v))) vvv))
         ; if v is a constant or unbound variable, we use it (not its value)
         (if (null vv) (push v bbb)
             (push (second (car vv)) bbb))); a constant binding
      (reverse bbb); return bindings in the order corresponding to vars
 )); end of bindings-of


(defun bind-var-of (schema var+obj); May 16/20
;``````````````````````````````````
; On the variable list of the given schema, replace the given variable
; (the car of var+obj) with var+obj ; this is done destructively, and
; if another binding is already present, it is replaced.
;
 (let ((var (car var+obj)) vars var+obj1 rest)
      (setq vars (retrieve-schema-entries :vars schema))
      (if (member var vars); var still unbound?
          (prog2 (rplaca (member var (cdr (member :vars schema))) var+obj)
                 schema)
          (cond 
            ((setq var+obj1 ; var already bound? If so, replace binding
              (find-if #'(lambda (x) (and (listp x) (eq (car x) var))) vars))
             (rplaca (member var+obj1 (cdr (member :vars schema))) var+obj)
             schema)
            (t schema))); return schema with no change
 )); end of bind-var-of


;; I THOUGHT I'D NEED IT, BUT SO FAR I HAVEN'T
(defun search-to-next-keyword (obj lst); May 16/20
;`````````````````````````````````````
; Return the trailing portion of lst that starts with obj, provided that
; the search does not pass over any keyword other than :comment. However,
; an initial keyword in lst does not block the search. Strings in lst
; (which will generally be comments in the use of this function) will
; be bypassed unless obj is exactly that string.
;
 (let (rest)
      (cond ((atom lst) nil); unexpected 
            (t (setq rest
                 (member-if 
                   #'(lambda (x) 
                      (or (equal x obj) 
                        (and (keywordp x) (not (eq x :comment))))) (cdr lst)))
                 (if (keywordp (car rest)) nil rest)))
 )); end of search-to-next-keyword
                                                  

(defun retrieve-schema-entries (key schema); tested
;``````````````````````````````````````````
; Return the list of entries that fall under the given (colon-prefixed)
; 'key' in the given 'schema' (a schema or schema-instance). 'Key' should
; not be :comment, and both occurrences of :comment and top-level list
; elements that are strings (and thus comments) are skipped.
;
 (let (entries-etc entries)
      (when (eq key :comment)
            (format t "~%*** :comment is not allowed as a retrieval key ~
                          for schema entries")
            (return-from retrieve-schema-entries nil))
      ; Find the starting point for the entries after the given key:
      (setq entries-etc 
          (cdr (member key schema)))
      (dolist (e entries-etc)
          (if (or (and (keywordp e) (not (eq e :comment)))
                  (member e '(:end end end.))); **maybe allow :end only??
              (return nil)); exit loop at 1st non-comment keyword or end
          ; Collect non-comment entries. Assume that top-level strings 
          ; are comments, even if :comment keywords are missing:
          (if (not (or (eq e :comment) (stringp e)))
              (push e entries)))
      (reverse entries); note: this includes condition names, like ?a.
 )); end of retrieve-schema-entries


(defun req-supports-of (var goal-schema-inst); tested
;````````````````````````````````````````
; Find the variables vv of goal-schema-inst s.t. for each v in vv, the
; goal schema specifies (on var v); (actually, var, v can be any terms)
;
; **At some point we may want to use other relations here, such as 'on.p',
;   'partially-on', 'centered-on', 'mounted-on', or the like.
;
 (let (conds supports)
      ; Retrieve static conds from 'goal-schema-inst':
      (setq conds 
          (retrieve-schema-entries :static-conds goal-schema-inst))
      ; select the conds matching (on var ...)
      (dolist (c conds)
          (if (and (listp c) (equal (ldiff c (cddr c)) `(on ,var)))
              (push (third c) supports)))
      (reverse supports)
 )); end of req-supports-of


(defun req-properties (var goal-schema-inst);
;````````````````````````````````````````````
; Select the type and nonfluent properties of variable 'var' in the
; given 'goal-schema-inst'; VAR IS ASSUMED TO BE AN UNBOUND VARIABLE;
; The properties could be relational (e.g., size-relations), and the
; the relata may be variables, so we have to apply bindings and
; equalities for variables other that 'var' before we know what
; constant entities 'var' may be related to.
; ** I EXPECT TO CHANGE CONDS TO INFIX FORM EVENTUALLY!
;    CURRENTLY ONLY TYPE INFO IS ASSUMED TO BE IN PREFIX FORM
;
 (let (conds var=const bindings eqs non-eqs)
      (setq conds 
          (append
            (mapcar #'reverse ; types are written as e.g., (?x block)
              (retrieve-schema-entries :types goal-schema-inst))
                              ; WHEREAS CONDS ARE CURRENTLY PREFIX-FORM!
            (retrieve-schema-entries :nonfluent-conds goal-schema-inst)))
            ; Note: nonfluent-conds may include constraints equating a
            ;       variable to a constant (but no var-to-var equalities)
      ; See if these conditions equate var to a constant:
      (setq var=const 
          (find-if #'(lambda (x) 
                       (and (listp x) (member (car x) '(= eq equal))
                                      (eq (second x) var))) conds))
                                      
                               
      (if var=const (return-from req-properties (list var=const)))

      ; Apply bindings and other equalities. (An equality (= v <const>)
      ; without a corresponding binding (v <const>) indicates that the
      ; object corresponding to v (named by <const>) has not been put
      ; in the desired place yet.)
      (setq bindings (retrieve-schema-entries :vars goal-schema-inst))
      (setq bindings (remove-if #'atom bindings)); drop unbound variables;
      ; Create & add bindings corresponding to equalities (= v <const>):
      (setq eqs (remove-if-not
                 #'(lambda (x) (and (listp x) (member (car x) '(= eq equal))))
                  conds)); ** should we allow only '=', or only 'eq'?
      (setq conds (reverse ; set-difference generally reverses the 1st list
                    (set-difference conds eqs :test #'equal)))
      (setq bindings (append (mapcar #'cdr eqs) bindings))
      (if bindings
          (dolist (binding bindings)
              (setq conds (subst (second binding) (first binding) conds))))
      ; Now remove conds not involving var:
      (remove-if-not #'(lambda (x) (and (listp x) (occurs-within var x))) conds)
 )); end of req-properties

(defun occurs-within (atm expr); also defined in ./unification*lisp
;`````````````````````````````
; If the given atom occurs properly within the body of expr,
; return t, otherwise nil; we also allow a non-list expr like 
; (occurs-within 'x '(a . x)), which yields t.
;
  (if (atom expr) nil
      (if (and (listp expr)
               (or (eq atm (car expr)) (eq atm (cdr expr)))) t
          (or (occurs-within atm (car expr))
              (occurs-within atm (cdr expr))))
 )); end of occurs-within


(defun var? (x) ; is x a variable, i.e., an atom with first character"?"
;~~~~~~~~~~~~~  ; Elsewhere (./unification*) the same def'n is used for 'var'
 (if (and x (symbolp x))
     (char= (nth 0 (coerce (string x) 'list)) #\?)
     nil ))
 

(defun find-all-instances (descr curr-state-ht);
;``````````````````````````````````````````````
; 'descr' is a lambda abstract, using ':l' for lambda.
;    The body of 'descr' consists of conjoined +ve or -ve literals, one
;    of which must be +ve & contain all variables, including the lambda 
;    variable(s).
;    e.g., (:l (?x ?y) (and (between ?x ?z ?y) (red ?x) (blue ?y)));
;    e.g., (:l (?x) (on ?x ?y)); everything that's on something
;    NB: A non-equality constraint like "?y is not the table" would
;        have to be expressed as something like (not (is-table ?y)),
;        where we've added (is-table table) to the set of facts.
; 'curr-state-ht' is a hash table for positive facts comprising the 
;    current state, indexed under the fact as a whole, the predicate
;    alone, and the predicate in combination with one argument (for
;    each argument, if there's more than one).
; We find all instances of the :l-variable(s)? as follows: 
; (simplest possible method) Start with all the instances of the
; positive predication (in the lambda-description) that contains 
; all the variables, and successively restrict the tuples with the
; remaining conjuncts. Use predication templates, in addition to the
; current set of instances, as arguments in 'constrain-relation', 
; so that argument correspondences will be clear, w/o any indexing.
; In the end, project the resulting list of predicate instances onto
; the :l-variable "dimensions"
;
 (let (body lambda-vars vars main-conjunct facts neglist poslist ind inds)
      (setq lambda-vars (second descr))
      (setq body 
            (if (not (eq (car (third descr)) 'and)); just one predication?
                (list (third descr)); list it, for uniformity
                (cdr (third descr)))); drop the "and"
      (setq vars (remove-duplicates ; look for variables in the "flattened",
                   (remove nil      ; unnegated predications, appended together
                     (mapcar #'(lambda (x) (if (var? x) x nil)) 
                       (apply #'append 
                         (mapcar 
                           #'(lambda (x) (if (eq (car x) 'not) (second x) x))
                            body))))))
      ; find positive conjunct containing all variables
      (dolist (conjunct body)
            (when (subsetp vars ; is the set of all vars a subset of (& thus
                    (remove nil ; equal to) the variables of conjunct?
                      (mapcar #'(lambda (x) (if (var? x) x nil))
                                (cdr conjunct))))
                  (setq main-conjunct conjunct)
                  (return nil))) ; exit loop
      (when (null main-conjunct)
            (format t "~%*** Description ~s ~
                      ~%      given to 'find-all-instances' didn't contain ~
                      ~%      a predication that covers all variables" descr)
            (return-from find-all-instances nil))

      ; Retrieve the facts in curr-state-ht matching main-conjunct
      (setq facts (get-matching-facts main-conjunct curr-state-ht))
      ; Now apply the remaining conjuncts as constraints on facts;
      ; first place negative facts after positive ones (also omitting 
      ; main-conjunct):
      (dolist (conjunct body)
          (if (eq (car conjunct) 'not) 
              (push conjunct neglist)
              (if (not (equal conjunct main-conjunct))
                  (push conjunct poslist))))
      (setq body (append (reverse poslist) (reverse neglist)))
      (dolist (conjunct body)
          (setq facts 
            (constrain-relation main-conjunct conjunct facts curr-state-ht)))

      ; Now project 'facts" onto the dimensions picked out by the :l-variables
      ; 'Project-relation' uses indices for argument positions to be picked
      ; out by the projection, so we compute these first by assigning
      ; index properties to the variables (the properties will be global, 
      ; but harmless):
      (setq ind 1)
      (dolist (arg (cdr main-conjunct))
          (incf ind) (if (var? arg) (setf (get arg 'index) ind)))
      (setq inds (mapcar #'(lambda (x) (get x 'index)) lambda-vars))
      ; Return the projecton of facts onto the positions corr. to the
      ; Lambda variables:
      (project-relation facts inds)
 )); end of find-all-instances


(defun storage-keys (fact); tested
;`````````````````````````
; Find the list of keys for hash-table storage of 'fact': the fact as a whole, 
; the predicate, and the predicate with one argument at a time, if there
; is more than one (while other arguments are set to nil).
; 
; We also allow facts that are atoms or of form (<atom>); in the 1st case
; the atom is the only key on the output list, and in the second both 
; <atom> and (<atom>) are on the output list; (we always want to be able
; to retrieve via the fact as a whole, and via the predicate)..
;
 (let (keys key n)
      (cond ((atom fact) (list fact))
            ((null (cdr fact)) (list (car fact) fact)); e.g., (pred (pred))
            (t (setq keys (list fact (car fact))); entire fact, & pred
               (when (cddr fact); more than one argument?
                     (setq n (length (cdr fact))); no. of args
                     (dotimes (i n)
                         (setq key (list (car fact)))
                         (dotimes (j n)
                             (if (= i j) (push (nth i (cdr fact)) key)
                                         (push nil key)))
                         (setq key (reverse key))
                         (push key keys)))
               keys))
 )); end of storage-keys
       

(defun store-fact (fact ht); tested
;``````````````````````````
; Store the 'fact' in hash table 'ht' (that uses 'equal' as test) using as keys: 
; - the entire fact, 
; - the predicate (unless the fact is atomic or a 1-element list 
;   (i.e., pred with 0 arguments)
; - if the pred has >= 2 arguments, patterns of form  (pred arg1 nil nil ...), 
;   (pred nil arg2 nil nil ...), (pred nil nil arg3 nil nil ...), etc.
; Return T if the fact was new, and NIL otherwise.
;
 (let ()
      (if (gethash fact ht) (return-from store-fact nil))
      (dolist (key (storage-keys fact)) (push fact (gethash key ht)))
      T
 )); end of store-fact


(defun store-facts (facts ht)
;````````````````````````````
 (dolist (fact facts) (store-fact fact ht))
 ); end of store-facts


(defun remove-fact (fact ht); tested
;```````````````````````````
; Delete 'fact' from hash table 'ht', under all its keys
;
 (if (gethash fact ht); is 'fact' actually in ht? 
     (prog2 (dolist (key (storage-keys fact))
               (setf (gethash key ht) 
                     (remove fact (gethash key ht) :test #'equal)))
            t) ; signal that the fact was removed
     nil; fact wasn't present in ht
 )); end of remove-fact


(defun get-matching-facts (pred-patt ht); fairly well tested
;``````````````````````````````````````
; pred-patt: e.g., (between B ?x C); any vars in pred-patt are
;    assumed to be distinct;
; Note that we need to use retrieval key (between B nil nil) or
;    (between nil nil C), and filter the results, because ground
;    wffs are stored under the entire wff as key, under the pred,
;    and under the pred in combination with one argument at a time.
;
; Retrieve the list of facts matching pred-patt from hash table ht.
; This is dependent on the restricted set of keys that are used in 
; storing facts: If there is a variable in pred-patt (as in the
; example), we need to make the variable and all but one non-var
; argument "don't-cares" (nil) in the retrieval. The we filter out
; instances that don't have the required non-variable arguments.
;
 (if (atom pred-patt) ; special (unexpected) cases of arg-less preds
     (if (symbolp pred-patt) ; facts are stored as list elements
         (return-from get-matching-facts (gethash pred-patt ht))
         ; not a symbol
         (return-from get-matching-facts nil)))
 (if (null (cdr pred-patt)); of form (p)
     (if (symbolp (car pred-patt)) ; use p as key
         (return-from get-matching-facts (gethash (car pred-patt) ht))
         ; of form (p) where p is not a symbol
         (return-from get-matching-facts nil)))
    
 ; at this point we have something of form (p x1 ... xk) where k >= 1
 ; and some of the xi may be variables (the expected case).
 (let ((nvars 0) nconst key const facts select-facts)
      (dolist (arg (cdr pred-patt))
         (if (var? arg) (incf nvars)))
      (setq nconst (- (length (cdr pred-patt)) nvars))
      ; 4 cases: no vars; no consts; vars & 1 const; vars & > 1 consts
      (cond ((zerop nvars) (gethash pred-patt ht)); singleton list of facts
            ((zerop nconst) (reverse (gethash (car pred-patt) ht)))
            ((= nconst 1) 
             (setq key (mapcar #'(lambda (x) (if (var? x) nil x)) 
                               pred-patt))
             (reverse (gethash key ht)))
            (t; >= 1 variable and > 1 constant
              ; for the key, set all var's and all but one const. to nil
              ; Pick a constant to retain in the key:
              (setq const (find-if #'(lambda (x) (not (var? x))) 
                                    (cdr pred-patt)))
              (setq key 
                (cons (car pred-patt)
                  (mapcar #'(lambda (x) (if (not (eq x const)) nil x))
                          (cdr pred-patt))))
              (setq facts (gethash key ht)); (in reverse store-order)
              ; filter out facts whose constant args don't match those
              ; of pred-patt:
              (dolist (fact facts)
                 (if (not (member nil
                            (mapcar #'(lambda (x y) (or (var? x) (equal x y)))
                                     (cdr pred-patt) (cdr fact))))
                     (push fact select-facts))); this causes correct order
              select-facts))
 )); end of get-matching-facts
                        

(defun get-facts (key ht); tested
;```````````````````````
; This is basically just hash table retrieval via 'key', but with safeguards
; if improper keys are used: We don't allow an entirely nil ("don't care") 
; argument list (because then just the predicate name should have been used), 
; and we don't allow more than one non-nil argument if there is a nil argument.
; (The constraints are intended to avoid unreasonable (> O(n)) storage
; requirements.)
;
 (let (tail)
   (cond ((listp key)
          (when (null (member-if #'(lambda (x) x) (cdr key)))
                (format t "~%### WARNING: hash table access tried with ~
                  improper key ~s; ~%      ~s was used instead" key (car key))
                (return-from get-facts (gethash (car key) ht)))
          (when (member nil key)
                (setq tail (member-if #'(lambda (x) x) (cdr key)))
                (when (member-if #'(lambda (x) x) (cdr tail))
                      (format t "~%### WARNING: hash table access with improper ~
                       key ~%      ~s aborted; can use 1 non-nil arg with 1 nil" key)
                      (return-from get-facts nil)))
          (gethash key ht))
         (t (gethash key ht))) ; key is an atom
 )); end of get-facts


(defun constrain-relation (pred1-patt {~}pred2-patt rel1 kb-ht); lightly tested
;````````````````````````````````````````````````````````````
; pred1-patt: of form (pred1 arg1 ... argn); in general, argi may
;     be a variable.
; {~}pred2-patt: of form (pred2 arg'1 ... arg'm) or 
;     (not (pred2 arg'1 ... arg'm)), where m =< n and all variables 
;     of pred2-patt occur in pred1-patt; {~}pred2-patt also allows
;     for equalities or negated equalities, using one of {=,eq,equal};
; rel1: a list of relation instances, a subset of pred1-patt instances;
;
; If {-}pred2-patt is unnegated, the result is a restricted subset
; of the rel1-instances, where only those are retained and returned 
; whose arguments, when used to instantiate {~}pred2-patt, lead to 
; existing hash-table entries.
;
; If {-}pred2-patt is negated, the procedure is similar, except that
; only rel1-instances are retained where the pred2 hash-table lookup
; does *not* lead to existing entries.
;
 (let (positive key rel2)
      ; deal with equality constraints first
      (if (and (listp {~}pred2-patt) (member (car {~}pred2-patt) '(= eq equal)))
          (return-from constrain-relation
             (constrain-relation-by-equality pred1-patt {~}pred2-patt rel1)))
      ; now inequality constraints
      (if (and (listp {~}pred2-patt) (eq (car {~}pred2-patt) 'not)
               (listp (second {~}pred2-patt)) 
               (member (car (second {~}pred2-patt)) '(= eq equal)))
          (return-from constrain-relation 
             (constrain-relation-by-inequality pred1-patt {~}pred2-patt rel1)))
      ; other constraints (=> require consulting '{~}pred2-patt'-facts in kb-ht)
      (setq positive (not (and (listp {~}pred2-patt) 
                               (eq (car {~}pred2-patt) 'not))))
      (dolist (item rel1)
          ; set variables occurring in pred1-patt to corresponding
          ; elements of the item predication:
          (mapcar #'(lambda (x y) (if (var? x) (set x y)))
                  (cdr pred1-patt) (cdr item))
          ; construct a retrieval key for pred2 accordingly:
          (setq key (mapcar #'(lambda (x) (if (var? x) (eval x) x))
                            (if positive {~}pred2-patt (second {~}pred2-patt))))
          (if (gethash key kb-ht)
              (if positive (push item rel2))
              (if (not positive) (push item rel2))))
      (reverse rel2)
      ; It will come out preserving the ordering in rel1.
 )); end of constrain-relation


(defun constrain-relation-by-equality (pred-patt equality rel); tested
;`````````````````````````````````````````````````````````````
; pred-patt: provides variable argument positions for the predications in rel;
; equality: e.g., (= ?y B3), i.e., we fix the value of a variable in pred-patt
;           and hence of corresponding arguments in the rel predications;
;           the reverse form, e.g., (= B3 ?y), is also handled;
; rel: a set of ground predications (of form  pred-patt), to be filtered with
;           the equality; NB: no error check is done to ensure that pred-patt
;           and rel use the same predicate. Also no check is made on the form
;           of equality. The calling program might use 'eq' or 'equal', instead
;           of '=', but the initial symbol in the given equality is ignored.
;    
 (let (var val (i -1) result)
      ; find the position of the variable in the equality in pred-patt, and
      ; then go through the elements of rel, retaining for output just those
      ; that have the constant specified by the equality at the position
      ; determined from the pred-patt:
      (setq var (second equality); e.g., (= ?y B3) ... the expected form
            val (third equality))
      (if (not (var? var))
          (setq var (third equality); e.g., (= B3 ?y)
                val (second equality)))
      (cond ((atom pred-patt) rel); unexpected: pred-patt should be (pred ...)
            (t (dolist (x pred-patt)
               (incf i) (if (equal x var) (return nil))); exit loop (i is set)
               (dolist (r rel)
                  (if (equal (nth i r) val) (push r result)))
               result))
 )); end of constrain-relation-by-equality


(defun constrain-relation-by-inequality (pred-patt inequality rel);
;``````````````````````````````````````````````````````````````````
; pred-patt: provides variable argument positions for the predications in rel;
; inequality: e.g., (not (= ?y B3)), i.e., we preclude the value of a variable 
;           in pred-patt and hence of corresponding arguments in the rel pred-
;           ications; the reverse form, e.g., (not (= B3 ?y)), is also handled;
; rel: a set of ground predications (of form  pred-patt), to be filtered with
;           the inequality; NB: no error check is done to ensure that pred-patt
;           and rel use the same predicate. Also no check is made on the form
;           of inequality.
;    
 (let (var val (i -1) result)
      ; find the position of the variable in the equality in pred-patt, and
      ; then go through the elements of rel, retaining for output just those
      ; that have the constant specified by the equality at the position
      ; determined from the pred-patt:
      (setq var (second (second inequality)); e.g., (not (= ?y B3)) ... expected 
            val (third (second inequality)))
      (if (not (var? var))
          (setq var (third (second inequality)); e.g., (not (= B3 ?y))
                val (second (second inequality))))
      (cond ((atom pred-patt) rel); unexpected: pred-patt should be (pred ...)
            (t (dolist (x pred-patt)
               (incf i) (if (equal x var) (return nil))); exit loop (i is set)
               (dolist (r rel)
                  (if (not (equal (nth i r) val)) (push r result)))
               result))
 )); end of constrain-relation-by-inequality



(defun project-relation (rel indices); tested
;```````````````````````````````````` 
; 'rel' is assumed to be a set of k-tuples, with k  >= 1. (If it is 1,
; the list 'rel' is returned unchanged.) 'Indices' is the list of argument
; indices indicating what "dimensions" of rel should be retained (projecting
; other dimensions onto them), and in what order they should be returned.
; The result is a set k-tuples, where k =|indices|, except that if k=1,
; the 1-tuples are reduced to individual atoms, not singleton lists.
;
; Method: Run through the 'rel' tuples, and for each tuple, pull out the
; elements corresponding to the 'indices', and if this "subtuple" has not yet
; been encountered before (as registered in an ad-hoc hash table), push it
; onto the result list and register it in the ad-hoc hash table.
; Return the reverse of the final result list.
; 
 (let (ht key result)
      ; Is 'rel' unary, i.e., a list of atoms or single-element lists?
      (if (or (null rel) (atom (car rel)) (null (cdar rel)))
          (return-from project-relation rel))
      (setq ht (make-hash-table :test #'equal))
      (dolist (tuple rel)
          (setq key nil)
          (dolist (i indices)
              (push (nth (- i 1) tuple) key))
          (setq key (reverse key))
          (when (null (gethash key ht))
              (setf (gethash key ht) T) 
              (push key result)))
      (if (null (cdr indices)); if just one index, "flatten" the result
          (setq result (apply #'append result)))
      (reverse result)
 )); end of project-relation 


(defun choose-easily-reached (objs curr-state-ht)
;````````````````````````````````````````````````
; Make a convenient choice from 'objs'; prefer less-obstructed blocks,
; & perhaps ones near to 'posn' (if we're using metric reinformation);
; Assume that objects have been "load-marked" in terms of how many  
; objects would have to be moved to dig them up; these markings must
; be updated each time an object is moved.
;
 (let (ranked-objs)
      (setq ranked-objs 
        (sort objs #'< :key #'(lambda (x) (get x 'load))))
      ; if we use *robot-posn*, we should do proximity-rank as well
      ; and perhaps trade off proximity to robot & low load. For now
      ; we ignore the robot position. Another variant would be to find
      ; the lowest load-value among 'objs', and then select the objs that
      ; have the lowest value. This would avoid the O(n log n) sorting.
      (car ranked-objs)
 )); end of choose-easily-reached


(defun directly-on (x kb-ht); tested
;```````````````````````````
; Retrieve the objects directly on object x
; 
 (if (symbolp x) 
     (project-relation (gethash `(on nil ,x) kb-ht) '(2)) nil)
 ); end of directly-on 


(defun transitively-on (x kb-ht); tested
;`````````````````````````````
; Find the set of objects that x directly or indirectly supports;
; Note that since 2 blocks, say, halfway up a stack can support the
; same objects further up, we need to guard against duplication of
; objects.
;
 (if (not (symbolp x)) (return-from transitively-on nil))
 (let ((riders (directly-on x kb-ht)))
      (remove-duplicates
         (append riders 
            (apply #'append 
               (mapcar #'(lambda (y) (transitively-on y kb-ht)) riders))))
 )); end-of transitively-on
     

(defun mark-objects-with-load (kb-ht); tested
;````````````````````````````````````
; Mark blocks & other objects that participate in on-relations with the 
; number of objects they are supporting directly or indirectly. In general
; the blocks and on-participants will be almost the same, except that the
; latter will include the table. But retrieving both sets ensures that
; won't miss any blocks that, for example, are *in* a container rather
; than *on* something, and that movable objects will be included even
; if some blocks were inadvertently not predicated as such. We could
; refine this easily to allow for sizes or weights of "riders".
;
 (let ((objs (gethash 'block kb-ht)))
      ; At this point 'objs' is of form ((block <arg1>) (block <arg2>) ...)
      (setq objs 
        (union (mapcar #'second objs)
          (remove-duplicates
            (apply #'append
              (mapcar #'cdr (gethash 'on kb-ht))))))
      (dolist (obj objs)
         (setf (get obj 'load)
               (length (transitively-on obj kb-ht))))
 )); end of mark-objects-with-load kb-ht)
       


