(ns clojure-noob.core
  (:gen-class))

;------------------
;how to wrap a word in parens:
;cseb
(wat)

;slurp: move closing paren to left to include next expression
;starting with this, put cursor in the inner form and do > with left paren
(map inc (1 2 3) 4)
;then you get
(map inc (1 2 3 4))

;barf, the converse:
(map inc (1 2 3 4))
;use  < (
(map inc (4 1 2 3 4))
;think of: from inside a form, point at an element or expression, then type the paren you want it to get in or out of

; move to the opening or closing paren with typing paren inside a form while in normal mode 
(+ 4 2 6 123 )

;; move things around with <f and >f (for forms), and <e and >e (for elements)

; just in case I wasn't using 'form' correctly earlier (maybe i meant s-expression? the stuff inside parens) 
; a form is 'structurally valid code' such as
1
"a string"
["a", "vector", "of" , "string"]


;;--------------------------
;;1 Syntax
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, My Name Is Inigo Montoya"))

;all operations are: opening paren, operator, operands, closing paren
(+ 1 2 6)

;space is used to separate operands
(str "a panda " "eats" " shoots and leaves")


; if boolean-form
(if (> 7 8)
  ;then form
  (do (println "success")
      "fresh")
  ;optional else form
  (do (println "fail")
      "garbage "))
;note that the do operator lets us enclose multiple forms into a single form

;when is like a combo of if and do with no else
(when true 
  (println "it was")
  "great")

;def binds names to values
(def heroes 
  ["trashmaster"
   "judgmentalist"
   "captain panic"])

(print heroes)

;;2. Data Structures
; nil indicates no value
; nil? is the function that checks for nilness
(nil? 1)
; nil can represent logical falseness, but are not equivalent
(= nil false)

(if nil
  "logically true form" 
  "logically false form")
; all other values are logically truthy
(if 1
  "logically true form" 
  "logically false form")

;built ins can be checked with = operator
(= "p" "p")
(= 'prr' 'prr')
(= ["ate"] ["ate"])

;todo for numbers, see also 'coercion' and 'contagion'
;note that clojure can represent ratios directly:
(print 1/5)
(print (/ 1 5))

(print (/ 1.0 5.0))

(def chewie "Chewbacca")
; concatenation via str function.
; do quotation marks in the usual way with escape sequence started backslash
(print (str chewie " \"what a wookie\" "))

;maps.  similar to dictionaries or hashes.
{:a 1
 :b "boring example"
 :c {}}

;get returns nil if it doesn't find the key
(get {:a 1 :b 44} :b)

;but can take a default
(get {:karma "beeb" :dogma "boop" } "hi" "QUITE")

(def foomap {
             :a 1 
             :b 44 
             :c 
             {
              :ar [1,3] 
              :br "gorp"}})

;;ways to do the same thing
(get (get foomap :c) :ar))
; looks up nested values
(get-in  foomap [:c :ar]))
;; use the map as a function whose operand is the element you want to look up
((foomap :c) :ar))
;; keywords (used earlier for map keys) can be used as functions which look up corresponding value in a data structure
(:c foomap)
;; as an alternative to map literals, create a map with hash-map function
(hash-map :first "bruce" :second "steve")
;; todo see how clojure lets you create sorted maps

;;vectors.
;; get an element from this vector literal:
(get [1 4 56] 1)
;; create a vector with vector function
(vector 234 "shinty-six" 23 44.44)
;; append to a vector
(conj (vector 234 "shinty-six" 23 44.44) 44.44 999)

;;lists
;; similar to vectors.  but get function won't work.  instead use
(nth '(3 44.44 "shinty-six") 2)
;;also create lists with list function
(list "b" "a" "c")
(str (list "b" "a" "c"))
;; elements get prepended to a list:
(conj(list "b" "a" "c") 8)

;;sets. collections of unique values, like in real math.
;; exception trying to create invalid set
#{"a" "b" "a"}
;; nothing trying to make a set invalid
(conj #{"a" "b"} "a")
;;check for set membership:
(:a #{:a :b})
(:c #{:a :b})
(get #{:a :b} :c)

;;create sets from vectors and lists with set function
(set '(1 4 4 4 9))
(set [1 4 4 4 9])
;; use this to eg check list or vector membership
(get (set '(1 4 4 4 9)) 5)
(get (set [1 4 4 4 9]) 4)
;;use hash-set or sorted-set to create those two things respectively.
;;todo see sorted-set-by to define how a set is sorted

;;2.9 symbols and naming
;; associate a name to something.  will become important when discussing macros and 
;; 'programs as data'
(def babadook ["monster", "mash"])
(identity 'test)

;;quoting.
;; giving the clojure (runtime? repl? interpreter?) a symbol returns the "object" referred to by the symbol.
(def babadook ["monster", "mash"])
babadook
;; quoting a symbol tells clojure to use the symbol as a data structure.
(def babadook ["monster", "mash"])
'babadook
;; use eval function to evaluate a symbol.
(def babadook ["monster", "mash"])
(eval babadook)
;; quote a collection to leave all symbols in it unevaluated
(def babadook ["monster", "mash"])
(second '(babadook 2 33))

;;3.  Functions
;; a 'function call' is a term for an expression where the operator is a function expression.
;; a function expression is just an expression which returns a function.
(or + -)
;; stuff that returns 6
((or + -) 6)
((and (= 1 1) +) 1 2 3)
((first [+ 0]) 1 2 3)
;;numbers and strings are not functions
(1)
;;functions take any expressions (including other function expressions) as arguments
;; inc function increments by 1
(inc 1.1)
;; map function (not the map data structure) applies its first argument (a function expression) to each member of a collection
(map inc '(3.3 33 9 12))
(map inc [3.3 33 9 12])
;; clojure evaluates all function arguments recursively before passing them to a function
(+ (inc 199) (/ 100 (- 7 2)))

;;3.2 function calls, macros, special forms.
;; a special form is a form that doesn't always evaluate all of its operands.  e.g. the if form.
(if - 
  "neverbummer"
  "bummer")

(map str '("a" "b"))

;;3.3 defining functions
(defn growlAt 
  "growl at the name given"
  [growlee]
  (str "grrrrrr " growlee "!"))
(growlAt "c-3p0")

(defn esteban!
  "Return 'Esteban!' the number of times input"
  [numtimes]
  ((defn estebanrec
     "internal recursive helper to replicate and str esteban a bunch of times"
     [numt counter currentString]
     (if (= counter numt)
       currentString
       (estebanrec numt (inc counter) (str currentString "Esteban! "))
       )
     ) numtimes 0 "")
  )
(esteban! 18)

((defn add [x y] (+ x y)) 3 4)
;;use the doc function to view a docstring in the repl
(doc map)
;;params: 0 or more.  for more see the recursive function in esteban!
((defn noparam [] (print "no spriiiinggs! hehehe!")))
;;multi-arity functions: each arity definition enclosed in parens and has its own arg list
(defn afewways
  ([place insultingname whenn]
   (str "get to the " place " you " insultingname "! " whenn))
  ([place insultingname]
   (str "get to the " place " you " insultingname "! Now!!"))
  ([place]
   (str "get to the " place "! Now!!"))
  )
(afewways "chopper" "fool" "In 15 minutes!")
(afewways "ranch" "buckaroo" )
(afewways "pizza hut" )
;;overload by arity
(defn afewways2
  ([place insultingname whenn]
   (str "get to the " place " you " insultingname "! " whenn))
  ([place insultingname]
   (afewways2 place insultingname "now!!") )
  ([place]
   (afewways2 place "fool" "now!!"))
  )
(afewways2 "chopper")
;;variable arity. put the rest of the args in a list with the name given.
;;the "rest param" (ie the list with the ampersand in front of it) must always come last
( (defn varar
  [who [& epithets]]
  (map (defn yell [epithet] (str who ", you " epithet)) epithets)) "Carl" ["simpleton", "garbagemuncher"])
( (defn smells 
    [place & scents]
    (str "At " place " it smells like " (clojure.string/join " and " scents) ".")) "the beach" "apples" "ladybugs" "old shoes")

;;Destructuring. Bind symbols to values within a collection
;;This associates the symbol secondItem to the second element of the vector that was passed in
((defn secondThing
  [[firstItem secondItem]]
  secondItem) ["wat" "who"])
;; same thing, without destructuring:
((defn secondr
   [items]
   (first (next items))) ["wat" "who"])
;;more tricky destructuring, with rest args:
((defn picker
  [[choice1 choice2 & otherChoices]] 
  (print (str "First choice is " choice1))
  (print (str " second choice is " choice2))
  (print (str " and the rest: " (clojure.string/join ", " otherChoices)))
  ) ["tennant" "eccleston" "davison" "mccoy" "smith" "capaldi"])
;;remember that when destructuring you need to use double braces, otherwise the names are bound as required argument names, 
;;rather than mapped to elements of the single vector argument passed in.
;;suppose we wanted to do destructuring in multiple vector arguments:
((defn docAndCompanion
   [[choice1 choice2 & otherDocs] [pick1 pick2 & otherCompanions]]
   (print (str "first: " choice1 " and " pick1))
   (print (str " second " choice2 " and " pick2))
   (print (str " and the others " (str 
                                    (clojure.string/join ", " otherDocs) 
                                    " "
                                    (clojure.string/join ", " otherCompanions))
               )
          )
   )
 ["tennant" "eccleston" "davison" "mccoy" "smith" "capaldi"] ["piper" "agyeman" "coleman" "gillan" "tate" "capaldi"])
;; todo: label only the last arg ?
;; todo: how to join 2 sequences?
;; destructuring maps:
((defn coords
 [{ex :x why :y}]
 (print (str "x is " ex " and " " y is " why))) {:x 34.33 :y 22.2})
;; ie: associate the symbol ex with the value for the key x in the map passed in.
;; sugar for just associating a symbol having the same name as a key in the map.  or "breaking out keywords":
((defn coords
 [{:keys [x y]}]
 (print (str "x is " x " and " " y is " y))) {:x 34.33 :y 22.2})

;; 3.3.4 Function Body.
;;Function body can contain any forms.  The value of the last form evaluated is returned.
((defn doin'
  []
  (- 333 1)
  (print "whaha")
  "egreyegra"))
;; 3.3.5 Operations like - and + are also just functions.

;; 3.4 Anonymous functions.
((fn [mitchell webb]
  (str mitchell " and the " webb)) "angel summoner" "bmx bandit")

(map (fn [doThing]
       (str "one can " doThing )) ["summon angels" "ride a bmx"])
;; can associate an anonymous function with a name thusly (notice we use 'def', not 'defn'. thus it seems defn is a sugar for def + fn.):
(def welcome (fn [tothe] (str "welcome to the " tothe)))
(welcome "human race")

;;more sugar for anon functions. The % indicates first arg passed to the function.  
;;The # is like wrapping the sexp following it in a (fn [%] ....)
(#(* % %) 8)
((fn [%] (* % %)) 8)
;;to use this sugar with more than one arg, do %1 %2 ...
(#(* %1 %2) 3 4 )
;;also: rest param:
(#(clojure.string/join %&) "beep " "boop " "baap ")
;;3.5 returning functions.  returned functions are closures.
(defn incrementer
  [x]
  (fn 
    [incrementby]
    (+ x  incrementby)))
((incrementer 3) 2)

;;4.  All together.
( def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])
;;we want to make a 'right-' prefixed version of every
;;key value pair that has a 'left-' prefix.
(defn needs-matching-part?
  [part]
  (re-find #"^left-" (:name part)))
;;(needs-matching-part? {:name "left-leg" :size 2})

(defn make-matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-" )
   :size (:size part) })
;;(make-matching-part {:name "left-arm" :size 3})

(defn symmetrize-body-parts
  "Expects sequence of maps, each of which have one :name and one :size"
  [asym-body-parts]
  (loop [remaining-asym-body-parts asym-body-parts final-body-parts []]
  (if (empty? remaining-asym-body-parts)
    final-body-parts
    (let [[part & remaining] remaining-asym-body-parts 
         final-body-parts (conj final-body-parts part)] 
      (if (needs-matching-part? part) 
        (recur remaining (conj final-body-parts (make-matching-part part)))
        (recur remaining final-body-parts))))))
(symmetrize-body-parts asym-hobbit-body-parts)

;;breakdown:
;;4.2 let.
;; binds names on the right to values on the left.  
;; In this case binds a destructured vector (decomposed to part and remaining) to remaining-asym-body-parts.
;; then binds 'final-body-parts' to the previous value of final-body-parts with part appended to the end.
    (let [[part & remaining] remaining-asym-body-parts 
         final-body-parts (conj final-body-parts part)] 
;;Importantly, let introduces a new scope, so you can shadow previously defined variables.
(def x 0)
(let [x 1] x)
;;vs
(def x 0)
(let [y 1] x)
;;the value of a let form is the last form in its body which gets evaluated

;;4.3 loop.
;;loop provides another way to do recursion.
;;first line of loop introduces bindings:
;; here we bind remaining-asym-body-parts initially to asym-body-parts
;; and the inital value bound to final-body-parts is []
  (loop [remaining-asym-body-parts asym-body-parts final-body-parts []]
         final-body-parts (conj final-body-parts part)] 
      (if (needs-matching-part? part) 
        ;;recur is like calling the anonymous function created by loop
        (recur remaining (conj final-body-parts (make-matching-part part)))
        (recur remaining final-body-parts))))))

;;some runnable examples:
(loop [iteration 0]
  (println (str "iteration " iteration))
  ;;can think of this as manually putting in a loop termination condition
           (if (> iteration 4)
             (print "done")
             ;;think of recur as sort of like continue, but manually setting the variables for the next iteration 
             (recur (inc iteration))))

;;defined without use of loop:
(defn iterPrint
  ([]
  (iterPrint 0))
  ([iteration]
   (println (str "iteration " iteration))
   (if (> iteration 4)
     (println "done")
     (iterPrint (inc iteration)))
   ))
(iterPrint)
 
;;4.4 Regular expressions
;; pound, open quote, close quote.
#"regular expression"
;; in the symmetrizer, re-find uses one to find whether the :name starts with "left-"
;;then make-matching-part uses a regex to replace "left-" with "right-"

;;4.5 shorter symmetrizer with reduce.
;; sum with reduce
(reduce + [1 2 3 4])
;; alternatively
(+(+ (+ 1 2) 3) 4)
;;Reduce does:
 ;;apply given function to the first 2 elements of a sequence, then apply the function to that result and the next element of the sequence, and so on
;;also takes an initial value:
(reduce + 15 [1 2 3 4])

;;a way that reduce could be implemented:
(defn my-reduce 
  ([f initial coll]
  (loop [result initial 
         remaining coll]
    (let [[current & rest] remaining]
    (if (empty? remaining)
      result 
      (recur (f result current) rest)))))
  ([f [head & tail]]
   (my-reduce f (f head (first tail)) (rest tail)))
  )
(my-reduce + 15 [1 2 3 4])

;; the hobbit part thing with reduce
( def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])


(defn needs-matching-part?
  [part]
  (re-find #"^left-" (:name part)))

(defn make-matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-" )
   :size (:size part) })

;;this would be what we use in place of something like our sum function in the previous
;;usage of reduce.  There, we had a function that took in two parameters, (an unseen item and the result of all our prior operations)
;; and it returned the sum of those (an integer), which would have been the last of the 'result of all prior operations'.  
;; Here we will take in two parameters and return a collection (rather 
;; than a sum).
(defn collectSymmetrized
  [resultCol part]
  (if (needs-matching-part? part)
      (conj resultCol (make-matching-part part) part)
      (conj resultCol part)
    )
  )

(defn symmetrize-body-parts
  "Expects sequence of maps, each of which have one :name and one :size"
  [asym-body-parts]
  (reduce collectSymmetrized [] asym-hobbit-body-parts))
(symmetrize-body-parts asym-hobbit-body-parts)


