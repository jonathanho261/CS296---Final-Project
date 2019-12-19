(ns adventure.core
  (:gen-class))
  

(require '[clojure.string :as str])


;; Map
(def init-map
  {:PAR {:desc "Welcome to Pennsylvania Avenue Residence Hall! You can smell food from the dining hall and see lots of excited freshman."
           :title "in PAR"
           :dir {:north :CRCE}
           :contents #{:iCard}}
   :CRCE {:desc "The smaller gym on campus. You can see some buff dudes trying too hard and some awkward skinny people looking lost."
              :title "in CRCE"
              :dir {:south :PAR :north :illini-union :west :morrow-plots}
              :contents #{}}
    :morrow-plots {:desc "Oldest experimental cornfield in the world."
              :title "at the Morrow Plots"
              :dir {:east :CRCE}
              :contents #{:corncob}}
    :illini-union {:desc "Big building on the main quad."
              :title "in the Union"
              :dir {:south :CRCE :west :green-street :north :grainger}
              :contents #{}}
    :green-street {:desc "Lots of cars, people, and shops. Looks like a good place to hangout."
              :title "on Green Street"
              :dir {:east :illini-union}
              :contents #{}}
    :grainger {:desc "Filled with a bunch of nerds. Smells kind of funky. Not the place to be."
              :title "in Grainger"
              :dir {:south :illini-union :north :DCL}
              :contents #{}}
    :DCL {:desc "Big place. CBTF in the basement, Engineering Career Services on the third floor."
              :title "in DCL"
              :dir {:south :grainger :north :ECEB}
              :contents #{}}
    :ECEB {:desc "Smells worse than Grainger. The ECE department should invest in showers..."
              :title "in ECEB"
              :dir {:south :DCL :east :siebel}
              :contents #{}}
    :siebel {:desc "Best place on campus... but also a lot of nerds."
              :title "in Siebel"
              :dir {:west :ECEB}
              :contents #{}}
   })

;; Items
(def init-items
 {:iCard {:desc "Don't forget your identication!"
            :name "iCard" }
  :corncob {:desc "It's a corncob! Might want to cook it before you eat it!"
            :name "corncob"}
  :corn-on-a-cob {:desc "Mhmmm... looks delicious."
            :name "corn on a cob"}
  
  })


;; Adventurer
(def init-adventurer
  {:location :PAR
   :inventory #{}
   :name "Steve"
   :hobbies "Eating corn"
   :tick 0
   :seen #{}})



;; # Interaction
;; A simple parser that can remember facts.
;;

;; # Actions
;;
;; Let an *action* be a function with a specific
;; interface: It will take in a *state* followed by
;; a sequence of arguments.  It will return a vector
;; consisting of a new state and a response string.
;;
;; Here is a simple example, a post-increment function.
;; Assume the state is a hash-map with a key :vars
;; and a value containing a hash of the variables and
;; their values.


(defn post-increment
  "Action: post-increment
   Returns the value of a variable from the state and increments it."
  [state var]
  (if-let [val (get-in state [:vars var])]
    [(update-in state [:vars var] inc) val]
    [(assoc-in state [:vars var] 1) 0]))

;; <pre><code>
;; interaction.core=> (post-increment {:vars {:x 10}} :x)
;; [{:vars {:x 11}} 10]
;; </code></pre>

;; ## Your work
;;
;; Fill in the code for these functions.
;;

(defn lookup-var
  "Given a state and a variable name, return the value of the variable
  if it has been defined, otherwise return 0."
  [state var]
  (if-let [val (get-in state [:vars var])]
    [state val]
    [state 0])
  )

;; <pre><code>
;; interaction.core=> (lookup-var {:vars {:x 10}} :x)
;; [{:vars {:x 10}} 10]
;; </code></pre>

(defn set-plus
  "Action: set-plus.  Set var = e1 + e2, return the sum as a result."
  [state var e1 e2]
  (cond
     (and (number? e1) (number? e2)) [(assoc-in state [:vars var] (+ e1 e2)) (+ e1 e2)]
     (number? e1) [(assoc-in state [:vars var] (+ e1 (second (lookup-var state e2)))) (+ e1 (second (lookup-var state e2)))]
     (number? e2) [(assoc-in state [:vars var] (+ e2 (second (lookup-var state e1)))) (+ e2 (second (lookup-var state e1)))]
     :else [(assoc-in state [:vars var] (+ (second (lookup-var state e1)) (second (lookup-var state e2)))) (+ (second (lookup-var state e1)) (second (lookup-var state e2)))]
      )
    ; [(update-in state [:vars var] e2) (+' (get-in state [:vars e1]) e2)]
)

;; <pre><code>
;; interaction.core=> (set-plus {:vars {:x 10}} :y :x 20)
;; [{:vars {:x 10 :y 30}} 30]
;; </code></pre>

(defn set-var
  "Action: set-var. Set var = e1.  Return the new value as a result."
  [state var e1]
  (let [val (get-in state [:vars var])]
    (if (= 0 (second (lookup-var state e1))) [(assoc-in state [:vars var] e1) e1]
    [(assoc-in state [:vars var] (second (lookup-var state e1))) (second (lookup-var state e1))])))

;; <pre><code>
;; interaction.core=> (set-var {:vars {:x 10}} :y :x)
;; [{:vars {:x 10 :y 10}} 10]
;; </code></pre>

(defn there-is-a
  "Action: there-is-a.  Remember that an object obj exists.
  Returns \"There is a obj\" as a result."
  [state object]
  [(assoc-in state [:objects object] []) (str "There is a " (name object) ".")]
  )

;; <pre><code>
;; interaction.core=> (there-is-a {:vars {:x 10}} :shoe)
;; [{:vars {:x 10 :y 10}
;;   :objects {:shoe []}} "There is a shoe."]
;; </code></pre>

(defn the-obj-is
  "Action: there-obj-a.  Remember an adjective that applies to an object.
  Returns \"The obj is adj\" as a result."
  [state object adj]
  [(update-in state [:objects object] conj adj) (str "The " (name object) " is " (name adj) ".")]
  )
;; <pre><code>
;; interaction.core=> (the-obj-is {:vars {:x 10} :objects {:shoe []}} :shoe :blue)
;; [{:vars {:x 10} :objects {:shoe [:blue]}} "The shoe is blue."]
;; </code></pre>



(defn describe-obj 
  "Describe the given object \"The obj is adj\" if it exists in state . If not, return \"There is no obj\""
  [state object]
  (let [adjs '()
        objs (state :objects)]
      (cond
        (= nil (objs object)) [state (str "There is no " (name object) ".")]
        :else (loop [retval [state]
                    nu-objs (objs object)]
                      (if (empty? nu-objs) retval
                      ; (print nu-objs)
                      ; (conj retval (second (the-obj-is state object (first nu-objs))))
                        (recur (conj retval (second (the-obj-is state object (first nu-objs)))) (rest nu-objs))))))
)
;; <pre><code>
;; interaction.core=> (describe-obj  {:vars {:x 10} :objects {:shoe [:blue :big]}} :shoe)
;; [{:vars {:x 10}, :objects {:shoe [:blue :big]}} "The shoe is blue." "The shoe is big."]
;; </code></pre>

(defn dissoc-in
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (assoc m k newmap))
      m)
    (dissoc m k)))


(defn forget-obj
  "Delete the given object and return \"What obj?\""
  [state object]
  [(dissoc-in state [:objects object]) (str "What " (name object) "?")]
  )
;; <pre><code>
;; interaction.core=> (forget-obj {:objects {:show [:exciting]}} :show)
;; [{:objects {}} "What show?"]
;; </code></pre>


;; # Action Environment
;;
;; The runtime environment is a vector of the form
;;
;; ``` [ phrase action phrase action ...]```
;;
;; The "phrase" is a canonicalized vector of keywords.
;; (Remember that a keyword is a symbol starting with a colon, like :name.)
;; The `@` character will represent a variable.

(def initial-env [  [:postinc "@"] post-increment
                    [:lookup "@"] lookup-var
                    [:set "@" :to "@" :plus "@"] set-plus
                    [:set "@" :to "@"] set-var
                    [:there :is :a "@"] there-is-a
                    [:the "@" :is "@"] the-obj-is
                    [:describe "@"] describe-obj
                    [:forget "@"] forget-obj])  ;; add your other functions here

;; # Parsing
;;
;; This is mostly code from the lecture.

(defn canonicalize
  "Given an input string, strip out whitespaces, lowercase all words, and convert to a vector of keywords."
  [input]
     (map keyword (-> input
    str/lower-case
    (str/replace #"[?!.]" "")
    (str/split #" +")
  ))
)
;; <pre><code>
;; interaction.core> (canonicalize "The shoe is blue.")
;; [:the :shoe :is :blue]
;; </code></pre>

(defn match [pattern input]
  (loop [pattern pattern
    input input
    vars '()]
      (cond (and (empty? pattern) (empty? input)) (reverse vars)
        (or (empty? pattern) (empty? input)) nil
        (= (first pattern) "@")
          (recur (rest pattern)
            (rest input)
              (cons (first input) vars))
        (= (first pattern) (first input))
          (recur (rest pattern)
            (rest input)
              vars)
        :fine-be-that-way nil
      )
  )
)

(defn react
  "Given a state and a canonicalized input vector, search for a matching phrase and call its corresponding action.
  If there is no match, return the original state and result \"I don't know what you mean.\""
  [state input-vector]
  ; (let [input (canonicalize input-vector)]
    ; (print input)
    (loop [idx 0]
      (if (>= idx (count (state :runtime))) "I don't know what you mean."
        (if-let [vars (match (get (state :runtime) idx) input-vector)]
          ; (apply (state (inc idx)) vars)
          (apply (get (state :runtime) (inc idx)) state vars)
          (recur (+ idx 2))))
    )
  )

;; <pre><code>
;; interaction.core> (react {:vars {:x 10} :runtime initial-env} [:postinc :x])
;; [ {:vars {:x 11} :runtime { ... omitted for space ... }}  10]
;; </code></pre>

(defn repl
  "Start a REPL using the given environment.  The :runtime key should map to the action environment.
  Prints out the result and loops the state for another round.  Quits when you say bye.
  You may need (flush) to print out '>' without a newline "
  [env]
  (react {} (read-line))
  )


(defn status [state]
  (let [location (get-in state [:adventurer :location])
        the-map (:map state)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((get-in state [:adventurer :seen]) location)
      (print (-> the-map location :desc)))
    (update-in state [:adventurer :seen] #(conj % location))
    ))

; (defn go [state dir]
;   (let [location (get-in [:adventurer :location] player)
;         dest ((get-in [:map location :dir] state) dir)]
;     (if (nil? dest)
;       (do (println "You can't go that way.")
;           player)
;       (assoc-in state [:adventurer :location] dest))))

(defn -main
  "Initialize the adventure"
  [& args]
  (loop [local-state {:map init-map :adventurer init-adventurer :items init-items}]
    (let [pl (status local-state) 
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-state ))))

; (defn -main
;   "I don't do a whole lot ... yet."
;   [& args]
;   (println "Hello, World!")
;   (print (repl initial-env)))
