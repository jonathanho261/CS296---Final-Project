(ns adventure.core
  (:gen-class))
  

(require '[clojure.string :as str])


;; Map
(def init-map
  {:PAR {:desc "Welcome to Pennsylvania Avenue Residence Hall! You can smell food from the dining hall and see lots of excited freshman."
           :title "in PAR"
           :dir {:north :CRCE}
           :contents #{:icard}}
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
              :contents #{:book}}
    :DCL {:desc "Big place. CBTF in the basement, Engineering Career Services on the third floor."
              :title "in DCL"
              :dir {:south :grainger :north :ECEB}
              :contents #{}}
    :ECEB {:desc "Smells worse than Grainger. The ECE department should invest in showers..."
              :title "in ECEB"
              :dir {:south :DCL :east :siebel}
              :contents #{:free-tshirt}}
    :siebel {:desc "Best place on campus... but also a lot of nerds."
              :title "in Siebel"
              :dir {:west :ECEB}
              :contents #{:diploma}}
   })

;; Items
(def init-items
 {:icard {:desc "Don't forget your identication!"
            :name "iCard" }
  :corncob {:desc "It's a corncob! Might want to cook it before you eat it!"
            :name "corncob"}
  :corn-on-a-cob {:desc "Mhmmm... looks delicious."
            :name "corn on a cob"}
  :book {:desc "It's a book."
            :name "book"}
  :free-tshirt {:desc "It's a t shirt. It's free. It's a free tshirt."
            :name "free-tshirt"}
  :diploma {:desc "Piece of paper, seems kinda useless."
            :name "diploma"}
  
  })


;; Adventurer
(def init-adventurer
  {:location :PAR
   :inventory #{}
   :name "Steve"
   :goal "eat corn from the Blessed Morrow Plots"
   :tick 0
   :seen #{}})

(defn examine [state item]
  (let [inventory (get-in state [:adventurer :inventory])]
    (cond
     (contains? inventory item) (when true
                                  (println "You have a(n)" (name item) "\b.\n"(get-in state [:items item :desc]))
                                  state
                                )
      :else (when true
              (println (str "You don't have that item" (get-in state [:adventurer :name]) "!"))
              state
            ) 
     )
  )
)

(defn cook [state item]
  (let [inventory (get-in state [:adventurer :inventory])]
  ; (print (get-in (update-in (update-in state [:adventurer :inventory] #(disj % :corncob)) [:adventurer :inventory] #(conj % :corn-on-a-cob)) [:adventurer]))
  ; (print (contains? inventory :corncob))
    (cond 
      ; (inventory :corncob) (update-in (update-in state [:adventurer :inventory] #(disj % :corncob)) #(conj % :corn-on-a-cob))
      
      ; (update-in (dissoc-in state [:adventurer :inventory item]) conj :corn-on-a-cob)
      ; (print (disj inventory :corncob))
      (contains? inventory :corncob) (when true
                                        (println "You cooked a corncob and got a corn-on-a-cob! Eat it!\n")
                                        (update-in (update-in state [:adventurer :inventory] #(disj % :corncob)) [:adventurer :inventory] #(conj % :corn-on-a-cob))  
                                        )
      :else (when true
                (println "You can't cook that!")
                state)
      )
      ; (println "You can't cook that!")
      ; state
  )
)

(defn inventory [state]
  (println (get-in state [:adventurer :inventory]))
  state
  )

(defn biography [state]
  (println (str "\n\n\nHi my name is " (get-in state [:adventurer :name]) ".\nMy goal in life is " (get-in state [:adventurer :goal]) ".\nHelp me achieve my dream!\n"))
)

(defn quit [state]
  (update-in state [:done] not)
  )


(defn take [state item]
  (let [location (get-in state [:adventurer :location])
        contents (get-in state [:map location :contents])]
        ; (println contents)
        ; (println item)
        ; (println (contains? contents item))
        (cond
          (contains? contents item) (when true
                                      (println (str "You picked up " (name item) ".\n"))
                                      (update-in (update-in state [:map location :contents] #(disj % item)) [:adventurer :inventory] #(conj % item))
                                    )
          :else (when true
                  (println "You can't pick that up!\n")
                  state
                )
          
          ))

)


(defn drop [state item]
  (let [inventory (get-in state [:adventurer :inventory])]
    (cond
      (contains? inventory item) (when true
                                    (println (str "You dropped " (name item) ".\n"))
                                    (update-in state [:adventurer :inventory] #(disj % item))
                                    )
      :else (when true
              (println "You don't have that item!")
              state)
      )
    )
  )

(defn eat [state item]
  (let [inventory (get-in state [:adventurer :inventory])]
    (cond
      (and (contains? inventory :corn-on-a-cob) (= item :corn-on-a-cob)) (when true
                                    (println )
                                    (println (str "You ate " (name item) "."))
                                    (println (str "You've eaten the fruit of the holy Morrow Plots. You feel a sense of pride and accomplishment."))
                                    (println "You've done it, you've won at life.")
                                    (println )
                                    (update-in (update-in state [:adventurer :inventory] #(disj % item)) [:done] not)
                                    )
      :else (when true
              (println "You can't eat that item!\n")
              state)
      )
    )
  )

; (defn pickup [state item])

(defn status [state]
  (let [location (get-in state [:adventurer :location])
        the-map (:map state)]
    (println (str "\nYou are " (-> the-map location :title) ". "))
    (when-not ((get-in state [:adventurer :seen]) location)
      (println (-> the-map location :desc)))
    (println (str "Items in the room: " (get-in state [:map location :contents])))
    (println (str "Directions you can go: " (get-in state [:map location :dir])))
    (update-in state [:adventurer :seen] #(conj % location))
    ))

(defn go [state dir]
  (let [location (get-in state [:adventurer :location])
        dest ((get-in state [:map location :dir]) dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          state)
      (assoc-in state [:adventurer :location] dest))))

(defn north [state]
  (let [location (get-in state [:adventurer :location])
        dest ((get-in state [:map location :dir]) :north)]
        (print dest)
    (if (nil? dest)
      (do (println "You can't go that way.")
          state)
      (assoc-in state [:adventurer :location] dest))))


(defn east [state]
  (let [location (get-in state [:adventurer :location])
        dest ((get-in state [:map location :dir]) :east)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          state)
      (assoc-in state [:adventurer :location] dest))))

(defn west [state]
  (let [location (get-in state [:adventurer :location])
        dest ((get-in state [:map location :dir]) :west)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          state)
      (assoc-in state [:adventurer :location] dest))))

(defn south [state]
  (let [location (get-in state [:adventurer :location])
        dest ((get-in state [:map location :dir]) :south)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          state)
      (assoc-in state [:adventurer :location] dest))))


;; # Action Environment
;;
;; The runtime environment is a vector of the form
;;
;; ``` [ phrase action phrase action ...]```
;;
;; The "phrase" is a canonicalized vector of keywords.
;; (Remember that a keyword is a symbol starting with a colon, like :name.)
;; The `@` character will represent a variable.

(def initial-env [  
                    [:go "@"] go
                    [:north] north
                    [:n] north
                    [:south] south
                    [:s] south
                    [:east] east
                    [:e] east
                    [:west] west
                    [:w] west
                    [:cook "@"] cook
                    [:inventory] inventory
                    [:i] inventory
                    [:drop "@"] drop
                    [:eat "@"] eat
                    [:quit] quit
                    [:exit] quit
                    [:take "@"] take
                    [:pickup "@"] take
                    [:biography] biography
                    [:examine "@"] examine
                    [:look "@"] examine
                    ]
)  ;; add your other functions here

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


(defn -main
  "Initialize the adventure"
  [& args]
  (let [intro {:adventurer init-adventurer  :runtime initial-env }]
    (react intro [:biography]))

  (loop [local-state {:map init-map :adventurer init-adventurer :items init-items :runtime initial-env :done false}]
  (if (get-in local-state [:done])  (println (str "Actions taken: " (get-in local-state [:adventurer :tick]) "\nBye!"))
  
  ;(when true
                            ; (println (str "Actions taken: " (get-in local-state [:adventurer :tick])))
                          ;   (println "G'Bye!")
                          ; )
    (let [pl (status local-state) 
          _  (println "What do you want to do?")
          command (read-line)]
          ; (go pl :north)
          ; (print (get-in pl [:adventurer]))
          ; (print (get-in (react pl (canonicalize command)) [:adventurer :location]))
          ; (print (get-in pl [:adventurer :inventory]))
          ; (print (react pl (canonicalize command)))
          ; (print (cook pl :corncob))
          ; (print (get-in (update-in (react pl (canonicalize command)) [:adventurer :tick] inc) [:adventurer]))
      (recur (update-in (react pl (canonicalize command)) [:adventurer :tick] inc) )))))

; (defn -main
;   "I don't do a whole lot ... yet."
;   [& args]
;   (println "Hello, World!")
;   (print (repl initial-env)))


