(ns the-regenetron.core
  (:require
    [reagent.core :as reagent :refer [atom]]
    [cljs.pprint :as pp]
    [clojure.string :as string]))

(enable-console-print!)

(println "This text is printed from src/the-regenetron/core.cljs. Go ahead and edit it and see reloading in action.")



;; TODO:
;;  * make npc algorithm handle movement around the map
;;  * make npc algorithm act as well as plan!
;;  * game loop / structure?
;;     * ticks.
;;         every user actions takes a number of ticks
;;         same number of ticks per npc - some npc actions will interupt user?
;;                                      - debug opportunity for all npc actions every tick
;;  * explore having a more complex world - more map / more tasks
;;      what can the AI support? some play required here and some tweaking
;;  * some thought about missions, story, modal simulation / phase transitions
;;
;;
;;
;;  sketch:
;;
;;   make automata
;;     every tick
;;       if doing something - continue doing it (but maybe consider changing task occassionally?
;;       if doing nothing, choose a new task
;;         needs - food (quantity, variety, quality),
;;               - water
;;               - sleep,
;;               - company,
;;               - warmth, ???
;;               - security (surplus food, water in house)
;;         known actions
;;               - sleep (best in a bed)
;;               - stop and rest for a bit
;;               - pick berries
;;               - fetch water
;;               - fetch wood
;;               - dependant actions:
;;                  - to pick berries I must first go to the bushes
;;                  - to drink water I must first go to the well
;;                  - to fetch water I must first be in possession of a bucket
;;                  - to fetch wood I must first be in possession of an axe
;;         must construct complete plan of action, eg. go to house,
;;                                                     pick up bucket,
;;                                                     go to well,
;;                                                     fill up bucket,
;;                                                     go to house,
;;                                                     pour water into butt.
;;            plan must be costed (eg. "17 ticks")
;;   make player controlled character
;;   make inevitable plot arc
;;     early exploration phase
;;     missions from regenetron (change her name?)
;;     contagious violence
;;       stealing, lying
;;     karma system (affects generosity, when overall karma is low, system must become less efficient - specialisation of actors)
;;       possible action - ask another character for a thing - if karma is right and they can help, then they will oblige
;;     specialisation
;;       pride in a task as a need (to drive specialisation)?
;;       security -> stockpile of things one is good at making?
;;       lower tick cost to perform some subtasks (other costs raised to compensate?)
;;
;;   more ideas: bread baking, axe making, basket weaving, crop harvesting, goat milking, chickens, day-night cycle,
;;               regular trading times - or just go to people's houses and wait
;;
;;   yet more ideas: at least one character who uses non-traditional gender pronouns
;;                   varied characters: a witch?, a wizard?, King Gizzard?,
;;                      Rene Girard, Socrates, Julie D'Aubigny, ...




;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;;    Static data    ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO: is every npc going to have it's own map as well as the world having territory?
(def places {:beach {:label "the beach"}
             :village-0 {:label  "the centre of the village"
                         :has #{:well}}
             :village-1 {:label  "the South side of the village"}
             :village-2 {:label  "the North side of the village"}
             :village-3 {:label  "the East side of the village"
                         :has #{:ada's-house}}
             :village-4 {:label  "the West side of the village"
                         :has #{:player's-house}}
             :north-borders {:label  "the North Borders"
                             ;:object 100
                             ;:has {:id  :berry-bushes       ; TODO: not :has here, because :has is mutable
                             ;      :has [{:id     :berries
                             ;             :amount 100}]}
                             }
             })

(def raw-links
  [[:beach     "the beach"                     14 18 "the village"         :village-1]
   [:village-1 "the South side of the village"  5  5 "the village centre"  :village-0]
   [:village-2 "the North side of the village"  5  5 "the village centre"  :village-0]
   [:village-3 "the East side of the village"   5  5 "the village centre"  :village-0]
   [:village-4 "the West side of the village"   5  5 "the village centre"  :village-0]
   [:village-2 "the village"                   12 17 "the North Borders"   :north-borders]
   ])

(defn make-links [links]
  (into {}
        (for [[a _ _ ->b-ticks ->b-label b] links]
          (do
            (when-not (places a) (println "UNKNOWN PLACE" a))
            (when-not (places b) (println "UNKNOWN PLACE" b))
            [a [{:to    b
                 :label ->b-label
                 :ticks ->b-ticks}]]))))

(def links
  (merge-with (comp vec concat)
              (make-links raw-links)
              (make-links (map reverse raw-links))))

(println links)

(comment

  (def raw-steps
    [
     [:drink-water #{:water-at-hand} 1 #{:less-thirst}]
     [:draw-water #{:well} 3 #{:water-at-hand}]
     [:eat-berries #{:berries-in-hand} 1 #{:less-hunger}]
     [:pick-berries #{:berry-bushes} 3 #{:berries-in-hand}]
     ])

  (def tasks

    ; should always(?) be possible to move to any of the places
    ;
    )

  (def need-multipliers                                     ; unnecesary? we can control the rate of change in both directions
    {:drink 1000
     :sleep 100
     :food  100}))

;(def objects
;  (atom
;    {100 {:desc "North Borders"
;          :has  [5]}
;
;     1 {:desc "axe"}
;     2 {:desc "berry"}
;     3 {:desc "berry"}
;     4 {:desc "berry"}
;     5 {:desc "berry bush"
;        :has  [2 3 4]}
;     }))

; TODO: =======================================================
; {[:loc :id] {details...}
;  [:npc :id] {details...}
;  [:npc :id] {details...}}
; TODO: =======================================================

(def abstract
  #{:food
    :drink
    :sleep
    :chat
    :warmth
    :security})

(def steps
  ; TODO!!! Verbs should be parameterised! "Eat <item>"
  ; or is it better not to? all the specific information must be specified -
  ; eating berries IS different to eating bread
  ;  - you need berries!
  ;  - it provides a different amount of calories
  ;  - berries have more water in them (provide more drink)
  [
   {:name     "drink water"
    :ticks    1
    :provides {[:npc :drink] {:quantity 1}}
    :consumes {[:npc :water] {:quantity 1}}}

   {:name     "draw water"
    :ticks    5
    :provides {[:loc :water] {:quantity 5}}
    :requires {[:loc :well] {:quantity 1}}}

   {:name     "eat berry"
    :ticks    2
    :provides {[:npc :drink] {:quantity 0.2}
               [:npc :food]  {:quantity 0.8}}
    :consumes {[:npc :berry] {:quantity 1}}}

   ;{:name     "pick berry"                                  ; TODO: somehow model transferring berries from the bush to the hand.
   ; :ticks    3
   ; :provides {:id  :hand
   ;            :has {:berries 1}}
   ; :consumes {:id  :hand
   ;            :has :empty}
   ; :requires {:location {:has {:id  :berry-bushes          ; this is convoluted - :requires {:berries 1} should suffice???
   ;                             :has {:id     :berries
   ;                                   :amount 1}}}}}

   ;{:name     "pick berry"
   ; :ticks    3
   ; :provides {[:npc :berry] {:quantity 1}}
   ; :consumes {[:loc :berry] {:quantity 1}}}

   ; special case logic for actions that move things around?
   ;   "take" move <x> from 'at location' to 'holding' and set ownership
   ;   "drop" inverse ^
   ; maybe a function to return steps based on things they provide - returns all matching steps, plus synthetic ones

   ; "ensure holding" <x> ("hold")

   {:name     "sleep"
    :ticks    80
    :provides {[:npc :sleep] {:quantity 100}}
    :requires {[:loc :bed] {:owned true}}}

   {:name     "nap"
    :ticks    20
    :provides {[:npc :sleep] {:quantity 10}}}

   {:name     "fell a tree"
    :ticks    100
    :requires {[:npc :axe] {:quantity 1}}
    :consumes {[:loc :standing-tree] {:quantity 1}}
    :provides {[:loc :felled-tree] {:quantity 1}}}
   ]

  )


(defn merge-key [m k f a b]
  (let [v (f (k a) (k b))]
    (if v
      (assoc m k v)
      m)))

(defn nil+ [a b]
  (if (and a b)
    (+ a b)
    (or a b)))

(defn add-objects [a b]
  (-> {}
      (merge-key :quantity (fnil + 1 1) a b)
      ;TODO: merge other attributes
      ))

(defn subtract-objects [a b]
  (-> {}
      (merge-key :quantity (fnil - 1 1) a b)
      ;TODO: merge other attributes
      ))

(def merge-add (partial merge-with add-objects))
(def merge-subtract (partial merge-with subtract-objects))
(defn merge-remove [a b]
  (apply dissoc a (keys b)))

(defn has->provides
  "Traverses the :has tree for a node and builds a map of what it provides."
  [npc-loc thing]
  ;(println 'has->provides)

  (let [children (:has thing)
        children-maps (for [child children]
                       {[npc-loc (:id child)] (dissoc child :id)})]
    (apply merge-add
      (concat children-maps
              (when (seq children)
                (map (partial has->provides npc-loc) children)))))

  ;(let [children (:has thing)
  ;      children-ids (map #(vector npc-loc (:id %)) children)]
  ;  (distinct
  ;    (concat
  ;      children-ids
  ;      (when (seq children)
  ;        (mapcat (partial has->provides npc-loc) children)))))

  ; TODO
  ;convert what teh npc :has (recursively) to a map of what they can provide
  ;{:provides {:at :berry-bushes}}
  )

(def npc-needs [[:npc :food]
                [:npc :drink]
                [:npc :sleep]
                [:npc :chat]
                [:npc :warmth]
                [:npc :security]])

(def default-npc-reqs
  (zipmap npc-needs (repeat {:quantity 0})))

(def game-state
  (atom

    {
     ;:objects   {1 {:id :axe}
     ;            2 {:id :berry}
     ;            3 {:id :berry}
     ;            4 {:id :berry}
     ;            5 {:id :berry-bush
     ;               :has [2 3 4]}
     ;            }

     :locations {:north-borders {:id  :north-borders
                                 :has [{:id  :berry-bush
                                        :has [{:id :berry}
                                              {:id :berry}
                                              {:id :berry}]}]

                                 ; How much detail does :provides need?
                                 ; maybe try just a list of ids for now?
                                 ;:provides [{:id :berry-bush
                                 ;            :has [{:id :berry}
                                 ;                  {:id :berry}
                                 ;                  {:id :berry}]}              ; derived from :has when :has changes
                                 ;           {:id :berry}
                                 ;           {:id :berry}
                                 ;           {:id :berry}]

                                 ;{[:loc :berry-bush] {:quantity 1}
                                 ; [:loc :berry]      {:quantity 3}}
                                 }
                 :village-0     {:id  :village-0
                                 :has [{:id :well}]}

                 :beach         {:id  :beach
                                 :has [{:id  :palm-tree
                                        :has [{:id       :berry
                                               :quantity 5}]}]}

                 } ;TODO: pre-calculate per frame? on change? recursive :has

     :people    {:some-id? {:reqs        (merge
                                           default-npc-reqs
                                           {[:npc :food]  {:quantity 100} ; TODO: should these be negative?
                                            [:npc :drink] {:quantity 50}
                                            [:npc :sleep] {:quantity 10}
                                            [:npc :chat]  {:quantity 0} ; capped at 75? some way to differentiate between needs and wants?
                                            })
                            :has         [{:id       :water
                                           :quantity 3}            ; liquid hold limit is low, but can carry eg. a bucket that can hold more
                                          ;{:id       :berry
                                          ; :quantity 5}            ; fifo queue when picking things up - hold limit
                                          {:id  :bucket
                                           :has [{:id       :water
                                                  :quantity 50}]}]
                            :known-steps steps
                            :location    :beach
                            :busy        nil}

                 }

     }

    ))

(defn mapvals [m f]
  (into {} (for [[k v] m]
             [k (f v)])))

;TODO: when should this happen?
(swap! game-state update :locations mapvals
       (fn [loc]
         (assoc loc :provides (has->provides :loc loc))))

;(def object-locations
;  (atom
;    {:north-borders [5]}))

; TODO: Possession!!!
;   {:has {:has {:has ...}}} ?
;   locations possess
;     nested (perhaps deeply)
;   characters possess
;     in their hands - limited storage space! must be able to carry a sizeable log - maybe don't model individual hands!
;                    - lru cache? drop oldest thing picked up when picking up a new thing if you run out of carry space
;     in their houses
;     everywhere? - I chopped down that tree, but I can't carry it all at once! - is this a different concept ("ownership"?)
;     ownership - flag on item to say who owns it.
;               - also characters maintain mental list of things they own and where they are, so that their expectations can be violated.
;   steps
;     require possession in hand or in location
;
;   one big list of game objects
;     actually a map keyed by id
;     { "bag of holding" {:has #{"cheese sandwich"}
;                         :owned :bob / false}
;       "cheese sandwich" {} }



(defn fixed-steps-providing [npc item-key]
  (filter
    (fn [{:keys [provides]}]
      (get provides item-key))
    (:known-steps npc)))

(defn synthetic-steps-providing [locs [npc-loc item-id]]
  (if (= npc-loc :npc)
    (when-not (abstract item-id)
      [{:name     (str "take " (name item-id))
        :ticks    1
        :provides {[:npc item-id] {:quantity 1}}
        :consumes {[:loc item-id] {:quantity 1}}}])
    (for [[loc-id loc] locs :when (-> loc :provides (get [:loc item-id]))]
      {:name     (str "go to " (name (:id loc)))            ; better!
       :ticks    5                                             ; calculate!
       :provides (:provides loc)})))                        ;TODO: needs to be {[:loc :id] quantity} !!!

(defn steps-providing [npc locs item-key]
  (concat
    (fixed-steps-providing npc item-key)
    (synthetic-steps-providing locs item-key)))

;(def verbs
;  [
;   {:desc     "{who} drink|drinks {what}"
;    :ticks    1
;    :provides {:id     :drink
;               :amount :drinkable}
;    :consumes {:drinkable 1}}
;   {:desc     "{who} eat|eats {what}"
;    :ticks    [1 :edible]                                   ; (* 1 (:edible what))
;    :consumes {:edible 1}
;    }
;   ])

;(def nouns
;  [{:id        "water"
;    :drinkable {:provides {:drink 5}}}
;   {:id     "berry"
;    :edible {:ticks    2
;             :provides {:food  5
;                        :drink 1}}}])

;(comment
;  (defverb "{who} drink|drinks {what}" [place who what]
;
;           )
;  )

;(def npc
;  ; characters could have 'known-skills' - transferred by socialising, improved by practice (reduced cost)
;  {:needs       {:food     100
;                 :drink    50
;                 :sleep    10
;                 :chat     0                                ; capped at 75? some way to differentiate between needs and wants?
;                 :warmth   0
;                 :security 0}
;   :has         [{:id  :hand
;                  :has [{:id     :water
;                         :amount 1}]}
;                 {:id  :hand
;                  :has [{:id  :bucket
;                         :has [{:id     :water
;                                :amount 5}]}]}
;                 {:id  :house                               ; rooms / stuff? / precarity??? - not sure this is something an npc 'has' - distinguish between 'has - about person' and 'has - claims ownership'?
;                  :has []}]
;   :known-steps steps
;   :location    :beach
;   :busy        nil}
;
;  )

;(def items-by-need-addressed
;
;  {:thirst [{:water 1} {:berries 0.2}]
;   :hunger [{:berries 0.8} {:bread 1} {:meat 2}]
;   :tiredness [{:sleep 1}]}
;
;  )


; PLUS synthetic step {:name "go to <...>" :ticks <distance>


;(defn be-busy [npc]
;  ;TODO!
;  npc)

;(defn make-busy [npc map]
;
;  )

(defn ensure-greatest-need [npc]
  (if-let [reqs (get-in npc [:busy :reqs])]
    npc
    (assoc-in npc [:busy :reqs] (->> npc
                                     :reqs
                                     (sort-by (comp :quantity second))
                                     (take-last 1)
                                     (into {})))

    ;(let [[id amount] (last (sort-by (comp :quantity second) (:needs npc)))]
    ;  (assoc-in npc [:busy :need] {[:npc id] amount}))
    ))

(defn apply-chain [reqs step]
  (merge-subtract reqs (:provides step)))

(defn score-reqs [reqs]
  (apply - 0 (map (comp :quantity reqs) npc-needs)))

(defn score-option [npc option]
  (if (-> option :score :thought)
    option
    (let [cost (transduce (map :ticks) + (:chain option))   ; should we add value of things consumed? - how would we value them, AND - their only value is in their ability to be consumed!!!
          benefit (- (score-reqs (reduce apply-chain (:reqs npc) (:chain option)))
                     (score-reqs (:reqs npc)))]
      ;TODO:
      ; copmlete? (but possibly un-optimised)
      ;   cost? ticks and other things we value
      ;     divided by:
      ;   benefit? things we value
      ; else
      ;   minus static amount for each unmet dependency
      (assoc option :score {:cost    cost
                            :benefit benefit
                            :value   (float (/ benefit cost))
                            :thought (when (:optimised option) (:thinking (:busy npc)))})))
  )

(defn find-requirements [requirements step]
  (let [step-requires (:requires step)
        step-consumes (:consumes step)
        step-provides (:provides step)]
    (as-> requirements x
          (reduce conj x step-requires)
          (reduce conj x step-consumes)
          (reduce disj x step-provides))))

(defn add-step-to [{:keys [start chain end] :as option} reqs]
  (fn [step]
    (let [
          ;_ (println step)
          ;new-reqs (find-requirements reqs step)



          new-reqs (-> reqs
                       (merge-add (:requires step) (:consumes step))
                       (merge-remove (:provides step)))

          _ (println '(keys new-reqs) (keys new-reqs))
          _ (println (-> start :provides keys set))
          complete? (empty? (remove (-> start :provides keys set) (keys new-reqs)))
          _ (println complete?)
          ]
      (-> option
          (update :chain conj (assoc step :complete complete?
                                          :reqs new-reqs
                                          :min 1
                                          :n 1))
          (assoc :complete complete?)))))

(defn prepend-steps [{:keys [start chain end] :as option} npc locs]
  ;(println "prepend-steps")
  (let [reqs (or (some-> chain last :reqs) (:reqs end))
        ;reqs (reduce find-requirements (-> end :consumes keys set) chain)
        ;_ (println 1)
        ;_ (println '(keys reqs) (keys reqs))
        ;_ (println '[locs] [locs])
        useful-steps (distinct (mapcat (partial steps-providing npc locs) (keys reqs)))
        ;_ (println 2)
        ;_ (println 'useful-steps useful-steps)
        ;steps (:known-steps npc)
        ;useful-steps (remove #(->> % :provides keys (filter reqs) empty?) steps)
        ]
    (mapv (add-step-to option reqs) useful-steps)))

(defn optimise [option npc world]
  ; TODO
  ; iterate over - all individual items, all consecutive pairs/triples/quads/etc... adjusting multipliers
  ;    eg. "drink water" * (min water-available drinking-needed)
  (assoc option :optimised true)
  )

(defn consider-option [npc locs]
  (fn [{:keys [complete optimised] :as option}]
    ;(println "consider-option")
    (cond
      optimised [option]
      complete [(optimise option npc locs)]
      :else (prepend-steps option npc locs))))

(defn grow-options [[npc locs]]
  (let [options (get-in npc [:busy :options])
        loc-provides (-> npc :location locs :provides)
        ;need (get-in npc [:busy :reqs])
        options-considered 5]

    (if (empty? options)
      (-> npc
          (assoc-in [:busy :options] [{:start {:provides (merge-add (has->provides :npc npc)
                                                                    loc-provides)}
                                               :chain '()
                                               :end   {:reqs (get-in npc [:busy :reqs])}}])
          (assoc-in [:busy :thinking] 1)
          (update :busy dissoc :reqs))

      ; take first n options
      ;   incomplete?
      ;     prepend steps
      ;   else
      ;     optimise multipliers
      ; score options
      ; sort options

      (let [[to-consider unconsidered] (split-at options-considered options)
            considered (mapcat (consider-option npc locs) to-consider)
            ;_ (println 'X)
            _ (doall considered)
            ;_ (println 'Y)
            ]

        (-> npc
            (assoc-in [:busy :options] (->> (concat considered unconsidered)
                                            (mapv (partial score-option npc))
                                            (sort-by (comp :value :score))))
            (update-in [:busy :thinking] inc))))

    ; if options empty then seed from greatest-need
    ; if x options are complete then choose one to act on
    ; sort exiting options by cost:benefit (consumes/requires : provides)
    ; pick first n, and mapcat fn over them
    ;   fn extends each option to one or more options

    ;options [{:consumes {:ticks 10}                         ; must be recalculated by walking the whole path forward,
    ;          :provides {:drink 5}
    ;          :requires {:at :well}
    ;
    ;          :steps [{:name     "drink water"
    ;                   :ticks    1
    ;                   :provides {:drink 1}
    ;                   :consumes {:water 1}}
    ;                  {:name     "draw water"
    ;                   :ticks    5
    ;                   :provides {:water 5}
    ;                   :requires {:at :well}}]
    ;          }]

    ; need to iterate over options potentially extending them (or aborting them?)
    ; need to know which options are most worth considering


    ; list of options
    ;   mapping of every input option to 1 or more output options (or the same option unchanged if out of thinking time)


    ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; if option chain is incomplete, prepend steps that match requirements
    ; if option chain is complete, iterate over - all individual items, all consecutive pairs/triples/quads/etc... adjusting multipliers
    ;    eg. drink water x (min water-available drinking-needed)
    ; if option chain is complete, and multipliers have been adjusted, then report cost:benefit

    ; if thinking cost is high relative to patience then perform first completed option
    ; if thinking cost is low relative to patience then complete a few more options before picking the best by cost:benefit
    ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




    )
  )

  ; How about...
  ;   build hypothetical tree
  ;   for current state
  ;     try every known action (prepend - imagine we'd just done it)
  ;     score the results (are there still unmet requirements?, what is the cost? what is the benefit?)
  ;     for each resulting state (in score order - expend more resources thinking about more better outcomes)
  ;       try every known action (prepend to chain)
  ;       score the results
  ;   explore this tree
  ;     focus on high scoring paths
  ;     stop when a path (or several paths to choose between?) is found that has all its requirements met.
  ;     when needs are less urgent, more time can be spent thinking, looking for possible good action chains
  ;     when needs are urgent, pick first possible strategy that meets needs.

  ; this could be helped by 'habits' - actions that often go well together - eg. go to well, draw water, drink water

(defn describe-options [npc]

  (doseq [option (-> npc :busy :options)]

    (println
      (str (when-not (:complete option)
             "     ...   -> ")
           (string/join " -> " (map :name (reverse (:chain option))))))

    )
  npc
  )



(defn refresh-provides [node npc-loc]
  (assoc node :provides (has->provides npc-loc node)))

(defn remove-hasnts [node]
  (if-let [has (:has node)]
    (assoc node :has (map remove-hasnts (remove nil? has)))
    node))

(defn find-quantities
  "list all paths to quantities of the item-id"
  [x item-id]
  (cond
    (= (:id x) item-id) [[(or (:quantity x) 1)]]
    (:has x) (mapcat (fn [item idx]
                       (map #(cons idx %) (find-quantities item item-id))) (:has x) (range))
    :else []))

(defn consume [node item-id quantity]
  (let [paths (find-quantities node item-id)]
    (loop [x node
           quantity-to-consume quantity
           [path & paths] paths]
        (if (pos? quantity-to-consume)
          (if path                                        ; TODO: else exception?
            (let [quantity-available (last path)
                  remainder (- quantity-available quantity-to-consume)
                  x-path (interleave (repeat :has) (butlast path))]
              (if (pos? remainder)
                (assoc-in x (concat x-path [:quantity]) remainder)
                (recur
                  (assoc-in x x-path nil)
                  (- remainder)
                  paths)))
            (assoc x :error :not-enough-stuff!))            ; error, but return having made the progress we've made
          x))))

(defn step-consume [[npc locs] [npc-loc item] {:keys [quantity]}]
  (println 'step-consume)
  (if (= npc-loc :npc)
    [(remove-hasnts (consume npc item quantity)) locs]
    [npc (update locs (:location npc) #(remove-hasnts (consume % item quantity)))]))

(defn begin-step [[npc locs]]
  (println 'begin-step)
  (let [[step & todo] (:todo (:busy npc))
        consumes (:consumes step)
        [npc locs] (reduce-kv step-consume [npc locs] consumes)]
    (println 'begin-step_1)
    [(-> npc
         (update :busy assoc :doing step)
         (update :busy assoc :todo todo))
     (update locs (:location npc) #(refresh-provides % :loc))]))

(defn tick-step [[npc locs]]
  [(update-in npc [:busy :doing :ticks] dec) locs])

(defn provide [node item-id v]
  (update node :has conj (assoc v :id item-id)))

(defn step-provide [[npc locs] [npc-loc item] v]
  (if (= npc-loc :npc)
    [(provide npc item v) locs]
    [npc (update locs (:location npc) #(provide % item v))]))

(defn end-step [[npc locs]]
  (let [step (:doing (:busy npc))
        provides (:provides step)
        [npc locs] (reduce-kv step-provide [npc locs] provides)]
    [(update npc :busy dissoc :doing)
     (update locs (:location npc) #(refresh-provides % :loc))]))

(defn steps-completed [[npc locs]]
  [(update npc dissoc :busy) locs])

(defn make-progress [[npc locs]]

  ;TODO: :food, :drink etc. get added to :has tree, not removed from :reqs

  ; consume
  ; tick, tick, tick
  ; provide

  (let [{:keys [todo doing]} (:busy npc)]
    (cond
      (and todo (not doing)) (begin-step [npc locs])
      (pos? (:ticks doing)) (tick-step [npc locs])
      (and doing (zero? (:ticks doing))) (end-step [npc locs])
      (not todo) (steps-completed [npc locs]))))

(defn try-to-choose-an-option [npc]
  (let [thinking (-> npc :busy :thinking)
        best-option-so-far (-> npc :busy :options first)
        thought (-> best-option-so-far :score :thought)]
    (if (> thinking (+ thought 3))
      (-> npc
          (assoc-in [:busy :todo] (:chain best-option-so-far))
          (dissoc :options :thinking))
      npc)))

(defn choose-greatest-need [npc]
  (assoc-in npc [:busy :reqs] (->> npc
                                   :reqs
                                   (sort-by (comp :quantity second))
                                   (take-last 1)
                                   (into {}))))

(defn algorithm [[npc locs :as npc-locs]]


  (cond
    (or (-> npc :busy :doing)
        (-> npc :busy :todo)) (make-progress npc-locs)
    (-> npc :busy :reqs) [(try-to-choose-an-option (grow-options npc-locs)) locs]
    :else [(choose-greatest-need npc) locs])


  ;(let [busy (:busy npc)]
  ;
  ;
  ;
  ;  )



  ;(if (:busy npc)
  ;  (be-busy npc)
  ;  (make-busy npc map))
  ;npc
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;;    Game state     ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; defonce
(def state (atom {:story [{:html [:h1 "Welcome to The Regenetron"]}
                          {:html [:p "A paragraph of epic introductory text..."]}
                          {:html [:p "~"]}
                          {:html [:p "I awake to find myself lying on the beach in the glorious morning sun."]}]
                  :actors [{:name  "Ada Lovelace"
                            :needs {:drink  57
                                    :food  100
                                    :sleep 123
                                    }
                            :location :beach
                            :has #{}
                            :abode :adas-house}]
                  :pc     {:location :beach}
                  ;:people {:player {:location :beach
                  ;                 :actions  [{:id    1
                  ;                             :label "look around"}
                  ;                            {:id    2
                  ;                             :label "head into the village"}]}}
                  :time 0
                  :world {:places {:beach     {:people [:player]
                                               :things [:coconut]}
                                   :village-0 {:people []
                                               :things []}}}

                  }))

(defn actions [{:keys [people world]}]

  (let []) (-> people :player :location)

  )

(comment "actor data model:"
         ; angst = combination (sum?) of all need values
         {:name     "Ada Lovelace"
          :needs    {:drink 57
                     :food  100
                     :sleep 123
                     }
          :location :beach
          :has      #{}
          :abode    :adas-house
          :task     {:steps [{:step   :goto
                              :target :village-0
                              :ticks  18}
                             {:step  :draw-water
                              :ticks 3}
                             {:step :drink-water
                              :ticks 1}]}}

         )

(defn age-actor [needs]
  (-> needs
      (update :drink #(+ % 3))
      (update :food #(+ % 1))
      (update :sleep #(+ % 1))))

(defn tick-actor [{:keys [needs task] :as actor}]


  (assoc actor :needs (age-actor needs))


  ; if actor has a task in progress then continue task
  ; actor
  ;   task
  ;     step in progress, remaining ticks
  ;     remaining: step, step, step

  ; if actor has no task in progress, then make a plan
  ;   determine greatest need
  ;   select possible actions that meet need
  ;     cost them, and pick the cheapest

  )

(defn tick []
  (println "Hello"))

(defn render-story [{:keys [story]}]
  (into [:div] (map :html story)))

(defn render-controls [{:keys [pc]}]

  [:div

   [:div {:style {:display "inline-block"}}
    [:span "Health: " [:b "100% "]]
    [:br]
    [:span "Hunger: " [:b "10% "]]
    [:br]
    [:span "Tiredness: " [:b "10% "]]
    [:br]
    [:span "Location: " [:b (-> pc :location name)] " "]]

   [:div {:style {:display "inline-block"
                  :margin "10px"}}
    [:span "I "

     (into [:select]

           (->> pc :actions (mapv (fn [{:keys [id label]}]
                                    [:option {:value id} label]))))

     [:input {:type "button"
              :value "Tick!"
              :on-click tick}]
     ]]

   ]

  )

(defn render []
  (let [state @state]
    [:div
     (render-story state)
     (render-controls state)]))

(reagent/render-component [render] (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)



; TODO: input affects world state
;  movement, map

