(ns the-regenetron.core
    (:require
      [reagent.core :as reagent :refer [atom]]
      [cljs.pprint :as pp]))

(enable-console-print!)

(println "This text is printed from src/the-regenetron/core.cljs. Go ahead and edit it and see reloading in action.")



;; TODO:
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




;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;;    Static data    ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                             :has #{:berry-bushes}}
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

(def raw-steps
  [
   [:drink-water  #{:water-at-hand}    1 #{:less-thirst}]
   [:draw-water   #{:well}             3 #{:water-at-hand}]
   [:eat-berries  #{:berries-in-hand}  1 #{:less-hunger}]
   [:pick-berries #{:berry-bushes}     3 #{:berries-in-hand}]
   ])

(def tasks

  ; should always(?) be possible to move to any of the places
  ;
  )

(def need-multipliers ; unnecesary? we can control the rate of change in both directions
  {:drink 1000
   :sleep  100
   :food   100})

(def steps
  [
   {:name     "drink water"
    :ticks    1
    :provides {:drink 1}
    :consumes {:water 1}}

   {:name     "draw water"
    :ticks    5
    :provides {:water 5}
    :requires {:at :well}}

   {:name     "eat berry"
    :ticks    2
    :provides {:drink 0.2
               :food  0.8}
    :consumes {:berries 1}}

   {:name     "pick berry"
    :ticks    3
    :provides {:berries 1}
    :requires {:at :berry-bushes}}

   {:name     "sleep"
    :ticks    80
    :provides {:sleep 100}
    :requires {:at :home}}

   {:name     "nap"
    :ticks    20
    :provides {:sleep 10}}]

  )

(def npc
  ; characters could have 'known-skills' - transferred by socialising, improved by practice (reduced cost)
  {:needs       {:food     100
                 :drink    50
                 :sleep    10
                 :chat     0                                ; capped at 75? some way to differentiate between needs and wants?
                 :warmth   0
                 :security 0}
   :has         [{:id  :hand
                  :has [{:id     :water
                         :amount 1}]}
                 {:id  :hand
                  :has [{:id  :bucket
                         :has [{:id     :water
                                :amount 5}]}]}
                 {:id  :house                               ; rooms / stuff? / precarity??? - not sure this is something an npc 'has' - distinguish between 'has - about person' and 'has - claims ownership'?
                  :has []}]
   :known-steps steps
   :location    :beach
   :busy        nil}

  )

;(def items-by-need-addressed
;
;  {:thirst [{:water 1} {:berries 0.2}]
;   :hunger [{:berries 0.8} {:bread 1} {:meat 2}]
;   :tiredness [{:sleep 1}]}
;
;  )


; PLUS synthetic step {:name "go to <...>" :ticks <distance>


(defn be-busy [npc]
  ;TODO!
  npc)

(defn make-busy [npc map]

  )

(defn ensure-greatest-need [npc]
  (if-let [need (get-in npc [:busy :need])]
    npc
    (assoc-in npc [:busy :need] (last (sort-by second (:needs npc))))))

(defn score-option [option]

  ;TODO:
  ; copmlete? (but possibly un-optimised)
  ;   cost? ticks and other things we value
  ;     divided by:
  ;   benefit? things we value
  ; else
  ;   minus static amount for each unmet dependency
  option
  )

(defn find-requirements [requirements step]
  (let [step-requires (keys (:requires step))
        step-consumes (keys (:consumes step))
        step-provides (keys (:provides step))]
    (as-> requirements x
          (reduce conj x step-requires)
          (reduce conj x step-consumes)
          (reduce disj x step-provides))))

(defn add-step-to [{:keys [start chain end] :as option} reqs]
  (fn [step]
    (let [new-reqs (find-requirements reqs step)
          complete? (empty? (remove (-> start :provides keys set) new-reqs))]
      (-> option
          (update :chain conj step)
          (assoc :complete complete?)))))

(defn prepend-steps [{:keys [start chain end] :as option} npc world]
  (let [reqs (reduce find-requirements (-> end :consumes keys set) chain)
        steps (:known-steps npc)
        useful-steps (remove #(->> % :provides keys (filter reqs) empty?) steps)]
    (map (add-step-to option reqs) useful-steps)))

(defn optimise [option npc world]
  ; TODO
  ; iterate over - all individual items, all consecutive pairs/triples/quads/etc... adjusting multipliers
  ;    eg. "drink water" * (min water-available drinking-needed)
  (assoc option :optimised true)
  )

(defn consider-option [npc world]
  (fn [{:keys [complete optimised] :as option}]
    (println "consider-option:fn")
    (cond
      optimised [option]
      complete [(optimise option npc world)]
      :else (prepend-steps option npc world))))

(defn has->provides [npc]
  ; TODO
  ;convert what teh npc :has (recursively) to a map of what they can provide
  {:provides {:at :berry-bushes}})

(defn grow-options [npc world]
  (let [options (get-in npc [:busy :options])
        need (get-in npc [:busy :need])
        options-considered 5]

    (if (empty? options)
      (assoc-in npc [:busy :options] [{:start (has->provides npc)
                                       :chain []
                                       :end   {:consumes (into {} [need])}}])

      ; take first n options
      ;   incomplete?
      ;     prepend steps
      ;   else
      ;     optimise multipliers
      ; score options
      ; sort options

      (let [[to-consider unconsidered] (split-at options-considered options)
            considered (mapcat (consider-option npc world) to-consider)]
        (->> (concat considered unconsidered)
             (map score-option)
             (sort-by :score)
             (assoc-in npc [:busy :options]))))

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

(defn algorithm [npc map]

  (-> npc
      ensure-greatest-need
      (grow-options map)

      )

  (let [busy (:busy npc)]



    )



  (if (:busy npc)
    (be-busy npc)
    (make-busy npc map)))


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

