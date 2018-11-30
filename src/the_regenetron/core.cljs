(ns the-regenetron.core
  (:require
    [reagent.core :as reagent :refer [atom]]
    [cljs.pprint :as pp]
    [clojure.string :as string]
    [the-regenetron.systems.production :as production]
    [the-regenetron.systems.aging :as aging]))

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

;  ; characters could have 'known-skills' - transferred by socialising, improved by practice (reduced cost)

; this could be helped by 'habits' - actions that often go well together - eg. go to well, draw water, drink water

(defn has-map [f tree]
  (f (update tree :has #(->> %
                             (map (partial has-map f))
                             (remove nil?)
                             vec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;;    Static data    ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn new-item ;TODO: is quantity meaningful here, or is each item just 1 item in the object tree?
  ([id] (new-item id 1))
  ([id quantity]
    {:id id :quantity quantity}))

(def pile-of-sticks (-> (new-item :sticks)
                        (aging/dies-after 200 nil)))

(def berry (-> (new-item :berry)
               (aging/dies-after 50 nil)))
(def berry-bush (-> (new-item :berry-bush)
                    (production/produces-every 5 berry)
                    (aging/dies-after 400 pile-of-sticks)))
(def scrubland (-> (new-item :scrubland)
                   (production/produces-every 20 berry-bush)))

(def well (-> (new-item :well)))

;TODO: is every npc going to have it's own map as well as the world having territory?
(def places {:beach {:label "the beach"
                     :id  :beach
                     :has [{:id  :palm-tree
                            :has [berry berry berry berry berry]}]}
             :village-0 {:label  "the centre of the village"
                         :id :village-0
                         :has [well]}
             :village-1 {:label  "the South side of the village"
                         :id :village-1}
             :village-2 {:label  "the North side of the village"
                         :id :village-2}
             :village-3 {:label  "the East side of the village"
                         :id :village-3}
             :village-4 {:label  "the West side of the village"
                         :id :village-4}
             :north-borders {:label  "the North Borders"
                             :id  :north-borders
                             :has [berry
                                   berry
                                   berry
                                   berry-bush
                                   scrubland

                                   ;{:id  :berry-bush
                                   ; :system/production {:growth 0
                                   ;                     :target 3
                                   ;                     :prototype {:id :berry}}
                                   ; :has [{:id :berry}
                                   ;       {:id :berry}
                                   ;       {:id :berry}]}
                                   ]}})

(def raw-links
  [[:beach     "the beach"                     14 18 "the village"         :village-1]
   [:village-1 "the South side of the village"  5  5 "the village centre"  :village-0]
   [:village-2 "the North side of the village"  5  5 "the village centre"  :village-0]
   [:village-3 "the East side of the village"   5  5 "the village centre"  :village-0]
   [:village-4 "the West side of the village"   5  5 "the village centre"  :village-0]
   [:village-2 "the village"                   12 17 "the North Borders"   :north-borders]
   ])

(def bi-links (set (concat raw-links (map reverse raw-links))))

(defn all-paths-from [from bi-links to]
  (for [[link-from _ _ ticks description link-to :as this-link] bi-links :when (= link-from from)
        child-path (if (= link-to to)
                     [nil]
                     (all-paths-from link-to (disj bi-links this-link) to))]
    (cons {:ticks       ticks
           :description description
           :from        link-from
           :to          link-to} child-path)))

(defn find-best-route [[from to]]
  (->> (all-paths-from from bi-links to)
       (map (fn [path]
              {:path  path
               :ticks (transduce (map :ticks) + 0 path)}))
       (apply min-key :ticks)))

(def routes
  (into {} (for [from (keys places)
                 to (keys places)
                 :when (not= from to)]
             [[from to] (find-best-route [from to])])))

(def abstract
  #{:food
    :drink
    :sleep
    :chat
    :warmth
    :security})

(def steps
  [{:name     "drink water"
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
   ])

(defn merge-key [m k f a b]
  (let [v (f (k a) (k b))]
    (if v
      (assoc m k v)
      m)))

(defn nil+ [a b]
  (if (and a b)
    (+ a b)
    (or a b)))

(def inc-nil (fnil inc 0))

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
  (let [children (:has thing)
        children-maps (for [child children]
                       {[npc-loc (:id child)] (dissoc child :id)})]
    (apply merge-add
      (concat children-maps
              (when (seq children)
                (map (partial has->provides npc-loc) children))))))

(def npc-needs [[:npc :food]
                [:npc :drink]
                [:npc :sleep]
                [:npc :chat]
                [:npc :warmth]
                [:npc :security]])

(def default-npc-reqs
  (zipmap npc-needs (repeat {:quantity 0})))

(def names
  [
   ;"Ada"
   "Hannah" "Eve" "Avalanche" "Lambda"

   "Albert" "Eliezer" "Sigmund" "Carl" "René"
   ])

(def game-state
  (atom
    {:story     [{:html [:h1 "Welcome to The Regenetron (Working Title)"]}
                 {:html [:p "A paragraph of epic introductory text..."]}
                 {:html [:p "~"]}
                 {:html [:p "I awake to find myself lying on the beach in the glorious morning sun."]}]
     :locations places
     :people    {:ada {:name        "Ada"
                       :reqs        (merge
                                      default-npc-reqs
                                      {[:npc :food]  {:quantity 0} ; TODO: should these be negative?
                                       [:npc :drink] {:quantity 50}
                                       [:npc :sleep] {:quantity 10}
                                       [:npc :chat]  {:quantity 0} ; capped at 75? some way to differentiate between needs and wants?
                                       })
                       :has         [{:id       :water
                                      :quantity 3}            ; liquid hold limit is low, but can carry eg. a bucket that can hold more
                                     ;{:id       :berry
                                     ; :quantity 5}            ; fifo queue when picking things up - hold limit?
                                     {:id  :bucket
                                      :has [{:id       :water
                                             :quantity 5}]}]
                       :known-steps steps
                       :location    :beach
                       :busy        nil}}}))

(let [seed (volatile! 0)]
  (defn pseudo-rand []
    (vswap! seed inc)
    (let [x (* (Math/sin @seed) 10000)]
      (- x (Math/floor x)))))

(defn pseudo-rand-int [n]
  (Math/floor (* (pseudo-rand) n)))

(defn pseudo-rand-nth [coll]
  (nth coll (pseudo-rand-int (count coll))))

(defn add-people [state]
  (let [more-people (into {} (for [name names]
                               [(keyword (string/lower-case name))
                                {:name        name
                                 :reqs        (merge default-npc-reqs
                                                     {[:npc :food] {:quantity 50}})
                                 :has         []
                                 :known-steps steps
                                 :location    (pseudo-rand-nth (keys places))
                                 :busy nil}]))]
    (update state :people merge more-people)))

(defn mapvals [m f]
  (into {} (for [[k v] m]
             [k (f v)])))

;TODO: when should this happen?
(swap! game-state update :locations mapvals
       (fn [loc]
         (assoc loc :provides (has->provides :loc loc))))
(swap! game-state add-people)

(defn palette [input]
  (let [[r g b] (cycle (re-seq #"\d{3}" (str (hash input))))
        r (mod (js/parseInt r) 128)
        g (mod (js/parseInt g) 128)
        b (mod (js/parseInt b) 128)]
    {:light (str "rgb(" (+ r 128) \, (+ g 128) \, (+ b 128) ")")
     :dark  (str "rgb(" r \, g \, b ")")}))

(defn has-palette [node]
  (let [prototype-palette (fn [node]
                            (if (:system/production node)
                              (update-in node [:system/production :prototype] has-palette)
                              node))]
    (-> node
        (assoc :palette (palette (or (:name node) (:id node) "")))
        prototype-palette
        (update :has #(mapv has-palette %)))))

(swap! game-state update :people mapvals has-palette)

(swap! game-state update :locations mapvals has-palette)

(defn fixed-steps-providing [npc item-key]
  (filter
    (fn [{:keys [provides]}]
      (get provides item-key))
    (:known-steps npc)))

(defn synthetic-steps-providing [loc-key locs [npc-loc item-id]]
  (if (= npc-loc :npc)
    (when-not (abstract item-id)
      [{:name     (str "take " (name item-id))
        :ticks    1
        :provides {[:npc item-id] {:quantity 1}}
        :consumes {[:loc item-id] {:quantity 1}}}])
    (for [[loc-id loc] locs :when (-> loc :provides (get [:loc item-id]))]
      (let [route (routes [loc-key loc-id])]
        {:name       (str "go to " (-> places (:id loc) :label))
         :ticks      (:ticks route)
         :provides   (:provides loc)
         :idempotent true
         :sub-steps  (map (fn [{:keys [ticks description from to]}]
                            {:name       (str "go to " description)
                             :ticks      ticks
                             :idempotent true
                             :from       from
                             :to         to})
                          (:path route))})))) ;OLD TODO: needs to be {[:loc :id] quantity} !!!


(defn steps-providing [npc locs item-key]
  (concat
    (fixed-steps-providing npc item-key)
    (synthetic-steps-providing (:location npc) locs item-key)))

(defn scale-quantities [has scale-factor]
  (mapvals has (fn [value]
                 (update value :quantity #(* (or % 1) scale-factor)))))

(defn apply-chain [reqs step]
  (let [multiplier (or (:quantity step) (:ideal-n step))]
    (merge-subtract reqs (scale-quantities (:provides step) multiplier))))

(defn clip-0 [x]
  (if (pos? x) x 0))

(defn score-reqs [reqs]
  (apply - 0 (map (comp clip-0 :quantity reqs) npc-needs)))

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
                            :thought (when (:optimised option) (:thinking (:busy npc)))}))))

(defn get-maximum-desired
  "What's the most it's worth doing this step?"
  [step reqs]
  (if (:idempotent step)
    1
    (Math/ceil
      (apply max
             (for [[k provided-value] (:provides step)
                   :let [required-value (reqs k)]
                   :when required-value]
               (/ (:quantity required-value)
                  (:quantity provided-value)))))))

(defn add-step-to [{:keys [start] :as option} reqs]
  (fn [step]
    (let [maximum-desired (get-maximum-desired step reqs)
          new-reqs (-> reqs
                       (merge-add (:requires step) (scale-quantities (:consumes step) maximum-desired))
                       (merge-remove (scale-quantities (:provides step) maximum-desired)))
          complete? (empty? (remove (-> start :provides keys set) (keys new-reqs)))]
      (-> option
          (update :chain conj (assoc step :complete complete?
                                          :reqs new-reqs
                                          :ideal-n maximum-desired))
          (assoc :complete complete?)))))

(defn prepend-steps [{:keys [chain end] :as option} npc locs]
  (let [reqs (or (some-> chain first :reqs) (:reqs end))
        useful-steps (distinct (mapcat (partial steps-providing npc locs) (keys reqs)))]
    (mapv (add-step-to option reqs) useful-steps)))

(defn get-maximum-available
  "What's the most we can do this step?"
  [step has]
  ; for every thing required, what is the maximum number of times we could perform the step - min of those
  (if (:idempotent step)
    1
    (Math/floor
      (apply min
             (for [[k required-value] (merge (:consumes step)
                                             (:requires step))
                   :let [provided-value (has k {:quantity 0})]]
               (/ (:quantity provided-value)
                  (:quantity required-value)))))))

(defn multiply-quantities [parent-quantity {:keys [quantity ticks consumes provides sub-steps]
                                            :or {quantity 1}
                                            :as step}]
  (let [quantity (* parent-quantity quantity)]
    (merge step
      (when ticks {:ticks (* quantity ticks)})
      (when consumes {:consumes (mapvals consumes #(update % :quantity * quantity))})
      (when provides {:provides (mapvals provides #(update % :quantity * quantity))})
      (when sub-steps {:sub-steps (map (partial multiply-quantities quantity) sub-steps)}))))

(defn optimise [option npc world]

  ;TODO: does not work on travelling!!! - can we make it work recursively on sub-steps, but still handle travelling sensibly?
  ; eurgh! good idea but hard - carry limits?
  ;              "go and fetch wood"
  ;              how do we multiply that? go multiple times?
  ;                     repeat x 10 (up to lower of amount needed and amount available)
  ;                       go x1 (can only go once at a time)
  ;                          pick up x 5 (up to carry limit)
  ;                       return x1
  ;                          drop x 5 (up to amount being held)

  ; some tasks are scalable - cutting down a tree, do it more times, there is more wood and fewer trees
  ; some tasks aren't - go to the woods and there are 3 trees, go there twice and there are still only 3 !!!

  ; OLD:
  ; iterate over - all individual items, all consecutive pairs/triples/quads/etc... adjusting multipliers
  ;    eg. "drink water" * (min water-available drinking-needed)
  ; OR
  ; double pass
  ;   wants 100 food -> wants 100 berries -> wants to take 100 berries -> there are 5 berries
  ;   filled up by 5 <-  eat berry x 5   <-     take berry x 5         <- there are 5 berries

  (loop [has (-> option :start :provides)
         optimised (assoc option :chain []
                                 :optimised true)
         [step & chain] (-> option :chain)]
    (if step
      (let [maximum-available (get-maximum-available step has)
            maximum-desired (:ideal-n step)
            n (min maximum-available maximum-desired)
            new-has (-> has
                      (merge-remove (scale-quantities (:consumes step) n))
                      (merge-add (scale-quantities (:provides step) n)))
            optimised-step (multiply-quantities 1 (assoc step :max-n maximum-available :quantity n))]
        (recur new-has
               (update optimised :chain conj optimised-step)
               chain))
      optimised)))

(defn consider-option [npc locs]
  (fn [{:keys [complete optimised] :as option}]
    (cond
      optimised [option]
      complete [(optimise option npc locs)]
      :else (prepend-steps option npc locs))))

(defn grow-options [[npc locs]]
  (let [options (get-in npc [:busy :options])
        loc-provides (-> npc :location locs :provides)
        options-considered 5]
    (if (empty? options)
      (-> npc
          (assoc-in [:busy :options] [{:start {:provides (merge-add (has->provides :npc npc)
                                                                    loc-provides)}
                                               :chain '()
                                               :end   {:reqs (get-in npc [:busy :reqs])}}])
          (update-in [:busy :thinking] inc)
          (update :busy dissoc :reqs))
      (let [[to-consider unconsidered] (split-at options-considered options)
            considered (mapcat (consider-option npc locs) to-consider)
            _ (doall considered)]
        (-> npc
            (assoc-in [:busy :options] (->> (concat considered unconsidered)
                                            (mapv (partial score-option npc))
                                            (sort-by (comp - :value :score))))
            (update-in [:busy :thinking] inc))))))

(defn refresh-provides [node npc-loc]
  (assoc node :provides (has->provides npc-loc node)))

(defn remove-hasnts [node]
  (if-let [has (:has node)]
    (assoc node :has (mapv remove-hasnts (remove nil? has)))
    node))

(defn find-quantities
  "list all paths to quantities of the item-id"
  [x item-id]
  (cond
    (= (:id x) item-id) [[(or (:quantity x) 1)]]
    (:has x) (mapcat (fn [item idx]
                       (map #(cons idx %) (find-quantities item item-id))) (:has x) (range))
    :else []))

(def not-enough-stuff (js/Error. "Not enough stuff!"))

(defn consume [node item-id quantity]
  (let [paths (find-quantities node item-id)]
    (loop [x node
           quantity-to-consume quantity
           [path & paths] paths]
      (if (pos? quantity-to-consume)
        (if path
          (let [quantity-available (last path)
                remainder (- quantity-available quantity-to-consume)
                x-path (interleave (repeat :has) (butlast path))]
            (if (pos? remainder)
              (assoc-in x (concat x-path [:quantity]) remainder)
              (recur
                (assoc-in x x-path nil)
                (- remainder)
                paths)))
          (throw not-enough-stuff))
        x))))

(defn step-consume [[npc locs] [npc-loc item] {:keys [quantity]}]
  (if (= npc-loc :npc)
    [(remove-hasnts (consume npc item quantity)) locs]
    [npc (update locs (:location npc) #(remove-hasnts (consume % item quantity)))]))

(defn begin-step [[npc locs]]
  (try
    (let [[step & todo] (:todo (:busy npc))
          consumes (:consumes step)
          [npc locs] (reduce-kv step-consume [npc locs] consumes)]
      [(-> npc
           (update :busy assoc :doing step)
           (update :busy assoc :todo todo))
       (update locs (:location npc) #(refresh-provides % :loc))])
    (catch js/Error e
      (if (= e not-enough-stuff)
        (println (:name npc) "didn't have enough stuff to" (-> npc :busy :todo first :name))
        (throw e))
      [(assoc npc :busy nil) locs])))

(defn tick-step [[npc locs]]
  [(update-in npc [:busy :doing :ticks] dec) locs])

(defn provide [node item-id v]
  (let [quantity-required (:quantity (get (:reqs node) [:npc item-id]))]
    (if quantity-required
      (update-in node [:reqs [:npc item-id] :quantity] - (:quantity v))
      (update node :has conj (assoc v :id item-id)))))

(defn step-provide [[npc locs] [npc-loc item] v]
  (if (= npc-loc :npc)
    [(provide npc item v) locs]
    [npc (update locs (:location npc) #(provide % item v))]))

(defn end-step [[npc locs]]
  (let [step (:doing (:busy npc))
        provides (:provides step)
        new-location (:to step)
        [npc locs] (reduce-kv step-provide [npc locs] provides)]
    [(-> npc
         (update :busy dissoc :doing)
         (update :location #(or new-location %)))
     (update locs (:location npc) #(refresh-provides % :loc))]))

(defn steps-completed [[npc locs]]
  [(update npc dissoc :busy) locs])

(defn make-progress [[npc locs]]
  (let [{:keys [todo doing]} (:busy npc)]
    (cond
      (and todo (not doing)) (begin-step [npc locs])
      (pos? (:ticks doing)) (tick-step [npc locs])
      (and doing (zero? (:ticks doing))) (end-step [npc locs])
      (not todo) (steps-completed [npc locs]))))

(defn flatten-chain [chain]
  (mapcat #(or (:sub-steps %) [%]) chain))

(defn describe-decision [npc]
  (println
    (:name npc)
    "decided to"
    (string/join ", " (for [[desc quantity] (-> npc
                                                :busy
                                                :todo
                                                (->> (map (juxt :name :quantity))))]
                        (if quantity
                          (str desc "(x" quantity ")")
                          desc))))
  npc)

(defn try-to-choose-an-option [npc]
  (let [thinking (-> npc :busy :thinking)
        best-option-so-far (-> npc :busy :options first)
        thought (-> best-option-so-far :score :thought)]
    (cond
      (and (> thinking (+ thought 3))
           (:optimised best-option-so-far))
      (-> npc
          (assoc-in [:busy :todo] (flatten-chain (:chain best-option-so-far)))
          (update :busy dissoc :options :thinking)
          describe-decision)

      (> thinking (+ thought 10))
      (do
        (println (:name npc) "is stuck!!!")
        (assoc npc :stuck true))

      :else
      npc)))

(defn choose-greatest-need [npc]
  (assoc-in npc [:busy :reqs] (->> npc
                                   :reqs
                                   (sort-by (comp :quantity second))
                                   (take-last 1)
                                   (into {}))))

(defn exhaust-reqs [reqs]
  (-> reqs
      (update-in [[:npc :food] :quantity] #(+ % 0.02))
      (update-in [[:npc :drink] :quantity] #(+ % 0.05))
      (update-in [[:npc :sleep] :quantity] #(+ % 0.01))
      (update-in [[:npc :chat] :quantity] #(+ % 0.005)) ; variable by character?
      ))

(defn exhaust-character [character]
  (update character :reqs exhaust-reqs))

(defn algorithm [[npc locs :as npc-locs]]
  (let [[npc' locs'] (cond
                       (or (-> npc :busy :doing)
                           (-> npc :busy :todo)) (make-progress npc-locs)
                       (-> npc :busy :reqs) [(try-to-choose-an-option (grow-options npc-locs)) locs]
                       :else [(choose-greatest-need npc) locs])]
    [(exhaust-character npc') locs']))

(defn tick-npcs [{:keys [people] :as game-state}]
  ; TODO: give each NPC a map in their head - then they can think without looking at the world?
  ; but they still need to mutate the world sometimes!!! - they should only ever mutate their own location though!
  (reduce (fn [{:keys [people locations] :as game-state} person-key]
            (let [person (person-key people)
                  [person' locations'] (algorithm [person locations])]
              (-> game-state
                  (assoc-in [:people person-key] person')
                  (assoc :locations locations'))))
          game-state
          (keys people)))

(defn run-systems [location]
  (has-map (fn [node]
             (some->
               node
               production/produce
               aging/age
               (refresh-provides :loc)))
           location))

(defn tick-locs [game-state]
  (update game-state :locations mapvals run-systems))

(defn inc-ticks [game-state]
  (update game-state :ticks inc-nil))

(def tick-game-state (comp
                       inc-ticks
                       tick-locs
                       tick-npcs
                       ))

(defn tick []
  (swap! game-state tick-game-state))

(defn autotick []
  (tick)
  (when (:auto @game-state)
    (js/setTimeout autotick 500)))

(defn render-story [{:keys [story]}]
  (into [:div] (map :html story)))

(defn render-controls [{:keys [pc ticks]}]
  [:div
   [:div {:style {:display "inline-block"}}
    ;[:span "Health: " [:b "100% "]]
    ;[:br]
    ;[:span "Hunger: " [:b "10% "]]
    ;[:br]
    ;[:span "Tiredness: " [:b "10% "]]
    ;[:br]
    ;[:span "Location: " [:b (-> pc :location name)] " "]
    ]

   [:div {:style {:display "inline-block"
                  :margin "10px"}}
    [:span
     ;"I "
     ;(into [:select]
     ;      (->> pc :actions (mapv (fn [{:keys [id label]}]
     ;                               [:option {:value id} label]))))
     [:input {:type "button"
              :value "Tick!"
              :on-click tick}]
     [:input {:type     "button"
              :value    "Tick! (x5)"
              :on-click #(do (tick)
                             (tick)
                             (tick)
                             (tick)
                             (tick))}]
     [:input {:type "button"
              :value "Tick continually..."
              :on-click #(do
                           (swap! game-state update :auto not)
                           (autotick))}]
     [:span "ticks: " ticks]]]])

(defn render-has [node]
  (into
    [:div
     {:style (merge {:border           (str "2px solid " (-> node :palette :dark))
                     :border-radius    "7px"
                     :background-color (-> node :palette :light)
                     :color            (-> node :palette :dark)
                     :display          "inline-block"
                     :margin           "3px"}
                    (when (#{:food :drink :sleep} (:id node))
                      {:color      :black
                       :background (let [pct (if-let [p (:quantity node)]
                                               (str p)
                                               "100")]
                                     (str "linear-gradient(90deg, "
                                          (-> node :palette :dark) " " pct "%, "
                                          (-> node :palette :light) " " pct "%)"))}))}
     [:div
      {:style {:display "inline-block"
               :padding "3px"}}
      (or (:name node) (:id node))
      (when-let [l (-> node :location places :label)]
        (str " at " l))
      (when-let [q (:quantity node)]
        (str " x " (Math/floor q)))
      (when-let [t (:ticks node)]
        (str " (" t ")"))]]
    (map render-has (:has node))))

(defn render-has-overview [node]
  (into
    [:div
     {:style {:border           (str "2px solid " (-> node :palette :dark))
              :border-radius    "7px"
              :background-color (-> node :palette :light)
              :color            (-> node :palette :dark)
              :display          "inline-block"
              :margin          "3px"}}
     [:div
      {:style {:display "inline-block"
               :padding "3px"}}
      (or (:name node) (:id node))
      (when-let [q (:quantity node)]
        (str " x " (Math/floor q)))]]
    (map render-has (:has node))))

(defn reqs->has [node]
  (update node :has conj (has-palette {:id      :needs
                                       :has     (for [[[_ k] {:keys [quantity]}] (:reqs node)]
                                                  {:id       k
                                                   :quantity quantity})})))

(defn thinking->has [node]
  (if-let [t (-> node :busy :thinking)]
    (update node :has conj (has-palette {:id       :thinking
                                         :quantity t}))
    node))

(defn doing->has [node]
  (let [t (-> node :busy :todo)
        d (-> node :busy :doing)
        actions (remove nil? (cons d t))]
    (if (seq actions)
      (update node :has conj (has-palette {:id  :doing
                                           :has (for [action actions]
                                                  {:id       (:name action)
                                                   :quantity (:quantity action)
                                                   :ticks    (:ticks action)})}))
      node)))

(defn doing->has-overview [node]
  (if-let [doing (-> node :busy :doing)]
    (update node :has conj (has-palette {:id       (:name doing)
                                         :quantity (:quantity doing)}))
    node))

(defn stuck->has [node]
  (if (:stuck node)
    (update node :has conj {:id :STUCK!!!
                            :palette {:dark "rgb(255,255,0)"
                                      :light "rgb(150,0,0)"}})
    node))

(defn render-npcs [state]
  (for [[_ npc] (:people state)]
    [:div {:key (str (random-uuid))}
     (-> npc
         reqs->has
         thinking->has
         doing->has
         stuck->has
         render-has)]))

(defn render-locs [state]
  (for [[_ loc] (:locations state)]
    [:div {:key (str (random-uuid))}
     (render-has loc)]))

(defn render []
  (let [state @game-state]
    [:div
     ;(render-story state)
     (render-controls state)
     (render-npcs state)
     (render-locs state)]))


(defn render-loc-overviews [state]
  (for [[loc-key loc] (:locations state)]
    [:div {:key (str (random-uuid))}
     [:div
      [:br]
      [:div (:label loc)]
      [:div {:style {:border           (str "2px solid rgb(0,0,50)")
                     :border-radius    "7px"
                     :background-color "rgb(220,220,255)"
                     :color            "rgb(0,0,50)"
                     :display          "inline-block"
                     :margin           "3px"}}
       (for [thing (:has loc)]
         [:div {:key (str (random-uuid))}
          (render-has-overview thing)])]

      [:div {:style {:border           (str "2px solid rgb(50,40,0)")
                     :border-radius    "7px"
                     :background-color "rgb(255,240,200)"
                     :color            "rgb(50,40,0)"
                     :display          "inline-block"
                     :margin           "3px"}}
       (for [npc (filter #(= loc-key (:location %)) (vals (:people state)))]
         [:div {:key (str (random-uuid))}
          (-> npc
              thinking->has
              doing->has-overview
              stuck->has
              render-has-overview)])]]]))

(defn render-overview []
  (let [state @game-state]
    [:div
     (render-controls state)
     (render-loc-overviews state)]))

(reagent/render-component [render] (. js/document (getElementById "app")))

(defn on-js-reload [])

;TODO: bug when :has (water x0)