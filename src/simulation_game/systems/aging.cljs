(ns simulation-game.systems.aging)



(defn dies-after [node limit corpse]
  (update node :system/aging assoc
          :age 0
          :limit limit
          :corpse corpse))

(defn aged [node age]
  (update node :system/aging assoc :age age))

(defn age [node]
  (if-let [{:keys [age limit corpse]} (:system/aging node)]
    (if (< age limit)
      (update-in node [:system/aging :age] inc)
      corpse)
    node))
