(ns the-regenetron.systems.aging)



(defn dies-after [node limit corpse]
  (assoc node :system/aging {:age    0
                             :limit  limit
                             :corpse corpse}))

(defn age [node]
  (if-let [{:keys [age limit corpse]} (:system/aging node)]
    (if (< age limit)
      (update-in node [:system/aging :age] inc)
      corpse)
    node))
