(ns simulation-game.systems.diminishing)


(def diminishes?
  #{:flames})

(defn diminish [node]
  (if (diminishes? (:id node))
    (let [new-quantty (dec (:quantity node 1))]
      (if (pos? new-quantty)
        (assoc node :quantity new-quantty)
        nil))
    node))
