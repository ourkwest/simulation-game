(ns the-regenetron.systems.production)


(defn produces-every [node target prototype]
  (assoc node :system/production {:growth    0
                                  :target    target
                                  :prototype prototype}))

(defn produce [node]
  (if-let [{:keys [growth target prototype]} (:system/production node)]
    (if (> growth target)
      (-> node
          (assoc-in [:system/production :growth] 0)
          (update :has conj prototype))
      (update-in node [:system/production :growth] inc))
    node))
