(ns calculations.core)


(def data (->> [{:unit :area
                 :deps ["PLANK-1" "PAINT-1"]
                 :id "WALL-S"
                 :width 5000
                 :height 2000}
                {:id "PLANK-1"
                 :unit :lm
                 :width 2000
                 :height 350
                 :waste 1.1
                 :deps ["SPIKE-1" "SCREWS-1"]}
                {:id "PAINT-1"
                 :unit :litre
                 :litre-per-m2 0.45
                 :litres-per-bucket 1.76}
                {:id "SPIKE-1"
                 :unit :package
                 :per-package 50
                 :per-lm 20}
                {:id "SCREWS-1"
                 :unit :package
                 :per-package 50
                 :per-lm 10}]
               (map (fn [{:keys [id] :as v}] [id v]))
               (into {})))

(defn get-parent-child [data parent-id child-id]
  (map #(get data %) [parent-id child-id]))

(defn get-m2 [{:keys [width height]}]
  (/ (* width height) 1000000.0))

(defn calculate-lm [area-width area-height lm-width lm-height]
  (let [amount-width (Math/ceil (/ area-width lm-width))
        amount-height (Math/ceil (/ area-height lm-height))
        total-amount (* amount-width amount-height)
        total-lm (/ (* total-amount lm-width) 100)]
    total-lm))

(defn calculate-buckets
  "total: total amount to calculate against
  per-unit: how much can each unit take?"
  [total per-unit]
  (Math/ceil (/ total per-unit)))

(defn dig-deeper [data parent-id children-ids]
  (reduce (fn [out child-id]
            (assoc out child-id (calc data parent-id child-id)))
          {} children-ids))


(def calc nil)
(defmulti calc (fn [data parent-id child-id] (get-in data [child-id :unit])))

(defmethod calc :area [data parent-id child-id]
  (let [{:keys [deps unit]} (get data child-id)]
    {:id child-id
     :unit unit
     :children (dig-deeper data child-id deps)}))

(defmethod calc :lm [data parent-id child-id]
  (let [[parent child] (get-parent-child data  parent-id child-id)
        {area-width :width area-height :height} parent
        {lm-width :width lm-height :height deps :deps unit :unit} child
        lm (calculate-lm area-width area-height lm-width lm-height)
        data (update-in data [child-id] merge {:lm lm})]
    {:id child-id
     :amount lm
     :unit unit
     :children (dig-deeper data child-id deps)}))

(defmethod calc :litre [data parent-id child-id]
  (let [[parent child] (get-parent-child data parent-id child-id)
        {:keys [litre-per-m2 litres-per-bucket deps unit]} child
        m2 (get-m2 parent)
        nr-buckets (calculate-buckets (* m2 litre-per-m2) litres-per-bucket)]
    {:id child-id
     :unit unit
     :amount (* m2 litre-per-m2)
     :buckets nr-buckets
     :litres-per-bucket litres-per-bucket
     :children (dig-deeper data child-id deps)}))

(defmethod calc :package [data parent-id child-id]
  (let [[parent child] (get-parent-child data parent-id child-id)
        {:keys [lm]} parent
        {:keys [deps per-lm unit]} child
        amount (Math/ceil (/ lm per-lm))]
    {:id child-id
     :unit unit
     :amount amount
     :children (dig-deeper data child-id deps)}))

(do
  (println "---")
  (clojure.pprint/pprint (calc data nil "WALL-S")))
