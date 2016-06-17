(ns deftish_1
    (:require [clojure.string :as string]
              [clojure.set :as set]))

;; todo:

;; - create an aggregation function, adapting exising clojure, and figure out where aggregation values
;;   go. (They go into all rows?)

;; - add some tests

;; The Def table is a set of maps. Each map has the same keys. Rows of the table are the maps, and columns are
;; the keys. Note that every map has the same keys, which is critical since the keys are columns, or if you
;; prefer, the keys symbolically refer to multi-valued variables.

;; run the demo in Leiningen:
;; cd project_euler
;; lein repl
;; (load-file "deftish_1.clj")
;; (deftish_1/demo-deft)

;; user=> (deftish_1/demo-deft)
;; #{{:make volkswagen, :model golf, :disp nil, :color white} {:make "Toyota", :model "corolla", :disp 1.8, :color white} {:make Mercedes-Benz, :model "220b", :disp nil, :color white}}


;; When adding a new row, need to start with an empty row {:key nil} for all keys.
(defn empty-row [table]
  (reduce #(merge %1 {%2 nil}) {} (keys (first table))))

(defn add-row [table add]
  (conj table (merge (empty-row table) add)))

;; Force the key to be a keyword, in case someone passes the wrong type. Or is that necessary? Test this.
(defn add-column [table key value]
  (map #(assoc %1 (keyword key) value) table))

;; Not much point in a function for this. At best a macro.
;; (defn update-column [row key value]
;;   (assoc row :model "220b"))
  
;; Update a row that meets a specific constraint. This works, but definitely needs to be generalized.
(defn merc-220 [table]
  (reduce (fn [mymap row] 
              (if (= "mercedes" (get row :make))
                  (conj mymap (assoc row :model "220b" :make 'Mercedes-Benz))
                (conj mymap row)))
              #{} table))

;; Yes, a function called (demo-table) that returns a test table.
(defn demo-table []
  #{{:make "toyota" :model "corolla"}
  {:make "volkswagen" :model "golf"}})

;; (map) wants to return a list, so we have to (set) that into a set.  Also shows how to send a var with a
;; list of args to (assoc) via (apply (partial )) as opposed to passing hard coded arguments.
(defn where
  "Like a where clause run against each row of table, if the constraint is true do the rowfun on this row."
  [table constraint rowfun]
  (set (map #(if (constraint %)
                 (apply (partial assoc %) (rowfun %))
               %) table)))

;; Deft pretty print the table into a list of strings.

;; Convert each column rkey of the table into a formatted string, and return a list of the strings. This
;; "prints" one row into a list. Use reduce since we want a string which is an aggregate of the row.
(defn prow [mrow rkeys]
  (reduce #(str (format "%12s" (%2 mrow)) %1) "" rkeys))

;; Create a list where each item is a string for output. First item is the head line where column names are
;; key names.  Print the results: (doseq [curr (dpp)] (print curr))
(defn dpp []
  (let [table (demo-table)
       rkeys (vec (sort-by comp (keys (first table))))]
       (do
           (cons
            (println-str (reduce #(str (format "%12s" (name %2)) %1) "" rkeys))
            (map (fn [mrow] (println-str (prow mrow rkeys))) table)))))

;; Create a generalized aggregate function so we can do things like sum, max, etc.
;; Probably need a column arg where the result will be stored.
;; Might be able to call add-column to store the result.

;; Single purpose agg, saves count of sum of string length of :make in column :sum-make.
(defn agg [table]
  (add-column table 'sum-make (reduce #(+ %1 (count (str(:make %2)))) 0 table)))


;; This is Deftish clojure, using the thread first macro (->).
;; This demos the (where) function which takes two fn's as args.
;; Also less elegant function (merc-220) which works, but uses knowledge of deftish internals.
(defn demo-deft []
  (-> #{} (add-row {:make "toyota" :model "corolla"})
      (add-column 'disp 0)
      (add-row {:make "mercedes"})
      (add-row {:make "volkswagen" :model "golf"})
      (add-column 'color 'white)
      (where (fn [row] (= "toyota" (:make row)))
             (fn [row] [:make "Toyota" :disp 1.8]))
      (where (fn [row] (and (= "golf" (:model row)) (= "volkswagen" (:make row))))
             (fn [row] [:disp 2.2]))
      (merc-220)
      (agg)))

;; Everything below is historical, or notes and will be removed soo.
  
;; Nest function calls to build and modify a table. -> thread-first macro makes this way more legible.
(defn -main []
  (let [table #{}]
       (merc-220
       (add-column
        (add-row 
         (add-row 
          (add-column 
           (add-row table {:make "toyota" :model "corolla"})
           'disp 0)
          {:make "mercedes"})
         {:make 'volkswagen :model 'golf})
        'color 'white))))


;; Assume that the first row has all columns. This is normally a safe assumption.  A set of table keys is not
;; what was needed for add-row. Maybe it will be useful for something else.
(defn table-keys [table]
  (set (vec (keys (first table)))))

(defn t1 []
  (table-keys #{{:make 'toyota :model 'corolla}
              {:make 'volkswagen :model 'golf}}))

(defn t2 [table]
  (merge (empty-row table) {:make "mercedes"}))

;; reduce will use 2 args, and if supplied with 1 arg it will eat the first 2 components of arg 1
;; Supply reduce with 2 args: set set
;; This fails if arg 1 is a map, no surprise since we aren't supplying kv pairs.
;; Works if arg 1 is [] () or #{}
(defn add-column-demo-2 []
  (reduce #(conj %1 (assoc %2 :disp 0)) 
          #{} 
          #{{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}}))


(defn update-column-demo []
  (reduce #(conj %1
               (update-in %2 [:make] clojure.string/capitalize))
               #{}
               #{{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}}))


;; Works, sort of. Adds new column :disp for existing :make, but fails to add new row :make and the failure is
;; silent. Need to look at the source for (join) and figure out how to get it to detect and/or report
;; non-joinable data. Or need some way to stop users from asking for things they shouldn't ask for.
(defn add-column-demo-1 []
  (let [result (conj ()
                     {:make 'toyota :model 'corolla}
                     {:make 'volkswagen :model 'golf}
                     )
       new (conj ()
                 {:make 'volkswagen :disp 1.8}
                 {:make 'toyota :disp 1.0}
                 {:make 'mercedes :disp 2.2}
                 )]
                   (clojure.set/join result new)))

;; Works. Select a single column of all rows, returns a set.
;; Using (conj () ...) this returns a list instead of a set.
(defn single-col-map []
  (let [result #{
       {:make 'toyota :model 'corolla}
       {:make 'volkswagen :model 'golf}
       }]
       (reduce (fn [set row] 
                   (conj set (select-keys row [:make] ))) #{} result)))

