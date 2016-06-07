(ns deftish_1)

(require 'clojure.string) ;For (capitalize)
(require 'clojure.set) ;For (join)

;; run the demo in Leiningen:
;; cd project_euler
;; lein repl
;; (load-file "deftish_1.clj")
;; (deftish_1/t4)

;; Output is a set of maps. Note every map has the same keys.

;; user=> (deftish_1/t4)
;; #{{:make volkswagen, :model golf, :disp nil, :color white} {:make "Toyota", :model "corolla", :disp 1.8, :color white} {:make Mercedes-Benz, :model "220b", :disp nil, :color white}}


;; When adding a new row, need to start with an empty row {:key nil} for all keys.
(defn empty-row [table]
  (reduce #(merge %1 {%2 nil}) {} (keys (first table))))

(defn add-row [table add]
  (conj table (merge (empty-row table) add)))

;; Force the key to be a keyword 
(defn add-column [table key value]
  (reduce #(conj %1 (assoc %2 (keyword key) value)) #{} table))

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

(defn where [table constraint rowfun]
  (reduce (fn [mymap row] 
              (if (constraint row)
                  (conj mymap (apply (partial assoc row) (rowfun row)))
                (conj mymap row))) #{} table))

;; How to use a variable for the second arg to assoc? Need to pass [map key val & kvs] so
;; I need to make a variable containing kvs. See (source assoc).


;; works
(defn t5 []
  (let [args [:model "220b" :make 'Mercedes-Benz]] (apply (partial assoc {:make 'mercedes}) args)))

;; works also
(defn t6 []
  (let [args #{:model "220b" :make 'Mercedes-Benz}] (apply (partial assoc {:make 'mercedes}) args)))

;; works also
(defn t7 []
  (let [row {:make 'mercedes} 
       rf (fn [row] [:model "220b" :make 'Mercedes-Benz])] (apply (partial assoc row) (rf row) )))

(defn t8 []
  (-> #{}
      (add-row {:make "toyota" :model "corolla"})
      (where (fn [row] (= "toyota" (get row :make)))
             (fn [row] [:make "Toyota" :disp 1.8]))))

;; This is Deftish clojure, using the thread first macro (->).
;; This demos the (where) function which takes two fn's as args.
;; Also less elegant function (merc-220) which works, but uses knowledge of deftish internals.
(defn t4 []
  (-> #{} (add-row {:make "toyota" :model "corolla"})
      (add-column 'disp 0)
      (add-row {:make "mercedes"})
      (add-row {:make 'volkswagen :model 'golf})
      (add-column 'color 'white)
      (where (fn [row] (= "toyota" (get row :make)))
             (fn [row] [:make "Toyota" :disp 1.8]))
      (merc-220)))

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

;; Yes, a function called (table) that returns a test table.
(defn table []
  #{{:make 'toyota :model 'corolla}
  {:make 'volkswagen :model 'golf}})


;; Assume that the first row has all columns. This is normally a safe assumption.  A set of table keys is not
;; what was needed for add-row. Maybe it will be useful for something else.
(defn table-keys [table]
  (set (vec (keys (first table)))))

(defn t1 []
  (table-keys #{{:make 'toyota :model 'corolla}
              {:make 'volkswagen :model 'golf}}))

(defn t2 [table]
  (merge (empty-row table) {:make "mercedes"}))

(defn model-value-as-list []
  (let [result (conj ()
                     {:make 'toyota :model 'corolla}
                     {:make 'volkswagen :model 'golf}
                     )]
                      (reduce (fn [list row] 
                                  (conj list (get row :make))) () result)))

;; Works, returns new list of map with :make capitalized.
(defn mod-col-map []
  (let [result (conj ()
                     {:make 'toyota :model 'corolla}
                     {:make 'volkswagen :model 'golf}
                     )]
                      (reduce (fn [list row] 
                                  (conj list (assoc row :make (clojure.string/capitalize (get row :make )))))
                              () result)))


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

