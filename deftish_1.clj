(ns deftish_1
    (:require [clojure.string :as string]
              [clojure.set :as set]))

;; todo:

;; - Implement Deft dcc (declare control column)

;; - Seems like a good idea to have a where-true-for-all-rows since we have used that at least twice.

;; - x Add a column arg to rowid, sort entire table based on come criteria, then call rowid with new column
;;     arg to add a column that is numerically sorted based on the table row sort order. This assumes
;;     operation order and (essentially) a single transactions for the sort and rowid.

;; - x Now that we have a working col-max that takes a table and returns a map of column keys and max column
;;     string length, add this to (dpp) in the (map) that creates the per-column format string.

;; - add some tests

;; - Improve (dpp) Add a list of cols to print, cols with max length of each col's values, use a function like
;;   one of with-accum functions to dynamically cal the col widths.  Also, might be nice to calculate all col
;;   widths in a single pass. That would be like a Deft aggration code block.

;; The Def table is a set of maps. Each map has the same keys. Rows of the table are the maps, and columns are
;; the keys. Note that every map has the same keys, which is critical since the keys are columns, or if you
;; prefer, the keys symbolically refer to multi-valued variables.

;; run the demo in Leiningen:
;; cd project_euler
;; lein repl
;; (load-file "deftish_1.clj")
;; (deftish_1/demo-deft)


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


;; Aggregation: retains states between rows. Return max string length of values in column col
;;
;; This works fine for a single column. How about doing max len of several data columns each assigned to a
;; resulting column?

(defn agg-maxlen 
  "Get the max length of data in column col. Return the scalar value. Initialize with the column length of the
key which might be longer than any of the data values."
  [table col]
  (let [key (keyword col)]
       (reduce 
        #(let [curr_len (count (str(key %2)))] 
              (if (> curr_len %1) curr_len %1))  (count (name key)) table)))


(defn col-max
  "Return a single row map where keys are table's keys, the values are the corresponding max col width,
  that is: max string length."
  [table]
  (reduce-kv (fn foo [map key val](merge map {key (agg-maxlen table key)})) {} (first table)))

;; Function to make format string %witdhs

(defn fmts [width] (str "%" (+ 2 width) "s"))

;; Deft pretty print the table into a list of strings.

;; Convert each column rkey of the table into a formatted string, and return a list of the strings. This
;; "prints" one row into a list. Use reduce since we want a string which is an aggregate of the row.

(defn prow [mrow rkeys width col-widths]
  (reduce #(str (format (fmts (%2 col-widths)) (%2 mrow)) %1) "" rkeys))

;; Print the results: (doseq [curr (dpp (demo-deft))] (print curr))

(defn dpp
  "deft pretty print. Create a list where each item is a string for output. First item is the header line using the keys as column names."
  [table]
  (let [rkeys (vec (sort-by comp (keys (first table))))
       col-widths (col-max table)]
       (do
           (cons
            (println-str (reduce #(str (format (fmts (%2 col-widths)) (name %2)) %1) "" rkeys))
            (map (fn [mrow] (println-str (prow mrow rkeys 14 col-widths))) table)))))


;; Seems like add-column could be generalized to allow the value arg to be a function. The trick is to know
;; when or if the value function uses one of the other args, like table. Does %1 do that?

;; Called as:
;; (agg-add-col 'maxlen-make agg)))
(defn agg-add-col [table key func]
  (add-column table key (func table)))

;; Create a generalized aggregate function so we can do things like sum, max, etc.
;; Probably need a column arg where the result will be stored.
;; Might be able to call add-column to store the result.


;; (map) returns a list, so we have to (set) that into a set.  This shows how to send a var containing a list
;; of args to (assoc) via (apply (partial )) as opposed to passing hard coded arguments.

(defn where
  "Like a where clause run against each row of table. If the constraint is true do the rowfun on this row."
  [table constraint rowfun]
  (set (map #(if (constraint %)
                 (apply (partial assoc %) (rowfun %))
               %) table)))

(declare demo-deft)

;; Add a rowid. Numbering starts with 1. Rows numbers reflect the sort order of the table rows at the time
;; rowid is called. We can use this to do a sql-like unique row access. We could map a sequence against the
;; table, even if the sequence is an out of order map using the rowid values as keys. This could be used for
;; sort. The alternative is Noah's Church-encoding sortable keys, as used in Deft dcc. Implementing dcc might
;; be a good idea, even if rowid as a key works.

;; Rows are numbered descending so they'll be in the opposite order of the alphabetic sorting of :model. I
;; need something sorted reversed for testing, and the rows seem to stay ordered, and I haven't yet figured
;; out why.

(defn rowid [table col]
  (let [newcol (keyword col)]
  (map #(assoc %1  newcol %2) table
       (reductions + (map (fn [row] 1) table)))))


;; This is Deftish clojure, using the thread first macro (->). This demos add-row, add-column and the exciting
;; (where) function which takes two fn's as args. Also the less elegant function (merc-220) which works, but
;; relies knowledge of deftish internals.

;; The idioms for aggregation are not fully developed. There are two: 1) aggregate all rows for a single value
;; per column. 2) aggregate all rows and assign each row its own value. These are explored in some of the
;; functions necessary for Deft pretty print (dpp) above.

;; Discussion: Pure Deft would use "variable" names and not keys, as well as more natural syntax in
;; general. Forcing the user to fuss with keys, and to wrap all their code inside a macro makes this Deft-ish
;; and not real Deft. At the same time, Lisp syntax makes the Deft/Deft-ish distinction less distinct.

(defn demo-deft []
  (-> #{}
      (add-row {:make "mercedes" :model ""})
      (merc-220)
      (add-column 'disp 0)
      (add-row {:make "volkswagen" :model "golf"})
      (add-row {:make "toyota" :model "corolla"})
      (add-column "color" "white")
      (where (fn [row] (= "toyota" (:make row)))
             (fn [row] [:make "Toyota" :disp 1.8]))
      (where (fn [row] (and (= "golf" (:model row)) (= "volkswagen" (:make row))))
             (fn [row] [:disp 2.2]))
      (where (fn [row] (= "220b" (:model row)))
             (fn [row] [:disp 2.2 :color "green"]))
      ((fn [table] (add-column table :maxlen-make (agg-maxlen table 'make))))
      (where (fn [row] true)
             (fn [row] [:model (string/capitalize (:model row)) :make (string/capitalize (:make row))]))
      ((fn [table] (sort-by :make table)))
      (rowid 'make-sort)
      (where (fn [row] true)
             (fn [row] [:cid (* 61.0128 (:disp row))]))))


