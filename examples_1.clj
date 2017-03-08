
;; Use if-not for when to stop
(defn s3 []
  (reduce (fn [a v ] 
              (if-not (>= v 35)
                  (do
                      (prn a " " v)
                      (+ a 3))
                    (reduced {:a a :v v}))) 0 (iterate inc 0)))



;; Apply sends a list to a function as arg list, for variadic functions.

problem_7=> (max [6 5 4 3])
[6 5 4 3]
problem_7=> (max 6 5 4 3)
6
problem_7=> (apply max 6 5 4 3)
IllegalArgumentException Don't know how to create ISeq from: java.lang.Long  clojure.lang.RT.seqFrom (RT.java:542)

problem_7=> (apply max [6 5 4 3])
6
problem_7=> (let [foo [6 5 4 3 2]] (apply max foo))
6
problem_7=> 



;; Named args make it easy to use defaults for optional args, but are awkward otherwise.

;; (solution)
;; (solution :wanted 100)
;; (solution :wanted 10001)

(defn solution [ & {:keys [wanted] :or {wanted 100}}]
  (loop [checkme 3
        foundp [2]]
        ;; wanted (or 100)]
        (if (>= (count foundp) wanted)
            (last foundp)
          (recur (+ checkme 1) (if (checkp checkme foundp)
                                   (conj foundp checkme)
                                 foundp)))))



;; Derived from dpp, return a map of rowid and another column's values This returns a list of maps. We really
;; want values to be keys, and the rowid value to be values. Sort and then convert back later.
;; (defn sortall_1 []
;;   (let [rkeys [:rowid :make]] (map (fn [mrow] {:rowid (:rowid mrow) :make (:make mrow)}) (demo-deft))))

;; (defn sortall []
;;   (let [rkeys [:rowid :make]] (map (fn [mrow] {(keyword (:make mrow)) (:rowid mrow) }) (demo-deft))))

;; Why not just sort the rows as necessary, dynamically?

;; (reverse (sort-by :make (sortall_1)))
;; (sort-by :make (sortall_1))

;; (assign-rowid-ascending (sort-by :rowid (demo-deft)))


;; Need a map of col names and max value width for dpp.

;; deftish_1=>  (map #(str (name %)) (keys (first (demo-table))))
;; ("make" "model")

;; deftish_1=>  (keys (first (demo-table)))
;; (:make :model)

;; deftish_1=> (reduce (fn foo [map key](merge map {key 0})) {} (keys (first (demo-table))))
;; {:make 0, :model 0}

;; deftish_1=> (reduce-kv (fn foo [map key val](merge map {key 0})) {} (first (demo-table)))
;; {:make 0, :model 0}


(let [order [55 77 99 11]
     x [{:foo 2 :bar 11}
     {:bar 99 :foo 1}
     {:bar 55 :foo 2}
     {:foo 1 :bar 77}]]
     (sort-by 
      #((into {} (map-indexed (fn [i e] [e i]) order)) (:bar %)) 
      x))
(let [x [{:foo 2 :bar 11}
     {:bar 99 :foo 1}
     {:bar 55 :foo 2}
     {:foo 1 :bar 77}]] (sort-by (juxt :foo :bar) x))

(let [x [{:foo 2 :bar 11}
     {:bar 99 :foo 1}
     {:bar 55 :foo 2}
     {:foo 1 :bar 77}]] (sort-by :bar x))

;; Sort :bar in the order 55, 77, 99, 11
(let [order [55 77 99 11]
     x [{:foo 2 :bar 11}
     {:bar 99 :foo 1}
     {:bar 55 :foo 2}
     {:foo 1 :bar 77}]]
     (sort-by 
      #((into {} (map-indexed (fn [i e] [e i]) order)) (:bar %)) 
      x))


;; idiom: change one map into another, or into another kind of map.
;; Also, change other collections into a different collection.

(def x {:Blabla 1, :foo 1, :bla-bla 1, :Bla 2, :bla/bla 1, :bla 4, :blub 2, :hello 1, :Foo 2})
(into (sorted-map-by (fn [key1 key2]
                         (compare [(get x key2) key2]
                                  [(get x key1) key1])))  x)

;; Improved solution that doesn't lose keys with duplicate values.
;; http://clojuredocs.org/clojure.core/sorted-map-by
;; http://stackoverflow.com/questions/1528632/how-do-you-use-sorted-map-by-in-clojure
(into (sorted-map-by (fn [key1 key2] (compare [(key1 my-map2) key1] [(key2 my-map2) key2]))) my-map2)

;; To make sure that the sorting works, we can make sure that the comparator 
;; works on unique values

user=> (let [results {:A 1 :B 2 :C 2 :D 5 :E 1 :F 1}]
            (into (sorted-map-by (fn [key1 key2]
                                     (compare [(get results key2) key2]
                                              [(get results key1) key1])))
                  results))

=> {:D 5, :C 2, :B 2, :F 1, :E 1, :A 1}
> (into (hash-map) {:foo 1 :bar 2 :baz 3 :boz 4})
{:foo 1, :bar 2, :boz 4, :baz 3}

> (into #{} [:foo 1 :bar 2 :baz 3 :boz 4])
#{1 2 3 4 :foo :bar :boz :baz}

> (into [] ['foo 'bar 'baz 'boz])
[foo bar baz boz]

> (into (list) ['foo 'bar 'baz])
(baz bar foo)

(into (list) ['foo 'bar 'baz 'boz])


;; Not working. See agg-maxlen.
(defn agg-where [table constraint func]
  (let [multival 
    (reduce 
     #(let [row %2
           result %1]
           (if (constraint row result)
               (func row result)
             result))  0 table)]
             (where table (fn [row] (true))
                    (fn [row] [:maxlen-model multival]))))


;; Everything below is historical, or notes and will be removed soon.
  
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



(defn dpp2 []
  (let [table (demo-table)
       rkeys (vec (sort-by comp (keys (first table))))]
       (do
           (cons
            (println-str (reduce #(str (format "%12s" (name %2)) %1) "" rkeys))
            (map (fn [mrow] (println-str (prow mrow rkeys))) table)))))



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
      ;; arg to -> must be list, and (fn) must be inside list
      ((fn foo [table] (add-column table :maxlen-make (agg table))))))

;; does not work
;; ClassCastException clojure.lang.LazySeq cannot be cast to clojure.lang.IFn  deftish-1/demo-deft/fn--1671 (deftish_1.clj:121)
(defn demo-deft []
  (-> #{} (add-row {:make "toyota" :model "corolla"})
      (#((add-column % :maxlen-make (agg %))))))



> (-> 4 ((fn [arg] (str 1 arg))) (#(str  2 % 3)))
"2143"
> (-> 4 ((fn [arg] (str 1 arg))))
"14"


> (-> 4 ((fn [arg] (str arg 1 arg))))
"414"
> 

(#(prn %) 'foo)


> (#(prn %1 %2) 'foo 'bar)
foo bar
nil

;; like -> but named var can be in any position
;; use for agg-add-column
(as-> 'foo xx (str 'bar xx) (str xx 'baz))




(-> 'foo (str))

> (-> 4 (#(+ % 1)) (#(- % 1)) (#(+ % 1)))
5

> (-> 4 (#(str % 1)))
"41"
> (-> 4 (#(str 1 %)))
"14"

(defn pp4 []
  (let [table (demo-table)
       rkeys (vec (sort-by comp (keys (first table))))]
       (map #(format "%s %s\n" %1 (%1 %2)) 
            rkeys
            table)))

;; works, returns a list of make and model for first row.
(defn pp1 []
  (let [table (demo-table)
       rkeys (vec (sort-by comp (keys (first table))))]
       (map (fn [mkey] list (mkey (first table))) rkeys)))

;; Need to get the list for each row of the table
(defn pp2 []
  (let [table (demo-table)
       rkeys (vec (sort-by comp (keys (first table))))]
       (map (fn [mrow] 
                (map (fn [ikey] 
                         (format "%s: %s " ikey (ikey mrow))) rkeys))) table))
;; Try print-str, prn-str, etc.
;; Remember: (name ikey) to get the key as a string.
(defn prow-print [mrow rkeys]
  (do 
      (doseq [ikey rkeys]
             (print (format "%12s " (ikey mrow))))
      (prn)))
  
(defn pp3 []
  (let [table (demo-table)
       rkeys (vec (sort-by comp (keys (first table))))]
       (do
           (doseq [ikey rkeys] (print (format "%12s" (name ikey))))
           (prn)
           (map (fn [mrow] (prow-print mrow rkeys)) table))))

;; Build a format string key: %s and so on for a list of keys Probably easier to call format on each key for
;; the current row, at least for now.
(defn mkfmt [labels]
  (clojure.string/join " " (map (fn [key] (str key ": %s")) labels)))

;; Works with nake format inside apply because format is variadic.
(defn dd1 []
  (let [fmt (apply str ["%s " "%s " "%s "])]
       (apply format fmt  ['foo 'bar 1])))

;; format is already variadic, so just use it naked with apply
(apply format "%s %s\n" ['foo 1])

;; nope, see above
;; (apply (partial (format "%s %s" args))) ['foo 1])
;; (apply (partial (format "%s %s")) ['foo 1])


(defn d3 []
  (let [table (demo-table)
       rkeys (vec (sort-by comp (keys (first table))))]
       (map #(str %) rkeys)))

;; print row
(defn prow-1 [mrow rkeys]
  (map (fn [ikey] 
           (format "%s: %s " ikey (ikey mrow))) rkeys))

;; prints kv pairs, need to print the whole row
(defn prow-2 [mrow rkeys]
  (doseq [ikey rkeys]
         (prn (format "%s: %s " ikey (ikey mrow)))))

;; At least this does something.
;; (doseq [x (pp3)]
;;        (prn x))

;; (doseq [x (demo-table)] (prn x))
       



deftish_1=> (loop [x (range 10)] (prn x))
(0 1 2 3 4 5 6 7 8 9)
nil
deftish_1=> (doseq [x (range 10)] (prn x))
0
1
2
3
4
5
6
7
8
9
nil
deftish_1=> (run! #(prn %)  ['a 'b 'c])
a
b
c
nil


;; Original uses reduce and conj.
(defn add-column-v1 [table key value]
  (reduce #(conj %1 (assoc %2 (keyword key) value)) #{} table))

(defn mod-col-map-set-demo []
  (let [table #{{:make 'toyota :model 'corolla}
       {:make 'volkswagen :model 'golf}}]
       (map #(assoc % :make (string/capitalize (:make %))) table)))

(defn mod-col-map-set []
  (let [table #{{:make 'toyota :model 'corolla}
                    {:make 'volkswagen :model 'golf}}]
                    (map #(assoc % :make (string/capitalize (:make %))) table)))

(defn mod-col-map-list []
  (let [table (list {:make 'toyota :model 'corolla}
                    {:make 'volkswagen :model 'golf})]
                    (map #(assoc % :make (string/capitalize (:make %))) table)))

(defn mod-col-map-v3 []
  (let [result (conj ()
                     {:make 'toyota :model 'corolla}
                     {:make 'volkswagen :model 'golf}
                     )]
                      (reduce (fn [list row] 
                                  #(assoc row :make (string/capitalize (:make row))))
                              () result)))

(defn mod-col-map-v2 []
  (let [result (conj ()
                     {:make 'toyota :model 'corolla}
                     {:make 'volkswagen :model 'golf}
                     )]
                      (reduce (fn [list row] 
                                  (conj list (assoc row :make (string/capitalize (:make row)))))
                              () result)))

;; Works, returns new list of map with :make capitalized.
(defn mod-col-map-v1 []
  (let [result (conj ()
                     {:make 'toyota :model 'corolla}
                     {:make 'volkswagen :model 'golf}
                     )]
                      (reduce (fn [list row] 
                                  (conj list (assoc row :make (string/capitalize (get row :make )))))
                              () result)))

(defn model-value-as-list []
  (let [result (conj ()
                     {:make 'toyota :model 'corolla}
                     {:make 'volkswagen :model 'golf}
                     )]
                      (reduce (fn [list row] 
                                  (conj list (get row :make))) () result)))

;; Returns the updated table as a set, but awkward because reduce aggregates, forcing us to (conj) inside to
;; rebuild a new set, as opposed to (map) which needs no (conj) because (map) is not an aggregate function.
(defn where-v1 [table constraint rowfun]
  (reduce (fn [mymap row] 
              (if (constraint row)
                  (conj mymap (apply (partial assoc row) (rowfun row)))
                (conj mymap row))) #{} table))

;; Experimental. Only slightly different from where-v1 in that the (conj) is outside the (if)
(defn where-v3 [table constraint rowfun]
  (reduce (fn [mymap row] 
              (conj mymap (if (constraint row)
                              (apply (partial assoc row) (rowfun row))
                            row))) #{} table))

;; works, starts with a map, and the result is still a map (mostly)
(set (map #(str % 'foo) #{'bar 'baz 'boz}))

;; works, same result as above, but we have to conj to turn an aggregate result into a map.
(reduce #(conj %1 (str %2 'foo)) #{} #{'bar 'baz 'boz})


;; (map) wants to return a list, so we have to (set) that into a set.
(defn where-v2 [table constraint rowfun]
  (set (map #(if (constraint %)
            (apply (partial assoc %) (rowfun %))
          %) table)))

;; test (where-v3)
(defn t10 []
  (-> (table-demo)
      (where-v3 (fn [row] (= "toyota" (get row :make)))
                (fn [row] [:make "Toyota" :disp 1.8]))))

;; test (where-v2)
(defn t9 []
  (-> #{}
      (add-row {:make "toyota" :model "corolla"})
      (where-v2 (fn [row] (= "toyota" (get row :make)))
                (fn [row] [:make "Toyota" :disp 1.8]))))
;; test (where)
(defn t8 []
  (-> #{}
      (add-row {:make "toyota" :model "corolla"})
      (where-v1 (fn [row] (= "toyota" (get row :make)))
             (fn [row] [:make "Toyota" :disp 1.8]))))

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



;; thread-last
(defmacro ->>
    "Threads the expr through the forms. Inserts x as the
  last item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  last item in second form, etc."
  {:added "1.1"}
  [x & forms]
  (loop [x x,
        forms forms]
        (if forms
            (let [form (first forms)
                 threaded (if (seq? form)
                              (with-meta `(~(first form) ~@(next form)  ~x) (meta form))
                            (list form x))]
                            (recur threaded (next forms)))
          x)))

;; thread-first. Second item? Second after the function name, I guess, but the first param.
(defmacro ->
  "Threads the expr through the forms. Inserts x as the
  second item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  second item in second form, etc."
  {:added "1.0"}
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))


;; Aha. reduce needs 3 args.
(reduce  #(str %1 %2)
         {} (range 4))


;; Works to create set, just like #{}
;; (set [{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}])


;; reduce will use 2 args, and if supplied with 1 arg it will eat the first 2 components of arg 1
;; Supply reduce with 2 args: set set
;; This fails if arg 1 is a map, no surprise since we aren't supplying kv pairs.
;; Works if arg 1 is [] () or #{}
(reduce #(conj %1 (assoc %2 :disp 0)) #{} #{{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}})

;; Works, update every row
(reduce #(conj %1
               (update-in %2 [:make] clojure.string/capitalize))
               #{}
               #{{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}})

;; Works, only updates :make toyota
(reduce #(conj %1
               (if (= (get %2 :make) 'toyota)
                   (update-in %2 [:make] clojure.string/capitalize)
                 %2))
               #{}
               #{{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}})

;; Works. Add a row. Use {} map instead of #{} set. 
(defn add-row [add]
  (let [result #{{:make 'toyota :model 'corolla}
       {:make 'volkswagen :model 'golf}}]
       (conj result add)))

(add-row {:make 'mercedes :disp 2.2})

;; only returns the new row, but with a model.
(clojure.set/join
 (add-row {:make 'mercedes :disp 2.2})
 #{{:make 'mercedes :model "220b"}})

;; Also only returns the new row. Clearly need to (conj) with the other rows, somehow. This is why it silently
;; fails below with the one row that isn't part of the natural join.
(clojure.set/join
 #{{:make 'mercedes, :disp 2.2} {:make 'toyota, :model 'corolla} {:make 'volkswagen, :model 'golf}}
 #{{:make 'mercedes :model "220b"}}) 

;; Try to add a new key (column)
;; Still developing. Need to associate keys from new with keys in result, and add a new key :disp
;; (join)? What is #{} vs (conj)?

;; Works. Adds new column based on a natural join, here on key aka column :make
(let [result #{{:make 'toyota :model 'corolla}
     {:make 'volkswagen :model 'golf}}
     new #{{:make 'volkswagen :disp 1.8}
     {:make 'toyota :disp 1.0}}] (clojure.set/join result new))

;; Fails. PersistentArrayMap cannot be cast to java.util.Map$Entry  clojure.lang.APersistentMap$KeySeq.first
;; Need a set of map so #{} of {}
(defn fails-map-of-map []
(let [result {{:make 'toyota :model 'corolla}
     {:make 'volkswagen :model 'golf}}
     new {{:make 'volkswagen :disp 1.8}
     {:make 'toyota :disp 1.0}}] (clojure.set/join result new)))

;; Works, peforms natural join. Even using sets, it still doesn't add a row.
(let [result #{{:make 'toyota :model 'corolla}
     {:make 'volkswagen :model 'golf}}
     new #{{:make 'volkswagen :disp 1.8}
     {:make 'toyota :disp 1.0}
     {:make 'mercedes :disp 2.2}}
     ]
     (clojure.set/join result new))


;; Works. Modify values for a given column
;; (let [row {:make 'volkswagen :model 'golf}] (assoc row :make (clojure.string/capitalize (get row :make ))))


;; Works, sort of. Returns a #{} set instead of a () list
(let [result (conj ()
                     {:make 'toyota :model 'corolla}
                     {:make 'volkswagen :model 'golf}
                     )]
                      (clojure.set/project result [:make]))

;; Works. Returns a #{} set even if you start with a list via (conj () ...)
(let [result #{
     {:make 'toyota :model 'corolla}
     {:make 'volkswagen :model 'golf}
     }]
     (clojure.set/project result [:make]))

;; Works. Use #{} set instead of () list
(let [result #{
     {:make 'toyota :model 'corolla}
     {:make 'volkswagen :model 'golf}
     }]
     (reduce (fn [set row] 
                 (conj set (select-keys row [:make] ))) #{} result))

;; Works
;; (select-keys {:make 'volkswagen :model 'golf} '(:make))

       
;; (assert (= 1 result) (str "Got " result))))

;; Returns (toyota volkswagen)
;; (map #(:make %) #{{:make 'toyota :model 'corolla}
;;      {:make 'volkswagen :model 'golf}})

;; Works
(update-in  {:make 'toyota :model 'corolla} [:make] clojure.string/capitalize)

;; Works
(update-in  {:make 'toyota :model 'corolla} [:make] (fn [val] (clojure.string/capitalize val)))

;; Fails same as assoc. Both should work.
(reduce  #(let [mapi %]
                (conj mapi {:disp 0}))
        {{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}})

;; Works. Adds :disp 0 to map
(conj {:make 'toyota :model 'corolla} {:disp 0})

(loop  #(let [mapi %1]
                (conj mapi {:disp 0}))
        {{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}})

(into (set) [{:make 'toyota :model 'corolla} {:disp 1.8}])

(map #(into %  [{:disp 0}]) #{{:make 'toyota :model 'corolla}})
{:make 'volkswagen :model 'golf}})

(reduce (fn [mapi] 
            (assoc mapi :disp 0))
        #{{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}})

;; Nope
(reduce #(assoc %1 :disp 0)
        #{{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}})


;; nope
(reduce  #(let [mapi %1
               key :disp
               val 0] (assoc mapi key val))
        {{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}})


;; Works, outputs a vector, doesn't work on #{}
(reduce  #(let [mapi %1] mapi)
        {{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}})

;; user=> (assoc {:make 'toyota :model 'corolla} :disp 0)
;; {:make toyota, :model corolla, :disp 0}

(map (fn [mapi] (into mapi  [{:disp 0}])) #{{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}})

;; Works, returns list of maps
;; user=> (map (fn [mapi] (into mapi  [{:disp 0}])) #{{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}})
;; ({:make toyota, :model corolla, :disp 0} {:make volkswagen, :model golf, :disp 0})

;; Works but returns a list of map
;; user=> (map (fn [mapi] (into mapi  [{:disp 0}])) #{{:make 'toyota :model 'corolla} })
;; ({:make toyota, :model corolla, :disp 0})

;; Might need to union with #{}
;; Didn't work but produced somethign differen:
;; [{:make toyota, :model corolla} {:make volkswagen, :model golf} {:disp 0}]
;; Oddly, same result regardless of arg order %1 %2
(reduce  #(let [mapi %1
               seti %2]
               (clojure.set/union seti (conj mapi {:disp 0}))) #{}
               {{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}})
       
;; yes. Inner conj first arg must be set.
(conj
 (conj #{} (assoc {:make 'toyota :model 'corolla} :disp 0))
 (assoc  {:make 'volkswagen :model 'golf} :disp 0))

;; nope
(assoc-in  {:make 'toyota :model 'corolla} [:make] (clojure.string/capitalize :make))

;; no
(union {} {:foo 'bar} {:baz 'boz})

(conj {} {:foo 'bar} {:baz 'boz})

;; set wants a collection, but list or map doesn't work.
(set [{:foo 'bar} {:baz 'boz}])

(assoc {:make 'toyote :model 'corolla} :disp 0)

user=> (into {} [{:make 'toyota :model 'corolla} {:disp 1.8}])
{:make toyota, :model corolla, :disp 1.8}

user=> (into {:make 'toyota :model 'corolla} [{:disp 1.8}])
{:make toyota, :model corolla, :disp 1.8}

;; Not quite adding a key to each of a set of maps.
;; http://stackoverflow.com/questions/32688146/clojure-iterate-over-map-of-sets
(reduce (fn [m' key-seq]
            (assoc-in m' key-seq x)) ;; accumulate changes
          m   ;; initial-value
          v)) ;; collection to loop over

(reduce (fn [m' key-seq] 
            (assoc-in  m' :disp 0))
        #{{:make 'toyota :model 'corolla} {:make 'volkswagen :model 'golf}})


;; Works. Adds new column based on a natural join, here on key aka column :make
(let [result #{{:make 'toyota :model 'corolla}
     {:make 'volkswagen :model 'golf}}
     new #{{:make 'volkswagen :disp 1.8}
                 {:make 'toyota :disp 1.0}}] (clojure.set/join result new))


(def stooges [{:name "Larry" :birthday "Oct 05"} 
              {:name "Curly" :birthday "Jun 19"} 
              {:name "Moe"   :birthday "Oct 22"}])

(map #(select-keys % [:name]) stooges)

;; Works
(let [row {:make 'volkswagen :model 'golf}] (assoc row :make (clojure.string/capitalize (get row :make ))))

;; returns list (toyota volkswagen)
(let [result (conj ()
                   {:make 'toyota :model 'corolla}
                   {:make 'volkswagen :model 'golf}
                   )]
                    (reduce (fn [list row] 
                                (conj list (get row :make))) () result)))


;; change seq to set and select some part of it. Sets have only unique values.
;; Also: index, rename, join
(clojure.set/select #(<  5 %1) (set (range 10)))

;; create a set from a sequence, eliminate dups and put 1 into the set
(into #{1}  [2 2 3 3])


;; Works
(get {:make "volkswagen"} :make)

;; Doesn't work #{} is not {}
(get (set '(:make volkswagen)) :make)

;; Works, a sequence of set. In this context, unclear the value of set vs map.
(let [result (conj ()
                   #{:make 'toyota :model 'corolla}
                   #{:make 'volkswagen :model 'golf}
                   )] result))


;; These two lines seem to be equivalent
(reduce + (range 10))
(reduce #(+ %1 %2) (range 10))

(reduce (fn [arg val] (prn arg val) (+ arg val) ) 2 (range 10))

;; It is not necessarily sensible for the fn's result to be val, but we can do it:
(reduce (fn [arg val] (prn arg val) (+ arg val) val) 2 (range 10))
(reduce (fn [arg val] (prn arg val) (+ arg val) arg) 2 (range 10))

(reduce #(let [foo %1] (+ 1 foo) ) (range 10))

;; Calculate primes
(reduce
 (fn [primes number]
     (if (some zero? (map (partial mod number) primes))
         primes
       (conj primes number)))
 [2]
 (take 1000 (iterate inc 3)))


