(ns quick-web.core
  (:gen-class :main true)
  (:require [clojure.java.io :refer :all]
            [image-resizer.resize :refer :all]
            [image-resizer.core :refer :all]
            [image-resizer.format :as format]
            [image-resizer.util :as util])
  (:import
   [javax.imageio ImageIO]
   [java.io ByteArrayOutputStream]))


;; https://github.com/josephwilk/image-resizer/blob/master/README.md

(defn fdim [filex] 
  (if (and 
       (not (.isDirectory filex)) 
       (not= (.getName filex) ".picasa.ini"))
    (let [fname (.getName filex)
          dims (dimensions (ImageIO/read (file dirname fname)))]
      (vec (cons fname dims)))
    nil))


(defn -main
  "List files and X Y dimensions in pixels."
  []
  (let [dirname "/Users/twl/ipad_photos/share"
        directory (clojure.java.io/file dirname)
        files (file-seq directory)]
    (remove nil? (map #(fdim %) files))
    ))

  ;; (format/as-file
  ;;  (resize (file "tea-party/mad-hatter.jpg") 10 10)
  ;;  "/tmp/tea-party/mad-hatter.jpg") ; => "/tmp/tea-party/mad-hatter_10x5.jpg"

  ;; ;;Saving under a specific name
  ;; (format/as-file
  ;;  (resize (file "tea-party/mad-hatter.jpg") 10 10)
  ;;  "/tmp/tea-party/tiny-hatter.jpg"
  ;;  :verbatim) ; => "/tmp/tea-party/tiny-hatter.jpg"

  ;;                                       ;To a stream (Useful for s3)
  ;; (format/as-stream (resize (file "tea-party/mad-hatter.jpg") 10 10) "jpg") ; => #<ByteArrayInputStream>

