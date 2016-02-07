(ns temproj.core
  (:gen-class)
  (:use fungp.core) ;; include the core framework
  (:use fungp.util) ;; include utility functions
  (:use clojure.pprint) 
  (:use fungp.tutorial))

(defn mydiv
  [x y]
  (if (zero? y) 0
      (/ x y)))
   
   
(def functions
  '[[+ 2][- 2][* 2][mydiv 2][inc 1][dec 1]])
  
(def parameter
 ['x])
 
(def nums
 (map float (range 10)))
 
 (def inp
  "This defines the range of input to use as training input. The first argument for map here uses a shortcut for
   single-variable anonymous functions in Clojure."
  (map #(* 2 (- % 5)) (range 10)))

(defn outp
  [x]
  
  ;(+ (* x x) x 1)
  ;(+ (* x x) x 5)
  ;(* x x x)
  (+ (* x x x) 5)
)

(def io
  (map float (map outp inp)))

 
 
(defn fitness 
  [individual]
  (try (let [f (compile-tree individual parameter)
    results (map f inp)]
    (reduce + (map off-by-sq io results)))
    (catch Exception e (println e) (println individual))))
                   
(defn report
;;;[generation best-error best-program]
;;;(println (str "Generation:\t" generation "\n"))
;;;(println (str "Best error:\t" best-error "\n"))
;;;(println (str "Best program:\t" best-program "\n"))
;;;maybe (flush)

[tree fitness]
  (pprint tree)
  (println (str "Error:\t" fitness "\n"))
  (flush)
)

(defn guess_the_functions[]
(println "!!!!!!Starting the evolution!!!!!!!")
(let [options{:iterations 15 :migrations 15 :num-islands 6 :population-size 100 :tournament-size 7 :mutation-probability 0.5
				:max-depth 10 :terminals parameter :numbers nums :fitness fitness :functions functions :report report}
	 [tree score] (rest (run-genetic-programming options))]
	 (do (println "Evolution complete!")
	     (report tree score)))
)

(defn -main
  "Run the evolution to guess the function"
  [& args]
  (guess_the_functions) )