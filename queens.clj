(defn choose [xs n]
  (cond
    (< (count xs) n) []
    (= n 0) [[]]
    :else (concat
            (map (partial cons (first xs)) (choose (next xs) (dec n)))
            (choose (next xs) n)
            )
    )
  )

(defn pairs [n] (for [x (range n) y (range n)] [x,y]))

(defn abs [n] (if (< n 0) (* -1 n) n))

(defn queensIntersect [p q]
  (cond
    (= (first p) (first q)) true
    (= (last p) (last q)) true
    (= (abs (- (first p) (first q))) (abs (- (last p) (last q)))) true
    :else false
    )
  )

(defn someIntersect [ps]
  (some (partial apply queensIntersect)
    (choose ps 2)
    )
  )

(defn solve [n]
  (count
    (remove someIntersect
      (choose (pairs n) n)
      )
    )
  )

;(print (choose '(1 2 3 4) 4))
;(println (someIntersect '([0 0] [0 1] [0 2])))
(println (solve 6))