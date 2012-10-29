(ns shackles.examples
  (:require [shackles.kernel :as kernel]
            [shackles.finite :as finite]
            [shackles.int :as int]
            [shackles.search :as search]))

;; sudoku

(defn- transpose [coll]
  (apply map list coll))

(defn sudoku [n-root assignments]
  (let [n (* n-root n-root)
        vars&doms (for [row (range 0 n)
                        col (range 0 n)]
                    (let [var (kernel/make-var [row col])
                          value (nth (nth assignments row) col)
                          domain (if (= 0 value)
                                   (int/int 1 n)
                                   (int/int value value))]
                      [var domain]))
        vars (map first vars&doms)
        rows (partition n vars)
        cols (transpose rows)
        squares (for [row-group (partition n-root rows)
                      col-group (partition n-root (transpose row-group))]
                  (flatten col-group))
        state (-> kernel/empty-state
                  (kernel/add-vars vars&doms)
                  (kernel/add-props (for [group (concat rows cols squares)]
                                    (kernel/all-different group))))
        search-strat (finite/->Exhaust vars (finite/->ChooseUnassigned) (finite/->SplitAssign))
        solution  (first (search/depth-first search/non-failed-leaf? search-strat state))]
    (when solution
      (for [row rows]
        (for [var row]
          (kernel/assigned (get-in solution [:doms var])))))))

(comment
  (let [problem [[0 0 0 2 0 5 0 0 0]
                 [0 9 0 0 0 0 7 3 0]
                 [0 0 2 0 0 9 0 6 0]
                 [2 0 0 0 0 0 4 0 9]
                 [0 0 0 0 7 0 0 0 0]
                 [6 0 9 0 0 0 0 0 1]
                 [0 8 0 4 0 0 1 0 0]
                 [0 6 3 0 0 0 0 8 0]
                 [0 0 0 6 0 8 0 0 0]]]
    (doseq [line (sudoku 3 problem)]
      (prn line))))
