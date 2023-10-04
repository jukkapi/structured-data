(ns structured-data)

(defn do-a-thing [x]
  (let [dx (+ x x)]
    (Math/pow dx dx)))

(defn spiff [v] (+ (get v 0) (get v 2)))

(defn cutify [v] (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (= (- y2 y1) (- x2 x1))
    ))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- y2 y1) (- x2 x1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[xx1 yy1] [xx2 yy2]] inner]
    (and (contains-point? outer (point xx1 yy1)) (contains-point? outer (point xx2 yy2)))))

(defn title-length [book] (count (:title book)))

(defn author-count [book] (count (:authors book)))

(defn multiple-authors? [book] (> (author-count book) 1))


(defn add-author [book new-author]
  (let [aa (:authors book)]
    (assoc book :authors (conj aa new-author))))

(defn alive? [author] (not (contains? author :death-year)))

(defn element-lengths [collection] (map count collection))

(defn second-elements [collection]
  (let [se (fn [e] (get e 1))]
    (map se collection)))

(defn titles [books] (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))


(defn stars [n] (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))


(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
    (let [as (fn [b] (:authors b))]
      (clojure.set/union (set (apply concat (map as books))))))


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (fn [n] (str (:name n)))
        byear (fn [by] (if (contains? by :birth-year)(str " (" (:birth-year by) " -") (str "")))
        dyear (fn [dy] (if (contains? dy :death-year)(str " " (:death-year dy) ")")(str " )")))]
    (str (name author) (byear author) (if (contains? author :birth-year)(dyear author)(str "")))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [cs (fn [bc] (cond (= 0 bc) (str "No books")
                      :else (str bc " book" (if (> bc 1)(str "s")) ". ")))]
    (str (cs (count books)) (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn[b](has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= (:name a) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  ( > (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
