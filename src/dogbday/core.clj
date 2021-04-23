(ns dogbday.core
  (:require [java-time :as t]
            [clojure.string :as s]
            [clojure.pprint :as pprint]))


(defn days-starting [date len]
  (let [until (t/plus date len)
        forever (map #(t/plus date (t/days %))
                     (range))
        year-of-dates (filter #(t/before? % until)
                              (take 370 forever))]
    year-of-dates))

(defn split-into [coll n]
  (let [size (count coll)
        increment (-> (/ (count coll) n) float int)
        indices (range 0 (- size 1) increment)]
    (take n
          (map #(nth coll %) indices))))

(defn dog-days-for-birthdate [birthdate len]
  (let [dates (split-into (days-starting birthdate
                                         len)
                          7)]
    dates))

(defn dates-for [start-date month-days]
  (let [year (t/as start-date :year)
        dates-in-year (map (fn [[month day]]
                             (t/local-date year month day))
                           month-days)]))

(defn month-day [date]
  (map #(t/as date %)
       [:month-of-year :day-of-month]))

(defn modular-nth [coll n]
  (nth coll (mod n (count coll))))

(defn dogdays-after
  "Given one year of dates (assuming first is the birthdate) gives the nth dogday after the birthdate. "
  [first-dogyear-dates n]
  (let [first-year (t/as (first first-dogyear-dates) :year)
        year-add (quot n (count first-dogyear-dates))
        dog-month-day (month-day (modular-nth first-dogyear-dates n))
        dogday-list (conj (apply list dog-month-day) (+ first-year year-add))
        dogday (apply t/local-date dogday-list)]
    dogday))

(defn indexed-next-dogdays
  "Returns the `n` dogdays (with index) after `from-date` for `birthdate`"
  [birthdate from-date n]
  (let [first-year-dates (dog-days-for-birthdate birthdate (t/years 1))
        all-dogdays (map (partial dogdays-after first-year-dates)
                         (range))
        dogdays-idxd (partition 2
                                (interleave (range)
                                            all-dogdays))]
    (take n (filter (fn [[idx date]]
                      (t/before? from-date date))
                    dogdays-idxd))))

(defn date-format [date]
  (t/format "YYYY-MM-dd" date))

(defn input-date [date-string]
  (apply t/local-date
         (map #(Integer/parseInt %)
              (s/split date-string #"-"))))

(defn ordinal-format [num]
  (pprint/cl-format nil "~:R" num))

(defn -main [& args]
  (let [today (t/local-date)
        birthday (if-let [date-string (first args)]
                   (input-date date-string)
                   today)
        num (if-let [num-str (second args)]
              (Integer/parseInt num-str)
              7)
        from-date (if-let [from-str (nth args 2 false)]
                    (input-date from-str)
                    today)
        dates-idxd (indexed-next-dogdays birthday from-date num)]
    (println (format "Theses are the next %d dog birthdays after %s for birthday %s:\n%s"
                     num
                     (date-format from-date)
                     (date-format birthday)
                     (s/join "\n"
                             (map (fn [[idx date]]
                                    (format "%s (%s dog-birthday)"
                                            (date-format date)
                                            (ordinal-format idx)))
                                  dates-idxd))))))
