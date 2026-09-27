(ns DrClojure.json-helper
  "Simple pure-Clojure JSON parser for running official Parinfer test suites."
  (:require [clojure.string :as str]))

(defn- parse-string [^String s idx]
  (let [len (.length s)
        sb (StringBuilder.)]
    (loop [i (inc idx)]
      (if (>= i len)
        (throw (ex-info "Unterminated string" {:idx idx}))
        (let [c (.charAt s i)]
          (cond
            (= c \") [(.toString sb) (inc i)]
            (= c \\)
            (let [next-c (.charAt s (inc i))]
              (case next-c
                \" (do (.append sb \") (recur (+ i 2)))
                \\ (do (.append sb \\) (recur (+ i 2)))
                \/ (do (.append sb \/) (recur (+ i 2)))
                \b (do (.append sb \backspace) (recur (+ i 2)))
                \f (do (.append sb \formfeed) (recur (+ i 2)))
                \n (do (.append sb \newline) (recur (+ i 2)))
                \r (do (.append sb \return) (recur (+ i 2)))
                \t (do (.append sb \tab) (recur (+ i 2)))
                \u (let [hex (.substring s (+ i 2) (+ i 6))
                         ch (char (Integer/parseInt hex 16))]
                     (.append sb ch)
                     (recur (+ i 6)))
                (do (.append sb next-c) (recur (+ i 2)))))
            :else
            (do (.append sb c) (recur (inc i)))))))))

(defn parse-json [^String s]
  (let [len (.length s)]
    (letfn [(skip-ws [i]
              (loop [idx i]
                (if (and (< idx len) (Character/isWhitespace (.charAt s idx)))
                  (recur (inc idx))
                  idx)))
            (parse-val [i]
              (let [i (skip-ws i)]
                (if (>= i len)
                  (throw (ex-info "Unexpected EOF" {}))
                  (let [c (.charAt s i)]
                    (cond
                      (= c \{) (parse-obj (inc i))
                      (= c \[) (parse-arr (inc i))
                      (= c \") (parse-string s i)
                      (or (= c \-) (Character/isDigit c)) (parse-num i)
                      (.startsWith s "true" i) [true (+ i 4)]
                      (.startsWith s "false" i) [false (+ i 5)]
                      (.startsWith s "null" i) [nil (+ i 4)]
                      :else (throw (ex-info (str "Unexpected char: " c) {:idx i})))))))
            (parse-num [i]
              (let [j (loop [idx i]
                        (if (and (< idx len)
                                 (let [c (.charAt s idx)]
                                   (or (Character/isDigit c)
                                       (= c \.) (= c \-) (= c \+) (= c \e) (= c \E))))
                          (recur (inc idx))
                          idx))
                    num-str (.substring s i j)]
                [(if (re-find #"[.eE]" num-str)
                   (Double/parseDouble num-str)
                   (Long/parseLong num-str))
                 j]))
            (parse-arr [i]
              (loop [idx i, acc []]
                (let [idx (skip-ws idx)]
                  (if (>= idx len)
                    (throw (ex-info "Unterminated array" {}))
                    (if (= (.charAt s idx) \])
                      [acc (inc idx)]
                      (let [[v next-i] (parse-val idx)
                            next-i (skip-ws next-i)
                            c (.charAt s next-i)]
                        (cond
                          (= c \]) [(conj acc v) (inc next-i)]
                          (= c \,) (recur (inc next-i) (conj acc v))
                          :else (throw (ex-info (str "Expected , or ] but got " c) {:idx next-i})))))))))
            (parse-obj [i]
              (loop [idx i, acc {}]
                (let [idx (skip-ws idx)]
                  (if (>= idx len)
                    (throw (ex-info "Unterminated object" {}))
                    (if (= (.charAt s idx) \})
                      [acc (inc idx)]
                      (let [[k after-k] (parse-string s idx)
                            after-colon (let [c-idx (skip-ws after-k)]
                                          (if (= (.charAt s c-idx) \:)
                                            (inc c-idx)
                                            (throw (ex-info "Expected :" {:idx c-idx}))))
                            [v next-i] (parse-val after-colon)
                            next-i (skip-ws next-i)
                            c (.charAt s next-i)]
                        (cond
                          (= c \}) [(assoc acc (keyword k) v) (inc next-i)]
                          (= c \,) (recur (inc next-i) (assoc acc (keyword k) v))
                          :else (throw (ex-info (str "Expected , or } but got " c) {:idx next-i})))))))))]
      (first (parse-val 0)))))
