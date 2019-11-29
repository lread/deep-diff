(ns lambdaisland.deep-diff.printer
  (:require [clojure.string :as string]
            [fipp.engine :as fipp]
            [puget.color :as color]
            [puget.dispatch]
            [puget.printer :as puget]
            [arrangement.core])
  (:import (java.text SimpleDateFormat)
           (java.util TimeZone)
           (java.sql Timestamp)
           (java.io StringWriter)))

(defn print-deletion [printer expr]
  (let [no-color (assoc printer :print-color false)]
    (color/document printer ::deletion [:span "-" (puget/format-doc no-color (:- expr))])))

(defn print-insertion [printer expr]
  (let [no-color (assoc printer :print-color false)]
    (color/document printer ::insertion [:span "+" (puget/format-doc no-color (:+ expr))])))

(defn print-mismatch [printer expr]
  [:group
   [:span ""] ;; needed here to make this :nest properly in kaocha.report/print-expr '=
   [:align
    (print-deletion printer expr) :line
    (print-insertion printer expr)]])

(defn print-other [printer expr]
  (let [no-color (assoc printer :print-color false)]
    (color/document printer ::other [:span "-" (puget/format-doc no-color expr)])))

(defn- map-handler [this value]
  (let [ks (#'puget/order-collection (:sort-keys this) value (partial sort-by first arrangement.core/rank))
        entries (map (partial puget/format-doc this) ks)]
    [:group
     (color/document this :delimiter "{")
     [:align (interpose [:span (:map-delimiter this) :line] entries)]
     (color/document this :delimiter "}")]))

(def ^:private ^ThreadLocal thread-local-utc-date-format
  (proxy [ThreadLocal] []
    (initialValue []
      (doto (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSS-00:00")
        (.setTimeZone (TimeZone/getTimeZone "GMT"))))))

(def ^:private print-date
  (puget/tagged-handler
    'inst
    #(.format ^SimpleDateFormat (.get thread-local-utc-date-format) %)))

(def ^:private ^ThreadLocal thread-local-utc-timestamp-format
  (proxy [ThreadLocal] []
    (initialValue []
      (doto (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss")
        (.setTimeZone (TimeZone/getTimeZone "GMT"))))))

(def ^:private print-timestamp
  (puget/tagged-handler
    'inst
    #(str (.format ^SimpleDateFormat (.get thread-local-utc-timestamp-format) %)
          (format ".%09d-00:00" (.getNanos ^Timestamp %)))))

(def ^:private print-calendar
  (puget/tagged-handler
    'inst
    #(let [formatted (format "%1$tFT%1$tT.%1$tL%1$tz" %)
           offset-minutes (- (.length formatted) 2)]
       (str (subs formatted 0 offset-minutes)
            ":"
            (subs formatted offset-minutes)))))

(def ^:private print-handlers
  {'lambdaisland.deep_diff.diff.Deletion
   print-deletion

   'lambdaisland.deep_diff.diff.Insertion
   print-insertion

   'lambdaisland.deep_diff.diff.Mismatch
   print-mismatch

   'clojure.lang.PersistentArrayMap
   map-handler

   'clojure.lang.PersistentHashMap
   map-handler

   'clojure.lang.MapEntry
   (fn [printer value]
     (let [k (key value)
           v (val value)]
       (let [no-color (assoc printer :print-color false)]
         (cond
           (instance? lambdaisland.deep_diff.diff.Insertion k)
           [:span
            (print-insertion printer k)
            (if (coll? v) (:map-coll-separator printer) " ")
            (color/document printer ::insertion (puget/format-doc no-color v))]

           (instance? lambdaisland.deep_diff.diff.Deletion k)
           [:span
            (print-deletion printer k)
            (if (coll? v) (:map-coll-separator printer) " ")
            (color/document printer ::deletion (puget/format-doc no-color v))]

           :else
           [:span
            (puget/format-doc printer k)
            (if (coll? v) (:map-coll-separator printer) " ")
            (puget/format-doc printer v)]))))

   'java.util.Date
   print-date

   'java.util.GregorianCalendar
   print-calendar

   'java.sql.Timestamp
   print-timestamp

   'java.util.UUID
   (puget/tagged-handler 'uuid str)})

(defn- print-handler-resolver [extra-handlers]
  (fn [^Class klz]
    (and klz (get (merge @#'print-handlers extra-handlers)
                  (symbol (.getName klz))))))

(defn register-print-handler!
  "Register an extra print handler.

  `type` must be a symbol of the fully qualified class name. `handler` is a
  Puget handler function of two arguments, `printer` and `value`."
  [type handler]
  (alter-var-root #'print-handlers assoc type handler))

(defn puget-printer
  ([]
   (puget-printer {}))
  ([opts]
   (let [extra-handlers (:extra-handlers opts)]
     (puget/pretty-printer (merge {:width          (or *print-length* 100)
                                   :print-color    true
                                   :color-scheme   {::deletion  [:black :bg-red]
                                                    ::insertion [:black :bg-green]
                                                    ::other     [:black :bg-yellow]
                                                    :delimiter  [:blue]
                                                    ;; puget uses green and red for
                                                    ;; boolean/tag, but we want to reserve
                                                    ;; those for diffed values.
                                                    :boolean    [:bold :cyan]
                                                    :tag        [:magenta]}
                                   :print-handlers  (print-handler-resolver extra-handlers)}
                                  (dissoc opts :extra-handlers))))))

(defn format-doc [expr printer]
  (puget/format-doc printer expr))

(defn fixup-indent-colors
  "We fixup to not colorize indents.
  Ideally this would be supported by puget and fipp, but it currently is not"
  [printed-doc]
  (loop [printed-doc printed-doc
         multiline-colorings (re-seq
                              #"(?msx)                     # allow comments, multiline and match all
                                (\u001b\[[0-9;]*[mK])      # any escape sequence
                                ([^\u001b]*?\n[^\u001b]*?) # followed by text with at least one
                                                           # newline and no embedded escape sequences
                                (\u001b\[0[mK])            # followed by a no-color sequence"
                              printed-doc)]
    (if (not (first multiline-colorings))
      printed-doc
      (let [[_ on-esc text off-esc] (first multiline-colorings)
            new-text (string/replace text #"(?ms)(\n *)" (str off-esc "$1" on-esc))]
        (recur (.replace printed-doc (str on-esc text off-esc) (str on-esc new-text off-esc))
               (rest multiline-colorings))))) )

(defn print-doc [doc printer]
  (let [doc-sw (StringWriter.)
        opts {:width (:width printer) :writer doc-sw}]
    (fipp.engine/pprint-document doc opts)
    (spit (or (:output-file printer) *out*) (fixup-indent-colors (str doc-sw)))))
