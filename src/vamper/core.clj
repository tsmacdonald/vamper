(ns vamper.core
  (:require
   [clojure.math :as math]
   [overtone.inst.synth :refer :all]
   [overtone.live :refer :all]))

(defn string
  [freq duration]
  (with-overloaded-ugens
    (* (line:kr 1 1 duration FREE)
       (pluck (* (white-noise)
                 (env-gen (perc 0.001 5) :action FREE))
              1 1 (/ 1 freq) (* duration 2) 0.25))))

(definst harpsichord [freq 440]
  (let [duration 3/2
        snd      (string freq duration)
        t1       (* 0.2 (string (* 2/1 freq) duration))
        t2       (* 0.15 (string (* 3/2 freq) duration))
        t3       (* 0.1 (string (* 4/3 freq) duration))
        t4       (* 0.1 (string (* 5/4 freq) duration))
        snd      (+ snd (mix [t1 t2 t3 t4]))]
    snd))

(def pitch-ratio-a-minor
  {:a 1
   :b 9/8
   :c 6/5
   :d 4/3
   :e 3/2
   :f 8/5
   :g 9/5})

(def a-hz 440)

(defn note->hertz
  [[note octave]]
  (let [octave-adjust (* a-hz (math/pow 2 (- octave 2)))]
    (* octave-adjust (pitch-ratio-a-minor note))))

(def rise-ye-lazy-lubber-bass
  (let [pitches
        (map note->hertz
             (partition 2
                        [:a 0 :a 0 :g -1 :g -1
                         :a 0 :e -1 :a 0 :a 0
                         :a 0 :a 0 :g -1 :g -1
                         :a 0 :e -1 :a 0
                         ;; A2
                         :a 0 :a 0 :g -1 :g -1
                         :a 0 :e -1 :a 0 :a 0
                         :a 0 :a 0 :g -1 :g -1
                         :a 0 :e -1 :a 0
                         ;; B1
                         :a 1 :b 1 :c 1 :c 1
                         :e 1 :e 0 :a 1 :a 0
                         :a 1 :b 1 :c 1 :c 1
                         :d 1 :e 1 :a 1
                         ;; B2
                         :a 1 :b 1 :c 1 :d 1
                         :e 1 :e 0 :a 1 :a 0
                         :a 1 :b 1 :c 1 :c 1
                         :d 1 :e 1 :a 1]))
        durations
        [2 2 2 2
         2 2 2 2
         2 2 2 2
         2 2 4
         ;; A2
         2 2 2 2
         2 2 2 2
         2 2 2 2
         2 2 4
         ;; B1
         2 2 2 2
         2 2 2 2
         2 2 2 2
         2 2 4
         ;; B2
         2 2 2 2
         2 2 2 2
         2 2 2 2
         2 2 4]
        times (reductions + 0 durations)]
    (map vector times pitches)))

(def rise-ye-lazy-lubber-tune
  (let [pitches
        (map note->hertz
             (partition 2
                        [:a 2 :a 2 :b 2 :g 1 :a 2
                         :c 2 :d 2 :e 2 :c 2 :a 2 :a 1
                         :a 2 :a 2 :c 2 :g 1 :b 2
                         :c 2 :d 2 :e 2 :c 2 :a 2
                         ;; A2
                         :a 2 :a 2 :b 2 :g 1 :a 2
                         :c 2 :d 2 :e 2 :c 2 :a 2 :a 1
                         :a 2 :a 2 :c 2 :g 1 :b 2
                         :c 2 :d 2 :e 2 :c 2 :a 2 :g 2
                         ;; B1
                         :a 3 :e 2 :g 2 :d 2 :e 2 :d 2 :c 2 :a 3
                         :g 2 :e 2 :d 2 :c 2 :a 2 :g 2
                         :a 3 :e 2 :g 2 :d 2 :e 2 :d 2 :c 2 :e 2
                         :d 2 :e 2 :g 2 :a 3 :a 2 :g 2
                         ;; B2
                         :a 3 :e 2 :g 2 :d 2 :e 2 :d 2 :c 2 :a 3
                         :g 2 :e 2 :d 2 :c 2 :a 2 :g 2
                         :a 3 :e 2 :g 2 :d 2 :e 2 :d 2 :c 2 :e 2
                         :d 2 :e 2 :g 2 :a 3 :a 2]
                        ))
        durations
        [2 1 1 3 1
         1 1 1 1 2 2
         2 1 1 3 1
         1 1 1 1 4
         ;; A2
         2 1 1 3 1
         1 1 1 1 2 2
         2 1 1 3 1
         1 1 1 1 3 1
         ;; B1
         1 1 1 1 1 1 1 1
         1 1 1 1 3 1
         1 1 1 1 1 1 1 1
         1 1 1 1 3 1
         ;; B2
         1 1 1 1 1 1 1 1
         1 1 1 1 3 1
         1 1 1 1 1 1 1 1
         1 1 1 1 4]
        times (reductions + 0 durations)]
    (map vector times pitches)))

(let [tempo 108]
  (play #_ flute (metronome (* 4 tempo)) rise-ye-lazy-lubber-tune)
  (play #_flute (metronome (* 4 tempo)) rise-ye-lazy-lubber-bass))

(def melody
  (let [pitches
        [67 67 67 69 71                 ; Row, row, row your boat,
         71 69 71 72 74                 ; Gently down the stream,
         79 79 79 74 74 74 71 71 71     ; Merrily, merrily, merrily, merrily,
         67 67 67 74 72 71 69 67]       ; Life is but a dream!
        durations
        [1 1 2/3 1/3 1
         2/3 1/3 2/3 1/3 2
         1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3
         2/3 1/3 2/3 1/3 2]
        times (reductions + 0 durations)]
    (map vector times pitches)))

(defn play
  ([metro notes] (play harpsichord metro notes))
  ([instrument metro notes]
   (let [play-note (fn [[beat freq]] (at (metro beat) (instrument freq)))]
     (dorun (map play-note notes)))))

(defn after [beats metro] (comp metro #(+ % beats)))

(comment
  (play (metronome 120) melody)
  (play-round (metronome 120) melody)

  (let [metro (metronome 120)]
    (play metro melody)
    (play (after 0.24 metro) melody)))
