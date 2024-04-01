(ns vamper.core
  (:require
   [clojure.math :as math]
   [overtone.live :refer :all]))

(definst simple-flute [freq 880
                       duration 15/108
                       amp 0.5
                       attack 0.1
                       decay 0.5
                       sustain 0.8
                       release 1
                       gate 1
                       out 0]
  (let [env  (env-gen (adsr attack decay sustain release) gate :action FREE)
        mod1 (lin-lin:kr (sin-osc:kr 6) -1 1 (* freq 0.99) (* freq 1.01))
        mod2 (lin-lin:kr (lf-noise2:kr 1) -1 1 0.2 1)
        mod3 (lin-lin:kr (sin-osc:kr (ranged-rand 4 6)) -1 1 0.5 1)
        sig (distort (* env (sin-osc [freq mod1])))
        sig (* amp sig mod2 mod3)]
    (with-overloaded-ugens
      (* (line:kr 1 1 duration FREE)
         sig))))

(defn swing-durations
  [durations]
  (let [[first-beat other-beats] (partition-by #(< % 4) durations)
        first-beat+1             (if (seq other-beats)
                                   (concat first-beat [(first other-beats)])
                                   first-beat)
        [n1 n2 & rest-of-beat]   (map (partial apply -)
                                      (map vector (drop 1 first-beat+1) first-beat+1))
        new-beat                 (concat [(+ 1/3 n1) (- n2 1/3)] rest-of-beat)]
    (if (seq other-beats)
      (concat new-beat (swing-durations other-beats))
      new-beat)))

(defn swing-it
  [part]
  (let [cumulative-durations (reductions + 0 part)]
    (swing-durations cumulative-durations)))

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
        (swing-it
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
          1 1 1 1 4])
        times (reductions + 0 durations)]
    (map vector times durations pitches)))

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
    (map vector times durations pitches)))

(defn beats->time
  [beats tempo]
  (* beats
     (/ 15 tempo)))

(defn note-player
  "Expects the instrument to accept [freq duration amplitute]"
  [metro instrument tempo volume]
  (fn [[beat duration-in-beats freq]]
    (at (metro beat)
      (instrument freq (beats->time duration-in-beats tempo) volume))))

(defn play
  ([instrument metro notes tempo volume]
   (dorun (map (note-player metro instrument tempo volume) notes))))

(defn after [beats metro] (comp metro #(+ % beats)))

(defn play-tune!
  ([] (play-tune! 108))
  ([tempo]
   (let [metro (metronome (* 4 tempo))]
     (play simple-flute metro rise-ye-lazy-lubber-tune tempo 0.75)
     (play simple-flute metro rise-ye-lazy-lubber-bass tempo 2))))

(comment
  (play (metronome 120) melody)
  (play-round (metronome 120) melody)

  (let [metro (metronome 120)]
    (play metro melody)
    (play (after 0.24 metro) melody)))
