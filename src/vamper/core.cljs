(ns vamper.core
  (:require [clojure.string :as str]
            [tonejs]))

(def synth (.toMaster (new js/Tone.MembraneSynth)))

(defn play-note
  [pitch duration start-time]
  (.triggerAttackRelease synth pitch duration start-time))

(def soldiers-joy
  (str/split "F4 G4 A4 F4 D4 F4 A4 F4 D4 F4 A4 A4 D5 D5 D5 D5 C#5 B4 A4 F4 D4 F4 A4 F4 D4 F4 G4 G4 E4 E4 E4 E4" #"\s+"))

(defn play-notes
  [notes spacing]
  (let [now (.now js/Tone)]
    (doseq [[time-offset note] (map-indexed vector notes)]
      (play-note note "8n" (+ now (* spacing time-offset))))))

(comment
  (def delay-slider (.getElementById js/document "delayOffset"))
  (def output (.getElementById js/document "output-container"))

  (defn update-delay-slider
    []
    (-> output
        .-innerHTML
        (set! (.-value delay-slider))))
  (update-delay-slider)

  (-> delay-slider
      .-oninput
      (set! update-delay-slider)))
