(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:bedroom1 {:desc "It's a big bedroom. Your head hurts, and you don't remember what happened last night. You found yourself on an unfamiliar comfortable soft bed. You want to know where you are. You might want to search first but you can also check around."
              :title "in the bedroom"
              :dir {:south :hallway
                    :north :bathroom1}
              :contents #{:letter}}

   :bathroom1 {:desc "You enter a small bathroom. You wash your face."
               :title "in the bathroom"
               :dir {:south :bedroom1}
               :contents #{}}

   :hallway {:desc "You are in a strangely long hallway. Lots of old pictures are on the wall. You know this place is special."
             :title "in the hallway"
             :dir {:north :bedroom1
                   :south :living-room
                   :down :basement}
             :contents #{:picture}}

   :living-room {:desc "You enter a huge living-room. It is so cool but its not your house. The door to the outside in right in front of you. You should go and check whether you can go out!"
                 :title "in the living-room"
                 :dir {:north :hallway
                       :west :bedroom2
                       :east :dining-room}
                 :contents #{:key-to-garage}}

   :bedroom2 {:desc "You are in a larger bedroom."
              :title "in the large bedroom"
              :dir {:south :studio
                    :east :living-room}
              :contents #{:flower}}

   :studio {:desc "You enter a studio room. A lot of books are here. Besides, there is a glass of water on the table."
            :title "in the studio"
            :dir {:north :bedroom2}
            :contents #{:water}}

   :dining-room {:desc "You are in the dining room. Although there are lots of seats, no one is here."
                 :title "in the dining-room"
                 :dir {:west :living-room
                       :north :kitchen}
                 :contents #{}}

   :kitchen {:desc "You are in the kitchen. You can eat the food here if you want! By the way... Is something hidden besides the stove?"
             :title "in the kitchen"
             :dir {:north :garage
                   :south :dining-room
                   :up :second-floor}
             :contents #{:food}}

   :garage {:desc "You enter a garage. It is huge and you think it is a good place to hide things. You probably want to search this place. You wonder, why should I search?"
            :title "in the garage"
            :dir {:south :kitchen}
            :contents #{:key-go-down}}

   :basement {:desc "You are in the basement of the house! It's dark but you know this are probably more clues around here!"
              :title "in the basement"
              :dir {:north :bar
                    :south :storeroom
                    :up :hallway}
              :contents #{:doll}}

   :bar {:desc "Uh there is a bar in the house! It is a messy place but the shining stuff besides the empty bottle on the bar get your attention."
         :title "in the bar"
         :dir {:south :basement}
         :contents #{:key-to-outside}}

   :storeroom {:desc "You are in the storeroom. Are there any useful tools that you probably can use?"
               :title "in the storeroom"
               :dir {:north :basement}
               :contents #{:hammer}}

   :second-floor {:desc "Wow you find the hidden second-floor in the kitchen! There are also plenty of pics on the wall. What do these pics mean? While thinking, you reach the second floor."
                  :title "going second-floor"
                  :dir {:north :loft
                        :south :bedroom3
                        :down :kitchen}
                  :contents #{}}

   :bedroom3 {:desc "It is a tiny tiny bedroom. There is a cute girl lying on the bed, looking at you! Do you want to talk with her?"
              :title "in the pink bedroom with a girl"
              :dir {:north :second-floor}
              :contents #{}}

   :loft {:desc "You enter the loft of the house. It is full of dust. You noticed a window besides you. What can you do?"
          :title "in the loft"
          :dir {:south :second-floor}
          :contents #{}}})

(def adventurer
  {:location :bedroom1
   :name ""
   :strength 50
   :inventory #{}
   :seen #{}})

(def help "Useful Command:\n \n Directions:\n\n south/north/west/east/up/down/\n\n Actions:\n\n pick/search/look/eat/drink/read/OUT and some hidden actions\n\n Status:\n\n help/check/health \n\n Hint: All roads lead to Rome and the truth is always hidden.\n")

(defn health [player]
  (let [health (player :strength)]
    (do (print "Health: ") (println health) player)))

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))

(defn pick [player]
  (let [curr-room (player :location)
        thing (-> the-map curr-room :contents)
        thing_have (player :inventory)]
    (if (empty? thing)
      (do (println "There is no item to pick. ") player)
      (let [vect (into [] thing)
            stuff (nth vect 0)
            intersect (clojure.set/intersection thing_have thing)]
        (if (empty? intersect)
          (do (println (str "You picked something up! You should check it. ")) (update-in player [:inventory] #(conj % stuff)))
          (do (println (str "You have already got the item here.")) player))))))

(defn check [player]
  (let [thing (player :inventory)]
    (if (empty? thing)
      (do (println "You got nothing. ") player)
      (do (print "You have ") (apply print thing) (print (str " in your pocket. ")) player))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (if (identical? dest :garage)
        (let [thing (player :inventory)]
          (if (nil? (thing :key-to-garage))
            (do (println "The door to garage is locked. You need the key.") player)
            (do (println "You have the key to the garage!") (assoc-in player [:location] dest))))
        (assoc-in player [:location] dest)))))

(defn search [player]
  (let [location (player :location)
        content (-> the-map location :contents)]
    (if (empty? content)
      (do (print (str "There is nothing here. "))
          player)
      (let [vect (into [] content)
            stuff (nth vect 0)]
      (if (contains? (player :inventory) stuff)
        (do (print (str "There is nothing here. ")) player)
        (do (print (str "There is ")) (apply print content) (print (str " here. ")) player))))))

(defn readLetter [player]
  (let [thing (player :inventory)]
    (if (empty? thing)
      (do (println "There is nothing for you to read. ") player)
      (if (nil? (thing :letter))
        (do (println (str "You did not get the message yet. Maybe you should go back and find it."))
            player)
        (do (println (str "You open the letter. It says: You have to find a way out. Hahaha!"))
            player)))))

(defn down [player]
  (let [location (player :location)
        dest (-> the-map location :dir :down)]
    (if (nil? dest)
      (do (println "You can't go that way.") player)
      (if (identical? dest :kitchen)
        (assoc-in player [:location] dest)
        (let [thing (player :inventory)]
          (if (nil? (thing :key-go-down))
            (do (println "There is a door going downstairs but you need the key first. ") player)
            (do (println "You have the key!") (assoc-in player [:location] dest))))))))

(defn eat [player]
  (let [thing (player :inventory)]
    (if (nil? (thing :food))
      (do (println "Sadly you don't have anything to eat... Maybe find the kitchen first?") player)
      (do (println "You eat your food and now you are full of energy!") (update-in (update-in player [:strength] + 30) [:inventory] #(disj % (thing :food)))))))

(defn drink [player]
  (let [thing (player :inventory)]
    (if (nil? (thing :water))
      (do (println "Sadly you don't have anything to drink... AHHHH I NEED WATER!") player)
      (do (println "You drink the water and now you are not thistry!") (update-in (update-in player [:strength] + 20) [:inventory] #(disj % (thing :water)))))))

(defn talk [player]
  (let [location (player :location)
        thing (player :inventory)]
    (if (not (identical? location :bedroom3))
      (do (println "Hmm... are you just talking to yourself?") player)
      (if (not (or (nil? (thing :picture)) (nil? (thing :flower)) (nil? (thing :doll))))
        (do (println "Hi~ I can't tell you who I am but if you have what I want I can help you. ")
            (println "Let's see... Wow you have all my missing treasures! Thank you and I will give you a gift. ")
            (update-in (update-in player [:inventory] #(disj % :picture :flower :doll)) [:inventory] #(conj % :sword)))
        (do (println "Hi~ I can't tell you who I am but if you have what I want I can help you. ")
            (println "I don't see what I need. You can go now.") player)))))

(defn break [player]
  (let [location (player :location)
        stuff (player :inventory)]
    (if (identical? location :loft)
      (if (nil? (stuff :hammer))
        (do (println "You tried your best but you cannot break the window. You need a tool to help you.") player)
        (if (>= (player :strength) 100)
          (do (println "You know you got a hammer in your pocket! You break the window and successfully get out! But a lot of misteries are in this house... you can search again if you want.") (System/exit 0))
          (do (println "Although you have a hammer but you are too weak to use it successfully. You need to eat and drink.") player)))
      (do (println "There is nothing for you to break! STOP!") player))))

(defn OUT [player]
  (let [location (player :location)
        stuff (player :inventory)]
    (if (identical? location :living-room)
      (if (not (nil? (stuff :key-to-outside)))
        (do (println "You have the key to outside but you have a last question to answer")
            (println "Here is the question: ")
            (println "How many NULL pointers do you have in a tree have 17 nodes?")
            (let [answer "18"
                  your-answer (read-line)]
              (if (= answer your-answer)
                (do (println "You successfully get out!")
                    (let [stuff (player :inventory)]
                      (if (nil? (stuff :sword))
                        (do (println "You met a monster, since you don't have a weapon, you are eaten. The magic power protected you and you have to start again. But you did not lose anything!")
                            (assoc-in (assoc-in (assoc-in player [:location] :bedroom1) [:inventory] #{}) [:strength] 50) player)
                        (do (println "You met a monster, and you have the sword from the girl! You killed it and you become a hero.")
                            (println "You suddenly wake up. Oh it is just a dream!")
                            (System/exit 0)))))
                (do (println "AHH you failed and you should try again by OUT command") player))))
        (do (println "Sadly you don't have a key to outside and you should find it.") player))
      (do (println "You tried. The walls are unbreakable.") player))))

(defn respond [player command]
  (match command
    [:health] (health player)
    [:OUT] (OUT player)
    [:break] (break player)
    [:talk] (talk player)
    [:help] (do (println help) player)
    [:eat] (eat player)
    [:drink] (drink player)
    [:read] (readLetter player)
    [:check] (check player)
    [:pick] (pick player)
    [:search] (search player)
    [:look] (update-in player [:seen] #(disj % (-> player :location)))
    [:north] (go :north player)
    [:south] (go :south player)
    [:west] (go :west player)
    [:east] (go :east player)
    [:up] (go :up player)
    [:down] (down player)
    _ (do (println "I don't understand you.")
          player)))

(defn -main
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println " What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))
