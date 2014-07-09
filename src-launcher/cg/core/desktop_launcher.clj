(ns cg.core.desktop-launcher
    (:use cg.core)
    (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication LwjglApplicationConfiguration]
             [org.lwjgl.input Keyboard])
    (:gen-class))

(defn run-game
    "Run the game and return the LWJGL instance"
    []
    (Keyboard/enableRepeatEvents true)
    (let [config (LwjglApplicationConfiguration.)]
        (doto config
            (-> .width (set! 1280))
            (-> .height (set! 720))
            (-> .title (set! "Meta Strategy"))
            (-> .resizable (set! false))
            )
        (LwjglApplication. cg-game config)))

(defn -main [& args]
  (run-game))
