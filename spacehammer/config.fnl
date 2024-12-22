;; Copyright (c) 2017-2020 Ag Ibragimov & Contributors
;;
;;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;;
;;; Contributors:
;;   Jay Zawrotny <jayzawrotny@gmail.com>
;;
;;; URL: https://github.com/agzam/spacehammer
;;
;;; License: MIT
;;


(require-macros :lib.macros)
(local windows (require :windows))
(local emacs (require :emacs))
(local slack (require :slack))
(local discord (require :discord))

(local {:concat concat
        :logf logf} (require :lib.functional))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SpoonInstall setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local install (hs.loadSpoon "SpoonInstall"))
(set install.use_syncinstall true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URLDispatcher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local default-browser "org.mozilla.firefoxdeveloperedition")
(local jira-app "org.epichrome.app.Jira")
(local chrome-app "com.google.Chrome")

(install:andUse
 "URLDispatcher"
 {:config {:url_patterns
           [[ "https?://meet.google.com" chrome-app ]
            [ "https?://stream.meet.google.com" chrome-app ]
            [ "https?://drive.google.com" chrome-app ]
            [ "https?://docs.google.com" chrome-app ]
            [ "https?://accounts.google.com" chrome-app ]
            [ "https?://gather.town" chrome-app ]
            [ "https?://meet.jit.si" chrome-app ]]
           :default_handler default-browser}
  :start true})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local browser-app-name "Firefox Developer Edition")
(local mail-app-name "Thunderbird")
(local editor-app-name "Emacs")
(local terminal-app-name "iTerm2")
(local launcher-app-name "Alfred 5")

(hs.grid.setGrid "3x2" "1080x1920")
(hs.grid.setGrid "3x2" "27MU67")
(set hs.grid.HINTS [["f" "s" "c"] ["p" "t" "d"]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hs.console.clearConsole)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn activator
  [app-name ?then-fn]
  "
  A higher order function to activate a target app. It's useful for quickly
  binding a modal menu action or hotkey action to launch or focus on an app.
  Takes a string application name
  Returns a function to activate that app.

  Example:
  (local launch-emacs (activator \"Emacs\"))
  (launch-emacs)
  "
  (fn activate []
    (windows.activate-app app-name)
    (when ?then-fn (?then-fn))))

(fn toggle-console
  []
  "
  A simple action function to toggle the hammer spoon console.
  Change the keybinding in the common keys section of this config file.
  "
  (if-let [console (hs.console.hswindow)]
          (hs.closeConsole)
          (hs.openConsole)))

(fn reset-hs []
  (io.popen "killall Hammerspoon && open -a Hammerspoon &"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local return
       {:key :delete
        :title "Back"
        :action :previous})

(local hyper
       [:alt :shift :ctrl :cmd])

(local meh
       [:alt :shift :ctrl])

(local tab-navigation-items
       [{:key :n
         :title "Next tab"
         :action (fn [] (hs.eventtap.keyStroke [:⌘ :⇧] "]"))}
        {:key :p
         :title "Previous tab"
         :action (fn [] (hs.eventtap.keyStroke [:⌘ :⇧] "["))}])

(local tabs-items
       [{:key :t
         :title "Tabs"
         :items tab-navigation-items}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local window-thirds
       [{:key "asd"
         :title "Thirds"}
        {:key :a
         :action (fn [] (hs.grid.set (hs.window.focusedWindow) "0,0 1x2"))}
        {:key :s
         :action (fn [] (hs.grid.set (hs.window.focusedWindow) "1,0 1x2"))}
        {:key :d
         :action (fn [] (hs.grid.set (hs.window.focusedWindow) "2,0 1x2"))}])

(local window-two-thirds
       [{:mods [:shift]
         :key "ad"
         :title "Two-thirds"}
        {:mods [:shift]
         :key :a
         :action (fn [] (hs.grid.set (hs.window.focusedWindow) "0,0 2x2"))}
        {:mods [:shift]
         :key :d
         :action (fn [] (hs.grid.set (hs.window.focusedWindow) "1,0 2x2"))}])

(local window-jumps
       [{:key "hjkl"
         :title "Jump"}
        {:key :h
         :action "windows:jump-window-left"
         :repeatable false}
        { :key :j
         :action "windows:jump-window-above"
         :repeatable false}
        {:key :k
         :action "windows:jump-window-below"
         :repeatable false}
        {:key :l
         :action "windows:jump-window-right"
         :repeatable false}])

(local window-move-screens
       [{:key "n, p"
         :title "Move next\\previous screen"}
        {:mods [:shift]
         :key "n, p"
         :title "Move up\\down screens"}
        {:key :n
         :action "windows:move-south"
         :repeatable true}
        {:key :p
         :action "windows:move-north"
         :repeatable true}
        {:mods [:shift]
         :key :n
         :action "windows:move-west"
         :repeatable true}
        {:mods [:shift]
         :key :p
         :action "windows:move-east"
         :repeatable true}])

(local window-bindings
       (concat
        [return
         {:key :o
          :title "Last Window"
          :action "windows:jump-to-last-window"}]
        window-thirds
        window-two-thirds
        window-jumps
        window-move-screens
        [{:key :m
          :title "Maximize"
          :action "windows:maximize-window-frame"}
         ;; {:key :c
         ;;  :title "Center"
         ;;  :action "windows:center-window-frame"}
         {:key :g
          :title "Grid"
          :action "windows:show-grid"}
         {:key :u
          :title "Undo"
          :action "windows:undo-action"}]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apps Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local app-bindings
       [return
        {:key :e
         :title "Editor"
         :action (activator editor-app-name)}
        {:key :g
         :title "Chrome"
         :action (activator "Google Chrome")}
        {:key :w
         :title "Web browser"
         :action (activator browser-app-name)}
        {:key :t
         :title "Terminal"
         :action (activator terminal-app-name)}
        {:key :s
         :title "Slack"
         :action (activator "Slack")}
        {:key :m
         :title "Mail"
         :action (activator mail-app-name)}
        {:key :d
         :title "Docs"
         :action (activator "Dash")}
        {:key :c
         :title "Chat"
         :action (activator "Discord")}
         ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local system-bindings
       [return
        {:key :d
         :title "Sleep display"
         :action (fn [] (hs.caffeinate.lockScreen))}
        {:key :s
         :title "Sleep"
         :action (fn [] (hs.caffeinate.systemSleep))}
         ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Media Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local mpc-command "MPD_HOST=$HOME/.mpd/mpd.socket /opt/homebrew/bin/mpc")

(local media-bindings
       [return
        {:key :return
         :title "Find Music"
         :action
         (activator
          launcher-app-name
          (fn [] (hs.eventtap.keyStrokes "mpd ")))
         }
        {:key :s
         :title "Status"
         :action
         (activator
          launcher-app-name
          (fn [] (hs.eventtap.keyStrokes "mpdstatus")))
         }
        {:key :t
         :title "Playtime Search"
         :action
         (activator
          launcher-app-name
          (fn [] (hs.eventtap.keyStrokes "mpdtime ")))
         }
        {:key :p
         :title "Play or Pause"
         :action (fn [] (os.execute (.. mpc-command " toggle")))}
        {:key :h
         :title "Prev Track"
         :action (fn [] (os.execute (.. mpc-command " prev")))}
        {:key :l
         :title "Next Track"
         :action (fn [] (os.execute (.. mpc-command " next")))}
        {:key :j
         :title "Volume Down"
         :action (fn [] (os.execute (.. mpc-command " volume -10")))
         :repeatable true}
        {:key :k
         :title "Volume Up"
         :action (fn [] (os.execute (.. mpc-command " volume +10")))
         :repeatable true}
        ])

(local current-app-bindings
       [return {}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hammerspoon Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local hammerspoon-bindings
       [return
        {:key :c
         :title "Console"
         :action toggle-console}
        {:key :r
         :title "Reload config"
         :action hs.reload}
        ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Menu & Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local menu-items
       [{:key    :return
         :title  "Launcher"
         :action (activator launcher-app-name)}
        {:key   :w
         :title "Window"
         :enter "windows:enter-window-menu"
         :exit "windows:exit-window-menu"
         :items window-bindings}
        {:key   :a
         :title "Apps"
         :items app-bindings}
        {:key :s
         :title "System"
         :items system-bindings}
        {:key    :j
         :title  "Jump"
         :action "windows:jump"}
        {:key   :m
         :title "Media"
         :items media-bindings}
        {:mods  [:shift]
         :key   ";"
         :title "Paletro"
         :action (fn [] (hs.eventtap.keyStroke [:⌘] :F16))}
        {:key   :h
         :title "Hammerspoon"
         :items hammerspoon-bindings}
        ])

(local common-keys
       [{:mods [:cmd]
         :key :return
         :action "lib.modal:activate-modal"}
        {:mods hyper
         :key :l
         :action "apps:next-app"}
        {:mods hyper
         :key :h
         :action "apps:prev-app"}
        {:mods hyper
         :key :o
         :action "emacs:edit-with-emacs"}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App Specific Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn app-specific-items
  [top-level-items this-app-items]
       (concat
        menu-items
        top-level-items
        [{:key ","
          :title "App-specific"
          :items this-app-items}]))

(local chrome-keys
       [{:mods [:cmd :shift]
         :key :l
         :action "chrome:open-location"}
        {:mods [:alt]
         :key :k
         :action "chrome:next-tab"
         :repeat true}
        {:mods [:alt]
         :key :j
         :action "chrome:prev-tab"
         :repeat true}])

(local browser-items
        [{:key "'"
          :title "Edit with Emacs"
          :action "emacs:edit-with-emacs"}
         {:key "d"
          :title "Developer Tools"
          :action (fn [] (hs.eventtap.keyStroke [:⌘ :⌥] "i"))}
         ])

(local chrome-config
       {:key "Google Chrome"
        :keys chrome-keys
        :items (app-specific-items tabs-items browser-items)})

(local firefox-config
       {:key "Firefox Developer Edition"
        :keys []
        :items (app-specific-items tabs-items browser-items)})

(local discord-config
       {:key "Discord"
        :keys []
        :items (app-specific-items (discord.navigation) (discord.items))})

(local emacs-config
       {:key "Emacs"
        :launch "emacs:maximize"
        :items []
        :keys []})

(local hammerspoon-config
       {:key "Hammerspoon"
        :items (concat
                menu-items
                [{:key :r
                  :title "Reload Console"
                  :action hs.reload}
                 {:key :c
                  :title "Clear Console"
                  :action hs.console.clearConsole}])
        :keys []})

(local slack-config
       {:key "Slack"
        :items []
        :keys [{:mods [:cmd]
                :key  :g
                :action "slack:scroll-to-bottom"}
               {:mods [:ctrl]
                :key :r
                :action "slack:add-reaction"}
               {:mods [:ctrl]
                :key :h
                :action "slack:prev-element"}
               {:mods [:ctrl]
                :key :l
                :action "slack:next-element"}
               {:mods [:ctrl]
                :key :t
                :action "slack:thread"}
               {:mods [:ctrl]
                :key :p
                :action "slack:prev-day"}
               {:mods [:ctrl]
                :key :n
                :action "slack:next-day"}
               {:mods [:ctrl]
                :key :e
                :action "slack:scroll-up"
                :repeat true}
               {:mods [:ctrl]
                :key :y
                :action "slack:scroll-down"
                :repeat true}
               {:mods [:ctrl]
                :key :i
                :action "slack:next-history"
                :repeat true}
               {:mods [:ctrl]
                :key :o
                :action "slack:prev-history"
                :repeat true}
               {:mods [:ctrl]
                :key :j
                :action "slack:down"
                :repeat true}
               {:mods [:ctrl]
                :key :k
                :action "slack:up"
                :repeat true}]})

(local hammerspoon-config
       {:key "Hammerspoon"
        :items (concat
                menu-items
                [{:key :r
                  :title "Reload Console"
                  :action hs.reload}
                 {:key :c
                  :title "Clear Console"
                  :action hs.console.clearConsole}])
        :keys []})

(local terminal-items [])

(local terminal-config
       {:key terminal-app-name
        :items (app-specific-items tabs-items terminal-items)
        :keys []})

(local apps
       [chrome-config
        discord-config
        firefox-config
        emacs-config
        hammerspoon-config
        slack-config
        terminal-config])

(local config
       {:title "Main Menu"
        :items menu-items
        :keys  common-keys
        :enter (fn [] (windows.hide-display-numbers))
        :exit  (fn [] (windows.hide-display-numbers))
        :apps  apps
        :hyper {:key :F18}})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

config
