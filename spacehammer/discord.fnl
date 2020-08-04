
(local channel-navigation
       [
        {:key :n
         :title "Next channel with unreads"
         :action (fn [] (hs.eventtap.keyStroke [:⇧ :⌥] :down))}
        {:key :p
         :title "Previous channel with unreads"
         :action (fn [] (hs.eventtap.keyStroke [:⇧ :⌥] :up))}
        {:key :j
         :title "Next channel"
         :action (fn [] (hs.eventtap.keyStroke [:⌥] :down))}
        {:key :k
         :title "Previous channel"
         :action (fn [] (hs.eventtap.keyStroke [:⌥] :up))}
        ])

(local server-navigation
       [
        {:key :j
         :title "Next server"
         :action (fn [] (hs.eventtap.keyStroke [:⌘ :⌥] :down))}
        {:key :k
         :title "Previous server"
         :action (fn [] (hs.eventtap.keyStroke [:⌘ :⌥] :up))}
        ])

(fn navigation
  []
  [{:key :b
    :title "Channels"
    :items channel-navigation }
   {:key :t
    :title "Servers"
    :items server-navigation }
   ])

(fn items
  []
  [{:key :space
    :title "Quick switcher"
    :action (fn [] (hs.eventtap.keyStroke [:⌘] :k))}
   ])

{:navigation navigation
 :items items}
