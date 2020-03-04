hs.loadSpoon("SpoonInstall")
Install=spoon.SpoonInstall
hyper = {"cmd", "alt", "ctrl"}

-- Hello World
hs.hotkey.bind(hyper, "W", function()
  hs.notify.new({title="Hammerspoon", informativeText="Hello World"}):send()
end)

-- Resize window to left column of screen
hs.hotkey.bind(hyper, "Left", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w/2
    f.h = max.h

    win:setFrame(f)
end)

-- Resize window to right column of screen
hs.hotkey.bind(hyper, "Right", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end)

-- Auto reload hammerspoon configs
function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.notify.new({title="Hammerspoon", informativeText="Config loaded"}):send()

-- Spotify controls
-- Play the next song in queue
hs.hotkey.bind({"cmd", "ctrl"}, "Right", function()
    hs.spotify.next()
    hs.spotify.displayCurrentTrack()
end)

-- Play the previous song in queue
hs.hotkey.bind({"cmd", "ctrl"}, "Left", function()
    hs.spotify.previous()
    hs.spotify.displayCurrentTrack()
end)

-- Pause Spotify
hs.hotkey.bind({"cmd", "ctrl"}, "Space", function()
    hs.spotify.playpause()
end)

-- Launch or Focus Firefox
hs.hotkey.bind(hyper, "b", function() 
    hs.application.launchOrFocus("Firefox") 
end)

-- Launch iterm
hs.hotkey.bind(hyper, "t", function()
    hs.application.launchOrFocus("iTerm")
end)
