# Hammerspoon setup details

1. Install hammerspoon from [here](https://github.com/Hammerspoon/hammerspoon)
2. Unzip and move `Hammerspoon.app` to `Applications/` folder.
3. Open the app and allow accessibility permissions. 
4. The hammerspoon config is stored at `~/.hammerspoon/init.lua`. This is the config that does all the work.
5. You can reload the config by using the hammerhead icon on the menu bar or by writing a function that starts a pathwatcher that reloads the lua files on saving any file.
