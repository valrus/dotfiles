-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This is where you actually apply your config choices

config.font = wezterm.font('Maple Mono', { weight = 'Medium' })
config.color_scheme = 'nord-light'

local scheme_colors = wezterm.color.get_builtin_schemes()[config.color_scheme]

config.window_frame = {
  -- The font used in the tab bar.
  -- Roboto Bold is the default; this font is bundled
  -- with wezterm.
  -- Whatever font is selected here, it will have the
  -- main font setting appended to it to pick up any
  -- fallback fonts you may have used there.
  font = wezterm.font { family = 'Roboto', weight = 'Bold' },

  -- The size of the font in the tab bar.
  -- Default to 10.0 on Windows but 12.0 on other systems
  font_size = 12.0,

  -- The overall background color of the tab bar when
  -- the window is focused
  active_titlebar_bg = scheme_colors.selection_bg,

  -- The overall background color of the tab bar when
  -- the window is not focused
  inactive_titlebar_bg = scheme_colors.selection_bg,
}

local ansi_color_names = {
    black = 1,
    maroon = 2,
    green = 3,
    olive = 4,
    navy = 5,
    purple = 6,
    teal = 7,
    silver = 8,
}

local bright_color_names = {
    grey = 1,
    red = 2,
    lime = 3,
    yellow = 4,
    blue = 5,
    fuchsia = 6,
    aqua = 7,
    white = 8,
}


config.colors = {
  tab_bar = {
    -- The color of the strip that goes along the top of the window
    -- (does not apply when fancy tab bar is in use)
    background = '#0b0022',

    -- The active tab is the one that has focus in the window
    active_tab = {
      -- The color of the background area for the tab
      bg_color = scheme_colors.background,
      -- The color of the text for the tab
      fg_color = scheme_colors.ansi[ansi_color_names.black],

      -- Specify whether you want "Half", "Normal" or "Bold" intensity for the
      -- label shown for this tab.
      -- The default is "Normal"
      intensity = 'Bold',

      -- Specify whether you want "None", "Single" or "Double" underline for
      -- label shown for this tab.
      -- The default is "None"
      underline = 'None',

      -- Specify whether you want the text to be italic (true) or not (false)
      -- for this tab.  The default is false.
      italic = false,

      -- Specify whether you want the text to be rendered with strikethrough (true)
      -- or not for this tab.  The default is false.
      strikethrough = false,
    },

    -- Inactive tabs are the tabs that do not have focus
    inactive_tab = {
      bg_color = config.window_frame.active_titlebar_bg,
      fg_color = '#808080',

      -- The same options that were listed under the `active_tab` section above
      -- can also be used for `inactive_tab`.
      intensity = 'Half',
    },

    -- You can configure some alternate styling when the mouse pointer
    -- moves over inactive tabs
    inactive_tab_hover = {
      bg_color = scheme_colors.brights[bright_color_names.blue],
      fg_color = scheme_colors.background,
      italic = true,

      -- The same options that were listed under the `active_tab` section above
      -- can also be used for `inactive_tab_hover`.
    },

    -- The new tab button that let you create new tabs
    new_tab = {
      bg_color = config.window_frame.active_titlebar_bg,
      fg_color = scheme_colors.foreground,

      -- The same options that were listed under the `active_tab` section above
      -- can also be used for `new_tab`.
    },

    -- You can configure some alternate styling when the mouse pointer
    -- moves over the new tab button
    new_tab_hover = {
      bg_color = scheme_colors.ansi[ansi_color_names.navy],
      fg_color = scheme_colors.brights[bright_color_names.white],
      italic = false,

      -- The same options that were listed under the `active_tab` section above
      -- can also be used for `new_tab_hover`.
    },
  },
}

-- and finally, return the configuration to wezterm
return config
