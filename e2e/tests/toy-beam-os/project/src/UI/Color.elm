module UI.Color exposing (Color, color)


type alias Color =
    -- window border
    { activeWindowOuterBorder : String
    , activeWindowInnerBottomRightBorder : String
    , activeWindowInnerTopLeftBorder : String
    , inactiveWindowOuterBorder : String

    -- chrome
    , activeChromeBg : String
    , inactiveChromeBg : String

    -- window content
    , windowContentBorder : String
    , windowContentBg : String
    , windowContentCanvasBg : String

    -- text
    , activeStatusBarText : String
    , inactiveStatusBarText : String
    , activeWindowTitleText : String
    , inactiveWindowTitleText : String
    , defaultButtonText : String
    , activeButtonText : String
    , disabledButtonText : String

    -- divider
    , dividerBg : String
    , dividerShadow : String
    }


color : Color
color =
    -- window border
    { activeWindowOuterBorder = "#262626"
    , activeWindowInnerBottomRightBorder = "#8a8a8a"
    , activeWindowInnerTopLeftBorder = "#bbbbbb"
    , inactiveWindowOuterBorder = "#a2a2a2"

    -- chrome
    , activeChromeBg = "#cccccc"
    , inactiveChromeBg = "#dddddd"

    -- window content
    , windowContentBorder = "#a8a8a8"
    , windowContentBg = "#eeeeee"
    , windowContentCanvasBg = "#ffffff"

    -- text
    , activeStatusBarText = "#000000"
    , inactiveStatusBarText = "#8a8a8a"
    , activeWindowTitleText = "#262626"
    , inactiveWindowTitleText = "#818181"
    , defaultButtonText = "#262626"
    , activeButtonText = "#ffffff"
    , disabledButtonText = "#9d9d9d"

    -- divider
    , dividerBg = "#888888"
    , dividerShadow = "#ffffff"
    }
