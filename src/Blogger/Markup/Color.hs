module Blogger.Markup.Color where

-- Word8 is an 8-bit unsigned integer type
import Data.Maybe (listToMaybe)
import Data.Word (Word8)

-- | A data type representing colors
data Color
  = RGB Word8 Word8 Word8

getBluePart :: Color -> Word8
getBluePart color =
  case color of
    RGB _ _ blue -> blue

data Brightness
  = Dark
  | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data AnsiColor
  = AnsiColor Brightness EightColor

ansiColorToVGA :: AnsiColor -> Color
ansiColorToVGA ansicolor =
  case ansicolor of
    AnsiColor Dark Black ->
      RGB 0 0 0
    AnsiColor Bright Black ->
      RGB 85 85 85
    AnsiColor Dark Red ->
      RGB 170 0 0
    AnsiColor Bright Red ->
      RGB 255 85 85

ansiToUbuntu :: AnsiColor -> Color
ansiToUbuntu ansiColor =
  case ansiColor of
    AnsiColor brightness color ->
      case brightness of
        Dark ->
          case color of
            Black -> RGB 1 1 1
            Red -> RGB 22 56 43
            Green -> RGB 57 181 74
            Yellow -> RGB 255 199 6
            Blue -> RGB 0 111 184
            Magenta -> RGB 118 38 113
            Cyan -> RGB 44 181 233
            White -> RGB 204 204 204
        Bright ->
          case color of
            Black -> RGB 128 128 128
            Red -> RGB 255 0 0
            Green -> RGB 0 255 0
            Yellow -> RGB 255 255 0
            Blue -> RGB 0 0 255
            Magenta -> RGB 255 0 255
            Cyan -> RGB 0 255 255
            White -> RGB 255 255 255

isBright :: AnsiColor -> Bool
isBright c =
  case c of
    (AnsiColor Bright _) -> True
    _ -> False
