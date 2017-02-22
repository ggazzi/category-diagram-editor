module MeasureText exposing (measureText)

import Native.MeasureText


measureText : String -> String -> Int
measureText =
    Native.MeasureText.measureText
