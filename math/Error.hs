module Error (
  Error(..)
) where

import Types
import Util

data Error = Error {
  getErrFile :: File,
  getErrPos  :: Pos,
  getMsg     :: String
}

instance Show Error where
  show err = let
    file = getErrFile err
    fname = getFname file
    src = getFsrc file
    srcLines = sanl src
    msg = getMsg err
    (Pos row col) = getErrPos err
    col1 = col + 1
    rowStr = show $ row + 1
    colStr = show $ col1
    indentSize = max 4 $ length rowStr + 1
    indentStr = indent indentSize
    in concat [
      "\n", fname, ":", rowStr, ":", colStr, ": error:\n",
      modifyLines (indentErrMsgLine indentSize) msg, "\n",
      indentStr, "|\n",
      padStart (indentSize - 1) rowStr, " | ", srcLines !! row, "\n",
      indentStr, "|", replicate col1 ' ', "^"
    ]

indent :: Int -> String
indent n = replicate n ' '

indentErrMsgLine :: Int -> String -> String
indentErrMsgLine n msg = indent n ++ msg