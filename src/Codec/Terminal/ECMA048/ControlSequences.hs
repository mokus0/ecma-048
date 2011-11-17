module Codec.Terminal.ECMA048.ControlSequences where

import Data.Word

isParam :: Word8 -> Bool
isParam c = 0x30 <= c && c <= 0x3f

isIntermediate :: Word8 -> Bool
isIntermediate c = 0x20 <= c && c <= 0x2f

isOpcode :: Word8 -> Bool
isOpcode c = 0x40 <= c && c <= 0x7e
