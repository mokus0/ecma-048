module Codec.Terminal.ECMA048.ICF where

import Data.Word

data ICF
    = DMI
    | INT
    | EMI
    | RIS
    | CMD
    | LS2
    | LS3
    | LS3R
    | LS2R
    | LS1R
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

isICF :: Word8 -> Bool
isICF 0x60 = True
isICF 0x61 = True
isICF 0x62 = True
isICF 0x63 = True
isICF 0x64 = True
isICF 0x6e = True
isICF 0x6f = True
isICF 0x7c = True
isICF 0x7d = True
isICF 0x7e = True
isICF    _ = False

encodeICF :: ICF -> Word8
encodeICF DMI  = 0x60
encodeICF INT  = 0x61
encodeICF EMI  = 0x62
encodeICF RIS  = 0x63
encodeICF CMD  = 0x64
encodeICF LS2  = 0x6e
encodeICF LS3  = 0x6f
encodeICF LS3R = 0x7c
encodeICF LS2R = 0x7d
encodeICF LS1R = 0x7e

decodeICF :: Word8 -> ICF
decodeICF 0x60 = DMI
decodeICF 0x61 = INT
decodeICF 0x62 = EMI
decodeICF 0x63 = RIS
decodeICF 0x64 = CMD
decodeICF 0x6e = LS2
decodeICF 0x6f = LS3
decodeICF 0x7c = LS3R
decodeICF 0x7d = LS2R
decodeICF 0x7e = LS1R
decodeICF c = error (unwords ["decodeICF:", show c, "is not a known independent control function"])

