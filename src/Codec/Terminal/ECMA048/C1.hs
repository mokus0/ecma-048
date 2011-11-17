module Codec.Terminal.ECMA048.C1 where

import Data.Attoparsec
import Data.Word

data C1
    = BPH
    | NBH
    | NEL
    | SSA
    | ESA
    | HTS
    | HTJ
    | VTS
    | PLD
    | PLU
    | RI
    | SS2
    | SS3
    | DCS
    | PU1
    | PU2
    | STS
    | CCH
    | MW
    | SPA
    | EPA
    | SOS
    | SCI
    | CSI
    | ST
    | OSC
    | PM
    | APC
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

bph base = word8 (base + 0x02) >> return BPH <?> "BPH"
nbh base = word8 (base + 0x03) >> return NBH <?> "NBH"
nel base = word8 (base + 0x05) >> return NEL <?> "NEL"
ssa base = word8 (base + 0x06) >> return SSA <?> "SSA"
esa base = word8 (base + 0x07) >> return ESA <?> "ESA"
hts base = word8 (base + 0x08) >> return HTS <?> "HTS"
htj base = word8 (base + 0x09) >> return HTJ <?> "HTJ"
vts base = word8 (base + 0x0a) >> return VTS <?> "VTS"
pld base = word8 (base + 0x0b) >> return PLD <?> "PLD"
plu base = word8 (base + 0x0c) >> return PLU <?> "PLU"
ri  base = word8 (base + 0x0d) >> return RI  <?> "RI"
ss2 base = word8 (base + 0x0e) >> return SS2 <?> "SS2"
ss3 base = word8 (base + 0x0f) >> return SS3 <?> "SS3" 
dcs base = word8 (base + 0x10) >> return DCS <?> "DCS"
pu1 base = word8 (base + 0x11) >> return PU1 <?> "PU1"
pu2 base = word8 (base + 0x12) >> return PU2 <?> "PU2"
sts base = word8 (base + 0x13) >> return STS <?> "STS"
cch base = word8 (base + 0x14) >> return CCH <?> "CCH"
mw  base = word8 (base + 0x15) >> return MW  <?> "MW"
spa base = word8 (base + 0x16) >> return SPA <?> "SPA"
epa base = word8 (base + 0x17) >> return EPA <?> "EPA"
sos base = word8 (base + 0x18) >> return SOS <?> "SOS"
sci base = word8 (base + 0x1a) >> return SCI <?> "SCI"
csi base = word8 (base + 0x1b) >> return CSI <?> "CSI"
st  base = word8 (base + 0x1c) >> return ST  <?> "ST"
osc base = word8 (base + 0x1d) >> return OSC <?> "OSC"
pm  base = word8 (base + 0x1e) >> return PM  <?> "PM"
apc base = word8 (base + 0x1f) >> return APC <?> "APC"

isC1 :: Word8 -> Word8 -> Bool
isC1 base c = c >= base && c < (base + 0x20) && not (reserved (c-base))
    where
        reserved 0x00 = True
        reserved 0x01 = True
        reserved 0x04 = True
        reserved 0x19 = True
        reserved    _ = False

encodeC1 :: Word8 -> C1 -> Word8
encodeC1 base BPH = base + 0x02
encodeC1 base NBH = base + 0x03
encodeC1 base NEL = base + 0x05
encodeC1 base SSA = base + 0x06
encodeC1 base ESA = base + 0x07
encodeC1 base HTS = base + 0x08
encodeC1 base HTJ = base + 0x09
encodeC1 base VTS = base + 0x0a
encodeC1 base PLD = base + 0x0b
encodeC1 base PLU = base + 0x0c
encodeC1 base RI  = base + 0x0d
encodeC1 base SS2 = base + 0x0e
encodeC1 base SS3 = base + 0x0f 
encodeC1 base DCS = base + 0x10
encodeC1 base PU1 = base + 0x11
encodeC1 base PU2 = base + 0x12
encodeC1 base STS = base + 0x13
encodeC1 base CCH = base + 0x14
encodeC1 base MW  = base + 0x15
encodeC1 base SPA = base + 0x16
encodeC1 base EPA = base + 0x17
encodeC1 base SOS = base + 0x18
encodeC1 base SCI = base + 0x1a
encodeC1 base CSI = base + 0x1b
encodeC1 base ST  = base + 0x1c
encodeC1 base OSC = base + 0x1d
encodeC1 base PM  = base + 0x1e
encodeC1 base APC = base + 0x1f

decodeC1 :: Word8 -> Word8 -> C1
decodeC1 base c = dec (c - base)
    where
        dec 0x02 = BPH
        dec 0x03 = NBH
        dec 0x05 = NEL
        dec 0x06 = SSA
        dec 0x07 = ESA
        dec 0x08 = HTS
        dec 0x09 = HTJ
        dec 0x0a = VTS
        dec 0x0b = PLD
        dec 0x0c = PLU
        dec 0x0d = RI 
        dec 0x0e = SS2
        dec 0x0f = SS3 
        dec 0x10 = DCS
        dec 0x11 = PU1
        dec 0x12 = PU2
        dec 0x13 = STS
        dec 0x14 = CCH
        dec 0x15 = MW 
        dec 0x16 = SPA
        dec 0x17 = EPA
        dec 0x18 = SOS
        dec 0x1a = SCI
        dec 0x1b = CSI
        dec 0x1c = ST 
        dec 0x1d = OSC
        dec 0x1e = PM 
        dec 0x1f = APC
        dec _ = error (unwords ["decodeC0:", show c, "is not in the C1 set (using offset", show base ++ ")"])
