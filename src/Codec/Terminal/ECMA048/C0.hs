module Codec.Terminal.ECMA048.C0 where

import Data.Attoparsec
import Data.Word

data C0
    = NUL
    | SOH
    | STX
    | ETX
    | EOT
    | ENQ
    | ACK
    | BEL
    | BS
    | HT
    | LF
    | VT
    | FF
    | CR
    | SO
    | SI
    | DLE
    | DC1
    | DC2
    | DC3
    | DC4
    | NAK
    | SYN
    | ETB
    | CAN
    | EM
    | SUB
    | ESC
    | IS4
    | IS3
    | IS2
    | IS1
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

nul = word8 0x00 >> return NUL <?> "NUL"
soh = word8 0x01 >> return SOH <?> "SOH"
stx = word8 0x02 >> return STX <?> "STX"
etx = word8 0x03 >> return ETX <?> "ETX"
eot = word8 0x04 >> return EOT <?> "EOT"
enq = word8 0x05 >> return ENQ <?> "ENQ"
ack = word8 0x06 >> return ACK <?> "ACK"
bel = word8 0x07 >> return BEL <?> "BEL"
bs  = word8 0x08 >> return BS  <?> "BS"
ht  = word8 0x09 >> return HT  <?> "HT"
lf  = word8 0x0a >> return LF  <?> "LF"
vt  = word8 0x0b >> return VT  <?> "VT"
ff  = word8 0x0c >> return FF  <?> "FF"
cr  = word8 0x0d >> return CR  <?> "CR"
so  = word8 0x0e >> return SO  <?> "SO"
si  = word8 0x0f >> return SI  <?> "SI"
dle = word8 0x10 >> return DLE <?> "DLE"
dc1 = word8 0x11 >> return DC1 <?> "DC1"
dc2 = word8 0x12 >> return DC2 <?> "DC2"
dc3 = word8 0x13 >> return DC3 <?> "DC3"
dc4 = word8 0x14 >> return DC4 <?> "DC4"
nak = word8 0x15 >> return NAK <?> "NAK"
syn = word8 0x16 >> return SYN <?> "SYN"
etb = word8 0x17 >> return ETB <?> "ETB"
can = word8 0x18 >> return CAN <?> "CAN"
em  = word8 0x19 >> return EM  <?> "EM"
sub = word8 0x1a >> return SUB <?> "SUB"
esc = word8 0x1b >> return ESC <?> "ESC"
is4 = word8 0x1c >> return IS4 <?> "IS4"
is3 = word8 0x1d >> return IS3 <?> "IS3"
is2 = word8 0x1e >> return IS2 <?> "IS2"
is1 = word8 0x1f >> return IS1 <?> "IS1"

isC0 :: Word8 -> Bool
isC0 c = c < 0x20

encodeC0 :: C0 -> Word8
encodeC0 NUL = 0x00
encodeC0 SOH = 0x01
encodeC0 STX = 0x02
encodeC0 ETX = 0x03
encodeC0 EOT = 0x04
encodeC0 ENQ = 0x05
encodeC0 ACK = 0x06
encodeC0 BEL = 0x07
encodeC0 BS  = 0x08
encodeC0 HT  = 0x09
encodeC0 LF  = 0x0a
encodeC0 VT  = 0x0b
encodeC0 FF  = 0x0c
encodeC0 CR  = 0x0d
encodeC0 SO  = 0x0e
encodeC0 SI  = 0x0f
encodeC0 DLE = 0x10
encodeC0 DC1 = 0x11
encodeC0 DC2 = 0x12
encodeC0 DC3 = 0x13
encodeC0 DC4 = 0x14
encodeC0 NAK = 0x15
encodeC0 SYN = 0x16
encodeC0 ETB = 0x17
encodeC0 CAN = 0x18
encodeC0 EM  = 0x19
encodeC0 SUB = 0x1a
encodeC0 ESC = 0x1b
encodeC0 IS4 = 0x1c
encodeC0 IS3 = 0x1d
encodeC0 IS2 = 0x1e
encodeC0 IS1 = 0x1f

decodeC0 :: Word8 -> C0
decodeC0 0x00 = NUL
decodeC0 0x01 = SOH
decodeC0 0x02 = STX
decodeC0 0x03 = ETX
decodeC0 0x04 = EOT
decodeC0 0x05 = ENQ
decodeC0 0x06 = ACK
decodeC0 0x07 = BEL
decodeC0 0x08 = BS 
decodeC0 0x09 = HT 
decodeC0 0x0a = LF 
decodeC0 0x0b = VT 
decodeC0 0x0c = FF 
decodeC0 0x0d = CR 
decodeC0 0x0e = SO 
decodeC0 0x0f = SI 
decodeC0 0x10 = DLE
decodeC0 0x11 = DC1
decodeC0 0x12 = DC2
decodeC0 0x13 = DC3
decodeC0 0x14 = DC4
decodeC0 0x15 = NAK
decodeC0 0x16 = SYN
decodeC0 0x17 = ETB
decodeC0 0x18 = CAN
decodeC0 0x19 = EM 
decodeC0 0x1a = SUB
decodeC0 0x1b = ESC
decodeC0 0x1c = IS4
decodeC0 0x1d = IS3
decodeC0 0x1e = IS2
decodeC0 0x1f = IS1
decodeC0 c = error (unwords ["decodeC0:", show c, "is not in the C0 set"])
