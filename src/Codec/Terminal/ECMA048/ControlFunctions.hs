module Codec.Terminal.ECMA048.ControlFunctions where

import Codec.Terminal.ECMA048.ControlSequences
import Data.ByteString as BS
import Data.Word

data ControlFunction
    = ICH
    | CUU
    | CUD
    | CUF
    | CUB
    | CNL
    | CPL
    | CHA
    | CUP
    | CHT
    | ED
    | EL
    | IL
    | DL
    | EF
    | EA

    | DCH
    | SSE
    | CPR
    | SU
    | SD
    | NP
    | PP
    | CTC
    | ECH
    | CVT
    | CBT
    | SRS
    | PTX
    | SDS
    | SIMD

    | HPA
    | HPR
    | REP
    | DA
    | VPA
    | VPR
    | HVP
    | TBC
    | SM
    | MC
    | HPB
    | VPB
    | PM
    | SGR
    | DSR
    | DAQ

    | SL
    | SR
    | GSM
    | GSS
    | FNT
    | TSS
    | JFY
    | SPI
    | QUAD
    | SSU
    | PFS
    | SHS
    | SVS
    | IGS
    | IDCS

    | PPA
    | PPR
    | PPB
    | SPD
    | DTA
    | SHL
    | SLL
    | FNK
    | SPQR
    | SEF
    | PEC
    | SSW
    | SACS
    | SAPV
    | STAB
    | GCC

    | TATE
    | TALE
    | TAC
    | TCC
    | TSR
    | SCO
    | SRCS
    | SCS
    | SLS
    | SCP
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

isKnown, isReserved, isPrivate :: ByteString -> Bool

isKnown c = case unpack c of
    [c]         -> isKnown0 c
    [0x20, c]   -> isKnown1 c
    _           -> False
isReserved c = case unpack c of
    [c]         -> isReserved0 c
    [0x20, c]   -> isReserved1 c
    _           -> False
isPrivate c = case unpack c of
    [c]         -> isPrivate0 c
    [0x20, c]   -> isPrivate1 c
    _           ->
        not (BS.null c)
        && BS.all isIntermediate (BS.init c)
        && isOpcode (BS.last c)

isKnown0, isKnown1 :: Word8 -> Bool
isKnown0 c = 0x40 <= c && c <= 0x6f && not (isReserved0 c)
isKnown1 c = 0x40 <= c && c <= 0x6f && not (isReserved1 c)

isReserved0, isReserved1 :: Word8 -> Bool
isReserved0 0x5f = True
isReserved0    _ = False
isReserved1 0x4e = True
isReserved1 0x69 = True
isReserved1 0x6a = True
isReserved1 0x6c = True
isReserved1 0x6d = True
isReserved1 0x6e = True
isReserved1 0x6f = True
isReserved1    _ = False

isPrivate0, isPrivate1 :: Word8 -> Bool
isPrivate0 c = 0x70 <= c && c <= 0x7e
isPrivate1 = isPrivate0

encodeControlFunction ICH   = pack [0x40]
encodeControlFunction CUU   = pack [0x41]
encodeControlFunction CUD   = pack [0x42]
encodeControlFunction CUF   = pack [0x43]
encodeControlFunction CUB   = pack [0x44]
encodeControlFunction CNL   = pack [0x45]
encodeControlFunction CPL   = pack [0x46]
encodeControlFunction CHA   = pack [0x47]
encodeControlFunction CUP   = pack [0x48]
encodeControlFunction CHT   = pack [0x49]
encodeControlFunction ED    = pack [0x4a]
encodeControlFunction EL    = pack [0x4b]
encodeControlFunction IL    = pack [0x4c]
encodeControlFunction DL    = pack [0x4d]
encodeControlFunction EF    = pack [0x4e]
encodeControlFunction EA    = pack [0x4f]

encodeControlFunction DCH   = pack [0x50]
encodeControlFunction SSE   = pack [0x51]
encodeControlFunction CPR   = pack [0x52]
encodeControlFunction SU    = pack [0x53]
encodeControlFunction SD    = pack [0x54]
encodeControlFunction NP    = pack [0x55]
encodeControlFunction PP    = pack [0x56]
encodeControlFunction CTC   = pack [0x57]
encodeControlFunction ECH   = pack [0x58]
encodeControlFunction CVT   = pack [0x59]
encodeControlFunction CBT   = pack [0x5a]
encodeControlFunction SRS   = pack [0x5b]
encodeControlFunction PTX   = pack [0x5c]
encodeControlFunction SDS   = pack [0x5d]
encodeControlFunction SIMD  = pack [0x5e]

encodeControlFunction HPA   = pack [0x60]
encodeControlFunction HPR   = pack [0x61]
encodeControlFunction REP   = pack [0x62]
encodeControlFunction DA    = pack [0x63]
encodeControlFunction VPA   = pack [0x64]
encodeControlFunction VPR   = pack [0x65]
encodeControlFunction HVP   = pack [0x66]
encodeControlFunction TBC   = pack [0x67]
encodeControlFunction SM    = pack [0x68]
encodeControlFunction MC    = pack [0x69]
encodeControlFunction HPB   = pack [0x6a]
encodeControlFunction VPB   = pack [0x6b]
encodeControlFunction PM    = pack [0x6c]
encodeControlFunction SGR   = pack [0x6d]
encodeControlFunction DSR   = pack [0x6e]
encodeControlFunction DAQ   = pack [0x6f]

encodeControlFunction SL    = pack [0x20, 0x40]
encodeControlFunction SR    = pack [0x20, 0x41]
encodeControlFunction GSM   = pack [0x20, 0x42]
encodeControlFunction GSS   = pack [0x20, 0x43]
encodeControlFunction FNT   = pack [0x20, 0x44]
encodeControlFunction TSS   = pack [0x20, 0x45]
encodeControlFunction JFY   = pack [0x20, 0x46]
encodeControlFunction SPI   = pack [0x20, 0x47]
encodeControlFunction QUAD  = pack [0x20, 0x48]
encodeControlFunction SSU   = pack [0x20, 0x49]
encodeControlFunction PFS   = pack [0x20, 0x4a]
encodeControlFunction SHS   = pack [0x20, 0x4b]
encodeControlFunction SVS   = pack [0x20, 0x4c]
encodeControlFunction IGS   = pack [0x20, 0x4d]
encodeControlFunction IDCS  = pack [0x20, 0x4f]

encodeControlFunction PPA   = pack [0x20, 0x50]
encodeControlFunction PPR   = pack [0x20, 0x51]
encodeControlFunction PPB   = pack [0x20, 0x52]
encodeControlFunction SPD   = pack [0x20, 0x53]
encodeControlFunction DTA   = pack [0x20, 0x54]
encodeControlFunction SHL   = pack [0x20, 0x55]
encodeControlFunction SLL   = pack [0x20, 0x56]
encodeControlFunction FNK   = pack [0x20, 0x57]
encodeControlFunction SPQR  = pack [0x20, 0x58]
encodeControlFunction SEF   = pack [0x20, 0x59]
encodeControlFunction PEC   = pack [0x20, 0x5a]
encodeControlFunction SSW   = pack [0x20, 0x5b]
encodeControlFunction SACS  = pack [0x20, 0x5c]
encodeControlFunction SAPV  = pack [0x20, 0x5d]
encodeControlFunction STAB  = pack [0x20, 0x5e]
encodeControlFunction GCC   = pack [0x20, 0x5f]

encodeControlFunction TATE  = pack [0x20, 0x60]
encodeControlFunction TALE  = pack [0x20, 0x61]
encodeControlFunction TAC   = pack [0x20, 0x62]
encodeControlFunction TCC   = pack [0x20, 0x63]
encodeControlFunction TSR   = pack [0x20, 0x64]
encodeControlFunction SCO   = pack [0x20, 0x65]
encodeControlFunction SRCS  = pack [0x20, 0x66]
encodeControlFunction SCS   = pack [0x20, 0x67]
encodeControlFunction SLS   = pack [0x20, 0x68]
encodeControlFunction SCP   = pack [0x20, 0x6b]


decodeControlFunction c = case unpack c of
    [0x40]       -> Just ICH
    [0x41]       -> Just CUU
    [0x42]       -> Just CUD
    [0x43]       -> Just CUF
    [0x44]       -> Just CUB
    [0x45]       -> Just CNL
    [0x46]       -> Just CPL
    [0x47]       -> Just CHA
    [0x48]       -> Just CUP
    [0x49]       -> Just CHT
    [0x4a]       -> Just ED
    [0x4b]       -> Just EL
    [0x4c]       -> Just IL
    [0x4d]       -> Just DL
    [0x4e]       -> Just EF
    [0x4f]       -> Just EA

    [0x50]       -> Just DCH
    [0x51]       -> Just SSE
    [0x52]       -> Just CPR
    [0x53]       -> Just SU
    [0x54]       -> Just SD
    [0x55]       -> Just NP
    [0x56]       -> Just PP
    [0x57]       -> Just CTC
    [0x58]       -> Just ECH
    [0x59]       -> Just CVT
    [0x5a]       -> Just CBT
    [0x5b]       -> Just SRS
    [0x5c]       -> Just PTX
    [0x5d]       -> Just SDS
    [0x5e]       -> Just SIMD

    [0x60]       -> Just HPA
    [0x61]       -> Just HPR
    [0x62]       -> Just REP
    [0x63]       -> Just DA
    [0x64]       -> Just VPA
    [0x65]       -> Just VPR
    [0x66]       -> Just HVP
    [0x67]       -> Just TBC
    [0x68]       -> Just SM
    [0x69]       -> Just MC
    [0x6a]       -> Just HPB
    [0x6b]       -> Just VPB
    [0x6c]       -> Just PM
    [0x6d]       -> Just SGR
    [0x6e]       -> Just DSR
    [0x6f]       -> Just DAQ

    [0x20, 0x40] -> Just SL
    [0x20, 0x41] -> Just SR
    [0x20, 0x42] -> Just GSM
    [0x20, 0x43] -> Just GSS
    [0x20, 0x44] -> Just FNT
    [0x20, 0x45] -> Just TSS
    [0x20, 0x46] -> Just JFY
    [0x20, 0x47] -> Just SPI
    [0x20, 0x48] -> Just QUAD
    [0x20, 0x49] -> Just SSU
    [0x20, 0x4a] -> Just PFS
    [0x20, 0x4b] -> Just SHS
    [0x20, 0x4c] -> Just SVS
    [0x20, 0x4d] -> Just IGS
    [0x20, 0x4f] -> Just IDCS

    [0x20, 0x50] -> Just PPA
    [0x20, 0x51] -> Just PPR
    [0x20, 0x52] -> Just PPB
    [0x20, 0x53] -> Just SPD
    [0x20, 0x54] -> Just DTA
    [0x20, 0x55] -> Just SHL
    [0x20, 0x56] -> Just SLL
    [0x20, 0x57] -> Just FNK
    [0x20, 0x58] -> Just SPQR
    [0x20, 0x59] -> Just SEF
    [0x20, 0x5a] -> Just PEC
    [0x20, 0x5b] -> Just SSW
    [0x20, 0x5c] -> Just SACS
    [0x20, 0x5d] -> Just SAPV
    [0x20, 0x5e] -> Just STAB
    [0x20, 0x5f] -> Just GCC

    [0x20, 0x60] -> Just TATE
    [0x20, 0x61] -> Just TALE
    [0x20, 0x62] -> Just TAC
    [0x20, 0x63] -> Just TCC
    [0x20, 0x64] -> Just TSR
    [0x20, 0x65] -> Just SCO
    [0x20, 0x66] -> Just SRCS
    [0x20, 0x67] -> Just SCS
    [0x20, 0x68] -> Just SLS
    [0x20, 0x6b] -> Just SCP
    
    _            -> Nothing
