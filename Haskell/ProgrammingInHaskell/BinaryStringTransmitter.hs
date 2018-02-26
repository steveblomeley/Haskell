-- Binary string transmitter
-- Assumption: our binary encoding is little-endian, ie: 1011 = (1*1) + (0*2) + (1*4) + (1*8) = 13

import Data.Char
type Bit = Int


-- (1) Convert bits to integer
-- Hint: Use iterate to provide an infinite series of binary weights: 1, 2, 4, 8, etc
bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip (iterate (*2) 1) bits]

-- But given that:
--     bin2int [b1,b2,b4,b8] = b1 + (2*b2) + (4*b4) + (8*b8)
-- We can factor out "2" from each step:
--     bin2int [b1,b2,b4,b8] = b1 + 2 * (b2 + 2 * (b4 + 2 * (b8 + 2 * (0))))
-- And that looks a lot like a fold:
bin2int' :: [Bit] -> Int
bin2int' bits = foldr (\bit acc -> bit + 2 * acc) 0 bits


-- (2) Convert integer to bits
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin i = i `mod` 2 : int2bin (i `div` 2)


-- (3) Convert bits to a byte (ie: right pad with zeros to 8 bits)
bits2byte :: [Bit] -> [Bit]
bits2byte xs = take 8 (xs ++ repeat 0)

addParity :: [Bit] -> [Bit]
addParity xs = xs ++ [(length $ filter (==0) xs) `mod` 2] 


-- (4) Encode string as bytes
encode :: [Char] -> [Bit]
encode s = concat [addParity $ bits2byte $ int2bin $ ord c | c <- s]

-- We can achieve this using composition + map, like this:
encode' :: [Char] -> [Bit]
encode' = concat . map (addParity . bits2byte . int2bin . ord)


-- (5) Decode stream of bits string (by doing stream -> bytes -> chars)
checkParity :: [Bit] -> [Bit]
checkParity xs = if parityBit == expectedParity then byte else error "bad parity bit"
                 where byte = take 8 xs 
                       parityBit = head $ reverse xs
                       expectedParity = (length $ filter (==0) byte) `mod` 2

decode :: [Bit] -> [Char]
decode [] = []
decode bs = (chr $ bin2int $ checkParity $ take 9 bs) : decode (drop 9 bs)

-- We can decompose this into simpler functions
stream2bytes :: [Bit] -> [[Bit]]
stream2bytes [] = []
stream2bytes s = take 9 s : stream2bytes (drop 9 s)

decode' :: [Bit] -> [Char]
decode' stream = map (chr . bin2int . checkParity) (stream2bytes stream)


-- (6) Create a mock transmission channel
channel :: [Bit] -> [Bit]
channel = id


-- (7) Perform a mock transmission of data - with real encoding and decoding at each end
transmit :: [Char] -> [Char]
transmit = decode' . channel . encode'


-- (8) We could now factor in the caesar cypher, so we get something like:
encrypt :: [Char] -> [Char]
encrypt = id -- TODO: caesar cypher encrypt

decrypt :: [Char] -> [Char]
decrypt = id -- TODO: caesar cypher decrypt

transmitEncrypted :: [Char] -> [Char]
transmitEncrypted = decrypt . transmit . encrypt
