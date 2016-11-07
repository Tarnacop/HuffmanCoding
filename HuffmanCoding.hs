module HuffmanCoding where

import Data.Char
import Data.List

{- Huffman Codes -}

data Tree c = Leaf c Int | Branch (Tree c) (Tree c) Int
    deriving (Show, Eq, Ord, Read)

data Bit = Z | I
    deriving (Eq, Ord)

instance Show Bit where
    show Z = "0"
    show I = "1"
    showList [] = id
    showList (x:xs) = \out -> (show x) ++ showList xs out

{--- Decoding ---}
-- Notice that this should work for types more general than Char (our c).
-- Question:
decode :: Eq c => (Tree c, [Bit]) -> [c]
decode (root, bits) = decodeAux root root bits

-- You may or may not wish to use a helper function as follows for
-- decode (this function will not be marked, and you can leave it
-- undefined if you don't use it):
decodeAux :: Eq c => Tree c -> Tree c -> [Bit] -> [c]
decodeAux (Leaf c i) root [] = [c]
decodeAux (Leaf c i) root bits = c:(decodeAux root root bits)
decodeAux (Branch lt rt i) root [] = []
decodeAux (Branch lt rt i) root (b:bits) | b == Z = (decodeAux lt root bits)
                                         | b == I = (decodeAux rt root bits)


{-- decompression --}

{- The input String has the following format:

   * An integer n coded as a sequence of digits.

   * This is followed by exact n characters, have a tree write with
     show, that can be read with read.

   * A sequence of 0's and 1's (characters) representing a sequence of bits.

   The output should be some text.

-}

------------------- Aux functions -----------------------------------------

-- Retrieve only the string containing the number
takeNumberAux :: String -> String
takeNumberAux [] = []
takeNumberAux (c:s) | isDigit c = c:(takeNumberAux s)
                    | otherwise = []

-- Count the digits of the number give in the string
countDigits :: String -> Int
countDigits s = length (takeNumberAux s)

-- Take the number from the string as an int
takeNumber :: String -> Int
takeNumber s = read ns where ns = takeNumberAux s

-- Retrive only the string containing the tree
takeTreeAux :: String -> String
takeTreeAux s = take (takeNumber s) (drop (countDigits s) s)

-- Take the tree from the string as a tree
takeTree :: String -> Tree Char
takeTree s = read ts where ts = takeTreeAux s

-- Retrieve only the string containing the bit list
takeBitsAux :: String -> String
takeBitsAux s = drop ((countDigits s) + (takeNumber s)) s

-- Read the bit string and convert it into bit list
bitsRead :: String -> [Bit]
bitsRead [] = []
bitsRead (c:s) | c == '0' = Z:(bitsRead s)
               | c == '1' = I:(bitsRead s)

-- Take the bits from the string as a bit list
takeBits :: String -> [Bit]
takeBits s = bitsRead bits where bits = takeBitsAux s

----------------------------------------------------------------------------

decompress :: String -> String
decompress s = decode (takeTree s, takeBits s)

{--- Decompression for a smarter compression algorithm: For a short
string or a random string, the Huffman code of the string is longer
than the string. In this case, we produce the original string with a '*'
at the front, indicating that no compression was performed.

However, we need to simulate this using `charlength`, since we're
outputting a bitsequence as characters.  charlength is the bit-length
of a single character. We could change this to simulate a different
character encoding.  ---}

charlength :: Int
charlength = 8

--gives the length in "bits" of a string
memSize :: String -> Int
memSize s = 8 * (length s)

-- Smarter decompression, as discussed above. The input is either *
-- followed by a string, or as in the original decompression function:
decompress' :: String -> String
decompress' (c:s) | c == '*' = s
                  | otherwise = decompress (c:s)

{--- Generate the frequency table ---}
--An element of the type Freq is a symbol together with its frequency.
type Freq c = (c,Int)

leaf :: Freq c -> Tree c
leaf (c,i) = Leaf c i

freq :: Tree c -> Int
freq (Leaf _ i) = i
freq (Branch _ _ i) = i

------------------------------ Aux functions -------------------------------
countElement :: Eq c => c -> [c] -> Int
countElement _ [] = 0
countElement e (x:xs) | e == x = 1 + (countElement e xs)
                      | otherwise = countElement e xs

-- Delete all the entries of a given element in a list
deleteElement :: Eq c => c -> [c] -> [c]
deleteElement _ [] = []
deleteElement e (x:xs) | e == x = deleteElement e xs
                       | otherwise = x:(deleteElement e xs)

----------------------------------------------------------------------------
--Generates a frequency table.
tabulate :: Eq c => [c] -> [Freq c]
tabulate [] = []
tabulate (x:xs) = (x, (countElement x (x:xs))):(tabulate (deleteElement x xs))

-- Produce a Huffman tree from a list of Huffman trees.
-- https://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html
-- Question:
makeTree :: [Tree c] -> Tree c
-- Collects a list of trees into an optimal prefix tree.
makeTree t = makeTree' (sortBy sortGuard t)

-- You may wish to use a helper function such as this:
merge :: Tree c -> Tree c -> Tree c
merge firstTree secondTree = Branch firstTree secondTree (freq firstTree + freq secondTree)

-------------- OTHER AUX FUNCTIONS ---------------------------------------
tabulateToTree :: [Freq c] -> [Tree c]
tabulateToTree [] = []
tabulateToTree (x:xs) = (leaf x):(tabulateToTree xs)

sortGuard :: Tree c -> Tree c -> Ordering
sortGuard x y | freq x > freq y = GT
              | otherwise = LT

makeTree' :: [Tree c] -> Tree c
makeTree' [t] = t
makeTree' (t:tt:ts) = makeTree' newTs
                    where newTs = (insertBy sortGuard (merge t tt) ts)
--------------------------------------------------------------------------


-- Question:
-- Generate a tree from list of Freqs (using makeTree above):
generateTree :: [Freq c] -> Tree c
generateTree xs = makeTree (tabulateToTree xs)

-- Encoding table.
-- A key is a key-value pair (an entry in a map/table).
type Key c = (c,[Bit])

-- The whole coding table
type CodingTable c = [Key c]

-- Question:
-- Given a tree, generates a coding table
makeTable :: Eq c => Tree c -> CodingTable c
makeTable t = makeTable' t []

---------- AUX FUNCTION ------------------------------------------------------
makeTable' :: Eq c => Tree c -> [Bit] -> CodingTable c
makeTable' (Leaf c w) bits = [(c, reverse bits)]
makeTable' (Branch lt rt _) bits = (makeTable' lt (Z:bits)) ++ (makeTable' rt (I:bits))
------------------------------------------------------------------------------

-- Question:
-- Takes a string of symbols to a bit string, based on a given coding table
encodeUsingTable :: Eq c => CodingTable c -> [c] -> [Bit]
encodeUsingTable ct [] = []
encodeUsingTable ct (c:cs) = (snd ctElement) ++ (encodeUsingTable ct cs)
                             where ctElement = (findElementInTable c ct)

------------------- AUX FUNCTION ----------------------------------------------
findElementInTable :: Eq c => c -> CodingTable c -> Key c
findElementInTable element (c:ct) | element == (fst c) = c
                                  | otherwise = findElementInTable element ct
-------------------------------------------------------------------------------

-- Question:
-- Encodes directly from the tree (more efficient).
encodeUsing :: Eq c => Tree c -> [c] -> [Bit]
encodeUsing tree [] = []
encodeUsing tree (c:cs) = (getBits tree c []) ++ (encodeUsing tree cs)

--------------------------- AUX FUNCTION --------------------------------------
getBits :: Eq c => Tree c -> c -> [Bit] -> [Bit]
getBits (Leaf c w) element bits | c == element = reverse bits
                                | otherwise = []
getBits (Branch lt rt _) element bits = (getBits lt element (Z:bits)) ++ (getBits rt element (I:bits))
-------------------------------------------------------------------------------

-- Question:
-- From a string of symbols, generate the coding tree and the encoding
encode :: Eq c => [c] -> (Tree c, [Bit])
encode cs = (tree, bits)
          where tree = generateTree (tabulate cs)
                bits = encodeUsing tree cs

-- Encoding trees

-- Question:
-- Compressing a string. This should be the inverse of decompress.
-- That is, this should output a string of the form
--
-- n ++ t ++ c
--
-- Where,
--    * n is a read from an integer
--    * t is read from a tree, and contains exactly n characters.
--    * c is string of bits.
compress :: String -> String
compress s = (show n) ++ (show t) ++ (show c)
           where t = generateTree (tabulate s)
                 n = length (show t)
                 c = encodeUsing (generateTree (tabulate s)) s

-- Question:
-- Smarter compression: if the encoded string is larger than the input string,
-- instead output the input string with a '*' in front.
compress' :: String -> String
compress' s | ((memSize (takeCharPart compressedString)) + (length (takeBitsAux compressedString))) > (memSize s) = ('*':s)
            | otherwise = compressedString
            where compressedString = compress s

-------------------------------- AUX FUNCTIONS ---------------------------------
takeCharPart :: String -> String
takeCharPart s = take (intLength + treeLength) s
               where intLength = countDigits s
                     treeLength = length (takeTreeAux s)
--------------------------------------------------------------------------------
