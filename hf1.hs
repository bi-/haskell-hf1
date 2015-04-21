{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.State
import Control.Monad.Writer
import Data.Char (chr, ord)
import Data.List (isPrefixOf, maximumBy)

type Dictionary = [String]

dictionary :: Dictionary

dictionary = [ [chr x] | x <- [0..127] ]

test_dictionary =
  [ map ord (concat dictionary) == [0..127]
  , all (\str -> length str == 1) dictionary
  ]

prefixes :: String -> Dictionary -> [(Int, String)]
prefixes s d = [ t | t@(_,x) <- zip [0..] d, x `isPrefixOf` s]

test_prefixes =
  [ prefixes "" dictionary == []
  , prefixes "almafa" []   == []
  , prefixes "almafa" ["al", "alma", "fa", "korte"] == [(0, "al"), (1, "alma")]
  , prefixes "barack" dictionary                    == [(98, "b")]
  ]

longest :: [(Int, String)] -> (Int, String)
longest x = maximumBy (\a b -> compare (length $ snd a) (length $ snd b)) x

test_longest =
  [ longest [(30, "a"), (20, "abc"), (15, "ab")]  == (20, "abc")
  , longest [(30, "a"), (20, "abc"), (15, "abc")] == (15, "abc")
  ]

munch :: MonadState Dictionary m => String -> m (Int, String, String)
munch x = do
    d <- get
    let (i,v) = longest $ prefixes x d
    return (i, v, drop (length v) x)

test_munch =
  [ evalState (munch "a")      ["a"]            == (0, "a", "")
  , evalState (munch "almafa") ["a"]            == (0, "a", "lmafa")
  , evalState (munch "barack") ["a", "ba", "b"] == (1, "ba", "rack")
  ]

append :: MonadState Dictionary m => String -> String -> m ()
append _ []  = return ()

append x (y:ys) = do
    d <- get
    let z = x ++ [y]
    when (z `notElem` d) $ put (d ++ [z])

test_append =
  [ execState (append "a" "")   []         == []
  , execState (append "a" "")   dictionary == dictionary
  , execState (append "a" "bc") []         == ["ab"]
  , execState (append "a" "bc") ["ab"]     == ["ab"]
  ]

encode :: String -> WriterT [Int] (State Dictionary) ()
encode [] = return () 

encode x = do
  d <- get
  (i, p, r) <- munch x
  when ( length (d) < 256) $ append p r  
  tell ([i])
  encode r


test_encode =
  [ evalState (execWriterT (encode ""))         []         == []
  , evalState (execWriterT (encode "aaa"))      ["a"]      == [0, 1]
  , evalState (execWriterT (encode "aaaa"))     ["a"]      == [0, 1, 0]
  , evalState (execWriterT (encode "aaaaa"))    ["a"]      == [0, 1, 1]
  , evalState (execWriterT (encode "abababab")) ["a", "b"] == [0, 1, 2, 4, 1]
  , evalState (execWriterT (encode "aaabbbccc")) dictionary
    == [97, 128, 98, 130, 99, 132]
  ]

decode :: [Int] -> WriterT String (State Dictionary) ()

decode [] = return ()

decode (x:[]) = do
  d <- get
  let i = d!!x
  tell (i)    

decode (x:y:xs) = do
  d <- get
  let i = d!!x
  let j = if length d > y then d!!y else i
  append i j 
  tell(i)
  decode (y:xs)

test_decode =
  [ evalState (execWriterT (decode []))           []         == []
  , evalState (execWriterT (decode [0]))          ["a"]      == "a"
  , evalState (execWriterT (decode [0, 1, 1, 0])) ["a", "b"] == "abba"
  , evalState (execWriterT (decode [0, 1, 2, 0])) ["a", "b"] == "ababa"
  , evalState (execWriterT (decode [97, 128, 98, 130, 99, 132])) dictionary
    == "aaabbbccc"
  ]

compress :: String -> [Int]
compress x = evalState (execWriterT (encode x)) dictionary  

test_compress =
  [ compress ""          == []
  , compress "a"         == [97]
  , compress "aaa"       == [97, 128]
  , compress "aaabbbccc" == [97, 128, 98, 130, 99, 132]
  ]

decompress :: [Int] -> String
decompress x = evalState (execWriterT (decode x)) dictionary

test_decompress =
  [ decompress []                          == ""
  , decompress [97]                        == "a"
  , decompress [97, 128]                   == "aaa"
  , decompress [97, 128, 98, 130, 99, 132] == "aaabbbccc"
  ]

prop_compressDecompress :: String -> Bool
prop_compressDecompress x = transform x  == (decompress $ compress $ transform x)

transform :: String -> String
transform x = map chr [ ord(a) `div` 2  | a <- x]

compressFile :: FilePath -> FilePath -> IO () 
compressFile i o = do
  t <- readFile i
  writeFile o $ [chr a | a <- compress t]

decompressFile :: FilePath -> FilePath -> IO ()
decompressFile i o = do
  t <- readFile i
  writeFile o $ decompress [ord a | a <- t]
