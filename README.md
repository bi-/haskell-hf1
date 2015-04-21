# haskell-hf1

LZW tömörítés
A feladatok egymásra épülnek, ezért a megadásuk sorrendjében kell ezeket megoldani! A függvények definíciójában lehet, sőt javasolt is alkalmazni a korábban definiált függvényeket.

Az egyes feladatokhoz a tesztesetek logikai értékekből álló listaként adottak. Tekintve, hogy a tesztesetek, bár odafigyelés mellett íródnak, nem fedik le minden esetben a függvény teljes működését, határozottan javasolt még külön próbálgatni a megoldásokat beadás előtt, vagy megkérdezni az oktatókat!

A feladatban az LZW tömörítést fogjuk megvalósítani. A megvalósításhoz szükség lesz a FlexibleContexts kiterjesztésre:

{-# LANGUAGE FlexibleContexts #-}
Az alábbi modulokat, függvényeket érdemes importálni:

import Control.Monad.State
import Control.Monad.Writer
import Data.Char (chr, ord)
import Data.List (isPrefixOf, maximumBy)
Ha a tesztelést Windows alatt, GHCi-ből végezzük, IO kivételt kaphatunk. Ezt elkerülhetjük, ha

nyitunk egy terminált,
kiadjuk a chcp.com 65001 parancsot,
majd kiadjuk a ghci parancsot.
Szótár
Az algoritmus során egy szótárt építünk fel, ez esetünkben String értékek listája lesz. Vezessük be a következő típusszinonimát:

type Dictionary = [String]
Definiáljuk a dictionary konstanst, amely a 0..127 kódú karakterekből képzett 1 hosszú String értékek listája. A karakterkód karakterre való leképezéséhez használhatjuk a chr függvényt.

Típusa:

dictionary :: Dictionary
Tesztesetek:

test_dictionary =
  [ map ord (concat dictionary) == [0..127]
  , all (\str -> length str == 1) dictionary
  ]
Prefixek szótárból
Definiáljuk a prefixes függvényt, amely egy String érték szótárban található prefixeit adja vissza! Az eredmény (i, str) párokból álljon, ahol str a szótár i-edik eleme. A szótár indexelése nullával kezdődik. Annak eldöntésére, hogy egy String prefixe-e egy másiknak, használhatjuk az isPrefixOf függvényt.

Típusa:

prefixes :: String -> Dictionary -> [(Int, String)]
Tesztesetek:

test_prefixes =
  [ prefixes "" dictionary == []
  , prefixes "almafa" []   == []
  , prefixes "almafa" ["al", "alma", "fa", "korte"] == [(0, "al"), (1, "alma")]
  , prefixes "barack" dictionary                    == [(98, "b")]
  ]
Leghosszabb prefix
Adott index-prefix párok listája. Definiáljuk a longest függvényt, amely a lista leghosszabb prefixét tartalmazó párt adja vissza! A maximumkereséshez használhatjuk a maxmimumBy, az összehasonlításhoz a compare függvényt. Ha több azonos hosszú prefix is van a listában, az utolsót adjuk vissza -- ez megfelel a maximumBy függvény viselkedésének. Üres lista esetén nem kell működnie a függvénynek.

Típusa:

longest :: [(Int, String)] -> (Int, String)
Tesztesetek:

test_longest =
  [ longest [(30, "a"), (20, "abc"), (15, "ab")]  == (20, "abc")
  , longest [(30, "a"), (20, "abc"), (15, "abc")] == (15, "abc")
  ]
Leghosszabb prefix leharapása szóból
Tekintsük a State monádot a Dictionary típusparaméterrel, mint állapottal! Definiáljuk a munch monadikus függvényt, amely:

Lekérdezi az aktuális szótárt mint állapotot.
Kiszámítja a paraméterként kapott szó szótárbeli prefixei közül a leghosszabbat, és ennek indexét a szótárban.
Ezek alapján egy rendezett hármast ad vissza: az indexet, a prefixet, és a paraméterként kapott szó első n karakterének elhagyásával kapott szöveg, ha n a prefix hossza.
A függvényt valójában ne a State monáddal definiáljuk, hanem tetszőleges olyan monáddal, amely rendelkezik a State funkcionalitásával -- azaz a MonadState típusosztály példányaival.

Típusa:

munch :: MonadState Dictionary m => String -> m (Int, String, String)
Tesztesetek:

test_munch =
  [ evalState (munch "a")      ["a"]            == (0, "a", "")
  , evalState (munch "almafa") ["a"]            == (0, "a", "lmafa")
  , evalState (munch "barack") ["a", "ba", "b"] == (1, "ba", "rack")
  ]
Hozzáfűzés a szótárhoz
Definiáljuk az append monadikus függvényt, amely egy szó (1. paraméter) és egy feldolgozatlan szöveg (2. paraméter) alapján kiegészíti a szótárt!

Ha a feldolgozatlan szöveg üres, ne csináljunk semmit!
Ha nem üres, akkor:
Kérdezzük le a szótárt az állapotból!
Számítsuk ki az új szót: fűzzük a szó végére a feldolgozatlan szöveg első karakterét!
Ha ez az új szó nincs benne a szótárban, cseréljük le az állapotban tárolt szótárt: szúrjuk be a végére az új szót!
A "nincs benne a szótárban" feltétel kényelmes ellenőrzéséhez használhatjuk a notElem függvényt.

Típusa:

append :: MonadState Dictionary m => String -> String -> m ()
Tesztesetek:

test_append =
  [ execState (append "a" "")   []         == []
  , execState (append "a" "")   dictionary == dictionary
  , execState (append "a" "bc") []         == ["ab"]
  , execState (append "a" "bc") ["ab"]     == ["ab"]
  ]
Szó kódolása
A szótár tárolására a State monádot használtuk. A kódolás során a kimenetet egy listába fogjuk gyűjteni, erre pedig a Writer monádot fogjuk használni. Mivel mindkét monádra szükségünk van, a State típusra építjük a WriterT monádtranszformátort.

Definiáljuk az encode függvényt, amely

üres paraméter esetén nem csinál semmit,
egyébként pedig:
Lekérdezi a State monádból a szótárt.
A paraméterből kiharapja a leghosszabb szótárbeli prefixet -- ebből származik egy index, a prefix és a szó maradék része.
Ha a szótár hossza kisebb, mint 256, végrehajtja a hozzáfűzést a prefix és a maradék szó alapján.
Az indexből álló 1 hosszú listát mint kódot eltárolja a Write monádban.
Rekurzívan elkódolja a szó maradék részét.
Mivel a munch és append függvényeket polimorf módon, a MonadState típusosztály használatával írtuk meg, az encode függvény megvalósításakor nincs szükség a lift függvény használatára.

Típusa:

encode :: String -> WriterT [Int] (State Dictionary) ()
Tesztesetek:

test_encode =
  [ evalState (execWriterT (encode ""))         []         == []
  , evalState (execWriterT (encode "aaa"))      ["a"]      == [0, 1]
  , evalState (execWriterT (encode "aaaa"))     ["a"]      == [0, 1, 0]
  , evalState (execWriterT (encode "aaaaa"))    ["a"]      == [0, 1, 1]
  , evalState (execWriterT (encode "abababab")) ["a", "b"] == [0, 1, 2, 4, 1]
  , evalState (execWriterT (encode "aaabbbccc")) dictionary
    == [97, 128, 98, 130, 99, 132]
  ]
Szó dekódolása
A dekódolás fordítva fog működni: a visszafejtendő [Int] érték paraméter, a WriterT monád típusparamétere pedig a String, amelybe gyűjtögetjük a dekódolt szöveget. A kódoláshoz hasonlóan a dekódolás során is felépítjük a szótárt, a művelet a kódoláskor kapott szótár ismerete nélkül is elvégezhető.

Definiáljuk a decode függvényt, amely

üres lista esetén nem csinál semmit,
egyelemű lista esetén lekérdezi a szótárt, majd beteszi a Writer monádba a szótár i-edik elemét, ahol i a lista eleme,
legalább kételemű lista esetén pedig:
Lekérdezi a szótárt.
Kiszámítja a dekódolt szövegrészt: a szótár i-edik elemét, ahol i a lista fejeleme.
Kiszámítja a dekódolt szövegrész folytatását: a szótár j-edik elemét, ahol j a lista második eleme, ha van a szótárnak ennyi eleme; egyébként a szótár i-edik elemét.
Elvégzi a szótárhoz hozzáfűzést a dekódolt szövegrész és ennek folytatása alapján.
Beteszi a Writer monádba a dekódolt szövegrészt.
Rekurzívan dekódolja a lista fejelem nélküli részét. (Tehát a második, j-vel jelölt elemet hagyjuk bent a listában!)
Típusa:

decode :: [Int] -> WriterT String (State Dictionary) ()
Tesztesetek:

test_decode =
  [ evalState (execWriterT (decode []))           []         == []
  , evalState (execWriterT (decode [0]))          ["a"]      == "a"
  , evalState (execWriterT (decode [0, 1, 1, 0])) ["a", "b"] == "abba"
  , evalState (execWriterT (decode [0, 1, 2, 0])) ["a", "b"] == "ababa"
  , evalState (execWriterT (decode [97, 128, 98, 130, 99, 132])) dictionary
    == "aaabbbccc"
  ]
Szó tömörítése
Definiáljuk a compress függvényt, amely elvégzi egy szó kódolását a dictionary kezdőszótárral, és a Writer monádba gyűjtögetett kódot adja vissza!

Típusa:

compress :: String -> [Int]
Tesztesetek:

test_compress =
  [ compress ""          == []
  , compress "a"         == [97]
  , compress "aaa"       == [97, 128]
  , compress "aaabbbccc" == [97, 128, 98, 130, 99, 132]
  ]
Szó kitömörítése
Definiáljuk a decompress függvényt, amely elvégzi a dekódolást a dictionary kezdőszótárral, és a Writer monádba gyűjtögetett szöveget adja vissza!

Típusa:

decompress :: [Int] -> String
Tesztesetek:

test_decompress =
  [ decompress []                          == ""
  , decompress [97]                        == "a"
  , decompress [97, 128]                   == "aaa"
  , decompress [97, 128, 98, 130, 99, 132] == "aaabbbccc"
  ]
Tulajdonság alapú tesztelés
Definiáljuk a prop_compressDecompress tulajdonságot, amely egy String értéket transzformál, majd ellenőrzi, hogy ha alkalmazzuk rá először a compress, majd a decompress függvényt, ugyanezt az értéket kaptuk-e vissza! A transzformáció a következő: a String minden karakterét képezzük le a kódjára, osszuk el kettővel, majd képezzük vissza karakterré! Erre azért van szükség, mert a 128 és 255 közti karakterkódok nem szerepelnek a kezdeti szótárban, ezekre nem működnek a függvényeink. Az egészosztáshoz használjuk a div függvényt!

Típusa:

prop_compressDecompress :: String -> Bool
A teszteléshez használhatjuk a Test.QuickCheck modul quickCheck, verboseCheck függvényeit.

Fájl tömörítése
Definiáljuk a compressFile függvényt, amely beolvassa a bemeneti fájlt (1. paraméter), majd tömöríti, a kódot leképezi karakterekre, majd kiírja a kimeneti fájlba (2. paraméter)! A fájl beolvasásához a readFile, kiírásához a writeFile függvényt használhatjuk.

Típusa:

compressFile :: FilePath -> FilePath -> IO ()
Fájl kitömörítése
Definiáljuk a decompressFile függvényt, amely beolvassa a bemeneti fájlt (1. paraméter) és leképezi karakterkódokra, kitömöríti és kiírja a kimeneti fájlba (2. paraméter)!

Típusa:

decompressFile :: FilePath -> FilePath -> IO ()
