import Control.Monad (mapM_)

main = do
  let numbers = map show [0..9]
                ++ ["00", "01", "02", "03", "04", "05", "06", "07", "08", "09"]
                ++ map show [10..99]
  putStrLn "<dl>"
  mapM_ (\(n, w) -> putStrLn $ "  <dt>" ++ n ++ "</dt><dd>" ++ w ++ "</dd>") $ map (\n -> (n, word n)) numbers
  putStrLn "</dl>"


word :: String -> String
word "0"  = "hai"
word "1"  = "jää"
word "2"  = "kuu"
word "3"  = "luu"
word "4"  = "maa"
word "5"  = "puu"
word "6"  = "rae"
word "7"  = "suu"
word "8"  = "täi"
word "9"  = "voi"
word "00" = "hiha"
word "01" = "haju"
word "02" = "hauki"
word "03" = "huilu"
word "04" = "hame"
word "05" = "huopa"
word "06" = "hiiri"
word "07" = "hiisi"
word "08" = "hauta"
word "09" = "haavi"
word "10" = "jauho"
word "11" = "jojo"
word "12" = "joki"
word "13" = "joulu"
word "14" = "juomu"
word "15" = "jopo"
word "16" = "juuri"
word "17" = "jousi"
word "18" = "jeti"
word "19" = "jyvä"
word "20" = "koho"
word "21" = "koju"
word "22" = "keko"
word "23" = "kela"
word "24" = "kuomu"
word "25" = "kupu"
word "26" = "koira"
word "27" = "kusi"
word "28" = "kota"
word "29" = "kavio"
word "30" = "liha"
word "31" = "leija"
word "32" = "laki"
word "33" = "luola"
word "34" = "luumu"
word "35" = "lapio"
word "36" = "liero"
word "37" = "liesi"
word "38" = "luoti"
word "39" = "laiva"
word "40" = "maha"
word "41" = "maja"
word "42" = "muki"
word "43" = "mela"
word "44" = "muumi"
word "45" = "mopo"
word "46" = "muuri"
word "47" = "muusi"
word "48" = "maito"
word "49" = "muovi"
word "50" = "piuha"
word "51" = "paja"
word "52" = "puku"
word "53" = "pulu"
word "54" = "puuma"
word "55" = "pipo"
word "56" = "pora"
word "57" = "pusu"
word "58" = "pata"
word "59" = "paavi"
word "60" = "raha"
word "61" = "raja"
word "62" = "reki"
word "63" = "railo"
word "64" = "riimu"
word "65" = "rapu"
word "66" = "ruori"
word "67" = "ruusu"
word "68" = "ruoto"
word "69" = "rovio"
word "70" = "saha"
word "71" = "soija"
word "72" = "sika"
word "73" = "siili"
word "74" = "siima"
word "75" = "siipi"
word "76" = "saari"
word "77" = "susi"
word "78" = "suti"
word "79" = "sauva"
word "80" = "tuohi"
word "81" = "taija"
word "82" = "tiuku"
word "83" = "tuoli"
word "84" = "tiimi"
word "85" = "tipu"
word "86" = "tiira"
word "87" = "toosa"
word "88" = "tieto"
word "89" = "tavi"
word "90" = "vuohi"
word "91" = "vaja"
word "92" = "vaaka"
word "93" = "viulu"
word "94" = "vaimo"
word "95" = "vapa"
word "96" = "vuori"
word "97" = "vaasi"
word "98" = "vouti"
word "99" = "vauva"
word _ = undefined 

candidateWords :: String -> [String]
candidateWords n = [[firstConsonant, vowel1, secondConsonant, vowel2] | vowel1 <- "aeiou", vowel2 <- "aeiou"] ++ [[firstConsonant, vowel1, vowel2, secondConsonant, vowel3] | vowel1 <- "aeiou", vowel2 <- "aeiou", vowel3 <- "aeiou"]
  where consonants = letters n
        firstConsonant = head consonants
        secondConsonant = head (tail consonants)

letters :: [Char] -> [Char]
letters digits = map convert digits

convert :: Char -> Char
convert '0' = 'h'
convert '1' = 'j'
convert '2' = 'k'
convert '3' = 'l'
convert '4' = 'm'
convert '5' = 'p'
convert '6' = 'r'
convert '7' = 's'
convert '8' = 't'
convert '9' = 'v'
convert _   = undefined

