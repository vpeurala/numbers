import Control.Monad (mapM_)

prefix :: String
prefix = unlines [
  "<!DOCTYPE html>",
  "<html>",
  "  <head>",
  "    <title>Kortit</title>",
  "    <meta charset='utf-8' />",
  "    <style type='text/css'>",
  "      body {",
  "        font-family: 'Arial';",
  "        font-size: 2em;",
  "      }",
  "      dt {",
  "        float: left;",
  "      }",
  "      dd {",
  "        padding-left: 3em;",
  "        padding-bottom: 1em;",
  "      }",
  "    </style>",
  "  </head>",
  "  <body>",
  "    <dl>" ]

postfix :: String
postfix = unlines [
  "    </dl>",
  "  </body>",
  "</html>" ]

main = do
  putStr prefix
  let kortit = [Kortti maa arvo | maa <- [Pata, Hertta, Ruutu, Risti], arvo <- [1..13]]
  mapM_ (\k -> putStrLn $ "      <dt>" ++ show k ++ "</dt><dd>" ++ naama k ++ "</dd>") kortit
  putStr postfix

data Maa = Pata | Hertta | Ruutu | Risti

instance Show Maa where
  show Pata = "\9824"
  show Hertta = "\9825"
  show Ruutu = "\9826"
  show Risti = "\9827"

type Arvo = Int

data Kortti = Kortti Maa Arvo

instance Show Kortti where
  show (Kortti maa arvo) = show maa ++ show arvo

naama :: Kortti -> String
-- Pata
naama (Kortti Pata 1)  = "Tuire Huhtamäki"
naama (Kortti Pata 2)  = "Tapani Levanto"
naama (Kortti Pata 3)  = "Aino Levanto"
naama (Kortti Pata 4)  = "Kaisa Heikkerö"
naama (Kortti Pata 5)  = "Eeva Heikkerö"
naama (Kortti Pata 6)  = "Mikko Heikkerö"
naama (Kortti Pata 7)  = "Eeva Levanto"
naama (Kortti Pata 8)  = "Jouni Levanto"
naama (Kortti Pata 9)  = "Maija Koivuluoma"
naama (Kortti Pata 10) = "Paavo Koivuluoma"
naama (Kortti Pata 11) = "Jussi Peurala"
naama (Kortti Pata 12) = "Elina Peurala"
naama (Kortti Pata 13) = "Antti Peurala"
-- Hertta
naama (Kortti Hertta 1)  = "Olli Savo"
naama (Kortti Hertta 2)  = "Henri Haapanen"
naama (Kortti Hertta 3)  = "Kimmo Kari"
naama (Kortti Hertta 4)  = "Olli Joukio"
naama (Kortti Hertta 5)  = "Ilkka Kaartinen"
naama (Kortti Hertta 6)  = "Tomppa Harju"
naama (Kortti Hertta 7)  = "Tapio Ahokas"
naama (Kortti Hertta 8)  = "Aki Harjuhaahto"
naama (Kortti Hertta 9)  = "Mikko Järvinen"
naama (Kortti Hertta 10) = "Otso Alho"
naama (Kortti Hertta 11) = "Niko Hyttinen"
naama (Kortti Hertta 12) = "Jukka Mantere"
naama (Kortti Hertta 13) = "Toni Osolanus"
-- Ruutu
naama (Kortti Ruutu 1)  = "Joni Freeman"
naama (Kortti Ruutu 2)  = "Pekka Enberg"
naama (Kortti Ruutu 3)  = "Sami Honkonen"
naama (Kortti Ruutu 4)  = "Jari Aarniala"
naama (Kortti Ruutu 5)  = "Samuli Karjula"
naama (Kortti Ruutu 6)  = "Janne Hietamäki"
naama (Kortti Ruutu 7)  = "Timo Puronen"
naama (Kortti Ruutu 8)  = "Tiina Kiuru"
naama (Kortti Ruutu 9)  = "Timo Rantalaiho"
naama (Kortti Ruutu 10) = "Tuomas Kärkkäinen"
naama (Kortti Ruutu 11) = "Marko Sibakov"
naama (Kortti Ruutu 12) = "Markus Hjort"
naama (Kortti Ruutu 13) = "Hannu Terävä"
-- Risti
naama (Kortti Risti 1)  = "Lauri Korkeaoja"
naama (Kortti Risti 2)  = "Henrik Tähtivuori"
naama (Kortti Risti 3)  = "Tuomas Vaittinen"
naama (Kortti Risti 4)  = "Tuomo Torkkola"
naama (Kortti Risti 5)  = "Otto Jaakonsaari"
naama (Kortti Risti 6)  = "Timo Huhtala"
naama (Kortti Risti 7)  = "Matti Hirvola"
naama (Kortti Risti 8)  = "Juha Niemi"
naama (Kortti Risti 9)  = "Tomi Wallenius"
naama (Kortti Risti 10) = "Esa Edvik"
naama (Kortti Risti 11) = "Jussi Autere"
naama (Kortti Risti 12) = "Jyrki Laiho"
naama (Kortti Risti 13) = "Jukka Purma"
-- Tyhjä
naama _                 = undefined

