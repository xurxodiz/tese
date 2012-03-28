module EasterEggs where
  
import Maybe
import Codec.Binary.UTF8.String
  
table = [
          ("que queres",
              ["Eu o que quero é xamón xamón!"]),
          ("when did Lars_Ulrich die",
              ["Well, he should've died in that accident..."]),
          ("dónde estará mi carro",
              ["No pasó la ITV y lo mandé al taller."])
        ]
  
easterEgg :: String -> [String]
easterEgg s = map encodeString (fromMaybe [] $ lookup ss table)
              where ss = decodeString s