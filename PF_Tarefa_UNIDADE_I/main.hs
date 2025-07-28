import Data.Char (toLower, isAlpha)

normalizeChar :: Char -> Char
normalizeChar c
  | c `elem` "áàâã" = 'a'
  | c `elem` "éê"   = 'e'
  | c `elem` "í"     = 'i'
  | c `elem` "óôõ" = 'o'
  | c `elem` "úü"   = 'u'
  | c == 'ç'      = 'c'
  | otherwise     = c

normalizeText :: String -> String
normalizeText text = [normalizeChar (toLower c) | c <- text]

monoAlphaCipherE :: [Char] -> String -> String
monoAlphaCipherE key plainText =
    let
        plainAlphabet = ['a'..'z']
        encryptionMap = zip plainAlphabet key
        readyToEncryptText = normalizeText plainText

        substitute char =
            if isAlpha char
            then case lookup char encryptionMap of
                     Just cipherChar -> cipherChar -- Retorna o caractere do mapa
                     Nothing         -> char       -- Caso improvável, mantém o original
            else char
    
    in [substitute c | c <- readyToEncryptText]


monoAlphaCipherD :: [Char] -> String -> String
monoAlphaCipherD key cipherText =
    let
        plainAlphabet = ['a'..'z']

        -- Para descriptografar, o mapa é invertido: (letra_cifrada, letra_limpa)
        decryptionMap = zip key plainAlphabet

        substitute char =
            if isAlpha char
            then case lookup char decryptionMap of
                     Just plainChar -> plainChar -- Retorna o caractere limpo
                     Nothing        -> char      -- Mantém caracteres não mapeados
            else char

    in [substitute c | c <- cipherText]


main :: IO ()
main = do
    let plainAlphabet = "abcdefghijklmnopqrstuvwxyz"
    let key           = "ZYNGWQAMXPKVULCEFRIBSJDOTH"
    let textToEncrypt = "Criptografia Monoalfabética"
    
    putStrLn $ "Alfabeto Limpo: " ++ plainAlphabet
    putStrLn $ "Chave:            " ++ key
    putStrLn ""
    
    putStrLn $ "Texto Original: " ++ textToEncrypt
    
    let encryptedText = monoAlphaCipherE key textToEncrypt
    putStrLn $ "Texto Cifrado:  " ++ encryptedText
    
    let decryptedText = monoAlphaCipherD key encryptedText
    putStrLn $ "Texto Decifrado: " ++ decryptedText