-- Definição de um novo tipo de dado que representa a estrutura de uma expressão aritmética
data Expressao = Numero Int
               | Variavel String
               | Adicao Expressao Expressao
               | Subtracao Expressao Expressao
               | Multiplicacao Expressao Expressao 
               | Divisao Expressao Expressao
               | Exponenciacao Expressao Expressao
               deriving (Show)

-- Define um sinônimo de tipo para a "memória" de variáveis
type Memoria = [(String, Int)]

-- Função principal que traduz uma expressão para um programa Python
traduzExpressao :: Memoria -> Expressao -> String
traduzExpressao memoria expressao =
    -- Traduz as variáveis da memória para atribuições em Python
    let atribuicoes = map (\(var, val) -> var ++ " = " ++ show val) memoria
    -- Constrói a string final do programa
    in unlines atribuicoes ++ "\n" ++ "resultado = " ++ traduz expressao ++ "\n" ++ "print(resultado)"

-- Função auxiliar recursiva que traduz cada parte da expressão
traduz :: Expressao -> String
traduz (Numero n) = show n
traduz (Variavel s) = s
traduz (Adicao e1 e2) = "(" ++ traduz e1 ++ " + " ++ traduz e2 ++ ")"
traduz (Subtracao e1 e2) = "(" ++ traduz e1 ++ " - " ++ traduz e2 ++ ")"
traduz (Multiplicacao e1 e2) = "(" ++ traduz e1 ++ " * " ++ traduz e2 ++ ")"
traduz (Divisao e1 e2) = "(" ++ traduz e1 ++ " / " ++ traduz e2 ++ ")"
traduz (Exponenciacao e1 e2) = "(" ++ traduz e1 ++ " ** " ++ traduz e2 ++ ")"

-- Exemplo hardcoded de uso
main :: IO ()
main = do
    let memoria = [("x", 10), ("y", 3)]
    let expressao = Multiplicacao (Adicao (Variavel "x") (Numero 5)) (Exponenciacao (Variavel "y") (Numero 2))
    let programaPython = traduzExpressao memoria expressao
    putStrLn "--- Programa Python Gerado ---"
    putStrLn programaPython
    putStrLn "--- Fim do Programa ---"