import Control.Monad.IO.Class (liftIO)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import GHC.Generics (Generic)
import System.Console.GetOpt (ArgDescr (NoArg))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Cliente = Cliente
  { clienteid :: Maybe Int,
    nome :: Text,
    telefone :: Text
  }
  deriving (Show, Generic)

data Servico = Servico
  { servicoid :: Maybe Int,
    cliente_id :: Int,
    descricao :: Text,
    valor :: Double,
    data_servico :: Text
  }
  deriving (Show, Generic)

calculovalortotal :: Int -> [Servico] -> Double
calculovalortotal id servicos = sum [valor s | s <- servicos, cliente_id s == id] -- Calcular o valor total dos servicos de um cliente

contarservicosporcliente :: Int -> [Servico] -> Int
contarservicosporcliente id servicos = length [s | s <- servicos, cliente_id s == id]

quantidadeporcliente :: [Cliente] -> [Servico] -> [(Cliente, Int, Double)]
quantidadeporcliente clientes servicos = [(c, contarservicosporcliente (fromMaybe 0 (clienteid c)) servicos, calculovalortotal (fromMaybe 0 (clienteid c)) servicos) | c <- clientes]

clientecommaisservicos :: [(Cliente, Int, Double)] -> Maybe (Cliente, Int)
clientecommaisservicos [] = Nothing
clientecommaisservicos lista = Just $ let (c, n, _) = maximumBy (comparing (\(_, n, _) -> n)) lista in (c, n)

-- Função para ver o total faturado
calculafaturamentototal :: [Servico] -> Double
calculafaturamentototal servico = sum [valor s | s <- servico]

-- Cliente que mais gastou

clientequemaisgastou :: [(Cliente, Int, Double)] -> Maybe (Cliente, Double)
clientequemaisgastou [] = Nothing
clientequemaisgastou lista = let (c, _, total) = maximumBy (comparing (\(_, _, t) -> t)) lista in Just (c, total)

-- Serviço mais caro

servicomaiscaro :: [Servico] -> Maybe Servico
servicomaiscaro [] = Nothing
servicomaiscaro servico = Just (maximumBy (comparing valor) servico)

-- Criação das variaveis

cliente1 = Cliente (Just 1) (TL.pack "Fulano") (TL.pack "1134-1123")
cliente2 = Cliente (Just 2) (TL.pack "Ciclano") (TL.pack "2234-2234")
cliente3 = Cliente (Just 3) (TL.pack "Beltrano") (TL.pack "3234-3123")
clienteteste = [cliente1, cliente2, cliente3]

servico1 = Servico (Just 1) 1 (TL.pack "Serviço1") 100 (TL.pack "2025-09-28")
servico2 = Servico (Just 2) 1 (TL.pack "Serviço 2") 200 (TL.pack "2025-09-28")
servico3 = Servico (Just 3) 2 (TL.pack "Serviço 3") 300 (TL.pack "2025-09-28")
servico4 = Servico (Just 4) 3 (TL.pack "Serviço 4") 400 (TL.pack "2025-09-28")
servico5 = Servico (Just 5) 1 (TL.pack "Serviço 5") 500 (TL.pack "2025-09-28")
servicoteste = [servico1, servico2, servico3, servico4, servico5]

main :: IO ()
main = do
  putStrLn "Valor total do cliente 1:"
  print  (calculovalortotal 1 servicoteste)

  putStrLn "\nQuantidade de serviços do cliente 1:"
  print  (contarservicosporcliente 1 servicoteste)

  putStrLn "\nQuantidade e valor total por cliente:"
  print  (quantidadeporcliente clienteteste servicoteste)

  putStrLn "\nCliente com mais serviços:"
  print  (clientecommaisservicos (quantidadeporcliente clienteteste servicoteste))

  putStrLn "\nCliente que mais gastou:"
  print  (clientequemaisgastou (quantidadeporcliente clienteteste servicoteste))

  putStrLn "\nServiço mais caro:"
  print  (servicomaiscaro servicoteste)

  putStrLn "\nFaturamento total:"
  print  (calculafaturamentototal servicoteste)


-- Esse just que fica na saida é por conta do Maybe das funções tratar listas vazias
