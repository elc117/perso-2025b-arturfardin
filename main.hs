{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Network.HTTP.Types.Status (status404, status500)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (HostPreference, defaultSettings, setHost, setPort)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- Ainda não sei se vou usar todos esses imports, mas é bom ter eles prontos caso eu precise

main :: IO ()
main = scotty 3000 $
  get "/:servicos/:id" $ do
 -- Buscar no banco de dados


  get "/:servicos/:cadastro" $ do
-- Rota de listar serviços

  post "/:servicos" $ do
-- Colocar no banco de dados



  delete "/servicos/:id" $ do
-- Deletar do banco de dados
