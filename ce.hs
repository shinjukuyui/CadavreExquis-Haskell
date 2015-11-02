{-# LANGUAGE OverloadedStrings #-}

import System.Random
import qualified Data.Text as T
import Database.MySQL.Simple
import Web.Authenticate.OAuth
import Web.Twitter.Conduit
import Web.Twitter.Types
import Network.HTTP.Conduit
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import Data.Yaml

  {-
    設定情報
  -}
data CeConfigurations = CeConfigurations {
  ceDatabase :: String,
  ceDatabaseUser :: String,
  ceDatabasePassword :: String,
  ceDatabaseType :: String,
  ceOauthConsumerKey :: String,
  ceOauthConsumerSecret :: String,
  ceOauthToken :: String,
  ceOauthTokenSecret :: String
} deriving (Show)
instance FromJSON CeConfigurations where
  parseJSON (Object v) = CeConfigurations <$>
    v .: "database" <*>
    v .: "databaseUser" <*>
    v .: "databasePassword" <*>
    v .: "databaseType" <*>
    v .: "oauthConsumerKey" <*>
    v .: "oauthConsumerSecret" <*>
    v .: "oauth_token" <*>
    v .: "oauth_token_secret"
  parseJSON _ = error "Can't parse CeConfigurations from YAML/JSON"

  {-
    0から引数で与えられた値までのランダムな値を整数で返す
  -}
rand :: Int -> IO Int
rand n = (randomRIO(0, n) :: IO Int)

  {-
    引数で与えられたCharをStringに変換する
  -}
ctos :: Char -> String
ctos = (:[])

  {-
    引数で与えられた文字列のリストからランダムな文字列を返す
  -}
sel :: [String] -> IO String
sel xs = do
  n <- (rand(length xs - 1))
  return (xs !! n)

  {-
    指定された辞書と語彙から登録数を返す
  -}
cntSrc :: Connection -> String -> String -> IO Int
cntSrc conn d s = do
  [Only i] <- query conn "select count(*) from sentence_source where dictionaryType = ? and messageType = ?" (d, s)
  return i

  {-
    指定された辞書と語彙からランダムな一つを返す
  -}
selSrc :: Connection -> String -> String -> IO String
selSrc conn d s = do
  i <- cntSrc conn d s
  n <- (rand(i - 1))
  [Only r] <- query conn "select sentence from sentence_source where dictionaryType = ? and messageType = ? order by sourceId limit ?, 1" (d, s, n)
  return r

  {-
    テンプレートの"*"を置き換えるための名詞を選ぶ
    引数で名詞のリストと形容詞のリストを受け取って、
    70%の確率で「形容詞+名詞」を、30%の確率で「名詞だけ」をランダムに選んで返す
  -}
seln :: Connection -> String -> IO String
seln conn d = do
  n <- rand(9)
  if n < 7
  then do
    qua <- selSrc conn d "02"
    non <- selSrc conn d "01"
    return (qua ++ non)
  else selSrc conn d "01"

  {-
    与えられた文字が"*"なら（形容詞+）名詞、"#"なら数値の文字列に置き換え、
    それ以外なら文字列化して返す
  -}
rep :: Char -> Connection -> String -> IO String
rep c conn d
  | c == '*' = seln conn d
  | c == '#' = fmap show (rand(9))
  | otherwise = sel[(ctos c)]

  {-
    文末の処理をする
    70%の確率で句点をつけて、それ以外なら簡単符をつける
  -}
fin :: String -> IO String
fin s = do
  n <- rand(9)
  if n < 7
  then return (s ++ "。")
  else return (s ++ "！")

  {-
    テンプレートになる文字列とデータベースを受け取って、
    テンプレートを置き換えて文章にして返す
  -}
gen :: Connection -> String -> String -> IO String
gen conn d t = do
  if elem '*' t
  then do
    let ss = span (/= '*') t
    n <- rep '*' conn d
    gen conn d (fst(ss) ++ n ++ tail(snd(ss)))
  else if elem '#' t
    then do
      let ss = span (/= '#') t
      n <- rep '#' conn d
      gen conn d (fst(ss) ++ n ++ tail(snd(ss)))
  else fin t

  {-
    ついったーにPostする
  -}
tw :: CeConfigurations -> String -> IO Status
tw c s = do
  let auth = twitterOAuth {
    oauthConsumerKey = BS.pack (ceOauthConsumerKey c),
    oauthConsumerSecret = BS.pack (ceOauthConsumerSecret c)
  }
  let cred = Credential [ ("oauth_token", BS.pack (ceOauthToken c)), ("oauth_token_secret", BS.pack (ceOauthTokenSecret c)) ]
  let twInfo =  setCredential auth cred def
  m <- newManager tlsManagerSettings
  runResourceT $ do
    r <- call twInfo m $ update (T.pack s)
    return r

  {-
    優美なる死体遊び
  -}
main :: IO ()
main = do
  conf <- BS.readFile "ce.yml"
  let parsedConf = decode conf :: Maybe CeConfigurations
  case parsedConf of
    Nothing -> error "Could not parse config file."
    (Just v) -> do
      conn <- connect defaultConnectInfo {
        connectDatabase = ceDatabase v,
        connectUser = ceDatabaseUser v,
        connectPassword = ceDatabasePassword v
      }
        -- テンプレートからランダムに一つ選ぶ
      t <- selSrc conn (ceDatabaseType v) "00"
        -- 選んだテンプレートをもとに文を作る
      s <- gen conn (ceDatabaseType v) t
      putStrLn s
        -- ついったーにPostする
      r <- tw v s
        -- 結果を表示する
      print r
