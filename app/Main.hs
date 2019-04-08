{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.State
import           Data.FileEmbed
import           Data.Maybe
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Language.Haskell.HsColour.CSS
import           Lucid
import           Lucid.Base
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative
import           Options.Applicative.Simple
import           System.Directory
import           System.FilePath
import           Text.Show.Pretty (Value(..), parseValue)

data Opts =
  Opts
    { optsPort :: Int
    , optsDir :: FilePath
    } deriving (Show)

app :: FilePath -> Application
app dir req respond = do
  case pathInfo req of
    [fp] -> do
      contents <- readFile (dir <> "/" <> (T.unpack fp))
      case lookup (takeExtension (T.unpack fp)) supported of
        Nothing ->
          reply
            (html_
               (body_
                  (do p_ (small_ "(Unknown file type. Display as plain text.)")
                      pre_ (toHtml contents))))
        Just generate ->
          do stylesheet <- getStylesheet
             reply
               (html_
                  (do head_ (style_ stylesheet)
                      body_ (generate contents)))
    _ -> do
      files <-
        fmap
          (filter (isJust . flip lookup supported . takeExtension) .
           filter (not . all (== '.')))
          (getDirectoryContents dir)
      reply
        (html_
           (body_
              (do p_
                    (do "Renderable files in "
                        code_ (toHtml dir))
                  ul_
                    (mapM_
                       (\file ->
                          li_
                            (a_ [href_ (fromString ("/" ++ file))] (toHtml file)))
                       files))))
  where
    reply html =
      respond
        (responseLBS status200 [("Content-Type", "text/html")] (renderBS html))

getStylesheet :: IO T.Text
getStylesheet =
  if dev
    then T.readFile "webshow.css"
    else pure (T.decodeUtf8 $(embedFile "webshow.css"))
  where
    dev = True

supported :: [(String, String -> Html ())]
supported =
  [ ( ".hs"
    , \contents ->
        case parseValue contents of
          Just val -> evalState (commuteHtmlT (valueToHtml val)) 0
          Nothing -> do
            p_
              (small_
                 "(Invalid Haskell Show value. Displaying as Haskell source.)")
            pre_ (toHtmlRaw (hscolour False 0 contents)))
  ]

main :: IO ()
main = do
  (opts, ()) <-
    simpleOptions
      "1.0"
      "Webshow"
      "Show printed output from languages"
      (Opts <$>
       option auto (long "port" <> short 'p' <> help "Port number to listen on" <> value 3333) <*>
       strOption (long "directory" <> short 'd' <> help "Directory to look at" <> value "."))
      empty
  putStrLn ("Listening on http://localhost:" ++ show @Int (optsPort opts))
  run (optsPort opts) (app (optsDir opts))

valueToHtml :: MonadState Int m => Value -> HtmlT m ()
valueToHtml =
  \case
    String string -> block "string" (toHtml string)
    Char char -> block "char" (toHtml char)
    Float float -> block "float" (toHtml float)
    Integer integer -> block "integer" (toHtml integer)
    Ratio n d ->
      block
        "ratio"
        (do valueToHtml n
            "/"
            valueToHtml d)
    Neg n ->
      block
        "neg"
        (do "-"
            valueToHtml n)
    List xs ->
      togglable
        "list"
        (\button -> do
           button (inline "brace" "[")
           unless
             (null xs)
             (block
                "contents "
                (table_
                   (mapM_
                      (\(i, e) ->
                         tr_
                           (do td_
                                 [class_ "field-comma-td"]
                                 (if i > 0
                                    then ", "
                                    else "")
                               td_ [class_ "field-value-td"] (valueToHtml e)))
                      (zip [0 :: Int ..] xs))))
           button (inline "brace" "]"))
    Con name xs ->
      togglable
        "con"
        (\button -> do
           when (not (null xs)) (inline "brace" "(")
           button (inline "con-name" (toHtml name))
           block "contents" (mapM_ (\e -> block "con-slot" (valueToHtml e)) xs)
           when (not (null xs)) (inline "brace" ")"))
    Tuple xs ->
      block
        "tuple"
        (do when (not (null xs)) (inline "brace" "(")
            block
              "contents"
              (table_
                 (mapM_
                    (\(i, e) ->
                       tr_
                         (do td_
                               [class_ "field-comma-td"]
                               (if i > 0
                                  then ", "
                                  else "")
                             td_ [class_ "field-value-td"] (valueToHtml e)))
                    (zip [0 :: Int ..] xs)))
            when (not (null xs)) (inline "brace" ")"))
    InfixCons {} -> block "infix-con" "TODO: infix"
    Rec name xs ->
      togglable
        "rec"
        (\button -> do
           when (not (null xs)) (inline "brace" "(")
           button (inline "con-name" (toHtml name))
           inline "brace" " {"
           block
             "contents"
             (table_
                (mapM_
                   (\(i, (n, e)) ->
                      tr_
                        (do td_
                              [class_ "field-comma-td"]
                              (if i > 0
                                 then ", "
                                 else "")
                            td_
                              [class_ "field-name-td"]
                              (inline "field-name" (toHtml n))
                            td_ [class_ "field-equals-td"] (inline "equals" "=")
                            td_ [class_ "field-value-td"] (valueToHtml e)))
                   (zip [0 :: Int ..] xs)))
           inline "brace" "}"
           when (not (null xs)) (inline "brace" ")"))
  where
    inline name inner = span_ [class_ name] inner
    block name inner = div_ [class_ name] inner
    togglable cls inner = do
      uuid <- fmap (T.pack  . show) get
      modify (+ 1)
      div_
        [class_ ("toggle " <> cls)]
        (do input_ [type_ "checkbox", class_ "check", id_ uuid]
            div_ [class_ "inner"] (inner (\html -> label_ [for_ uuid] html)))

isSimple :: Value -> Bool
isSimple =
  \case
    String {} -> True
    Char {} -> True
    Float {} -> True
    Integer {} -> True
    Ratio {} -> True
    Neg {} -> True
    List [] -> True
    Con _ [] -> True
    Tuple [] -> True
    Rec _ [] -> True
    _ -> False
