{-# LANGUAGE OverloadedStrings #-}
module Main where


import Debug.Trace
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe
import Data.Char (isSpace)


import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.API.InlineMode.InlineQueryResult
import Telegram.Bot.API.InlineMode.InputMessageContent (defaultInputTextMessageContent)

import Shupai (shupai)

type Model = ()

data Action
  = InlineEcho InlineQueryId Text

echoBot :: BotApp Model Action
echoBot = BotApp
  { botInitialModel = ()
  , botAction = updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _
  | isJust $ updateInlineQuery update =  do
      query <- updateInlineQuery update
      let queryId = inlineQueryId query
      let msg =  inlineQueryQuery query
      if Text.length msg <= 140 || Text.length msg > 0 then
        seq msg $ Just $ InlineEcho queryId msg
      else Nothing
  | otherwise = Nothing

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  InlineEcho queryId msg -> model <# do
    let result = (defInlineQueryResultGeneric (InlineQueryResultId "000"))
          { inlineQueryResultTitle = Just "发送竖排结果"
          , inlineQueryResultInputMessageContent = let
              shupaiText = Text.pack . shupai . Text.unpack $ msg
              in 
               Just (defaultInputTextMessageContent shupaiText)
          }
        thumbnail = defInlineQueryResultGenericThumbnail result
        article = defInlineQueryResultArticle thumbnail
        answerInlineQueryRequest = defAnswerInlineQuery queryId [article]
    _ <- runTG  answerInlineQueryRequest
    return ()

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ echoBot env

main :: IO ()
main = do
  content <- readFile "tgtoken"
  let token = Token $ Text.pack $ strip content
  run token

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
