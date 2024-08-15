{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-} 

import Network.Wai
import Network.Wai.Handler.Warp 

import Network.HTTP.Types (status200, status405) 

import Data.Text
import Data.Aeson
import Data.Attoparsec.Text


-- Network.Wai.responseBuilder собирает ответ сервера
-- Посылаем такой ответ на неправильный
badRequest :: Response
badRequest = responseBuilder status405 [] "Bad request"


jsonResponse :: ToJSON a => a -> Response
jsonResponse
  = responseBuilder status200 [] . fromEncoding . toEncoding


-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
serverApp :: Application
serverApp req send = do
    response <-
        case requestMethod req of
            -- у нас только GET-запросы, поэтому только два случая: GET и _
            "GET" ->
                case pathInfo req of
                    -- смотрим на наш путь запроса, в пути переменные Text, пытаемся прочитать их в Double 
                    -- (см. afterSqrtParse и afterBinParse)
                    ["sqrt", x] -> afterSqrtParse $ parseOnly (double) x
                    [op, x, y] -> afterBinParse op (parseOnly (double) x) (parseOnly (double) y)
                    -- если путь не вида /sqrt/x или /binop/x/y - посылаем ответ на плохой запрос
                    _ -> pure badRequest
            _ -> pure badRequest
    send response

-- После парсинга получаем либо сообщение об ошибке парсинга, либо double
-- Если все числа распарсились в double - запускаем соответствующую функцию, если нет - badRequest
afterSqrtParse :: Either String Double -> IO Response
afterSqrtParse (Right d) = getSqrtResponse d
afterSqrtParse (Left _) = pure badRequest

-- Аналогично, но хотим оба числа
afterBinParse :: Text -> Either String Double -> Either String Double -> IO Response
afterBinParse _ (Left _) _ = pure badRequest
afterBinParse _ _ (Left _) = pure badRequest
afterBinParse op (Right x) (Right y) = getBinResponse op x y

-- Ответ на запрос на вычисление корня
-- здесь и далее создаём ответ из Data.Object
getSqrtResponse :: Double -> IO Response
getSqrtResponse x = do
    case x >= 0 of
        True -> pure $ jsonResponse $ object
            [ "operator" .= ("sqrt" :: Value)
            , "arguments" .= [x]
            , "result" .= sqrt x
            , "error" .= (Null :: Value)
            ]
        False -> pure $ jsonResponse $ object
            [ "operator" .= ("sqrt" :: Value)
            , "arguments" .= [x]
            , "result" .= (Null :: Value)
            , "error" .= ("Negative number square root" :: Value)
            ]

-- Рассматриваем все бинарные операции
getBinResponse :: Text -> Double -> Double -> IO Response
getBinResponse op x y = do
    case op of
        "add" -> pure $ jsonResponse $ object
            [ "operator" .= ("add" :: Value)
            , "arguments" .= [x, y]
            , "result" .= (x + y)
            , "error" .= (Null :: Value)
            ]
        "sub" -> pure $ jsonResponse $ object
            [ "operator" .= ("sub" :: Value)
            , "arguments" .= [x, y]
            , "result" .= (x - y)
            , "error" .= (Null :: Value)
            ]
        "mul" -> pure $ jsonResponse $ object
            [ "operator" .= ("mul" :: Value)
            , "arguments" .= [x, y]
            , "result" .= (x * y)
            , "error" .= (Null :: Value)
            ]
        "div" -> case y of
            0 -> pure $ jsonResponse $ object
                [ "operator" .= ("div" :: Value)
                , "arguments" .= [x, y]
                , "result" .= (Null :: Value)
                , "error" .= ("Division by zero" :: Value)
                ]
            _ -> pure $ jsonResponse $ object
                [ "operator" .= ("div" :: Value)
                , "arguments" .= [x, y]
                , "result" .= (x / y)
                , "error" .= (Null :: Value)
                ]
        "pow" -> case and [x == 0, y <= 0] of
            True -> pure $ jsonResponse $ object
                [ "operator" .= ("pow" :: Value)
                , "arguments" .= [x, y]
                , "result" .= (Null :: Value)
                , "error" .= ("Zero to non-positive power" :: Value)
                ]
            False -> pure $ jsonResponse $ object
                [ "operator" .= ("pow" :: Value)
                , "arguments" .= [x, y]
                , "result" .= (x ** y)
                , "error" .= (Null :: Value)
                ]
        _ -> pure $ badRequest

main :: IO ()
main = do
    let port = 8000
    putStrLn $ "Listening on port " ++ show port
    run port serverApp
