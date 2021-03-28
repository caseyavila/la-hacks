{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

import System.IO
import Yesod
import Text.Lucius

data LaHacks = LaHacks

mkYesod "LaHacks" [parseRoutes|
/style.css StyleR GET
/ HomeR GET
/draft DraftR GET POST
/question/#Int QuestionR GET
|]

instance Yesod LaHacks

getStyleR :: Handler Css
getStyleR = do
    sendFile "text/css" "site/style.css"

getHomeR :: Handler Html
getHomeR = do 
    sendFile "text/html" "site/index.html"

getDraftR :: Handler Html
getDraftR = do
    sendFile "text/html" "site/draft.html"

getQuestionR :: Int -> Handler Html
getQuestionR id = do
    question <- liftIO $ questionFromID id
    let hello = init $ tail $ htmlParse question
    defaultLayout [whamlet|
        <!DOCTYPE html>
        <html>
            <head>
                <meta charset="utf-8">
                <meta name="viewport" content="width=device-width">
                <title>la-hacks
                <link href="/style.css" rel="stylesheet" type="text/css"/>
            <body>
                <h1>Question:
                <p>#{hello}
                <h1>Response:
                <form action="" method="post">
                    <textarea name="question-textarea" rows="4" cols="50">
                    <div>
                        <input type="submit" value="Submit"/>
|]

postDraftR :: Handler Html
postDraftR = do
    number <- liftIO $ logNumber
    question <- lookupPostParam "draft-textarea"
    case question of
        Nothing -> redirect DraftR
        Just question -> do
            liftIO $ putStrLn $ "New question: " ++ (show $ number)
            -- Write question to file
            liftIO $ writeFile ("questions/" ++ (show $ number)) (show $ question)
            -- Update number file
            liftIO $ writeFile "number" (show $ number + 1)
            redirect $ QuestionR number

questionFromID :: Int -> IO String
questionFromID id = do
    handle <- openFile ("questions/" ++ (show $ id)) ReadWriteMode
    contents <- hGetContents handle
    return contents
    
logNumber :: IO Int
logNumber = do
    handle <- openFile "number" ReadWriteMode
    contents <- hGetContents handle
    return (read contents :: Int)

htmlParse :: String -> String
htmlParse ('\\':'r':'\\':'n':xs) = '<' : 'b' : 'r' : '>' : htmlParse xs
htmlParse (x:xs)       = x : htmlParse xs
htmlParse ""           = ""

main :: IO ()
main = warp 3000 LaHacks
