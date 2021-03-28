{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

import System.Directory
import System.IO
import Yesod
import Text.Lucius

data LaHacks = LaHacks

mkYesod "LaHacks" [parseRoutes|
/style.css StyleR GET
/ HomeR GET
/draft DraftR GET POST
/question/#Int QuestionR GET POST
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

postDraftR :: Handler Html
postDraftR = do
    number <- liftIO $ logNumber Nothing
    question <- lookupPostParam "question-textarea"
    liftIO $ createDirectoryIfMissing True ("responses/" ++ (show number))
    case question of
        Nothing -> redirect DraftR
        Just question -> do
            liftIO $ putStrLn $ "New question: " ++ (show number)
            -- Write question to file
            liftIO $ writeFile ("questions/" ++ (show number)) (show question)
            -- Update number file
            liftIO $ writeFile "number" (show $ number + 1)
            redirect $ QuestionR number

getQuestionR :: Int -> Handler Html
getQuestionR id = do
    question <- liftIO $ questionFromID id
    let hello = init $ tail $ htmlParse question
    responses <- liftIO $ responseArray id
    actualResponses <- liftIO $ responsesFromArray id (clean responses)
    liftIO $ print actualResponses
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
                    <textarea name="response-textarea" rows="4" cols="50">
                    <div>
                        <input type="submit" value="Submit"/>
                <h3>Responses:
                <p>#{show actualResponses}
|]

postQuestionR :: Int -> Handler Html
postQuestionR id = do
    number <- liftIO $ logNumber $ Just id
    response <- lookupPostParam "response-textarea"
    case response of
        Nothing -> redirect $ HomeR
        Just response -> do
            -- Write response to file
            let directory = "responses/" ++ (show id) ++ "/"
            liftIO $ writeFile (directory ++ (show number)) (show response)
            liftIO $ writeFile (directory ++ "number") (show $ number + 1)
            redirect $ QuestionR id


questionFromID :: Int -> IO String
questionFromID id = do
    handle <- openFile ("questions/" ++ (show id)) ReadWriteMode
    contents <- hGetContents handle
    return contents
                
responseArray :: Int -> IO [String]
responseArray id = do
    let directory = "responses/" ++ (show id) ++ "/"
    fileList <- getDirectoryContents directory
    print $ clean fileList
    return fileList

clean :: [String] -> [String]
clean (".":xs) = clean xs
clean ("..":xs) = clean xs
clean ("number":xs) = clean xs
clean (x:xs)       = x : clean xs
clean []           = []

responsesFromArray :: Int -> [String] -> IO [String]
responsesFromArray id [] = do return []
responsesFromArray id (x : xs) = do
    handle <- openFile ("responses/" ++ (show id) ++ "/" ++ x) ReadMode
    contents <- hGetContents handle
    rest <- responsesFromArray id xs
    return (contents:rest)

-- Input Nothing to get question number, Just x to get number of responses
-- to question x
logNumber :: Maybe Int -> IO Int
logNumber question = do
    case question of
        Nothing -> do
            let fileName = "number"
            fileExist <- doesFileExist fileName
            if fileExist
                then do
                    handle <- openFile fileName ReadMode
                    contents <- hGetContents handle
                    return (read contents :: Int)
                else do
                    writeFile fileName "0"
                    putStrLn "No number file, creating a new one"
                    return 0
        Just question -> do
            let fileName = "responses/" ++ (show question) ++ "/number"
            fileExist <- doesFileExist fileName
            if fileExist
                then do
                    handle <- openFile fileName ReadMode
                    contents <- hGetContents handle
                    return (read contents :: Int)
                else do
                    createDirectoryIfMissing True ("responses/" ++ (show question))
                    writeFile fileName "0"
                    putStrLn "No number file, creating a new one"
                    return 0

htmlParse :: String -> String
htmlParse ('\\':'r':'\\':'n':xs) = '<' : 'b' : 'r' : '>' : htmlParse xs
htmlParse (x:xs)       = x : htmlParse xs
htmlParse ""           = ""

main :: IO ()
main = warp 3000 LaHacks
