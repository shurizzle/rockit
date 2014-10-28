module RockIt.Template
  (
    Format(..)
  , parse
  , render
  ) where

data Format = Playlist
            | Author
            | Title
            | Extension
            | Number
            | FormattedNumber
            | Percent
            | Custom String
  deriving (Eq, Show)

parse :: String -> [Format]
parse []           = []
parse ('%':'%':xs) = Percent : parse xs
parse ('%':'a':xs) = Author : parse xs
parse ('%':'p':xs) = Playlist : parse xs
parse ('%':'t':xs) = Title : parse xs
parse ('%':'e':xs) = Extension : parse xs
parse ('%':'N':xs) = Number : parse xs
parse ('%':'n':xs) = FormattedNumber : parse xs
parse (x:xs)       =
  case parse xs of
    (Custom x'):xs' -> (Custom $ x:x') : xs'
    rest            -> Custom [x] : rest

render :: [Format] -> String -> String -> String -> String -> Int -> String -> String
render format playlist author title extension number formattedNumber =
    render' format playlist author title extension (show number) formattedNumber

render' :: [Format] -> String -> String -> String -> String -> String -> String -> String
render' []                   _        _      _     _         _      _               = ""
render' (Playlist:xs)        playlist author title extension number formattedNumber = playlist ++ render' xs playlist author title extension number formattedNumber
render' (Author:xs)          playlist author title extension number formattedNumber = author ++ render' xs playlist author title extension number formattedNumber
render' (Title:xs)           playlist author title extension number formattedNumber = title ++ render' xs playlist author title extension number formattedNumber
render' (Extension:xs)       playlist author title extension number formattedNumber = extension ++ render' xs playlist author title extension number formattedNumber
render' (Number:xs)          playlist author title extension number formattedNumber = number ++ render' xs playlist author title extension number formattedNumber
render' (FormattedNumber:xs) playlist author title extension number formattedNumber = formattedNumber ++ render' xs playlist author title extension number formattedNumber
render' (Percent:xs)         playlist author title extension number formattedNumber = "%" ++ render' xs playlist author title extension number formattedNumber
render' ((Custom x):xs)      playlist author title extension number formattedNumber = x ++ render' xs playlist author title extension number formattedNumber
