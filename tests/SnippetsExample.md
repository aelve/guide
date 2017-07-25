    {-# LANGUAGE OverloadedStrings #-}

    module Main where

    import {{Hackage}}[Data.Monoid] ((<>))
    import {{Hackage}}[Web.Spock]
    import {{Hackage}}[Web.Spock.Config]

        main :: IO ()
        main = do
          config <- defaultSpockCfg () PCNoDatabase ()
          runSpock 8080 $ spock config app

        app :: SpockM () () () ()
        app = do
{{HltLine}}         -- When “/” is requested, show “Hello world!”
{{HltLine}}         get root $
{{HltLine}}            text "Hello world!"
