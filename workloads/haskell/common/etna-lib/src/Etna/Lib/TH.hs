{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Etna.Lib.TH (mkStrategies, mkMain) where

import Etna.Lib.Types (ExpArgs (..), Result)
import Data.Char (toLower)
import Language.Haskell.TH

mkStrategies :: Q Exp -> [Name] -> Q [Dec]
mkStrategies code = foldr (combine . go) (return [])
  where
    go :: Name -> Q [Dec]
    go name =
      let test = mkName $ propToTest $ nameBase name
       in [d|$(varP test) = $(code) $(varE name)|]

    combine :: Q [Dec] -> Q [Dec] -> Q [Dec]
    combine qds qds' = do
      ds <- qds
      ds' <- qds'
      return (ds ++ ds')

mkMain :: IO [String] -> IO [String] -> Q [Dec]
mkMain ims ips = do
  ms <- runIO ims
  ps <- runIO ips
  let mps = concatMap (\m -> map (m,) ps) ms
      amps = concatMap (\(m, p) -> map (\a -> (m, a, p)) (strategyAliases m)) mps
  [d|
    main :: IO ()
    main = do
      args <- getArgs
      let ExpArgs workload strategy prop timeout =
            parseExpArgs (head args)
          test = fromJust $ lookup (strategy, prop) mmap
      run (workload, strategy, prop) timeout test

    mmap :: [((String, String), IO Result)]
    mmap = $(listE (map mkAliasPair amps))
    |]
  where
    mkAliasPair :: (String, String, String) -> Q Exp
    mkAliasPair (m, a, p) = do
      t <- lookupName m (propToTest p)
      [|((a, p), $(varE t))|]

    lookupName pre suf = do
      let name = pre ++ "." ++ suf
      mv <- lookupValueName name
      case mv of
        -- TODO: might want more flexible behavior here
        Nothing -> error ("could not find:" ++ name)
        Just v -> return v


mkMainSampler :: IO [String] -> IO [String] -> Q [Dec]
mkMainSampler ims ips = do
  ms <- runIO ims
  ps <- runIO ips
  let mps = concatMap (\m -> map (m,) ps) ms
      amps = concatMap (\(m, p) -> map (\a -> (m, a, p)) (strategyAliases m)) mps
  [d|
    main :: IO ()
    main = do
      args <- getArgs
      let ExpArgs workload strategy prop timeout =
            parseExpArgs (head args)
          test = fromJust $ lookup (strategy, prop) mmap
      sample (workload, strategy, prop) timeout test

    mmap :: [((String, String), IO Result)]
    mmap = $(listE (map mkAliasPair amps))
    |]
  where
    mkAliasPair :: (String, String, String) -> Q Exp
    mkAliasPair (m, a, p) = do
      t <- lookupName m (propToTest p)
      [|((a, p), $(varE t))|]

    lookupName pre suf = do
      let name = pre ++ "." ++ suf
      mv <- lookupValueName name
      case mv of
        -- TODO: might want more flexible behavior here
        Nothing -> error ("could not find:" ++ name)
        Just v -> return v


propToTest :: String -> String
propToTest = ("test_" ++) . tail . dropWhile (/= '_')

strategyAliases :: String -> [String]
strategyAliases m =
  let lower = map toLower m
      extra = case lower of
        "quick" -> ["quickcheck"]
        "small" -> ["smallcheck"]
        "lean" -> ["leancheck"]
        "hedgehog" -> ["hh"]
        _ -> []
   in m : lower : extra
