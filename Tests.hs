module Tests where

import Control.Monad
import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Map as Map

import Data.DRS

import PGF2
import Tenant
import Utility
import Model

-- handler gr core tests = putStr $ unlines $ map (\(x,y) -> x++show y) $ zip (map (++"\t") tests ) ( map (\string -> map (\x -> core ( x) ) (parse gr (mkCId "DicksonEng") (startCat gr) string)) tests )

-- import System.Environment.FindBin

gr :: IO PGF
gr = readPGF "./Tenant.pgf"

cat2funs :: String -> IO ()
cat2funs cat = do
  gr' <- gr
  let fs = functionsByCat gr' (show cat)
  let ws = filter (isLower . head . show) fs
  let is = map (reverse . dropWhile (\x ->  (==) x '_' || isUpper x) . reverse .show ) ws
  putStrLn (unwords is)

catByPOS :: String -> IO ()
catByPOS  pos = do
  gr' <- gr
  let allCats = categories gr'
  let cats = filter (isPrefixOf pos . show) allCats
  putStrLn (unwords (map show cats))

trans = id

run f tests = do
  gr' <- gr
  let Just eng = Map.lookup "TenantEng" (languages gr')
  let ss = map (chomp . lc_first) tests
  let p =  parse eng (startCat gr')
  let Just incompleteparse = readExpr "ParseIncomplete"
  let Just noaccountfail = readExpr "ParseFailed somewhere"
  let failingparse n string = fromMaybe noaccountfail (readExpr ("ParseFailed at " ++ (show n) ++ " " ++ string))
  let t p = case p of
        ParseOk ((e,prob):rest) -> e
        (ParseFailed offset token) -> failingparse offset token
        ParseIncomplete -> incompleteparse
  let ts = map (t . p) ss
  let zs = zip (map (++"\t") tests) ts
  putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

{-

ans tests = do
  gr	<- readPGF "./Tenant.pgf"
  let ss = map (chomp . lc_first) tests
  let ps = map ( parses gr ) ss
  let ts = map (map ( (linear gr) <=< transform ) ) ps
  let zs = zip (map (++"\t") tests) ts
  putStrLn (unlines (map (\(x,y) -> x ++ (show $ unwords (map displayResult y))) zs) )

displayResult = fromMaybe "Nothing"

reps tests = do
  gr	<- readPGF "./Tenant.pgf"
  let ss = map (chomp . lc_first) tests
  let ps = map ( parses gr ) ss
  let ts = map (map (\x -> (((unmaybe . rep) x) (term2ref drsRefs var_e) ))) ps
  let zs = zip (map (++"\t") tests) ts
  putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

lf tests = do
  gr  <- readPGF "./Tenant.pgf"
  let ss = map (chomp . lc_first) tests
  let ps = map ( parses gr ) ss
  let ts = map (map (\p -> drsToLF (((unmaybe . rep) p) (DRSRef "r1"))) ) ps
  let zs = zip (map (++"\t") tests) ts
  putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

fol tests = do
  gr  <- readPGF "./Tenant.pgf"
  let ss = map (chomp . lc_first) tests
  let ps = map ( parses gr ) ss
  let ts = map (map (\p -> drsToFOL ( (unmaybe . rep) p (term2ref drsRefs var_e) ) ) ) ps
  let zs = zip (map (++"\t") tests) ts
  putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

-}

dic_test = [

	"Heather Forman's refrigerator doesn't need fixing."
	, "Heather Forman's oven keeps burning everything she tries to cook."
	, "The oven's temperature control needs to be adjusted."
	, "The building manager promises to get someone to look at it."
	, "The electricity in Carl Harris's apartment keeps going on and off."
	, "The building manager guesses Mr Harris’s fuse box needs to be checked."
	, "The building manager goes to Mr Harris's apartment and he looks at it right away."
	, "The light out front of the building is out."
	, "The building manager wants to help, but he is busy and he doesn’t have his ladder."
	, "Mr Won says he can fix the light."
	, "The building manager gives Mr Won a lightbulb to fix the light."
	, "Mr Two wants the building manager to do something about his neighbor's dog, which barks all night."
	, "The building manager doesn't want to go near the dog."
	, "The building manager promises to call Mr Two's neighbor to ask him to keep the dog quiet."
	, "Mrs Threet cannot open her kitchen window."
	, "The kitchen window is jammed shut"
	, "The building manager says he can't help her."
	, "Mrs Threet says her cousin can help her open the window."
	, "Some tenants have complaints and the building manager promises to fix some of their problems."


  ]

-- vim: set ts=2 sts=2 sw=2 noet:
