module Main where

import PGF2
import Tenant
import LogicalForm hiding ((==))
import Evaluation

import Model
import WordsCharacters

import Data.Maybe
import Control.Monad
import Data.List.Split
import Data.List

main :: IO ()
main = do
	s <- getLine
	let l = (chomp . lc_first . leading_space) s
	let unknown = unwords (filter (flip notElem morpho) (words l))
	putStrLn ("Unknown_words: " ++ unknown )
	let ps =  parse eng (startCat gr) l
	let Just incompleteparse = readExpr "ParseIncomplete"
	let Just noaccountfail = readExpr "ParseFailed somewhere"
	let failingparse n string = fromMaybe noaccountfail (readExpr ("ParseFailed at " ++ (show n) ++ " " ++ string))
	let t ps = case ps of
		ParseOk ((e,prob):rest) -> e
		(ParseFailed offset token) -> failingparse offset token
		ParseIncomplete -> incompleteparse
	-- let ls = map (linear gr <=< transform) ps
	let p = t ps
	putStrLn ("Parsed: " ++ show p )
	-- let urs = map (unmaybe . rep) ps
	-- let reps = map (\ur -> ur (term2ref drsRefs var_e)) urs
	-- putStrLn ("Representation: " ++ show reps )
	-- let lfs = map (\ur -> drsToLF (ur (term2ref drsRefs var_e))) urs
	-- putStrLn ("LF: " ++ show lfs )
	putStrLn ("Answer: No answer" )
	let course = (label . fg) p
	putStrLn ("Course: " ++ course )

label :: GUtt -> String
label (GQUt (GMkQS _ _ _ (GWH_Pred _ _)))	= "WH"
label (GQUt (GMkQS _ _ _ (GWH_ClSlash _ _)))	= "WH"
label (GQUt (GMkQS _ _ _ (GYN _)))	= "YN"
label (GQUt (GMkQS _ _ _ (GTagS _ _)))	= "Tag"
label _				= "Unparseable"

takeCourse :: String -> String -> String
takeCourse _ "WH" = "WH"
takeCourse "WH" _ = "WH"
takeCourse _ "YN" = "YN"
takeCourse "YN" _ = "YN"
takeCourse _ "Tag"  = "Tag"
takeCourse "Tag" _  = "Tag"
takeCourse _ "S"  = "S"
takeCourse "S" _  = "S"
takeCourse "Unparseable" _  = "Unparseable"
takeCourse _  _   = error "undefined course, not WH, YN, S, or Unparseable"

bestAnswer :: [Maybe String] -> String
bestAnswer ss = 
	foldl takeAnswer "No answer" (map (fromMaybe "No answer") ss)

takeAnswer :: String -> String -> String
takeAnswer _ "yes" = "yes"
takeAnswer "yes" _ = "yes"
takeAnswer _ "no" = "no"
takeAnswer "no" _  = "no"
takeAnswer a b@('M' : 'a' : _)  = collateAnswer a b -- Mandy
takeAnswer a b@('A' : 'l' : _)  = collateAnswer a b -- Alice
takeAnswer a b@('A' : 'r' : _)  = collateAnswer a b -- Ariel
takeAnswer a b@('S' : 'a' : _)  = collateAnswer a b -- Sabrina
takeAnswer "none" _ = "none of Mandy, Alice, Ariel or Sabrina"
takeAnswer _ "none" = "none of Mandy, Alice, Ariel or Sabrina"
takeAnswer "No answer" _ = "No answer"
takeAnswer _ "No answer" = "No answer"
takeAnswer _  _   = error "undefined answer, not Yes, No, Mandy, Alice, Ariel or Sabrina, none or No answer"

collateAnswer a b = formatUp $ nub $ filter
	(\x -> x ==	"Mandy"
	|| x ==	"Alice"
	|| x ==	"Ariel"
	|| x ==	"Sabrina"
	) (concatMap (splitOn " , " ) (splitOn " or " (a ++ " , " ++ b)))

formatUp es = let parts = splitAt 1 (reverse es)
	in case snd parts of 
		[] -> concat (fst parts)
		_ -> concat  (intersperse " , " (snd parts) ++ [" or "] ++ fst parts )
--
-- vim: set ts=2 sts=2 sw=2 noet:
