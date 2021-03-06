module WordsCharacters where

import Data.Char

import PGF
import System.Environment.FindBin

-- path = getProgPath
-- file = path >>= \p -> return ( (++) p "/Happier.pgf")
-- gr = file >>= \f -> return ( readPGF f )
gr = readPGF "/home/drbean/GF/question/conversation/complaint/tenant/Tenant.pgf"

cat2funs :: String -> IO [CId]
cat2funs cat = do
	gr' <- gr
	let fs = functionsByCat gr' (mkCId cat)
	let ws = filter (isLower . head . showCId) fs
	let is = map (reverse . dropWhile (\x ->  (==) x '_' || isUpper x || isNumber x) . reverse .showCId ) ws
	return (map mkCId is )

gfWords :: [(String, IO [CId])]
gfWords = [
	("A",a)
	, ("Adv",adv)
	-- , ("Aux",aux)
	, ("Conj",conj)
	, ("Det",det)
	, ("N",n)
	, ("CN",cn)
	, ("PN",pn)
	, ("Pron",pron)
	, ("Prep",prep)
	-- , ("Rel",rel)
	, ("Tag",tag)
	, ("V",v)
	, ("V2",v2)
	, ("V3",v3)
	, ("VV",vv)
	, ("VS",vs)
	, ("V2A",v2a)
	]

posName :: String -> String
posName "A"	= "Adjective"
posName "Adv"	= "Adverb"
posName "Aux"	= "Auxiliary"
posName "Conj"	= "Conjunction"
posName "Det"	= "Determiner"
posName "N"	= "Uncount Noun"
posName "CN"	= "Count Noun"
posName "PN"	= "Proper Noun"
posName "Pron"	= "Pronoun"
posName "Prep"	= "Preposition"
posName "Rel"	= "Relative Pronoun"
posName "Tag"	= "Question Tag"
posName "V"	= "Verb"
posName "V2"	= "Verb + object"
posName "V3"	= "Verb + obj1 + obj2"
posName "VV"	= "Verb + verb"
posName "VS"	= "Verb + sentence"
posName "V2S"	= "Verb + object + sentence"
posName "V2A"	= "Verb + object + adjective"


a	= cat2funs "AP"
adv	= cat2funs "Adv"
conj	= cat2funs "Conj"
det	= cat2funs "Det"
n	= cat2funs "N"
cn	= cat2funs "CN"
pn	= cat2funs "PN"
prep	= cat2funs "Prep"
pron	= cat2funs "NP"
v	= cat2funs "V"
v2	= cat2funs "V2"
v3	= cat2funs "V3"
vv	= cat2funs "VV"
vs	= cat2funs "VS"
v2a	= cat2funs "V2A"
tag = return ( map mkCId tags )





aux = [
	"doesn't"
	, "don't"
	, "does"
	, "do"
	, "is"
	, "are"
	, "isn't"
	, "aren't"
	, "would"
	, "should"
	]
	

rel = [


	]

tags = [
	"doesn't he"
	, "doesn't she"
	, "doesn't it"
	, "don't they"
	, "does he"
	, "does she"
	, "does it"
	, "do they"
	, "isn't he"
	, "isn't she"
	, "isn't it"
	, "aren't they"
	, "is he"
	, "is she"
	, "is it"
	, "are they"
	]

{-


about	: Prep;
adjusted	: V2;
all night	: Adv;
and	: Conj;
apartment	: CN;
ask	: V2V;
at	: Prep;
bark	: V;
building	: CN;
building manager	: CN;
burn	: V2;
busy	: V;
but	: Conj;
call_V2V	: V2V;
call	: V2;
cannot
can't
check	: V2;
cook	: V2;
cousin	: CN;
do_about	: V3;
dog	: CN;
dog	: PlaceNoun;
electricity	: N;
fix	: V2;
fixing	: N;
fuse box	: CN;
get	: V2V;
give	: V3;
go_to	: V2;
go	: V;
guess	: VS;
help_V	: V;
help_V2	: V2;
help_V2V	: V2V;
in_LOCPREP	: Prep;
Heather	: PN;
Heather Forman	: PN;
Ms Forman	: PN;
jam	: V2A;
keep	: VV;
keep_V2A	: V2A;
kitchen window	: CN;
window	: CN;
ladder	: CN;
light	: CN;
lightbulb	: CN;
look_at	: V2;
Mr Two	: PN;
Mr Won	: PN;
Carl	: PN;
Carl Harris	: PN;
Mr Harris	: PN;
Mrs Threet	: PN;
near	: LocPrep;
need	: VV;
need_V2	: V2;
neighbor	: CN;
off	: Adv;
on	: Adv;
open	: V2;
out	: Adv;
out front	: Prep;
oven	: CN;
promise	: VV;
quiet	: AP;
refrigerator	: CN;
right away	: Adv;
say	: VS;
shut	: AP;
temperature control	: CN;
to	: Prep;
try	: VV;
want	: VV;
want_V2V	: V3V;
tenant	: CN;
complaint	: CN;
problem	: CN;


-}

-- vim: set ts=2 sts=2 sw=2 noet:
