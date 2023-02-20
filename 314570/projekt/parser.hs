-- Parser module for regular expressions.
-- 
-- Features:
-- - parses basic regex stuff: literals, concatenation of expressions, alternative of expressions and Kleenie star operator
-- - smart constructors automatically simplify abstract syntax tree
-- - parses parentheses (not nested and without names) and "\w" "\d" "+" metacharacters
-- - parser uses Alternative over Maybe monad

module RegexParser where 
import Control.Applicative
import Data.Set
 
-- ------------------------------------------------------------------------------------
-- AST 
-- ------------------------------------------------------------------------------------
data ReExpr =
  Eps
  | Lit (Set Char)
  | Seq ReExpr ReExpr
  | Alt ReExpr ReExpr
  | Star ReExpr 
  | Grp ReExpr
  deriving(Eq)

-- ------------------------------------------------------------------------------------
-- Show Instance 
-- ------------------------------------------------------------------------------------
instance Show ReExpr where
  show Eps = "Ɛ"
  show (Lit x) = show x
  show (Seq expr1 expr2) = show expr1 ++ " `sequence` " ++ show expr2
  show (Alt expr1 expr2) = "( " ++ show expr1 ++ ") `alternative` (" ++ show expr2 ++ " )"
  show (Star expr) = "(Kleenie star " ++ show expr ++ " )"
  show (Grp expr) = "( " ++ show expr ++ " )"

-- ------------------------------------------------------------------------------------
-- Some defines
-- ------------------------------------------------------------------------------------
metacharacters = ['*', '.', '(', ')', '|'] 
digits = ['0' .. '9'] 
alphaUpper = ['A' .. 'Z']
alphaLower = ['a' .. 'z']

alpha = alphaLower ++ alphaUpper
alphaNum = digits ++ alphaLower ++ alphaUpper

type ParseResult = Maybe (ReExpr -> ReExpr, String)
type ParserType = String -> ParseResult

-- ------------------------------------------------------------------------------------
-- Smart constructors
-- ------------------------------------------------------------------------------------
reseq :: ReExpr -> ReExpr -> ReExpr
reseq Eps Eps = Eps
reseq Eps expr = expr
reseq expr Eps = expr
reseq expr1 expr2 = Seq expr1 expr2

realt :: ReExpr -> ReExpr -> ReExpr
realt Eps Eps = Eps
realt Eps expr = expr
realt expr Eps = expr
realt expr1 expr2 = Alt expr1 expr2

restar :: ReExpr -> ReExpr
restar Eps = Eps
restar (Lit x) = Star (Lit x)
restar (Grp e) = Star (Grp e)
restar (Seq e1 e2) = Seq e1 (restar e2)
restar (Alt e1 e2) = Alt e1 (restar e2)

replus :: ReExpr -> ReExpr
replus Eps = Eps
replus expr = Seq expr (Star expr)

mkgroup :: ReExpr -> ReExpr
mkgroup Eps = Eps
mkgrpup = Grp 

-- ------------------------------------------------------------------------------------
-- Parsers
-- ------------------------------------------------------------------------------------
parseLit :: String -> ParseResult
parseLit [] = Nothing
parseLit (x:xs) = if x `elem`  metacharacters
                  then Nothing
                  else Just (\expr -> expr `reseq` Lit (singleton x), xs)


parseStar :: String -> ParseResult
parseStar ('*':xs) = Just (restar, xs) 
parseStar _ = Nothing

parsePlus :: String -> ParseResult
parsePlus ('+':xs) = Just (replus, xs)
parsePlus _ = Nothing
                                   

parseAny :: String -> ParseResult
parseAny ('.':xs) = Just ((`reseq` Lit (fromList alphaNum)), xs)
parseAny _ = Nothing

parseAlt :: String -> ParseResult
parseAlt ('|':xs) = let (match,remainder) = break (\c -> c == '|' || c == '(') xs in
                          case parseAcc match Eps of
                               Eps -> case parseAcc remainder Eps of
                                      Eps -> Nothing
                                      expr2 -> Just ((`realt` expr2), "") 
                               expr2 -> Just ((`realt` expr2), remainder) 
parseAlt _ = Nothing

parseGrp :: String -> ParseResult
parseGrp ('(':xs) = 
  let (match,remainder) = break ( == ')') xs in
    case remainder of
    [] -> Nothing
    (closing_par:tail) -> 
       case parseAcc match Eps of
         Eps -> Nothing
         expr2 -> Just ((`reseq` Grp expr2), tail) 
parseGrp _ = Nothing


parseMeta :: String -> ParseResult
parseMeta ('\\':y:xs) = 
  case y of
    'w' -> Just ((`reseq` Lit (fromList alpha)), xs)
    'd' -> Just ((`reseq` Lit (fromList digits)), xs)
    y | y `elem` metacharacters -> Just ((`reseq` Lit (singleton y)), xs)
    _ -> Nothing
parseMeta _ = Nothing

combinedParser :: ParserType
combinedParser str = parseAlt str <|> parseMeta str <|> parseGrp str <|> parseStar str <|> parseLit str <|> parseAny str <|> parsePlus str


-- ------------------------------------------------------------------------------------
-- Parsing function
-- ------------------------------------------------------------------------------------
parseAcc :: String -> ReExpr -> ReExpr 
parseAcc [] expr = expr 
parseAcc str expr =
    case combinedParser str of
    Nothing -> Eps 
    Just (exprFun, newStr) -> parseAcc newStr (exprFun expr) 

parse :: String -> ReExpr
parse [] = Eps
parse str = parseAcc str Eps
