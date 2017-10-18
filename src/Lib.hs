{-# LANGUAGE FlexibleContexts #-}

module Lib where
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.String
import Control.Applicative 
import Data.List(find,intersperse)
import Data.Map(Map)
import qualified Data.Map as M
import Debug.Trace

readMaybe s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

fnNames = "fghelmnop"
idNames = "abcxyzijk"

data Ty = FnTy Ty Ty | Ty String
  deriving (Show, Eq, Ord)

data Expr = Lam String Ty Expr
          | And [Expr]
          | App Expr [Expr]
          | Var String
          | Lit String Ty
          deriving (Show, Eq, Ord)

renameBinders m (Lam a t e) =
  case (readMaybe a, M.lookup a m) of
    (Just an, _) -> let r = [(case t of
                               FnTy _ _ -> fnNames
                               Ty _ -> idNames) !! an]
                   in Lam r t (renameBinders (M.insert a r m) e)
    _ -> Lam a t (renameBinders m e)
renameBinders m (And l) = And $ map (renameBinders m) l
renameBinders m (App h as) = App (renameBinders m h) (map (renameBinders m) as)
renameBinders m (Var s) = case M.lookup s m of
                            Just s' -> Var s'
                            Nothing -> Var s
renameBinders m (Lit s t) = Lit s t

tyOfLength' 0 = "e"
tyOfLength' 1 = "<e,t>"
tyOfLength' n = "<" ++ "e," ++ tyOfLength' (n-1) ++ ">"

tyOfLength n = Ty $ tyOfLength' n

lambdaP' :: Parser (Expr, Map Expr Int)
lambdaP' = (do
  char '\\' >> space
  binders <- some (space >> alphaNumChar)
  space >> char '.' >> space
  exprs_maps <- sepBy1 (do
                          (head, m) <- lambdaP'
                          space
                          args <- sepBy lambdaP' (many $ char ' ')
                          return (head, map fst args, M.insert head (length args) (M.unions (m : map snd args))))
               (space >> char ',' >> space)
  let exprs = map (\(a,b,_) -> (a,b)) exprs_maps
  let maps = M.unions (map (\(_,_,x) -> x) exprs_maps)
  return (let loop bs = case bs of
                [] -> And (map (\(h,as) ->
                                 case as of
                                   [] -> h
                                   _ -> App (varOrLit (length as) h) as) exprs)
                (b:bs) -> Lam [b] (case M.lookup (Var [b]) maps of
                                    Nothing -> Ty "e"
                                    Just n -> tyOfLength n)
                                 (loop bs)
         in loop binders
         , maps))
  <|> (between (char '(' >> space) (space >> char ')') lambdaP')
  <|> (do
          s <- some (alphaNumChar <|> char '_')
          return (Var s, M.empty))
  where varOrLit n (Var s) | length s == 1 = Var s
                           | otherwise = Lit s (tyOfLength n)
        varOrLit _ t = t

lambdaP = do
  (e, _) <- lambdaP'
  return e

spfP :: Parser Expr
spfP = between
       (char '(' >> space)
       (space >> char ')')
       ((do string "lambda"
            space
            id <- char '$' >> some alphaNumChar
            char ':'
            ty <- parseTy
            space
            body <- spfP
            space
            return $ Lam id ty body)
         <|>
         (do string "and:<t*,t>"
             space
             And <$> sepBy1 spfP (many $ char ' '))
         <|>
         (do h <- spfP
             space
             args <- sepBy1 spfP (many $ char ' ')
             return $ App h args))
       <|> (char '$' >> (Var <$> some (alphaNumChar <|> char '_')))
       <|> (do id <- some (alphaNumChar <|> char '_')
               char ':'
               ty <- parseTy
               return $ Lit id ty)
    where parseTy = ((do char '<'
                         a <- parseTy
                         char ','
                         b <- parseTy
                         char '>'
                         return $ FnTy a b)
                     <|>
                     (Ty <$> some lowerChar))

ex1 = parse lambdaP "\\fgxy. f x, g x y, with x y"
ex2 = parse spfP "(lambda $0:<e,t> (lambda $1:<e,t> (lambda $2:e (lambda $3:e (and:<t*,t> ($0 $2) ($1 $3) (with:<e,<e,t>> $2 $3))))))"
ex3 = parse spfP "(lambda $0:e (lambda $1:e (lambda $2:e (and:<t*,t> (person:<e,t> $0) (person:<e,t> $1) (bag:<e,t> $2) (look_at:<e,<e,t>> $0 $1) (with:<e,<e,t>> $1 $2)))))"
ex4 = parse spfP "(lambda $0:<e,t> (lambda $1:<e,t> (lambda $2:e (and:<t*,t> ($0 (lambda $3:e $3)) ($1 $2) (with:<e,<e,t>> $2 $1)))))"

parse :: (Show e, Show (Token s)) => Parsec e s Expr -> s -> Expr
parse p s = case runParser p "" s of
  Right r -> renameBinders M.empty r
  Left l -> error (show l)

showLambda' (Lam a t e) = a ++ showLambda' e
showLambda' e = ". " ++ showLambda'' e

showLambda'' (Lam a t e) = "(\\" ++ a ++ showLambda' e ++ ")"
showLambda'' (And es) = concat (intersperse ", " (map showLambda'' es))
showLambda'' (App h as) = showLambda'' h ++ " " ++ concat (intersperse " " (map showLambda'' as))
showLambda'' (Var s) = s
showLambda'' (Lit s t) = s

showLambda (Lam a t e) = "\\" ++ a ++ showLambda' e
showLambda e = showLambda'' e

showSPFty (Ty s) = s
showSPFty (FnTy a b) = "<" ++ showSPFty a ++ "," ++ showSPFty b ++ ">"

showSPF (Lam a t e) = "(lambda " ++ a ++ ":" ++ showSPFty t ++ " " ++ showSPF e ++ ")"
showSPF (And es) = "(and:<t*,t> " ++ concat (intersperse " " (map showSPF es)) ++ ")"
showSPF (App h as) = "(" ++ showSPF h ++ " " ++ (concat (intersperse " " (map showSPF as))) ++ ")"
showSPF (Var s) = "$" ++ s
showSPF (Lit s t) = s ++ ":" ++ showSPFty t
