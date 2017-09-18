module Data.TreeDiff.Pretty (
    -- * Explicit dictionary
    Pretty (..),
    ppExpr,
    ppEditExpr,
    -- * pretty
    prettyPretty,
    prettyExpr,
    prettyEditExpr,
    -- * ansi-wl-pprint
    ansiWlPretty,
    ansiWlExpr,
    ansiWlEditExpr,
    -- * Utilities
    escapeName,
    ) where

import Data.Char          (isAlphaNum, isPunctuation, isSymbol, ord)
import Data.TreeDiff.Expr
import Numeric            (showHex)
import Text.Read          (readMaybe)

import qualified Data.Map                     as Map
import qualified Text.PrettyPrint             as HJ
import qualified Text.PrettyPrint.ANSI.Leijen as WL

-- | Because we don't want to commit to single pretty printing library,
-- we use explicit dictionary.
data Pretty doc = Pretty
    { ppCon        :: ConstructorName -> doc
    , ppRec        :: [(FieldName, doc)] -> doc
    , ppLst        :: [doc] -> doc
    , ppCpy        :: doc -> doc
    , ppIns        :: doc -> doc
    , ppDel        :: doc -> doc
    , ppSep        :: [doc] -> doc
    , ppParens     :: doc -> doc
    , ppHang       :: doc -> doc -> doc
    }

-- | Escape field or constructor name
--
-- >>> putStrLn $ escapeName "Foo"
-- Foo
--
-- >>> putStrLn $ escapeName "_,_"
-- _,_
--
-- >>> putStrLn $ escapeName "-3"
-- `-3`
--
-- >>> putStrLn $ escapeName "kebab-case"
-- kebab-case
--
-- >>> putStrLn $ escapeName "inner space"
-- `inner space`
--
-- >>> putStrLn $ escapeName $ show "looks like a string"
-- "looks like a string"
--
escapeName :: String -> String
escapeName n
    | null n                     = "``"
    | isValidString n            = n
    | all valid n && headNotMP n = n
    | otherwise                  = "`" ++ concatMap e n ++ "`"
  where
    e '`'               = "\\`"
    e '\\'              = "\\\\"
    e ' '               = " "
    e c | not (valid c) = "\\x" ++ showHex (ord c) ";"
    e c                 = [c]

    valid c = (isAlphaNum c || isSymbol c || isPunctuation c)
        && c `notElem` "[](){}`"

    headNotMP ('-' : _) = False
    headNotMP ('+' : _) = False
    headNotMP _         = True

    isValidString s = case readMaybe s :: Maybe String of
        Just _ -> True
        Nothing -> False

ppExpr :: Pretty doc -> Expr -> doc
ppExpr p = ppExpr' p False

ppExpr' :: Pretty doc -> Bool -> Expr -> doc
ppExpr' p = impl where
    impl _ (App x []) = ppCon p (escapeName x)
    impl b (App x xs) = ppParens' b $ ppHang p (ppCon p (escapeName x)) $
        ppSep p $ map (impl True) xs
    impl _ (Rec x xs) = ppHang p (ppCon p (escapeName x)) $ ppRec p $
        map ppField' $ Map.toList xs
    impl _ (Lst xs)   = ppLst p (map (impl False) xs)

    ppField' (n, e) = (escapeName n, impl False e)

    ppParens' True  = ppParens p
    ppParens' False = id

ppEditExpr :: Pretty doc -> Edit EditExpr -> doc
ppEditExpr p = ppSep p . ppEdit False
  where
    ppEdit b (Cpy (EditExp expr)) = [ ppCpy p $ ppExpr' p b expr ]
    ppEdit b (Cpy expr) = [ ppEExpr b expr ]
    ppEdit b (Ins expr) = [ ppIns p (ppEExpr b expr) ]
    ppEdit b (Del expr) = [ ppDel p (ppEExpr b expr) ]
    ppEdit b (Swp x y) =
        [ ppDel p (ppEExpr b x)
        , ppIns p (ppEExpr b y)
        ]

    ppEExpr _ (EditApp x []) = ppCon p (escapeName x)
    ppEExpr b (EditApp x xs) = ppParens' b $ ppHang p (ppCon p (escapeName x)) $
        ppSep p $ concatMap (ppEdit True) xs
    ppEExpr _ (EditRec x xs) = ppHang p (ppCon p (escapeName x)) $ ppRec p $
        map ppField' $ Map.toList xs
    ppEExpr _ (EditLst xs)   = ppLst p (concatMap (ppEdit False) xs)
    ppEExpr b (EditExp x)    = ppExpr' p b x

    ppField' (n, e) = (escapeName n, ppSep p $ ppEdit False e)

    ppParens' True  = ppParens p
    ppParens' False = id

-------------------------------------------------------------------------------
-- pretty
-------------------------------------------------------------------------------

-- | 'Pretty' via @pretty@ library.
prettyPretty :: Pretty HJ.Doc
prettyPretty = Pretty
    { ppCon    = HJ.text
    , ppRec    = HJ.braces . HJ.sep . HJ.punctuate HJ.comma
               . map (\(fn, d) -> HJ.text fn HJ.<+> HJ.equals HJ.<+> d)
    , ppLst    = HJ.brackets . HJ.sep . HJ.punctuate HJ.comma
    , ppCpy    = id
    , ppIns    = \d -> HJ.char '+' HJ.<> d
    , ppDel    = \d -> HJ.char '-' HJ.<> d
    , ppSep    = HJ.sep
    , ppParens = HJ.parens
    , ppHang   = \d1 d2 -> HJ.hang d1 2 d2
    }

-- | Pretty print 'Expr' using @pretty@.
prettyExpr :: Expr -> HJ.Doc
prettyExpr = ppExpr prettyPretty

-- | Pretty print @'Edit' 'EditExpr'@ using @pretty@.
prettyEditExpr :: Edit EditExpr -> HJ.Doc
prettyEditExpr = ppEditExpr prettyPretty

-------------------------------------------------------------------------------
-- ansi-wl-pprint
-------------------------------------------------------------------------------

ansiWlPretty :: Pretty WL.Doc
ansiWlPretty = Pretty
    { ppCon    = WL.text
    , ppRec    = WL.encloseSep WL.lbrace WL.rbrace WL.comma
               . map (\(fn, d) -> WL.text fn WL.<+> WL.equals WL.</> d)
    , ppLst    = WL.list
    , ppCpy    = WL.dullwhite
    , ppIns    = \d -> WL.green (WL.char '+' WL.<> d)
    , ppDel    = \d -> WL.red (WL.char '-' WL.<> d)
    , ppSep    = WL.sep
    , ppParens = WL.parens
    , ppHang   = \d1 d2 -> WL.hang 2 (d1 WL.</> d2)
    }

-- | Pretty print 'Expr' using @ansi-wl-pprint@.
ansiWlExpr :: Expr -> WL.Doc
ansiWlExpr = ppExpr ansiWlPretty

-- | Pretty print @'Edit' 'EditExpr'@ using @ansi-wl-pprint@.
ansiWlEditExpr :: Edit EditExpr -> WL.Doc
ansiWlEditExpr = ppEditExpr ansiWlPretty
