-- | Utilities to pretty print 'Expr' and 'EditExpr'
module Data.TreeDiff.Pretty (
    -- * Explicit dictionary
    Pretty (..),
    ppExpr,
    ppEditExpr,
    ppEditExprCompact,
    -- * pretty
    prettyPretty,
    prettyExpr,
    prettyEditExpr,
    prettyEditExprCompact,
    -- * ansi-wl-pprint
    ansiWlPretty,
    ansiWlExpr,
    ansiWlEditExpr,
    ansiWlEditExprCompact,
    -- ** background
    ansiWlBgPretty,
    ansiWlBgExpr,
    ansiWlBgEditExpr,
    ansiWlBgEditExprCompact,
    -- * Utilities
    escapeName,
) where

import Data.Char          (isAlphaNum, isPunctuation, isSymbol, ord)
import Data.Either        (partitionEithers)
import Data.TreeDiff.Expr
import Numeric            (showHex)
import Text.Read          (readMaybe)

import qualified Data.TreeDiff.OMap           as OMap
import qualified Text.PrettyPrint             as HJ
import qualified Text.PrettyPrint.ANSI.Leijen as WL

-- $setup
-- >>> import qualified Data.TreeDiff.OMap as OMap
-- >>> import Data.TreeDiff.Expr

-- | Because we don't want to commit to single pretty printing library,
-- we use explicit dictionary.
data Pretty doc = Pretty
    { ppCon    :: ConstructorName -> doc            -- ^ Display 'ConstructorName'
    , ppApp    :: doc -> [doc] -> doc               -- ^ Display 'App'
    , ppRec    :: doc -> [(FieldName, doc)] -> doc  -- ^ Display 'Rec'
    , ppLst    :: [doc] -> doc                      -- ^ Display 'Lst'
    , ppCpy    :: doc -> doc                        -- ^ Display unchanged parts
    , ppIns    :: doc -> doc                        -- ^ Display added parts
    , ppDel    :: doc -> doc                        -- ^ Display removed parts
    , ppEdits  :: [doc] -> doc                      -- ^ Combined edits (usually some @sep@ combinator)
    , ppEllip  :: doc                               -- ^ Ellipsis
    , ppParens :: doc -> doc                        -- ^ Parens an expression
    }

-- | Escape field or constructor name
--
-- >>> putStrLn $ escapeName "Foo"
-- Foo
--
-- >>> putStrLn $ escapeName "_×_"
-- _×_
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
-- >>> putStrLn $ escapeName $ show "tricky" ++ "   "
-- `"tricky"   `
--
-- >>> putStrLn $ escapeName "[]"
-- `[]`
--
-- >>> putStrLn $ escapeName "_,_"
-- `_,_`
--
escapeName :: String -> String
escapeName n
    | null n                      = "``"
    | isValidString n             = n
    | all valid' n && headNotMP n = n
    | otherwise                   = "`" ++ concatMap e n ++ "`"
  where
    e '`'               = "\\`"
    e '\\'              = "\\\\"
    e ' '               = " "
    e c | not (valid c) = "\\x" ++ showHex (ord c) ";"
    e c                 = [c]

    valid c = isAlphaNum c || isSymbol c || isPunctuation c
    valid' c = valid c && c `notElem` "[](){}`\","

    headNotMP ('-' : _) = False
    headNotMP ('+' : _) = False
    headNotMP _         = True

    isValidString s
        | length s >= 2 && head s == '"' && last s == '"' =
            case readMaybe s :: Maybe String of
                Just _ -> True
                Nothing -> False
    isValidString _         = False

-- | Pretty print an 'Expr' using explicit pretty-printing dictionary.
ppExpr :: Pretty doc -> Expr -> doc
ppExpr p = ppExpr' p False

ppExpr' :: Pretty doc -> Bool -> Expr -> doc
ppExpr' p = impl where
    impl _ (App x []) = ppCon p (escapeName x)
    impl b (App x xs) = ppParens' b $ ppApp p (ppCon p (escapeName x)) (map (impl True) xs)
    impl _ (Rec x xs) = ppRec p (ppCon p (escapeName x)) $
        map ppField' $ OMap.toList xs
    impl _ (Lst xs)   = ppLst p (map (impl False) xs)

    ppField' (n, e) = (escapeName n, impl False e)

    ppParens' True  = ppParens p
    ppParens' False = id

-- | Pretty print an @'Edit' 'EditExpr'@ using explicit pretty-printing dictionary.
ppEditExpr :: Pretty doc -> Edit EditExpr -> doc
ppEditExpr = ppEditExpr' False

-- | Like 'ppEditExpr' but print unchanged parts only shallowly
ppEditExprCompact :: Pretty doc -> Edit EditExpr -> doc
ppEditExprCompact = ppEditExpr' True

ppEditExpr' :: Bool -> Pretty doc -> Edit EditExpr -> doc
ppEditExpr' compact p = go
  where
    go = ppEdits p . ppEdit False

    ppEdit b (Cpy (EditExp expr)) = [ ppCpy p $ ppExpr' p b expr ]
    ppEdit b (Cpy expr) = [ ppEExpr b expr ]
    ppEdit b (Ins expr) = [ ppIns p (ppEExpr b expr) ]
    ppEdit b (Del expr) = [ ppDel p (ppEExpr b expr) ]
    ppEdit b (Swp x y) =
        [ ppDel p (ppEExpr b x)
        , ppIns p (ppEExpr b y)
        ]

    ppEExpr _ (EditApp x []) = ppCon p (escapeName x)
    ppEExpr b (EditApp x xs) = ppParens' b $ ppApp p (ppCon p (escapeName x)) (concatMap (ppEdit True) xs)
    ppEExpr _ (EditRec x xs) = ppRec p (ppCon p (escapeName x)) $
        justs ++ [ (n, ppEllip p) | n <- take 1 nothings ]
      where
        xs' = map ppField' $ OMap.toList xs
        (nothings, justs) = partitionEithers xs'

    ppEExpr _ (EditLst xs)   = ppLst p (concatMap (ppEdit False) xs)
    ppEExpr b (EditExp x)    = ppExpr' p b x

    ppField' (n, Cpy (EditExp e)) | compact, not (isScalar e) = Left n
    ppField' (n, e) = Right (escapeName n, go e)

    ppParens' True  = ppParens p
    ppParens' False = id

    isScalar (App _ []) = True
    isScalar _          = False

-------------------------------------------------------------------------------
-- pretty
-------------------------------------------------------------------------------

-- | 'Pretty' via @pretty@ library.
prettyPretty :: Pretty HJ.Doc
prettyPretty = Pretty
    { ppCon    = HJ.text
    , ppRec    = \c xs -> prettyGroup (c HJ.<+> HJ.char '{') (HJ.char '}')
               $ map (\(fn, d) -> HJ.sep [HJ.text fn HJ.<+> HJ.equals, d]) xs
    , ppLst    = prettyGroup (HJ.char '[') (HJ.char ']')
    , ppCpy    = id
    , ppIns    = \d -> HJ.char '+' HJ.<> d
    , ppDel    = \d -> HJ.char '-' HJ.<> d
    , ppEdits  = HJ.sep
    , ppEllip  = HJ.text "..."
    , ppApp    = \f xs -> HJ.sep [ f, HJ.nest 2 $ HJ.sep xs ]
    , ppParens = HJ.parens
    }

prettyGroup :: HJ.Doc -> HJ.Doc -> [HJ.Doc] -> HJ.Doc
prettyGroup l r xs = HJ.cat [l, HJ.sep (map (HJ.nest 2) (prettyPunct (HJ.char ',') r xs))]

prettyPunct :: HJ.Doc -> HJ.Doc -> [HJ.Doc] -> [HJ.Doc]
prettyPunct _   end []     = [end]
prettyPunct _   end [x]    = [x HJ.<> end]
prettyPunct sep end (x:xs) = (x HJ.<> sep) : prettyPunct sep end xs

-- | Pretty print 'Expr' using @pretty@.
--
-- >>> prettyExpr $ Rec "ex" (OMap.fromList [("[]", App "bar" [])])
-- ex {`[]` = bar}
prettyExpr :: Expr -> HJ.Doc
prettyExpr = ppExpr prettyPretty

-- | Pretty print @'Edit' 'EditExpr'@ using @pretty@.
prettyEditExpr :: Edit EditExpr -> HJ.Doc
prettyEditExpr = ppEditExpr prettyPretty

-- | Compact 'prettyEditExpr'.
prettyEditExprCompact :: Edit EditExpr -> HJ.Doc
prettyEditExprCompact = ppEditExprCompact prettyPretty

-------------------------------------------------------------------------------
-- ansi-wl-pprint
-------------------------------------------------------------------------------

-- | 'Pretty' via @ansi-wl-pprint@ library (with colors).
ansiWlPretty :: Pretty WL.Doc
ansiWlPretty = Pretty
    { ppCon    = WL.text
    , ppRec    = \c xs -> ansiGroup (c WL.<+> WL.lbrace) WL.rbrace
               $ map (\(fn, d) -> WL.text fn WL.<+> WL.equals WL.</> d) xs
    , ppLst    = ansiGroup WL.lbracket WL.rbracket
    , ppCpy    = WL.dullwhite
    , ppIns    = \d -> WL.green $ WL.plain $ WL.char '+' WL.<> d
    , ppDel    = \d -> WL.red   $ WL.plain $ WL.char '-' WL.<> d
    , ppApp    = \f xs -> WL.group $ WL.nest 2 $ f WL.<$> WL.vsep xs
    , ppEdits  = WL.sep
    , ppEllip  = WL.text "..."
    , ppParens = WL.parens
    }

ansiGroup :: WL.Doc -> WL.Doc -> [WL.Doc] -> WL.Doc
ansiGroup l r xs = WL.group $ WL.nest 2 (l WL.<$$> WL.vsep (WL.punctuate WL.comma xs) WL.<> r)

-- | Pretty print 'Expr' using @ansi-wl-pprint@.
ansiWlExpr :: Expr -> WL.Doc
ansiWlExpr = ppExpr ansiWlPretty

-- | Pretty print @'Edit' 'EditExpr'@ using @ansi-wl-pprint@.
ansiWlEditExpr :: Edit EditExpr -> WL.Doc
ansiWlEditExpr = ppEditExpr ansiWlPretty

-- | Compact 'ansiWlEditExpr'
ansiWlEditExprCompact :: Edit EditExpr -> WL.Doc
ansiWlEditExprCompact = ppEditExprCompact ansiWlPretty

-------------------------------------------------------------------------------
-- Background
-------------------------------------------------------------------------------

-- | Like 'ansiWlPretty' but color the background.
ansiWlBgPretty :: Pretty WL.Doc
ansiWlBgPretty = ansiWlPretty
    { ppIns    = \d -> WL.ondullgreen $ WL.white $ WL.plain $ WL.char '+' WL.<> d
    , ppDel    = \d -> WL.ondullred   $ WL.white $ WL.plain $ WL.char '-' WL.<> d
    }

-- | Pretty print 'Expr' using @ansi-wl-pprint@.
ansiWlBgExpr :: Expr -> WL.Doc
ansiWlBgExpr = ppExpr ansiWlBgPretty

-- | Pretty print @'Edit' 'EditExpr'@ using @ansi-wl-pprint@.
ansiWlBgEditExpr :: Edit EditExpr -> WL.Doc
ansiWlBgEditExpr = ppEditExpr ansiWlBgPretty

-- | Compact 'ansiWlBgEditExpr'.
ansiWlBgEditExprCompact :: Edit EditExpr -> WL.Doc
ansiWlBgEditExprCompact = ppEditExprCompact ansiWlBgPretty
