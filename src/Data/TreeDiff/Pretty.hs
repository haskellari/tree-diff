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

import qualified Data.TreeDiff.OMap            as OMap
import qualified Prettyprinter                 as PP
import qualified Prettyprinter.Render.Terminal as PP

type PlainDoc = PP.Doc ()
type AnsiDoc = PP.Doc PP.AnsiStyle

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
prettyPretty :: Pretty PlainDoc
prettyPretty = Pretty
    { ppCon    = PP.pretty
    , ppRec    = \c xs -> prettyGroup (c PP.<+> PP.pretty '{') (PP.pretty '}')
               $ map (\(fn, d) -> PP.sep [PP.pretty fn PP.<+> PP.equals, d]) xs
    , ppLst    = prettyGroup (PP.pretty '[') (PP.pretty ']')
    , ppCpy    = id
    , ppIns    = \d -> PP.pretty '+' PP.<> d
    , ppDel    = \d -> PP.pretty '-' PP.<> d
    , ppEdits  = PP.sep
    , ppEllip  = PP.pretty "..."
    , ppApp    = \f xs -> PP.sep [ f, PP.nest 2 $ PP.sep xs ]
    , ppParens = PP.parens
    }

prettyGroup :: PlainDoc -> PlainDoc -> [PlainDoc] -> PlainDoc
prettyGroup l r xs = PP.cat [l, PP.sep (map (PP.nest 2) (prettyPunct (PP.pretty ',') r xs))]

prettyPunct :: PlainDoc -> PlainDoc -> [PlainDoc] -> [PlainDoc]
prettyPunct _   end []     = [end]
prettyPunct _   end [x]    = [x PP.<> end]
prettyPunct sep end (x:xs) = (x PP.<> sep) : prettyPunct sep end xs

-- | Pretty print 'Expr' using @pretty@.
--
-- >>> prettyExpr $ Rec "ex" (OMap.fromList [("[]", App "bar" [])])
-- ex {`[]` = bar}
prettyExpr :: Expr -> PlainDoc
prettyExpr = ppExpr prettyPretty

-- | Pretty print @'Edit' 'EditExpr'@ using @pretty@.
prettyEditExpr :: Edit EditExpr -> PlainDoc
prettyEditExpr = ppEditExpr prettyPretty

-- | Compact 'prettyEditExpr'.
prettyEditExprCompact :: Edit EditExpr -> PlainDoc
prettyEditExprCompact = ppEditExprCompact prettyPretty

-------------------------------------------------------------------------------
-- ansi-wl-pprint
-------------------------------------------------------------------------------

-- | 'Pretty' via @ansi-wl-pprint@ library (with colors).
ansiWlPretty :: Pretty AnsiDoc
ansiWlPretty = Pretty
    { ppCon    = PP.pretty
    , ppRec    = \c xs -> ansiGroup (c PP.<+> PP.lbrace) PP.rbrace
               $ map (\(fn, d) -> PP.pretty fn PP.<+> PP.equals <> PP.softline <> d) xs
    , ppLst    = ansiGroup PP.lbracket PP.rbracket
    , ppCpy    = PP.annotate (PP.colorDull PP.White)
    , ppIns    = \d -> PP.annotate (PP.color PP.Green) $ PP.unAnnotate $ PP.pretty '+' PP.<> d
    , ppDel    = \d -> PP.annotate (PP.color PP.Red)   $ PP.unAnnotate $ PP.pretty '-' PP.<> d
    , ppApp    = \f xs -> PP.group $ PP.nest 2 $ f <> PP.line <> PP.vsep xs
    , ppEdits  = PP.sep
    , ppEllip  = PP.pretty "..."
    , ppParens = PP.parens
    }

ansiGroup :: AnsiDoc -> AnsiDoc -> [AnsiDoc] -> AnsiDoc
ansiGroup l r xs = PP.group $ PP.nest 2 (l <> linebreak <> PP.vsep (PP.punctuate PP.comma xs) PP.<> r)

linebreak :: PP.Doc ann
linebreak = PP.flatAlt PP.line mempty

-- | Pretty print 'Expr' using @ansi-wl-pprint@.
ansiWlExpr :: Expr -> AnsiDoc
ansiWlExpr = ppExpr ansiWlPretty

-- | Pretty print @'Edit' 'EditExpr'@ using @ansi-wl-pprint@.
ansiWlEditExpr :: Edit EditExpr -> AnsiDoc
ansiWlEditExpr = ppEditExpr ansiWlPretty

-- | Compact 'ansiWlEditExpr'
ansiWlEditExprCompact :: Edit EditExpr -> AnsiDoc
ansiWlEditExprCompact = ppEditExprCompact ansiWlPretty

-------------------------------------------------------------------------------
-- Background
-------------------------------------------------------------------------------

-- | Like 'ansiWlPretty' but color the background.
ansiWlBgPretty :: Pretty AnsiDoc
ansiWlBgPretty = ansiWlPretty
    { ppIns    = \d -> PP.annotate (PP.bgColorDull PP.Green) $ PP.annotate (PP.color PP.White) $ PP.unAnnotate $ PP.pretty '+' PP.<> d
    , ppDel    = \d -> PP.annotate (PP.bgColorDull PP.Red)   $ PP.annotate (PP.color PP.White) $ PP.unAnnotate $ PP.pretty '-' PP.<> d
    }

-- | Pretty print 'Expr' using @ansi-wl-pprint@.
ansiWlBgExpr :: Expr -> AnsiDoc
ansiWlBgExpr = ppExpr ansiWlBgPretty

-- | Pretty print @'Edit' 'EditExpr'@ using @ansi-wl-pprint@.
ansiWlBgEditExpr :: Edit EditExpr -> AnsiDoc
ansiWlBgEditExpr = ppEditExpr ansiWlBgPretty

-- | Compact 'ansiWlBgEditExpr'.
ansiWlBgEditExprCompact :: Edit EditExpr -> AnsiDoc
ansiWlBgEditExprCompact = ppEditExprCompact ansiWlBgPretty
