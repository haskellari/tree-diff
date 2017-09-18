# tree-diff

## Examples

![](https://raw.githubusercontent.com/phadej/tree-diff/master/cabal-diff.png)

![](https://raw.githubusercontent.com/phadej/tree-diff/master/cabal-diff-2.png)

## Pretty-printing

### pretty

```haskell
prettyP :: Pretty PP.Doc
prettyP = Pretty
    { ppCon    = PP.text
    , ppRec    = PP.braces . PP.sep . PP.punctuate PP.comma
               . map (\(fn, d) -> PP.text fn PP.<+> PP.equals PP.<+> d)
    , ppLst    = PP.brackets . PP.punctuate PP.comma . PP.sep
    . ppCpy    = id
    , ppIns    = \d -> PP.char '+' PP.<> d
    , ppDel    = \d -> PP.char '-' PP.<> d
    , ppSep    = PP.sep
    , ppParens = PP.parens
    , ppHang   = \d1 d2 -> PP.hang d1 2 d2
    }
```

### ansi-wl-pprint

```haskell
prettyP :: Pretty PP.Doc
prettyP = Pretty
    { ppCon    = PP.text
    , ppRec    = PP.encloseSep PP.lbrace PP.rbrace PP.comma
               . map (\(fn, d) -> PP.text fn PP.<+> PP.equals PP.<+> d)
    , ppLst    = PP.list
    , ppCpy    = PP.dullwhite
    , ppIns    = \d -> PP.green (PP.char '+' PP.<> d)
    , ppDel    = \d -> PP.red (PP.char '-' PP.<> d)
    , ppSep    = PP.sep
    , ppParens = PP.parens
    , ppHang   = \d1 d2 -> PP.hang 2 (d1 PP.</> d2)
    }
```
