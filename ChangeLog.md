# Revision history for tree-diff

## 0.0.2

- Print compact diffs
- Add `ToExpr` instance for `ShortByteString`

## 0.0.1

- Use `BS.readFile` and `BS.writeFile` in `ediffGolden`.
  This makes files read and written in UTF8 independently of locale.
  Fixes builds on appveyor.

## 0.0.0.1

- Move to `build-type: Simple`

## 0

- First version. Released on an unsuspecting world.
