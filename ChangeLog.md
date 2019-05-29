## 0.1

- Support GHC-7.4 ... 8.8 (use allow-newer for GHC-8.8-alpha).
- Use raw GHC.Generics (drop `generics-sop` dependency)
- Use own memoising (Vector lookup, drop `MemoTrie` dependency)
- Singleton data-types (both `data` and `newtype`s) are printed in App form (i.e. no-record).
- Change license to GPL-2.0-or-later

## 0.0.2.1

- Support semigroups-0.19, hashable-1.3 and generics-sop-0.5

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
