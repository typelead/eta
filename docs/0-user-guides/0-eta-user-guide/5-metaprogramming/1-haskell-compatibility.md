# Haskell Compatibility

## Overview

In this section, we will discuss how Eta's metaprogramming interacts with Haskell's metaprogramming. This section is important to understand if you are using a Haskell package in your Eta project that uses Template Haskell.

## Boot Packages

Most of Eta's metaprogramming facilities are provided by the `eta-meta` boot package which will be installed with Eta itself. The `eta-meta` package is equivalent to the `template-haskell` package in Haskell. In Eta, `template-haskell` is patched by re-exporting `eta-meta`'s modules.

| Eta Module                             | Haskell Module                                       |
| ------------------------------         | ---------------------------------------------------- |
| `Language.Eta.Meta`                    | `Language.Haskell.TH`                                |
| `Language.Eta.Meta.Lib`                | `Language.Haskell.TH.Lib`                            |
| `Language.Eta.Meta.Ppr`                | `Language.Haskell.TH.Ppr`                            |
| `Language.Eta.Meta.PprLib`             | `Language.Haskell.TH.PprLib`                         |
| `Language.Eta.Meta.Quote`              | `Language.Haskell.TH.Quote`                          |
| `Language.Eta.Meta.Syntax`             | `Language.Haskell.TH.Syntax`                         |
| `Language.Eta.Meta.LanguageExtensions` | `Language.Haskell.TH.LanguageExtensions`             |
| `Language.Eta.Meta.Binary`             | `Language.Haskell.TH.Binary`                         |

If you wish to use any Eta-specific functionality or Abstract Syntax Tree extensions, then you should use `eta-meta` directly. If you are attempting to make your project compilable by both GHC and Eta, use `template-haskell`.
