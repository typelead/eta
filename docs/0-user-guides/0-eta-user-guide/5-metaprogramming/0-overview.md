# Overview

## Introduction

Metaprogramming provides a way to manipulate code as you would data. Eta provides a form of metaprogramming called [template metaprogramming](https://en.wikipedia.org/wiki/Template_metaprogramming) based on GHC's TemplateHaskell. Template metaprogramming allows you to instantiate code templates by supplying the appropriate parameters for a given situation.

## Example

In the example below, `makeLenses` is a template function from the [lens](http://hackage.haskell.org/package/lens) package that is evaluated at compile-time and generates some code. The function takes a single parameter: a type.

```eta
import Control.Lens

data Person = Person { _me :: String,
                       _age :: Double }

makeLenses ''Person
```

The code above will simplify to the code below once the template function `makeLenses` gets run during compilation.

```eta
import Control.Lens

data Person = Person { _me :: String,
                       _age :: Double }

age :: Lens' Person Double
age f (Person x1 x2) = fmap (\x -> Person x1 x) (f x2)
{-# INLINE age #-}

me :: Lens' Person String
me f (Person x1 x2) = fmap (\x -> Person x x2) (f x1)
{-# INLINE me #-}
```

## More Information

In the future, there will be more detailed documentation on the metaprogramming facilities offered by Eta. In the meantime, you can check out [this](https://markkarpov.com/tutorial/th.html) link for a quick introduction to Template Haskell. Most of the code supplied should work the same as it does in Haskell.

## Next Section

In the next section, we will discuss the compatibility of Eta's template metaprogramming with Haskell.
