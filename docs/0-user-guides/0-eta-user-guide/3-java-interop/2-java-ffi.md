# Java Foreign Imports

## General Syntax

The general syntax for Java Foreign Import Declarations is:


```eta
foreign import java [safety] "[import-string]" [eta-identifier]
  :: [arg-type-1] -> [arg-type-2] -> .. -> [return-type]
```

1. `[safety]` - `safe` or `unsafe`.
    - `unsafe` is the option you would typically select. In this case, the java method identified in the `[import-string]` will be run directly. This can be dangerous if the function can block in which case it will block the Eta RTS and reduce efficiency.
    - `safe` is the option you would select for functions that you would expect to block for some time, so they will be safely run in another thread to prevent the call from blocking the Eta RTS. This option must also be used when importing a Java method that eventually calls an exported Eta function.

2. `[import-string]` can take the following forms:
    - `[java-method-name]`: Binds to an instance method. `[java-method-name]` should be an unqualified Java instance method name.
    - `@static [java-method-name]`: Binds to a static method. `[java-method-name]` should be a fully qualified Java static method name.
    - `@new`: Binds to a constructor. The class to construct will be determined by the return type of the declaration.
    - `@field [java-field-name]`: Binds to a getter or setter of an instance field, determined by the type signature. `[java-field-name]` should be an unqualified Java instance field name.
    - `@static @field [java-field-name]`: Binds to a getter or setter of a field, determined by the type signature. `[java-field-name]` should be a fully qualified Java static field name.
    - `@interface [java-interface-method]`: Binds to an interface method, determined by the type signature. `[java-interface-name]` should be a unqualified Java interface method name.
    - `@wrapper [java-interface-method]`: Used for generating an Eta function that will generate an interface implementation, determined by the type signature. `[java-interface-name]` should be a unqualified Java interface method name. See [Working With Java Interfaces](/docs/user-guides/eta-user-guide/java-interop/java-generics#working-with-java-interfaces) for more information.
    - `@wrapper @abstract [java-abstract-method]`: Used for generating an Eta function that will generate an abstract class implementation, determined by the type signature. `[java-method]` should be a unqualified Java abstract method name. See [Working With Java Interfaces](/docs/user-guides/eta-user-guide/java-interop/java-generics#working-with-java-interfaces) for more information.
    - Not present: If you do not specify an import string, it will be taken as an instance method import and the `[java-method-name]` is taken to be the same as `[eta-identifier]`.

3. `[eta-identifier]` should be a valid Eta identifier that will be used for calling the corresponding Java method inside of Eta code.

4. `[argTypeN]` should be a marshallable Eta type. See [Marshalling Between Java and Eta Types](/docs/user-guides/eta-user-guide/java-interop/jwts#marshalling-between-java-and-eta-types).
5. `[returnType]` can be of three forms:
    - `Java [jwt] [return-type]`: This is the form that is used typically and is always safe to use. `[jwt]` should be the JWT for the class which the declaration pertains. If the declaration has a `@static` annotation, this can be left free with a type variable instead of a concrete type. `[return-type]` should be a marshallable Eta type.
    - `IO [return-type]`: This form is also safe and can be used for convenience. Note that if the import string does not have a `@static` annotation, you must supply the relevant JWT as the first argument (`[argType1]`). `[return-type]` should be a marshallable Eta type.
    - `[return-type]`: This form has no monadic context and should only be used for immutable Java objects whose methods do not perform any side effects. Note that if the declaration does not have a `@static` annotation, you must supply the relevant JWT as the first argument (`[argType1]`). `[return-type]` should be a marshallable Eta type.

## Importing Instance Methods

Let’s import the `boolean canExecute()` [instance method](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#canExecute()) from the [java.io.File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html) class.



The following are all equivalent ways of performing the import:



```eta
data File = File @java.io.File
  deriving Class

foreign import java unsafe canExecute :: Java File Bool

foreign import java unsafe "canExecute" canExecute1 :: Java File Bool

foreign import java unsafe "canExecute" canExecute2 :: File -> IO Bool

-- Note: The example below is shown for illustration purposes and should never
-- be done in practice because "canExecute" is not a pure function.

foreign import java unsafe "canExecute" canExecute3 :: File -> Bool
```

## Importing Static Methods

Let’s import the `File createTempFile(String, String)` [static method](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String)) from the [java.io.File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html) class.



The following are all equivalent ways of performing the import:



```eta
data File = File @java.io.File
  deriving Class

foreign import java unsafe "@static java.io.File.createTempFile"
  createTempFile  :: String -> String -> Java a File

foreign import java unsafe "@static java.io.File.createTempFile"
  createTempFile1 :: String -> String -> IO File

-- Note: The example below is shown for illustration purposes and should never
-- be done in practice because "createTempFile" is not a pure function.

foreign import java unsafe "@static java.io.File.createTempFile"
  createTempFile2 :: String -> String -> File
```

## Importing Constructors

Let’s import the `File(String)` [constructor](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#File(java.lang.String)) from the [java.io.File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html) class.



The following are all equivalent ways of performing the import:



```eta
data File = File @java.io.File
  deriving Class

foreign import java unsafe "@new" newFile  :: String -> Java a File

foreign import java unsafe "@new" newFile1 :: String -> IO File

foreign import java unsafe "@new" newFile2 :: String -> File
```

## Importing Instance Fields

Let’s import the `private String path` [instance field](http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/7u40-b43/java/io/File.java/#165) from the [java.io.File](http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/7u40-b43/java/io/File.java) class. Note that the imports shown below are purely for illustration purposes and will throw an exception if called because `path` is a private field.



The following are all equivalent ways of performing the get/set imports:



```eta
data File = File @java.io.File
  deriving Class

-- Imports for getting the field
foreign import java unsafe "@field path" getFilePath  :: Java File String

foreign import java unsafe "@field path" getFilePath1 :: File -> IO String

foreign import java unsafe "@field path" getFilePath2 :: File -> String

-- Imports for setting the field.
foreign import java unsafe "@field path" setFilePath  :: String -> Java File ()
foreign import java unsafe "@field path" setFilePath1 :: File -> String -> IO ()
-- Note that setting the value of a field is always an impure operation so a pure
-- import is not supported by the compiler
```

## Importing Static Fields

Let’s import the `String pathSeparator` [static field](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#pathSeparator) from the [java.io.File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html) class.



The following are all equivalent ways of performing the get/set imports:



```eta
-- Imports for getting the field

foreign import java unsafe "@static @field java.io.File.pathSeparator"
  getPathSeparator  :: Java a String

foreign import java unsafe "@static @field java.io.File.pathSeparator"
  getPathSeparator1 :: IO String

foreign import java unsafe "@static @field java.io.File.pathSeparator"
  getPathSeparator2 :: String

-- Imports for setting the field.
-- NOTE: These imports are only shown for illustration purposes, but they will
-- crash if used since `pathSeparator` is a final field.

foreign import java unsafe "@static @field java.io.File.pathSeparator"
  setPathSeparator  :: String -> Java a ()

foreign import java unsafe "@static @field java.io.File.pathSeparator"
  setPathSeparator1 :: String -> IO ()
```

## Working with the Java Monad

Now that we've gotten an idea of how to use imports, how do we use them in Eta code? Eta code must eventually run in the IO monad and we currently don't know how that can be done if we have an import that runs in the Java monad.



In the [Java](https://github.com/typelead/eta/blob/master/libraries/base/Java/Core.hs#L37) module in the base package, the following functions are available:



```eta
-- Execute a Java action in the IO monad.
java :: Java c a -> IO a

-- Execute a Java action in the IO monad with respect to the
-- given object.
javaWith :: (Class c) => c -> Java c a -> IO a

-- Execute a Java action in the Java monad of another class
-- with respect to the given object.
(<.>) :: (Class c) => c -> Java c a -> Java b a
withObject :: (Class c) => c -> Java c a -> Java b a

-- Chain Java actions.
(>-) :: (Class b) => Java a b -> Java b c -> Java a c

-- Execute an IO action inside of the Java monad
io :: IO a -> Java c a

-- Execute a Java action purely, i.e. order of execution does not matter.
unsafePerformJava :: Java c a -> a

-- Analagous to `javaWith`, but pure.
unsafePerformJavaWith :: (Class c) => c -> Java c a -> a
```

Using the imports from above, we can write the following program:



```eta
main :: IO ()
main = do
  executes <- java $ do
    file <- newFile "./dir/prog.exe"
    io $ putStrLn "Executing an IO action inside of Java!"
    file <.> canExecute
  if executes
  then putStrLn "File can execute!"
  else putStrLn "File cannot execute!"
```

Using different combinators, we can write it like this:



```eta
main :: IO ()
main = do
  -- Similar to Java code:
  -- File file = new File("./dir/prog.exe");
  file <- java $ newFile "./dir/prog.exe"
  putStrLn "Executing an IO action inside of Java!"
  -- Similar to Java code:
  -- boolean executes = file.canExecute();
  executes <- javaWith file canExecute
  if executes
  then putStrLn "File can execute!"
  else putStrLn "File cannot execute!"
```

Or:



```eta
main :: IO ()
main = java $ do
  -- Similar to Java code:
  -- boolean executes = new File("./dir/prog.exe").canExecute();
  executes <- newFile "./dir/prog.exe" >- canExecute
  io $ putStrLn "Executing an IO action inside of Java!"
  if executes
  then io $ putStrLn "File can execute!"
  else io $ putStrLn "File cannot execute!"
```

## Next Section

We will now proceed with handling arrays and subclasses.
