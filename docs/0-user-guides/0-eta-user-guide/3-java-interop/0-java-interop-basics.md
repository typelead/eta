# Basics

## Prerequisites

You must have a basic understanding of sequenceables/monads to understand the rest of the section. Please check out the chapter on **Sequenceable** in [Tour of Eta](https://tour.eta-lang.org) for better understanding.

## Quick Start

When interfacing with Java, you should import the `Java` module from the standard library:



```eta
import Java
```

This will import the `Java` monad and related helper functions for working inside the monad.



Consider the following Java code:



```java
package eta.example;

public class Counter {

  private int counter;

  public int publicCounter;

  public static final int COUNTER_MAX = 1000;

  public static int numCounters;

  public Counter() {}

  public Counter(int initial) {
    this.counter = initial;
  }

  public void increment() {
    this.counter = Math.min(this.counter + 1, COUNTER_MAX);
    this.publicCounter = counter;
  }

  public int get() {
    return counter;
  }

  public void set(int value) {
    this.counter = Math.min(value, COUNTER_MAX);
    this.publicCounter = counter;
  }
}
```

A Java method is simply a function that takes an object as an implicit argument bound to the `this` variable. Implicit contexts such as these can be represented as a monad, a state monad to be specific. A state monad threads state through each monadic action so the state is being passed around internally even though it’s not visible in the code.



This correspondence is the basis for the built-in `Java` monad in Eta.



The above example can be imported as follows:



```eta
data Counter = Counter @eta.example.Counter
  deriving Class

foreign import java unsafe "@new" newCounter :: Java a Counter
foreign import java unsafe "@new" newCounterWith :: Int -> Java a Counter
foreign import java unsafe increment :: Java Counter ()
foreign import java unsafe get :: Java Counter Int
foreign import java unsafe set :: Int -> Java Counter ()
foreign import java unsafe "@static @field eta.example.Counter.COUNTER_MAX"
  cOUNTER_MAX :: Java a Int
foreign import java unsafe "@field publicCounter" getPublicCounter
  :: Java Counter Int
foreign import java unsafe "@field publicCounter" setPublicCounter
  :: Int -> Java Counter ()
foreign import java unsafe "@static @field numCounters" getNumCounters
  :: Java a Int
foreign import java unsafe "@static @field numCounters" setNumCounters
  :: Int -> Java a ()
```

## Defining a Java Wrapper Type

When working with the FFI, you need a way to refer to a given Java class inside of Eta. This is done through Java Wrapper Types (JWTs).



### General Syntax

```eta
data X = X @[class-name]
  deriving Class
```

- `[class-name]` should be the fully qualified Java class name and `X` should be the Eta name you would use to refer to the corresponding Java class in foreign imports. Note that `[class-name]` can also be converted to an array type by appending `[]`.

- The `Class` typeclass is a built-in typeclass that is a marker for a JWT. **Make sure all your JWTs derive a Class instance**.


## Working with Java Converters

In Eta, there is a clear distinction JWTs and normal Eta types. Moreover, only JWTs can be used in foreign imports/exports.

### The JavaConverter typeclass

JWTs are inconvenient to use directly in Eta because they are just wrappers of native Java objects. So, the following typeclass is defined in the standard library to help convert JWTs to common Eta types like lists.

```eta
-- The `a` type variable should be a normal Eta type
-- The `b` type variable should be a JWT or a primitive type (Byte, Short, Int, ...)
class JavaConverter a b where
  toJava   :: a -> b
  fromJava :: b -> a
```

Many instances are provided for you by default so you can simply use toJava or fromJava whenever you want to perform a conversion.

### Note :

`String` is a notable exception to that rule because it’s so commonly used that there’s a special case that allows it an automatically converts it to `JString`.

## The Java Monad

As mentioned before, the `Java` monad is used to contain the implicit `this` context. It can be effectively thought of as a state monad with a given Java object as the state.



```eta
newtype Java c a = Java {- Internal definition -}
```

As can be seen from the above definition, the `Java` monad has two type parameters `c` and `a`. The `c` parameter is the type of the implicit `this` context, and should be some JWT, and the `a` parameter is the return type of the monad.

### Working with the Java Monad

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
For instance, if the following functions are available:

```eta
newFile  :: String -> Java a File
canExecute :: Java File Bool
```
Then it is possible to write the following program:

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

## Java Foreign Import Declarations

Foreign import declarations are used to import a Java method as an Eta monadic action, typically in the Java monad.



### Simplified Syntax

The full syntax of the `foreign import` declaration is quite complex, but only a subset is needed here:

```eta
foreign import java unsafe "[import-string]" [eta-identifier]
  :: [arg-type-1] -> [arg-type-2] -> .. -> [return-type]
```

1. `[import-string]` can take the following forms:
    - `[java-method-name]`: Binds to an instance method. `[java-method-name]` should be an unqualified Java instance method name.
    - `@static [java-method-name]`: Binds to a static method. `[java-method-name]` should be a fully qualified Java static method name.
    - `@new`: Binds to a constructor. The class to construct will be determined by the return type of the declaration.
    - `@field [java-field-name]`: Binds to a getter or setter of an instance field, determined by the type signature. `[java-field-name]` should be an unqualified Java instance field name.
    - `@static @field [java-field-name]`: Binds to a getter or setter of a field, determined by the type signature. `[java-field-name]` should be a fully qualified Java static field name.

2. `[eta-identifier]` should be a valid Eta identifier that will be used for calling the corresponding Java method inside of Eta code.

3. `[argTypeN]` should be a marshallable Eta type. See [Marshalling Between Java and Eta Types](/docs/user-guides/eta-user-guide/java-interop/jwts#marshalling-between-java-and-eta-types).
4. `[returnType]` can be of three forms:
    - `Java [jwt] [return-type]`: This is the form that is used typically and is always safe to use. `[jwt]` should be the JWT for the class which the declaration pertains. If the declaration has a `@static` annotation, this can be left free with a type variable instead of a concrete type. `[return-type]` should be a marshallable Eta type.
    - `IO [return-type]`: This form is also safe and can be used for convenience. Note that if the import string does not have a `@static` annotation, you must supply the relevant JWT as the first argument (`[argType1]`). `[return-type]` should be a marshallable Eta type.
    - `[return-type]`: This form has no monadic context and should only be used for immutable Java objects whose methods do not perform any side effects. Note that if the declaration does not have a `@static` annotation, you must supply the relevant JWT as the first argument (`[argType1]`). `[return-type]` should be a marshallable Eta type.

For the full syntax, see [General Syntax](/docs/user-guides/eta-user-guide/java-interop/java-ffi#general-syntax).

## Exporting Eta Methods

Just as you can import Java methods into Eta, you can also export Eta functions into Java.



### General Syntax


```eta
foreign export java "[export-string]" [eta-identifier]
  :: [arg-type-1] -> [arg-type-2] -> .. -> [return-type]
```

1. `[export-string]` can take the following forms:
    - `[java-method-name]`: Binds to an instance method. `[java-method-name]` should be an unqualified Java instance method name. If not informed the instance method name will be equal to `[eta-identifier]`.
    - `@static [java-method-name]`: Binds to a static method. `[java-method-name]` should be a fully qualified Java static method name (e.g. `"@static com.org.SomeClass.someMethodName"`).

2. `[eta-identifier]` should be a valid Eta identifier for an existing Eta function that is the target of the export.

3. `[arg-type-n]` should be a marshallable Eta type.

4. `[returnType]` can be of three forms:
    - `Java [export-jwt] [return-type]`: This is the form that should be used if you want to export an instance method, although it can be used for a static method too.
        - `[export-jwt]` should be the JWT that refers to the class name of the exported class. If (and only if) the declaration has a `@static` annotation, this can be left free with a type variable instead of a concrete type. Also, the JWT *must not* be an imported JWT but a new one defined in eta. The export will create a new class with an instance method. The typical use case is to define a new JWT class that inherits from an existing java abstract class, implementing the abstract method or methods with one or more exports. However you could use a non abstract class too.
        - `[return-type]` should be a marshallable Eta type.
    - `IO [return-type]` or simply `[return-type]`: These forms can be used for convenience. Note that if you use any of them the export string *must* have a `@static [java-method-name]` annotation.
        - `[return-type]` should be a marshallable Eta type.
### Examples
- Exporting static methods:

```eta
foreign export java "@static eta.example.MyClass.sayHello" sayHelloEta :: IO ()
sayHelloEta = putStrLn "Hi"

foreign export java "@static eta.example.Numbers.zero" zero :: IO Int
zero = do
 putStrLn "Returning zero from eta"
 return 0

foreign export java "@static eta.example.Numbers.one" one :: Int
one = 1

foreign export java "@static eta.example.Numbers.addTwo" addTwo :: Int -> Java a Int
addTwo x = return $ x + 2

```

- Exporting an instance method for a new class that inherits from an existing one. Given an existing class `eta.example.Counter` like the defined above we can create another class in eta that inherits from it, adding one or more methods that can use the definitions of the superclass:

```eta

-- Importing an existing class
data JavaCounter = JavaCounter @eta.example.Counter
   deriving Class

-- eta.example.EtaCounter will be generated by eta
data EtaCounter = EtaCounter @eta.example.EtaCounter
   deriving Class

-- Required to make EtaCounter a subclass of JavaCounter
type instance Inherits EtaCounter = '[JavaCounter]

-- Importing methods from JavaCounter with ( c <: JavaCounter ) constraint
-- to make it works for its subclasses (including EtaCounter)
-- see: https://eta-lang.org/docs/user-guides/eta-user-guide/java-interop/arrays-subclasses#problem-resolution
foreign import java unsafe get :: ( c <: JavaCounter )
                               => Java c Int

foreign import java unsafe set :: ( c <: JavaCounter )
                               => Int -> Java c ()

-- implement an eta function that uses methods and state from the superclass
decrement :: Int -> Java EtaCounter Int
decrement x = do
  c <- get
  let c' = max (c - x) 0
  set c'
  return c'

foreign export java decrement :: Int -> Java EtaCounter Int
```

- Use of exported methods from the java side:

```java
// static methods
eta.example.MyClass.sayHello();
System.out.println("Zero: " + eta.example.Numbers.zero());
System.out.println("One: " + eta.example.Numbers.one());
System.out.println("Two: " + eta.example.Numbers.addTwo(0));

// instance method
eta.example.EtaCounter d = new eta.example.EtaCounter();
d.set(10);
System.out.println(d.decrement(5));
System.out.println(d.get());
```

## Next Section

We will now proceed with Java Wrapper Types in detail.
