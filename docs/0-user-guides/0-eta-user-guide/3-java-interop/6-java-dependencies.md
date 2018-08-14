# Java Dependencies

## Overview

If you are using Gradle, you can skip this section since have native support for Java dependencies.

Continue if you are directly using Etlas.

## Adding Java Files

You can include Java-related files like `.java`, `.class`, `.jar` files in your project by adding their paths to the `java-sources:` field in the Cabal file.

In this example, we’ll take a look at including Java source files in our project and importing the defined methods into Eta for use.

Setup a project, just like [Quick Start](/docs/user-guides/eta-user-guide/basics/quick-start), with the following changes:

1.  **Main.hs**

    ```eta
    import Java

    foreign import java unsafe "@static eta.first.Utils.createFile"
      createFile :: String -> IO ()

    -- Creates an empty file
    main :: IO ()
    main = createFile "HelloWorld.txt"
    ```

2.  Create a new folder called `java` and a file `Utils.java` with the following contents:

    ```java
    package eta.first;

    import java.nio.file.Files;
    import java.nio.file.Paths;

    public class Utils {

      /* This helper method lets us avoid variadic arguments which
         are a bit cumbersome to work with in Eta. */

      public static void createFile(String path) {
        Files.createFile(Paths.get(path));
      }
    }
    ```

    Your directory structure should look like this:

    ```
    eta-first
      -src
        -Main.hs
      -java
        -Utils.java
      -eta-first.cabal
      -Setup.hs
    ```

    Your directory structure may vary based on the options you chose in the `etlas init` step.

3.  Update `eta-first.cabal`, adding a `java-sources:` field:

    ```
    java-sources: java/Utils.java
    ```

    **NOTE:**

    You can add more Java-based files indented under the first entry with either relative or absolute paths. You can thus include arbitrary `.jar` files or even individual `.class` files that you need.

4.  That’s it! Run the example with `etlas run`.


## Adding Maven Dependencies

You can check out the section on [JVM Dependencies](/docs/user-guides/etlas-user-guide/configuration/dependency-management) for more information on the syntax used in the example.

### Example

In this example, we’ll be binding to the [Unirest](http://unirest.io/java) library.

Setup a project, just like [Quick Start](/docs/user-guides/eta-user-guide/basics/quick-start), with the following changes:

1.  Update `eta-first.cabal`, adding a `maven-depends:` field:

    ```
    maven-depends: com.mashape.unirest:unirest-java:1.4.9
    ```

2.  **Main.hs**

    ```eta
    {-# LANGUAGE FlexibleContexts, DataKinds, TypeFamilies #-}

    import Java

    -- Imports from the Unirest API
    data BaseRequest = BR @com.mashape.unirest.request.BaseRequest
      deriving Class

    data GetRequest = GR @com.mashape.unirest.request.GetRequest
      deriving Class

    type instance Inherits GetRequest = '[BaseRequest]

    data HttpResponse a = HResp (@com.mashape.unirest.http.HttpResponse a)
      deriving Class

    foreign import java unsafe "@static com.mashape.unirest.http.Unirest.shutdown"
      shutdownUnirest :: IO ()

    foreign import java unsafe "@static com.mashape.unirest.http.Unirest.get"
      get :: String -> Java a GetRequest

    foreign import java unsafe asString
      :: (a <: BaseRequest) => Java a (HttpResponse JString)

    foreign import java unsafe getBody
      :: (a <: Object) => Java (HttpResponse a) a

    -- Run a simple blocking GET request
    main :: IO ()
    main = do
      response <- java $ do
            get "https://jsonplaceholder.typicode.com/posts/1"
          >- asString
          >- getBody
      putStrLn $ read (show response)
      shutdownUnirest
    ```

3.  That’s it! Run the example with `etlas run`.

## Next Module

In the next module, we will cover functions.
