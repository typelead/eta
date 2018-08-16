# The API

## Designing the API

When we go about building a web service, we start by asking ourselves what sort of functionality the web service should provide to the clients.



For our example service, we will consider a single endpoint that computes the Fibonacci sequence at a given index.




The full specification looks something like this:



```
API Endpoint: GET /fibonacci/:n
Expected Output:

n >= 0:

Status Code: 200

{
   "n":<n>,
   "fib": <fib(n)>
}

n < 0:

Status Code: 412

Precondition Failed: n >= 0

```

## Implementing fib

We'll start by implementing the core `fib` function.



```eta
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

## Declaring the API Type

By convention, we declare a type called `API` that consists of the specification for the web service.



```eta
type API = "fibonacci" :> Capture "n" Int :> Get '[JSON] Value
```

### Notes :

- We assign a type `Int` to the captured `:n` segment of the route
- We return a `Value` which is the JSON type provided by the [aeson](https://hackage.haskell.org/package/aeson) package.
- We have completely declared our API as a type using the types provided by the [Servant DSL](https://hackage.haskell.org/package/servant-0.12/docs/Servant-API.html).

## Declaring the Proxy API Type

Then, we define a proxy type which we can lets us pass around our `API` type as if it's a type argument.



```eta
api :: Proxy API
api = Proxy
```

Note that the `Proxy` type only has a single value that can be constructed with the `Proxy` data constructor, but can take on any type based on the demand. Proxy types are typically used for sending type arguments to functions.

## Implementing the Server

We will now define the handler for the route we defined in the `API` type.



```eta
server :: Application
server = serve api fibHandler
  where fibHandler n
          | n >= 0 = return $ object ["n" .= n, "fib" .= fib n]
          | otherwise = throwError $ err412 { errBody = "Precondition Failed: n > 0" }
```

Note the type of `fibHandler` is:



```eta
fibHandler :: Int -> Handler Value
```

## Run!

We're now ready to run out web app.



```eta
main :: IO ()
main = run 9000 server
```

## Download

You can download the full source code for this app and run by following the steps below:



```
git clone https://github.com/typelead/eta-fib-service
cd eta-fib-service
etlas run
```

In another terminal window:



```sh
curl http://localhost:9000/fibonacci/20
```
