module Jann01 where

@Export 1.02
@Export2 "Hello"
@Hello {  }
@Hello1 []
@Hello2 [ "name", "name", "name" ]
data Hello = Hello { thing :: String, value :: String }

@Hello { value = "hello" }
data Hello1 = Hello1 {
    @Value
    thing1 :: String,
    @Override
    @Mutable { name = "Hello" }
    value1 :: String
  }

@Hello { value = "hello" }
data Hello2 = @Hello Hello2 {
    @Value
    thing2 :: String,
    @Override
    @Mutable { name = "Hello" }
    value2 :: String
  }
  | @Export @Hello
    @Export
    Hello3 { @Hello name :: String }

@Named "Hello"
val :: String
val = "String"
