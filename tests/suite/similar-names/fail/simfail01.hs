data Person = Person { name :: String }

nAme :: Int
nAme = 1

main = do
  print nAme
  print (name Person { name = "Hello" })
