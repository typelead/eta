data REC = REC {l::Int,r::Int} deriving (Read,Show)

main = print (read "REC {l=3,r=4}" :: REC)
