import Data.Tree
import Database.Migrator

main :: IO ()
main = putStrLn "Test suite not yet implemented"

a1 = Node (Migration "a1" "") []
a2 = Node (Migration "a2" "") [a1]
a3 = Node (Migration "a3" "") [a2]
a4 = Node (Migration "a4" "") [a3, b3]
a5 = Node (Migration "a5" "") [a4]
b1 = Node (Migration "b1" "") []
b2 = Node (Migration "b2" "") [b1, c1]
b3 = Node (Migration "b3" "") [b2]
b4 = Node (Migration "b4" "") [b3, c3]
c1 = Node (Migration "c1" "") []
c2 = Node (Migration "c2" "") [c1]
c3 = Node (Migration "c3" "") [c2]
c4 = Node (Migration "c4" "") [c3]
c5 = Node (Migration "c5" "") [c4]

