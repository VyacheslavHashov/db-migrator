import Database.Migrator

main :: IO ()
main = putStrLn "Test suite not yet implemented"

a1 = MgNode (Migration "a1" "") $ CrossDeps []
a2 = MgNode (Migration "a2" "") $ CrossDeps []
a3 = MgNode (Migration "a3" "") $ CrossDeps []
a4 = MgNode (Migration "a4" "") $ CrossDeps [drop 2 mb]
a5 = MgNode (Migration "a5" "") $ CrossDeps []
b1 = MgNode (Migration "b1" "") $ CrossDeps []
b2 = MgNode (Migration "b2" "") $ CrossDeps [mc]
b3 = MgNode (Migration "b3" "") $ CrossDeps []
b4 = MgNode (Migration "b4" "") $ CrossDeps [drop 2 mc]
c1 = MgNode (Migration "c1" "") $ CrossDeps []
c2 = MgNode (Migration "c2" "") $ CrossDeps []
c3 = MgNode (Migration "c3" "") $ CrossDeps []
c4 = MgNode (Migration "c4" "") $ CrossDeps []
c5 = MgNode (Migration "c5" "") $ CrossDeps []

ma = reverse [a1, a2, a3, a4, a5]
mb = reverse [b1, b2, b3, b4]
mc = reverse [c1, c2, c3, c4, c5]

graph :: MgGraph
graph = M.fromList [("A", ma), ("B", mb), ("C", mc)]

-- TODO test for filenames
-- TODO test for file header
-- TODO test for reading migrations from disk

