let 
  CType : Type =
    < CInt | CPtr : Text | CNoPtr : Text >
in

let
  BenchKind : Type =
    < List | Tree >
in

let
  CTest : Type =
    { haskellFile : Text
    , inputGenerators : List CType
    , outputPrinter : CType
    , kind : BenchKind
    }
in
[{fileName = "cons.pika"
 ,cTest = None CTest
 }

,{fileName = "plus.pika"
 ,cTest = None CTest
 }

,{fileName = "add1Head.pika"
 ,cTest = None CTest
 }

,{fileName = "listId.pika"
 ,cTest = None CTest
 }

,{fileName = "add1HeadDLL.pika"
 ,cTest = None CTest
 }

,{fileName = "even.pika"
 ,cTest = None CTest
 }

,{fileName = "foldr.pika"
 ,cTest = None CTest
 }

,{fileName = "sum.pika"
 ,cTest = Some
            { haskellFile = "tests/haskell/Sum.hs"
            , inputGenerators = [CType.CNoPtr "_generateIntList"]
            , outputPrinter = CType.CNoPtr "_printInt"
            , kind = BenchKind.List
            }
 }

,{fileName = "filterLt.pika"
 ,cTest = Some
            { haskellFile = "tests/haskell/FilterLt.hs"
            , inputGenerators = [CType.CInt, CType.CNoPtr "_generateIntList"]
            , outputPrinter = CType.CPtr "_printIntList"
            , kind = BenchKind.List
            }
 }

,{fileName = "mapAdd.pika"
 ,cTest = Some
            { haskellFile = "tests/haskell/MapAdd.hs"
            , inputGenerators = [CType.CInt, CType.CNoPtr "_generateIntList"]
            , outputPrinter = CType.CPtr "_printIntList"
            , kind = BenchKind.List
            }
 }

,{fileName = "leftList.pika"
 ,cTest = Some
            { haskellFile = "tests/haskell/LeftList.hs"
            , inputGenerators = [CType.CNoPtr "_generateBinaryTree"]
            , outputPrinter = CType.CPtr "_printIntList"
            , kind = BenchKind.Tree
            }
 }

,{fileName = "treeSize.pika"
 ,cTest = Some
            { haskellFile = "tests/haskell/TreeSize.hs"
            , inputGenerators = [CType.CNoPtr "_generateBinaryTree"]
            , outputPrinter = CType.CNoPtr "_printInt"
            , kind = BenchKind.Tree
            }
 }

,{fileName = "take.pika"
 ,cTest = Some
            { haskellFile = "tests/haskell/Take.hs"
            , inputGenerators = [CType.CNoPtr "_generateNat", CType.CNoPtr "_generateIntList"]
            , outputPrinter = CType.CPtr "_printIntList"
            , kind = BenchKind.List
            }
 }
]

