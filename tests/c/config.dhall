let 
  CType : Type =
    < CInt | CPtr : Text | CNoPtr : Text >
in

let
  CTest : Type =
    { haskellFile : Text
    , inputGenerators : List CType
    , outputPrinter : CType
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
 ,cTest = None CTest
 }

,{fileName = "filterLt.pika"
 ,cTest = Some
            { haskellFile = "tests/haskell/FilterLt.hs"
            , inputGenerators = [CType.CInt, CType.CNoPtr "_generateIntList"]
            , outputPrinter = CType.CPtr "_printIntList"
            }
 }

,{fileName = "mapAdd.pika"
 ,cTest = None CTest
 }

,{fileName = "leftList.pika"
 ,cTest = None CTest
 }

,{fileName = "treeSize.pika"
 ,cTest = None CTest
 }

,{fileName = "take.pika"
 ,cTest = None CTest
 }
]

