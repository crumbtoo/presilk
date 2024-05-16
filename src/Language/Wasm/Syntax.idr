module Language.Wasm.Syntax
--------------------------------------------------------------------------------
import Data.String
import Text.PrettyPrint.Prettyprinter.Doc
import Text.PrettyPrint.Prettyprinter.Render.String
import Derive.Prelude
--------------------------------------------------------------------------------
%language ElabReflection
%prefix_record_projections off
%default total
--------------------------------------------------------------------------------

public export
LocalIdx : Type
LocalIdx = Nat

public export
TypeIdx : Type
TypeIdx = Nat

namespace SectionId
  public export
  data SectionId = Custom
                 | Type_
                 | Import
                 | Function
                 | Table
                 | Memory
                 | Global
                 | Export
                 | Start
                 | Element
                 | Code
                 | Data

public export
record Section (a : Type) where
  constructor MkSection
  id      : SectionId
  content : a

namespace Type
  public export
  data HeapType = NoFunc
                | NoExtern
                | None
                | Any
                | Eq
                | I31
                | Struct
                | Index TypeIdx


  public export
  record RefType where
    constructor MkRefType
    null : Bool
    ht : HeapType

  public export
  data NumType = I32
               | I64
               | F32
               | F64

  public export
  data ValType = Num NumType
               | Ref RefType
               | I8
               | I16

  %runElab derive "HeapType" [Eq]
  %runElab derive "RefType" [Eq]
  %runElab derive "NumType" [Eq]
  %runElab derive "ValType" [Eq]

  public export
  infixr 1 :->

  public export
  record FuncType where
    constructor (:->)
    from : List ValType
    to   : List ValType

  public export
  record FieldType where
    constructor MkFieldType
    t   : ValType
    mut : Bool

  public export
  record StructType where
    constructor MkStructType
    fields : List FieldType

  namespace Comp
    public export
    data CompType = Func FuncType | Struct StructType

  public export
  record SubType where
    constructor MkSubType
    final  : Bool
    supers : List TypeIdx
    t      : CompType

  public export
  RecType : Type
  RecType = List SubType

||| Shorthand to create a non-subtype
non : CompType -> SubType
non = MkSubType False []

--------------------------------------------------------------------------------

public export
data Instr : List ValType -> Type where
  I32_const : Int32 -> Instr [Num I32]
  I32_mul : Instr [Num I32] -> Instr [Num I32] -> Instr [Num I32]
  Local_get : LocalIdx -> Instr [t]

public export
record Func t where
  constructor MkFunc
  locals : List ValType
  body   : Instr t

-- public export
-- record Expr s t where
--   constructor MkExpr

--------------------------------------------------------------------------------

public export
record Module where
  constructor MkModule
  typesec : Section (List RecType)
  funcsec : Section (List TypeIdx)
  codesec : Section (List (DPair (List ValType) Func))

--------------------------------------------------------------------------------
-- examples

export
twoTimesThree : Instr [Num I32]
twoTimesThree = I32_mul (I32_const 2) (I32_const 3)

export
exampleModule : Module
exampleModule = MkModule
  { typesec = MkSection
      { id = Type_
      , content =
        [ [ non $ Struct . MkStructType $
              [ MkFieldType (Num I32) False
              , MkFieldType (Num I32) False
              ]
          ]
        , [ non $ Func $ [Num I32] :-> [Num I32]
          ]
        ]
      }
  , funcsec = MkSection
      { id = Function
      , content = [1]
      }
  , codesec = MkSection
      { id = Code
      , content =
        [ ([Num I32] ** MkFunc
           { locals = []
           , body = I32_mul (I32_const 3) (Local_get 0)
           })
        ]
      }
  }

