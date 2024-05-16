module Language.Wasm.Binary
--------------------------------------------------------------------------------
import Data.ByteString as BS
import System.File
import Data.Bits
import Data.Fin
import Data.List1
import Data.List
import Data.Vect
import Language.Wasm.Syntax
--------------------------------------------------------------------------------
%prefix_record_projections off
%default total
--------------------------------------------------------------------------------

encodeLEB : Bits64 -> List Bits8
encodeLEB m =
  case m <= 127 of
       True => [cast m]
       False => let b = cast m `setBit` 7
                    k = m `assert_smaller` (m `shiftR` 7)
                in b :: encodeLEB k

encodeSLEB : Int64 -> List Bits8
encodeSLEB n =
  case done of
       True  => [b]
       False => b' :: encodeSLEB (n `assert_smaller` n')
  where
    b : Bits8
    b = cast $ n .&. 0x7f
    b' = b .|. 0x80
    n' = n `shiftR` 7
    done = (n' == 0 && (b .&. 0x40) == 0)
        || (n' == -1 && (b .&. 0x40) /= 0)

putU32 : Bits32 -> List Bits8
putU32 = encodeLEB . cast

putI32 : Int32 -> List Bits8
putI32 = encodeSLEB . cast

putVec : (List a -> List Bits8) -> List a -> List Bits8
putVec f v = putU32 (cast $ length v) ++ f v

putVec' : (a -> List Bits8) -> List a -> List Bits8
putVec' f v = putU32 (cast $ length v) ++ foldMap f v

--------------------------------------------------------------------------------

putSectionId : SectionId -> Bits8
putSectionId sid =
  case sid of
       Custom   => 0
       Type_    => 1
       Import   => 2
       Function => 3
       Table    => 4
       Memory   => 5
       Global   => 6
       Export   => 7
       Start    => 8
       Element  => 9
       Code     => 10
       Data     => 11

putSection : (a -> List Bits8) -> Section a
          -> List Bits8
putSection f (MkSection i cnt) =
  if l == 0
     then []
     else putSectionId i :: putU32 (cast $ length cnt') ++ cnt'
  where
    cnt' = f cnt
    l : Bits32
    l = cast $ length cnt'

--------------------------------------------------------------------------------
-- types

putNumType : NumType -> Bits8
putNumType x =
  case x of
       I32          => 0x7f
       I64          => 0x7e
       F32          => 0x7d
       F64          => 0x7c

||| Heap types are encoded as either a single byte, or as a type index encoded
||| as a positive signed integer.
putHeapType : HeapType -> List Bits8
putHeapType NoFunc    = [0x73]
putHeapType NoExtern  = [0x72]
putHeapType None      = [0x71]
putHeapType Any       = [0x6f]
putHeapType Eq        = [0x6d]
putHeapType I31       = [0x6c]
putHeapType Struct    = [0x6b]
putHeapType (Index k) = encodeSLEB (cast k)

putRefType : RefType -> List Bits8
putRefType (MkRefType False ht) = [0x64]
putRefType (MkRefType True ht)  = 0x63 :: putHeapType ht

putValType : ValType -> List Bits8
putValType (Num nt) = [putNumType nt]
putValType (Ref rt) = putRefType rt
putValType I8       = [0x78]
putValType I16      = [0x77]
  --      Ref False ht => [0x64]
  --      Ref True  ht => 0x63 :: putValType ht
  --      NoFunc       => ?b
  --      NoExtern     => ?c
  --      None         => ?d
  --      Any          => ?e
  --      Eq           => ?f
  --      I31          => ?g
  --      Struct       => ?h
  --      Index n      => ?a

putResultType : (ls : List ValType) -> List Bits8
putResultType ls = putU32 (cast $ length ls) ++ (foldMap putValType ls)

putFuncType : FuncType -> List Bits8
putFuncType ft = putResultType ft.from ++ putResultType ft.to 

putMut : Bool -> Bits8
putMut False = 0x00
putMut True  = 0x01

putFieldType : FieldType -> List Bits8
putFieldType (MkFieldType t mut) = putValType t ++ [putMut mut]

putStructType : StructType -> List Bits8
putStructType (MkStructType fields) = putVec' putFieldType fields

putCompType : CompType -> List Bits8
putCompType (Func x)   = 0x60 :: putFuncType x
putCompType (Struct x) = 0x5f :: putStructType x

putSubType : SubType -> List Bits8
putSubType (MkSubType _ [] t) = putCompType t
putSubType (MkSubType final sts t) =
    b :: putVec' (pure . cast) sts ++ putCompType t
  where
    b : Bits8
    b = if final then 0x4f else 0x50

putRecType : RecType -> List Bits8
putRecType [st] = putSubType st
putRecType sts  = ?putRecType_rhs_1

--------------------------------------------------------------------------------
-- code

groupTypes : List ValType -> List (Nat, ValType)
groupTypes = map (\ts => (length ts, head ts)) . group

putLocals : List ValType -> List Bits8
putLocals ls = putVec' (uncurry putLocals') $ groupTypes ls
  where
    putLocals' : Nat -> ValType -> List Bits8
    putLocals' n t = putU32 (cast n) ++ putValType t

-- https://webassembly.github.io/gc/core/binary/instructions.html
putInstr : Instr t -> List Bits8
putInstr (I32_const i) = 0x41 :: putI32 i
putInstr (I32_mul x y) = putInstr x ++ putInstr y ++ [0x6c]
putInstr (Local_get i) = 0x20 :: putU32 (cast i)

putExpr : Instr t -> List Bits8
putExpr e = putInstr e ++ [0x0b]

putFunc : DPair (List ValType) Func -> List Bits8
putFunc (t ** (MkFunc ls body)) = putLocals ls ++ putExpr body

putCode : DPair (List ValType) Func -> List Bits8
putCode c = putU32 (cast $ length c') ++ c'
  where c' = putFunc c

--------------------------------------------------------------------------------

export
putModule : Module -> List Bits8
putModule m = magic ++ version ++ contents
  where
    magic : List Bits8
    magic   = [0x00, 0x61, 0x73, 0x6d]
    version : List Bits8
    version = [0x01, 0x00, 0x00, 0x00]
    contents = putSection (putVec' putRecType) m.typesec
            ++ putSection (putVec' (putU32 . cast)) m.funcsec
            ++ putSection (putVec' putCode) m.codesec

export
writeTest : IO ()
writeTest = do
    let m = exampleModule
        bs = putModule m
        bs' = BS.pack bs
        err = \e => print e *> pure ()
    r <- withFile "/tmp/out.wasm" WriteTruncate err $ \f =>
           writeByteString f bs' *> pure (Right ())
    case r of
         Left e => print e
         Right _ => pure ()

