module Tptcc.CType
  ( CType (..)
  , Member (..)
  , TypeKind (..)
  , base
  , baseWithSigned
  , baseFromSpecifiers
  , pointer
  , array
  , function
  , struct
  , union
  , enum
  , sizeof
  , withMemberOffsets
  , memberByName
  , sameTypeChain
  , renderType
  , renderTypePretty
  ) where

import Data.Char (isAsciiLower)

data TypeKind
  = Void
  | Char
  | Short
  | Int
  | Long
  | Struct
  | Union
  | Pointer
  | Array
  | Function
  | Enum
  deriving (Eq, Ord, Show)

data CType
  = BaseType {typeKind :: TypeKind, typeSigned :: Bool}
  | PointerType {pointsTo :: CType}
  | ArrayType {arrayLength :: Integer, pointsTo :: CType}
  | FunctionType {returnType :: CType, parameterTypes :: [CType]}
  | StructType {typeId :: String, members :: [Member]}
  | UnionType {typeId :: String, members :: [Member]}
  | EnumType {typeId :: String, enumMembers :: [String]}
  deriving (Eq, Show)

data Member = Member
  { memberName :: String
  , memberType :: CType
  , memberOffset :: Maybe Integer
  }
  deriving (Eq, Show)

base :: String -> CType
base kind = baseWithSigned kind True

baseWithSigned :: String -> Bool -> CType
baseWithSigned kind signed = BaseType {typeKind = baseKind kind, typeSigned = signed}

baseFromSpecifiers :: [String] -> CType
baseFromSpecifiers specifiers =
  baseWithSigned kind signed
  where
    normalized = map (map toUpperAscii) specifiers
    signed = "UNSIGNED" `notElem` normalized
    kind
      | "VOID" `elem` normalized = "VOID"
      | "CHAR" `elem` normalized = "CHAR"
      | "SHORT" `elem` normalized = "SHORT"
      | "LONG" `elem` normalized = "LONG"
      | otherwise = "INT"

pointer :: CType -> CType
pointer = PointerType

array :: Integer -> CType -> CType
array = ArrayType

function :: CType -> [CType] -> CType
function = FunctionType

struct :: String -> [Member] -> CType
struct = StructType

union :: String -> [Member] -> CType
union = UnionType

enum :: String -> [String] -> CType
enum = EnumType

sizeof :: CType -> Integer
sizeof ty =
  case ty of
    ArrayType len target -> len * sizeof target
    StructType _ members' -> sum (map (sizeof . memberType) members')
    UnionType _ members' -> maximum (0 : map (sizeof . memberType) members')
    _ -> 1

withMemberOffsets :: Bool -> [Member] -> [Member]
withMemberOffsets isStruct members'
  | isStruct = reverse (snd (foldl addStructMember (0, []) members'))
  | otherwise = [member {memberOffset = Just 0} | member <- members']
  where
    addStructMember (offset, acc) member =
      (offset + sizeof (memberType member), member {memberOffset = Just offset} : acc)

memberByName :: String -> [Member] -> Maybe Member
memberByName wanted =
  go
  where
    go [] = Nothing
    go (member : rest)
      | memberName member == wanted = Just member
      | otherwise = go rest

sameTypeChain :: CType -> CType -> Bool -> Bool
sameTypeChain lhs rhs allowLengthMismatch =
  case (lhs, rhs) of
    (PointerType a, PointerType b) -> sameTypeChain a b allowLengthMismatch
    (ArrayType la a, ArrayType lb b) ->
      (allowLengthMismatch || la == lb) && sameTypeChain a b allowLengthMismatch
    (BaseType ka sa, BaseType kb sb) -> ka == kb && sa == sb
    (FunctionType ra pa, FunctionType rb pb) -> ra == rb && pa == pb
    (StructType ida _, StructType idb _) -> ida == idb
    (UnionType ida _, UnionType idb _) -> ida == idb
    (EnumType ida _, EnumType idb _) -> ida == idb
    _ -> False

renderType :: CType -> String
renderType ty =
  case ty of
    PointerType target -> renderType target <> "*"
    ArrayType len target -> renderType target <> "[" <> renderLength len <> "]"
    _ -> renderKind (typeKindOf ty)

renderTypePretty :: CType -> String
renderTypePretty = renderPrettyChain . reverse . typeChain

typeChain :: CType -> [CType]
typeChain ty =
  case ty of
    PointerType target -> ty : typeChain target
    ArrayType _ target -> ty : typeChain target
    _ -> [ty]

renderPrettyChain :: [CType] -> String
renderPrettyChain [] = "?"
renderPrettyChain (ty : modifiers) = renderPrettyModifiers (renderPrettyAtom ty) modifiers

renderPrettyModifiers :: String -> [CType] -> String
renderPrettyModifiers rendered [] = rendered
renderPrettyModifiers rendered modifiers@(ArrayType {} : _) =
  let (arrays, rest) = span isArrayType modifiers
      renderedArrays = concatMap renderArrayModifier (reverse arrays)
   in renderPrettyModifiers (rendered <> renderedArrays) rest
renderPrettyModifiers rendered (PointerType {} : rest) = renderPrettyModifiers (rendered <> "*") rest
renderPrettyModifiers rendered (ty : rest) = renderPrettyModifiers (rendered <> renderPrettyAtom ty) rest

renderPrettyAtom :: CType -> String
renderPrettyAtom ty =
  case ty of
    FunctionType ret params ->
      "FUNCTION((" <> concatMap ((<> ", ") . renderTypePretty) params <> ") -> " <> renderTypePretty ret <> ")"
    _ -> renderKind (typeKindOf ty)

isArrayType :: CType -> Bool
isArrayType ArrayType {} = True
isArrayType _ = False

renderArrayModifier :: CType -> String
renderArrayModifier (ArrayType len _) = "[" <> renderLength len <> "]"
renderArrayModifier _ = ""

renderLength :: Integer -> String
renderLength len
  | len >= 0 = show len
  | otherwise = "?"

typeKindOf :: CType -> TypeKind
typeKindOf (BaseType kind _) = kind
typeKindOf PointerType {} = Pointer
typeKindOf ArrayType {} = Array
typeKindOf FunctionType {} = Function
typeKindOf StructType {} = Struct
typeKindOf UnionType {} = Union
typeKindOf EnumType {} = Enum

baseKind :: String -> TypeKind
baseKind kind =
  case map toUpperAscii kind of
    "VOID" -> Void
    "CHAR" -> Char
    "SHORT" -> Short
    "INT" -> Int
    "LONG" -> Long
    "STRUCT" -> Struct
    "UNION" -> Union
    "ENUM" -> Enum
    other -> error ("invalid base type kind: " <> other)

renderKind :: TypeKind -> String
renderKind kind =
  case kind of
    Void -> "VOID"
    Char -> "CHAR"
    Short -> "SHORT"
    Int -> "INT"
    Long -> "LONG"
    Struct -> "STRUCT"
    Union -> "UNION"
    Pointer -> "POINTER"
    Array -> "ARRAY"
    Function -> "FUNCTION"
    Enum -> "ENUM"

toUpperAscii :: Char -> Char
toUpperAscii c
  | isAsciiLower c = toEnum (fromEnum c - 32)
  | otherwise = c
