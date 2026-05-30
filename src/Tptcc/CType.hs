module Tptcc.CType
  ( CType (..)
  , Member (..)
  , TypeKind (..)
  , base
  , baseWithSigned
  , baseFromSpecifiers
  , isBaseSpecifiers
  , isIntegerType
  , isPointerType
  , isScalarType
  , integerPromotion
  , usualArithmeticConversion
  , defaultArgumentPromotion
  , applyPointers
  , decayArrayParameter
  , decayExpressionType
  , dereferenceType
  , dereferenceTypeMaybe
  , compatiblePointerTargets
  , compatiblePointerTypes
  , pointer
  , array
  , function
  , variadicFunction
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

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

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
  | FunctionType {returnType :: CType, parameterTypes :: [CType], functionIsVariadic :: Bool}
  | StructType {typeId :: Text, members :: [Member]}
  | UnionType {typeId :: Text, members :: [Member]}
  | EnumType {typeId :: Text, enumMembers :: [Text]}
  deriving (Eq, Show)

data Member = Member
  { memberName :: Text
  , memberType :: CType
  , memberOffset :: Maybe Integer
  }
  deriving (Eq, Show)

base :: Text -> CType
base kind = baseWithSigned kind True

baseWithSigned :: Text -> Bool -> CType
baseWithSigned kind signed = BaseType {typeKind = baseKind kind, typeSigned = signed}

baseFromSpecifiers :: [Text] -> CType
baseFromSpecifiers specifiers =
  baseWithSigned kind signed
  where
    normalized = map Text.toUpper specifiers
    signed = "UNSIGNED" `notElem` normalized
    kind
      | "VOID" `elem` normalized = "VOID"
      | "CHAR" `elem` normalized = "CHAR"
      | "SHORT" `elem` normalized = "SHORT"
      | "LONG" `elem` normalized = "LONG"
      | otherwise = "INT"

isBaseSpecifiers :: [Text] -> Bool
isBaseSpecifiers specifiers =
  not (null normalized)
    && all (`elem` baseSpecifierWords) normalized
    && length signSpecifiers <= 1
    && not (baseSpecifiers == ["VOID"] && not (null signSpecifiers))
    && validBaseWords baseSpecifiers
  where
    normalized = map Text.toUpper specifiers
    signSpecifiers = filter (`elem` ["SIGNED", "UNSIGNED"]) normalized
    baseSpecifiers = filter (`notElem` ["SIGNED", "UNSIGNED"]) normalized

baseSpecifierWords :: [Text]
baseSpecifierWords = ["VOID", "CHAR", "SHORT", "INT", "LONG", "SIGNED", "UNSIGNED"]

validBaseWords :: [Text] -> Bool
validBaseWords words' =
  case words' of
    [] -> True
    ["VOID"] -> True
    ["CHAR"] -> True
    ["INT"] -> True
    ["SHORT"] -> True
    ["SHORT", "INT"] -> True
    ["INT", "SHORT"] -> True
    ["LONG"] -> True
    ["LONG", "INT"] -> True
    ["INT", "LONG"] -> True
    _ -> False

isIntegerType :: CType -> Bool
isIntegerType ty =
  case ty of
    BaseType Void _ -> False
    BaseType {} -> True
    EnumType {} -> True
    _ -> False

isPointerType :: CType -> Bool
isPointerType PointerType {} = True
isPointerType _ = False

isScalarType :: CType -> Bool
isScalarType ty = isIntegerType ty || isPointerType ty

integerPromotion :: CType -> Maybe CType
integerPromotion ty =
  case ty of
    BaseType Char _ -> Just (base "INT")
    BaseType Short _ -> Just (base "INT")
    BaseType Int signed -> Just (BaseType Int signed)
    BaseType Long signed -> Just (BaseType Long signed)
    EnumType {} -> Just (base "INT")
    _ -> Nothing

usualArithmeticConversion :: CType -> CType -> Maybe CType
usualArithmeticConversion lhs rhs = do
  lhs' <- integerPromotion lhs
  rhs' <- integerPromotion rhs
  combinePromotedIntegers lhs' rhs'

defaultArgumentPromotion :: CType -> CType
defaultArgumentPromotion ty = fromMaybe ty (integerPromotion ty)

applyPointers :: Integer -> CType -> CType
applyPointers count ty
  | count <= 0 = ty
  | otherwise = applyPointers (count - 1) (pointer ty)

decayArrayParameter :: CType -> CType
decayArrayParameter (ArrayType _ target) = pointer target
decayArrayParameter ty = ty

decayExpressionType :: CType -> CType
decayExpressionType ty =
  case ty of
    ArrayType _ target -> pointer target
    FunctionType {} -> pointer ty
    _ -> ty

dereferenceTypeMaybe :: CType -> Maybe CType
dereferenceTypeMaybe ty =
  case ty of
    PointerType target -> Just target
    ArrayType _ target -> Just target
    _ -> Nothing

dereferenceType :: CType -> CType
dereferenceType ty = fromMaybe ty (dereferenceTypeMaybe ty)

compatiblePointerTargets :: CType -> CType -> Bool
compatiblePointerTargets (BaseType Void _) _ = True
compatiblePointerTargets _ (BaseType Void _) = True
compatiblePointerTargets lhs rhs = sameTypeChain lhs rhs True

compatiblePointerTypes :: CType -> CType -> Bool
compatiblePointerTypes (PointerType lhs) (PointerType rhs) = compatiblePointerTargets lhs rhs
compatiblePointerTypes _ _ = False

combinePromotedIntegers :: CType -> CType -> Maybe CType
combinePromotedIntegers lhs rhs =
  case (lhs, rhs) of
    (BaseType lhsKind lhsSigned, BaseType rhsKind rhsSigned)
      | lhsKind == rhsKind && lhsSigned == rhsSigned -> Just lhs
      | lhsSigned == rhsSigned -> Just (higherRankInteger lhs rhs)
      | not lhsSigned && integerRank lhsKind >= integerRank rhsKind -> Just lhs
      | not rhsSigned && integerRank rhsKind >= integerRank lhsKind -> Just rhs
      | lhsSigned && signedCanRepresentUnsigned lhsKind rhsKind -> Just lhs
      | rhsSigned && signedCanRepresentUnsigned rhsKind lhsKind -> Just rhs
      | lhsSigned -> Just (BaseType lhsKind False)
      | otherwise -> Just (BaseType rhsKind False)
    _ -> Nothing

higherRankInteger :: CType -> CType -> CType
higherRankInteger lhs@(BaseType lhsKind _) rhs@(BaseType rhsKind _)
  | integerRank lhsKind >= integerRank rhsKind = lhs
  | otherwise = rhs
higherRankInteger lhs _ = lhs

integerRank :: TypeKind -> Int
integerRank kind =
  case kind of
    Char -> 1
    Short -> 2
    Int -> 3
    Enum -> 3
    Long -> 4
    _ -> 0

signedCanRepresentUnsigned :: TypeKind -> TypeKind -> Bool
signedCanRepresentUnsigned signedKind unsignedKind =
  integerRank signedKind > integerRank unsignedKind

pointer :: CType -> CType
pointer = PointerType

array :: Integer -> CType -> CType
array = ArrayType

function :: CType -> [CType] -> CType
function ret params = FunctionType ret params False

variadicFunction :: CType -> [CType] -> CType
variadicFunction ret params = FunctionType ret params True

struct :: Text -> [Member] -> CType
struct = StructType

union :: Text -> [Member] -> CType
union = UnionType

enum :: Text -> [Text] -> CType
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
  | isStruct = reverse (snd (foldl' addStructMember (0, []) members'))
  | otherwise = [member {memberOffset = Just 0} | member <- members']
  where
    addStructMember (offset, acc) member =
      (offset + sizeof (memberType member), member {memberOffset = Just offset} : acc)

memberByName :: Text -> [Member] -> Maybe Member
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
    (FunctionType ra pa va, FunctionType rb pb vb) -> ra == rb && pa == pb && va == vb
    (StructType ida _, StructType idb _) -> ida == idb
    (UnionType ida _, UnionType idb _) -> ida == idb
    (EnumType ida _, EnumType idb _) -> ida == idb
    _ -> False

renderType :: CType -> Text
renderType ty =
  case ty of
    PointerType target -> renderType target <> "*"
    ArrayType len target -> renderType target <> "[" <> renderLength len <> "]"
    _ -> renderKind (typeKindOf ty)

renderTypePretty :: CType -> Text
renderTypePretty = renderPrettyChain . reverse . typeChain

typeChain :: CType -> [CType]
typeChain ty =
  case ty of
    PointerType target -> ty : typeChain target
    ArrayType _ target -> ty : typeChain target
    _ -> [ty]

renderPrettyChain :: [CType] -> Text
renderPrettyChain [] = "?"
renderPrettyChain (ty : modifiers) = renderPrettyModifiers (renderPrettyAtom ty) modifiers

renderPrettyModifiers :: Text -> [CType] -> Text
renderPrettyModifiers rendered [] = rendered
renderPrettyModifiers rendered modifiers@(ArrayType {} : _) =
  let (arrays, rest) = span isArrayType modifiers
      renderedArrays = foldMap renderArrayModifier (reverse arrays)
   in renderPrettyModifiers (rendered <> renderedArrays) rest
renderPrettyModifiers rendered (PointerType {} : rest) = renderPrettyModifiers (rendered <> "*") rest
renderPrettyModifiers rendered (ty : rest) = renderPrettyModifiers (rendered <> renderPrettyAtom ty) rest

renderPrettyAtom :: CType -> Text
renderPrettyAtom ty =
  case ty of
    FunctionType ret params isVariadic ->
      let renderedParams = map renderTypePretty params <> ["..." | isVariadic]
       in "FUNCTION((" <> Text.intercalate ", " renderedParams <> ") -> " <> renderTypePretty ret <> ")"
    _ -> renderKind (typeKindOf ty)

isArrayType :: CType -> Bool
isArrayType ArrayType {} = True
isArrayType _ = False

renderArrayModifier :: CType -> Text
renderArrayModifier (ArrayType len _) = "[" <> renderLength len <> "]"
renderArrayModifier _ = ""

renderLength :: Integer -> Text
renderLength len
  | len >= 0 = Text.pack (show len)
  | otherwise = "?"

typeKindOf :: CType -> TypeKind
typeKindOf (BaseType kind _) = kind
typeKindOf PointerType {} = Pointer
typeKindOf ArrayType {} = Array
typeKindOf FunctionType {} = Function
typeKindOf StructType {} = Struct
typeKindOf UnionType {} = Union
typeKindOf EnumType {} = Enum

baseKind :: Text -> TypeKind
baseKind kind =
  case Text.toUpper kind of
    "VOID" -> Void
    "CHAR" -> Char
    "SHORT" -> Short
    "INT" -> Int
    "LONG" -> Long
    "STRUCT" -> Struct
    "UNION" -> Union
    "ENUM" -> Enum
    other -> error ("invalid base type kind: " <> Text.unpack other)

renderKind :: TypeKind -> Text
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
