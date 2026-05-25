module Tptcc.Ast
  ( Node (..)
  , NodeChild (..)
  , PostfixOp (..)
  , NodeField (..)
  , NodeValue (..)
  , nodeTypeIdFor
  , nodeTypes
  , renderAst
  ) where

import Data.List (sortOn)

import Tptcc.Token (SourcePos, Token (..), TokenValue (..))

data Node = Node
  { nodeTypeId :: !Int
  , nodeName :: String
  , nodePos :: SourcePos
  , nodeChildren :: [NodeChild]
  , nodeFields :: [NodeField]
  }
  deriving (Eq, Show)

data NodeChild
  = ChildNode Node
  | ChildToken Token
  | ChildPostfix PostfixOp
  deriving (Eq, Show)

data PostfixOp = PostfixOp
  { postfixType :: String
  , postfixValue :: Maybe NodeValue
  }
  deriving (Eq, Show)

data NodeField = NodeField
  { fieldName :: String
  , fieldValue :: NodeValue
  }
  deriving (Eq, Show)

data NodeValue
  = NodeRef Node
  | NodeList [Node]
  | StringValue String
  | StringList [String]
  | IntValue Integer
  | IntList [Integer]
  | BoolValue Bool
  | MissingValue
  deriving (Eq, Show)

nodeTypeIdFor :: String -> Int
nodeTypeIdFor name =
  case lookup name nodeTypes of
    Just typeId -> typeId
    Nothing -> error ("invalid node type: " ++ name)

nodeTypes :: [(String, Int)]
nodeTypes =
  zip
    [ "PROGRAM"
    , "STATEMENT"
    , "DECLARATION"
    , "TYPE_SPECIFIER"
    , "IDENTIFIER"
    , "EXPRESSION"
    , "TERM"
    , "FACTOR"
    , "INT"
    , "PARAMETER_LIST"
    , "PARAMETER"
    , "ARGUMENT_LIST"
    , "FUNCTION_CALL"
    , "BLOCK"
    , "STATEMENT"
    , "LOCAL_DECLARATION"
    , "ASSIGNMENT"
    , "IF"
    , "FOR"
    , "WHILE"
    , "RETURN"
    , "ADDRESS_OF"
    , "DEREFERENCE"
    , "STRING_LITERAL"
    , "CHARACTER"
    , "TERNARY"
    , "SUM_EXPRESSION"
    , "MULTIPLICATIVE_EXPRESSION"
    , "INITIALIZER_LIST"
    , "DECLARATOR"
    , "DIRECT_DECLARATOR"
    , "INIT_DECLARATOR"
    , "INITIALIZER"
    , "DECLARATION_SPECIFIER"
    , "CAST_EXPRESSION"
    , "UNARY_EXPRESSION"
    , "POSTFIX_EXPRESSION"
    , "PRIMARY_EXPRESSION"
    , "ABSTRACT_DECLARATOR"
    , "DIRECT_ABSTRACT_DECLARATOR"
    , "PARAMETER_DECLARATION"
    , "PARAMETER_LIST"
    , "STRUCT_DECLARATION_LIST"
    , "STRUCT_OR_UNION_SPECIFIER"
    , "STRUCT_DECLARATION"
    , "STRUCT_DECLARATOR_LIST"
    , "LOGICAL_OR_EXPRESSION"
    , "LOGICAL_AND_EXPRESSION"
    , "INCLUSIVE_OR_EXPRESSION"
    , "INCLUSIVE_XOR_EXPRESSION"
    , "INCLUSIVE_AND_EXPRESSION"
    , "EQUALITY_EXPRESSION"
    , "RELATIONAL_EXPRESSION"
    , "SHIFT_EXPRESSION"
    , "BREAK"
    , "CONTINUE"
    , "STORAGE_CLASS_SPECIFIER"
    , "TYPE_NAME"
    , "SWITCH"
    , "CASE"
    , "DEFAULT"
    , "ENUM_SPECIFIER"
    , "ENUM_DECLARATION_LIST"
    , "ENUM_MEMBER_DECLARATION"
    , "EMPTY_STATEMENT"
    , "ASM"
    , "ASM_ARGUMENT_LIST"
    , "ASM_ARGUMENT"
    , "REGISTER_IDENTIFIER"
    , "REGISTER_IDENTIFIER_LIST"
    ]
    [1 ..]

renderAst :: Node -> String
renderAst node = unlines (renderNode 0 node)

renderNode :: Int -> Node -> [String]
renderNode indent node =
  [pad indent <> "N " <> nodeName node]
    <> concatMap (renderChild indent) (zip [(1 :: Int) ..] (nodeChildren node))
    <> concatMap (renderField indent) (sortOn fieldName (nodeFields node))

renderChild :: Int -> (Int, NodeChild) -> [String]
renderChild indent (index, child) =
  [pad (indent + 1) <> "I " <> show index]
    <> case child of
      ChildNode node -> renderNode (indent + 2) node
      ChildToken token -> [pad (indent + 2) <> renderToken token]
      ChildPostfix op -> renderPostfixOp (indent + 2) op

renderField :: Int -> NodeField -> [String]
renderField indent field =
  case fieldValue field of
    NodeRef child ->
      [pad (indent + 1) <> "F " <> fieldName field <> " node"]
        <> renderNode (indent + 2) child
    NodeList children ->
      [pad (indent + 1) <> "F " <> fieldName field <> " list"]
        <> concatMap (renderChild (indent + 1)) (zip [(1 :: Int) ..] (map ChildNode children))
    StringValue value -> [pad (indent + 1) <> "F " <> fieldName field <> " string " <> renderString value]
    StringList values -> [pad (indent + 1) <> "F " <> fieldName field <> " strings " <> renderStringList values]
    IntValue value -> [pad (indent + 1) <> "F " <> fieldName field <> " int " <> show value]
    IntList values -> [pad (indent + 1) <> "F " <> fieldName field <> " ints " <> show values]
    BoolValue value -> [pad (indent + 1) <> "F " <> fieldName field <> " bool " <> renderBool value]
    MissingValue -> [pad (indent + 1) <> "F " <> fieldName field <> " missing"]

renderBool :: Bool -> String
renderBool True = "true"
renderBool False = "false"

pad :: Int -> String
pad indent = replicate (indent * 2) ' '

renderToken :: Token -> String
renderToken token = "T " <> tokenName token <> " " <> renderTokenValue (tokenValue token)

renderTokenValue :: TokenValue -> String
renderTokenValue (ValueString value) = renderString value
renderTokenValue (ValueInt value) = show value

renderPostfixOp :: Int -> PostfixOp -> [String]
renderPostfixOp indent op =
  [pad indent <> "O " <> postfixType op]
    <> case postfixValue op of
      Just (NodeRef node) ->
        [pad (indent + 1) <> "V node"] <> renderNode (indent + 2) node
      Just (StringValue value) -> [pad (indent + 1) <> "V string " <> renderString value]
      Just (IntValue value) -> [pad (indent + 1) <> "V int " <> show value]
      Just (BoolValue value) -> [pad (indent + 1) <> "V bool " <> renderBool value]
      Just MissingValue -> [pad (indent + 1) <> "V missing"]
      Just (NodeList nodes) ->
        [pad (indent + 1) <> "V list"] <> concatMap (renderChild (indent + 1)) (zip [(1 :: Int) ..] (map ChildNode nodes))
      Just (StringList values) -> [pad (indent + 1) <> "V strings " <> renderStringList values]
      Just (IntList values) -> [pad (indent + 1) <> "V ints " <> show values]
      Nothing -> []

renderStringList :: [String] -> String
renderStringList values = "[" <> commaJoin (map renderString values) <> "]"

commaJoin :: [String] -> String
commaJoin [] = ""
commaJoin [value] = value
commaJoin (value : values) = value <> "," <> commaJoin values

renderString :: String -> String
renderString value = "\"" <> concatMap escapeChar value <> "\""

escapeChar :: Char -> String
escapeChar '\\' = "\\\\"
escapeChar '"' = "\\\""
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\t' = "\\t"
escapeChar char = [char]
