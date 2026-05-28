module Tptcc.Ast
  ( Node (..)
  , NodeChild (..)
  , PostfixOp (..)
  , NodeField (..)
  , NodeValue (..)
  , NodeKind (..)
  , nodeKindFor
  , nodeKindName
  , nodeName
  , nodeIs
  , nodeTypeIdFor
  , nodeTypes
  , foldMapNode
  , renderAst
  ) where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text

import Tptcc.Token (SourcePos, Token (..), TokenValue (..))

data Node = Node
  { nodeTypeId :: !Int
  , nodeKind :: NodeKind
  , nodePos :: SourcePos
  , nodeChildren :: [NodeChild]
  , nodeFields :: [NodeField]
  }
  deriving stock (Eq, Show)

data NodeKind
  = NodeProgram
  | NodeStatement
  | NodeDeclaration
  | NodeTypeSpecifier
  | NodeIdentifier
  | NodeExpression
  | NodeTerm
  | NodeFactor
  | NodeInt
  | NodeParameterList
  | NodeParameter
  | NodeArgumentList
  | NodeArgumentExpressionList
  | NodeFunctionCall
  | NodeBlock
  | NodeLocalDeclaration
  | NodeAssignment
  | NodeIf
  | NodeFor
  | NodeWhile
  | NodeDoWhile
  | NodeReturn
  | NodeAddressOf
  | NodeDereference
  | NodeStringLiteral
  | NodeCharacter
  | NodeTernary
  | NodeSumExpression
  | NodeMultiplicativeExpression
  | NodeInitializerList
  | NodeDeclarator
  | NodeDirectDeclarator
  | NodeInitDeclarator
  | NodeInitializer
  | NodeDeclarationSpecifier
  | NodeCastExpression
  | NodeUnaryExpression
  | NodePostfixExpression
  | NodePrimaryExpression
  | NodeAbstractDeclarator
  | NodeDirectAbstractDeclarator
  | NodeParameterDeclaration
  | NodeStructDeclarationList
  | NodeStructOrUnionSpecifier
  | NodeStructDeclaration
  | NodeStructDeclaratorList
  | NodeLogicalOrExpression
  | NodeLogicalAndExpression
  | NodeInclusiveOrExpression
  | NodeInclusiveXorExpression
  | NodeInclusiveAndExpression
  | NodeEqualityExpression
  | NodeRelationalExpression
  | NodeShiftExpression
  | NodeBreak
  | NodeContinue
  | NodeStorageClassSpecifier
  | NodeTypeName
  | NodeSwitch
  | NodeCase
  | NodeDefault
  | NodeEnumSpecifier
  | NodeEnumDeclarationList
  | NodeEnumMemberDeclaration
  | NodeEmptyStatement
  | NodeAsm
  | NodeAsmArgumentList
  | NodeAsmArgument
  | NodeRegisterIdentifier
  | NodeRegisterIdentifierList
  | NodeGoto
  | NodeLabel
  deriving stock (Eq, Ord, Enum, Bounded, Show)

data NodeChild
  = ChildNode Node
  | ChildToken Token
  | ChildPostfix PostfixOp
  deriving stock (Eq, Show)

data PostfixOp = PostfixOp
  { postfixType :: Text
  , postfixValue :: Maybe NodeValue
  }
  deriving stock (Eq, Show)

data NodeField = NodeField
  { fieldName :: Text
  , fieldValue :: NodeValue
  }
  deriving stock (Eq, Show)

data NodeValue
  = NodeRef Node
  | NodeList [Node]
  | StringValue Text
  | StringList [Text]
  | IntValue Integer
  | IntList [Integer]
  | BoolValue Bool
  | MissingValue
  deriving stock (Eq, Show)

nodeKindFor :: Text -> NodeKind
nodeKindFor name =
  case lookup name nodeKindsByName of
    Just kind -> kind
    Nothing -> error ("invalid node type: " ++ Text.unpack name)

nodeName :: Node -> Text
nodeName = nodeKindName . nodeKind

nodeIs :: NodeKind -> Node -> Bool
nodeIs kind node = nodeKind node == kind

nodeTypeIdFor :: Text -> Int
nodeTypeIdFor name =
  case lookup (nodeKindFor name) nodeTypes of
    Just typeId -> typeId
    Nothing -> error ("invalid node type: " ++ Text.unpack name)

nodeTypes :: [(NodeKind, Int)]
nodeTypes = zip allNodeKinds [1 ..]

allNodeKinds :: [NodeKind]
allNodeKinds = [minBound .. maxBound]

nodeKindsByName :: [(Text, NodeKind)]
nodeKindsByName = [(nodeKindName kind, kind) | (kind, _) <- nodeTypes]

nodeKindName :: NodeKind -> Text
nodeKindName kind =
  case kind of
    NodeProgram -> "PROGRAM"
    NodeStatement -> "STATEMENT"
    NodeDeclaration -> "DECLARATION"
    NodeTypeSpecifier -> "TYPE_SPECIFIER"
    NodeIdentifier -> "IDENTIFIER"
    NodeExpression -> "EXPRESSION"
    NodeTerm -> "TERM"
    NodeFactor -> "FACTOR"
    NodeInt -> "INT"
    NodeParameterList -> "PARAMETER_LIST"
    NodeParameter -> "PARAMETER"
    NodeArgumentList -> "ARGUMENT_LIST"
    NodeArgumentExpressionList -> "ARGUMENT_EXPRESSION_LIST"
    NodeFunctionCall -> "FUNCTION_CALL"
    NodeBlock -> "BLOCK"
    NodeLocalDeclaration -> "LOCAL_DECLARATION"
    NodeAssignment -> "ASSIGNMENT"
    NodeIf -> "IF"
    NodeFor -> "FOR"
    NodeWhile -> "WHILE"
    NodeDoWhile -> "DO_WHILE"
    NodeReturn -> "RETURN"
    NodeAddressOf -> "ADDRESS_OF"
    NodeDereference -> "DEREFERENCE"
    NodeStringLiteral -> "STRING_LITERAL"
    NodeCharacter -> "CHARACTER"
    NodeTernary -> "TERNARY"
    NodeSumExpression -> "SUM_EXPRESSION"
    NodeMultiplicativeExpression -> "MULTIPLICATIVE_EXPRESSION"
    NodeInitializerList -> "INITIALIZER_LIST"
    NodeDeclarator -> "DECLARATOR"
    NodeDirectDeclarator -> "DIRECT_DECLARATOR"
    NodeInitDeclarator -> "INIT_DECLARATOR"
    NodeInitializer -> "INITIALIZER"
    NodeDeclarationSpecifier -> "DECLARATION_SPECIFIER"
    NodeCastExpression -> "CAST_EXPRESSION"
    NodeUnaryExpression -> "UNARY_EXPRESSION"
    NodePostfixExpression -> "POSTFIX_EXPRESSION"
    NodePrimaryExpression -> "PRIMARY_EXPRESSION"
    NodeAbstractDeclarator -> "ABSTRACT_DECLARATOR"
    NodeDirectAbstractDeclarator -> "DIRECT_ABSTRACT_DECLARATOR"
    NodeParameterDeclaration -> "PARAMETER_DECLARATION"
    NodeStructDeclarationList -> "STRUCT_DECLARATION_LIST"
    NodeStructOrUnionSpecifier -> "STRUCT_OR_UNION_SPECIFIER"
    NodeStructDeclaration -> "STRUCT_DECLARATION"
    NodeStructDeclaratorList -> "STRUCT_DECLARATOR_LIST"
    NodeLogicalOrExpression -> "LOGICAL_OR_EXPRESSION"
    NodeLogicalAndExpression -> "LOGICAL_AND_EXPRESSION"
    NodeInclusiveOrExpression -> "INCLUSIVE_OR_EXPRESSION"
    NodeInclusiveXorExpression -> "INCLUSIVE_XOR_EXPRESSION"
    NodeInclusiveAndExpression -> "INCLUSIVE_AND_EXPRESSION"
    NodeEqualityExpression -> "EQUALITY_EXPRESSION"
    NodeRelationalExpression -> "RELATIONAL_EXPRESSION"
    NodeShiftExpression -> "SHIFT_EXPRESSION"
    NodeBreak -> "BREAK"
    NodeContinue -> "CONTINUE"
    NodeStorageClassSpecifier -> "STORAGE_CLASS_SPECIFIER"
    NodeTypeName -> "TYPE_NAME"
    NodeSwitch -> "SWITCH"
    NodeCase -> "CASE"
    NodeDefault -> "DEFAULT"
    NodeEnumSpecifier -> "ENUM_SPECIFIER"
    NodeEnumDeclarationList -> "ENUM_DECLARATION_LIST"
    NodeEnumMemberDeclaration -> "ENUM_MEMBER_DECLARATION"
    NodeEmptyStatement -> "EMPTY_STATEMENT"
    NodeAsm -> "ASM"
    NodeAsmArgumentList -> "ASM_ARGUMENT_LIST"
    NodeAsmArgument -> "ASM_ARGUMENT"
    NodeRegisterIdentifier -> "REGISTER_IDENTIFIER"
    NodeRegisterIdentifierList -> "REGISTER_IDENTIFIER_LIST"
    NodeGoto -> "GOTO"
    NodeLabel -> "LABEL"

foldMapNode :: Monoid m => (Node -> m) -> Node -> m
foldMapNode visit node =
  visit node <> foldMap (foldMapNode visit) (nodeReferences node)

nodeReferences :: Node -> [Node]
nodeReferences node =
  concatMap childReferences (nodeChildren node)
    <> concatMap fieldReferences (nodeFields node)

childReferences :: NodeChild -> [Node]
childReferences = \case
  ChildNode node -> [node]
  ChildPostfix op -> postfixReferences op
  ChildToken _ -> []

postfixReferences :: PostfixOp -> [Node]
postfixReferences op =
  maybe [] valueReferences (postfixValue op)

fieldReferences :: NodeField -> [Node]
fieldReferences = valueReferences . fieldValue

valueReferences :: NodeValue -> [Node]
valueReferences = \case
  NodeRef node -> [node]
  NodeList nodes -> nodes
  StringValue _ -> []
  StringList _ -> []
  IntValue _ -> []
  IntList _ -> []
  BoolValue _ -> []
  MissingValue -> []

renderAst :: Node -> String
renderAst node = Text.unpack (Text.unlines (renderNode 0 node))

renderNode :: Int -> Node -> [Text]
renderNode indent node =
  [pad indent <> "N " <> nodeName node]
    <> concatMap (renderChild indent) (zip [(1 :: Int) ..] (nodeChildren node))
    <> concatMap (renderField indent) (sortOn fieldName (nodeFields node))

renderChild :: Int -> (Int, NodeChild) -> [Text]
renderChild indent (index, child) =
  [pad (indent + 1) <> "I " <> Text.pack (show index)]
    <> case child of
      ChildNode node -> renderNode (indent + 2) node
      ChildToken token -> [pad (indent + 2) <> renderToken token]
      ChildPostfix op -> renderPostfixOp (indent + 2) op

renderField :: Int -> NodeField -> [Text]
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
    IntValue value -> [pad (indent + 1) <> "F " <> fieldName field <> " int " <> Text.pack (show value)]
    IntList values -> [pad (indent + 1) <> "F " <> fieldName field <> " ints " <> Text.pack (show values)]
    BoolValue value -> [pad (indent + 1) <> "F " <> fieldName field <> " bool " <> renderBool value]
    MissingValue -> [pad (indent + 1) <> "F " <> fieldName field <> " missing"]

renderBool :: Bool -> Text
renderBool True = "true"
renderBool False = "false"

pad :: Int -> Text
pad indent = Text.replicate (indent * 2) " "

renderToken :: Token -> Text
renderToken token = "T " <> tokenName token <> " " <> renderTokenValue (tokenValue token)

renderTokenValue :: TokenValue -> Text
renderTokenValue (ValueString value) = renderString value
renderTokenValue (ValueInt value) = Text.pack (show value)

renderPostfixOp :: Int -> PostfixOp -> [Text]
renderPostfixOp indent op =
  [pad indent <> "O " <> postfixType op]
    <> case postfixValue op of
      Just (NodeRef node) ->
        [pad (indent + 1) <> "V node"] <> renderNode (indent + 2) node
      Just (StringValue value) -> [pad (indent + 1) <> "V string " <> renderString value]
      Just (IntValue value) -> [pad (indent + 1) <> "V int " <> Text.pack (show value)]
      Just (BoolValue value) -> [pad (indent + 1) <> "V bool " <> renderBool value]
      Just MissingValue -> [pad (indent + 1) <> "V missing"]
      Just (NodeList nodes) ->
        [pad (indent + 1) <> "V list"] <> concatMap (renderChild (indent + 1)) (zip [(1 :: Int) ..] (map ChildNode nodes))
      Just (StringList values) -> [pad (indent + 1) <> "V strings " <> renderStringList values]
      Just (IntList values) -> [pad (indent + 1) <> "V ints " <> Text.pack (show values)]
      Nothing -> []

renderStringList :: [Text] -> Text
renderStringList values = "[" <> commaJoin (map renderString values) <> "]"

commaJoin :: [Text] -> Text
commaJoin [] = ""
commaJoin [value] = value
commaJoin (value : values) = value <> "," <> commaJoin values

renderString :: Text -> Text
renderString value = "\"" <> Text.concatMap escapeChar value <> "\""

escapeChar :: Char -> Text
escapeChar '\\' = "\\\\"
escapeChar '"' = "\\\""
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\t' = "\\t"
escapeChar char = Text.singleton char
