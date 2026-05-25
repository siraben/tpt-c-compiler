module Tptcc.Parser
  ( parse
  ) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, modify')
import qualified Data.Set as Set
import Data.Void (Void)
import qualified Text.Megaparsec as MP

import Tptcc.Ast
import Tptcc.Token

data ParserState = ParserState
  { parserTypedefs :: Set.Set String
  }
  deriving (Eq, Show)

type ParserM = StateT ParserState (MP.Parsec Void [Token])

parse :: [Token] -> Either String Node
parse tokens =
  case MP.runParser (evalStateT parseProgram ParserState {parserTypedefs = Set.empty}) "<tokens>" tokens of
    Left err -> Left (show err)
    Right ast -> Right ast

parseProgram :: ParserM Node
parseProgram = do
  program <- emptyNode "PROGRAM"
  declarations <- manyUntil "EOF" parseDeclarationWithTerminator
  pure program {nodeChildren = map ChildNode declarations}

parseDeclarationWithTerminator :: ParserM Node
parseDeclarationWithTerminator = do
  declaration <- parseDeclaration
  let isFunctionDefinition = hasBoolField "is_function" declaration && hasField "block" declaration
  unless isFunctionDefinition (expect ";")
  pure declaration

parseDeclaration :: ParserM Node
parseDeclaration = do
  declaration <- emptyNode "DECLARATION"
  specifier <- parseDeclarationSpecifier
  declarators <- parseDeclarators
  let blockFields =
        case break hasBlockField declarators of
          (_, decl : _) -> fieldByName "block" decl
          _ -> []
      cleanDeclarators = map (removeField "block") declarators
      firstDeclarator = listToMaybe cleanDeclarators
      fields =
        [ NodeField "declarators" (NodeList cleanDeclarators)
        , NodeField "specifier" (NodeRef specifier)
        ]
          <> maybe [] (\decl -> [NodeField "declarator" (NodeRef decl)]) firstDeclarator
      functionFields =
        case firstDeclarator of
          Just decl | hasBoolField "is_function" decl -> [NodeField "is_function" (BoolValue True)]
          _ -> []
      result = declaration {nodeFields = fields <> functionFields <> blockFields}
  when (storageClassKind specifier == Just "typedef") $
    mapM_ rememberTypedef declarators
  pure result

parseDeclarators :: ParserM [Node]
parseDeclarators = do
  done <- anyCheck [";"]
  blockDone <- anyCheck ["{"]
  if done || blockDone
    then pure []
    else do
      declarator <- parseDeclarator
      withInitializerOrBlock <- addInitializerOrBlock declarator
      if hasBlockField withInitializerOrBlock
        then pure [withInitializerOrBlock]
        else do
          hasComma <- accept ","
          if hasComma
            then do
              rest <- parseDeclarators
              pure (withInitializerOrBlock : rest)
            else pure [withInitializerOrBlock]

addInitializerOrBlock :: Node -> ParserM Node
addInitializerOrBlock declarator = do
  startsBlock <- check "{"
  if startsBlock
    then do
      block <- parseBlock
      pure (addField (NodeField "block" (NodeRef block)) declarator)
    else do
      hasInitializer <- accept "="
      if hasInitializer
        then do
          initializer <- parseInitializer
          pure (addField (NodeField "initializer" (NodeRef initializer)) declarator)
        else pure declarator

parseDeclarationSpecifier :: ParserM Node
parseDeclarationSpecifier = do
  node <- emptyNode "DECLARATION_SPECIFIER"
  storageClass <- parseStorageClassSpecifier
  typeSpecifier <- parseTypeSpecifier
  pure
    node
      { nodeFields =
          [ NodeField "storage_class" (NodeRef storageClass)
          , NodeField "type_specifier" (NodeRef typeSpecifier)
          ]
      }

parseStorageClassSpecifier :: ParserM Node
parseStorageClassSpecifier = do
  node <- emptyNode "STORAGE_CLASS_SPECIFIER"
  hasStorageClass <- check "STORAGE_CLASS"
  kind <-
    if hasStorageClass
      then tokenString <$> nextToken
      else pure "auto"
  pure node {nodeFields = [NodeField "kind" (StringValue kind)]}

parseTypeSpecifier :: ParserM Node
parseTypeSpecifier = do
  node <- emptyNode "TYPE_SPECIFIER"
  expectType "TYPE_SPECIFIER"
  token <- peekToken
  case tokenString token of
    "struct" -> do
      specifier <- parseStructOrUnionSpecifier
      pure node {nodeFields = [NodeField "kind" (NodeRef specifier)]}
    "union" -> do
      specifier <- parseStructOrUnionSpecifier
      pure node {nodeFields = [NodeField "kind" (NodeRef specifier)]}
    "enum" -> do
      specifier <- parseEnumSpecifier
      pure node {nodeFields = [NodeField "kind" (NodeRef specifier)]}
    _ -> do
      kinds <- gatherTypeKinds
      pure node {nodeFields = [NodeField "kind" (StringList kinds)]}

gatherTypeKinds :: ParserM [String]
gatherTypeKinds = do
  isType <- check "TYPE_SPECIFIER"
  if isType
    then do
      value <- tokenString <$> nextToken
      rest <- gatherTypeKinds
      pure (value : rest)
    else pure []

parseStructOrUnionSpecifier :: ParserM Node
parseStructOrUnionSpecifier = do
  node <- emptyNode "STRUCT_OR_UNION_SPECIFIER"
  token <- nextToken
  idFields <-
    ifM
      (check "ID")
      ( do
          identifier <- parseIdentifier
          pure [NodeField "id" (NodeRef identifier)]
      )
      (pure [])
  declarationFields <-
    ifM
      (accept "{")
      ( do
          declarations <- parseStructDeclarationLists
          expect "}"
          pure [NodeField "declaration" (NodeList declarations)]
      )
      (pure [])
  pure node {nodeFields = [NodeField "is_struct" (BoolValue (tokenString token == "struct"))] <> idFields <> declarationFields}

parseStructDeclarationLists :: ParserM [Node]
parseStructDeclarationLists = do
  done <- check "}"
  if done
    then pure []
    else do
      declaration <- parseStructDeclarationList
      expect ";"
      rest <- parseStructDeclarationLists
      pure (declaration : rest)

parseStructDeclarationList :: ParserM Node
parseStructDeclarationList = do
  node <- emptyNode "STRUCT_DECLARATION_LIST"
  typeSpecifier <- parseTypeSpecifier
  declarator <- parseDeclarator
  declarators <- parseMoreDeclarators
  pure node {nodeChildren = map ChildNode (declarator : declarators), nodeFields = [NodeField "type_specifier" (NodeRef typeSpecifier)]}

parseMoreDeclarators :: ParserM [Node]
parseMoreDeclarators = do
  hasComma <- accept ","
  if hasComma
    then do
      declarator <- parseDeclarator
      rest <- parseMoreDeclarators
      pure (declarator : rest)
    else pure []

parseEnumSpecifier :: ParserM Node
parseEnumSpecifier = do
  node <- emptyNode "ENUM_SPECIFIER"
  expect "TYPE_SPECIFIER"
  identifier <- parseIdentifier
  declarationFields <-
    ifM
      (accept "{")
      ( do
          declaration <- parseEnumDeclarationList
          expect "}"
          pure [NodeField "declaration" (NodeRef declaration)]
      )
      (pure [])
  pure node {nodeFields = [NodeField "id" (NodeRef identifier)] <> declarationFields}

parseEnumDeclarationList :: ParserM Node
parseEnumDeclarationList = do
  node <- emptyNode "ENUM_DECLARATION_LIST"
  firstMember <- parseEnumMemberDeclaration
  rest <- parseMoreEnumMemberDeclarations
  pure node {nodeChildren = map ChildNode (firstMember : rest)}

parseMoreEnumMemberDeclarations :: ParserM [Node]
parseMoreEnumMemberDeclarations = do
  hasComma <- accept ","
  closes <- check "}"
  if hasComma && not closes
    then do
      member <- parseEnumMemberDeclaration
      rest <- parseMoreEnumMemberDeclarations
      pure (member : rest)
    else pure []

parseEnumMemberDeclaration :: ParserM Node
parseEnumMemberDeclaration = do
  node <- emptyNode "ENUM_MEMBER_DECLARATION"
  identifier <- parseIdentifier
  valueFields <-
    ifM
      (accept "=")
      ( do
          value <- tokenInteger <$> nextToken
          pure [NodeField "value" (IntValue value)]
      )
      (pure [])
  pure node {nodeFields = [NodeField "id" (NodeRef identifier)] <> valueFields}

parseDeclarator :: ParserM Node
parseDeclarator = do
  node <- emptyNode "DECLARATOR"
  pointerLevel <- countWhile "*"
  direct <- parseDirectDeclarator
  let functionFields =
        if hasField "parameter_list" direct
          then [NodeField "is_function" (BoolValue True)]
          else []
  pure
    node
      { nodeFields =
          [ NodeField "direct_declarator" (NodeRef direct)
          , NodeField "id" (NodeRef (directDeclaratorId direct))
          , NodeField "pointer_level" (IntValue pointerLevel)
          ]
            <> functionFields
      }

parseDirectDeclarator :: ParserM Node
parseDirectDeclarator = do
  node <- emptyNode "DIRECT_DECLARATOR"
  baseFields <- parseDirectDeclaratorBase
  suffixFields <- parseDirectDeclaratorSuffixes
  pure node {nodeFields = baseFields <> suffixFields}

parseDirectDeclaratorBase :: ParserM [NodeField]
parseDirectDeclaratorBase = do
  hasId <- check "ID"
  if hasId
    then do
      identifier <- parseIdentifier
      pure [NodeField "id" (NodeRef identifier)]
    else do
      hasParen <- accept "("
      if hasParen
        then do
          declarator <- parseDeclarator
          expect ")"
          pure
            [ NodeField "declarator" (NodeRef declarator)
            , NodeField "id" (NodeRef (directDeclaratorId declarator))
            ]
        else failAtPeek "Unexpected token in direct declarator"

parseDirectDeclaratorSuffixes :: ParserM [NodeField]
parseDirectDeclaratorSuffixes = do
  dimensions <- parseDimensions
  parameterList <- parseOptionalParameterList
  pure $
    [NodeField "dimensions" (IntList dimensions)]
      <> maybe [] (\params -> [NodeField "parameter_list" (NodeRef params)]) parameterList

parseDimensions :: ParserM [Integer]
parseDimensions = do
  hasDimension <- accept "["
  if hasDimension
    then do
      dimension <-
        ifM
          (anyCheck ["INT", "UNSIGNED_INT"])
          (tokenInteger <$> nextToken)
          (pure (-1))
      expect "]"
      rest <- parseDimensions
      pure (dimension : rest)
    else pure []

parseOptionalParameterList :: ParserM (Maybe Node)
parseOptionalParameterList = do
  hasParams <- accept "("
  if hasParams
    then do
      params <- parseParameterList
      expect ")"
      pure (Just params)
    else pure Nothing

parseParameterList :: ParserM Node
parseParameterList = do
  node <- emptyNode "PARAMETER_LIST"
  closes <- check ")"
  if closes
    then pure node
    else do
      firstParam <- parseParameterDeclaration
      voidOnly <- parameterIsVoidOnly firstParam
      if voidOnly
        then pure node
        else do
          (isVariadic, rest) <- parseMoreParameters
          pure
            node
              { nodeChildren = map ChildNode (firstParam : rest)
              , nodeFields = [NodeField "is_variadic" (BoolValue True) | isVariadic]
              }

parseMoreParameters :: ParserM (Bool, [Node])
parseMoreParameters = do
  hasComma <- accept ","
  if hasComma
    then do
      isVariadic <- accept "..."
      if isVariadic
        then pure (True, [])
        else do
          param <- parseParameterDeclaration
          (restVariadic, rest) <- parseMoreParameters
          pure (restVariadic, param : rest)
    else pure (False, [])

parseParameterDeclaration :: ParserM Node
parseParameterDeclaration = do
  node <- emptyNode "PARAMETER_DECLARATION"
  typeSpecifier <- parseTypeSpecifier
  needsDeclarator <- not <$> anyCheck [",", ")"]
  declaratorFields <-
    if needsDeclarator
      then do
        declarator <- parseDeclarator
        pure [NodeField "declarator" (NodeRef declarator)]
      else pure []
  pure node {nodeFields = [NodeField "type_specifier" (NodeRef typeSpecifier)] <> declaratorFields}

parseInitializer :: ParserM Node
parseInitializer = do
  hasList <- accept "{"
  if hasList
    then do
      initializer <- parseInitializerList
      _ <- accept ","
      expect "}"
      pure initializer
    else do
      node <- emptyNode "INITIALIZER"
      value <- parseAssignmentExpression
      pure node {nodeFields = [NodeField "value" (NodeRef value)]}

parseInitializerList :: ParserM Node
parseInitializerList = do
  node <- emptyNode "INITIALIZER_LIST"
  firstInitializer <- parseInitializer
  rest <- parseMoreInitializers
  pure node {nodeChildren = map ChildNode (firstInitializer : rest)}

parseMoreInitializers :: ParserM [Node]
parseMoreInitializers = do
  hasComma <- accept ","
  closes <- check "}"
  if hasComma && not closes
    then do
      initializer <- parseInitializer
      rest <- parseMoreInitializers
      pure (initializer : rest)
    else pure []

parseBlock :: ParserM Node
parseBlock = do
  node <- emptyNode "BLOCK"
  expect "{"
  statements <- manyUntil "}" parseStatement
  expect "}"
  pure node {nodeChildren = map ChildNode statements}

parseStatement :: ParserM Node
parseStatement = do
  node <- emptyNode "STATEMENT"
  isIf <- check "IF"
  child <-
    if isIf
      then parseUnmatchedStatement
      else parseNonIfStatement
  pure node {nodeFields = [NodeField "child" (NodeRef child)]}

parseNonIfStatement :: ParserM Node
parseNonIfStatement = do
  token <- peekToken
  case effectiveTokenName token of
    "TYPE_SPECIFIER" -> do
      declaration <- parseDeclaration
      expect ";"
      pure declaration
    "STORAGE_CLASS" -> do
      declaration <- parseDeclaration
      expect ";"
      pure declaration
    "RETURN" -> do
      returnNode <- parseReturn
      expect ";"
      pure returnNode
    "{" -> parseBlock
    "FOR" -> parseFor
    "WHILE" -> parseWhile
    "SWITCH" -> parseSwitch
    "CASE" -> parseCase
    "DEFAULT" -> parseDefault
    "BREAK" -> do
      node <- emptyNode "BREAK"
      expect "BREAK"
      expect ";"
      pure node
    "CONTINUE" -> do
      node <- emptyNode "CONTINUE"
      expect "CONTINUE"
      expect ";"
      pure node
    "ASM" -> do
      asmNode <- parseAsm
      expect ";"
      pure asmNode
    ";" -> nextToken >> emptyNode "EMPTY_STATEMENT"
    _ -> do
      expression <- parseExpression
      expect ";"
      pure expression

parseMatchedStatement :: ParserM Node
parseMatchedStatement = do
  isIf <- accept "IF"
  if isIf
    then do
      node <- emptyNode "IF"
      expect "("
      condition <- parseExpression
      expect ")"
      trueCase <- parseMatchedStatement
      expect "ELSE"
      falseCase <- parseMatchedStatement
      pure
        node
          { nodeFields =
              [ NodeField "condition" (NodeRef condition)
              , NodeField "true_case" (NodeRef trueCase)
              , NodeField "false_case" (NodeRef falseCase)
              ]
          }
    else parseNonIfStatement

parseUnmatchedStatement :: ParserM Node
parseUnmatchedStatement = do
  isIf <- accept "IF"
  if isIf
    then do
      node <- emptyNode "IF"
      expect "("
      condition <- parseExpression
      expect ")"
      trueCase <- parseStatement
      hasElse <- accept "ELSE"
      falseCaseFields <-
        if hasElse
          then do
            statementNode <- emptyNode "STATEMENT"
            child <- parseUnmatchedStatement
            pure [NodeField "false_case" (NodeRef statementNode {nodeFields = [NodeField "child" (NodeRef child)]})]
          else pure []
      pure
        node
          { nodeFields =
              [ NodeField "condition" (NodeRef condition)
              , NodeField "true_case" (NodeRef trueCase)
              ]
                <> falseCaseFields
          }
    else parseMatchedStatement

parseWhile :: ParserM Node
parseWhile = do
  node <- emptyNode "WHILE"
  expect "WHILE"
  expect "("
  condition <- parseExpression
  expect ")"
  statement <- parseStatement
  pure node {nodeFields = [NodeField "condition" (NodeRef condition), NodeField "statement" (NodeRef statement)]}

parseSwitch :: ParserM Node
parseSwitch = do
  node <- emptyNode "SWITCH"
  expect "SWITCH"
  expect "("
  condition <- parseExpression
  expect ")"
  block <- parseBlock
  pure node {nodeFields = [NodeField "condition" (NodeRef condition), NodeField "block" (NodeRef block)]}

parseCase :: ParserM Node
parseCase = do
  node <- emptyNode "CASE"
  expect "CASE"
  value <- parsePrimaryExpression
  expect ":"
  statement <- parseStatement
  pure node {nodeFields = [NodeField "value" (NodeRef value), NodeField "statement" (NodeRef statement)]}

parseDefault :: ParserM Node
parseDefault = do
  node <- emptyNode "DEFAULT"
  expect "DEFAULT"
  expect ":"
  statement <- parseStatement
  pure node {nodeFields = [NodeField "statement" (NodeRef statement)]}

parseAsm :: ParserM Node
parseAsm = do
  node <- emptyNode "ASM"
  expect "ASM"
  expect "("
  asmParts <- parseAsmStrings
  inputFields <-
    ifM
      (accept ":")
      ( do
          inputs <- parseAsmArgumentList
          pure [NodeField "inputs" (NodeRef inputs)]
      )
      (pure [])
  outputFields <-
    ifM
      (accept ":")
      ( do
          outputs <- parseAsmArgumentList
          pure [NodeField "outputs" (NodeRef outputs)]
      )
      (pure [])
  clobberFields <-
    ifM
      (accept ":")
      ( do
          clobbers <- parseRegisterIdentifierList
          pure [NodeField "clobbers" (NodeRef clobbers)]
      )
      (pure [])
  expect ")"
  pure node {nodeFields = [NodeField "asm" (StringValue (joinAsm asmParts))] <> inputFields <> outputFields <> clobberFields}

parseAsmStrings :: ParserM [String]
parseAsmStrings = do
  hasString <- check "STRING_LITERAL"
  if hasString
    then do
      value <- stripQuotes . tokenString <$> nextToken
      rest <- parseAsmStrings
      pure (value : rest)
    else pure []

parseAsmArgumentList :: ParserM Node
parseAsmArgumentList = do
  node <- emptyNode "ASM_ARGUMENT_LIST"
  done <- anyCheck [":", ")"]
  if done
    then pure node {nodeFields = [NodeField "arguments" (NodeList [])]}
    else do
      firstArgument <- parseAsmArgument
      rest <- parseMoreAsmArguments
      pure node {nodeFields = [NodeField "arguments" (NodeList (firstArgument : rest))]}

parseMoreAsmArguments :: ParserM [Node]
parseMoreAsmArguments = do
  hasComma <- accept ","
  if hasComma
    then do
      argument <- parseAsmArgument
      rest <- parseMoreAsmArguments
      pure (argument : rest)
    else pure []

parseAsmArgument :: ParserM Node
parseAsmArgument = do
  node <- emptyNode "ASM_ARGUMENT"
  asmSymbol <- parseRegisterIdentifier
  expect "="
  cSymbol <- parseIdentifier
  pure node {nodeFields = [NodeField "asm_symbol" (NodeRef asmSymbol), NodeField "c_symbol" (NodeRef cSymbol)]}

parseRegisterIdentifierList :: ParserM Node
parseRegisterIdentifierList = do
  node <- emptyNode "REGISTER_IDENTIFIER_LIST"
  hasIdentifier <- check "ID"
  if hasIdentifier
    then do
      firstIdentifier <- parseRegisterIdentifier
      rest <- parseMoreRegisterIdentifiers
      pure node {nodeChildren = map ChildNode (firstIdentifier : rest)}
    else pure node

parseMoreRegisterIdentifiers :: ParserM [Node]
parseMoreRegisterIdentifiers = do
  hasComma <- accept ","
  if hasComma
    then do
      identifier <- parseRegisterIdentifier
      rest <- parseMoreRegisterIdentifiers
      pure (identifier : rest)
    else pure []

parseRegisterIdentifier :: ParserM Node
parseRegisterIdentifier = do
  node <- emptyNode "REGISTER_IDENTIFIER"
  token <- nextToken
  unless (effectiveTokenName token == "ID") $
    failAt token "Expected register identifier"
  pure node {nodePos = tokenPos token, nodeFields = [NodeField "id" (StringValue (tokenString token))]}

parseFor :: ParserM Node
parseFor = do
  node <- emptyNode "FOR"
  expect "FOR"
  expect "("
  initialization <-
    ifM
      (anyCheck ["TYPE_SPECIFIER", "STORAGE_CLASS"])
      (Just <$> parseDeclaration)
      (ifM (check ";") (pure Nothing) (Just <$> parseExpression))
  expect ";"
  condition <- ifM (check ";") (pure Nothing) (Just <$> parseExpression)
  expect ";"
  update <- ifM (check ")") (pure Nothing) (Just <$> parseExpression)
  expect ")"
  statement <- parseStatement
  pure
    node
      { nodeFields =
          maybe [] (\n -> [NodeField "initialization" (NodeRef n)]) initialization
            <> maybe [] (\n -> [NodeField "condition" (NodeRef n)]) condition
            <> maybe [] (\n -> [NodeField "update" (NodeRef n)]) update
            <> [NodeField "statement" (NodeRef statement)]
      }

parseReturn :: ParserM Node
parseReturn = do
  node <- emptyNode "RETURN"
  expect "RETURN"
  hasValue <- not <$> check ";"
  if hasValue
    then do
      value <- parseExpression
      pure node {nodeFields = [NodeField "value" (NodeRef value)]}
    else pure node

parseExpression :: ParserM Node
parseExpression = do
  node <- emptyNode "EXPRESSION"
  firstExpression <- parseAssignmentExpression
  rest <- parseCommaExpressions
  pure node {nodeChildren = map ChildNode (firstExpression : rest)}

parseCommaExpressions :: ParserM [Node]
parseCommaExpressions = do
  hasComma <- accept ","
  if hasComma
    then do
      expression <- parseAssignmentExpression
      rest <- parseCommaExpressions
      pure (expression : rest)
    else pure []

parseAssignmentExpression :: ParserM Node
parseAssignmentExpression = do
  lhs <- parseTernaryExpression
  isAssignment <- anyCheck ["=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="]
  if isAssignment
    then do
      opToken <- nextToken
      rhs <- parseAssignmentExpression
      node <- emptyNodeAt "ASSIGNMENT" (tokenPos opToken)
      pure
        node
          { nodeFields =
              [ NodeField "op" (StringValue (tokenString opToken))
              , NodeField "lhs" (NodeRef lhs)
              , NodeField "rhs" (NodeRef rhs)
              ]
          }
    else pure lhs

parseTernaryExpression :: ParserM Node
parseTernaryExpression = do
  condition <- parseLogicalOrExpression
  hasQuestion <- accept "?"
  if hasQuestion
    then do
      node <- emptyNode "TERNARY"
      trueCase <- parseAssignmentExpression
      expect ":"
      falseCase <- parseLogicalOrExpression
      pure
        node
          { nodeFields =
              [ NodeField "condition" (NodeRef condition)
              , NodeField "true_case" (NodeRef trueCase)
              , NodeField "false_case" (NodeRef falseCase)
              ]
          }
    else pure condition

parseLogicalOrExpression :: ParserM Node
parseLogicalOrExpression = parseOperandOnlyNode "LOGICAL_OR_EXPRESSION" ["||"] parseLogicalAndExpression

parseLogicalAndExpression :: ParserM Node
parseLogicalAndExpression = parseOperandOnlyNode "LOGICAL_AND_EXPRESSION" ["&&"] parseInclusiveOrExpression

parseInclusiveOrExpression :: ParserM Node
parseInclusiveOrExpression = parseOperandOnlyNode "INCLUSIVE_OR_EXPRESSION" ["|"] parseInclusiveXorExpression

parseInclusiveXorExpression :: ParserM Node
parseInclusiveXorExpression = parseOperandOnlyNode "INCLUSIVE_XOR_EXPRESSION" ["^"] parseInclusiveAndExpression

parseInclusiveAndExpression :: ParserM Node
parseInclusiveAndExpression = parseOperandOnlyNode "INCLUSIVE_AND_EXPRESSION" ["&"] parseEqualityExpression

parseEqualityExpression :: ParserM Node
parseEqualityExpression = parseBinaryNode "EQUALITY_EXPRESSION" ["==", "!="] parseRelationalExpression

parseRelationalExpression :: ParserM Node
parseRelationalExpression = parseBinaryNode "RELATIONAL_EXPRESSION" ["<", "<=", ">", ">="] parseShiftExpression

parseShiftExpression :: ParserM Node
parseShiftExpression = parseBinaryNode "SHIFT_EXPRESSION" ["<<", ">>"] parseSumExpression

parseSumExpression :: ParserM Node
parseSumExpression = parseBinaryNode "SUM_EXPRESSION" ["+", "-"] parseTerm

parseTerm :: ParserM Node
parseTerm = parseBinaryNode "MULTIPLICATIVE_EXPRESSION" ["*", "/", "%"] parseCastExpression

parseBinaryNode :: String -> [String] -> ParserM Node -> ParserM Node
parseBinaryNode nodeName' operators subParser = do
  firstOperand <- subParser
  hasOperator <- anyCheck operators
  if hasOperator
    then do
      node <- emptyNode nodeName'
      children <- parseBinaryRest operators subParser [ChildNode firstOperand]
      pure node {nodeChildren = children}
    else pure firstOperand

parseBinaryRest :: [String] -> ParserM Node -> [NodeChild] -> ParserM [NodeChild]
parseBinaryRest operators subParser children = do
  hasOperator <- anyCheck operators
  if hasOperator
    then do
      opToken <- nextToken
      operand <- subParser
      parseBinaryRest operators subParser (children <> [ChildToken opToken, ChildNode operand])
    else pure children

parseOperandOnlyNode :: String -> [String] -> ParserM Node -> ParserM Node
parseOperandOnlyNode nodeName' operators subParser = do
  firstOperand <- subParser
  hasOperator <- anyCheck operators
  if hasOperator
    then do
      node <- emptyNode nodeName'
      children <- parseOperandOnlyRest operators subParser [firstOperand]
      pure node {nodeChildren = map ChildNode children}
    else pure firstOperand

parseOperandOnlyRest :: [String] -> ParserM Node -> [Node] -> ParserM [Node]
parseOperandOnlyRest operators subParser children = do
  hasOperator <- anyCheck operators
  if hasOperator
    then do
      _ <- nextToken
      operand <- subParser
      parseOperandOnlyRest operators subParser (children <> [operand])
    else pure children

parseCastExpression :: ParserM Node
parseCastExpression = do
  startsCast <- check "("
  hasType <- checkAt 1 "TYPE_SPECIFIER"
  if startsCast && hasType
    then do
      node <- emptyNode "CAST_EXPRESSION"
      expect "("
      typeName <- parseTypeName
      pointerLevel <- countWhile "*"
      expect ")"
      castExpression <- parseCastExpression
      pure
        node
          { nodeFields =
              [ NodeField "type_specifier" (NodeRef typeName)
              , NodeField "pointer_level" (IntValue pointerLevel)
              , NodeField "cast_expression" (NodeRef castExpression)
              ]
          }
    else parseUnaryExpression

parseTypeName :: ParserM Node
parseTypeName = do
  node <- emptyNode "TYPE_NAME"
  typeSpecifier <- parseTypeSpecifier
  declarator <- parseAbstractDeclarator
  pure node {nodeFields = [NodeField "type_specifier" (NodeRef typeSpecifier), NodeField "declarator" (NodeRef declarator)]}

parseAbstractDeclarator :: ParserM Node
parseAbstractDeclarator = do
  node <- emptyNode "ABSTRACT_DECLARATOR"
  pointerLevel <- countWhile "*"
  directFields <-
    ifM
      (anyCheck ["[", "("])
      ( do
          direct <- parseDirectAbstractDeclarator
          pure [NodeField "direct_abstract_declarator" (NodeRef direct)]
      )
      (pure [])
  pure node {nodeFields = [NodeField "pointer_level" (IntValue pointerLevel)] <> directFields}

parseDirectAbstractDeclarator :: ParserM Node
parseDirectAbstractDeclarator = do
  node <- emptyNode "DIRECT_ABSTRACT_DECLARATOR"
  baseFields <-
    ifM
      (accept "(")
      ( do
          declarator <- parseAbstractDeclarator
          expect ")"
          pure [NodeField "declarator" (NodeRef declarator)]
      )
      (pure [])
  (children, parameterField) <- parseDirectAbstractDeclaratorSuffixes
  pure node {nodeChildren = map ChildNode children, nodeFields = baseFields <> parameterField}

parseDirectAbstractDeclaratorSuffixes :: ParserM ([Node], [NodeField])
parseDirectAbstractDeclaratorSuffixes = do
  hasArray <- accept "["
  if hasArray
    then do
      value <- parseIntegerConstant
      expect "]"
      (restChildren, fields) <- parseDirectAbstractDeclaratorSuffixes
      pure (value : restChildren, fields)
    else do
      hasParams <- accept "("
      if hasParams
        then do
          params <- parseParameterList
          expect ")"
          (children, fields) <- parseDirectAbstractDeclaratorSuffixes
          pure (children, NodeField "parameter_list" (NodeRef params) : fields)
        else pure ([], [])

parseIntegerConstant :: ParserM Node
parseIntegerConstant = do
  node <- emptyNode "INT"
  value <- tokenInteger <$> nextToken
  pure node {nodeFields = [NodeField "value" (IntValue value)]}

parseUnaryExpression :: ParserM Node
parseUnaryExpression = do
  token <- peekToken
  case effectiveTokenName token of
    op | op `elem` ["++", "--"] -> parseUnaryWithChild "UNARY_EXPRESSION" (tokenString token) (nextToken >> parseUnaryExpression)
    "SIZEOF" -> parseSizeofExpression
    op | op `elem` ["&", "*", "+", "-", "~", "!"] -> parseUnaryWithChild "UNARY_EXPRESSION" (tokenString token) (nextToken >> parseCastExpression)
    _ -> parsePostfixExpression

parseSizeofExpression :: ParserM Node
parseSizeofExpression = do
  node <- emptyNode "UNARY_EXPRESSION"
  expect "SIZEOF"
  child <-
    ifM
      ((&&) <$> check "(" <*> checkAt 1 "TYPE_SPECIFIER")
      ( do
          expect "("
          typeName <- parseTypeName
          expect ")"
          pure typeName
      )
      parseUnaryExpression
  pure node {nodeFields = [NodeField "operator" (StringValue "SIZEOF"), NodeField "child" (NodeRef child)]}

parseUnaryWithChild :: String -> String -> ParserM Node -> ParserM Node
parseUnaryWithChild name op childParser = do
  node <- emptyNode name
  child <- childParser
  pure node {nodeFields = [NodeField "operator" (StringValue op), NodeField "child" (NodeRef child)]}

parsePostfixExpression :: ParserM Node
parsePostfixExpression = do
  start <- tokenPos <$> peekToken
  primary <- parsePrimaryExpression
  ops <- parsePostfixOps
  suffixOp <- parsePostfixSuffixOp
  let allOps = ops <> suffixOp
  if null allOps
    then pure primary
    else do
      node <- emptyNode "POSTFIX_EXPRESSION"
      pure node {nodePos = start, nodeChildren = map ChildPostfix allOps, nodeFields = [NodeField "primary_expression" (NodeRef primary)]}

parsePostfixOps :: ParserM [PostfixOp]
parsePostfixOps = do
  token <- peekToken
  case effectiveTokenName token of
    "[" -> do
      expect "["
      value <- parseExpression
      expect "]"
      rest <- parsePostfixOps
      pure (PostfixOp "[" (Just (NodeRef value)) : rest)
    "(" -> do
      expect "("
      value <- parseArgumentList
      expect ")"
      rest <- parsePostfixOps
      pure (PostfixOp "(" (Just (NodeRef value)) : rest)
    "." -> do
      expect "."
      value <- parseIdentifier
      rest <- parsePostfixOps
      pure (PostfixOp "." (Just (NodeRef value)) : rest)
    "->" -> do
      expect "->"
      value <- parseIdentifier
      rest <- parsePostfixOps
      pure (PostfixOp "->" (Just (NodeRef value)) : rest)
    _ -> pure []

parsePostfixSuffixOp :: ParserM [PostfixOp]
parsePostfixSuffixOp = do
  token <- peekToken
  case effectiveTokenName token of
    "++" -> nextToken >> pure [PostfixOp "++" Nothing]
    "--" -> nextToken >> pure [PostfixOp "--" Nothing]
    _ -> pure []

parseArgumentList :: ParserM Node
parseArgumentList = do
  node <- emptyNode "ARGUMENT_LIST"
  closes <- check ")"
  if closes
    then pure node
    else do
      firstArgument <- parseAssignmentExpression
      rest <- parseMoreArguments
      pure node {nodeChildren = map ChildNode (firstArgument : rest)}

parseMoreArguments :: ParserM [Node]
parseMoreArguments = do
  hasComma <- accept ","
  if hasComma
    then do
      argument <- parseAssignmentExpression
      rest <- parseMoreArguments
      pure (argument : rest)
    else pure []

parsePrimaryExpression :: ParserM Node
parsePrimaryExpression = do
  token <- peekToken
  case effectiveTokenName token of
    "INT" -> do
      node <- emptyNode "INT"
      value <- tokenInteger <$> nextToken
      pure node {nodeFields = [NodeField "value" (IntValue value)]}
    "UNSIGNED_INT" -> do
      node <- emptyNode "INT"
      value <- tokenInteger <$> nextToken
      pure node {nodeFields = [NodeField "is_unsigned" (BoolValue True), NodeField "value" (IntValue value)]}
    "ID" -> do
      node <- emptyNode "IDENTIFIER"
      value <- tokenString <$> nextToken
      pure node {nodeFields = [NodeField "value" (StringValue value)]}
    "STRING_LITERAL" -> do
      node <- emptyNode "STRING_LITERAL"
      value <- stripQuotes . tokenString <$> nextToken
      pure node {nodeFields = [NodeField "value" (StringValue value)]}
    "CHARACTER" -> do
      node <- emptyNode "CHARACTER"
      value <- tokenValue <$> nextToken
      let nodeValue =
            case value of
              ValueInt intValue -> IntValue intValue
              ValueString stringValue -> StringValue stringValue
      pure node {nodeFields = [NodeField "value" nodeValue]}
    "(" -> do
      expect "("
      expression <- parseExpression
      expect ")"
      pure expression
    _ -> failAtPeek "Unexpected token in primary expression"

parseIdentifier :: ParserM Node
parseIdentifier = do
  node <- emptyNode "IDENTIFIER"
  token <- nextToken
  unless (effectiveTokenName token == "ID") $
    failAt token "Unexpected identifier"
  pure node {nodePos = tokenPos token, nodeFields = [NodeField "id" (StringValue (tokenString token))]}

emptyNode :: String -> ParserM Node
emptyNode name = do
  token <- peekToken
  emptyNodeAt name (tokenPos token)

emptyNodeAt :: String -> SourcePos -> ParserM Node
emptyNodeAt name pos =
  pure
    Node
      { nodeTypeId = nodeTypeIdFor name
      , nodeName = name
      , nodePos = pos
      , nodeChildren = []
      , nodeFields = []
      }

manyUntil :: String -> ParserM Node -> ParserM [Node]
manyUntil end parser = do
  done <- check end
  if done
    then pure []
    else do
      item <- parser
      rest <- manyUntil end parser
      pure (item : rest)

peekToken :: ParserM Token
peekToken = lift (MP.lookAhead MP.anySingle)

nextToken :: ParserM Token
nextToken = lift MP.anySingle

check :: String -> ParserM Bool
check expected = do
  token <- peekToken
  pure (effectiveTokenName token == expected)

anyCheck :: [String] -> ParserM Bool
anyCheck names = do
  token <- peekToken
  pure (effectiveTokenName token `elem` names)

checkAt :: Int -> String -> ParserM Bool
checkAt offset expected = do
  tokens <- lift MP.getInput
  case drop offset tokens of
    token : _ -> pure (effectiveTokenName token == expected)
    [] -> pure False

accept :: String -> ParserM Bool
accept expected = do
  matches <- check expected
  when matches (nextToken >> pure ())
  pure matches

expect :: String -> ParserM ()
expect expected = do
  token <- nextToken
  unless (effectiveTokenName token == expected) $
    failAt token ("Expected " <> expected)

expectType :: String -> ParserM ()
expectType expected = do
  matches <- check expected
  unless matches (failAtPeek ("Expected " <> expected))

countWhile :: String -> ParserM Integer
countWhile expected = do
  accepted <- accept expected
  if accepted
    then (1 +) <$> countWhile expected
    else pure 0

rememberTypedef :: Node -> ParserM ()
rememberTypedef declarator =
  case stringField "id" (directDeclaratorId declarator) <|> stringField "value" (directDeclaratorId declarator) of
    Just name -> modify' (\state -> state {parserTypedefs = Set.insert name (parserTypedefs state)})
    Nothing -> pure ()

effectiveTokenName :: Token -> String
effectiveTokenName = tokenName

tokenString :: Token -> String
tokenString token =
  case tokenValue token of
    ValueString value -> value
    ValueInt value -> show value

tokenInteger :: Token -> Integer
tokenInteger token =
  case tokenValue token of
    ValueInt value -> value
    ValueString value -> read value

directDeclaratorId :: Node -> Node
directDeclaratorId node =
  case fieldByName "id" node of
    [NodeField _ (NodeRef identifier)] -> identifier
    _ -> node

parameterIsVoidOnly :: Node -> ParserM Bool
parameterIsVoidOnly parameter =
  pure $
    case fieldByName "type_specifier" parameter of
      [NodeField _ (NodeRef typeSpecifier)] -> stringListField "kind" typeSpecifier == Just ["void"]
      _ -> False

storageClassKind :: Node -> Maybe String
storageClassKind declarationSpecifier =
  case fieldByName "storage_class" declarationSpecifier of
    [NodeField _ (NodeRef storageClass)] -> stringField "kind" storageClass
    _ -> Nothing

hasBoolField :: String -> Node -> Bool
hasBoolField name node =
  case fieldByName name node of
    [NodeField _ (BoolValue True)] -> True
    _ -> False

hasField :: String -> Node -> Bool
hasField name node = not (null (fieldByName name node))

hasBlockField :: Node -> Bool
hasBlockField = hasField "block"

fieldByName :: String -> Node -> [NodeField]
fieldByName name node = filter ((== name) . fieldName) (nodeFields node)

stringField :: String -> Node -> Maybe String
stringField name node =
  case fieldByName name node of
    [NodeField _ (StringValue value)] -> Just value
    _ -> Nothing

stringListField :: String -> Node -> Maybe [String]
stringListField name node =
  case fieldByName name node of
    [NodeField _ (StringList value)] -> Just value
    _ -> Nothing

stripQuotes :: String -> String
stripQuotes value =
  case value of
    '"' : rest ->
      case reverse rest of
        '"' : middle -> reverse middle
        _ -> value
    _ -> value

joinAsm :: [String] -> String
joinAsm [] = ""
joinAsm [value] = value
joinAsm (value : values) = value <> "\n\t" <> joinAsm values

addField :: NodeField -> Node -> Node
addField field node = node {nodeFields = nodeFields node <> [field]}

removeField :: String -> Node -> Node
removeField name node = node {nodeFields = filter ((/= name) . fieldName) (nodeFields node)}

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond ifTrue ifFalse = do
  result <- cond
  if result then ifTrue else ifFalse

failAtPeek :: String -> ParserM a
failAtPeek message = peekToken >>= \token -> failAt token message

failAt :: Token -> String -> ParserM a
failAt token message =
  lift . fail $
    message
      <> " at "
      <> show (row (tokenPos token))
      <> ":"
      <> show (col (tokenPos token))
      <> " near "
      <> show (tokenString token)

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just value <|> _ = Just value
Nothing <|> other = other
