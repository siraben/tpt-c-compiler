module Tptcc.SymbolTable
  ( Symbol (..)
  , defaultSymbols
  ) where

import Data.Text (Text)

import Tptcc.CType
import Tptcc.Operand
import Tptcc.Tac (PlaceKind (..))

data Symbol = Symbol
  { symbolType :: CType
  , symbolPlace :: Maybe Operand
  , symbolIsTypeName :: Bool
  , symbolIsPrototype :: Bool
  }
  deriving (Eq, Show)

defaultSymbols :: [(Text, Symbol)]
defaultSymbols =
  [ constSymbol "NULL" (pointer (base "VOID")) 0
  , stdFunction "__print_unsigned_int" (function (base "VOID") [base "INT"]) "__tptcc_fn_print_unsigned_int" Nothing
  , stdFunction "__print_signed_int" (function (base "VOID") [base "INT"]) "__tptcc_fn_print_signed_int" Nothing
  , stdFunction "putchar" (function (base "VOID") [base "CHAR"]) "__tptcc_fn_putchar" Nothing
  , stdFunction "getchar" (function (base "CHAR") []) "__tptcc_fn_getchar" Nothing
  , stdFunction "getchar_nb" (function (base "CHAR") []) "__tptcc_fn_getchar_nb" Nothing
  , stdFunction "__scan_unsigned_int" (function (base "VOID") [pointer (base "INT")]) "__tptcc_fn_scan_unsigned_int" Nothing
  , stdFunction "set_colour" (function (base "VOID") [base "INT", base "INT"]) "__tptcc_fn_set_colour" Nothing
  , stdFunction "set_cursor" (function (base "VOID") [base "INT", base "INT"]) "__tptcc_fn_set_cursor" Nothing
  , stdFunction "__send_raw" (function (base "VOID") [base "INT", base "INT"]) "__tptcc_fn_send_raw" Nothing
  , stdFunction "__set_zero_char" (function (base "VOID") [base "INT", base "INT", base "INT", base "INT"]) "__tptcc_fn_set_zero_char" Nothing
  , stdFunction "__print_char_array" (function (base "VOID") [pointer (base "CHAR")]) "__tptcc_fn_print_char_array" (Just False)
  , stdFunction "vscroll" (function (base "VOID") []) "__tptcc_fn_vscroll" Nothing
  , stdFunction "hscroll" (function (base "VOID") []) "__tptcc_fn_hscroll" Nothing
  , stdFunction "set_terminal_mode" (function (base "VOID") [base "INT"]) "__tptcc_fn_set_terminal_mode" Nothing
  , stdFunction "get_terminal_mode" (function (base "INT") []) "__tptcc_fn_get_terminal_mode" Nothing
  , stdFunction "set_text_colour" (function (base "VOID") [base "INT"]) "__tptcc_fn_set_text_colour" Nothing
  , stdFunction "set_hrange" (function (base "VOID") [base "INT", base "INT"]) "__tptcc_fn_set_hrange" Nothing
  , stdFunction "set_vrange" (function (base "VOID") [base "INT", base "INT"]) "__tptcc_fn_set_vrange" Nothing
  , stdFunction "plot" (function (base "VOID") [base "INT", base "INT", base "INT"]) "__tptcc_fn_plot" Nothing
  , constSymbol "BLACK" (base "INT") 0
  , constSymbol "DARK_BLUE" (base "INT") 1
  , constSymbol "DARK_GREEN" (base "INT") 2
  , constSymbol "DARK_CYAN" (base "INT") 3
  , constSymbol "DARK_RED" (base "INT") 4
  , constSymbol "DARK_MAGENTA" (base "INT") 5
  , constSymbol "DARK_YELLOW" (base "INT") 6
  , constSymbol "GREY" (base "INT") 7
  , constSymbol "DARK_GREY" (base "INT") 8
  , constSymbol "BLUE" (base "INT") 9
  , constSymbol "GREEN" (base "INT") 10
  , constSymbol "CYAN" (base "INT") 11
  , constSymbol "RED" (base "INT") 12
  , constSymbol "MAGENTA" (base "INT") 13
  , constSymbol "YELLOW" (base "INT") 14
  , constSymbol "WHITE" (base "INT") 15
  , constSymbol "TERM_ENABLE_NL" (base "INT") 0x20
  , constSymbol "TERM_ENABLE_TERM_MODE_SCROLL" (base "INT") 0x10
  , constSymbol "TERM_ENABLE_SCROLLMASK" (base "INT") 0x08
  , constSymbol "TERM_ENABLE_ROW_ORIENTED" (base "INT") 0x04
  , constSymbol "TERM_ENABLE_ENABLE_COLOUR" (base "INT") 0x02
  , constSymbol "TERM_ENABLE_TERM_MODE" (base "INT") 0x01
  , constSymbol "TERM_DEFAULT" (base "INT") 0x25
  ]

stdFunction :: Text -> CType -> Text -> Maybe Bool -> (Text, Symbol)
stdFunction name ty target isVariadic =
  ( name
  , Symbol
      { symbolType = ty
      , symbolPlace =
          Just
            ( Operand
                { operandKind = Immediate
                , operandValue = OperandName target
                , operandIsStandardFunction = True
                , operandIsVariadic = isVariadic
                }
            )
      , symbolIsTypeName = False
      , symbolIsPrototype = False
      }
  )

constSymbol :: Text -> CType -> Integer -> (Text, Symbol)
constSymbol name ty value =
  ( name
  , Symbol
      { symbolType = ty
      , symbolPlace =
          Just
            ( Operand
                { operandKind = Immediate
                , operandValue = OperandInt value
                , operandIsStandardFunction = False
                , operandIsVariadic = Nothing
                }
            )
      , symbolIsTypeName = False
      , symbolIsPrototype = False
      }
  )
