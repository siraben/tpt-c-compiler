module Tptcc.CodeGen.Stdlib (renderStandardLibrary) where

import qualified Data.Map.Strict as Map

renderStandardLibrary :: [String] -> String
renderStandardLibrary =
  concatMap renderOne
  where
    renderOne name = maybe "" (substituteStdRegisters . stripInitialNewline) (Map.lookup name standardLibraryCode)

stripInitialNewline :: String -> String
stripInitialNewline ('\n' : rest) = rest
stripInitialNewline value = value

substituteStdRegisters :: String -> String
substituteStdRegisters [] = []
substituteStdRegisters ('%' : digit : rest)
  | Just reg <- lookup digit [('1', "r22"), ('2', "r23"), ('3', "r24"), ('4', "r25")] =
      reg <> substituteStdRegisters rest
substituteStdRegisters (char : rest) = char : substituteStdRegisters rest

standardLibraryCode :: Map.Map String String
standardLibraryCode =
  Map.fromList
    [
      ( "__print_unsigned_int"
      , "\n__tptcc_fn_print_unsigned_int:\n\ttest %1, %1\n\tjnz .__print_unsigned_int_not_zero\n\tmov %1, '0'\n\tst %1, term_print\n\tjmp .__print_unsigned_int_exit\n.__print_unsigned_int_not_zero:\n\tmov %2, 4\t\t; p = 4\n.__print_unsigned_int_fixed_point:\n\tmulh %3, %1, 52429\t; q = (n * 52429) >> 16\n\tshr %3, 3\t\t; q >>= 3\n\tmul %4, %3, 10\t\t; d*q\n\tsub %1, %4\t\t; remainder = n - d*q\n\tst %1, %2, .__print_unsigned_int_buf\t\t\n\tsub %2, 1\t\t; p--;\n\tmovf %1, %3\t\t; n = q\n\tjnz .__print_unsigned_int_fixed_point\n\n\tadd %2, 1\n.__print_unsigned_int_print_int:\n\tld %1, %2, .__print_unsigned_int_buf\n\tadd %1, '0'\n\tst %1, term_reg, term_base\n\tadd %2, 1\n\tcmp %2, 5\n\tjne .__print_unsigned_int_print_int\n\t\n.__print_unsigned_int_exit:\n\tret\n.__print_unsigned_int_buf:\n\tdw 0, 0, 0, 0, 0\n"
      )
    ,
      ( "__print_signed_int"
      , "\n__tptcc_fn_print_signed_int:\n    cmp %1, 0\n    jge .__print_signed_int_not_negative\n    mov %2, '-'\n    st %2, term_reg, term_base\n\txor %1, 65535\n    add %1, 1\n.__print_signed_int_not_negative:\n    call __tptcc_fn_print_unsigned_int\n    ret\n"
      )
    ,
      ( "__print_char_array"
      , "\n__tptcc_fn_print_char_array:\n    ld %2, %1\n    test %2, %2\n    jz .__print_char_array_exit\n    st %2, term_reg, term_base\n    add %1, 1\n    jmp __tptcc_fn_print_char_array\n.__print_char_array_exit:\n    ret\n"
      )
    ,
      ( "putchar"
      , "\n__tptcc_fn_putchar:\n    st %1, term_reg, term_base\n    ret\n"
      )
    ,
      ( "getchar"
      , "\n__tptcc_fn_getchar:\n    ld return_reg, term_input\n    test return_reg, return_reg\n    jz __tptcc_fn_getchar\n    ret\n"
      )
    ,
      ( "getchar_nb"
      , "\n__tptcc_fn_getchar_nb:\n    ld return_reg, term_input\n    ret\n"
      )
    ,
      ( "set_colour"
      , "\n__tptcc_fn_set_colour:\n    ; %1 = background, %2 = foreground\n    shl %1, 4\n    add %1, %2\n    st %1, term_colour\n    ret\n"
      )
    ,
      ( "set_text_colour"
      , "\n__tptcc_fn_set_text_colour:\n    st %1, term_colour\n    ret\n"
      )
    ,
      ( "__send_raw"
      , "\n__tptcc_fn_send_raw:\n    st %1, %2\n    ret\n"
      )
    ,
      ( "__set_zero_char"
      , "\n__tptcc_fn_set_zero_char:\n    exh %2, r0, %2\n    mov %1, %2, %1\n    st %1, term_print_e\n    exh %4, r0, %4\n    mov %3, %4, %3\n    st %3, term_print_o\n    ret\n"
      )
    ,
      ( "set_cursor"
      , "\n__tptcc_fn_set_cursor:\n    ; %1 = row, %2 = column\n    shl %1, 5\n    add %1, %2\n    st %1, term_cursor\n    ret\n"
      )
    ,
      ( "__scan_unsigned_int"
      , "\n__tptcc_fn_scan_unsigned_int:\n    mov %2, 0\n__scan_unsigned_int_loop:\n    call __tptcc_fn_getchar\n    st return_reg, term_reg, term_base\n    sub return_reg, '0'\n    cmp return_reg, 9\n    jg __scan_unsigned_int_not_digit\n    cmp return_reg, 0\n    jl __scan_unsigned_int_not_digit\n    mull %2, 10\n    add %2, return_reg\n    jmp __scan_unsigned_int_loop\n__scan_unsigned_int_not_digit:\n    st %2, %1\n    ret\n\n"
      )
    ,
      ( "vscroll"
      , "\n__tptcc_fn_vscroll:\n    mov %1, ' '\n    st %1, term_raw\n    ret\n"
      )
    ,
      ( "hscroll"
      , "\n__tptcc_fn_hscroll:\n    mov %1, ' '\n    st %1, term_base\n    ret\n"
      )
    ,
      ( "set_terminal_mode"
      , "\n__tptcc_fn_set_terminal_mode:\n    mov term_reg, %1\n    ret\n"
      )
    ,
      ( "get_terminal_mode"
      , "\n__tptcc_fn_get_terminal_mode:\n    mov return_reg, term_reg\n    ret\n"
      )
    ,
      ( "plot"
      , "\n__tptcc_fn_plot:\n    ; %1 = column/x, %2 = row/y, %3 = colour\n    shl %2, 8\n    add %2, %1\n    st %2, %3, term_plot\n    ret\n"
      )
    ,
      ( "set_hrange"
      , "\n__tptcc_fn_set_hrange:\n    ; %1 = start column, %2 = end column\n    shl %2, 5\n    add %2, %1\n    st %2, term_hrange\n    ret\n"
      )
    ,
      ( "set_vrange"
      , "\n__tptcc_fn_set_vrange:\n    ; %1 = start row, %2 = end row\n    shl %2, 5\n    add %2, %1\n    st %2, term_vrange\n    ret\n"
      )
    ]
