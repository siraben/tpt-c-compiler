%include "common"

%define return_reg r31
%define stack_pointer r30
%define base_pointer r29
%define term_reg r28
%define return_addr_reg r27

; Initialization and defining basic macros
%define term_base 40832
%define term_height 8
%define term_width 12
 
%eval term_input  term_base 0x00 +
%eval term_raw    term_base 0x04 +
%eval term_single term_base 0x05 +
%eval term_print  term_base 0x25 +
%eval term_term   term_base 0x26 +
%eval term_hrange term_base 0x42 +
%eval term_vrange term_base 0x43 +
%eval term_cursor term_base 0x44 +
%eval term_nlchar term_base 0x45 +
%eval term_colour term_base 0x46 +
%eval term_print_e term_base 0x40 +
%eval term_print_o term_base 0x41 +
%eval term_plot    term_base 0x60 +


%macro push thing
    subs stack_pointer, 1
    st thing, stack_pointer
%endmacro

%macro pop thing
    ld thing, stack_pointer
    adds stack_pointer, 1
%endmacro

%macro call thing
    push return_addr_reg
    jmp return_addr_reg, thing
%endmacro

%macro ret
    mov r26, return_addr_reg
    pop return_addr_reg
    jmp r26
%endmacro

%macro mull x, y
    mul x, x, y
%endmacro

jmp init
global_data_section:
    dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 119, 121, 114, 124, 113, 116, 118, 117, 120, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 75, 97, 98, 111, 111, 109, 33, 32, 89, 111, 117, 32, 108, 111, 115, 101, 33, 0, 35, 32, 111, 102, 32, 109, 105, 110, 101, 115, 10, 40, 54, 45, 49, 50, 41, 58, 32, 0, 10, 69, 110, 116, 101, 114, 32, 97, 10, 110, 117, 109, 98, 101, 114, 32, 116, 111, 10, 104, 101, 108, 112, 10, 114, 97, 110, 100, 111, 109, 105, 122, 101, 10, 40, 49, 45, 49, 48, 48, 41, 58, 32, 0, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 0, 89, 111, 117, 32, 119, 105, 110, 33, 0
init:
    mov term_reg, 0x25
    mov r1, { term_width 1 - 5 << }
    st r1, term_hrange
    mov r1, { term_height 1 - 5 << }
    st r1, term_vrange
    mov r1, 0x1000
    st r1, term_cursor
    mov r1, 0xF
    st r1, term_colour
    mov r1, 10
    st r1,  term_nlchar

start:
    mov stack_pointer,8191
	jmp __tptcc_fn_main
__tptcc_fn___scan_signed_int:
	push base_pointer
	mov base_pointer, stack_pointer
	push r1
	push r2
	push r3
	push r4
	mov r1, 0
	call __tptcc_fn_getchar
	mov r2, return_reg
	mov r3, 45
	cmp return_reg, r3
	je .label_1
.ssa_bb_1:
	jmp .label_0
.label_1:
	mov r3, 65535
	jmp .label_2
.label_0:
	mov r1, r2
	sub r1, 48
	mov r3, 1
.label_2:
	mov r22, r2
	call __tptcc_fn_putchar
.label_3:
	call __tptcc_fn_getchar
	mov r2, return_reg
	mov r4, 48
	cmp return_reg, r4
	jge .label_6
.ssa_bb_6:
	jmp .label_4
.label_6:
	mov r4, 57
	cmp r2, r4
	jle .label_5
.ssa_bb_8:
	jmp .label_4
.label_5:
	mov r22, r2
	call __tptcc_fn_putchar
	mull r1, 10
	sub r2, 48
	add r1, r2
	jmp .label_3
.label_4:
	mov r4, 65535
	cmp r3, r4
	je .label_8
.ssa_bb_11:
	jmp .label_7
.label_8:
	xor r1, 65535
	add r1, 1
.label_7:
	ld r3, base_pointer, 2
	st r1, r3
	mov return_reg, r2
.exit___scan_signed_int:
	pop r4
	pop r3
	pop r2
	pop r1
	pop base_pointer
	ret
__tptcc_fn_show_bombs:
	push r2
	push r1
	push r3
	push r4
	mov r2, 0
.label_10:
	mov r1, 8
	cmp r2, r1
	jl .label_13
.ssa_bb_2:
	jmp .exit_show_bombs
.label_13:
	mov r1, 0
.label_14:
	mov r3, 12
	cmp r1, r3
	jl .label_17
.ssa_bb_5:
	jmp .label_16
.label_17:
	mov r4, 97
	mov r3, r2
	mull r3, 12
	add r4, r3
	ld r4, r4, r1
	mov r3, 9
	cmp r4, r3
	jge .label_19
.ssa_bb_7:
	jmp .label_18
.label_19:
	mov r23, r1
	mov r22, r2
	call __tptcc_fn_set_cursor
	mov r23, 15
	mov r22, 12
	call __tptcc_fn_set_colour
	mov r25, 39294
	mov r24, 32511
	mov r23, 53118
	mov r22, 20121
	call __tptcc_fn_set_zero_char
	mov r22, 0
	call __tptcc_fn_putchar
.label_18:
	add r1, 1
	jmp .label_14
.label_16:
	add r2, 1
	jmp .label_10
.exit_show_bombs:
	pop r4
	pop r3
	pop r1
	pop r2
	ret
__tptcc_fn_sweep_cell:
	sub stack_pointer, 192
	push base_pointer
	mov base_pointer, stack_pointer
	push r1
	push r4
	push r3
	push r2
	push r6
	push r5
	push r7
	push r8
	push r10
	push r9
	ld r1, base_pointer, 194
	ld r4, base_pointer, 195
	mov r3, 97
	mov r2, r1
	mull r2, 12
	add r3, r2
	ld r3, r3, r4
	mov r2, 9
	cmp r3, r2
	jge .label_22
.ssa_bb_1:
	jmp .label_25
.label_22:
	call __tptcc_fn_show_bombs
	mov r23, 0
	mov r22, 0
	call __tptcc_fn_set_cursor
	mov r22, 12
	call __tptcc_fn_set_text_colour
	mov r2, 395
	mov r22, r2
	call __tptcc_fn_print_char_array
	mov r22, 9
	call __tptcc_fn_set_text_colour
	mov r23, r4
	mov r22, r1
	call __tptcc_fn_set_cursor
	mov r22, 0
	call __tptcc_fn_putchar
.label_24:
	mov r2, 1
	cmp r2, 0
	je .label_25
.ssa_bb_4:
	jmp .label_24
.label_25:
	add r2, base_pointer, 1
	add r3, r2, 0
	st r1, r3
	add r1, r2, 1
	st r4, r1
	add r2, 2
	mov r1, 0
.label_27:
	add r3, base_pointer, 1
	cmp r2, r3
	jg .label_29
.ssa_bb_10:
	jmp .label_28
.label_29:
	add r3, r2, 65534
	ld r4, r3
	add r3, r2, 65535
	ld r6, r3
	sub r2, 2
	mov r5, 97
	mov r3, r4
	mull r3, 12
	add r5, r3
	add r5, r6
	ld r7, r5
	mov r8, r7
	add r8, 48
	mov r5, 1
	mov r3, r4
	mull r3, 12
	add r5, r3
	add r5, r6
	st r8, r5
	mov r23, r6
	mov r22, r4
	call __tptcc_fn_set_cursor
	mov r3, 289
	add r3, r7
	ld r22, r3
	call __tptcc_fn_set_text_colour
	mov r22, r8
	call __tptcc_fn_putchar
	add r1, 1
	mov r3, 0
	cmp r7, r3
	je .label_31
.ssa_bb_12:
	jmp .label_122
.label_31:
	mov r10, r4
	sub r10, 1
	mov r9, r6
	sub r9, 1
	mov r8, r4
	add r8, 1
	mov r7, r6
	add r7, 1
	mov r5, 193
	mov r3, r4
	mull r3, 12
	add r5, r3
	add r5, r6
	ld r3, r5
	cmp r3, 0
	je .label_33
.ssa_bb_14:
	mov r3, 1
	mov r5, r10
	mull r5, 12
	add r3, r5
	add r3, r9
	ld r5, r3
	mov r3, 0
	cmp r5, r3
	je .label_37
.ssa_bb_16:
	mov r3, 1
	mov r5, r10
	mull r5, 12
	add r3, r5
	add r3, r9
	ld r5, r3
	mov r3, 70
	cmp r5, r3
	je .label_37
.ssa_bb_18:
	jmp .label_36
.label_37:
	mov r5, 1
	mov r3, r10
	mull r3, 12
	add r5, r3
	add r5, r9
	mov r3, 16
	st r3, r5
	add r3, r2, 0
	st r10, r3
	add r3, r2, 1
	st r9, r3
	add r2, 2
.label_36:
	mov r5, 1
	mov r3, r10
	mull r3, 12
	add r5, r3
	ld r5, r5, r7
	mov r3, 0
	cmp r5, r3
	je .label_41
.ssa_bb_22:
	mov r5, 1
	mov r3, r10
	mull r3, 12
	add r5, r3
	ld r5, r5, r7
	mov r3, 70
	cmp r5, r3
	je .label_41
.ssa_bb_24:
	jmp .label_40
.label_41:
	mov r5, 1
	mov r3, r10
	mull r3, 12
	add r5, r3
	add r5, r7
	mov r3, 16
	st r3, r5
	add r3, r2, 0
	st r10, r3
	add r3, r2, 1
	st r7, r3
	add r2, 2
.label_40:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	ld r5, r5, r9
	mov r3, 0
	cmp r5, r3
	je .label_45
.ssa_bb_28:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	ld r5, r5, r9
	mov r3, 70
	cmp r5, r3
	je .label_45
.ssa_bb_30:
	jmp .label_44
.label_45:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	add r5, r9
	mov r3, 16
	st r3, r5
	add r3, r2, 0
	st r8, r3
	add r3, r2, 1
	st r9, r3
	add r2, 2
.label_44:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	ld r5, r5, r7
	mov r3, 0
	cmp r5, r3
	je .label_49
.ssa_bb_34:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	ld r5, r5, r7
	mov r3, 70
	cmp r5, r3
	je .label_49
.ssa_bb_36:
	jmp .label_48
.label_49:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	add r5, r7
	mov r3, 16
	st r3, r5
	add r3, r2, 0
	st r8, r3
	add r3, r2, 1
	st r7, r3
	add r2, 2
.label_48:
	mov r5, 1
	mov r3, r4
	mull r3, 12
	add r5, r3
	ld r5, r5, r9
	mov r3, 0
	cmp r5, r3
	je .label_53
.ssa_bb_40:
	mov r5, 1
	mov r3, r4
	mull r3, 12
	add r5, r3
	ld r5, r5, r9
	mov r3, 70
	cmp r5, r3
	je .label_53
.ssa_bb_42:
	jmp .label_52
.label_53:
	mov r5, 1
	mov r3, r4
	mull r3, 12
	add r5, r3
	add r5, r9
	mov r3, 16
	st r3, r5
	add r3, r2, 0
	st r4, r3
	add r3, r2, 1
	st r9, r3
	add r2, 2
.label_52:
	mov r5, 1
	mov r3, r4
	mull r3, 12
	add r5, r3
	ld r5, r5, r7
	mov r3, 0
	cmp r5, r3
	je .label_57
.ssa_bb_46:
	mov r5, 1
	mov r3, r4
	mull r3, 12
	add r5, r3
	ld r5, r5, r7
	mov r3, 70
	cmp r5, r3
	je .label_57
.ssa_bb_48:
	jmp .label_56
.label_57:
	mov r5, 1
	mov r3, r4
	mull r3, 12
	add r5, r3
	add r5, r7
	mov r3, 16
	st r3, r5
	add r3, r2, 0
	st r4, r3
	add r3, r2, 1
	st r7, r3
	add r2, 2
.label_56:
	mov r4, 1
	mov r3, r8
	mull r3, 12
	add r4, r3
	ld r4, r4, r6
	mov r3, 0
	cmp r4, r3
	je .label_61
.ssa_bb_52:
	mov r4, 1
	mov r3, r8
	mull r3, 12
	add r4, r3
	ld r4, r4, r6
	mov r3, 70
	cmp r4, r3
	je .label_61
.ssa_bb_54:
	jmp .label_60
.label_61:
	mov r4, 1
	mov r3, r8
	mull r3, 12
	add r4, r3
	add r4, r6
	mov r3, 16
	st r3, r4
	add r3, r2, 0
	st r8, r3
	add r3, r2, 1
	st r6, r3
	add r2, 2
.label_60:
	mov r4, 1
	mov r3, r10
	mull r3, 12
	add r4, r3
	ld r4, r4, r6
	mov r3, 0
	cmp r4, r3
	je .label_65
.ssa_bb_58:
	mov r4, 1
	mov r3, r10
	mull r3, 12
	add r4, r3
	ld r4, r4, r6
	mov r3, 70
	cmp r4, r3
	je .label_65
.ssa_bb_60:
	jmp .label_64
.label_65:
	mov r4, 1
	mov r3, r10
	mull r3, 12
	add r4, r3
	add r4, r6
	mov r3, 16
	st r3, r4
	add r3, r2, 0
	st r10, r3
	add r3, r2, 1
	st r6, r3
	add r2, 2
.label_64:
	jmp .label_122
.label_33:
	mov r3, 0
	cmp r4, r3
	jg .label_69
.ssa_bb_65:
	jmp .label_87
.label_69:
	mov r3, 0
	cmp r6, r3
	jg .label_74
.ssa_bb_67:
	jmp .label_71
.label_74:
	mov r5, 1
	mov r3, r10
	mull r3, 12
	add r5, r3
	ld r5, r5, r9
	mov r3, 0
	cmp r5, r3
	je .label_76
.ssa_bb_69:
	mov r5, 1
	mov r3, r10
	mull r3, 12
	add r5, r3
	ld r5, r5, r9
	mov r3, 70
	cmp r5, r3
	je .label_76
.ssa_bb_71:
	jmp .label_75
.label_76:
	mov r3, 1
	jmp .label_77
.label_75:
	mov r3, 0
.label_77:
	cmp r3, 0
	je .label_71
.ssa_bb_75:
	mov r5, 1
	mov r3, r10
	mull r3, 12
	add r5, r3
	add r5, r9
	mov r3, 16
	st r3, r5
	add r3, r2, 0
	st r10, r3
	add r3, r2, 1
	st r9, r3
	add r2, 2
.label_71:
	mov r3, 11
	cmp r6, r3
	jl .label_82
.ssa_bb_79:
	jmp .label_79
.label_82:
	mov r5, 1
	mov r3, r10
	mull r3, 12
	add r5, r3
	ld r5, r5, r7
	mov r3, 0
	cmp r5, r3
	je .label_84
.ssa_bb_81:
	mov r5, 1
	mov r3, r10
	mull r3, 12
	add r5, r3
	ld r5, r5, r7
	mov r3, 70
	cmp r5, r3
	je .label_84
.ssa_bb_83:
	jmp .label_83
.label_84:
	mov r3, 1
	jmp .label_85
.label_83:
	mov r3, 0
.label_85:
	cmp r3, 0
	je .label_79
.ssa_bb_87:
	mov r5, 1
	mov r3, r10
	mull r3, 12
	add r5, r3
	add r5, r7
	mov r3, 16
	st r3, r5
	add r3, r2, 0
	st r10, r3
	add r3, r2, 1
	st r7, r3
	add r2, 2
.label_79:
	mov r5, 1
	mov r3, r10
	mull r3, 12
	add r5, r3
	ld r5, r5, r6
	mov r3, 0
	cmp r5, r3
	je .label_88
.ssa_bb_91:
	mov r5, 1
	mov r3, r10
	mull r3, 12
	add r5, r3
	ld r5, r5, r6
	mov r3, 70
	cmp r5, r3
	je .label_88
.ssa_bb_93:
	jmp .label_87
.label_88:
	mov r5, 1
	mov r3, r10
	mull r3, 12
	add r5, r3
	add r5, r6
	mov r3, 16
	st r3, r5
	add r3, r2, 0
	st r10, r3
	add r3, r2, 1
	st r6, r3
	add r2, 2
.label_87:
	mov r3, 7
	cmp r4, r3
	jl .label_92
.ssa_bb_99:
	jmp .label_110
.label_92:
	mov r3, 0
	cmp r6, r3
	jg .label_97
.ssa_bb_101:
	jmp .label_94
.label_97:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	ld r5, r5, r9
	mov r3, 0
	cmp r5, r3
	je .label_99
.ssa_bb_103:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	ld r5, r5, r9
	mov r3, 70
	cmp r5, r3
	je .label_99
.ssa_bb_105:
	jmp .label_98
.label_99:
	mov r3, 1
	jmp .label_100
.label_98:
	mov r3, 0
.label_100:
	cmp r3, 0
	je .label_94
.ssa_bb_109:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	add r5, r9
	mov r3, 16
	st r3, r5
	add r3, r2, 0
	st r8, r3
	add r3, r2, 1
	st r9, r3
	add r2, 2
.label_94:
	mov r3, 11
	cmp r6, r3
	jl .label_105
.ssa_bb_113:
	jmp .label_102
.label_105:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	ld r5, r5, r7
	mov r3, 0
	cmp r5, r3
	je .label_107
.ssa_bb_115:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	ld r5, r5, r7
	mov r3, 70
	cmp r5, r3
	je .label_107
.ssa_bb_117:
	jmp .label_106
.label_107:
	mov r3, 1
	jmp .label_108
.label_106:
	mov r3, 0
.label_108:
	cmp r3, 0
	je .label_102
.ssa_bb_121:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	add r5, r7
	mov r3, 16
	st r3, r5
	add r3, r2, 0
	st r8, r3
	add r3, r2, 1
	st r7, r3
	add r2, 2
.label_102:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	ld r5, r5, r6
	mov r3, 0
	cmp r5, r3
	je .label_111
.ssa_bb_125:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	ld r5, r5, r6
	mov r3, 70
	cmp r5, r3
	je .label_111
.ssa_bb_127:
	jmp .label_110
.label_111:
	mov r5, 1
	mov r3, r8
	mull r3, 12
	add r5, r3
	add r5, r6
	mov r3, 16
	st r3, r5
	add r3, r2, 0
	st r8, r3
	add r3, r2, 1
	st r6, r3
	add r2, 2
.label_110:
	mov r3, 0
	cmp r6, r3
	jg .label_117
.ssa_bb_133:
	jmp .label_114
.label_117:
	mov r5, 1
	mov r3, r4
	mull r3, 12
	add r5, r3
	ld r5, r5, r9
	mov r3, 0
	cmp r5, r3
	je .label_119
.ssa_bb_135:
	mov r5, 1
	mov r3, r4
	mull r3, 12
	add r5, r3
	ld r5, r5, r9
	mov r3, 70
	cmp r5, r3
	je .label_119
.ssa_bb_137:
	jmp .label_118
.label_119:
	mov r3, 1
	jmp .label_120
.label_118:
	mov r3, 0
.label_120:
	cmp r3, 0
	je .label_114
.ssa_bb_141:
	mov r5, 1
	mov r3, r4
	mull r3, 12
	add r5, r3
	add r5, r9
	mov r3, 16
	st r3, r5
	add r3, r2, 0
	st r4, r3
	add r3, r2, 1
	st r9, r3
	add r2, 2
.label_114:
	mov r3, 11
	cmp r6, r3
	jl .label_125
.ssa_bb_145:
	jmp .label_122
.label_125:
	mov r5, 1
	mov r3, r4
	mull r3, 12
	add r5, r3
	ld r5, r5, r7
	mov r3, 0
	cmp r5, r3
	je .label_127
.ssa_bb_147:
	mov r5, 1
	mov r3, r4
	mull r3, 12
	add r5, r3
	ld r5, r5, r7
	mov r3, 70
	cmp r5, r3
	je .label_127
.ssa_bb_149:
	jmp .label_126
.label_127:
	mov r3, 1
	jmp .label_128
.label_126:
	mov r3, 0
.label_128:
	cmp r3, 0
	je .label_122
.ssa_bb_153:
	mov r5, 1
	mov r3, r4
	mull r3, 12
	add r5, r3
	add r5, r7
	mov r3, 16
	st r3, r5
	add r3, r2, 0
	st r4, r3
	add r3, r2, 1
	st r7, r3
	add r2, 2
.label_122:
	jmp .label_27
.label_28:
	ld r2, 298
	add r2, r1
	st r2, 298
.exit_sweep_cell:
	pop r9
	pop r10
	pop r8
	pop r7
	pop r5
	pop r6
	pop r2
	pop r3
	pop r4
	pop r1
	pop base_pointer
	add stack_pointer, 192
	ret
__tptcc_fn_add_to_surrounding_cells:
	push base_pointer
	mov base_pointer, stack_pointer
	push r2
	push r6
	push r8
	push r7
	push r5
	push r4
	push r1
	push r3
	push r9
	ld r2, base_pointer, 2
	ld r6, base_pointer, 3
	mov r8, r2
	sub r8, 1
	mov r7, r6
	sub r7, 1
	mov r5, r2
	add r5, 1
	mov r4, r6
	add r4, 1
	mov r1, 0
	cmp r2, r1
	jg .label_131
.ssa_bb_1:
	jmp .label_136
.label_131:
	mov r1, 0
	cmp r6, r1
	jg .label_134
.ssa_bb_3:
	jmp .label_133
.label_134:
	mov r3, 97
	mov r1, r8
	mull r1, 12
	add r3, r1
	add r3, r7
	ld r1, r3
	ld r9, base_pointer, 4
	add r1, r9
	st r1, r3
.label_133:
	mov r3, 97
	mov r1, r8
	mull r1, 12
	add r3, r1
	add r3, r6
	ld r1, r3
	ld r9, base_pointer, 4
	add r1, r9
	st r1, r3
	mov r1, 11
	cmp r6, r1
	jl .label_137
.ssa_bb_7:
	jmp .label_136
.label_137:
	mov r3, 97
	mull r8, 12
	add r3, r8
	add r3, r4
	ld r1, r3
	ld r8, base_pointer, 4
	add r1, r8
	st r1, r3
.label_136:
	mov r1, 0
	cmp r6, r1
	jg .label_140
.ssa_bb_13:
	jmp .label_139
.label_140:
	mov r3, 97
	mov r1, r2
	mull r1, 12
	add r3, r1
	add r3, r7
	ld r1, r3
	ld r8, base_pointer, 4
	add r1, r8
	st r1, r3
.label_139:
	mov r1, 11
	cmp r6, r1
	jl .label_143
.ssa_bb_17:
	jmp .label_142
.label_143:
	mov r3, 97
	mov r1, r2
	mull r1, 12
	add r3, r1
	add r3, r4
	ld r1, r3
	ld r8, base_pointer, 4
	add r1, r8
	st r1, r3
.label_142:
	mov r1, 7
	cmp r2, r1
	jl .label_146
.ssa_bb_21:
	jmp .exit_add_to_surrounding_cells
.label_146:
	mov r1, 0
	cmp r6, r1
	jg .label_149
.ssa_bb_23:
	jmp .label_148
.label_149:
	mov r2, 97
	mov r1, r5
	mull r1, 12
	add r2, r1
	add r2, r7
	ld r1, r2
	ld r3, base_pointer, 4
	add r1, r3
	st r1, r2
.label_148:
	mov r2, 97
	mov r1, r5
	mull r1, 12
	add r2, r1
	add r2, r6
	ld r1, r2
	ld r3, base_pointer, 4
	add r1, r3
	st r1, r2
	mov r1, 11
	cmp r6, r1
	jl .label_152
.ssa_bb_27:
	jmp .exit_add_to_surrounding_cells
.label_152:
	mov r2, 97
	mull r5, 12
	add r2, r5
	add r2, r4
	ld r1, r2
	ld r3, base_pointer, 4
	add r1, r3
	st r1, r2
.exit_add_to_surrounding_cells:
	pop r9
	pop r3
	pop r1
	pop r4
	pop r5
	pop r7
	pop r8
	pop r6
	pop r2
	pop base_pointer
	ret
__tptcc_fn_main:
	sub stack_pointer, 2
	push base_pointer
	mov base_pointer, stack_pointer
	mov r1, 97
	mov r2, 9
	st r2, r1
	mov r7, 97
	mov r1, 413
	mov r22, r1
	call __tptcc_fn_print_char_array
	add r1, base_pointer, 1
	push r1
	call __tptcc_fn___scan_signed_int
	add stack_pointer, 1
	ld r6, base_pointer, 1
	mov r4, 96
	sub r4, r6
	mov r22, 14
	call __tptcc_fn_set_text_colour
	mov r1, 433
	mov r22, r1
	call __tptcc_fn_print_char_array
	add r1, base_pointer, 2
	push r1
	call __tptcc_fn___scan_signed_int
	add stack_pointer, 1
	ld r2, base_pointer, 2
	xor r2, 65535
	add r2, 1
	mov r22, 10
	call __tptcc_fn_putchar
	mov r23, 8
	mov r22, 8
	call __tptcc_fn_set_colour
	mov r23, 0
	mov r22, 7
	call __tptcc_fn_set_cursor
	mov r1, 477
	mov r22, r1
	call __tptcc_fn_print_char_array
	mov r23, 0
	mov r22, 7
	call __tptcc_fn_set_cursor
	mov r23, 10
	mov r22, 10
	call __tptcc_fn_set_colour
	mov r1, 0
.label_154:
	cmp r1, r6
	jl .label_157
.ssa_bb_2:
	jmp .label_156
.label_157:
	mov r3, r2
	add r3, 1
	shl r3, 3
	xor r2, r3
	mov r3, r2
	shr r3, 5
	xor r2, r3
	mov r3, r2
	shl r3, 2
	xor r2, r3
	mov r10, r2
	and r10, 127
.label_158:
	mov r3, 96
	cmp r10, r3
	jge .label_160
.ssa_bb_5:
	mov r3, r7
	add r3, r10
	ld r5, r3
	mov r3, 9
	cmp r5, r3
	jge .label_160
.ssa_bb_7:
	jmp .label_159
.label_160:
	mov r3, r2
	add r3, 1
	shl r3, 3
	xor r2, r3
	mov r3, r2
	shr r3, 5
	xor r2, r3
	mov r3, r2
	shl r3, 2
	xor r2, r3
	mov r10, r2
	and r10, 127
	jmp .label_158
.label_159:
	mov r5, r7
	add r5, r10
	ld r3, r5
	add r3, 9
	st r3, r5
	mov r3, 299
	add r3, r10
	ld r5, r3
	mov r3, r5
	mull r3, 12
	sub r10, r3
	mov r13, r5
	sub r13, 1
	mov r12, r10
	sub r12, 1
	mov r9, r5
	add r9, 1
	mov r8, r10
	add r8, 1
	mov r3, 0
	cmp r5, r3
	jg .label_163
.ssa_bb_10:
	jmp .label_168
.label_163:
	mov r3, 0
	cmp r10, r3
	jg .label_166
.ssa_bb_12:
	jmp .label_165
.label_166:
	mov r11, 97
	mov r3, r13
	mull r3, 12
	add r11, r3
	add r11, r12
	ld r3, r11
	add r3, 1
	st r3, r11
.label_165:
	mov r11, 97
	mov r3, r13
	mull r3, 12
	add r11, r3
	add r11, r10
	ld r3, r11
	add r3, 1
	st r3, r11
	mov r3, 11
	cmp r10, r3
	jl .label_169
.ssa_bb_16:
	jmp .label_168
.label_169:
	mov r11, 97
	mull r13, 12
	add r11, r13
	add r11, r8
	ld r3, r11
	add r3, 1
	st r3, r11
.label_168:
	mov r3, 0
	cmp r10, r3
	jg .label_172
.ssa_bb_22:
	jmp .label_171
.label_172:
	mov r11, 97
	mov r3, r5
	mull r3, 12
	add r11, r3
	add r11, r12
	ld r3, r11
	add r3, 1
	st r3, r11
.label_171:
	mov r3, 11
	cmp r10, r3
	jl .label_175
.ssa_bb_26:
	jmp .label_174
.label_175:
	mov r11, 97
	mov r3, r5
	mull r3, 12
	add r11, r3
	add r11, r8
	ld r3, r11
	add r3, 1
	st r3, r11
.label_174:
	mov r3, 7
	cmp r5, r3
	jl .label_178
.ssa_bb_30:
	jmp .label_183
.label_178:
	mov r3, 0
	cmp r10, r3
	jg .label_181
.ssa_bb_32:
	jmp .label_180
.label_181:
	mov r5, 97
	mov r3, r9
	mull r3, 12
	add r5, r3
	add r5, r12
	ld r3, r5
	add r3, 1
	st r3, r5
.label_180:
	mov r5, 97
	mov r3, r9
	mull r3, 12
	add r5, r3
	add r5, r10
	ld r3, r5
	add r3, 1
	st r3, r5
	mov r3, 11
	cmp r10, r3
	jl .label_184
.ssa_bb_36:
	jmp .label_183
.label_184:
	mov r5, 97
	mull r9, 12
	add r5, r9
	add r5, r8
	ld r3, r5
	add r3, 1
	st r3, r5
.label_183:
	mov r22, 46
	call __tptcc_fn_putchar
.label_155:
	add r1, 1
	jmp .label_154
.label_156:
	mov r2, 97
	ld r1, r2
	sub r1, 9
	st r1, r2
	mov r23, 0
	mov r22, 0
	call __tptcc_fn_set_cursor
	mov r23, 15
	mov r22, 7
	call __tptcc_fn_set_colour
	mov r25, 65409
	mov r24, 33153
	mov r23, 33153
	mov r22, 33279
	call __tptcc_fn_set_zero_char
	mov r1, 0
.label_186:
	mov r2, 8
	cmp r1, r2
	jl .label_189
.ssa_bb_45:
	jmp .label_188
.label_189:
	mov r23, 40836
	mov r22, 0
	call __tptcc_fn_send_raw
.label_187:
	add r1, 1
	jmp .label_186
.label_188:
	mov r23, 0
	mov r22, 0
	call __tptcc_fn_set_cursor
	mov r1, 1
	mov r2, 0
	mov r3, 0
.label_190:
	mov r5, 1
	cmp r5, 0
	je .exit_main
.ssa_bb_50:
	mov r23, r2
	mov r22, r3
	call __tptcc_fn_set_cursor
	mov r22, 9
	call __tptcc_fn_set_text_colour
	mov r6, 1
	mov r5, r3
	mull r5, 12
	add r6, r5
	add r6, r2
	ld r5, r6
	mov r6, 70
	cmp r5, r6
	je .label_194
.ssa_bb_52:
	jmp .label_193
.label_194:
	mov r25, 127
	mov r24, 112
	mov r23, 28672
	mov r22, 28672
	call __tptcc_fn_set_zero_char
	mov r22, 0
	call __tptcc_fn_putchar
	jmp .label_204
.label_193:
	mov r6, 0
	cmp r5, r6
	je .label_197
.ssa_bb_55:
	jmp .label_196
.label_197:
	mov r25, 65409
	mov r24, 33153
	mov r23, 33153
	mov r22, 33279
	call __tptcc_fn_set_zero_char
	mov r22, 0
	call __tptcc_fn_putchar
	jmp .label_204
.label_196:
	mov r6, 66
	cmp r5, r6
	je .label_200
.ssa_bb_58:
	jmp .label_199
.label_200:
	mov r25, 39294
	mov r24, 32511
	mov r23, 53118
	mov r22, 20121
	call __tptcc_fn_set_zero_char
	mov r22, 0
	call __tptcc_fn_putchar
	jmp .label_204
.label_199:
	mov r6, 48
	cmp r5, r6
	jne .label_203
.ssa_bb_61:
	jmp .label_202
.label_203:
	mov r22, r5
	call __tptcc_fn_putchar
	jmp .label_204
.label_202:
	mov r23, 0
	mov r22, 9
	call __tptcc_fn_set_colour
	mov r22, 32
	call __tptcc_fn_putchar
.label_204:
	call __tptcc_fn_getchar
	mov r8, return_reg
	mov r23, r2
	mov r22, r3
	call __tptcc_fn_set_cursor
	mov r6, 70
	cmp r5, r6
	je .label_206
.ssa_bb_68:
	jmp .label_205
.label_206:
	mov r23, 15
	mov r22, 12
	call __tptcc_fn_set_colour
	mov r25, 127
	mov r24, 112
	mov r23, 28672
	mov r22, 28672
	call __tptcc_fn_set_zero_char
	mov r22, 0
	call __tptcc_fn_putchar
	jmp .label_213
.label_205:
	mov r6, 0
	cmp r5, r6
	je .label_209
.ssa_bb_71:
	jmp .label_208
.label_209:
	mov r23, 15
	mov r22, 7
	call __tptcc_fn_set_colour
	mov r25, 65409
	mov r24, 33153
	mov r23, 33153
	mov r22, 33279
	call __tptcc_fn_set_zero_char
	mov r22, 0
	call __tptcc_fn_putchar
	jmp .label_213
.label_208:
	mov r6, 66
	cmp r5, r6
	je .label_212
.ssa_bb_74:
	jmp .label_211
.label_212:
	mov r23, 15
	mov r22, 12
	call __tptcc_fn_set_colour
	mov r25, 39294
	mov r24, 32511
	mov r23, 53118
	mov r22, 20121
	call __tptcc_fn_set_zero_char
	mov r22, 0
	call __tptcc_fn_putchar
	jmp .label_213
.label_211:
	mov r7, 289
	mov r6, r5
	sub r6, 48
	add r7, r6
	ld r22, r7
	call __tptcc_fn_set_text_colour
	mov r22, r5
	call __tptcc_fn_putchar
.label_213:
	mov r5, 97
	cmp r8, r5
	je .label_217
.ssa_bb_80:
	jmp .label_214
.label_217:
	mov r5, 0
	cmp r2, r5
	jg .label_215
.ssa_bb_82:
	jmp .label_214
.label_215:
	sub r2, 1
	jmp .label_249
.label_214:
	mov r5, 100
	cmp r8, r5
	je .label_221
.ssa_bb_85:
	jmp .label_218
.label_221:
	mov r5, 11
	cmp r2, r5
	jl .label_219
.ssa_bb_87:
	jmp .label_218
.label_219:
	add r2, 1
	jmp .label_249
.label_218:
	mov r5, 119
	cmp r8, r5
	je .label_225
.ssa_bb_90:
	jmp .label_222
.label_225:
	mov r5, 0
	cmp r3, r5
	jg .label_223
.ssa_bb_92:
	jmp .label_222
.label_223:
	sub r3, 1
	jmp .label_249
.label_222:
	mov r5, 115
	cmp r8, r5
	je .label_229
.ssa_bb_95:
	jmp .label_226
.label_229:
	mov r5, 7
	cmp r3, r5
	jl .label_227
.ssa_bb_97:
	jmp .label_226
.label_227:
	add r3, 1
	jmp .label_249
.label_226:
	mov r5, 102
	cmp r8, r5
	je .label_231
.ssa_bb_100:
	jmp .label_230
.label_231:
	mov r6, 1
	mov r5, r3
	mull r5, 12
	add r6, r5
	ld r6, r6, r2
	mov r5, 70
	cmp r6, r5
	je .label_234
.ssa_bb_102:
	jmp .label_233
.label_234:
	mov r6, 1
	mov r5, r3
	mull r5, 12
	add r6, r5
	add r6, r2
	mov r5, 0
	st r5, r6
	jmp .label_236
.label_233:
	mov r6, 1
	mov r5, r3
	mull r5, 12
	add r6, r5
	ld r6, r6, r2
	mov r5, 0
	cmp r6, r5
	je .label_237
.ssa_bb_105:
	jmp .label_236
.label_237:
	mov r6, 1
	mov r5, r3
	mull r5, 12
	add r6, r5
	add r6, r2
	mov r5, 70
	st r5, r6
.label_236:
	jmp .label_249
.label_230:
	mov r5, 10
	cmp r8, r5
	je .label_240
.ssa_bb_111:
	mov r5, 114
	cmp r8, r5
	je .label_240
.ssa_bb_113:
	jmp .label_249
.label_240:
	cmp r1, 0
	je .label_243
.ssa_bb_115:
	mov r5, 97
	mov r1, r3
	mull r1, 12
	add r5, r1
	ld r5, r5, r2
	mov r1, 9
	cmp r5, r1
	jge .label_247
.ssa_bb_117:
	jmp .label_246
.label_247:
	mov r5, 97
	mov r1, r3
	mull r1, 12
	add r5, r1
	add r5, r2
	ld r1, r5
	sub r1, 9
	st r1, r5
	mov r1, 65535
	push r1
	push r2
	push r3
	call __tptcc_fn_add_to_surrounding_cells
	add stack_pointer, 3
.label_246:
	mov r1, 0
.label_243:
	push r2
	push r3
	call __tptcc_fn_sweep_cell
	add stack_pointer, 2
	ld r5, 298
	cmp r5, r4
	jge .label_250
.ssa_bb_123:
	jmp .label_249
.label_250:
	mov r23, 0
	mov r22, 0
	call __tptcc_fn_set_cursor
	mov r22, 10
	call __tptcc_fn_set_text_colour
	mov r1, 490
	mov r22, r1
	call __tptcc_fn_print_char_array
	mov return_reg, 0
	jmp .exit_main
.ssa_bb_125:
	mov r1, 0
	mov r2, 0
	mov r3, 0
	mov r4, 0
.label_249:
	jmp .label_190
.exit_main:
	pop base_pointer
	add stack_pointer, 2
	hlt
__tptcc_fn_print_char_array:
    ld r23, r22
    test r23, r23
    jz .__print_char_array_exit
    st r23, term_reg, term_base
    add r22, 1
    jmp __tptcc_fn_print_char_array
.__print_char_array_exit:
    ret
__tptcc_fn_send_raw:
    st r22, r23
    ret
__tptcc_fn_set_zero_char:
    exh r23, r0, r23
    mov r22, r23, r22
    st r22, term_print_e
    exh r25, r0, r25
    mov r24, r25, r24
    st r24, term_print_o
    ret
__tptcc_fn_getchar:
    ld return_reg, term_input
    test return_reg, return_reg
    jz __tptcc_fn_getchar
    ret
__tptcc_fn_putchar:
    st r22, term_reg, term_base
    ret
__tptcc_fn_set_colour:
    ; r22 = background, r23 = foreground
    shl r22, 4
    add r22, r23
    st r22, term_colour
    ret
__tptcc_fn_set_cursor:
    ; r22 = row, r23 = column
    shl r22, 5
    add r22, r23
    st r22, term_cursor
    ret
__tptcc_fn_set_text_colour:
    st r22, term_colour
    ret
