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
    dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 119, 121, 114, 124, 113, 116, 118, 117, 120, 0, 75, 97, 98, 111, 111, 109, 33, 32, 89, 111, 117, 32, 108, 111, 115, 101, 33, 0, 35, 32, 111, 102, 32, 109, 105, 110, 101, 115, 10, 40, 54, 45, 49, 50, 41, 58, 32, 0, 10, 69, 110, 116, 101, 114, 32, 97, 10, 110, 117, 109, 98, 101, 114, 32, 116, 111, 10, 104, 101, 108, 112, 10, 114, 97, 110, 100, 111, 109, 105, 122, 101, 10, 40, 49, 45, 49, 48, 48, 41, 58, 32, 0, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 0, 89, 111, 117, 32, 119, 105, 110, 33, 0
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
__tptcc_fn_queue_cell:
	push base_pointer
	mov base_pointer, stack_pointer
	push r2
	push r1
	mov r2, 1
	ld r1, base_pointer, 3
	mull r1, 12
	add r2, r1
	ld r1, base_pointer, 4
	ld r2, r2, r1
	mov r1, 0
	cmp r2, r1
	je .label_11
.ssa_bb_1:
	mov r2, 1
	ld r1, base_pointer, 3
	mull r1, 12
	add r2, r1
	ld r1, base_pointer, 4
	ld r2, r2, r1
	mov r1, 70
	cmp r2, r1
	je .label_11
.ssa_bb_3:
	jmp .label_10
.label_11:
	mov r2, 1
	ld r1, base_pointer, 3
	mull r1, 12
	add r2, r1
	ld r1, base_pointer, 4
	add r2, r1
	mov r1, 16
	st r1, r2
	ld r1, base_pointer, 2
	ld r2, base_pointer, 3
	st r2, r1
	ld r1, base_pointer, 2
	add r1, 1
	ld r2, base_pointer, 4
	st r2, r1
	ld r1, base_pointer, 2
	add r1, 2
	st r1, base_pointer, 2
.label_10:
	ld r1, base_pointer, 2
	mov return_reg, r1
.exit_queue_cell:
	pop r1
	pop r2
	pop base_pointer
	ret
__tptcc_fn_show_bombs:
	push r2
	push r1
	push r3
	push r4
	mov r2, 0
.label_14:
	mov r1, 8
	cmp r2, r1
	jl .label_17
.ssa_bb_2:
	jmp .exit_show_bombs
.label_17:
	mov r1, 0
.label_18:
	mov r3, 12
	cmp r1, r3
	jl .label_21
.ssa_bb_5:
	jmp .label_20
.label_21:
	mov r4, 97
	mov r3, r2
	mull r3, 12
	add r4, r3
	ld r4, r4, r1
	mov r3, 9
	cmp r4, r3
	jge .label_23
.ssa_bb_7:
	jmp .label_22
.label_23:
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
.label_22:
	add r1, 1
	jmp .label_18
.label_20:
	add r2, 1
	jmp .label_14
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
	push r8
	push r5
	push r6
	push r7
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
	jge .label_26
.ssa_bb_1:
	jmp .label_29
.label_26:
	call __tptcc_fn_show_bombs
	mov r23, 0
	mov r22, 0
	call __tptcc_fn_set_cursor
	mov r22, 12
	call __tptcc_fn_set_text_colour
	mov r2, 203
	mov r22, r2
	call __tptcc_fn_print_char_array
	mov r22, 9
	call __tptcc_fn_set_text_colour
	mov r23, r4
	mov r22, r1
	call __tptcc_fn_set_cursor
	mov r22, 0
	call __tptcc_fn_putchar
.label_28:
	mov r2, 1
	cmp r2, 0
	je .label_29
.ssa_bb_4:
	jmp .label_28
.label_29:
	add r2, base_pointer, 1
	add r3, r2, 0
	st r1, r3
	add r1, r2, 1
	st r4, r1
	add r2, 2
	mov r1, 0
.label_31:
	add r3, base_pointer, 1
	cmp r2, r3
	jg .label_33
.ssa_bb_10:
	jmp .label_32
.label_33:
	ld r3, r2, 65534
	add r4, r2, 65535
	ld r8, r4
	sub r2, 2
	mov r5, 97
	mov r4, r3
	mull r4, 12
	add r5, r4
	add r5, r8
	ld r6, r5
	mov r7, r6
	add r7, 48
	mov r5, 1
	mov r4, r3
	mull r4, 12
	add r5, r4
	add r5, r8
	st r7, r5
	mov r23, r8
	mov r22, r3
	call __tptcc_fn_set_cursor
	mov r4, 193
	add r4, r6
	ld r22, r4
	call __tptcc_fn_set_text_colour
	mov r22, r7
	call __tptcc_fn_putchar
	add r1, 1
	mov r4, 0
	cmp r6, r4
	je .label_35
.ssa_bb_12:
	jmp .label_58
.label_35:
	mov r7, r3
	sub r7, 1
	mov r6, r8
	sub r6, 1
	mov r5, r3
	add r5, 1
	mov r4, r8
	add r4, 1
	mov r9, 0
	cmp r3, r9
	jg .label_38
.ssa_bb_14:
	jmp .label_43
.label_38:
	mov r9, 0
	cmp r8, r9
	jg .label_41
.ssa_bb_16:
	jmp .label_40
.label_41:
	push r6
	push r7
	push r2
	call __tptcc_fn_queue_cell
	add stack_pointer, 3
	mov r2, return_reg
.label_40:
	push r8
	push r7
	push r2
	call __tptcc_fn_queue_cell
	add stack_pointer, 3
	mov r2, return_reg
	mov r9, 11
	cmp r8, r9
	jl .label_44
.ssa_bb_20:
	jmp .label_43
.label_44:
	push r4
	push r7
	push r2
	call __tptcc_fn_queue_cell
	add stack_pointer, 3
	mov r2, return_reg
.label_43:
	mov r7, 0
	cmp r8, r7
	jg .label_47
.ssa_bb_26:
	jmp .label_46
.label_47:
	push r6
	push r3
	push r2
	call __tptcc_fn_queue_cell
	add stack_pointer, 3
	mov r2, return_reg
.label_46:
	mov r7, 11
	cmp r8, r7
	jl .label_50
.ssa_bb_30:
	jmp .label_49
.label_50:
	push r4
	push r3
	push r2
	call __tptcc_fn_queue_cell
	add stack_pointer, 3
	mov r2, return_reg
.label_49:
	mov r7, 7
	cmp r3, r7
	jl .label_53
.ssa_bb_34:
	jmp .label_58
.label_53:
	mov r3, 0
	cmp r8, r3
	jg .label_56
.ssa_bb_36:
	jmp .label_55
.label_56:
	push r6
	push r5
	push r2
	call __tptcc_fn_queue_cell
	add stack_pointer, 3
	mov r2, return_reg
.label_55:
	push r8
	push r5
	push r2
	call __tptcc_fn_queue_cell
	add stack_pointer, 3
	mov r2, return_reg
	mov r3, 11
	cmp r8, r3
	jl .label_59
.ssa_bb_40:
	jmp .label_58
.label_59:
	push r4
	push r5
	push r2
	call __tptcc_fn_queue_cell
	add stack_pointer, 3
	mov r2, return_reg
.label_58:
	jmp .label_31
.label_32:
	ld r2, 202
	add r2, r1
	st r2, 202
.exit_sweep_cell:
	pop r9
	pop r7
	pop r6
	pop r5
	pop r8
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
	jg .label_62
.ssa_bb_1:
	jmp .label_67
.label_62:
	mov r1, 0
	cmp r6, r1
	jg .label_65
.ssa_bb_3:
	jmp .label_64
.label_65:
	mov r3, 97
	mov r1, r8
	mull r1, 12
	add r3, r1
	add r3, r7
	ld r1, r3
	ld r9, base_pointer, 4
	add r1, r9
	st r1, r3
.label_64:
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
	jl .label_68
.ssa_bb_7:
	jmp .label_67
.label_68:
	mov r3, 97
	mull r8, 12
	add r3, r8
	add r3, r4
	ld r1, r3
	ld r8, base_pointer, 4
	add r1, r8
	st r1, r3
.label_67:
	mov r1, 0
	cmp r6, r1
	jg .label_71
.ssa_bb_13:
	jmp .label_70
.label_71:
	mov r3, 97
	mov r1, r2
	mull r1, 12
	add r3, r1
	add r3, r7
	ld r1, r3
	ld r8, base_pointer, 4
	add r1, r8
	st r1, r3
.label_70:
	mov r1, 11
	cmp r6, r1
	jl .label_74
.ssa_bb_17:
	jmp .label_73
.label_74:
	mov r3, 97
	mov r1, r2
	mull r1, 12
	add r3, r1
	add r3, r4
	ld r1, r3
	ld r8, base_pointer, 4
	add r1, r8
	st r1, r3
.label_73:
	mov r1, 7
	cmp r2, r1
	jl .label_77
.ssa_bb_21:
	jmp .exit_add_to_surrounding_cells
.label_77:
	mov r1, 0
	cmp r6, r1
	jg .label_80
.ssa_bb_23:
	jmp .label_79
.label_80:
	mov r2, 97
	mov r1, r5
	mull r1, 12
	add r2, r1
	add r2, r7
	ld r1, r2
	ld r3, base_pointer, 4
	add r1, r3
	st r1, r2
.label_79:
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
	jl .label_83
.ssa_bb_27:
	jmp .exit_add_to_surrounding_cells
.label_83:
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
	mov r1, 221
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
	mov r1, 241
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
	mov r1, 285
	mov r22, r1
	call __tptcc_fn_print_char_array
	mov r23, 0
	mov r22, 7
	call __tptcc_fn_set_cursor
	mov r23, 10
	mov r22, 10
	call __tptcc_fn_set_colour
	mov r1, 0
.label_85:
	cmp r1, r6
	jl .label_88
.ssa_bb_2:
	jmp .label_87
.label_88:
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
	mov r8, r2
	and r8, 127
.label_89:
	mov r3, 96
	cmp r8, r3
	jge .label_91
.ssa_bb_5:
	mov r3, r7
	add r3, r8
	ld r5, r3
	mov r3, 9
	cmp r5, r3
	jge .label_91
.ssa_bb_7:
	jmp .label_90
.label_91:
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
	mov r8, r2
	and r8, 127
	jmp .label_89
.label_90:
	mov r5, r7
	add r5, r8
	ld r3, r5
	add r3, 9
	st r3, r5
	mov r3, 12
	mulh r5, r8, 5461
	mul r9, r5, 12
	sub r9, r8, r9
	cmp r9, r3
	jb .ssa_phi__label_90__label_93
	jmp .ssa_bb_10
.ssa_phi__label_90__label_93:
	jmp .label_93
.ssa_bb_10:
	add r5, 1
.label_93:
	mov r9, 12
	mulh r3, r8, 5461
	mul r3, r3, 12
	sub r3, r8, r3
	cmp r3, r9
	jb .ssa_phi__label_93__label_94
	jmp .ssa_bb_12
.ssa_phi__label_93__label_94:
	jmp .label_94
.ssa_bb_12:
	sub r3, 12
.label_94:
	mov r8, 1
	push r8
	push r3
	push r5
	call __tptcc_fn_add_to_surrounding_cells
	add stack_pointer, 3
	mov r22, 46
	call __tptcc_fn_putchar
.label_86:
	add r1, 1
	jmp .label_85
.label_87:
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
.label_95:
	mov r2, 8
	cmp r1, r2
	jl .label_98
.ssa_bb_17:
	jmp .label_97
.label_98:
	mov r23, 40836
	mov r22, 0
	call __tptcc_fn_send_raw
.label_96:
	add r1, 1
	jmp .label_95
.label_97:
	mov r23, 0
	mov r22, 0
	call __tptcc_fn_set_cursor
	mov r1, 1
	mov r2, 0
	mov r3, 0
.label_99:
	mov r5, 1
	cmp r5, 0
	je .exit_main
.ssa_bb_22:
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
	je .label_103
.ssa_bb_24:
	jmp .label_102
.label_103:
	mov r25, 127
	mov r24, 112
	mov r23, 28672
	mov r22, 28672
	call __tptcc_fn_set_zero_char
	mov r22, 0
	call __tptcc_fn_putchar
	jmp .label_113
.label_102:
	mov r6, 0
	cmp r5, r6
	je .label_106
.ssa_bb_27:
	jmp .label_105
.label_106:
	mov r25, 65409
	mov r24, 33153
	mov r23, 33153
	mov r22, 33279
	call __tptcc_fn_set_zero_char
	mov r22, 0
	call __tptcc_fn_putchar
	jmp .label_113
.label_105:
	mov r6, 66
	cmp r5, r6
	je .label_109
.ssa_bb_30:
	jmp .label_108
.label_109:
	mov r25, 39294
	mov r24, 32511
	mov r23, 53118
	mov r22, 20121
	call __tptcc_fn_set_zero_char
	mov r22, 0
	call __tptcc_fn_putchar
	jmp .label_113
.label_108:
	mov r6, 48
	cmp r5, r6
	jne .label_112
.ssa_bb_33:
	jmp .label_111
.label_112:
	mov r22, r5
	call __tptcc_fn_putchar
	jmp .label_113
.label_111:
	mov r23, 0
	mov r22, 9
	call __tptcc_fn_set_colour
	mov r22, 32
	call __tptcc_fn_putchar
.label_113:
	call __tptcc_fn_getchar
	mov r8, return_reg
	mov r23, r2
	mov r22, r3
	call __tptcc_fn_set_cursor
	mov r6, 70
	cmp r5, r6
	je .label_115
.ssa_bb_40:
	jmp .label_114
.label_115:
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
	jmp .label_122
.label_114:
	mov r6, 0
	cmp r5, r6
	je .label_118
.ssa_bb_43:
	jmp .label_117
.label_118:
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
	jmp .label_122
.label_117:
	mov r6, 66
	cmp r5, r6
	je .label_121
.ssa_bb_46:
	jmp .label_120
.label_121:
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
	jmp .label_122
.label_120:
	mov r7, 193
	mov r6, r5
	sub r6, 48
	add r7, r6
	ld r22, r7
	call __tptcc_fn_set_text_colour
	mov r22, r5
	call __tptcc_fn_putchar
.label_122:
	mov r5, 97
	cmp r8, r5
	je .label_126
.ssa_bb_52:
	jmp .label_123
.label_126:
	mov r5, 0
	cmp r2, r5
	jg .label_124
.ssa_bb_54:
	jmp .label_123
.label_124:
	sub r2, 1
	jmp .label_158
.label_123:
	mov r5, 100
	cmp r8, r5
	je .label_130
.ssa_bb_57:
	jmp .label_127
.label_130:
	mov r5, 11
	cmp r2, r5
	jl .label_128
.ssa_bb_59:
	jmp .label_127
.label_128:
	add r2, 1
	jmp .label_158
.label_127:
	mov r5, 119
	cmp r8, r5
	je .label_134
.ssa_bb_62:
	jmp .label_131
.label_134:
	mov r5, 0
	cmp r3, r5
	jg .label_132
.ssa_bb_64:
	jmp .label_131
.label_132:
	sub r3, 1
	jmp .label_158
.label_131:
	mov r5, 115
	cmp r8, r5
	je .label_138
.ssa_bb_67:
	jmp .label_135
.label_138:
	mov r5, 7
	cmp r3, r5
	jl .label_136
.ssa_bb_69:
	jmp .label_135
.label_136:
	add r3, 1
	jmp .label_158
.label_135:
	mov r5, 102
	cmp r8, r5
	je .label_140
.ssa_bb_72:
	jmp .label_139
.label_140:
	mov r6, 1
	mov r5, r3
	mull r5, 12
	add r6, r5
	ld r6, r6, r2
	mov r5, 70
	cmp r6, r5
	je .label_143
.ssa_bb_74:
	jmp .label_142
.label_143:
	mov r6, 1
	mov r5, r3
	mull r5, 12
	add r6, r5
	add r6, r2
	mov r5, 0
	st r5, r6
	jmp .label_145
.label_142:
	mov r6, 1
	mov r5, r3
	mull r5, 12
	add r6, r5
	ld r6, r6, r2
	mov r5, 0
	cmp r6, r5
	je .label_146
.ssa_bb_77:
	jmp .label_145
.label_146:
	mov r6, 1
	mov r5, r3
	mull r5, 12
	add r6, r5
	add r6, r2
	mov r5, 70
	st r5, r6
.label_145:
	jmp .label_158
.label_139:
	mov r5, 10
	cmp r8, r5
	je .label_149
.ssa_bb_83:
	mov r5, 114
	cmp r8, r5
	je .label_149
.ssa_bb_85:
	jmp .label_158
.label_149:
	cmp r1, 0
	je .label_152
.ssa_bb_87:
	mov r5, 97
	mov r1, r3
	mull r1, 12
	add r5, r1
	ld r5, r5, r2
	mov r1, 9
	cmp r5, r1
	jge .label_156
.ssa_bb_89:
	jmp .label_155
.label_156:
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
.label_155:
	mov r1, 0
.label_152:
	push r2
	push r3
	call __tptcc_fn_sweep_cell
	add stack_pointer, 2
	ld r5, 202
	cmp r5, r4
	jge .label_159
.ssa_bb_95:
	jmp .label_158
.label_159:
	mov r23, 0
	mov r22, 0
	call __tptcc_fn_set_cursor
	mov r22, 10
	call __tptcc_fn_set_text_colour
	mov r1, 298
	mov r22, r1
	call __tptcc_fn_print_char_array
	mov return_reg, 0
	jmp .exit_main
.ssa_bb_97:
	mov r1, 0
	mov r2, 0
	mov r3, 0
	mov r4, 0
.label_158:
	jmp .label_99
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
