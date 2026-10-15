; Hand-written Blynn graph reducer for R3. Driver defines TOP and ENTRY.
; r20/21: physical semispace bases; r22: logical hp; r23: logical sp;
; r24: logical stack root; r25: evacuation call stack; r26: return link.
; Heap pointers are logical word offsets >=128. Numeric payloads are low16.
; No C compiler is involved in this runtime.
%macro get dest,index
    add r12,r20,index
    ld dest,r12
%endmacro
%macro put index,value
    add r12,r20,index
    st value,r12
%endmacro
%macro argument dest,n
    add r12,r23,n
    add r12,r20
    ld r12,r12
    add r12,r20
    add r12,1
    ld dest,r12
%endmacro
%macro number dest,n
    argument dest,n
    add r12,r20,dest
    add r12,1
    ld dest,r12
%endmacro
%macro app dest,f,x
    add r12,r20,r22
    st f,r12
    add r12,1
    st x,r12
    mov dest,r22
    add r22,2
%endmacro
%macro push value
    cmp r25,evac_stack_end
    jge out_of_memory
    st value,r25
    add r25,1
%endmacro
%macro pop dest
    sub r25,1
    ld dest,r25
%endmacro
    jmp start
start:
    ; Drain the initial keyboard latch before delayed test input arrives,
    ; matching the C startup sequence rather than after heap setup.
    ld r0,0x9F80
    mov r20,heap_a
    mov r21,heap_b
    mov r22,{ 128 GRAPH_WORDS + }
    mov r23,{ TOP 1 - }
    mov r24,r23
    mov r25,evac_stack
    mov r1,ENTRY
    mov r2,0
    app r1,r1,r2
    mov r2,28
    app r1,r1,r2
    put r23,r1
    ; Same 12x8 white terminal setup as the C compiler.
    mov r1,352
    st r1,0x9FC2
    mov r1,224
    st r1,0x9FC3
    mov r1,0x1000
    st r1,0x9FC4
    mov r1,15
    st r1,0x9FC6
    mov r1,10
    st r1,0x9FC5
reduce:
    sub r1,r23,8
    cmp r22,r1
    jbe descend
    jmp r26,gc
    sub r1,r23,8
    cmp r22,r1
    ja out_of_memory
descend:
    get r1,r23
    cmp r1,128
    jl dispatch
    get r1,r1
    sub r23,1
    put r23,r1
    jmp reduce
dispatch:
    cmp r1,32
    jg bad_combinator
    add r12,r1,dispatch_table
    ld r12,r12
    jmp r12
dispatch_table:
    dw bad_combinator,foreign,op_y,op_q,op_s,op_b,op_bk,op_c
    dw op_r,op_v,op_t,op_k,op_ki,op_i,op_left,op_cons
    dw op_num,op_add,op_sub,op_mul,op_div,op_mod,op_eq,op_le
    dw op_num,op_newref,op_readref,op_writeref,finished,op_err,op_err2,op_errout,op_errend
; lazy2: r1=f,r2=x,r10=height. lazy3: r1=x1,r2=x2,r3=x3.
lazy2:
    add r3,r23,r10
    get r3,r3
    put r3,r1
    add r3,1
    put r3,r2
    add r23,r10
    sub r23,1
    put r23,r1
    jmp reduce
lazy3:
    add r6,r23,r10
    get r6,r6
    app r5,r1,r2
    put r6,r5
    add r6,1
    put r6,r3
    add r23,r10
    sub r23,1
    put r23,r5
    sub r23,1
    put r23,r1
    jmp reduce
op_y:
    argument r1,1
    add r2,r23,1
    get r2,r2
    mov r10,1
    jmp lazy2
op_q:
    argument r1,2
    argument r2,1
    app r2,r1,r2
    argument r1,3
    mov r10,3
    jmp lazy2
op_s:
    argument r1,2
    argument r2,3
    app r3,r1,r2
    argument r1,1
    mov r10,3
    jmp lazy3
op_b:
    argument r1,2
    argument r2,3
    app r2,r1,r2
    argument r1,1
    mov r10,3
    jmp lazy2
op_bk:
    argument r1,1
    argument r2,2
    mov r10,3
    jmp lazy2
op_c:
    argument r1,1
    argument r2,3
    argument r3,2
    mov r10,3
    jmp lazy3
op_r:
    argument r1,2
    argument r2,3
    argument r3,1
    mov r10,3
    jmp lazy3
op_v:
    argument r1,3
    argument r2,1
    argument r3,2
    mov r10,3
    jmp lazy3
op_t:
    argument r1,2
    argument r2,1
    mov r10,2
    jmp lazy2
op_k:
    argument r2,1
    mov r1,13
    mov r10,2
    jmp lazy2
op_ki:
    argument r2,2
    mov r1,13
    mov r10,2
    jmp lazy2
op_i:
    argument r1,1
    add r23,1
    put r23,r1
    jmp reduce
op_left:
    argument r1,2
    argument r2,1
    mov r10,3
    jmp lazy2
op_cons:
    argument r1,4
    argument r2,1
    argument r3,2
    mov r10,4
    jmp lazy3
op_num:
    argument r1,2
    add r2,r23,1
    get r2,r2
    mov r10,2
    jmp lazy2
op_add:
    number r1,1
    number r2,2
    add r2,r1,r2
    jmp numeric_result
op_sub:
    number r1,1
    number r2,2
    sub r2,r1,r2
    jmp numeric_result
op_mul:
    number r1,1
    number r2,2
    mul r2,r1,r2
numeric_result:
    mov r1,16
    mov r10,2
    jmp lazy2
op_eq:
    number r1,1
    number r2,2
    cmp r1,r2
    je true_result
    jmp false_result
op_le:
    number r1,1
    number r2,2
    cmp r1,r2
    jle true_result
false_result:
    mov r1,11
    mov r2,13
    mov r10,2
    jmp lazy2
true_result:
    mov r1,13
    mov r2,11
    mov r10,2
    jmp lazy2
op_div:
    mov r9,0
    jmp divide
op_mod:
    mov r9,1
divide:
    number r1,1
    number r2,2
    cmp r2,0
    je division_by_zero
    mov r8,r1
    xor r7,r1,r2
    cmp r1,0
    jge positive_dividend
    sub r1,r0,r1
positive_dividend:
    cmp r2,0
    jge positive_divisor
    sub r2,r0,r2
positive_divisor:
    mov r4,0
    mov r5,0
    mov r6,16
divide_bit:
    shl r4,1
    shl r5,1
    test r1,0x8000
    jz divide_no_bit
    or r5,1
divide_no_bit:
    shl r1,1
    cmp r5,r2
    jb divide_less
    sub r5,r2
    or r4,1
divide_less:
    sub r6,1
    jnz divide_bit
    cmp r9,0
    jne remainder_result
    mov r2,r4
    test r7,0x8000
    jz numeric_result
    sub r2,r0,r2
    jmp numeric_result
remainder_result:
    mov r2,r5
    test r8,0x8000
    jz numeric_result
    sub r2,r0,r2
    jmp numeric_result
op_newref:
    argument r2,1
    mov r1,24
    app r2,r1,r2
    argument r1,3
    argument r3,2
    mov r10,3
    jmp lazy3
op_readref:
    argument r1,3
    number r2,1
    argument r3,2
    mov r10,3
    jmp lazy3
op_writeref:
    argument r1,1
    argument r2,2
    add r2,1
    put r2,r1
    argument r1,4
    mov r2,11
    argument r3,3
    mov r10,4
    jmp lazy3
op_err:
    argument r1,1
    mov r2,32
    app r1,r1,r2
    mov r2,30
    app r1,r1,r2
    add r23,1
    put r23,r1
    jmp reduce
op_err2:
    argument r1,1
    mov r2,31
    argument r3,2
    mov r10,2
    jmp lazy3
op_errout:
    number r1,1
    st r1,0x9FA5
    argument r2,2
    mov r1,29
    mov r10,2
    jmp lazy2
foreign:
    number r1,1
    cmp r1,FFI_COUNT
    jge bad_foreign
    add r12,r1,ffi_table
    ld r12,r12
    jmp r12
ffi_putchar:
    number r1,2
    st r1,0x9FA5
    ; C putchar's return register is unspecified in this backend.
    ; This runtime returns the written byte, per the C library contract.
    jmp ffi_return_one
ffi_getchar:
    ld r1,0x9F80
    test r1,r1
    jz ffi_getchar
    mov r2,r1
    mov r1,16
    app r2,r1,r2
    argument r1,3
    app r1,r1,r2
    argument r2,2
    mov r10,3
    jmp lazy2
ffi_return_one:
    mov r2,r1
    mov r1,16
    app r2,r1,r2
    argument r1,4
    app r1,r1,r2
    argument r2,3
    mov r10,4
    jmp lazy2
; Cheney collection, with the same T/K/I simplifications as party_tpt.hs.
; Swap physical base registers instead of copying both arrays word-by-word.
gc:
    push r26
    ld r1,gc_count
    add r1,1
    st r1,gc_count
    mov r22,128
    mov r17,128
    mov r23,{ TOP 1 - }
    mov r18,root_table
gc_roots:
    ld r1,r18
    cmp r1,0
    je gc_stack_root
    jmp r26,evac
    st r1,r18
    add r18,1
    jmp gc_roots
gc_stack_root:
    get r1,r24
    jmp r26,evac
    add r12,r21,r23
    st r1,r12
gc_scan:
    cmp r17,r22
    jae gc_done
    add r12,r21,r17
    ld r1,r12
    jmp r26,evac
    add r12,r21,r17
    st r1,r12
    add r17,1
    cmp r1,16
    je gc_skip_number
    add r12,r21,r17
    ld r1,r12
    jmp r26,evac
    add r12,r21,r17
    st r1,r12
gc_skip_number:
    add r17,1
    jmp gc_scan
gc_done:
    ; A live graph can fit while its next left spine cannot. Retrying GC
    ; without reaching a primitive would otherwise loop forever.
    add r19,r22,8
    add r12,r21,r23
    ld r1,r12
gc_check_spine:
    cmp r1,128
    jl gc_spine_fits
    add r19,1
    cmp r19,r23
    ja out_of_memory
    add r12,r21,r1
    ld r1,r12
    jmp gc_check_spine
gc_spine_fits:
    mov r24,r23
    mov r1,r20
    mov r20,r21
    mov r21,r1
    pop r26
    jmp r26
; evac(r1) -> r1. Uses r2-r4,r12-r13; recursion saves n and link.
evac:
    cmp r1,128
    jl evac_return
    get r2,r1
evac_t_loop:
    cmp r2,128
    jl evac_kind
    get r3,r2
    cmp r3,10
    jne evac_k
    add r3,r1,1
    get r3,r3
    add r2,1
    get r4,r2
    put r1,r3
    add r13,r1,1
    put r13,r4
    mov r2,r3
    jmp evac_t_loop
evac_k:
    cmp r3,11
    jne evac_kind
    add r2,1
    get r3,r2
    add r13,r1,1
    put r13,r3
    mov r2,13
    put r1,r2
evac_kind:
    add r13,r1,1
    get r3,r13
    cmp r2,127
    je evac_forward
    cmp r2,126
    je evac_reducing
    cmp r2,13
    je evac_i
    mov r4,r22
    add r22,2
    cmp r22,r23
    jae out_of_memory
    add r12,r21,r4
    st r2,r12
    add r12,1
    st r3,r12
    mov r2,127
    put r1,r2
    put r13,r4
    mov r1,r4
    jmp r26
evac_forward:
    mov r1,r3
evac_return:
    jmp r26
evac_reducing:
    mov r2,127
    put r1,r2
    put r13,r22
    mov r1,r22
    add r22,2
    cmp r22,r23
    jae out_of_memory
    jmp r26
evac_i:
    mov r2,126
    put r1,r2
    push r26
    push r1
    mov r1,r3
    jmp r26,evac
    mov r3,r1
    pop r1
    pop r26
    get r2,r1
    cmp r2,127
    jne evac_i_direct
    add r13,r1,1
    get r4,r13
    add r12,r21,r4
    mov r2,13
    st r2,r12
    add r12,1
    st r3,r12
    mov r1,r4
    jmp r26
evac_i_direct:
    mov r2,127
    put r1,r2
    add r13,r1,1
    put r13,r3
    mov r1,r3
    jmp r26
finished:
    mov r1,0
    jmp stop
out_of_memory:
    mov r1,1
    jmp stop
bad_combinator:
    mov r1,2
    jmp stop
bad_foreign:
    mov r1,3
    jmp stop
division_by_zero:
    mov r1,4
    jmp stop
op_errend:
    mov r1,5
stop:
    st r1,exit_status
    hlt
exit_status:
    dw 65535
gc_count:
    dw 0
