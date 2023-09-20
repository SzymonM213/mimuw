global inverse_permutation

section .text
; inverse_permutation arguments:
; rdi - size of permutation (n)
; rsi - pointer to permutation (p[])
; If p[] is a correct permutation of [0, n-1] then inverse_permutation inverses it and returns true.
; Otherwise it returns false and p[] values remain unchanged.
inverse_permutation:
        CHANGE_SIGN_BIT equ 0x80000000                    ; Xor it with a value to change its sign bit.
        CLEAR_SIGN_BIT equ 0x7fffffff                     ; And it with a value to clear its sign bit.
        dec     rdi                                       ; If n was incorrect (n = 0 or n > MAX_INT + 1) then rdi is now greater than MAX_INT.
        cmp     rdi, CLEAR_SIGN_BIT                       ; CLEAR_SIGN_BIT = MAX_INT.
        ja      .false
        inc     edi                                       ; n is correct, so we can restore its value.
        mov     ecx, edi                                  ; Counter in check_data_loop (we iterate from the end of p[]).
        dec     ecx
.check_data_loop:                                         ; We check if p[i] is in range [0, n - 1] and if p[i] is unique.
        mov     eax, [rsi + 4 * rcx]
        and     eax, CLEAR_SIGN_BIT                       ; If we want to go to p[p[rcx]] we have to clear the sign bit.
        cmp     edi, eax                                  ; If p[rcx] > n - 1 we know that the original value of p[rcx] was negative or p[rcx] > n - 1.
        jbe     .false_and_repair                         ; So we know that p[] is not a permutation, but we have to repair p[].
        xor     dword [rsi + 4 * rax], CHANGE_SIGN_BIT    ; We change the sign bit of p[p[rcx]] (if p[] is a proper permutation we do it exactly once for all elements).
        cmp     dword [rsi + 4 * rax], 0                  ; If p[rax] is positive then we know that we changed its sign for a second time.
        jge     .false_and_repair
        dec     ecx
        jns     .check_data_loop
.verified_data:                                           ; We know that p[] is a permutation, so we can start inversing it (sign bit of all elements equals 1).
        xor     ecx, ecx                                  ; Beginning of the currently inversed cycle.
        xor     eax, eax                                  ; Index in which we were previously.
        mov     edx, [rsi]                                ; Value of current index.
.inverse_loop:
        xor     edx, CHANGE_SIGN_BIT                      ; We change the sign bit of p[rcx] to 0 to mark it as visited.
        xchg    eax, [rsi + 4 * rdx]                      ; p[i] becomes j for j such that p[j] = i and eax becomes next cycle element index.
        xchg    edx, eax                                  ; edx becomes next cycle element index, eax becomes current cycle element index.
        test    edx, edx                                  ; If edx is >= 0 we know that we have visited this element before so we inversed the whole cycle.
        jns     .endofcycle
        jmp     .inverse_loop
.endofcycle:
        inc     ecx                                       ; We search for the next cycle (first element that was not visited).
        cmp     ecx, edi                                  ; If we have visited all elements we know that we have inversed all cycles.
        jge     .true
        mov     edx, [rsi + 4 * rcx]                      ; We check if p[rcx] was visited before.
        test    edx, edx
        jns     .endofcycle
        jmp     .inverse_loop                             ; If it was not visited it is the first element of a new cycle.
.true:
        xor     eax, eax
        inc     eax
        ret
.false_and_repair:                                        ; We know that p[] is not a permutation, but before returning false we have to restore p[] to its original state.
        mov     edx, edi                                  ; We have to repair p[] from the end to the index which value was incorrect (because it was the order we iterated checking p[]).
.repair:
        mov     eax, [rsi + 4 * (rdx - 1)]
        and     eax, CLEAR_SIGN_BIT
        cmp     edi, eax                                  ; If p[rdx - 1] > n - 1 we know that the original value of p[rdx - 1] was inappropriate.
        jle     .false                                    ; So it is the last visited index in check_data_loop.
        xor     dword [rsi + 4 * rax], CHANGE_SIGN_BIT    ; We restore the sign bit of p[p[rdx - 1]].
        dec     edx
        cmp     edx, ecx
        jne     .repair
.false:
        xor     eax, eax
        ret