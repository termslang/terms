INIT
PUSH 0x02
PUSH 0x10



CONDITIONNOT 1001
ADD
CONDITIONEND 1001
STOP
; [17] JUMPI 1_yes
; [18] ADD

; [19] JUMPDEST 1_yes
; [20] STOP

CONDITIONNOT 1002
ADD
CONDITIONYES 1002
DIV
CONDITIONEND 1002
STOP
; [23] JUMPI 2_yes
; [24] ADD

; [27] JUMP 2_not
; [28] JUMPDEST 2_yes
; [29] DIV

; [30] JUMPDEST 2_not
; [31] STOP

CONDITIONYES 1003
DIV
CONDITIONEND 1003
STOP
;      ISZERO
; [34] JUMPI 3_not
; [36] DIV

; [37] JUMPDEST 3_not
; [38] STOP


CONDITIONYES 1004
DIV

CONDITIONYES 1005
DIV
CONDITIONNOT 1005
ADD
CONDITIONEND 1005
STOP

CONDITIONNOT 1004
ADD
CONDITIONEND 1004
STOP
; [23] JUMPI 2_yes
;    [24] ADD                --< switch body

; [27] JUMP 2_not
; [28] JUMPDEST 2_yes
;    [29] DIV                --> switch body

; [30] JUMPDEST 2_not
; [31] STOP


