            ; Write 0xe1fffcc4923d04b559f4d29a8bfc6cda04eb5b0d3c460751c2402c5c5cc9109c to record DEPOSIT EVENT
            ; write ^1 to record ^2
PUSH 0xe1fffcc4923d04b559f4d29a8bfc6cda04eb5b0d3c460751c2402c5c5cc9109c
PUSH 0x20
SSTORE
            ; Write 0x884edad9ce6fa2440d8a54cc123490eb96d2768479d49ff9c7366125a9424364 to record WITHDRAW EVENT
            ; write ^1 to record ^2
PUSH 0x884edad9ce6fa2440d8a54cc123490eb96d2768479d49ff9c7366125a9424364
PUSH 0x40
SSTORE
            ; Write 0x7ae187a0c04cf55b655ca83fa11d37854c882bf1fdcb588469b414731bb0e05a to record WITHDRAWFAILED EVENT
            ; write ^1 to record ^2
PUSH 0x7ae187a0c04cf55b655ca83fa11d37854c882bf1fdcb588469b414731bb0e05a
PUSH 0x60
SSTORE
            ; CONDITIONS
            ; conditions
INIT
            ; Payable fallback
            ; payable fallback
FALLBACK ()
            ; push REVENUE CONSTANT
            ; push ^1
CALLVALUE
            ; false 1001
            ; false ^1
CONDITIONNOT 1001
            ; stop
            ; stop
STOP
            ; end 1001
            ; end ^1
CONDITIONEND 1001
            ; Find index INDEX for address CALLER CONSTANT packed in USER #0 of bytes12 , address
            ; find index ^1 for address ^2 packed in ^3 of bytes12 , address
PUSH 0x00
PUSH 0x20
JUMPDEST TAGloop_MATCH_NUMBER9
DUP1
PUSH 0x010000
ADD
SLOAD
DUP1
DUP1
ISZERO
JUMPI TAGend_of_seq_MATCH_NUMBER9
PUSH 0x60
PUSH 0x02
EXP
DUP1
SWAP2
MUL
DIV
DUP1
CALLER
EQ
JUMPI TAGaddress_match_MATCH_NUMBER9
EQ
JUMPI TAGset_replacespot_MATCH_NUMBER9
PUSH 0x20
ADD
JUMP TAGloop_MATCH_NUMBER9
JUMPDEST TAGset_replacespot_MATCH_NUMBER9
SWAP1
POP
DUP1
PUSH 0x20
ADD
JUMP TAGloop_MATCH_NUMBER9
JUMPDEST TAGend_of_seq_MATCH_NUMBER9
POP
POP
SWAP1
DUP1
JUMPI TAGfinish_MATCH_NUMBER9
SWAP1
JUMP TAGfinish_MATCH_NUMBER9
JUMPDEST TAGaddress_match_MATCH_NUMBER9
POP
POP
JUMPDEST TAGfinish_MATCH_NUMBER9
PUSH 0x20
SWAP1
DIV
PUSH 0x80
MSTORE
            ; Read record USER #INDEX into bytes12 BALANCE , address void
            ; read record ^1 into bytes12 ^2 , address void
PUSH 0x80
MLOAD
PUSH 0x20
MUL
PUSH 0x010000
ADD
SLOAD
PUSH 0xa0
PUSH 0x02
EXP
SWAP1
DIV
PUSH 0xa0
MSTORE
            ; Increment BALANCE by REVENUE CONSTANT
            ; increment ^1 by ^2
CALLVALUE
PUSH 0xa0
MLOAD
ADD
PUSH 0xa0
MSTORE
            ; Write packed bytes12 BALANCE , address CALLER CONSTANT to record USER #INDEX
            ; write packed bytes12 ^1 , address ^2 to record ^3
PUSH 0xa0
MLOAD
PUSH 0xa0
PUSH 0x02
EXP
MUL
CALLER
OR
PUSH 0x80
MLOAD
PUSH 0x20
MUL
PUSH 0x010000
ADD
SSTORE
            ; Log DEPOSIT EVENT with topics CALLER CONSTANT , REVENUE CONSTANT
            ; log ^1 with topics ^2 , ^3
CALLVALUE
CALLER
PUSH 0x20
SLOAD
PUSH 0x20
PUSH 0x20
LOG3
            ; Return
            ; return
STOP
            ; jumpdest withdraw()
            ; jumpdest ^1
JUMPDEST withdraw()
            ; Find index INDEX of address CALLER CONSTANT packed in USER #0 of bytes12 , address
            ; find index ^1 of address ^2 packed in ^3 of bytes12 , address
PUSH 0x20
DUP1
DUP1
JUMPDEST TAGloop_MATCH_NUMBER16
PUSH 0x010000
ADD
SLOAD
DUP1
ISZERO
JUMPI TAGend0_MATCH_NUMBER16
PUSH 0x60
PUSH 0x02
EXP
DUP1
SWAP2
MUL
DIV
CALLER
EQ
JUMPI TAGend_MATCH_NUMBER16
PUSH 0x20
ADD
DUP1
JUMP TAGloop_MATCH_NUMBER16
JUMPDEST TAGend0_MATCH_NUMBER16
POP
POP
POP
PUSH 0x00
JUMPDEST TAGend_MATCH_NUMBER16
DIV
PUSH 0x80
MSTORE
            ; Read record USER #INDEX into bytes12 BALANCE , address void
            ; read record ^1 into bytes12 ^2 , address void
PUSH 0x80
MLOAD
PUSH 0x20
MUL
PUSH 0x010000
ADD
SLOAD
PUSH 0xa0
PUSH 0x02
EXP
SWAP1
DIV
PUSH 0xa0
MSTORE
            ; push BALANCE
            ; push ^1
PUSH 0xa0
MLOAD
            ; false 1002
            ; false ^1
CONDITIONNOT 1002
            ; stop
            ; stop
STOP
            ; end 1002
            ; end ^1
CONDITIONEND 1002
            ; Write packed bytes12 0 , address CALLER CONSTANT to record USER #INDEX
            ; write packed bytes12 ^1 , address ^2 to record ^3
PUSH 0x00
PUSH 0xa0
PUSH 0x02
EXP
MUL
CALLER
OR
PUSH 0x80
MLOAD
PUSH 0x20
MUL
PUSH 0x010000
ADD
SSTORE
            ; Send BALANCE to CALLER CONSTANT or see 2_2 on failure
            ; send ^1 to ^2 or see ^3 on failure
PUSH 0xa0
MLOAD
ISZERO
JUMPI TAG2_2
PUSH 0x00
PUSH 0x00
PUSH 0x00
PUSH 0x00
PUSH 0xa0
MLOAD
CALLER
PUSH 0x64
GAS
SUB
CALL
ISZERO
JUMPI TAG2_2
            ; Log WITHDRAW EVENT with topics CALLER CONSTANT , BALANCE
            ; log ^1 with topics ^2 , ^3
PUSH 0xa0
MLOAD
CALLER
PUSH 0x40
SLOAD
PUSH 0x20
PUSH 0x20
LOG3
            ; Stop
            ; stop
STOP
            ; jumpdest 2_2
            ; jumpdest ^1
JUMPDEST TAG2_2
            ; Write packed bytes12 BALANCE , address CALLER CONSTANT to record USER #INDEX
            ; write packed bytes12 ^1 , address ^2 to record ^3
PUSH 0xa0
MLOAD
PUSH 0xa0
PUSH 0x02
EXP
MUL
CALLER
OR
PUSH 0x80
MLOAD
PUSH 0x20
MUL
PUSH 0x010000
ADD
SSTORE
            ; Log WITHDRAWFAILED EVENT with topic CALLER CONSTANT
            ; log ^1 with topic ^2
CALLER
PUSH 0x60
SLOAD
PUSH 0x20
DUP1
LOG2
            ; Return
            ; return
STOP
            ; jumpdest balance()
            ; jumpdest ^1
JUMPDEST balance()
            ; Find index INDEX of address CALLER CONSTANT packed in USER #0 of bytes12 , address
            ; find index ^1 of address ^2 packed in ^3 of bytes12 , address
PUSH 0x20
DUP1
DUP1
JUMPDEST TAGloop_MATCH_NUMBER31
PUSH 0x010000
ADD
SLOAD
DUP1
ISZERO
JUMPI TAGend0_MATCH_NUMBER31
PUSH 0x60
PUSH 0x02
EXP
DUP1
SWAP2
MUL
DIV
CALLER
EQ
JUMPI TAGend_MATCH_NUMBER31
PUSH 0x20
ADD
DUP1
JUMP TAGloop_MATCH_NUMBER31
JUMPDEST TAGend0_MATCH_NUMBER31
POP
POP
POP
PUSH 0x00
JUMPDEST TAGend_MATCH_NUMBER31
DIV
PUSH 0x80
MSTORE
            ; Read record USER #INDEX into bytes12 BALANCE , address void
            ; read record ^1 into bytes12 ^2 , address void
PUSH 0x80
MLOAD
PUSH 0x20
MUL
PUSH 0x010000
ADD
SLOAD
PUSH 0xa0
PUSH 0x02
EXP
SWAP1
DIV
PUSH 0xa0
MSTORE
            ; Return uint256 at BALANCE
            ; return uint256 at ^1
PUSH 0x20
PUSH 0xa0
RETURN
            ; jumpdest users(uint256 index)
            ; jumpdest ^1
JUMPDEST users(uint256)
            ; Let INDEX get argument 1
            ; let ^1 get argument 1
PUSH 0x04
CALLDATALOAD
PUSH 0x80
MSTORE
            ; Read record USER #INDEX into bytes12 BALANCE , address USER
            ; read record ^1 into bytes12 ^2 , address ^3
PUSH 0x80
MLOAD
PUSH 0x20
MUL
PUSH 0x010000
ADD
SLOAD
DUP1
PUSH 0xa0
PUSH 0x02
EXP
SWAP1
DIV
PUSH 0xa0
MSTORE
PUSH 0x60
PUSH 0x02
EXP
DUP1
SWAP2
MUL
DIV
PUSH 0xc0
MSTORE
            ; Return address at USER
            ; return address at ^1
PUSH 0x14
PUSH 0xc0
PUSH 0x0C
ADD  ; add
RETURN;return
            ; jumpdest funds(uint256 index)
            ; jumpdest ^1


JUMPDEST funds(uint256) ;funds
            ; Let INDEX get argument 1
            ; let ^1 get argument 1
PUSH 0x04
CALLDATALOAD
PUSH 0x80
MSTORE
            ; Read record USER #INDEX into bytes12 BALANCE , address void
            ; read record ^1 into bytes12 ^2 , address void
PUSH 0x80
MLOAD; mload
PUSH 0x20
MUL
PUSH 0x010000
ADD
SLOAD
PUSH 0xa0
PUSH 0x02
EXP
SWAP1
DIV
PUSH 0xa0
MSTORE
            ; Return uint256 at BALANCE
            ; return uint256 at ^1
PUSH 0x20
PUSH 0xa0
RETURN
            ; jumpdest 0
            ; jumpdest ^1
JUMPDEST TAG0
            ; return
            ; return
STOP;
