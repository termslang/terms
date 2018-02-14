            ; Write 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef to record TRANSFER EVENT
            ; write ^1 to record ^2
PUSH 0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef
PUSH 0x20
SSTORE
            ; Write 0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925 to record APPROVAL EVENT
            ; write ^1 to record ^2
PUSH 0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925
PUSH 0x40
SSTORE
            ; Write 1000 to record SUPPLY
            ; write ^1 to record ^2
PUSH 0x03e8
PUSH 0x60
SSTORE
            ; Write 1000 to record 1 of ORIGIN CONSTANT
            ; write ^1 to record ^2 of ^3
PUSH 0x03e8
ORIGIN
PUSH 0x01
PUSH 0x20
MUL
ADD
SSTORE
            ; Write ORIGIN CONSTANT to record OWNER
            ; write ^1 to record ^2
ORIGIN
PUSH 0x80
SSTORE
            ; Write string 0x5465726d7320746f6b656e to record NAME #0
            ; write string ^1 to record ^2
PUSH 0x010000
SSTORESEQ 0x5465726d7320746f6b656e
            ; Write string 0x5454 to record SYMBOL #0
            ; write string ^1 to record ^2
PUSH 0x020000
SSTORESEQ 0x5454
            ; Write 18 to record DECIMALS
            ; write ^1 to record ^2
PUSH 0x12
PUSH 0xa0
SSTORE
            ; CONDITIONS
            ; conditions
INIT
            ; Payable fallback
            ; payable fallback
FALLBACK ()
            ; Return
            ; return
STOP
            ; jumpdest transfer(address to, uint256 value)
            ; jumpdest ^1
JUMPDEST transfer(address,uint256)
            ; Let TO get argument 1
            ; let ^1 get argument 1
PUSH 0x04
CALLDATALOAD
PUSH 0xc0
MSTORE
            ; Let VALUE get argument 2
            ; let ^1 get argument 2
PUSH 0x24
CALLDATALOAD
PUSH 0xe0
MSTORE
            ; Let FROM = CALLER CONSTANT
            ; let ^1 = ^2
CALLER
PUSH 0x0100
MSTORE
            ; Apply procedure "transfer"
            ; apply procedure ^1
REFJUMP TAG_TRANSFER_PROCEDURE
            ; Return
            ; return
STOP
            ; jumpdest transferFrom(address from, address to, uint256 value)
            ; jumpdest ^1
JUMPDEST transferFrom(address,address,uint256)
            ; Let FROM get argument 1
            ; let ^1 get argument 1
PUSH 0x04
CALLDATALOAD
PUSH 0x0100
MSTORE
            ; Let TO get argument 2
            ; let ^1 get argument 2
PUSH 0x24
CALLDATALOAD
PUSH 0xc0
MSTORE
            ; Let VALUE get argument 3
            ; let ^1 get argument 3
PUSH 0x44
CALLDATALOAD
PUSH 0xe0
MSTORE
            ; Let TMP = FROM - CALLER CONSTANT
            ; let ^1 = ^2 - ^3
CALLER
PUSH 0x0100
MLOAD
SUB
PUSH 0x0120
MSTORE
            ; Let REMAINING read record TMP
            ; let ^1 read record ^2
PUSH 0x0120
MLOAD
SLOAD
PUSH 0x0140
MSTORE
            ; push REMAINING
            ; push ^1
PUSH 0x0140
MLOAD
            ; push VALUE
            ; push ^1
PUSH 0xe0
MLOAD
            ; comp <=
            ; comp <=
GT
ISZERO
            ; false 1001
            ; false ^1
CONDITIONNOT 1001
            ; stop
            ; stop
STOP
            ; end 1001
            ; end ^1
CONDITIONEND 1001
            ; Decrement REMAINING by VALUE
            ; decrement ^1 by ^2
PUSH 0xe0
MLOAD
PUSH 0x0140
MLOAD
SUB
PUSH 0x0140
MSTORE
            ; Write REMAINING to record at TMP
            ; write ^1 to record at ^2
PUSH 0x0140
MLOAD
PUSH 0x0120
MLOAD
SSTORE
            ; Apply procedure "transfer"
            ; apply procedure ^1
REFJUMP TAG_TRANSFER_PROCEDURE
            ; Return
            ; return
STOP
            ; jumpdest approve(address spender, uint256 value)
            ; jumpdest ^1
JUMPDEST approve(address,uint256)
            ; Let SPENDER get argument 1
            ; let ^1 get argument 1
PUSH 0x04
CALLDATALOAD
PUSH 0x0160
MSTORE
            ; Let VALUE get argument 2
            ; let ^1 get argument 2
PUSH 0x24
CALLDATALOAD
PUSH 0xe0
MSTORE
            ; Let TMP = CALLER CONSTANT - SPENDER
            ; let ^1 = ^2 - ^3
PUSH 0x0160
MLOAD
CALLER
SUB
PUSH 0x0120
MSTORE
            ; Write VALUE to record at TMP
            ; write ^1 to record at ^2
PUSH 0xe0
MLOAD
PUSH 0x0120
MLOAD
SSTORE
            ; Log APPROVAL EVENT with topics CALLER CONSTANT , SPENDER , VALUE
            ; log ^1 with topics ^2 , ^3 , ^4
PUSH 0xe0
MLOAD
PUSH 0x0160
MLOAD
CALLER
PUSH 0x40
SLOAD
PUSH1 0x20
DUP1
LOG4
            ; Return
            ; return
STOP
            ; jumpdest mint(uint256 amount, address to)
            ; jumpdest ^1
JUMPDEST mint(uint256,address)
            ; Let AMOUNT get argument 1
            ; let ^1 get argument 1
PUSH 0x04
CALLDATALOAD
PUSH 0x0180
MSTORE
            ; Let TO get argument 2
            ; let ^1 get argument 2
PUSH 0x24
CALLDATALOAD
PUSH 0xc0
MSTORE
            ; push TO
            ; push ^1
PUSH 0xc0
MLOAD
            ; false 1002
            ; false ^1
CONDITIONNOT 1002
            ; let TO = CALLER CONSTANT
            ; let ^1 = ^2
CALLER
PUSH 0xc0
MSTORE
            ; end 1002
            ; end ^1
CONDITIONEND 1002
            ; Let BALANCE read record 1 of TO
            ; let ^1 read record ^2 of ^3
PUSH 0xc0
MLOAD
PUSH 0x01
PUSH 0x20
MUL
ADD
SLOAD
PUSH 0x01a0
MSTORE
            ; Grab record SUPPLY
            ; grab record ^1
PUSH 0x60
DUP1
SLOAD
SWAP1
MSTORE
            ; Increment BALANCE by AMOUNT
            ; increment ^1 by ^2
PUSH 0x0180
MLOAD
PUSH 0x01a0
MLOAD
ADD
PUSH 0x01a0
MSTORE
            ; Increment SUPPLY by AMOUNT
            ; increment ^1 by ^2
PUSH 0x0180
MLOAD
PUSH 0x60
MLOAD
ADD
PUSH 0x60
MSTORE
            ; Write BALANCE to record 1 of TO
            ; write ^1 to record ^2 of ^3
PUSH 0x01a0
MLOAD
PUSH 0xc0
MLOAD
PUSH 0x01
PUSH 0x20
MUL
ADD
SSTORE
            ; Write record SUPPLY
            ; write record ^1
PUSH 0x60
MLOAD
PUSH 0x60
SSTORE
            ; Return
            ; return
STOP
            ; jumpdest balanceOf(address owner)
            ; jumpdest ^1
JUMPDEST balanceOf(address)
            ; Let OWNER get argument 1
            ; let ^1 get argument 1
PUSH 0x04
CALLDATALOAD
PUSH 0x80
MSTORE
            ; Let BALANCE read record 1 of OWNER
            ; let ^1 read record ^2 of ^3
PUSH 0x80
MLOAD
PUSH 0x01
PUSH 0x20
MUL
ADD
SLOAD
PUSH 0x01a0
MSTORE
            ; Return uint256 at BALANCE
            ; return uint256 at ^1
PUSH 0x20
PUSH 0x01a0
RETURN
            ; jumpdest allowance(address from, address to)
            ; jumpdest ^1
JUMPDEST allowance(address,address)
            ; Let FROM get argument 1
            ; let ^1 get argument 1
PUSH 0x04
CALLDATALOAD
PUSH 0x0100
MSTORE
            ; Let TO get argument 2
            ; let ^1 get argument 2
PUSH 0x24
CALLDATALOAD
PUSH 0xc0
MSTORE
            ; Let TMP = FROM - TO
            ; let ^1 = ^2 - ^3
PUSH 0xc0
MLOAD
PUSH 0x0100
MLOAD
SUB
PUSH 0x0120
MSTORE
            ; Let REMAINING read record TMP
            ; let ^1 read record ^2
PUSH 0x0120
MLOAD
SLOAD
PUSH 0x0140
MSTORE
            ; Return uint256 at REMAINING
            ; return uint256 at ^1
PUSH 0x20
PUSH 0x0140
RETURN
            ; jumpdest name()
            ; jumpdest ^1
JUMPDEST name()
            ; Grab string NAME #0
            ; grab string ^1
PUSH 0x010000
JUMPDEST TAGloop_MATCH_NUMBER65
DUP1
DUP1
SLOAD
SWAP1
MSTORE
PUSH 0x20
ADD
DUP1
SLOAD
JUMPI TAGloop_MATCH_NUMBER65
            ; Return string at NAME #0
            ; return string at ^1
PUSH 0x010000
PUSH 0x00
MSTORE
PUSH 0x010000
DUP1
MLOAD
ADD
PUSH 0x20
ADD
PUSH 0x00
RETURN
            ; jumpdest symbol()
            ; jumpdest ^1
JUMPDEST symbol()
            ; Grab string SYMBOL #0
            ; grab string ^1
PUSH 0x020000
JUMPDEST TAGloop_MATCH_NUMBER68
DUP1
DUP1
SLOAD
SWAP1
MSTORE
PUSH 0x20
ADD
DUP1
SLOAD
JUMPI TAGloop_MATCH_NUMBER68
            ; Return string at SYMBOL #0
            ; return string at ^1
PUSH 0x020000
PUSH 0x00
MSTORE
PUSH 0x020000
DUP1
MLOAD
ADD
PUSH 0x20
ADD
PUSH 0x00
RETURN
            ; jumpdest decimals()
            ; jumpdest ^1
JUMPDEST decimals()
            ; Grab record DECIMALS
            ; grab record ^1
PUSH 0xa0
DUP1
SLOAD
SWAP1
MSTORE
            ; Return uint256 at DECIMALS
            ; return uint256 at ^1
PUSH 0x20
PUSH 0xa0
RETURN
            ; jumpdest totalSupply()
            ; jumpdest ^1
JUMPDEST totalSupply()
            ; Grab record SUPPLY
            ; grab record ^1
PUSH 0x60
DUP1
SLOAD
SWAP1
MSTORE
            ; Return uint256 at SUPPLY
            ; return uint256 at ^1
PUSH 0x20
PUSH 0x60
RETURN
            ; Procedure "transfer"
            ; procedure ^1
JUMPDEST TAG_TRANSFER_PROCEDURE
            ; Let FROM BALANCE read record 1 of FROM
            ; let ^1 read record ^2 of ^3
PUSH 0x0100
MLOAD
PUSH 0x01
PUSH 0x20
MUL
ADD
SLOAD
PUSH 0x01c0
MSTORE
            ; push FROM BALANCE
            ; push ^1
PUSH 0x01c0
MLOAD
            ; push VALUE
            ; push ^1
PUSH 0xe0
MLOAD
            ; comp <=
            ; comp <=
GT
ISZERO
            ; false 1003
            ; false ^1
CONDITIONNOT 1003
            ; stop
            ; stop
STOP
            ; end 1003
            ; end ^1
CONDITIONEND 1003
            ; Let TO BALANCE read record 1 of TO
            ; let ^1 read record ^2 of ^3
PUSH 0xc0
MLOAD
PUSH 0x01
PUSH 0x20
MUL
ADD
SLOAD
PUSH 0x01e0
MSTORE
            ; Decrement FROM BALANCE by VALUE
            ; decrement ^1 by ^2
PUSH 0xe0
MLOAD
PUSH 0x01c0
MLOAD
SUB
PUSH 0x01c0
MSTORE
            ; Increment TO BALANCE by VALUE
            ; increment ^1 by ^2
PUSH 0xe0
MLOAD
PUSH 0x01e0
MLOAD
ADD
PUSH 0x01e0
MSTORE
            ; Write FROM BALANCE to record 1 of FROM
            ; write ^1 to record ^2 of ^3
PUSH 0x01c0
MLOAD
PUSH 0x0100
MLOAD
PUSH 0x01
PUSH 0x20
MUL
ADD
SSTORE
            ; Write TO BALANCE to record 1 of TO
            ; write ^1 to record ^2 of ^3
PUSH 0x01e0
MLOAD
PUSH 0xc0
MLOAD
PUSH 0x01
PUSH 0x20
MUL
ADD
SSTORE
            ; Grab record TRANSFER EVENT
            ; grab record ^1
PUSH 0x20
DUP1
SLOAD
SWAP1
MSTORE
            ; Log TRANSFER EVENT with topics FROM , TO , data VALUE
            ; log ^1 with topics ^2 , ^3 , data ^4
PUSH 0xc0
MLOAD
PUSH 0x0100
MLOAD
PUSH 0x20
SLOAD
PUSH 0x20
PUSH 0xe0
LOG3
            ; Procedure end
            ; procedure end
BACKJUMP
            ; jumpdest 0
            ; jumpdest ^1
JUMPDEST TAG0
            ; return
            ; return
STOP
