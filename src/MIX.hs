module MIX where

data Reg
    = RA    -- Accumulator: 5-bytes and a sign
    | RX    -- Extension: 5-bytes and a sign
    | RI1   -- Index 1: 2-bytes and a sign
    | RI2   -- Index 2: 2-bytes and a sign
    | RI3   -- Index 3: 2-bytes and a sign
    | RI4   -- Index 4: 2-bytes and a sign
    | RI5   -- Index 5: 2-bytes and a sign
    | RI6   -- Index 6: 2-bytes and a sign
    | RJ    -- Jump: 2-bytes and its sign is always +

data Directives
    = ORIG  -- sets the value of the memory address to which the following instr will be alloc after compilation
    | EQU   -- used to define symbol's value
    | CON   -- the value of the given expr is copied directly into the current mem address
    | ALF   -- takes as operand five chars, constituting five bytes of a word which is copied directly into
    | END   -- marks the end of the program; its operand gives start address for a prog exec 

data Instructions
    = NOP
    | ADD
    | FADD
    | SUB
    | FSUB
    | MUL
    | FMUL
    | DIV
    | FDIV
    | NUM
    | CHAR
    | HLT
    | SLA
    | SRA
    | SLAX
    | SRAX
    | SLC
    | SRC
    | MOVE
    | LDA
    | LD1
    | LD2
    | LD3
    | LD4
    | LD5
    | LD6
    | LDX
    | LDAN
    | LD1N
    | LD2N
    | LD3N
    | LD4N
    | LD5N
    | LD6N
    | LDXN
    | STA
    | ST1
    | ST2
    | ST3
    | ST4
    | ST5
    | ST6
    | STX
    | STJ
    | STZ
    | JBUS
    | IOC
    | IN
    | OUT
    | JRED
    | JMP
    | JSJ
    | JOV
    | JNOV
    | JLE
    | JE
    | JG
    | JGE
    | JNE
    | JLE
    | JA 
    | JAN
    | JAZ
    | JANN
    | JANZ
    | JANP
    | J1
    | J1N
    | J1Z
    | J1NN
    | J1NZ
    | J1NP
    | J2 
    | J2N
    | J2Z
    | J2NN
    | J2NZ
    | J2NP
    | J3
    | J3N
    | J3Z
    | J3NN
    | J3NZ
    | J3NP
    | J4
    | J4N
    | J4Z
    | J4NN
    | J4NZ
    | J4NP
    | J5
    | J5N
    | J5Z
    | J5NN
    | J5NZ
    | J5NP
    | J6
    | J6N
    | J6Z
    | J6NN
    | J6NZ
    | J6NP
    | JX 
    | JXN
    | JXZ
    | JXNN
    | JXNZ
    | JXNP
    | INCA
    | DECA
    | ENTA
    | ENNA
    | INC1
    | DEC1
    | ENT1
    | ENN1
    | INC2
    | DEC2
    | ENT2
    | ENN2
    | INC3
    | DEC3
    | ENT3
    | ENN3
    | INC4
    | DEC4
    | ENT4
    | ENN4
    | INC5
    | DEC5
    | ENT5
    | ENN5
    | INC6
    | DEC6
    | ENT6
    | ENN6
    | INCX
    | DECX
    | ENTX
    | ENNX
    | CMPA
    | FCMP
    | CMP1
    | CMP2
    | CMP3
    | CMP4
    | CMP5
    | CMP6
    | CMPX
