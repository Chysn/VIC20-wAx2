;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                     wAx 8K
;                            Integrated Monitor Tools
;                           (c)2020-2022 Jason Justian
;                  
; Release 1  - May 16, 2020
; Release 2  - May 23, 2020
; Release 3  - May 30, 2020
; Release 4K - June 14, 2020
; Release 5  - July 6, 2020
; Release 6  - September 12, 2021
; wAx2       - January 16, 2022
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2020-2022 Jason Justian
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; LABEL DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; Configuration
*           = $a000             ; Assembly location
LIST_NUM    = $10               ; Display this many lines
SEARCH_L    = $10               ; Search this many pages (s * 256 bytes)
DEF_DEVICE  = $08               ; Default device number
SYM_END     = $02ff             ; Top of Symbol Table
MAX_LAB     = 19                ; Maximum number of user labels + 1
MAX_FWD     = 12                ; Maximum number of forward references

; Tool Setup
TOOL_COUNT  = $1b               ; How many tools are there?
WEDGE       = "."               ; The wedge character
T_DIS       = "D"               ; Tool character D for disassembly
T_XDI       = "E"               ; Tool character E for extended opcodes
T_ASM       = "A"               ; Tool character A for assembly
T_ASM_AL    = ","               ;   Alias for assembly
T_MEM       = "M"               ; Tool character : for memory dump
T_BIN       = "%"               ; Tool character % for binary dump
T_TST       = $b2               ; Tool character = for tester
T_BRK       = "B"               ; Tool character B for breakpoint
T_REG       = "R"               ; Tool character R for register set
T_REG_AL    = ";"               ;   Alias for register set
T_EXE       = "G"               ; Tool character G for code execute
T_SAV       = "S"               ; Tool character S for save
T_LOA       = "L"               ; Tool character L for load
T_FIL       = "F"               ; Tool character F for file search
T_SRC       = "H"               ; Tool character H for search
T_CPY       = "T"               ; Tool character T for transfer/fill
T_INT       = "I"               ; Tool character I for text display
T_COM       = "C"               ; Tool character C for compare
T_H2T       = "$"               ; Tool character $ for hex to base 10
T_T2H       = "#"               ; Tool character # for base 10 to hex
T_SYM       = $ac               ; Tool character * for symbol table management
T_BAS       = $ae               ; Tool character ^ for BASIC stage select
T_USR       = "U"               ; Tool character U for user plug-in
T_MEN       = "P"               ; Tool character P for plug-in menu
T_USL       = $5c               ; Tool character GPB for user list
T_EXI       = "X"               ; Ersatz command for exit
T_HLP       = $99               ; Tool character ? for help (PRINT token)
LABEL       = "@"               ; Symbol dereferencer (@)

; System resources - Routines
GONE        = $c7e4
CHRGET      = $0073
CHRGOT      = $0079
PRTFIX      = $ddcd             ; Print base-10 number
SYS         = $e12d             ; BASIC SYS start
SYS_BRK     = $e133             ; BASIC SYS continue after BRK
SYS_TAIL    = $e144             ; BAIC SYS end
CHROUT      = $ffd2             ; Print one character
WARM_START  = $0302             ; BASIC warm start vector
READY       = $c002             ; BASIC warm start with READY.
NX_BASIC    = $c7ae             ; Get next BASIC command
CUST_ERR    = $c447             ; Custom BASIC error message
SYNTAX_ERR  = $cf08             ; BASIC syntax error
ERROR_NO    = $c43b             ; Show error in Accumulator
SETLFS      = $ffba             ; Setup logical file
SETNAM      = $ffbd             ; Setup file name
SAVE        = $ffd8             ; Save
LOAD        = $ffd5             ; Load
CLOSE       = $ffc3             ; Close logical file
OPEN        = $ffc0             ; Open logical file
CLALL       = $ffe7             ; Close all files
IOSTATUS    = $90               ; I/O Status
CHKIN       = $ffc6             ; Define file as input
CHRIN       = $ffcf             ; Get input
CLRCHN      = $ffcc             ; Close channel
ASCFLT      = $dcf3             ; Convert base-10 to FAC1
MAKADR      = $d7f7             ; FAC1 to Integer
DEVICE      = $ba               ; Save device
ISCNTC      = $ffe1             ; Check Stop key

; System resources - Vectors and Pointers
IGONE       = $0308             ; Vector to GONE
CBINV       = $0316             ; BRK vector
BUFPTR      = $7a               ; Pointer to buffer
ERROR_PTR   = $22               ; BASIC error text pointer
SYS_DEST    = $14               ; Pointer for SYS destination

; System resources - Data
KEYWORDS    = $c09e             ; Start of BASIC kewords for detokenize
BUF         = $0200             ; Input buffer
CHARAC      = $07               ; Temporary character
KEYBUFF     = $0277             ; Keyboard buffer and size, for automatically
KBSIZE      = $c6               ;   advancing the assembly address
CURLIN      = $39               ; Current line number
MISMATCH    = $c2cd             ; "MISMATCH"
KEYCVTRS    = $028d             ; Keyboard codes

; System resources - Registers
ACC         = $030c             ; Saved Accumulator
XREG        = $030d             ; Saved X Register
YREG        = $030e             ; Saved Y Register
PROC        = $030f             ; Saved Processor Status

; Constants
; Addressing mode encodings
INDIRECT    = $10               ; e.g., JMP ($0306)
INDIRECT_X  = $20               ; e.g., STA ($1E,X)
INDIRECT_Y  = $30               ; e.g., CMP ($55),Y
ABSOLUTE    = $40               ; e.g., JSR $FFD2
ABSOLUTE_X  = $50               ; e.g., STA $1E00,X
ABSOLUTE_Y  = $60               ; e.g., LDA $8000,Y
ZEROPAGE    = $70               ; e.g., BIT $A2
ZEROPAGE_X  = $80               ; e.g., CMP $00,X
ZEROPAGE_Y  = $90               ; e.g., LDX $FA,Y
IMMEDIATE   = $a0               ; e.g., LDA #$2D
IMPLIED     = $b0               ; e.g., INY
RELATIVE    = $c0               ; e.g., BCC $181E
ACCUM       = $d0               ; e.g., ROR A

; Other constants
TABLE_END   = $f2               ; Indicates the end of mnemonic table
XTABLE_END  = $d2               ; End of extended instruction table
QUOTE       = $22               ; Quote character
LF          = $0d               ; Linefeed
CRSRUP      = $91               ; Cursor up
CRSRRT      = $1d               ; Cursor right
CRSRLF      = $9d               ; Cursor left
RVS_ON      = $12               ; Reverse on
RVS_OFF     = $92               ; Reverse off
HIGH_BYTE   = $b1               ; High Byte (>)
LOW_BYTE    = $b3               ; Low Byte (<)

; Assembler symbol table
; You can relocate and/or resize the symbol table by settnig SYM_END,
; MAX_LAB, and MAX_FWD to meet your needs. The remaining labels will be
; set automatically, and you shouldn't need to touch them.
;
; Note that one of the labels is reserved for the special @/> label, so
; add one more to MAX_LAB than you need.
ST_SIZE     = (MAX_LAB + MAX_FWD) * 3 + 1
SYMBOL_D    = SYM_END-ST_SIZE+1 ; Symbol label definitions
SYMBOL_AL   = SYMBOL_D+MAX_LAB  ; Symbol address low bytes
SYMBOL_AH   = SYMBOL_AL+MAX_LAB ; Symbol address high bytes
SYMBOL_F    = SYMBOL_AH+MAX_LAB ; Symbol unresolved forward references
SYMBOL_FL   = SYMBOL_F+MAX_FWD  ;   Forward reference low bytes
SYMBOL_FH   = SYMBOL_FL+MAX_FWD ;   Forward reference high bytes
OVERFLOW_F  = SYMBOL_FH+MAX_FWD ; Symbol unresolved reference overflow count

; wAx workspace
X_PC        = $03               ; Persistent Counter (2 bytes)
USER_VECT   = $05               ; Plug-in vector (2 bytes)
WORK        = $a4               ; Temporary workspace (2 bytes)
MNEM        = $a4               ; Current Mnemonic (2 bytes)
EFADDR      = $a6               ; Program Counter (2 bytes)
CHARDISP    = $a8               ; Character display for Memory (2 bytes)
LANG_PTR    = $a8               ; Language Pointer (2 bytes)
OPCODE      = $aa               ; Assembly target for hypotesting
OPERAND     = $ab               ; Operand storage (2 bytes)
IDX_OUT     = $ad               ; Buffer index - Output
IDX_IN      = $ae               ; Buffer index - Input
TOOL_CHR    = $af               ; Current function (T_ASM, T_DIS)
OUTBUFFER   = $0218             ; Output buffer (24 bytes)
INBUFFER    = $0230             ; Input buffer (22 bytes)
USR_STORE   = $0247             ; Plug-in storage (8 bytes)
IDX_SYM     = $024f             ; Temporary symbol index storage
SEARCH_C    = $0250             ; Search counter
INSTSIZE    = $0251             ; Instruction size
IGNORE_RB   = $0252             ; Ignore relative branch range for forward refs
TEMP_CALC   = $0253             ; Temporary calculation
BYTE_MOD    = $0253             ; Byte modifier (high byte (>) or low byte (<))
RANGE_END   = $0254             ; End of range (2 bytes)
BREAKPOINT  = $0256             ; Breakpoint data (3 bytes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; wAx API JUMP TABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This JMP table starts at $a000
jInstall:   jmp Install
jBuff2Byte: jmp Buff2Byte
jCharGet:   jmp CharGet
jCharOut:   jmp CharOut
jHex:       jmp Hex
jIncAddr:   jmp IncAddr
jIncPC:     jmp IncPC
jLookup:    jmp Lookup
jPrintBuff: jmp PrintBuff
jResetIn:   jmp ResetIn
jResetOut:  jmp ResetOut
jShowAddr:  jmp ShowAddr
jShowPC:    jmp ShowPC
jEAtoPC:    jmp EAtoPC
jPrintStr:  jmp PrintStr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; INSTALLER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
Install:    jsr Rechain         ; Rechain BASIC program
            jsr SetupVec        ; Set up vectors (IGONE and BRK)
            lda #DEF_DEVICE     ; Set default device number
            sta DEVICE          ; ,,
            lda #<PlugMenu      ; Set default plug-in (to Install)
            sta USER_VECT       ; ,,
            lda #>PlugMenu      ; ,,
            sta USER_VECT+1     ; ,,
            lda #<Intro         ; Print introduction message
            ldy #>Intro         ; ,,
            jsr PrintStr        ; ,,
            jmp (READY)         ; Warm start with READY prompt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
main:       jsr CHRGET          ; Get the character from input or BASIC
            cmp #WEDGE          ; Is it the assigned wedge character?
            bne exit            ; If not, exit gracefully
-loop:      jsr CHRGET          ; Get the next character to scan for a tool
            beq to_prompt       ;   Just show prompt character if nothing
            cmp #T_EXI          ; If X, then don't show the wedge character
            beq warmst          ;   ,,
            cmp #WEDGE          ; Trim all leading wedge characters
            beq loop            ; ,,
            ldy #$00            ; Is the second character a tool character?
-loop:      cmp ToolTable,y     ; ,,
            beq Prepare         ; If so, run the selected tool
            iny                 ; Else, check the characters in turn
            cpy #TOOL_COUNT     ; ,,
            bne loop            ; ,,
            lda #CRSRUP         ; Show error ? for unknown tool
            jsr CHROUT          ; ,,
            lda #"?"            ; ,,
            jsr CHROUT          ; ,,
            lda #LF             ; ,,
            jsr CHROUT          ; ,,     
to_prompt:  jsr CHRGOT          ; Restore flags for the found character
            jmp Return          ; Show prompt (maybe) and warm start
exit:       jsr CHRGOT          ; Restore flags for the found character
            jmp GONE+3          ; +3 because the CHRGET is already done

; Prepare for Tool Run
; A wedge character has been entered, and will now be interpreted as a wedge
; command. Prepare for execution by
; (1) Setting a return point
; (2) Putting the tool's start address-1 on the stack
; (3) Transcribing from BASIC or input buffer to the wAx input buffer
; (4) Reading the first four hexadecimal characters used by all wAx tools and
;     setting the Carry flag if there's a valid 16-bit number provided
; (5) RTS to route to the selected tool            
Prepare:    sta TOOL_CHR        ; Store the tool character
            lda #>Return-1      ; Push the address-1 of Return onto the stack
            pha                 ;   as the destination for RTS of the
            lda #<Return-1      ;   selected tool
            pha                 ;   ,,
            lda ToolAddr_H,y    ; Push the looked-up address-1 of the selected
            pha                 ;   tool onto the stack. The RTS below will
            lda ToolAddr_L,y    ;   pull off the address and route execution
            pha                 ;   to the appropriate tool
            jsr ResetIn         ; Initialize the input index for write
            sta IGNORE_RB       ; Clear Ignore Relative Branch flag
            lda #0              ; Clear byte modifier
            sta BYTE_MOD        ; ,,
            jsr Transcribe      ; Transcribe from CHRGET to INBUFFER
            lda #$ef            ; $0082 BEQ $008a -> BEQ $0073 (maybe)
            sta $83             ; ,,
RefreshPC:  jsr ResetIn         ; Re-initialize for buffer read
            jsr Buff2Byte       ; Convert 2 characters to a byte   
            bcc main_r          ; Fail if the byte couldn't be parsed
            sta EFADDR+1        ; Save to the EFADDR high byte
            jsr Buff2Byte       ; Convert next 2 characters to byte
            bcc main_r          ; Fail if the byte couldn't be parsed
            sta EFADDR          ; Save to the EFADDR low byte
main_r:     rts                 ; Pull address-1 off stack and go there
    
; Return from Wedge
; Return in one of two ways--
; (1) In direct mode, to a BASIC warm start without READY.
; (2) In a program, find the next BASIC command
Return:     jsr DirectMode      ; If in Direct Mode, warm start without READY.
            bne in_program      ;   ,,
            lda KBSIZE          ; If there's nothing in the keyboard buffer
            bne warmst          ;   already (like an existing prompt),
            lda #WEDGE          ;   store the wedge character in the keyboard
            sta KEYBUFF         ;   buffer as a convenience
            lda #1              ;   ,,
            sta KBSIZE          ;   ,,
warmst:     jmp (WARM_START)    ; Warm start to BASIC           
in_program: lda #$00            ; In a program, reset the keyboard buffer size
            sta KBSIZE          ;   to 0 to avoid any prompts
            jmp NX_BASIC        ; Otherwise, continue to next BASIC command   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; COMMON LIST COMPONENT
; Shared entry point for Disassembler and Memory Dump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
List:       bcc list_cont       ; If no start address, continue list at EFADDR
            lda #$ff            ; Default range to top of memory
            sta RANGE_END       ; ,,
            sta RANGE_END+1     ; ,,
            jsr Buff2Byte       ; 
            bcc start_list      ; If an optional end address is provided, set
            sta RANGE_END+1     ;   the end-of-range address
            jsr Buff2Byte       ;   ,,
            bcc start_list      ;   ,,
            sta RANGE_END       ;   ,,
            lda RANGE_END+1
            cmp EFADDR+1
            bcc lrange_bad
            lda RANGE_END
            cmp EFADDR
            bcs lrange_ok
lrange_bad: jmp list_r
lrange_ok:  ldx #$80            ; When X=$80, list won't stop after LIST_NUM
            bne ListLine        ;   lines, but will go through range unless STOP
list_cont:  lda X_PC            ; Otherwise, set the effective addresss to the
            sta EFADDR          ;  persistent counter to continue listing
            lda X_PC+1          ;  after the last address
            sta EFADDR+1        ;  ,,
start_list: ldx #LIST_NUM       ; Default if no number has been provided
ListLine:   txa
            pha
            jsr ResetOut
            jsr BreakInd        ; Indicate breakpoint, if it's here
            jsr wAxPrompt       ; Start each line with the wedge character
            lda #T_ASM_AL       ;   Followed by the assembly alias character
            jsr CharOut         ;   ,,
            jsr Space           ;   Followed by Space
            jsr ShowAddr        ;   Followed by the address
            lda TOOL_CHR        ; What tool is being used?
            cmp #T_MEM          ; Memory Dump
            beq to_mem          ; ,,
            cmp #T_BIN          ; Binary Dump
            beq to_bin          ; ,,
            cmp #T_USL          ; User list tool
            beq to_usr          ; ,,
            cmp #T_INT          ; Text list tool
            beq to_int          ; ,,
            jsr Space           ; Space goes after address for Disassembly
            jsr Disasm
            jmp continue
to_mem:     lda #":"            ; Memory editor character goes after address
            jsr CharOut         ; ,,
            jsr Memory          ; Do Memory display
            jmp continue
to_bin:     jsr CharOut         ; Binary editor character goes after address
            jsr BinaryDisp      ; Do Binary display
            jmp continue
to_int:     jsr TextDisp        ; Do text display
            jmp continue
to_usr:     jsr PlugIn            
continue:   jsr PrintBuff      
            pla
            tax
            bpl skip_range      ; If X bit 7 is clear, don't check range
            inx                 ; Compensate for dex to keep bit 7 set
            lda EFADDR+1        ; Check to see if we've gone beyond the
            cmp RANGE_END+1     ;   specified range
            bcc skip_range      ;   ,,
            lda EFADDR          ;   ,,
            cmp RANGE_END       ;   ,,
            bcs list_stop       ; If so, end the list
skip_range: jsr ISCNTC          ; Exit if STOP key is pressed
            beq list_stop       ; ,,          
            dex                 ; Exit if loop is done
            bne ListLine        ; ,,
            inx                 ; But if the loop is done, but a Shift key
            lda KEYCVTRS        ;   is engaged, then go back for one more
            and #$01            ;   ,,
            bne ListLine        ;   ,,
            lda TOOL_CHR        ; If the breakpoint was set, don't update
            cmp #T_BRK          ;   the persistent counter or show a tool
            beq list_r          ;   prompt
list_stop:  lda #WEDGE          ; Provide a tool for the next page in the key-
            sta KEYBUFF         ;   board buffer
            lda TOOL_CHR        ;   ,,
            sta KEYBUFF+1       ;   ,,
            lda #CRSRLF         ;   ,,
            sta KEYBUFF+2       ;   ,,
            lda #$03            ;   ,,
            sta KBSIZE          ;   ,,
            jsr EAtoPC          ; Update persistent counter with effective addr
list_r:     jmp EnableBP        ; Re-enable breakpoint, if necessary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; DISASSEMBLER COMPONENTS
; https://github.com/Chysn/wAx/wiki/6502-Disassembler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Disassemble
; Disassemble a single instruction at the effective address
Disasm:     jsr IncAddr         ; Get opcode
            jsr Lookup          ; Look it up
            bcc Unknown         ; Clear carry indicates an unknown opcode
            pha                 ; Store addressing mode for later
            jsr DMnemonic       ; Display mnemonic
            lda TOOL_CHR        ; If the search is being run, go directly
            cmp #T_SRC          ;   to the operand
            beq disasm_op       ;   ,,
            jsr Space
disasm_op:  pla                 ; Pass addressing mode to operand routine
            jmp DOperand        ; Display operand

; Unknown Opcode
Unknown:    lda #":"            ; Memory entry before an unknown byte
            jsr CharOut         ; ,,
            lda OPCODE          ; The unknown opcode is still here   
            jmp Hex             
            
; Mnemonic Display
DMnemonic:  lda MNEM+1          ; These locations are going to rotated, so
            pha                 ;   save them on a stack for after the
            lda MNEM            ;   display
            pha                 ;   ,,
            ldx #$03            ; Three characters...
-loop:      lda #$00
            ldy #$05            ; Each character encoded in five bits, shifted
shift_l:    asl MNEM+1          ;   as a 24-bit register into Accumulator, which
            rol MNEM            ;   winds up as a ROT0 code (A=1 ... Z=26)
            rol                 ;   ,,
            dey
            bne shift_l
            ;clc                ; Carry is clear from the last ROL
            adc #"@"            ; Get the PETSCII character
            jsr CharOut
            dex
            bne loop
            pla
            sta MNEM
            pla
            sta MNEM+1
mnemonic_r: rts

; Operand Display
; Dispatch display routines based on addressing mode
DOperand:   cmp #IMPLIED        ; Handle each addressing mode with a subroutine
            beq mnemonic_r      ; Implied has no operand, so it goes to some RTS
            cmp #ACCUM          ;
            bne ch_rel          ; Handle accumulator mode right here, because
            lda #"A"            ;   it's super-small
            jmp CharOut         ;   ,,
ch_rel:     cmp #RELATIVE
            beq DisRel
            cmp #IMMEDIATE
            beq DisImm
            cmp #ZEROPAGE       ; Subsumes all zeropage modes
            bcs DisZP
            cmp #ABSOLUTE       ; Subsumes all absolute modes
            bcs DisAbs
            ; Fall through to DisInd, because it's the only one left

; Disassemble Indirect Operand
DisInd:     pha
            lda #"("
            jsr CharOut
            pla
            cmp #INDIRECT
            bne ind_xy
            jsr Param_16
            jmp CloseParen
ind_xy:     pha
            jsr Param_8
            pla
            cmp #INDIRECT_X
            bne ind_y
            jsr Comma
            lda #"X"
            jsr CharOut
            jmp CloseParen
ind_y:      jsr CloseParen
            jsr Comma
            lda #"Y"
            jmp CharOut

; Disassemble Immediate Operand         
DisImm:     lda #"#"
            jsr CharOut
            jmp Param_8

; Disassemble Zeropage Operand
DisZP:      pha
            jsr Param_8
            pla
            sec
            sbc #ZEROPAGE
            jmp draw_xy         ; From this point, it's the same as Absolute            

; Disassemble Relative Operand
DisRel:     jsr HexPrefix
            jsr IncAddr         ; Get the operand of the instruction
            sta WORK
            and #$80            ; Get the sign of the operand
            beq sign
            ora #$ff            ; Extend the sign out to 16 bits, if negative
sign:       sta WORK+1          ; Set the high byte to either $00 or $ff
            lda WORK            ; Calculate offset from next instructions
            clc                 ; ,,
            adc EFADDR          ; ,,
            sta WORK            ; ,,
            lda WORK+1          ; ,,
            adc EFADDR+1        ; ,,
            jsr Hex             ; No need to save the high byte, just show it
            lda WORK            ; Show the low byte of the computed address
            jmp Hex             ; ,,
                            
; Disassemble Absolute Operand           
DisAbs:     pha                 ; Save addressing mode for use later
            jsr Param_16
            pla
            sec
            sbc #ABSOLUTE
draw_xy:    ldx #"X"
            cmp #$10
            beq abs_ind
            ldx #"Y"
            cmp #$20
            beq abs_ind
            rts
abs_ind:    jsr Comma           ; This is an indexed addressing mode, so
            txa                 ;   write a comma and index register
            jmp CharOut         ;   ,,
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MEMORY EDITOR COMPONENTS
; https://github.com/Chysn/wAx/wiki/Memory-Editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MemEditor:  sta TOOL_CHR        ; Update tool character for Prompt
            lda #$04            ; The number of allowed bytes is temporarily
            sta CHARAC          ;   stored in CHARAC.
            jsr DirectMode      ; If MemEditor is run in a BASIC program, allow
            beq start_mem       ;   more bytes per line, because we don't need
            lda #$08            ;   to worry about intrference with the PETSCII
            sta CHARAC          ;   display.
start_mem:  ldy #$00
-loop:      jsr Buff2Byte
            bcc edit_exit       ; Bail out on the first non-hex byte
            sta (EFADDR),y      
            iny
            cpy CHARAC
            bne loop
edit_exit:  cpy #$00
            beq asm_error
            tya
            tax
            jsr Prompt          ; Prompt for the next address
            jsr ClearBP         ; Clear breakpoint if anything was changed
edit_r:     rts

; Text Editor
; If the input starts with a quote, add characters until we reach another
; quote, or 0
TextEdit:   ldy #$00            ; Y=Data Index
-loop:      jsr CharGet
            beq asm_error       ; Return to MemEditor if 0
            cmp #QUOTE          ; Is this the closing quote?
            beq edit_exit       ; Return to MemEditor if quote
            sta (EFADDR),y      ; Populate data
            iny
            cpy #$10            ; String size limit
            beq edit_exit
            jmp loop
            
; Binary Editor
; If the input starts with a %, get one binary byte and store it in memory                   
BinaryEdit: sta TOOL_CHR        ; Update tool character for prompt
            jsr BinaryByte      ; Get 8 binary bits
            ;bcc edit_r         ; If invalid, errors at BinaryByte
            ldy #$00            ; Store the valid byte to memory
            sta (EFADDR),y      ; ,,
            iny                 ; Increment the byte count and return to
            jmp edit_exit       ;   editor            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; ASSEMBLER COMPONENTS
; https://github.com/Chysn/wAx/wiki/6502-Assembler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Centrally-located error jump
asm_error:  jmp AsmError

; Assemble 6502 Instruction
; Or data
Assemble:   bcc asm_r           ; Bail if the address is no good
            lda INBUFFER+4      ; If the user just pressed Return at the prompt,
            beq asm_r           ;   go back to BASIC
-loop:      jsr CharGet         ; Look through the buffer for either
            beq test            ;   0, which should indicate implied mode, or:
            ldy IDX_IN          ; If we've gone past the first character after
            cpy #$05            ;   the address, no longer pay attention to
            bne op_parts        ;   pre-data stuff
            cmp #LABEL          ; - = New label
            beq DefLabel        ; ,,
            cmp #":"            ; Colon = Byte entry (route to hex editor)
            beq MemEditor       ; ,,
            cmp #QUOTE          ; " = Text entry (route to text editor)
            beq TextEdit        ; ,,
            cmp #T_BIN          ; % = Binary entry (route to binary editor)
            beq BinaryEdit      ; ,,
op_parts:   cmp #"#"            ; # = Parse immediate operand (quotes and %)
            beq ImmedOp         ; ,,         
            cmp #"$"            ; $ = Parse the operand
            bne loop            ; ,,
main_op:    jsr GetOperand      ; Once $ is found, then grab the operand
test:       jsr Hypotest        ; Line is done; hypothesis test for a match
            bcc asm_error       ; Clear carry means the test failed
            ldy #$00            ; A match was found! Transcribe the good code
            lda OPCODE          ;   to the effective address. The number of
            sta (EFADDR),y      ;   bytes to transcribe is stored in the
            ldx INSTSIZE        ;   INTSIZE location.
            cpx #$02            ; Store the low operand byte, if indicated
            bcc nextline        ; ,,
            lda OPERAND         ; ,,
            iny                 ; ,,
            sta (EFADDR),y      ; ,,
            cpx #$03            ; Store the high operand byte, if indicated
            bcc nextline        ; ,,
            lda OPERAND+1       ; ,,
            iny                 ; ,,
            sta (EFADDR),y      ; ,,
nextline:   jsr ClearBP         ; Clear breakpoint on successful assembly
            jsr Prompt          ; Prompt for next line if in direct mode
asm_r:      rts

; Define Label
; Create a new label entry, and resolve any forward references to the
; new label.
DefLabel:   jsr CharGet         ; Get the next character after the label sigil
            jsr SymbolIdx       ; Get a symbol index for the label in A
            bcs have_label      ;
            jmp SymError        ; Error if no symbol index could be secured
have_label: jsr IsDefined       ; If this label is not yet defined, then
            bne is_def          ;   resolve the forward reference, if it
            sty IDX_SYM         ;   was used
            jsr ResolveFwd      ;   ,,
            ldy IDX_SYM
is_def:     lda EFADDR          ; Set the label address
            sta SYMBOL_AL,y     ; ,,
            lda EFADDR+1        ; ,,
            sta SYMBOL_AH,y     ; ,,
            jsr CharGet
            cmp #$00
            bne pull_code
            ldx #$00            ; Return to BASIC or prompt for the same
            jmp Prompt          ;   address again
pull_code:  ldy #$00            ; If there's code after the label, pull it
-loop:      iny                 ;   two spaces, replacing the label. This
            lda INBUFFER+5,y    ;   positions the instruction for use with
            sta INBUFFER+3,y    ;   Hypotest later
            bne loop            ;   ,,
            lda #$04            ; Reset the buffer position to the start
            sta IDX_IN          ;   of the code  
            sec                 ;   ,,
            jmp Assemble        ;   ,,
 
; Parse Immediate Operand
; Immediate operand octets are expressed in the following formats--
; (1) $dd       - Hexadecimal 
; (2) "c"       - Character
; (3) %bbbbbbbb - Binary
; (4) #d[d][d]  - Base 10
ImmedOp:    jsr CharGet         ; This is the character right after #
            cmp #"$"            ; If it's $, go back to get regular $ operand
            beq main_op         ; ,,
try_quote:  cmp #QUOTE          ; If it's a double quote, make sure it's a one
            bne try_binary      ;   character surrounded by quotes. If it is,
            jsr CharGet         ;   set it as the operand and convert it to
            sta OPERAND         ;   hex for the hypotester
            jsr CharGet         ;   ,,
            cmp #QUOTE          ;   ,,
            bne AsmError        ;   ,, Error if the second quote isn't here
            beq insert_hex      ;   ,,
try_binary: cmp #"%"            ; If it's a binary prefix sigil %, convert
            bne try_base10      ;   the eight binary bits and, if valid,
            jsr BinaryByte      ;   set the operand and convert it to hex
            ;bcc AsmError       ;   ,, (errors at BinaryByte)
            sta OPERAND         ;   ,,
            bcs insert_hex      ;   ,,
try_base10: lda $7b             ; Now look for a base-10 number by temporarily
            pha                 ;   setting CHRGET's buffer to wAx's input
            lda $7a             ;   buffer and scanning for base-10 digits.
            pha                 ;   ,,
            ldy #<INBUFFER+8    ; Point the CHRGET buffer at the location
            lda #>INBUFFER+8    ;   after the #
            sty $7a             ;   ,, 
            sta $7b             ;   ,,
            jsr CHRGOT          ; Call CHRGOT to start verifying numbers
            bcs AsmError        ;   ,,
            jsr ASCFLT          ; Convert the buffer text into FAC1
            jsr MAKADR          ; Convert FAC1 to 16-bit unsigned integer
            cmp #$00            ; High byte from MAKADR is in A
            bne AsmError        ; Error if high byte is set; too big for immed
            sty OPERAND         ; Low byte from MAKADR is in Y
            pla                 ; Put the CHRGET buffer back so BASIC doesn't
            sta $7a             ;   freak out
            pla                 ;   ,,
            sta $7b             ;   ,,
            ; Fall through to insert_hex
insert_hex: jsr ResetOut        ; Store the hex value of the operand after the
            sta INBUFFER+11     ;   #, so it can be matched by Hypotest.
            lda #"$"            ;   End it with 0 as a line delimiter
            sta INBUFFER+8      ;   ,,
            lda OPERAND         ;   ,,
            jsr Hex             ;   ,,
            lda OUTBUFFER       ;   ,,
            sta INBUFFER+9      ;   ,,
            lda OUTBUFFER+1     ;   ,,
            sta INBUFFER+10     ;   ,,
            jmp test
            
; Error Message
; Invalid opcode or formatting (ASSEMBLY)
; Failed boolean assertion (MISMATCH, borrowed from ROM)
AsmError:   ldx #$00            ; ?ASSMEBLY ERROR
            .byte $3c           ; TOP (skip word)
MisError:   ldx #$01            ; ?MISMATCH ERROR
            .byte $3c           ; TOP (skip word)
SymError:   ldx #$02            ; ?SYMBOL ERROR
            .byte $3c           ; TOP (skip word)
CannotRes:  ldx #$03            ; ?CAN'T RESOLVE ERROR 
            .byte $3c           ; TOP (skip word)
OutOfRange: ldx #$04            ; ?TOO FAR ERROR
            lda ErrAddr_L,x
            sta ERROR_PTR
            lda ErrAddr_H,x
            sta ERROR_PTR+1
            jmp CUST_ERR          

; Get Operand
; Populate the operand for an instruction
GetOperand: lda #1              ; 
            sta INSTSIZE        ; ,,
            jsr Buff2Byte       ; Get the first byte
            bcc getop_r         ; If invalid, return
            sta OPERAND+1       ; Default to being high byte
            jsr Buff2Byte
            bcs high_byte       ; If an 8-bit operand is provided, the first
            lda OPERAND+1       ;   byte is the low byte
            dec INSTSIZE        ; Set to 0 if only one operand byte
            dec IDX_IN          ; If there's one byte, step back the input index
high_byte:  sta OPERAND         ;   set the low byte with the input
            jsr CharGet         ; Check character after operand
            cmp #"+"            ; Perform addition
            beq add_op          ; ,,
            cmp #"-"            ; Perform subtraction
            beq sub_op
getop_r:    rts
sub_op:     jsr CharGet
            jsr Char2Nyb
            bcc getop_r
            sta TEMP_CALC
            sec
            lda OPERAND
            sbc TEMP_CALC
            sta OPERAND
            bcs repl_hex
            dec OPERAND+1
            jmp repl_hex
add_op:     jsr CharGet         ; Get the character after the operator
            jsr Char2Nyb        ; Treat it as a single hex digit
            bcc getop_r         ; Return if invalid hex digit
            clc                 ; Add the number to the operand
            adc OPERAND         ; ,,
            sta OPERAND         ; ,,
            bcc repl_hex        ; ,,
            inc OPERAND+1       ; ,,
repl_hex:   ldx IDX_IN          ; Set the index of the + to 0 so that parsing
            dex                 ;   ends where + used to be
            dex                 ;   ,,
            lda #0              ;   ,,
            sta INBUFFER,x      ;   ,,
            jsr ResetOut        ; Will be using the output buffer as tmp hex
            ldx #8              ; Starting index of instruction operand
            stx IDX_IN
            lda INSTSIZE        ; If only one byte was provided (INSTSIZE=0),
            beq pl1             ;   then update only the low byte (OPERAND)
            lda OPERAND+1       ; Otherwise, there are two bytes, so start by
            jsr Hex             ;   updating the high byte
            jsr copy_op         ;   ,,
            inc IDX_IN          ; Advance the index by 2 for the low byte
            inc IDX_IN          ; ,,
            jsr ResetOut        ; Same as above, reset the output buffer, which
pl1:        lda OPERAND         ;   holds the hex value of the low byte
            jsr Hex             ;   ,,
copy_op:    ldx IDX_IN          ; Index of the input that we're copying TO
            lda OUTBUFFER       ; Get first hex digit of output buffer
            sta INBUFFER,x      ; Copy it to input at index
            lda OUTBUFFER+1     ; Get second hex digit of output buffer
            sta INBUFFER+1,x    ; Copy it to input at index
pl_r:       rts
            
; Hypothesis Test
; Search through the language table for each opcode and disassemble it using
; the opcode provided for the candidate instruction. If there's a match, then
; that's the instruction to assemble at the effective address. If Hypotest tries
; all the opcodes without a match, then the candidate instruction is invalid.
Hypotest:   jsr ResetLang       ; Reset language table
reset:      ldy #$06            ; Offset disassembly by 6 bytes for buffer match   
            sty IDX_OUT         ;   b/c output buffer will be "$00AC INST"
            lda #OPCODE         ; Write location to effective addr for hypotesting
            sta EFADDR          ; ,,
            ldy #$00            ; Set the effective address high byte
            sty EFADDR+1        ; ,,
            jsr NextInst        ; Get next instruction in 6502 table
            cmp #XTABLE_END     ; If we've reached the end of the table,
            beq bad_code        ;   the assembly candidate is no good
            sta OPCODE          ; Store opcode to hypotesting location
            jsr IncAddr  
            jsr DMnemonic       ; Add mnemonic to buffer
            ldy #$01            ; Addressing mode is at (LANG_PTR)+1
            lda (LANG_PTR),y    ; Get addressing mode to pass to DOperand
            pha
            jsr DOperand        ; Add formatted operand to buffer
            lda #$00            ; Add 0 delimiter to end of output buffer so
            jsr CharOut         ;  the match knows when to stop
            pla
            cmp #RELATIVE       ; If the addressing mode is relative, test
            beq test_rel        ;   separately and check range
            cmp #ACCUM          ; If the addressing mode is accumulator,
            bne run_match       ;   test separately
            lda IDX_IN          ; If the candidate is greater than 4 characters,
            cmp #$0a            ;   it cannot be an accumulator instruction
            bcs reset           ;   ,, 
            ldy #$07            ; For accumulator mode, the character following
            lda INBUFFER,y      ;   the mnemonic may be either $00 (ROR), or
            beq ch_accum        ;   "A" (ROR A).
            cmp #"A"            ;   ,,
            bne reset
ch_accum:   lda #$09
            sta IDX_OUT
run_match:  jsr IsMatch
            bcc reset
match:      lda EFADDR          ; Set the INSTSIZE location to the number of
            sec                 ;   bytes that need to be programmed
            sbc #OPCODE         ;   ,,
            sta INSTSIZE        ;   ,,
            jmp RefreshPC       ; Restore the effective address to target addr
test_rel:   lda #$0a            ; For relative branch instructions, first check
            sta IDX_OUT         ;   the name of the instruction. If that checks
            jsr IsMatch         ;   out, compute the relative branch offset and
            bcc reset           ;   insert it into memory, if it's within range
            jsr RefreshPC       ;   ,,
            jsr ComputeRB       ;   ,,
            sty OPERAND         ;   ,,
            lda #$02            ;   ,, 
            sta INSTSIZE        ;   ,,
            sec                 ; Set carry to indicate success
            rts
bad_code:   clc                 ; Clear carry flag to indicate failure
            rts
            
; Compute Relative Branch Offset
; With branch instruction in EFADDR and target in OPERAND
; Return offset in Y if valid, or error message if too far
ComputeRB:  lda EFADDR+1        ; Stash the effective address, as the offset
            pha                 ;   is computed from the start of the next
            lda EFADDR          ;   instruction
            pha                 ;   ,,
            jsr IncAddr         ; EFADDR += 2
            jsr IncAddr         ; ,,
            lda OPERAND         ; Subtract operand from effective address
            sec                 ;   to get offset
            sbc EFADDR          ;   ,,
            tay                 ;   ,, (Y will be the RB offset)
            lda OPERAND+1       ;   ,,
            sbc EFADDR+1        ;   ,,
            tax                 ;   ,,
            pla                 ; Put back the tool's effective address
            sta EFADDR          ; ,,
            pla                 ; ,,
            sta EFADDR+1        ; ,,
            cpx #$ff            ; Check the range; the difference must be between
            beq neg             ;   $ff80 and $007f, inclusive
            cpx #$00            ;   ,,
            beq pos             ;   ,,
rb_err:     lda IGNORE_RB
            bne compute_r
            jmp OutOfRange      ; BASIC error if out of range
neg:        cpy #$80
            bcc rb_err
            rts
pos:        cpy #$80
            bcs rb_err
compute_r:  rts            
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MEMORY DUMP COMPONENT
; https://github.com/Chysn/wAx/wiki/Memory-Dump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Memory:     ldy #$00
-loop:      jsr ReverseOn
            cpy #0
            beq r_on
            cpy #2
            beq r_on
            lda #RVS_OFF
            jsr CharOut
r_on:       lda (EFADDR),y
            sta CHARDISP,y
            jsr Hex
            iny
            cpy #$04
            beq show_char
            jmp loop       
show_char:  jsr Space           ; Space after hex values
            jsr ReverseOn       ; Reverse on for the characters
            ldy #$00
-loop:      lda CHARDISP,y
            cmp #$a0            ; Everything from 160 on is allowed in the
            bcs add_char        ;   display unchaged
            cmp #$80            ; Change everything between 128 and 159 
            bcs alter_char      ; ,,
            cmp #$20            ; Show everything else at and above space
            bcs add_char        ; ,,
alter_char: lda #" "            ; Everything else gets a space
add_char:   jsr CharOut         ; ,,
            jsr IncAddr
next_char:  iny
            cpy #04
            bne loop            
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; BINARY DUMP COMPONENT
; https://github.com/Chysn/wAx/wiki/Memory-Dump#binary-dump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BinaryDisp: jsr IncAddr         ; Get byte at effetive address
            sta TEMP_CALC       ; Store byte for binary conversion
            pha                 ; Push for use as a hex number
            lda #%10000000      ; Start with high bit
-loop:      pha
            bit TEMP_CALC
            beq is_zero
            jsr ReverseOn
            lda #"1"
            jsr CharOut
            lda #RVS_OFF
            .byte $3c           ; TOP (skip word)
is_zero:    lda #"0"
            jsr CharOut
            pla
            lsr
            bne loop
            jsr Space
            pla
            jmp Hex

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; ASSERTION TESTER COMPONENT
; https://github.com/Chysn/wAx/wiki/Assertion-Tester 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Tester:     ldy #$00
-loop:      jsr Buff2Byte
            bcc test_r          ; Bail out on the first non-hex byte
            cmp (EFADDR),y
            bne test_err      
            iny
            bne loop
test_r:     tya                 ; Update effective address with number of
            clc                 ;   bytes tested, in order to update the
            adc EFADDR          ;   persistent counter
            sta X_PC            ;   ,,
            lda #$00            ;   ,,
            adc EFADDR+1        ;   ,,
            sta X_PC+1          ;   ,,
            rts
test_err:   jmp MisError

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; SUBROUTINE EXECUTION COMPONENT
; https://github.com/Chysn/wAx/wiki/Subroutine-Execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Execute:    bcc iterate         ; No address was provided; continue from BRKpt
            lda EFADDR          ; Set the temporary INT storage to the program
            sta SYS_DEST        ;   counter. This is what SYS uses for its
            lda EFADDR+1        ;   execution address, and I'm using that
            sta SYS_DEST+1      ;   system to borrow saved Y,X,A,P values
            lda #>RegDisp-1     ; Add the register display return address to
            pha                 ;   the stack, as the return point after the
            lda #<RegDisp-1     ;   SYS tail
            pha                 ;   ,,
            jsr SetupVec        ; Make sure the BRK handler is enabled
            jmp SYS             ; Call BASIC SYS, after the parameter parsing
iterate:    pla                 ; Remove return to Return from the stack; it
            pla                 ;   is not needed
            jsr SetupVec        ; Make sure the BRK handler is enabled
            jmp SYS_BRK         ; SYS with no tail return address
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; REGISTER COMPONENTS
; https://github.com/Chysn/wAx/wiki/Register-Editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Register:   jsr ResetIn
            jsr Buff2Byte
            bcc RegDisp
            sta ACC
            jsr Buff2Byte
            bcc register_r
            sta XREG
            jsr Buff2Byte
            bcc register_r
            sta YREG
            jsr Buff2Byte
            bcc register_r
            sta PROC
register_r: rts

; Register Display            
RegDisp:    jsr ResetOut
            lda #<Registers     ; Print register display bar
            ldy #>Registers     ; ,,
            jsr PrintStr        ; ,,
            ldy #$00            ; Get registers' values from storage and add
-loop:      lda ACC,y           ;   each one to the buffer. These values came
            jsr Hex             ;   from the hardware IRQ, and are A,X,Y,P
            jsr Space           ;   ,,
            iny                 ;   ,,
            cpy #$04            ;   ,,
            bne loop            ;   ,,
            tsx                 ; Add stack pointer to the buffer
            txa                 ; ,,
            jsr Hex             ; ,,
            jsr Space           ; ,,
            lda SYS_DEST+1      ; Print high byte of SYS destination
            jsr Hex             ; ,,
            lda SYS_DEST        ; Print low byte of SYS destination
            jsr Hex             ; ,,
            jmp PrintBuff       ; Print the buffer
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; BREAKPOINT COMPONENTS
; https://github.com/Chysn/wAx/wiki/Breakpoint-Manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetBreak:   php
            jsr ClearBP         ; Clear the old breakpoint, if it exists
            plp                 ; If no breakpoint is chosen (e.g., if B was
            bcc SetupVec        ;   by itself), just clear the breakpoint
            lda EFADDR          ; Add a new breakpoint at the effective address
            sta BREAKPOINT      ; ,,
            lda EFADDR+1        ; ,,
            sta BREAKPOINT+1    ; ,,
            ;ldy #$00           ; (Y is already 0 from ClearBP)
            lda (EFADDR),y      ; Stash it in the Breakpoint data structure,
            sta BREAKPOINT+2    ;   to be restored on the next break
            tya                 ; Write BRK to the breakpoint location
            sta (EFADDR),y      ;   ,,
            lda #CRSRUP         ; Cursor up to overwrite the command
            jsr CHROUT          ; ,,
            jsr DirectMode      ; When run inside a BASIC program, skip the
            bne SetupVec        ;   BRK line display
            ldx #$01            ; List a single line for the user to review
            jsr ListLine        ; ,,
            ; Fall through to SetupVec

; Set Up Vectors
; Used by installation, and also by the breakpoint manager                    
SetupVec:   lda #<main          ; Intercept GONE to process wedge
            sta IGONE           ;   tool invocations
            lda #>main          ;   ,,
            sta IGONE+1         ;   ,,
            lda #<Break         ; Set the BRK interrupt vector
            sta CBINV           ; ,,
            lda #>Break         ; ,,
            sta CBINV+1         ; ,,
            rts

; BRK Trapper
; Replaces the default BRK handler. Gets registers from hardware interrupt
; and puts them in the SYS register storage locations. Gets program counter
; and stores it in the persistent counter location. Then falls through
; to register display.
Break:      pla                 ; Get values from stack and put them in the
            tay                 ;   proper registers
            pla                 ;   ,,
            tax                 ;   ,,
            pla                 ;   ,,
            plp                 ; Get the processor status
            cld                 ; Escape hatch for accidentally-set Decimal flag
            jsr SYS_TAIL        ; Store regiters in SYS locations
            pla                 ; Get Program Counter from interrupt and put
            sta SYS_DEST        ;   it in the persistent counter
            pla                 ;   ,,
            sta SYS_DEST+1      ;   ,,
            lda #<BreakMsg      ; Print BRK indicator
            ldy #>BreakMsg      ; ,,
            jsr PrintStr        ; ,,
            jsr RegDisp         ; Show the register display
            jmp Return
            
; Clear Breakpoint   
; Restore breakpoint byte and zero out breakpoint data         
ClearBP:    lda BREAKPOINT      ; Get the breakpoint
            sta CHARAC          ; Stash it in a zeropage location
            lda BREAKPOINT+1    ; ,,
            sta CHARAC+1        ; ,,
            ldy #$00
            lda (CHARAC),y      ; What's currently at the Breakpoint?
            bne bp_reset        ; If it's not a BRK, then preserve what's there
            lda BREAKPOINT+2    ; Otherwise, get the breakpoint byte and
            sta (CHARAC),y      ;   put it back 
bp_reset:   sty BREAKPOINT      ; And then clear out the whole
            sty BREAKPOINT+1    ;   breakpoint data structure
            sty BREAKPOINT+2    ;   ,,
            rts

; Breakpoint Indicator
; Also restores the breakpoint byte, temporarily
BreakInd:   ldy #$00            ; Is this a BRK instruction?
            lda (EFADDR),y      ; ,,
            bne ind_r           ; If not, do nothing
            lda BREAKPOINT      ; If it is a BRK, is it our breakpoint?
            cmp EFADDR          ; ,,
            bne ind_r           ; ,,
            lda BREAKPOINT+1    ; ,,
            cmp EFADDR+1        ; ,,
            bne ind_r           ; ,,
            jsr ReverseOn       ; Reverse on for the breakpoint
            lda BREAKPOINT+2    ; Temporarily restore the breakpoint byte
            sta (EFADDR),y      ;   for disassembly purposes
ind_r:      rts        
                 
; Enable Breakpoint
; Used after disassembly, in case the BreakInd turned the breakpoint off
EnableBP:   lda BREAKPOINT+2
            beq enable_r
            lda BREAKPOINT
            sta CHARAC
            lda BREAKPOINT+1
            sta CHARAC+1
            ldy #$00            ; Write BRK to the breakpoint
            tya                 ; ,,
            sta (CHARAC),y      ; ,,
enable_r:   rts
                                                             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MEMORY SAVE AND LOAD COMPONENTS
; https://github.com/Chysn/wAx/wiki/Memory-Save-and-Load
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MemSave:    bcc save_err        ; Bail if the address is no good
            jsr Buff2Byte       ; Convert 2 characters to a byte   
            bcc save_err        ; Fail if the byte couldn't be parsed
            sta RANGE_END+1     ; Save to the range high byte
            jsr Buff2Byte       ; Convert next 2 characters to byte
            bcc save_err        ; Fail if the byte couldn't be parsed
            sta RANGE_END       ; Save to the range low byte
            jsr FileSetup       ; SETLFS, get filename length, etc.  
            ldx #<INBUFFER+9    ; ,,
            ldy #>INBUFFER+9    ; ,,
            jsr SETNAM          ; ,,
            lda #EFADDR         ; Set up SAVE call
            ldx RANGE_END       ; ,,
            ldy RANGE_END+1     ; ,,
            jsr SAVE            ; ,,
            bcs FileError
            jmp Linefeed
save_err:   jmp SYNTAX_ERR      ; To ?SYNTAX ERROR      

; Show System Disk Error            
FileError:  bne show_error      ; Error in A will be $00 if a cassette save is
            lda #$1e            ;   stopped, so override that to ?BREAK ERROR
show_error: jmp ERROR_NO 
            
; Memory Load
MemLoad:    jsr ResetIn         ; Reset the input buffer because there's no addr
            jsr FileSetup       ; SETLFS, get filename length, etc.
            ldx #<INBUFFER+1    ; Set location of filename
            ldy #>INBUFFER+1    ; ,,
            jsr SETNAM          ; ,,
            ldx DEVICE          ; If the device numbr is 1, skip the extra
            cpx #$01            ;   OPEN and go directly to LOAD
            beq cassette        ;   ,,
            jsr OPEN
            bcs open_err
            ldx #$42
            jsr CHKIN
            bcs open_err
            jsr CHRIN
            sta X_PC
            jsr CHRIN
            sta X_PC+1
open_err:   jsr CLRCHN
            lda #$42
            jsr CLOSE       
            ldx DEVICE          ; ,,
cassette:   ldy #$01            ; ,, (load to header location)
            jsr SETLFS          ; ,,
            lda #$00            ; Command for LOAD
            jsr LOAD            
            bcs FileError
            jsr DirectMode      ; Show the loaded range if the load is done in
            beq show_range      ;   direct mode
load_r:     rts
show_range: jsr ResetOut
            jsr Linefeed
            jsr wAxPrompt
            lda #T_DIS          ; Show the loaded range, if from disk
            jsr CharOut         ; ,,
            jsr Space           ; ,,
            ldx DEVICE          ; If the device numbr is 1, skip the start/end
            cpx #$01            ;   display
            bne disk
            ldx $033d           ; Update the persistent counter with the start
            stx X_PC            ;   of memory loaded from cassette
            ldx $033e           ;   ,,
            stx X_PC+1          ;   ,,
disk:       jsr ShowPC          ; Show the persistent counter
            jsr Space           ; Space between start and end
            lda $af             ; Show the end of the loaded range
            jsr Hex             ; ,,
            lda $ae             ; ,,
            jsr Hex             ; ,,
            jmp PrintBuff       ; ,,
        
; Disk Setup
; Clear breakpoint, set up logical file, get filename length, return in A
; for call to SETNAM            
FileSetup:  jsr ClearBP         ; Clear breakpoint
            lda #$42            ; Set up logical file
            ldx DEVICE          ; ,,
            ldy #$00            ; ,,
            jsr SETLFS          ; ,,
            jsr CharGet         ; Check that the filename begins with a
            cmp #QUOTE          ;   quote. If not, treat it as a zero-length
            bne setup_r         ;   name.
            ldy #$00
-loop:      jsr CharGet
            beq setup_err
            cmp #QUOTE
            beq setup_r
            iny
            bne loop            
setup_r:    tya
            rts
setup_err:  jmp save_err
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; SEARCH COMPONENTS
; https://github.com/Chysn/wAx/wiki/Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Search:     bcc srch_r          ; Bail if the address is no good
            lda INBUFFER+4      ; Bail if there's nothing to search
            beq srch_r          ; ,,
            lda #SEARCH_L       ; Set the search limit (in pages)
            sta SEARCH_C        ; ,,
next_srch:  jsr ISCNTC          ; Keep searching code until the user presses
            beq srch_stop       ;   Stop key
            lda EFADDR+1        ; Store the effective address high byte for
            pha                 ;   later comparison
            jsr ResetOut        ; Clear output buffer for possible result
            lda INBUFFER+4      ; What kind of search is this?
            cmp #QUOTE          ; Character search
            beq MemSearch       ; ,,
            cmp #":"            ; Convert a hex search into a character search
            beq SetupHex        ; ,,
            bne CodeSearch      ; Default to code search
check_end:  pla                 ; Has the effective address high byte advanced?
            cmp EFADDR+1        ;   ,,
            beq next_srch       ; If not, continue the search
            dec SEARCH_C        ; If so, decrement the search counter, and
            bne next_srch       ;   end the search if it's done
            inc SEARCH_C        ; If the shift key is held down, keep the
            lda KEYCVTRS        ;   search going
            and #$01            ;   ,,
            bne next_srch       ;   ,,
srch_stop:  jsr ResetOut        ; Start a new output buffer to indicate the
            jsr wAxPrompt       ;   ,,
            lda #T_SRC          ;   ending search address
            jsr CharOut         ;   ,,
            jsr Space           ;   followed by a space
            jsr ShowAddr        ;   ,,
            jsr PrintBuff       ;   ,,
srch_r:     rts   

; Code Search
; Disassemble code from the effective address. If the disassembly at that
; address matches the input, indicate the starting address of the match.
CodeSearch: lda #T_DIS
            jsr CharOut
            jsr ShowAddr
            jsr Semicolon       ; Adds comment so the disassembly works
            jsr Disasm          ; Disassmble the code at the effective address
            jsr IsMatch         ; If it matches the input, show the address
            bcc check_end       ; ,,
            lda #WEDGE          ; Show prompt if there's a match
            jsr CHROUT          ; ,,
            jsr PrintBuff       ; Print address and disassembly   
            jmp check_end       ; Go back for more      

; Memory Search
; Compare a sequence of bytes in memory to the input. If there's a match,
; indicate the starting address of the match.            
MemSearch:  ldy #$00
-loop:      lda INBUFFER+5,y
            cmp (EFADDR),y
            bne no_match
            iny
            bne loop
no_match:   cmp #QUOTE          ; Is this the end of the search?
            bne next_check
            lda #":"
            jsr CharOut            
            jsr ShowAddr
            jsr PrintBuff
next_check: jsr IncAddr  
            jmp check_end
            
; Setup Hex Search
; by converting a hex search into a memory search. Transcribe hex characters
; into the input as values.       
SetupHex:   lda #QUOTE          ; Changing the start of INBUFFER to a quote
            sta INBUFFER+4      ;   turns it into a memory search
            lda #$05            ; Place the input index after the quote so
            sta IDX_IN          ;   it can get hex bytes
            ldy #$00            ; Count the number of hex bytes
-loop:      jsr Buff2Byte       ; Is it a valid hex character?
            bcc setup_done      ; If not, the transcription is done
            sta INBUFFER+5,y    ; Store the byte in the buffer
            iny
            cpy #$08
            bne loop
setup_done: lda #QUOTE
            sta INBUFFER+5,y
            jmp check_end    
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; COPY COMPONENTS
; https://github.com/Chysn/wAx/wiki/Copy-and-Fill
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copy
MemCopy:    bcc copy_err        ; Get parameters as 16-bit hex addresses for
            jsr Buff2Byte       ; Source end
            bcc copy_err        ; ,,
            sta RANGE_END+1     ; ,,
            jsr Buff2Byte       ; ,,
            bcc copy_err        ; ,,
            sta RANGE_END       ; ,,
            jsr Buff2Byte       ; Target
            bcc copy_err        ; ,,
            sta X_PC+1          ; ,,
            jsr Buff2Byte       ; ,,    
            bcc copy_err        ; ,,
            sta X_PC            ; ,,
            ldx #$00            ; Copy memory from the start address...
-loop:      lda (EFADDR,x)      ; ,,
            sta (X_PC,x)        ; ...To the target address
            lda EFADDR+1        ; ,,
            cmp RANGE_END+1     ; Have we reached the end of the copy range?
            bne advance         ; ,,
            lda EFADDR          ; ,,
            cmp RANGE_END       ; ,,
            beq copy_end        ; If so, leave the copy tool
advance:    jsr IncAddr         ; If not, advance the effective address and the
            jsr IncPC           ;   persistent counter
            jmp loop            ;   and copy the next byte
copy_end:   jmp IncPC           ; Advance persistent counter  
copy_err:   jmp SYNTAX_ERR      ; ?SYNTAX ERROR if invalid parameters
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; NUMERIC CONVERSION COMPONENTS
; https://github.com/Chysn/wAx/wiki/Numeric-Conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Hex to Base-10
Hex2Base10: jsr ResetIn         ; Reset input buffer
            jsr Buff2Byte
            bcc hex_conv_r      ; There's no byte available, so bail
            sta EFADDR+1
            jsr Buff2Byte
            sta EFADDR
            bcs two_bytes       ; There are two good bytes
            lda EFADDR+1        ; If there's only one good byte, then
            sta EFADDR          ;   treat that as a low byte, and make the
            lda #$00            ;   high byte zero
            sta EFADDR+1        ;   ,,
two_bytes:  lda #WEDGE
            jsr CHROUT
            lda #"#"
            jsr CHROUT
            ldx EFADDR          ; Set up PRTFIX for base-10 integer output
            lda EFADDR+1        ; ,,
            jsr PRTFIX          ; ,,
            jsr Linefeed
hex_conv_r: rts            
            
; Base-10 to Hex
Base102Hex: jsr ResetOut
            jsr wAxPrompt
            jsr HexPrefix
            ldy #<INBUFFER
            lda #>INBUFFER
            sty $7a
            sta $7b
            jsr CHRGOT
            jsr ASCFLT
            jsr MAKADR
            lda SYS_DEST+1
            beq only_low
            jsr Hex
only_low:   lda SYS_DEST
            jsr Hex
b102h_r:    jmp PrintBuff
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; SYMBOLIC ASSEMBLER COMPONENTS
; https://github.com/Chysn/wAx/wiki/Symbol-Table-Manager
; https://github.com/Chysn/wAx/wiki/Labels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Set Persistent Counter
SetPC:      lda #0              ; Reset forward reference overflow counter
            sta OVERFLOW_F      ; ,,
            bcc setpc_r         ; Do only that if no address provided
EAtoPC:     lda EFADDR          ; Move effective address to persistent counter
            sta X_PC            ; ,,
            lda EFADDR+1        ; ,,
            sta X_PC+1          ; ,,
setpc_r:    rts                 ; ,,
            
; Assign or Initialize Labels
Labels:     lda INBUFFER        
            beq LabelList       ; If the tool is alone, show the symbol table
            jsr ResetIn
            jsr CharGet
            cmp #"-"            ; If - follows the tool, initialize the symbol
            beq init_clear      ;   table
            jsr SymbolIdx       ; Is this a valid symbol, and is there memory
            bcc label_err       ;   to assign it?
            jsr Buff2Byte
            bcc label_err
            sta SYMBOL_AH,y
            jsr Buff2Byte
            bcc label_err
            sta SYMBOL_AL,y
            rts            
label_err:  jmp SymError        
init_clear: lda #$00            ; Initialize bytes for the symbol table
            ldy #ST_SIZE-1      ;   See the Symbol Table section at the top for
-loop:      sta SYMBOL_D,y      ;   information about resizing or relocating the
            dey                 ;   symbol table
            bpl loop            ;   ,,
init_r:     rts
            
; Get Symbol Index
; Return symbol index in Y and set Carry
; Error (all symbols used, or bad symbol) if Carry clear
SymbolIdx:  cmp #"."
            bne sym_range
            ldy #MAX_LAB-1
            lda #"."+$80
            pha
            lda IDX_IN
            cmp #5
            bcc sym_found
            lda #0
            sta SYMBOL_AL,y
            sta SYMBOL_AH,y
            jmp sym_found
sym_range:  cmp #"@"            ; Allowed label names are 0-9, A-Z, and @
            beq good_label      ; ,,
            cmp #"0"            ; ,,
            bcc bad_label       ; ,,
            cmp #"Z"+1          ; ,,
            bcs bad_label       ; ,,
            cmp #"9"+1          ; ,,
            bcc good_label      ; ,,
            cmp #"A"            ; ,,
            bcs good_label      ; ,,
bad_label:  clc
            rts      
good_label: ora #$80            ; High bit set indicates symbol is defined
            pha
            ldy #MAX_LAB-2      ; See if the label is already in the table
-loop:      cmp SYMBOL_D,y      ; ,,
            beq sym_found       ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            ldy #MAX_LAB-2      ; If the symbol isn't already in use, look for
-loop:      lda SYMBOL_D,y      ;   an empty record
            beq sym_found       ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
            pla                 ; No empty symbol is found; all symbols are in        
            jmp bad_label       ;   use. Return for error
sym_found:  pla
            sta SYMBOL_D,y      ; Populate the symbol label table with the name
            sec                 ; Set Carry flag indicates success
            rts            
            
; Show Label List           
LabelList:  ldx #$00
-loop:      txa                 ; Save the iterator from PrintBuff, etc.
            pha                 ; ,,
            tay                 ; ,,
            lda SYMBOL_AL,y     ; Stash the current value in EFADDR
            sta EFADDR          ; ,,
            lda SYMBOL_AH,y     ; ,,
            sta EFADDR+1        ; ,,
            lda EFADDR          ; If this symbol is undefined (meaning, it is
            bne show_label      ;   $0000, then skip it)
            lda EFADDR+1        ;   ,,
            beq undefd          ; Undefined, but it might be a forward reference
show_label: jsr LabListCo       ; Add elements common to both listed item
            jsr ShowAddr
            jsr PrintBuff
next_label: pla
            tax
            inx
            cpx #MAX_LAB
            bne loop
            jsr ResetOut        ; Show the value of the persistent counter
            jsr Space           ; ,,
            jsr Space           ; ,,
            lda #"*"            ; ,,
            jsr CharOut         ; ,,
            jsr Space           ; ,,
            jsr ShowPC          ; ,,
            lda OVERFLOW_F      ; Show the overflow forward reference count
            beq lablist_r       ;   (if any)
            pha                 ;   ,,
            jsr Space           ;   ,,
            jsr ReverseOn       ;   ,,
            jsr GT              ;   ,,
            pla                 ;   ,,
            jsr Hex             ;   ,,
lablist_r:  jsr PrintBuff       ;   ,,
            rts
undefd:     stx IDX_SYM
            ldy #$00            ; Forward reference count for this label
            ldx #$00            ; Forward record index
-loop:      lda SYMBOL_F,x
            bpl next_undef
            and #$3f
            cmp IDX_SYM
            bne next_undef
            iny
next_undef: inx
            cpx #MAX_FWD
            bne loop
            cpy #$00
            beq next_label
show_fwd:   tya
            pha
            ldx IDX_SYM
            jsr LabListCo
            jsr ReverseOn
            jsr GT
            pla
            jsr Hex
fwd_d:      jsr PrintBuff
            jmp next_label

; Label List Common            
LabListCo:  jsr ResetOut
            jsr Space
            lda #LABEL
            jsr CharOut
            lda SYMBOL_D,x
            and #$7f
            jsr CharOut
            jmp Space
            
; Symbol is Defined
; Zero flag is clear if symbol is defined
IsDefined:  lda SYMBOL_AL,y
            bne is_defined
            lda SYMBOL_AH,y
is_defined: rts             

; Expand Symbol
; and return to Transcribe
ExpandSym:  sty IDX_SYM
            jsr ResetOut
            jsr HexPrefix
            lda BYTE_MOD        ; If < has been specified, then insert only
            cmp #LOW_BYTE       ;   the low byte of the address
            beq insert_lo       ;   ,,
            lda SYMBOL_AH,y     ; If > or no modifier has been specified, then
            jsr Hex             ;   insert the high byte of the address
            lda BYTE_MOD        ;   ,,
            cmp #HIGH_BYTE      ; If > has been specified, then skip the low
            beq do_expand       ;   byte
insert_lo:  lda SYMBOL_AL,y
            jsr Hex            
do_expand:  lda #$00            ; Add delimiter, since the hex operand can
            jsr CharOut         ;   vary in length
            ldy #$00            ; Transcribe symbol expansion into the
-loop:      lda OUTBUFFER,y     ;   input buffer
            beq expand_r        ;   ,,
            jsr AddInput        ;   ,,
            iny                 ;   ,,
            jmp loop            ;   ,,            
expand_r:   jmp Transcribe        
            
; Resolve Forward Reference            
ResolveFwd: lda IDX_SYM
            ora #$80            ; Set high bit, which is what we look for here
            ldx #$00            ; First order of business is finding unresolved
-loop:      cmp SYMBOL_F,x      ;   records that match the label
            beq fwd_used
            ora #$40            ; Also check for high-byte specifier
            cmp SYMBOL_F,x
            beq fwd_used
            and #%10111111      ; Mask away high-byte specifier for next check
            inx
            cpx #MAX_FWD
            bne loop
            rts                 ; Label not found in forward reference table
fwd_used:   lda SYMBOL_FL,x     ; A forward reference for this label has been
            sta CHARAC          ;   found; store the address in zero page for
            lda SYMBOL_FH,x     ;   updating the code at this address.
            sta CHARAC+1        ;   ,,
            ldy #$00            ; Get the byte at the reference address, which
            lda (CHARAC),y      ;   should be an instruction opcode
            jsr Lookup          ; Look it up
            bcs get_admode      ; ,,
            jmp CannotRes       ; Not a valid instruction; CAN'T RESOLVE ERROR
get_admode: cmp #RELATIVE       ; If it's a relative branch instruction,
            beq load_rel        ;   calculate the branch offset
            cmp #ABSOLUTE       ; Two bytes will be replaced, so make sure
            beq load_abs        ;   this instruction is one of the
            cmp #ABSOLUTE_X     ;   absolute addressing modes, or indirect mode
            beq load_abs        ;   ,,
            cmp #ABSOLUTE_Y     ;   ,,
            beq load_abs        ;   ,,
            cmp #INDIRECT       ;   ,,
            beq load_abs        ;   ,,
            cmp #IMPLIED        ; If an implied mode instruction is somehow
            bne load_immed      ;   being resolved, throw ASSEMBLY ERROR
            jmp CannotRes       ;   ,,
load_abs:   lda EFADDR          ; For an absolute mode instruction, just
            ldy #$01            ;   transfer the two bytes over
            sta (CHARAC),y      ;   ,,
            lda EFADDR+1        ;   ,,
            iny                 ;   ,,
            sta (CHARAC),y      ;   ,,
            jmp clear_back      ; Go back and see if there are more to resolve
load_rel:   lda EFADDR          ; The target is the current effective address
            sec                 ; Subtract the reference address and add
            sbc CHARAC          ;   two to get the offset
            sec                 ;   ,,
            sbc #$02            ;   ,,
            ldy #$01            ; Store the computed offset in the forward
            sta (CHARAC),y      ;   reference operand address
            jmp clear_back      ; Go back and see if there are more to resolve
load_immed: lda #$40            ; Check bit 6 of the label byte of the forward
            and SYMBOL_F,x      ;   reference symbol record. If it's set, it
            beq load_low        ;   means that the user wants the high byte
            lda EFADDR+1        ;   of the symbol target
            .byte $3c           ; Skip word
load_low:   lda EFADDR          ; For other instructions
            ldy #$01
            sta (CHARAC),y
clear_back: lda #$00            ; Clear the forward reference table record for
            sta SYMBOL_F,x      ;   re-use, and go back for additional
            jmp ResolveFwd      ;   forward references
            
; Add Forward Record
; For label in Y            
; Each forward reference record consists of three bytes-
; Offset 0 - Label Index OR %10000000
AddFwdRec:  ldx #$00            ; Search the forward symbol table for a
-loop:      lda SYMBOL_FL,x     ;   record in use with the same address.
            cmp EFADDR          ;   If such a record is found, re-use it
            bne next_used       ;   rather than searching the empty records
            lda SYMBOL_FH,x     ;   ,,
            cmp EFADDR+1        ;   ,,
            beq empty_rec       ;   ,,
next_used:  inx                 ;   ,,
            cpx #MAX_FWD        ;   ,,
            bne loop            ;   ,,
find_empty: ldx #$00            ; Now, search ALL the records, this time looking
-loop:      lda SYMBOL_F,x      ;   for an unused record.
            beq empty_rec       ; This is an empty record, so use it
            inx
            cpx #MAX_FWD        ; Check the limit of forward reference records
            bne loop
overflow:   inc OVERFLOW_F      ; Increment overflow counter if no records are
            beq overflow        ;   left; if it rolls to 0, set it to 1 instead
            jsr DirectMode      ; If the overflow happens in direct mode, show
            bne addfwd_r        ;   the Symbol Error. In BASIC, this condition
            jmp SymError        ;   can be caught, so keep going for multi-pass
empty_rec:  tya
            ora #$80            ; Set the high bit to indicate record in use
            ldy BYTE_MOD        ; If the label was prefixed with >, then mark
            cpy #HIGH_BYTE      ;   this forward record to use the high byte
            bne store_rec       ;   on resolution, by setting bit 6 of the
            ora #$40            ;   symbol forward label entry
store_rec:  sta SYMBOL_F,x      ; Store the label index in the record
            lda EFADDR          ; Store the current effective address in the
            sta SYMBOL_FL,x     ;   forward reference record for (hopefully)
            lda EFADDR+1        ;   later resolution
            sta SYMBOL_FH,x     ;   ,,
addfwd_r:   rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; BASIC STAGE SELECT COMPONENT
; https://github.com/Chysn/wAx/wiki/Change-BASIC-Stage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
BASICStage: jsr ResetIn         ; Reset the input buffer index
            sta EFADDR          ; Set default end page
            jsr Buff2Byte       ; Get the first hex byte
            bcc st_range        ; If no valid address was provided, show range
            sta EFADDR+1        ; This is the stage's starting page number
            jsr Buff2Byte       ; But the default can be overridden if a valid
            bcc ch_length       ;   starting page is provided
            sta EFADDR          ;   ,,
ch_length:  lda EFADDR+1        ; Make sure that the ending page isn't lower
            cmp EFADDR          ;   in memory than the starting page. If it is,
            bcc set_ptrs        ;   default the stage size to 3.5K
            clc                 ;   ,,
            adc #$0e            ;   ,,
            sta EFADDR          ;   ,,
set_ptrs:   lda EFADDR+1        ; Set up the BASIC start and end pointers
            sta $2c             ;   and stuff
            sta $2e             ;   ,,
            sta $30             ;   ,,
            sta $32             ;   ,,
            lda #$01            ;   ,,
            sta $2b             ;   ,,
            lda #$03            ;   ,,
            sta $2d             ;   ,,
            sta $2f             ;   ,,
            sta $31             ;   ,,
            lda #$00            ;   ,,
            sta $33             ;   ,,
            sta $37             ;   ,,
            lda EFADDR          ;   ,,
            sta $34             ;   ,,
            sta $38             ;   ,,
            ldy #$00            ; Clear the low byte. From here on out, we're     
            sty EFADDR          ;   dealing with the start of the BASIC stage
            ldy #$00            ; Look through the input buffer for an "N"
-loop:      lda INBUFFER,y      ;   character. This indicates that it is a
            beq finish          ;   new stage.
            cmp #"N"            ;   ,,
            beq new             ;   ,,
            iny
            cpy #$16            ; If we reach the end without seeing an "N",
            bne loop            ;   just rechain the area as if it were a BASIC
            beq finish          ;   program
new:        lda #$00            ; Zero out the first few bytes of the stage so
            ldy #$02            ;   that it looks like a NEW program. I'm not
-loop:      sta (EFADDR),y      ;   using BASIC's NEW at $c642 because it does
            dey                 ;   not store $00 at the page boundary, which
            bpl loop            ;   causes problems.
finish:     jsr Rechain
            jmp (READY)
st_range:   jsr ResetOut        ; Show the start and end pages of the current
            jsr wAxPrompt       ;   BASIC stage
            lda #$5e            ;   ,,
            jsr CharOut         ;   ,,
            jsr Space           ;   ,,
            lda $2c             ;   ,,
            jsr Hex             ;   ,,
            jsr Space           ;   ,,
            lda $34             ;   ,,
            jsr Hex             ;   ,,
            jmp PrintBuff       ;   ,,
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE LISTING COMPONENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
QUOTE_FL    = $0247             ; Quote flag for filename
FIRST_REC   = $0248             ; First record flag

Directory:  jsr CLALL           ; Close all files
            lsr FIRST_REC       ; Clear first record flag
            lda #1              ; SETNAM - (1) Set name length
            ldx #<lfs+1         ; - Set name as the $ used below
            ldy #>lfs+1         ; ,,
            jsr SETNAM          ; ,,
lfs:        lda #"$"            ; SETLFS - Set file number as $
            ldx DEVICE          ; - Device number
            ldy #0              ; - Command
            jsr SETLFS          ; ,,
            jsr OPEN
            bcs dFail           ; Fail if unable to open
            ldx #"$"
            jsr CHKIN
            jsr CharIn          ; Dispose of PRG header
            jsr CharIn          ; ,,
newline:    jsr ISCNTC          ; Exit if STOP key is pressed
            beq EOF             ;   end listing if it is      
            jsr ResetOut        ; Reset output buffer
            lda #0              ; Set quote flag to 0. Needs to be 0 because
            sta QUOTE_FL        ;   this flag has three possible states
            ldx #4              ; Dispose of next line pointer and
-loop:      jsr CharIn          ;   line number
            dex                 ;   ,,
            bne loop            ;   ,,
-loop:      jsr CharIn          ; Get next character from line
            beq EOL             ; 0 indicates end-of-line
            cmp #$22            ; Is character a quotation mark?
            bne proc_name       ; If not, go process the name
            lda QUOTE_FL        ; If it's the first quote in this line, add
            bne set_quote       ;   the load tool and the starting quote mark
            jsr wAxPrompt       ;   to the output buffer
            lda #T_LOA          ;   ,,
            jsr CharOut         ;   ,,
            lda #$22            ;   ,,
            jsr CharOut         ;   ,,
set_quote:  sec                 ; Set the quote flag to either %10000000 or
            ror QUOTE_FL        ;   %11000000
            bcc loop            ; Go back for next character
proc_name:  bit QUOTE_FL        ; Check the quote state
            bvs loop            ; If bit 6 is set, the quote is finished
            bpl loop            ; If bit 7 is clear, the quote hasn't started
            jsr CharOut         ; If quote has started but not finished, it's
            jmp loop            ;   part of a name
EOL:        bit QUOTE_FL        ; If a quote hasn't been finished, end of dir
            bvc EOF             ; ,,
            lda #$22            ; Add the ending quote mark and CR to the buffer
            jsr CharOut         ;   ,,
            bit FIRST_REC       ; Is this the first record?
            bpl subseq          ; If so, skip the display
            jsr Match           ; Perform a text match, if necessary
            bcc subseq          ; ,,
            jsr PrintBuff       ; Flush buffer to screen
subseq:     sec                 ; Set the flag to display subsequent lines
            ror FIRST_REC       ; ,,
            jmp newline
EOF:        jsr CLRCHN          ; Clear input channel and close file when
            lda #"$"            ;   a quote pair isn't found
            jsr CLOSE           ;   ,,
            jmp ResetOut        ; Clear output buffer and return    

; Fail with BASIC error message            
dFail:      lda #5              ; ?DEVICE NOT PRESENT
            jmp ERROR_NO        ; Display error and warm start     
                        
; Character Input
; With tests for failure            
CharIn:     jsr CHRIN
            pha
            lda IOSTATUS
            bne dFail
            pla
            rts
            
; Match Search
; If a quoted sting in the input buffer matches part of the filename in
; the output buffer, return with carry set. If there's no match, return
; with carry clear.
Match:      lda INBUFFER        ; Is the input buffer a quoted string?
            cmp #$22            ; ,,
            bne mfound          ; If not, match is OK
            ldx #2              ; Start of haystack (output buffer)
-next       ldy #1              ; Start of needle (input buffer)
-loop:      lda INBUFFER,y      ; Get the input buffer character
            cmp OUTBUFFER,x     ; Compare to output buffer character
            bne adv_out         ; If no match, advance output buffer, start over
            inx                 ; On match, increment both indices and check
            iny                 ;   for the next match
            lda INBUFFER,y      ; Is the string ending with no final quote?
            beq derror          ;   If so, it's a syntax error
            cmp #$22            ; If the string ends with a quote after a match,
            bne loop            ;   the match is successful
mfound:     sec                 ; Return with carry set on match
            rts                 ; ,,
adv_out:    inx                 ; Increment the output buffer index and begin
            lda OUTBUFFER,x     ;   another search, unless we've reached the
            beq unfound         ;   end of the input buffer
            cmp #$22            ;   ,,
            bne next            ;   ,,
unfound:    clc                 ; Return with carry clear if no match
            rts                 ; ,,
derror:     jmp SYNTAX_ERR              

; Rechain BASIC program
Rechain:    jsr $c533           ; Re-chain BASIC program to set BASIC
            lda $22             ;   pointers as a courtesy to the user
            ;clc                ;   ,, ($c533 always exits with Carry clear)
            adc #$02            ;   ,,
            sta $2d             ;   ,,
            lda $23             ;   ,,
            jmp $c655           ;   ,,
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TEXT DISPLAY COMPONENT (INTERPRET)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TextDisp:   lda #$22
            jsr CharOut
            ldy #$00
-loop:      jsr IncAddr
            cmp #$a0            ; Everything from 160 on is allowed in the
            bcs tadd_char       ;   display unchaged
            cmp #$80            ; Change everything between 128 and 159 
            bcs talter          ; ,,
            cmp #$20            ; Show everything else at and above space
            bcs tadd_char       ; ,,
talter:     lda #" "            ; Everything else gets a space
tadd_char:  jsr CharOut         ; ,,
            iny
            cpy #$0c
            bne loop
            lda #$22
            jsr CharOut
            rts 
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; COMPARE COMPONENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compare tool workspace
STATUS      = $0247             ; $00 = Non-Matching, $80 = Matching
COUNT       = $0248             ; Count of unmatched/matched bytes so far

Compare:    bcc cerror          ; Error if the first address is no good
            jsr Buff2Byte       ; Get high byte of range end
            bcc cerror          ; ,,
            sta RANGE_END+1     ; ,,
            jsr Buff2Byte       ; Get low byte of range end
            bcc cerror          ; ,,
            sta RANGE_END       ; ,,
            jsr Buff2Byte       ; Get high byte of compare start
            bcc cerror          ; ,,
            sta X_PC+1          ; ,,
            jsr Buff2Byte       ; Get low byte of compare start
            bcc cerror          ; ,,
            sta X_PC            ; ,,
            lda #$00            ; Reset status and counter
            sta STATUS          ; ,,
            sta COUNT           ; ,,
            sta COUNT+1         ; ,,
            bcs Start           ; Setup OK, do compare
cerror:     jmp $cf08           ; ?SYNTAX ERROR, warm start

; Start comparison
Start:      jsr StartLine       ; Start address line
            inc RANGE_END       ; Increase the range by 1 for
            bne compare         ;   the comparison
            inc RANGE_END+1     ;   ,,
compare:    lda EFADDR+1        ; Is the effective address in 
            cmp RANGE_END+1     ;   the compare range?
            bcc in_range        ;   ,,
            lda EFADDR          ;   ,,
            cmp RANGE_END       ;   ,,
            bcc in_range        ; If so, check for byte match
done:       jsr Toggle          ; Toggle to show the right color
            jsr EndLine         ; Show the last result count
            rts                 ; Done!
in_range:   ldx #$00            ; Compare EA to PC
            lda (EFADDR,x)      ; ,,
            cmp (X_PC,x)        ; ,,
            bne differs
matches:    lda STATUS          ; If the byte matches, and the previous byte
            beq show_last       ;   differed, then toggle the status and reset
            bne cnext
differs:    lda STATUS          ; If the byte differs, and the previous byte
            bne show_last       ;   matched, then toggle the status and reset
            beq cnext
show_last:  jsr Toggle            
            jsr EndLine         ; Show the count
            lda #$00            ; Reset the counter
            sta COUNT           ; ,,
            sta COUNT+1         ; ,,
            jsr StartLine       ; Start a new line
cnext:      inc COUNT           ; Increment the count of match/unmatch
            bne inc_mem         ; ,,
            inc COUNT+1         ; ,,
inc_mem:    jsr IncPC           ; Increment the comparison counters
            jsr IncAddr         ; ,,
            jsr $ffe1           ; Check STOP key
            beq done            ; End the comparison if STOP
            jmp compare         ; Do the next comparison

Toggle:     lda STATUS
            bne toggle_off
            lda #$80
            .byte $34           ; DOP (skip byte)
toggle_off: asl
            sta STATUS
            rts

; Start Line
; Reset the output buffer, and add the addresses
StartLine:  jsr ResetOut
            lda #"$"
            jsr CharOut
            jsr ShowAddr
            lda #","
            jsr CharOut
            jsr ShowPC
            lda #":"
            jsr CharOut
            rts

; End Line
; Complete the line by showing the count in red (differences) or
; green (matches)            
EndLine:    lda COUNT           ; If the count is zero, don't
            bne report          ;   finish the buffer, because it's
            lda COUNT+1         ;   probably the first group of bytes
            bne report          ;   ,,
            rts                 ;   ,,
report:     lda $0286           ; Store current color
            pha                 ; ,,
            lda STATUS
            beq green
red:        lda #$1c            ; Red
            .byte $3c           ; TOP (skip word)
green:      lda #$1e            ; Green
            jsr CharOut
            lda COUNT+1         ; Show number of matches/no matches
            jsr Hex             ;   before the change
            lda COUNT           ;   ,,
            jsr Hex             ;   ,,
            jsr PrintBuff       ;   ,,
            pla                 ; Restore original color
            sta $0286           ; ,,
            rts
                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; HELP COMPONENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Help:       lda #<HelpScr1      ; Print help screen 1. It's broken into pieces
            ldy #>HelpScr1      ;   because it's longer than 256 bytes
            jsr PrintStr        ;   ,,
            lda #<HelpScr2      ; Print help screen 2
            ldy #>HelpScr2      ; ,,
            jmp PrintStr        ; ,,  
                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; USER PLUG-IN
; https://github.com/Chysn/wAx/wiki/User-Plug-In
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PlugIn:     jmp (USER_VECT)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PLUG-IN MENU COMPONENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CurChar     = $0247

PlugMenu:   jsr ResetIn         ; Reset in to get just a single hex byte
            jsr CharGet         ; Get two characters from input
            sta CurChar         ; ,,
            jsr CharGet         ; ,,
            sta CurChar+1       ; ,,
            ldy #6              ; Y = last menu index
-loop       lda MenuChar1,y     ; Check first character
            cmp CurChar         ; ,,
            bne mnext           ; ,,
            lda MenuChar2,y     ; If it's found, check second character
            cmp CurChar+1       ; ,,
            beq cfound          ; Both match, so the menu item is found
mnext:      dey                 ; Iterate
            bpl loop            ; ,,
ShowMenu:   lda #<MenuText      ; The string wasn't found, so show menu
            ldy #>MenuText      ; ,,
            jmp PrintStr        ; ,,
cfound:     lda MenuLoc_L,y     ; Found item, so set plug-in vector based on
            sta $05             ;   looked up address
            lda MenuLoc_H,y     ;   ,,
            sta $06             ;   ,,
-loop:      ldx #0              ; Get character at screen position
            lda ($d1,x)         ; ,,
            cmp #WEDGE          ; Is it a . character?
            bne show_use:       ; If not, done positioning cursor, show usage
            lda #$11            ; Drop down one line
            jsr $ffd2           ; ,,
            jmp loop            ; And look again
show_use:   tya                 ; Show "USAGE"
            pha                 ; ,, Preserve found plug-in index
            lda #<UsageTxt      ; ,,
            ldy #>UsageTxt      ; ,,
            jsr PrintStr        ; ,,
            pla                 ; ,,
            tay                 ; ,,
            lda UsageLoc_L,y    ; Show usage instructions
            pha                 ; ,,
            lda UsageLoc_H,y    ; ,,
            tay                 ; ,,
            pla                 ; ,,
            jmp PrintStr        ; ,,
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Look up opcode
; Reset Language Table            
Lookup:     sta OPCODE          ; OPCODE is the found opcode
            jsr ResetLang       ; Reset the language table reference
-loop:      jsr NextInst        ; Get the next 6502 instruction in the table
            ldy TOOL_CHR        ; If the tool is the extended disassembly,
            cpy #T_XDI          ;   use the end of the extended table,
            bne std_table       ;   otherwise, use the standard 6502 table
            cmp #XTABLE_END     ; If we've reached the end of the table,
            .byte $3c           ; TOP (skip word)
std_table:  cmp #TABLE_END            
            beq not_found       ;   then the instruction is invalid
            cmp OPCODE          ; If the instruction doesn't match the opcode,
            bne loop            ;   keep searching.
found:      ldy #$01
            lda (LANG_PTR),y    ; A match was found! Set the addressing mode
            sec                 ;   and set the carry flag to indicate success
            rts
not_found:  clc                 ; Reached the end of the language table without
            rts                 ;   finding a matching instruction
                                    
; Reset Language Table            
ResetLang:  lda #<InstrSet-2    ; Start two bytes before the Instruction Set
            sta LANG_PTR        ;   table, because advancing the table will be
            lda #>InstrSet-2    ;   an early thing we do
            sta LANG_PTR+1      ;   ,,
            rts
            
; Next Instruction in Language Table
; Handle mnemonics by recording the last found mnemonic and then advancing
; to the following instruction. The opcode is returned in A.
NextInst:   lda #$02            ; Each language entry is two bytes. Advance to
            clc                 ;   the next entry in the table
            adc LANG_PTR        ;   ,,
            sta LANG_PTR        ;   ,,
            bcc ch_mnem         ;   ,,
            inc LANG_PTR+1      ;   ,,
ch_mnem:    ldy #$01            ; Is this entry an instruction record?
            lda (LANG_PTR),y    ; ,,
            and #$01            ; ,,
            beq adv_lang_r      ; If it's an instruction, return
            lda (LANG_PTR),y    ; Otherwise, set the mnemonic in the workspace
            sta MNEM+1          ;   as two bytes, five bits per character for
            dey                 ;   three characters. See the 6502 table for
            lda (LANG_PTR),y    ;   a description of the data encoding
            sta MNEM            ;   ,,
            jmp NextInst        ; Go to what should now be an instruction
adv_lang_r: ldy #$00            ; When an instruction is found, set A to its
            lda (LANG_PTR),y    ;   opcode and return
            rts
            
; Get Character
; Akin to CHRGET, but scans the INBUFFER, which has already been detokenized            
CharGet:    ldx IDX_IN
            lda INBUFFER,x
            php
            inc IDX_IN
            plp
            rts             
            
; Buffer to Byte
; Get two characters from the buffer and evaluate them as a hex byte
Buff2Byte:  jsr CharGet
            jsr Char2Nyb
            bcc buff2_r         ; Return with Carry clear if invalid
            asl                 ; Multiply high nybble by 16
            asl                 ;   ,,
            asl                 ;   ,,
            asl                 ;   ,,
            sta WORK
            jsr CharGet
            jsr Char2Nyb
            bcc buff2_r         ; Clear Carry flag indicates invalid hex
            ora WORK            ; Combine high and low nybbles
            ;sec                ; Set Carry flag indicates success
buff2_r:    rts
       
; Is Buffer Match            
; Does the input buffer match the output buffer?
; Carry is set if there's a match, clear if not
IsMatch:    ldy #$06            ; Offset for character after address
-loop:      lda OUTBUFFER,y     ; Compare the assembly with the disassembly
            cmp INBUFFER-2,y    ;   in the buffers
            bne not_found       ; See Lookup subroutine above
            iny
            cpy IDX_OUT
            bne loop            ; Loop until the buffer is done
            sec                 ; This matches; set carry
            rts

; Character to Nybble
; A is the character in the text buffer to be converted into a nybble
Char2Nyb:   cmp #"9"+1          ; Is the character in range 0-9?
            bcs not_digit       ; ,,
            cmp #"0"            ; ,,
            bcc not_digit       ; ,,
            sbc #"0"            ; If so, nybble value is 0-9
            rts
not_digit:  cmp #"F"+1          ; Is the character in the range A-F?
            bcs not_found       ; See Lookup subroutine above
            cmp #"A"         
            bcc not_found       ; See Lookup subroutine above
            sbc #"A"-$0a        ; The nybble value is 10-15
            rts
            
; Increment Effective Address
; Get the EA byte and advance EA by one
IncAddr:    ldx #$00
            lda (EFADDR,x)
            inc EFADDR
            bne next_r
            inc EFADDR+1
next_r:     rts

; Incremenet Persistent Counter
IncPC:      inc X_PC 
            bne pc_r 
            inc X_PC+1
pc_r:       rts 

; Commonly-Used Characters
Semicolon:  lda #";"            
            .byte $3c           ; TOP (skip word)
GT:         lda #">"
            .byte $3c           ; TOP (skip word)
ReverseOn:  lda #RVS_ON
            .byte $3c           ; TOP (skip word)
CloseParen: lda #")"
            .byte $3c           ; TOP (skip word)
Comma:      lda #","
            .byte $3c           ; TOP (skip word)
Space:      lda #" "
            .byte $3c           ; TOP (skip word)
HexPrefix:  lda #"$"
            .byte $3c           ; TOP (skip word)
wAxPrompt:  lda #WEDGE
            ; Fall through to CharOut
            
; Character to Output
; Add the character in A to the outut byffer            
CharOut:    sta CHARAC          ; Save temporary character
            tya                 ; Save registers
            pha                 ; ,,
            txa                 ; ,,
            pha                 ; ,,
            ldx IDX_OUT         ; Write to the next OUTBUFFER location
            lda CHARAC          ; ,,
            sta OUTBUFFER,x     ; ,,
            inc IDX_OUT         ; ,,
            pla                 ; Restore registers
            tax                 ; ,,
            pla                 ; ,,
            tay                 ; ,,
write_r:    rts             
            
; Write hexadecimal character
Hex:        pha                 ; Hex converter based on from WOZ Monitor,
            lsr                 ;   Steve Wozniak, 1976
            lsr
            lsr
            lsr
            jsr prhex
            pla
prhex:      and #$0f
            ora #"0"
            cmp #"9"+1
            bcc echo
            adc #$06
echo:       jmp CharOut

; Get Binary Byte
; Return in A     
BinaryByte: lda #$00
            sta TEMP_CALC
            lda #%10000000
-loop:      pha
            jsr CharGet
            cmp #"1"
            bne zero
            pla
            pha
            ora TEMP_CALC
            sta TEMP_CALC
            jmp next_bit
zero:       cmp #"0"
            bne bad_bin
next_bit:   pla
            lsr
            bne loop
            lda TEMP_CALC
            sec
            rts
bad_bin:    jmp AsmError
 
; Show Effective Address
; 16-bit hex address at effective address          
ShowAddr:   lda EFADDR+1
            jsr Hex
            lda EFADDR
            jmp Hex 

; Show Persistent Counter
; 16-bit hex address at persistent counter address          
ShowPC:     lda X_PC+1
            jsr Hex
            lda X_PC
            jmp Hex
            
; Show 8-bit Parameter           
Param_8:    jsr HexPrefix
            jsr IncAddr   
            jmp Hex            
            
; Show 16-Bit Parameter            
Param_16:   jsr HexPrefix
            jsr IncAddr   
            pha
            jsr IncAddr   
            jsr Hex
            pla
            jmp Hex

; Expand External Program Counter
; Replace asterisk with the X_PC
ExpandXPC:  jsr ResetOut
            jsr ShowPC
            ldy #$00
-loop:      lda OUTBUFFER,y
            jsr AddInput
            iny
            cpy #$04
            bne loop
            ; Fall through to Transcribe
            
; Transcribe to Buffer
; Get a character from the BASIC input buffer and transcribe it to the
; wAx input buffer. If the character is a BASIC token, then possibly
; explode it into individual characters.
Transcribe: lda #0              ; Clear the high or low byte modifier
            sta BYTE_MOD        ; ,,
post_mx:    jsr CHRGET          ; Get character from input buffer
            cmp #$00            ; If it's 0, then quit transcribing and return
            beq xscribe_r       ; ,,
            cmp #$ac            ; Replace an asterisk with the persistent
            beq ExpandXPC       ;   counter
            cmp #";"            ; If it's a comment, then quit transcribing
            beq comment         ;   unless we're in quote mode
            cmp #HIGH_BYTE      ; If it's > or <, it modifies the next label
            bne ch_low          ; ,,
            sta BYTE_MOD        ; ,,
            jmp post_mx         ; ,, Get more, but don't clear the modifier
ch_low:     cmp #LOW_BYTE       ; ,,
            bne ch_label        ; ,,
            sta BYTE_MOD        ; ,,
            jmp post_mx         ; ,, Get more, but don't clear the modifier
ch_label:   cmp #LABEL          ; Handle symbolic labels
            beq handle_sym      ; ,,
            cmp #QUOTE          ; If a quote is found, modify CHRGET so that
            bne ch_token        ;   spaces are no longer filtered out
            lda #$06            ; $0082 BEQ $0073 -> BEQ $008a
            sta $83             ; ,,
            lda #QUOTE          ; Put quote back so it can be added to buffer
ch_token:   cmp #$80            ; Is the character in A a BASIC token?
            bcc x_add           ; If it's not a token, just add it to buffer
            ldy $83             ; If it's a token, check the CHRGET routine
            cpy #$06            ;  and skip detokenization if it's been
            beq x_add           ;  modified.
            jsr Detokenize      ; Detokenize and continue transciption
            jmp Transcribe      ; ,,
x_add:      jsr AddInput        ; Add the text to the buffer and get more
            jmp Transcribe      ; ,,
xscribe_r:  jmp AddInput        ; Add the final zero, and fix CHRGET...
handle_sym: ldy $83             ; Don't handle symbols if the label is in quotes
            cpy #$06            ;   (as in an immediate operand, or text entry)
            beq x_add           ;   ,,
            ldy IDX_IN          ; If the label character occurs deep in the
            cpy #$0d            ;   input, don't handle a symbol. It's in the
            bcs x_add           ;   memory dump display.            
            lda IDX_IN          ; If . is the first character in the input
            cmp #$06            ;   buffer after the address, defer the
            bcs start_exp       ;   symbol for handling by the assembler
            lda #LABEL          ;   ,,
            jsr AddInput        ;   ,,
            jmp Transcribe      ;   ,,
start_exp:  jsr CHRGET          ; Get the next character, the label
            jsr SymbolIdx       ; Get the symbol index
            bcs get_label
            jmp SymError        ; If not, ?SYMBOL ERROR
get_label:  jsr IsDefined
            bne go_expand
            lda IDX_IN          ; The symbol has not yet been defined; parse
            pha                 ;   the first hex numbers to set the program
            jsr RefreshPC       ;   counter, then return the input index to
            pla                 ;   its original position
            sta IDX_IN          ;   ,,
            jsr AddFwdRec       ; Add forward reference record for label Y
            inc IGNORE_RB       ; Set relative branch ignore flag
go_expand:  jmp ExpandSym       ; Use $0000 as a placeholder
comment:    ldy $83
            cpy #$06
            beq add_only
            lda #$06            ; Move into quote mode so that label characters
            sta $83             ; are no longer expanded
            lda #$00
add_only:   beq x_add
           
; Reset Output Buffer
ResetOut:   lda #$00
            sta IDX_OUT
            rts    

; Reset Input Buffer
ResetIn:    lda #$00
            sta IDX_IN
            rts
                 
; Add Input
; Add a character to the input buffer and advance the counter
AddInput:   ldx IDX_IN
            cpx #$16            ; Wedge lines are limited to the physical
            bcs add_r           ;   line length
            sta INBUFFER,x
            inc IDX_IN
add_r:      rts
           
; Detokenize
; If a BASIC token is found, explode that token into PETSCII characters 
; so it can be disassembled. This is based on the ROM uncrunch code around $c71a
Detokenize: ldy #$65
            tax                 ; Copy token number to X
get_next:   dex
            beq explode         ; Token found, go write
-loop       iny                 ; Else increment index
            lda KEYWORDS,y      ; Get byte from keyword table
            bpl loop            ; Loop until end marker
            bmi get_next
explode:    iny                 ; Found the keyword; get characters from
            lda KEYWORDS,y      ;   table
            bmi last_char       ; If there's an end marker, mask byte and
            jsr AddInput        ;   add to input buffer
            bne explode
last_char:  and #$7f            ; Take out bit 7 and
            jmp AddInput        ;   add to input buffer
 
; Print Buffer
; Add a $00 delimiter to the end of the output buffer, and print it out           
PrintBuff:  lda #$00
            jsr CharOut
            lda #<OUTBUFFER
            ldy #>OUTBUFFER
            jsr PrintStr
print_done: lda #RVS_OFF        ; Reverse off after each line
            jsr CHROUT          ; ,,
            ; Fall through to Linefeed

Linefeed:   lda #LF
            jmp CHROUT             
           
; Print String
; Like BASIC's $cb1e, but not destructive to BASIC memory when coming from
; the BASIC input buffer (see $d4bb)         
PrintStr:   sta CHARAC
            sty CHARAC+1
            ldy #$00
-loop:      lda (CHARAC),y
            beq print_r
            jsr CHROUT
            lda #$00            ; Turn off quote mode for each character
            sta $d4             ; ,,
            iny
            bne loop
print_r:    rts            
            
; Prompt for Next Line
; X should be set to the number of bytes the effective address should be
; advanced
Prompt:     txa                 ; Based on the incoming X register, advance
            clc                 ;   the effecive address and store in the
            adc EFADDR          ;   persistent counter. This is how wAx
            sta X_PC            ;   remembers where it was
            lda #$00            ;   ,,
            adc EFADDR+1        ;   ,,
            sta X_PC+1          ;   ,,
            jsr ResetOut        ; Reset the output buffer to generate the prompt
            jsr wAxPrompt       ; The prompt begins with the wedge character
            lda #T_ASM          ;   Followed by the assembler tool character
            jsr CharOut         ;   ,,
            jsr Space           ;   Followed by a space
            jsr ShowPC          ; Show persistent counter
            lda TOOL_CHR        ; Check the tool character
            cmp #T_ASM          ; If it's assembler, then add a space
            bne crsr_over       ; ,,
            lda #" "            ; ,,
            .byte $3c           ; TOP (skip word)
crsr_over:  lda #CRSRRT         ; Cursor right if not assembler tool
            jsr CharOut
            ldy #$00
-loop:      lda OUTBUFFER,y     ; Copy the output buffer into KEYBUFF, which
            sta KEYBUFF,y       ;   will simulate user entry
            iny                 ;   ,,
            cpy #$08            ;   ,,
            bne loop            ;   ,,
            sty KBSIZE          ; Setting the buffer size will make it go
prompt_r:   rts            
                            
; In Direct Mode
; If the wAx tool is running in Direct Mode, the Zero flag will be set
DirectMode: ldy CURLIN+1
            iny
            rts
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ToolTable contains the list of tools and addresses for each tool
ToolTable:	.byte T_DIS,T_ASM,T_MEM,T_REG,T_EXE,T_BRK,T_TST,T_SAV,T_LOA,T_BIN
            .byte T_XDI,T_SRC,T_CPY,T_H2T,T_T2H,T_SYM,T_BAS,T_USR,T_USL
            .byte ",",";",T_FIL,T_INT,T_COM,T_HLP,T_MEN,LABEL
ToolAddr_L: .byte <List-1,<Assemble-1,<List-1,<Register-1,<Execute-1
            .byte <SetBreak-1,<Tester-1,<MemSave-1,<MemLoad-1,<List-1
            .byte <List-1,<Search-1,<MemCopy-1,<Hex2Base10-1,<Base102Hex-1
            .byte <SetPC-1,<BASICStage-1,<PlugIn-1,<List-1,
            .byte <Assemble-1,<Register-1,<Directory-1,<List-1,<Compare-1
            .byte <Help-1,<PlugMenu-1,<Labels-1
ToolAddr_H: .byte >List-1,>Assemble-1,>List-1,>Register-1,>Execute-1
            .byte >SetBreak-1,>Tester-1,>MemSave-1,>MemLoad-1,>List-1
            .byte >List-1,>Search-1,>MemCopy-1,>Hex2Base10-1,>Base102Hex-1
            .byte >SetPC-1,>BASICStage-1,>PlugIn-1,>List-1
            .byte >Assemble-1,>Register-1,>Directory-1,>List-1,>Compare-1
            .byte >Help-1,>PlugMenu-1,>Labels-1

; Plug-In Menu Data           
MenuText:   .asc LF," *** PLUG-IN MENU ***",LF,LF
            .asc ".P WAXFER",$0d
            .asc ".P RELOCATE",$0d
            .asc ".P DEBUG",$0d
            .asc ".P ML TO BASIC",$0d
            .asc ".P CHAR HELPER",$0d
            .asc ".P MUSIC",$0d
            .asc ".P MEM CONFIG",$0d
            .asc 0
            
MenuChar1:  .asc "W","R","D","M","C","M","M"
MenuChar2:  .asc "A","E","E","L","H","U","E"
MenuLoc_L:  .byte <uwAxfer,<uRelocate,<uDebug,<uML2BAS
            .byte <uChar,<uwAxScore,<uConfig
MenuLoc_H:  .byte >uwAxfer,>uRelocate,>uDebug,>uML2BAS
            .byte >uChar,>uwAxScore,>uConfig
UsageLoc_L: .byte <wAxferUse,<RelocUse,<DebugUse,<ML2BASUse
            .byte <CharUse,<wAxScUse,<ConfigUse
UsageLoc_H: .byte >wAxferUse,>RelocUse,>DebugUse,>ML2BASUse
            .byte >CharUse,>wAxScUse,>ConfigUse

; Built-In plug-in instructions
; Right padding is included to make it look prtty if the menu is used
; multiple times in the same place
UsageTxt:   .asc LF," USAGE:",LF,$00          
wAxferUse:  .asc ".U [B/T][ADDR]      ",LF,$00
RelocUse:   .asc ".U FROM TO NEWADDR  ",LF,$00
DebugUse:   .asc ".U ADDR             ",LF,$00
ML2BASUse:  .asc ".U FROM TO+1 [R/H/T]",LF,$00
CharUse:    .asc ".U [ADDR]           ",LF,$00
wAxScUse:   .asc ".U ADDR [R]         ",LF,$00
ConfigUse:  .asc ".U 0K/3K/24K        ",LF,$00
      

; Addresses for error message text
ErrAddr_L:  .byte <AsmErrMsg,<MISMATCH,<LabErrMsg,<ResErrMsg,<RBErrMsg
ErrAddr_H:  .byte >AsmErrMsg,>MISMATCH,>LabErrMsg,>ResErrMsg,>RBErrMsg

; Text display tables  
Intro:      .asc LF,"  *** WAXPANDER ***"
            .asc LF,"  BEIGEMAZE.COM/WAX",LF
            .asc LF,"  ENTER .? FOR HELP",LF,$00
Registers:  .asc LF,$b0,$c0,"A",$c0,$c0,"X",$c0,$c0,"Y",$c0,$c0
            .asc "P",$c0,$c0,"S",$c0,$c0,"PC",$c0,$c0,LF,".",";",$00
BreakMsg:   .asc LF,RVS_ON,"BRK",RVS_OFF,$00
HelpScr1:   .asc LF,LF," *** WAX COMMANDS ***",LF,LF
            .asc "D 6502 DIS E 6502+EXT",LF
            .asc "A ASSEMBLE R REGISTER",LF
            .asc "G GO       B BRKPOINT",LF
            .asc "R MEMORY   I TEXT",LF
            .asc "% BINARY   = ASSERT",LF
            .asc "T TRANSFER C COMPARE",LF,$00
HelpScr2:   .asc "@ LABELS   * SET PC",LF
            .asc "L LOAD     S SAVE",LF
            .asc "F FILES    ",$5e," STAGE",LF
            .asc "# DEC2HEX  $ HEX2DEC",LF
            .asc "U PLUG-IN  ",$5c," USR LIST",LF
            .asc "P MENU     X EXIT",LF,$00

; Error messages
AsmErrMsg:  .asc "ASSEMBL",$d9
LabErrMsg:  .asc "SYMBO",$cc
ResErrMsg:  .asc "CAN",$27,"T RESOLV",$c5
RBErrMsg:   .asc "TOO FA",$d2

; Instruction Set
; This table contains two types of one-word records--mnemonic records and
; instruction records. Every word in the table is in big-endian format, so
; the high byte is first.
;
; Mnemonic records are formatted like this...
;     fffffsss ssttttt1
; where f is first letter, s is second letter, and t is third letter. Bit
; 0 of the word is set to 1 to identify this word as a mnemonic record.
;
; Each mnemonic record has one or more instruction records after it.
; Instruction records are formatted like this...
;     oooooooo aaaaaaa0
; where o is the opcode and a is the addressing mode (see Constants section
; at the top of the code). Bit 0 of the word is set to 0 to identify this
; word as an instruction record.
InstrSet:   .byte $09,$07       ; ADC
            .byte $69,$a0       ; * ADC #immediate
            .byte $65,$70       ; * ADC zeropage
            .byte $75,$80       ; * ADC zeropage,X
            .byte $6d,$40       ; * ADC absolute
            .byte $7d,$50       ; * ADC absolute,X
            .byte $79,$60       ; * ADC absolute,Y
            .byte $61,$20       ; * ADC (indirect,X)
            .byte $71,$30       ; * ADC (indirect),Y
            .byte $0b,$89       ; AND
            .byte $29,$a0       ; * AND #immediate
            .byte $25,$70       ; * AND zeropage
            .byte $35,$80       ; * AND zeropage,X
            .byte $2d,$40       ; * AND absolute
            .byte $3d,$50       ; * AND absolute,X
            .byte $39,$60       ; * AND absolute,Y
            .byte $21,$20       ; * AND (indirect,X)
            .byte $31,$30       ; * AND (indirect),Y
            .byte $0c,$d9       ; ASL
            .byte $06,$70       ; * ASL zeropage
            .byte $16,$80       ; * ASL zeropage,X
            .byte $0e,$40       ; * ASL absolute
            .byte $1e,$50       ; * ASL absolute,X
            .byte $0a,$d0       ; * ASL accumulator
            .byte $10,$c7       ; BCC
            .byte $90,$c0       ; * BCC relative
            .byte $10,$e7       ; BCS
            .byte $b0,$c0       ; * BCS relative
            .byte $11,$63       ; BEQ
            .byte $f0,$c0       ; * BEQ relative
            .byte $12,$69       ; BIT
            .byte $24,$70       ; * BIT zeropage
            .byte $2c,$40       ; * BIT absolute
            .byte $13,$53       ; BMI
            .byte $30,$c0       ; * BMI relative
            .byte $13,$8b       ; BNE
            .byte $d0,$c0       ; * BNE relative
            .byte $14,$19       ; BPL
            .byte $10,$c0       ; * BPL relative
            .byte $14,$97       ; BRK
            .byte $00,$b0       ; * BRK implied
            .byte $15,$87       ; BVC
            .byte $50,$c0       ; * BVC relative
            .byte $15,$a7       ; BVS
            .byte $70,$c0       ; * BVS relative
            .byte $1b,$07       ; CLC
            .byte $18,$b0       ; * CLC implied
            .byte $1b,$09       ; CLD
            .byte $d8,$b0       ; * CLD implied
            .byte $1b,$13       ; CLI
            .byte $58,$b0       ; * CLI implied
            .byte $1b,$2d       ; CLV
            .byte $b8,$b0       ; * CLV implied
            .byte $1b,$61       ; CMP
            .byte $c9,$a0       ; * CMP #immediate
            .byte $c5,$70       ; * CMP zeropage
            .byte $d5,$80       ; * CMP zeropage,X
            .byte $cd,$40       ; * CMP absolute
            .byte $dd,$50       ; * CMP absolute,X
            .byte $d9,$60       ; * CMP absolute,Y
            .byte $c1,$20       ; * CMP (indirect,X)
            .byte $d1,$30       ; * CMP (indirect),Y
            .byte $1c,$31       ; CPX
            .byte $e0,$a0       ; * CPX #immediate
            .byte $e4,$70       ; * CPX zeropage
            .byte $ec,$40       ; * CPX absolute
            .byte $1c,$33       ; CPY
            .byte $c0,$a0       ; * CPY #immediate
            .byte $c4,$70       ; * CPY zeropage
            .byte $cc,$40       ; * CPY absolute
            .byte $21,$47       ; DEC
            .byte $c6,$70       ; * DEC zeropage
            .byte $d6,$80       ; * DEC zeropage,X
            .byte $ce,$40       ; * DEC absolute
            .byte $de,$50       ; * DEC absolute,X
            .byte $21,$71       ; DEX
            .byte $ca,$b0       ; * DEX implied
            .byte $21,$73       ; DEY
            .byte $88,$b0       ; * DEY implied
            .byte $2b,$e5       ; EOR
            .byte $49,$a0       ; * EOR #immediate
            .byte $45,$70       ; * EOR zeropage
            .byte $55,$80       ; * EOR zeropage,X
            .byte $4d,$40       ; * EOR absolute
            .byte $5d,$50       ; * EOR absolute,X
            .byte $59,$60       ; * EOR absolute,Y
            .byte $41,$20       ; * EOR (indirect,X)
            .byte $51,$30       ; * EOR (indirect),Y
            .byte $4b,$87       ; INC
            .byte $e6,$70       ; * INC zeropage
            .byte $f6,$80       ; * INC zeropage,X
            .byte $ee,$40       ; * INC absolute
            .byte $fe,$50       ; * INC absolute,X
            .byte $4b,$b1       ; INX
            .byte $e8,$b0       ; * INX implied
            .byte $4b,$b3       ; INY
            .byte $c8,$b0       ; * INY implied
            .byte $53,$61       ; JMP
            .byte $4c,$40       ; * JMP absolute
            .byte $6c,$10       ; * JMP indirect
            .byte $54,$e5       ; JSR
            .byte $20,$40       ; * JSR absolute
            .byte $61,$03       ; LDA
            .byte $a9,$a0       ; * LDA #immediate
            .byte $a5,$70       ; * LDA zeropage
            .byte $b5,$80       ; * LDA zeropage,X
            .byte $ad,$40       ; * LDA absolute
            .byte $bd,$50       ; * LDA absolute,X
            .byte $b9,$60       ; * LDA absolute,Y
            .byte $a1,$20       ; * LDA (indirect,X)
            .byte $b1,$30       ; * LDA (indirect),Y
            .byte $61,$31       ; LDX
            .byte $a2,$a0       ; * LDX #immediate
            .byte $a6,$70       ; * LDX zeropage
            .byte $b6,$90       ; * LDX zeropage,Y
            .byte $ae,$40       ; * LDX absolute
            .byte $be,$60       ; * LDX absolute,Y
            .byte $61,$33       ; LDY
            .byte $a0,$a0       ; * LDY #immediate
            .byte $a4,$70       ; * LDY zeropage
            .byte $b4,$80       ; * LDY zeropage,X
            .byte $ac,$40       ; * LDY absolute
            .byte $bc,$50       ; * LDY absolute,X
            .byte $64,$e5       ; LSR
            .byte $46,$70       ; * LSR zeropage
            .byte $56,$80       ; * LSR zeropage,X
            .byte $4e,$40       ; * LSR absolute
            .byte $5e,$50       ; * LSR absolute,X
            .byte $4a,$d0       ; * LSR accumulator
            .byte $73,$e1       ; NOP
            .byte $ea,$b0       ; * NOP implied
            .byte $7c,$83       ; ORA
            .byte $09,$a0       ; * ORA #immediate
            .byte $05,$70       ; * ORA zeropage
            .byte $15,$80       ; * ORA zeropage,X
            .byte $0d,$40       ; * ORA absolute
            .byte $1d,$50       ; * ORA absolute,X
            .byte $19,$60       ; * ORA absolute,Y
            .byte $01,$20       ; * ORA (indirect,X)
            .byte $11,$30       ; * ORA (indirect),Y
            .byte $82,$03       ; PHA
            .byte $48,$b0       ; * PHA implied
            .byte $82,$21       ; PHP
            .byte $08,$b0       ; * PHP implied
            .byte $83,$03       ; PLA
            .byte $68,$b0       ; * PLA implied
            .byte $83,$21       ; PLP
            .byte $28,$b0       ; * PLP implied
            .byte $93,$d9       ; ROL
            .byte $26,$70       ; * ROL zeropage
            .byte $36,$80       ; * ROL zeropage,X
            .byte $2e,$40       ; * ROL absolute
            .byte $3e,$50       ; * ROL absolute,X
            .byte $2a,$d0       ; * ROL accumulator
            .byte $93,$e5       ; ROR
            .byte $66,$70       ; * ROR zeropage
            .byte $76,$80       ; * ROR zeropage,X
            .byte $6e,$40       ; * ROR absolute
            .byte $7e,$50       ; * ROR absolute,X
            .byte $6a,$d0       ; * ROR accumulator
            .byte $95,$13       ; RTI
            .byte $40,$b0       ; * RTI implied
            .byte $95,$27       ; RTS
            .byte $60,$b0       ; * RTS implied
            .byte $98,$87       ; SBC
            .byte $e9,$a0       ; * SBC #immediate
            .byte $e5,$70       ; * SBC zeropage
            .byte $f5,$80       ; * SBC zeropage,X
            .byte $ed,$40       ; * SBC absolute
            .byte $fd,$50       ; * SBC absolute,X
            .byte $f9,$60       ; * SBC absolute,Y
            .byte $e1,$20       ; * SBC (indirect,X)
            .byte $f1,$30       ; * SBC (indirect),Y
            .byte $99,$47       ; SEC
            .byte $38,$b0       ; * SEC implied
            .byte $99,$49       ; SED
            .byte $f8,$b0       ; * SED implied
            .byte $99,$53       ; SEI
            .byte $78,$b0       ; * SEI implied
            .byte $9d,$03       ; STA
            .byte $85,$70       ; * STA zeropage
            .byte $95,$80       ; * STA zeropage,X
            .byte $8d,$40       ; * STA absolute
            .byte $9d,$50       ; * STA absolute,X
            .byte $99,$60       ; * STA absolute,Y
            .byte $81,$20       ; * STA (indirect,X)
            .byte $91,$30       ; * STA (indirect),Y
            .byte $9d,$31       ; STX
            .byte $86,$70       ; * STX zeropage
            .byte $96,$90       ; * STX zeropage,Y
            .byte $8e,$40       ; * STX absolute
            .byte $9d,$33       ; STY
            .byte $84,$70       ; * STY zeropage
            .byte $94,$80       ; * STY zeropage,X
            .byte $8c,$40       ; * STY absolute
            .byte $a0,$71       ; TAX
            .byte $aa,$b0       ; * TAX implied
            .byte $a0,$73       ; TAY
            .byte $a8,$b0       ; * TAY implied
            .byte $a4,$f1       ; TSX
            .byte $ba,$b0       ; * TSX implied
            .byte $a6,$03       ; TXA
            .byte $8a,$b0       ; * TXA implied
            .byte $a6,$27       ; TXS
            .byte $9a,$b0       ; * TXS implied
            .byte $a6,$43       ; TYA
            .byte $98,$b0       ; * TYA implied
            .byte TABLE_END,$00 ; End of 6502 table
Extended:   .byte $0b,$87       ; ANC
            .byte $0b,$a0       ; * ANC immediate
            .byte $2b,$a0       ; * ANC immediate
            .byte $98,$71       ; SAX
            .byte $87,$70       ; * SAX zero page
            .byte $97,$90       ; * SAX zero page,y
            .byte $83,$20       ; * SAX (indirect,x)
            .byte $8f,$40       ; * SAX absolute
            .byte $0c,$a5       ; ARR
            .byte $6b,$a0       ; * ARR immediate
            .byte $0c,$e5       ; ASR
            .byte $4b,$a0       ; * ASR immediate
            .byte $66,$03       ; LXA
            .byte $ab,$a0       ; * LXA immediate
            .byte $9a,$03       ; SHA
            .byte $9f,$60       ; * SHA absolute,y
            .byte $93,$30       ; * SHA (indirect),y
            .byte $98,$b1       ; SBX
            .byte $cb,$a0       ; * SBX immediate
            .byte $20,$e1       ; DCP
            .byte $c7,$70       ; * DCP zero page
            .byte $d7,$80       ; * DCP zero page,x
            .byte $cf,$40       ; * DCP absolute
            .byte $df,$50       ; * DCP absolute,x
            .byte $db,$60       ; * DCP absolute,y
            .byte $c3,$20       ; * DCP (indirect,x)
            .byte $d3,$30       ; * DCP (indirect),y
            .byte $23,$e1       ; DOP
            .byte $04,$70       ; * DOP zero page
            .byte $14,$80       ; * DOP zero page,x
            .byte $34,$80       ; * DOP zero page,x
            .byte $44,$70       ; * DOP zero page
            .byte $54,$80       ; * DOP zero page,x
            .byte $64,$70       ; * DOP zero page
            .byte $74,$80       ; * DOP zero page,x
            .byte $80,$a0       ; * DOP immediate
            .byte $82,$a0       ; * DOP immediate
            .byte $89,$a0       ; * DOP immediate
            .byte $c2,$a0       ; * DOP immediate
            .byte $d4,$80       ; * DOP zero page,x
            .byte $e2,$a0       ; * DOP immediate
            .byte $f4,$80       ; * DOP zero page,x
            .byte $4c,$c5       ; ISB
            .byte $e7,$70       ; * ISB zero page
            .byte $f7,$80       ; * ISB zero page,x
            .byte $ef,$40       ; * ISB absolute
            .byte $ff,$50       ; * ISB absolute,x
            .byte $fb,$60       ; * ISB absolute,y
            .byte $e3,$20       ; * ISB (indirect,x)
            .byte $f3,$30       ; * ISB (indirect),y
            .byte $60,$4b       ; LAE
            .byte $bb,$60       ; * LAE absolute,y
            .byte $60,$71       ; LAX
            .byte $a7,$70       ; * LAX zero page
            .byte $b7,$90       ; * LAX zero page,y
            .byte $af,$40       ; * LAX absolute
            .byte $bf,$60       ; * LAX absolute,y
            .byte $a3,$20       ; * LAX (indirect,x)
            .byte $b3,$30       ; * LAX (indirect),y
            .byte $73,$e1       ; NOP
            .byte $1a,$b0       ; * NOP implied
            .byte $3a,$b0       ; * NOP implied
            .byte $5a,$b0       ; * NOP implied
            .byte $7a,$b0       ; * NOP implied
            .byte $da,$b0       ; * NOP implied
            .byte $fa,$b0       ; * NOP implied
            .byte $93,$03       ; RLA
            .byte $27,$70       ; * RLA zero page
            .byte $37,$80       ; * RLA zero page,x
            .byte $2f,$40       ; * RLA absolute
            .byte $3f,$50       ; * RLA absolute,x
            .byte $3b,$60       ; * RLA absolute,y
            .byte $23,$20       ; * RLA (indirect,x)
            .byte $33,$30       ; * RLA (indirect),y
            .byte $94,$83       ; RRA
            .byte $67,$70       ; * RRA zero page
            .byte $77,$80       ; * RRA zero page,x
            .byte $6f,$40       ; * RRA absolute
            .byte $7f,$50       ; * RRA absolute,x
            .byte $7b,$60       ; * RRA absolute,y
            .byte $63,$20       ; * RRA (indirect,x)
            .byte $73,$30       ; * RRA (indirect),y
            .byte $98,$87       ; SBC
            .byte $eb,$a0       ; * SBC immediate
            .byte $9b,$1f       ; SLO
            .byte $07,$70       ; * SLO zero page
            .byte $17,$80       ; * SLO zero page,x
            .byte $0f,$40       ; * SLO absolute
            .byte $1f,$50       ; * SLO absolute,x
            .byte $1b,$60       ; * SLO absolute,y
            .byte $03,$20       ; * SLO (indirect,x)
            .byte $13,$30       ; * SLO (indirect),y
            .byte $9c,$8b       ; SRE
            .byte $47,$70       ; * SRE zero page
            .byte $57,$80       ; * SRE zero page,x
            .byte $4f,$40       ; * SRE absolute
            .byte $5f,$50       ; * SRE absolute,x
            .byte $5b,$60       ; * SRE absolute,y
            .byte $43,$20       ; * SRE (indirect,x)
            .byte $53,$30       ; * SRE (indirect),y
            .byte $9a,$31       ; SHX
            .byte $9e,$60       ; * SHX absolute,y
            .byte $9a,$33       ; SHY
            .byte $9c,$50       ; * SHY absolute,x
            .byte $a3,$e1       ; TOP
            .byte $0c,$40       ; * TOP absolute
            .byte $1c,$50       ; * TOP absolute,x
            .byte $3c,$50       ; * TOP absolute,x
            .byte $5c,$50       ; * TOP absolute,x
            .byte $7c,$50       ; * TOP absolute,x
            .byte $dc,$50       ; * TOP absolute,x
            .byte $fc,$50       ; * TOP absolute,x
            .byte $0b,$8b       ; ANE
            .byte $8b,$a0       ; * ANE immediate
            .byte $9a,$27       ; SHS
            .byte $9b,$60       ; * SHS absolute,y
            .byte $50,$5b       ; JAM
            .byte $02,$b0       ; * JAM implied           
            .byte $43,$29       ; HLT
            .byte $02,$b0       ; * HLT implied
            .byte XTABLE_END,$00; End of 6502 extended table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; wAx USER TOOLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WAXFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VIA Registers
DDR         = $9112             ; Data Direction Register
UPORT       = $9110             ; User Port
PCR         = $911c             ; Peripheral Control Register
IFR         = $911d             ; Interrupt flag register
IER         = $911e             ; Interrupt enable register

; Interrupt
NMI         = $0318             ; NMI Vector
HWNMI       = $feb2             ; Default NMI
RFI         = $ff56             ; Return from interrupt

; Locations
TIMER       = $a2               ; Timer low byte

; wAxfer Storage
RECEIVED    = $0247             ; Received a byte
HEADER      = $0248             ; Header byte index (0,1,2=done)
TERMMODE    = $0249             ; Terminal mode (bit 7 S=Term, C=PRG)
RUNNING     = $024a             ; Running
BASIC       = $024b             ; BASIC program

uwAxfer:    bcc ch_pk           ; If no address is provided, check for T
            lda #2              ; Set header as though the header has already
            sta HEADER          ;   been read
            jsr EAtoPC          ; Program counter will be start of memory
            lsr TERMMODE        ; Set PRG mode
            jmp setup           ; Continue to hardware setup
ch_pk:      lda #0              ; Header byte count clear
            sta HEADER          ; ,,
            sta TERMMODE        ; Default to PRG mode
            sta BASIC           ; Default to non-BASIC
            jsr ResetIn         ; Get first character
            jsr CharGet         ; ,,
            cmp #"T"            ; Terminal mode
            bne ch_basic        ; ,,
            sec                 ; Set Terminal mode
            ror TERMMODE        ; ,,
ch_basic:   cmp #"B"            ; Is BASIC specified?
            bne setup           ; ,,
            ror BASIC           ; ,,
setup:      lda #0              ; Set DDR to listen to all 8 data lines,
            sta DDR             ; ,,
            sta PCR             ; And set peripheral control to interrupt input
            sta RECEIVED        ; And reset received flag
            lda #<ISR           ; Install the new service routine, which will
            sta NMI             ;   listen for data on the User Port
            lda #>ISR           ;   ,,
            sta NMI+1           ;   ,,
            lda #%10001010      ; Enable CB2 interrupt
            sta IER             ; ,,
            bit TERMMODE        ; If this plug-in was invoked with an address or
            bpl standby         ;   PRG mode, just wait for STOP to be pressed
            jmp (READY)         ; For Terminal mode, just exit
standby:    lda #171            ; Add a prompt
            jsr $ffd2           ; ,,
            lsr RUNNING         ; Clear RUNNING flag
wait:       bit TIMER           ; Has the timer advanced 64 jiffies?
            bvc ch_stop         ;   If not, check for STOP
            bit RUNNING         ;   If so, has the first byte come?
            bmi Complete        ;   If so, end and show info
ch_stop:    jsr ISCNTC          ; Check for BREAK key
            beq Complete        ; If it's pressed, end data receive, show info
            bit RECEIVED        ; If data receive flag is off, just wait
            bpl wait            ; ,,
            lsr RECEIVED        ; Turn the flag off
            lda EFADDR          ; Are we at a multiple of 256 bytes?
            bne wait            ;   If not, wait
            lda #192            ; If so, show progress bar
            jsr $ffd2           ; ,,
            jmp wait            ; Back to start of wait loop

; Show Received Data Range            
Complete:   lda #179            ; Close the progress bar
            jsr $ffd2           ; ,,
            lda #$0d            ; Drop to next line
            jsr $ffd2           ; ,,
            bit RUNNING         ; Was any data received?
            bpl comp_r          ;   If not, just end without showing anything
is_data:    jsr ResetOut        ; Show final locations...
            lda #"."            ;    Disassemble tool
            jsr CharOut         ;    ,,
            jsr ShowPC          ;    Starting address
            lda #";"            ;      to
            jsr CharOut         ;      ,,
            jsr ShowAddr        ;    Ending address
            jsr PrintBuff       ; ,,
            bit BASIC           ; Rechain BASIC program, if specified with B
            bpl comp_r          ; ,,
            jsr Rechain         ; ,,
comp_r:     jmp (READY)         
  
; Interrupt Service Routine
; Handles NMI Interrupt triggered by CB2 high-to-low transition                                  
ISR:        pha
            txa
            pha
            tya
            pha
            lda #%00001000      ; Check the interrupt flag bit 3
            bit IFR             ; If it's set, User Port data is available
            bne recv            ; Otherwise, it's a normal hardware interrupt
hw:         jmp HWNMI           ; This is a normal old NMI
recv:       lda UPORT           ; Get the User Port byte
            bit TERMMODE        ; If the user did not specify a load address,
            bmi term_h          ;   handle terminal mode
            ldx HEADER          ; Get current header count
            cpx #2              ; If both header bytes have been received,
            beq prg             ;   handle PRG data
            sta EFADDR,x        ; Otherwise, store header bytes in effective
            sta X_PC,x          ;   address and persistent counter
            inc HEADER          ; Advance header
            bne ser_r           ; Return from interrupt
prg:        ldx #0              ; Store PRG data byte in current location
            sta (EFADDR,x)      ; ,,
            stx TIMER           ; Reset time since last byte
            sec                 ; Set byte received flag for caller
            ror RECEIVED        ; ,,
            sec                 ; Set when first data is received
            ror RUNNING         ; ,,
next_addr:  jsr IncAddr         ; Increment effective address
            jmp RFI             ; Return from interrupt
term_h:     cmp #10             ; Ignore ASCII LF
            beq ser_r           ; ,,
ch_del:     cmp #127            ; Convert terminal DEL to PETSCII delete
            bne addchar         ; ,,
            lda #20             ; ,,
addchar:    ldy KBSIZE          ; Add the character received to keyboard buffer
            cpy #10             ; ,, (avoid buffer overwrite)
            bcs ser_r           ; ,,
            sta KEYBUFF,y       ; ,,
            inc KBSIZE          ; Increment the buffer size
ser_r:      jmp RFI             ; Return from the User Port interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WAXFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relocate Workspace
SRC_END     = $0247             ; End of source range (2 bytes)
DEST_END    = $0249             ; End of destination range (2 bytes)
OFFSET      = $024b             ; Offset (X_PC - EFADDR, 2 bytes)
-OPERAND    = $024d             ; Instruction operand (2 bytes)           

            ; Parameter collection
uRelocate:  bcs okay            ; Error if invalid first argument (source start)
error:      jmp $cf08           ; ?SYNTAX ERROR, warm start
okay:       jsr Buff2Byte       ; Get high byte of source end
            bcc error           ; ,,
            sta SRC_END+1       ; ,,
            jsr Buff2Byte       ; Get low byte of source end
            bcc error           ; ,,
            sta SRC_END         ; ,,
            jsr Buff2Byte       ; Get high byte of destination start
            bcc error           ; ,,
            sta X_PC+1          ; ,,
            jsr Buff2Byte       ; Get low byte of destination start
            bcc error           ; ,,
            sta X_PC            ; ,,
            sec                 ; Calculate default offset (X_PC - EFADDR)
            sbc EFADDR          ;   This is initially used to calculate an
            sta OFFSET          ;   end-of-destination address, but both end-of-
            lda X_PC+1          ;   destination and offset can be overridden by
            sbc EFADDR+1        ;   additional optional 16-bit parameters.
            sta OFFSET+1        ;   ,,
            lda OFFSET          ; Calculate default end-of-destination 
            clc                 ;   address, for checking the end of the
            adc SRC_END         ;   relocate loop.
            sta DEST_END        ;   ,,
            lda OFFSET+1        ;   ,,
            adc SRC_END+1       ;   ,,
            sta DEST_END+1      ;   ,,
            jsr Buff2Byte       ; Get high byte of destination end
            bcc Relocate        ;   This is optional, so begin if not provided
            sta DEST_END+1      ;   ,,
            jsr Buff2Byte       ; Get low byte of destination end
            bcc error           ;   If only high byte was provided, but not the
            sta DEST_END        ;   low byte, it's a syntax error
            jsr Buff2Byte       ; Get high byte of offset override
            bcc Relocate        ;   This is optional, so begin if not provided
            sta OFFSET+1        ;   ,,
            jsr Buff2Byte       ; Get low byte of offset override
            bcc error           ;   If only high byte was provided, but not the
            sta OFFSET          ;   low byte, it's a syntax error
            ; Fall through to Relocate
            
            ; Relocation process
Relocate:   ldy #$00            ; Load instruction opcode
            lda (X_PC),y        ; ,,
            ldx #$03            ; Determine instruction length
            cmp #$20            ;    JSR is a special case, then various bit
            beq three           ;    patterns are tested
            bit $c3b9           ;    Test %00001000 from BASIC ROM
            beq one_or_two      ;    
            bit $c01a           ;    Test %00000101 from BASIC ROM
            beq one             ;
            bit $c50e           ;    Test %00010100 from BASIC ROM
            bne three           ;    
one_or_two: bit $c01e           ;    Test %10011111 from BASIC ROM
            bne two
one:        dex                 ;    Instruction is 1 byte
two:        dex                 ;    Instruction is 2 bytes
            bpl rnext           ; 1/2 bytes - Always advance to next instruction
three:      iny                 ; 3 bytes - Something to potentially update.
            lda (X_PC),y        ;   Grab the operand from the two bytes after
            sta OPERAND         ;   the opcode...
            cmp EFADDR          ;   ,,            
            iny                 ;   ,,
            lda (X_PC),y        ;   ,,
            sta OPERAND+1       ;   ,,
            sbc EFADDR+1        ; ...then check whether the range of the operand
            bcc rnext           ;   is within the original code range
            lda OPERAND         ;   ,,
            cmp SRC_END         ;   ,,
            lda OPERAND+1       ;   ,,
            sbc SRC_END+1       ;   ,,
            bcs rnext           ;   If not, advance to the next instruction
            dey                 ;   ,,
            lda OPERAND         ; If the operand is within this code range,
            adc OFFSET          ;   add the offset and copy it
            sta (X_PC),y        ;   ,,
            iny                 ;   ,,
            lda OPERAND+1       ;   ,,
            adc OFFSET+1        ;   ,,
            sta (X_PC),y        ;   ,,
            jsr ResetOut        ; Show list of changed addresses
            lda #'.'            ; ,,
            jsr CharOut         ; ,,
            jsr ShowPC          ; ,,
            jsr PrintBuff       ; ,,
            ldx #$03            ; ,, (reset X back to 3 for incrementing X_PC)
rnext:      txa                 ; Add X (instruction size) to the source pointer
            clc                 ; ,,
            adc X_PC            ; ,,
            sta X_PC            ; ,,
            bcc rcheck_end      ; ,,
            inc X_PC+1          ; ,,
rcheck_end: cmp DEST_END        ; Have we reached the end of the range?
            lda X_PC+1          ; ,,
            sbc DEST_END+1      ; ,,
            bcc Relocate        ; Get next instruction in the code
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DEBUG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BASIC Routines
UND_ERROR   = $c8e3             ; UNDEF'D STATEMENT ERROR

; Knapsack Generator
KNAPSACK    = $03ef             ; Knapsack storage (10 bytes)
BREAKPT     = KNAPSACK+10       ; Breakpoint address (2 bytes)
KNAPSIZE    = BREAKPT+2         ; Knapsack size (1 byte)

; Main routine entry point
; * If setting a breakpoint, its address is in EFADDR vector
; * If clearing a breakpoint, the Carry flag is clear
uDebug:     bcs NewKnap         ; A legal address has been provided in $a6/$a7
restore:    lda BREAKPT         ; Otherwise, restore the breakpoint to the
            sta EFADDR          ;   original code by copying the
            lda BREAKPT+1       ;   bytes in the knapsack back to the code
            sta EFADDR+1        ;   ,,
            ldy KNAPSIZE        ; Get knapsack code size (3-5, or 0)
            beq restore_r       ; Don't restore if KNAPSIZE isn't set
            dey
-loop       lda KNAPSACK+2,y    ; Move between 3 and 5 bytes back
            sta (EFADDR),y      ;   to their original locations
            dey                 ;   ,,
            bpl loop            ;   ,,
            lda #$00            ; Reset the knapsack size so that doing it again
            sta KNAPSIZE        ;   doesn't mess up the original code
restore_r:  rts                 ;   ,,

; New Knapsack
; Generate a new knapsack at the effective address
NewKnap:    lda KNAPSIZE        ; Don't install a knapsack if there's already
            bne knap_r          ;   one installed
            ldy #$00            ; (BRK)
            sty KNAPSACK        ; ,,
            lda #$ea            ; (NOP)
            sta KNAPSACK+1      ; ,,
next_inst:  tya                 ; Preserve Y against SizeOf
            pha                 ; ,,
            lda (EFADDR),y      ; A = Opcode of the breakpoint instruction
            jsr SizeOf          ; X = Size of instruction (1-3)
            pla
            tay
            bcs xfer            ; Error if branch or unknown instruction
            jmp UND_ERROR        ; ?UNDEF'D STATEMENT ERROR     
xfer:       lda (EFADDR),y      ; Move X bytes starting at Y index
            sta KNAPSACK+2,y    ; Y is a running count of knapsacked bytes
            iny                 ; ,,
            dex                 ; ,,
            bne xfer            ; ,,
            cpy #$03            ; If at least three bytes have been knapsacked
            bcc next_inst       ;   we're done
            lda EFADDR          ; Stash pointer in breakpoint storage for
            sta BREAKPT         ;   later restoration
            lda EFADDR+1        ;   ,,
            sta BREAKPT+1       ;   ,,
            sty KNAPSIZE        ; Save knapsack size for later
            lda #$ea            ; (NOP)
-loop:      cpy #$03            ; Pad code with more than three bytes with
            beq add_kjmp        ;   NOPs after the first three
            dey                 ;   ,,
            sta (EFADDR),y      ;   ,,
            bne loop            ;   ,,
add_kjmp:   ldy #$00
            lda #$4c            ; (JMP) This is the JMP to the knapsack
            sta (EFADDR),y      ; 
            lda #<KNAPSACK      ; Store knapsack JMP low byte
            iny                 ; ,,
            sta (EFADDR),y      ; ,,
            lda #>KNAPSACK      ; Store knapsack JMP high byte
            iny                 ; ,,
            sta (EFADDR),y      ; ,,
            lda KNAPSIZE        ; Calculate the return jump point (original
            tay                 ;   address + Y)
            clc                 ;   ,,
            adc EFADDR          ;   ,,
            sta KNAPSACK+3,y    ; Store return JMP low byte
            lda #$00
            adc EFADDR+1 
            sta KNAPSACK+4,y    ; Store return JMP high byte
            lda #$4c            ; (JMP) This is the JMP to the return point        
            sta KNAPSACK+2,y    ; ,,
knap_r:     rts 
    
; Size Of Instruction
; Given an opcode in A, return instruction size in X and set Carry flag
; Carry clear indicates an error (unknown or relative branch instruction)        
SizeOf:     jsr Lookup
            bcc size_r          ; Return with Carry clear to indicate error
            lsr                 ; Addressing mode is in high nybble, so
            lsr                 ;   shift it to the right to get an index
            lsr                 ;   ,,
            lsr                 ;   ,,
            tax                 ; Use that index to get the size from the
            lda AddrSize,x      ;   table
            tax                 ; X is the return value
            cpx #$01            ; Carry set = success, clear = failure
size_r:     rts            

; Size by addressing mode high nybble
AddrSize:   .byte 0,3,2,2,3,3,3,2,2,2,2,1,0,1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ML TO BASIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ML2BAS Workspace
LINE_NUM    = $0247             ; BASIC Line Number (2 bytes)
MODIFIER    = $0249             ; Relocate or absolute
FAIL_POINT  = $024a             ; BASIC program end restore point (2 bytes)

uML2BAS:    bcc merror          ; Error if the first address is no good
            jsr Buff2Byte       ; Get high byte of range end
            bcc merror          ; ,,
            sta RANGE_END+1     ; ,,
            jsr Buff2Byte       ; Get low byte of range end
            bcc merror          ; ,,
            sta RANGE_END       ; ,,
            jsr CharGet         ; If there's an R at the end of the command,
            cmp #"R"            ;   use the relocatable syntax instead of
            beq set_mod         ;   absolute addresses
            cmp #"H"            ; If there's an H at the end of the command,            
            beq set_mod         ;   this will create hex dump lines
            cmp #"T"            ; If there's a T at the end of the command,
            beq set_mod         ;   this will create assertion tests
            lda #$00 
set_mod:    sta MODIFIER
            lda #$00            ; Initialize fail point high byte
            sta FAIL_POINT+1    ; ,,
            lda $2b             ; Set persistent counter with start of
            sta X_PC            ;   BASIC
            lda $2c             ;   ,,
            sta X_PC+1          ;   ,,
            lda #$64            ; Start at line 100 by default
            sta LINE_NUM        ; ,,
            lda #$00            ; ,,
            sta LINE_NUM+1      ; ,,
            jsr NextLink        ; Is there an existing BASIC program?
            bcc found_end       ; If no existing program,
            jsr FindEnd         ; Find the last line number
            jsr IncLine         ; Increment it by 5
            lda X_PC            ; Set fail point, which preserves the existing
            sta FAIL_POINT      ;   BASIC program if the ML2BAS process results
            lda X_PC+1          ;   in an out of memory condition.
            sta FAIL_POINT+1    ;   ,,
found_end:  lda MODIFIER        ; If the code is not relocatable, skip the
            beq mStart          ;   PC setting up front
            jsr LinkBytes       ; Add link bytes to next line
            jsr LineNumber      ; Add line number to first line
            lda #WEDGE          ; Add wedge character to line
            jsr AddByte         ; ,,
            lda #$ac            ; Add PC set tool
            jsr AddByte         ; ,,
            jsr ResetOut        ; Add the start address to the output buffer
            jsr ShowAddr        ; ,,
            jsr AddBuffer       ; Add the output buffer to the BASIC line
            jsr mEndLine        ; Finish the first BASIC line
            jmp mStart          ; Start adding lines of 6502 code
merror:     jmp $cf08           ; ?SYNTAX ERROR, warm start
mStart:     jsr mChRange
            bcc range_ok
mdone:      jsr mEnd            ; Add $00,$00 to the the program
            jsr Rechain         ; Rechain BASIC program 
            jmp ($c002)         ; READY.
range_ok:   jsr LinkBytes
            jsr LineNumber
            lda #WEDGE          ; Add the wedge character
            jsr AddByte         ; ,,
            lda #T_ASM          ; Default to the assembler tool, but...
            ldy MODIFIER        ; If the modifier is T (assertion test), then
            cpy #"T"            ;   switch the tool over to =
            bne show_tool       ;   ,,
            lda #$b2            ;   ,,
show_tool:  jsr AddByte         ; Add the selected tool to the buffer
            lda MODIFIER        ; If the user requested relocatable code,
            beq show_addr       ;   add the * instead of the address
            cmp #"T"            ;   ,,
            beq show_addr       ;   ,,
            lda #$ac            ;   ,,
            jsr AddByte         ;   ,,
            jmp code_part       ;   ,,
show_addr:  jsr ResetOut        ; Add the current address to the BASIC line
            jsr ShowAddr        ; ,,
            jsr AddBuffer       ; ,,
code_part:  lda #" "            ; Space after address or *
            jsr AddByte         ; ,,
            jsr ResetOut        ; Reset output for the code portion
            lda MODIFIER        ; If the disassembly is in relocate mode,
            beq gen_code        ;   
            cmp #"H"            ;   check for hex dump modifier and
            beq HexDump         ;   handle that, if necessary. Otherwise, check
            cmp #"T"            ;   for assertion test modified and
            beq HexDump         ;   handle that, if necessary. Otherwise, check
            jsr CheckRel        ;   for relative branch. If so, disassemble the
            bcs code2buff       ;   instruction as two bytes.
gen_code:   jsr Disasm          ; Disassemble code to empty output buffer and
code2buff:  jsr AddBuffer       ;   add it to the BASIC LINE
            jsr mEndLine        ; End the line
            jmp mStart          ; Check for the next line of code

; Hex Dump Line
; Add up to six hex bytes to the current BASIC line buffer
HexDump:    lda #$04            ; Reset a byte counter; we'll add up to six
            sta $08             ;   bytes per BASIC line (see below)
            lda MODIFIER        ; If the modifier is assertion testing,
            cmp #"T"            ;   don't add a colon to the buffer
            beq add_hex         ;   ,,
            inc $08             ; If the modifier is not assertion testing,
            inc $08             ;   use six bytes instead of 4
            lda #":"            ; Add a colon to specify hex entry
            jsr AddByte         ; Add the wedge character to the buffer
add_hex:    jsr IncAddr         ; Add the hex data to the buffer
            jsr Hex             ; ,,
            jsr mChRange        ; Is the counter still in range?
            bcs code2buff       ; If not, finish the line
next_byte:  dec $08
            lda $08
            bne add_hex
            beq code2buff

; Add Link Bytes
; We're not trying to keep track of the starting addresses of each line,
; because BASIC can do that.
LinkBytes:  lda #$ff            ; Add two $ff bytes to start the next
            jsr AddByte         ;   BASIC line. These will be set by the BASIC
            jmp AddByte         ;   rechain operation at the end of the build
 
; Add Line Number            
LineNumber: lda LINE_NUM        ; Add the current line number to the
            jsr AddByte         ;   BASIC line 
            lda LINE_NUM+1      ;   ,,
            jsr AddByte         ;   ,,
IncLine:    lda #$05            ; Increment the line number by 5
            clc                 ; ,,
            adc LINE_NUM        ; ,,
            sta LINE_NUM        ; ,,
            lda #$00            ; ,,
            adc LINE_NUM+1      ; ,,
            sta LINE_NUM+1      ; ,,
            rts    
 
; Add Byte
; Add the byte in Accumulator to the BASIC line            
AddByte:    pha
            ldx #$00
            sta (X_PC,x)
            jsr IncPC
            lda X_PC+1          ; Check memory for end of BASIC
            cmp $34             ; ,,
            bcc ok              ; ,,
            lda X_PC            ; ,,
            cmp $33             ; ,,
            bcs OutOfMem        ; If at limit of memory, then ERROR
ok:         pla
            rts        

; Perform NEW, then show Out of Memory Error
OutOfMem:   jsr ResetOut        ; Show the current address, so the user
            lda #"$"            ;   knows where we ran out of BASIC
            jsr CharOut         ;   memory
            jsr ShowAddr        ;   ,,
            lda #$0d            ;   ,,
            jsr CharOut         ;   ,,
            jsr PrintBuff       ;   ,,
            lda FAIL_POINT+1    ; Is there an existing program?
            bne mrestore        ; If so, restore it instead of NEW
            jsr $c642           ; Perform NEW
            jmp $c435           ; Out of Memory Error + Warm Start                        
mrestore:   lda FAIL_POINT      ; Reset the bytes at the previous
            sta $07             ;   program end address to $00,
            lda FAIL_POINT+1    ;   essentially reversing everything
            sta $08             ;   this process did, and the rechain
            ldy #$00            ;   the BASIC program so it's like
            tya                 ;   nothing ever happened.
            sta ($07),y         ;   ,,
            iny                 ;   ,, 
            sta ($07),y         ;   ,,
            jsr Rechain         ;   ,,
            jmp $c435           ; Out of Memory Error + Warm Start

; End Program / End Line
; End the BASIC line or program
mEnd:       jsr mEndLine            
mEndLine:   lda #$00
            jmp AddByte

; Add Output Buffer
; Paste output buffer into the BASIC program, without the ending $00            
AddBuffer:  ldy #$00
-loop:      lda OUTBUFFER,y
            jsr AddByte
            iny
            cpy IDX_OUT
            bne loop
buffer_out: rts 

; Find End of Program
FindEnd:    jsr NextLink        ; Get the next BASIC line location
            bcs get_line        ; If a line was found, advance line number and
            rts                 ;   link pointer and try again; else, return
get_line:   iny                 ; Get the line number and update it
            lda (X_PC),y        ;   it      
            sta LINE_NUM        ;   ,,
            iny                 ;   ,,
            lda (X_PC),y        ;   ,,
            sta LINE_NUM+1      ;   ,,
            lda $07             ; Get the next link pointer and update X_PC
            sta X_PC            ;   ,,
            lda $08             ;   ,,
            sta X_PC+1          ; Keep looking for the end
            jmp FindEnd

NextLink:   ldy #$00            ; Set locations $07 and $08 to the next
            lda (X_PC),y        ; BASIC line pointer. If both are $00, then
            sta $07             ; we're at the end of the BASIC program,
            iny                 ; otherwise, the BASIC program continues
            lda (X_PC),y        ; at the specified address
            sta $08             ; ,,
            sec                 ; Set Carry if the link isn't $00/$00
            lda $07             ; ,,
            bne mnext_r         ; ,,
            lda $08             ; ,,
            bne mnext_r         ; ,,
clc_r:      clc                 ; Otherwise clear it to indicate end of program
mnext_r:    rts
         
; Check Relative Instruction
; for relocatable byte syntax            
CheckRel:   ldx #$00            ; Check the instruction at the effective address
            lda (EFADDR,x)      ; ,,
            jsr Lookup          ; If it doesn't exist, exit
            bcc clc_r           ; ,,
            cmp #$c0            ; Is the instruction relative mode?
            bne clc_r           ; If not, exit
            lda #":"            ; Add a colon to indicate that bytes follow
            jsr CharOut         ; ,,
            jsr IncAddr         ; Add the instruction opcode to the buffer
            jsr Hex             ; ,,
            jsr Space           ; Space between instruction and operand
            jsr IncAddr         ; Add the operand to the buffer
            jsr Hex             ; ,,
            lda #";"            ; Show the mnemonic for the instruction as
            jsr CharOut         ;   a comment, for the reader's benefit
            jsr DMnemonic       ;   ,,
            sec                 ; Set Carry to indicate that a relative
            rts                 ;   instruction was handled   

; Check Range
; Check to see if X_PC is greater than or equal to Range End  
; In range is Carry is clear; out of range if Carry is set    
mChRange:   lda RANGE_END+1
            cmp EFADDR+1
            bcc out_range
            bne min_range
            lda EFADDR
            cmp RANGE_END
            rts
out_range:  sec
            rts
min_range:  clc
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CHARACTER GENERATOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CURBYTE     = $0247             ; Current byte value

uChar:      bcc Canvas
            lda #$00
            sta $07
            lda $0288
            sta $08
scanline:   lda #$00
            sta CURBYTE
            ldx #$80            ; Set bit 7
            ldy #$00
check:      lda ($07),y
            cmp #":"
            beq cgnext
            txa
            ora CURBYTE
            sta CURBYTE
cgnext:     iny
            txa
            lsr
            tax
            bne check
output:     ldy #$00
            lda CURBYTE
            sta (EFADDR),y
            jsr IncAddr
            lda $07
            cmp #$9a
            beq char_r
            lda #$16
            clc
            adc $07
            sta $07
            bcc scanline
char_r:     rts

; Draw Canvas
Canvas:     jsr $e55f           ; Clear screen
            ldy #$08
cnextline:  ldx #$08
            lda #":"
-loop:      jsr $ffd2
            dex
            bne loop
            lda #$0d
            jsr $ffd2
            dey
            bne cnextline
            rts
            
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WAXSCORE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; System Resources
VOICE       = $900b
VOLUME      = $900e             ; Sound volume register/aux color

; User Tool Storage
DURATION    = $0247             ; Current duration
OCTAVE      = $0248             ; Current octave (0,1)
LEGATO      = $0249             ; Legato On (if bit 7)
RECORD      = $024a             ; Record mode on

; Constants
WHOLE       = $80               ; Whole note
HALF        = $40               ; Half note
QUARTER     = $20               ; Quarter note
EIGHTH      = $10               ; Eighth note

uwAxScore:  bcs maddr_ok        ; Bail if no valid address was provided
            rts                 ; ,,
maddr_ok:   lda #$08            ; Set volume
            sta VOLUME          ; ,,
            lda #$00            ; Initialize
            sta LEGATO          ;   Legato
            sta OCTAVE          ;   Octave Number
            jsr CharGet         ; Check for Record mode
            cmp #"R"            ; ,,
            beq Record          ; ,,
            jmp Player          ; Otherwise, play at address

; Record Score
; See above for key bindings
Record:     lda #QUARTER        ; Starting duration
            sta DURATION        ; ,,
            lda #$80            ; Set recording flag
            sta RECORD          ; ,,
-loop:      jsr GetKey          ; Get keypress
proc_key:   jsr IsNote          ; Is it a note or rest?
            bcs add_note
            cmp #7              ; DEL - Undo
            bne ch_cmd          ; ,,
            jmp Undo            ; ,,
ch_cmd:     cmp #5              ; Minus - Octave Down
            beq octave_d        ; ,,
            cmp #61             ; Plus - Octave Up
            beq octave_u        ; ,,
            cmp #45             ; Colon - Legato On
            beq legato_on       ; ,,
            cmp #22             ; Semicolon - Legato Off
            beq legato_off      ; ,,
            cmp #15             ; RETURN - Effect Placeholder
            beq effect          ; ,,
            cmp #39             ; f1 - Eighth note
            beq set8            ; ,,
            cmp #47             ; f3 - Quarter note
            beq set4            ; ,,
            cmp #55             ; f5 - Half note
            beq set2            ; ,,
            cmp #63             ; f7 - Whole note
            beq set1            ; ,,
            cmp #37             ; Period - Dot the note
            beq dot             ; ,,
            cmp #24             ; STOP
            bne loop
            lda #$00            ; Clear the keyboard buffer so it doesn't
            sta $c6             ;   flush to the screen on exit
            ldx #$00            ; Add $00 to indicate end-of-score
            sta (EFADDR,x)      ; ,,
            jsr ShowData        ; ,,
            jsr IncAddr         ; Increment effective address
            jmp EAtoPC          ; Update the persistent counter and exit
; Add an effect placeholder or effect command        
effect:     lda #$0f            ; Enter an effect placeholder into memory
            .byte $3c           ; TOP (skip word)
octave_u:   lda #$1f            ; Octave up
            .byte $3c
octave_d:   lda #$2f            ; octave down
            .byte $3c
legato_on:  lda #$3f            ; Legato on
            .byte $3c
legato_off: lda #$4f            ; Legato off
            jmp add_note+3
            
; Set a note duration            
set8:       lda #EIGHTH
            .byte $3c           ; TOP (skip word)
set4:       lda #QUARTER
            .byte $3c           ; TOP (skip word)
set2:       lda #HALF
            .byte $3c           ; TOP (skip word)
set1:       lda #WHOLE
            sta DURATION
            bne loop
; Dot the current duration
dot:        lda DURATION        ; Take the current duration
            lsr                 ;   and shift it right
            ora DURATION        ; Then add the duration back to dot note
            and #$f0            ; Keep the duration in the high nybble
            sta DURATION
            jmp loop
; Add a note            
add_note:   ora DURATION
            ldx #$00
            sta (EFADDR,x)
            jsr ShowData        ; Show note data
            jsr PlayNote        ; Play the note at the selected duration
            lda #$00            ; Turn off the voice
            sta VOICE           ; ,,
            jsr IncAddr         ; Increment the address
            jsr EAtoPC          ; Update the persistent counter and go back
            jmp loop

; Simple wAxScore player
Player:     lsr RECORD          ; Turn off recording flag
            jsr ShowData        ; Show the note about to be played
            jsr PlayNote        ; Play it
            php
            jsr IncAddr         ; Increment the address
            jsr EAtoPC          ;   ,,
            plp
            bcc player_r        ; If end-of-score marker, then end
            lda $c5             ; Check STOP key
            cmp #24             ; ,,
            beq player_r        ; ,,
            jmp Player          ; Play next note
player_r:   lda #$00            ; Turn off the playing voice
            sta VOICE           ; ,,
            rts 
    
; Undo        
; Go back to previous note to correct it                        
Undo:       lda EFADDR
            sec
            sbc #$01
            sta EFADDR
            bcs show_prev
            dec EFADDR+1
show_prev:  jsr ResetOut        ; Show the previous address to verify undo
            lda #"U"            ;   action
            jsr CharOut         ;   ,,
            jsr ShowAddr        ;   ,,
            jsr PrintBuff       ;   ,,
            jsr EAtoPC
            jmp loop                       
 
; Get Key
; With debounce, and return as A
GetKey:     lda $c5             ; Make sure any previous keys are released first
            cmp #$40            ; ,,
            bne GetKey          ; ,,
-loop:      lda $c5             ; Now wait for an actual keypress start
            cmp #$40            ; ,,
            beq loop            ; ,,
            rts

; Is Note or Rest
; Set Carry and return degree in A if it's a note or rest
; Clear Carry and return original key code in A if it's something else
IsNote:     pha                 ; Save the original keypress
            ldx #$0d            ; Loop through 13 notes and 1 rest key to see
-loop:      cmp Degree,x        ;   if it matches one of the chromatic degrees
            beq found_deg       ;   ,,
            dex                 ;   ,,
            bpl loop            ;   ,,
            pla                 ; Loop ends without finding a note or rest key,
            clc                 ;   so pull the original key and send back to
            rts                 ;   look for other key presses
found_deg:  pla                 ; Found degree; pull original key and discard
            txa                 ; A is now the chromatic degree
            sec                 ; Set Carry to indicate note or rest was found
            rts

; Delay A Jiffies
Delay:      ldy $c5
            clc
            adc TIMER
-loop:      cpy $c5             ; Has the key pressed changed?
            beq ch_time         ; If not, keep the delay going
            ldy $c5             ; Has the key been lifted (#$40)?
            cpy #$40            ; ,,
            beq ch_time         ; If so, keep the delay going
            bit RECORD          ; If a new key is pressed, are we in record
            bpl ch_time         ; If not, keep the delay going
            pla                 ; Bypassing the rest of the delay process, so
            pla                 ;   remove return address for Delay from stack
            pla                 ; And also the return address for PlayNote
            pla                 ; ,,
            tya                 ; Put the key on the stack
            pha                 ; ,,
            lda #$00            ; Turn off the voice
            sta VOICE           ; ,,
            jsr IncAddr         ; Increment the address
            jsr EAtoPC          ; Update the persistent counter
            pla                 ; Get back the last key pressed, and process
            jmp proc_key        ;   new key as a command
ch_time:    cmp TIMER
            bne loop
            rts

; Play Note
; At the effective address
; Set Carry if the note was played successfully
; Clear Carry if this is the end of the score
PlayNote:   ldx #$00
            lda (EFADDR,x)
            beq end_score
            pha
            and #$0f            ; Mask away the duration
            tax                 ; X is the chromatic degree
            lda OCTAVE          ; Choose the note table based on the
            bne oct1            ;   selected octave
            lda Oct0,x          ;   ,,
            jmp play            ;   ,,
oct1:       lda Oct1,x          ;   ,,
play:       tay
            pla
            cpx #$0f            ; This is an effect, so process it
            beq wsEffect        ; ,,
            and #$f0            ; Mask away the degree, leaving A as the
                                ;   duration. Now, with a quarter note having a
                                ;   value of 32, simply treating that as a
                                ;   jiffy counter would give us a tempo of
                                ;   112 beats per minute, which is probably
                                ;   okay in this system. The IRQ player will
                                ;   give the developer more control over
                                ;   tempo in a production environment.
            ldx TIMER           ; Sync to the start of the next jiffy, within a
-loop:      cpx TIMER           ;   few cycles
            beq loop            ;   ,,
            sty VOICE           ; Play the voice
            jsr Delay           ; Delay A jiffies, see above for why
            bit LEGATO          ; If Legato is on, do not turn off the voice
            bmi play_r          ; ,,
            lda #$00            ; Stop the voice
            sta VOICE           ; ,,
play_r:     sec
            rts
end_score:  lda #$00            ; Turn off the voice
            sta VOICE           ; ,,
            clc                 ; Nothing left to play; clear Carry and return
            rts                 ; ,,
            
; Process Effect
; in high nybble of A
wsEffect:   and #$f0
            cmp #$00            ; No effect
            bne ch_oct_up
            jmp play_r
ch_oct_up:  cmp #$10            ; Octave Up
            bne ch_oct_dn
            inc OCTAVE
            jmp play_r
ch_oct_dn:  cmp #$20            ; Octave Down
            bne ch_leg_on
            dec OCTAVE
            jmp play_r
ch_leg_on:  cmp #$30            ; Legato On
            bne ch_leg_off
            lda #$80
            sta LEGATO
            jmp play_r
ch_leg_off: cmp #$40            ; Legato Off
            bne play_r
            lsr LEGATO
            jmp play_r        

; Show Note Data
; At the effective address
ShowData:   jsr ResetOut        ; Show the data as it's entered
            lda #"@"            ;   as a wAx data entry command
            jsr CharOut         ;   ,,
            jsr ShowAddr        ;   ,,
            lda #":"            ;   ,,
            jsr CharOut         ;   ,,
            ldx #$00            ;   ,,
            lda (EFADDR,x)      ;   ,,
            jsr Hex             ;   ,,
            jmp PrintBuff       ;   ,,
    
; Key to Degree         2    3       5    6     7
;                SPC Q     W   E  R    T     Y    U  I
Degree:     .byte 32,48,56,9,1,49,10,2,50,58,11,3,51,12

; Degree to Note Value Tables
; Determined by electronic tuner
Oct0:       .byte 0,133,139,146,152,158,164,169,173,178,182,187,190,194
Oct1:       .byte 0,194,197,201,204,207,209,212,214,217,219,221,223,225
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MEMORY CONFIG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
uConfig:    jsr ResetIn
            jsr CharGet
            cmp #"0"
            beq GO0K
            cmp #"3"
            beq GO3K
            cmp #"2"
            beq GO24K
            rts
GO0K:       lda #16
            .byte $3c           ; TOP (skip word)
GO3K:       lda #4
            sta $0282
            lda #30
            sta $0284
            sta $0288
            jmp $fd32           ; Soft reset
GO24K:      jmp $fd22           ; Hard reset
