;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                     wAx 8K
;                            Integrated Monitor Tools
;                           (c)2020-2023 Jason Justian
;                  
; Release 1  - May 16, 2020
; wAx2       - January 23, 2022
; wAx2.1     - October 26, 2023
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2020-2023 Jason Justian
; uRelocate code by Michael Kircher, 2020, used with permission and my thanks
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
MAX_SYM     = 19                ; Maximum number of user symbols + 1
MAX_FWD     = 12                ; Maximum number of forward references
PLUGINS     = 7                 ; Number of included plug-ins

; Tool Setup
TOOL_COUNT  = $1b               ; How many tools are there?
WEDGE       = "."               ; The wedge character
T_DIS       = "D"               ; Tool character D for disassembly
T_XDI       = "E"               ; Tool character E for extended opcodes
T_ASM       = "A"               ; Tool character A for assembly
T_ASM_AL    = ","               ;   Alias for assembly
T_MEM       = "M"               ; Tool character M for memory dump
T_BIN       = "%"               ; Tool character % for binary dump
T_TST       = $b2               ; Tool character = for tester
T_BRK       = "B"               ; Tool character B for breakpoint
T_REG       = "R"               ; Tool character R for register set
T_REG_AL    = ";"               ;   Alias for register set
T_EXE       = "G"               ; Tool character G for go
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
T_EXI       = "X"               ; Ersatz command for exit
T_HLP       = $99               ; Tool character ? for help (PRINT token)
SIGIL       = "@"               ; Symbol sigil (@)
FWD_NAME    = "&"               ; Forward reference name (&) 
WILDCARD    = "="               ; Search wildcard character (=)

; System resources - Routines
GONE        = $c7e4
CHRGET      = $0073
CHRGOT      = $0079
PRTFIX      = $ddcd             ; Print base-10 number
SYS_REG     = $e133             ; BASIC SYS start - restore regs
SYS_TAIL    = $e144             ; BASIC SYS end - save regs
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
CHKOUT      = $ffc9             ; Define file as output
CHRIN       = $ffcf             ; Get input
CLRCHN      = $ffcc             ; Close channel
ASCFLT      = $dcf3             ; Convert base-10 to FAC1
MAKADR      = $d7f7             ; FAC1 to Integer
DEVICE      = $ba               ; Save device
ISCNTC      = $ffe1             ; Check Stop key
FNDVAR      = $d0e7             ; Find variable
LODFAC      = $dba2             ; Move variable to FAC1
STORFAC     = $dbd4             ; Store FAC1 to memory
MAKFP       = $d391             ; 2-byte integer to FAC1
SGNFAC      = $dc2b             ; Sign of FAC1 (A=$ff if negative)
LAPLUS      = $d867             ; Add FAC2 to FAC1
CHRTST      = $d113             ; Check character between A and Z
PLOT        = $fff0             ; Get or set cursor position
TALK        = $ffb4             ; Open device command channel
TALKSA      = $ff96             ; TALK secondary address
IECIN       = $ffa5             ; Get byte from serial device
UNTALK      = $ffab             ; Close command channel

; System resources - Vectors and Pointers
IGONE       = $0308             ; Vector to GONE
CBINV       = $0316             ; BRK vector
BUFPTR      = $7a               ; Pointer to buffer
ERROR_PTR   = $22               ; BASIC error text pointer
SYS_DEST    = $14               ; Pointer for SYS destination

; System resources - Data
KEYWORDS    = $c09e             ; Start of BASIC kewords for detokenize
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
DEF_TOKEN   = $96               ; DEF token, in case user enters .DEFnn
TABLE_END   = $f2               ; Indicates the end of mnemonic table
XTABLE_END  = $d2               ; End of extended instruction table
QUOTE       = $22               ; Quote character
CR          = $0d               ; Carriage Return
CRSRUP      = $91               ; Cursor up
CRSRRT      = $1d               ; Cursor right
CRSRLF      = $9d               ; Cursor left
RVS_ON      = $12               ; Reverse on
RVS_OFF     = $92               ; Reverse off
HIGH_BYTE   = $b1               ; High Byte (>)
LOW_BYTE    = $b3               ; Low Byte (<)

; Assembler symbol table
; You can relocate and/or resize the symbol table by setting SYM_END,
; MAX_SYM, and MAX_FWD to meet your needs. The remaining labels will be
; set automatically, and you shouldn't need to touch them.
;
; Note that one of the labels is reserved for the forward reference symbol @&
; so add one more to MAX_SYM than you need.
ST_SIZE     = (MAX_SYM + MAX_FWD) * 3 + 1
SYMBOL_D    = SYM_END-ST_SIZE+1 ; Symbol label definitions
SYMBOL_AL   = SYMBOL_D+MAX_SYM  ; Symbol address low bytes
SYMBOL_AH   = SYMBOL_AL+MAX_SYM ; Symbol address high bytes
SYMBOL_F    = SYMBOL_AH+MAX_SYM ; Symbol unresolved forward references
SYMBOL_FL   = SYMBOL_F+MAX_FWD  ;   Forward reference low bytes
SYMBOL_FH   = SYMBOL_FL+MAX_FWD ;   Forward reference high bytes
OVERFLOW_F  = SYMBOL_FH+MAX_FWD ; Symbol unresolved reference overflow count

; wAx workspace
C_PT        = $03               ; Command Pointer (2 bytes)
USER_VECT   = $05               ; Plug-in vector (2 bytes)
WORK        = $a4               ; Temporary workspace (2 bytes)
MNEM        = $a4               ; Current Mnemonic (2 bytes)
W_ADDR      = $a6               ; Working Address (2 bytes)
CHARDISP    = $a8               ; Character display for Memory (2 bytes)
LANG_PTR    = $a8               ; Language Pointer (2 bytes)
PREV_IDX    = $a8               ; Previous index
SEARCH_S    = $a8               ; Search size
OPCODE      = $aa               ; Assembly target for hypotesting
OPERAND     = $ab               ; Operand storage (2 bytes)
IDX_OUT     = $ad               ; Buffer index - Output
IDX_IN      = $ae               ; Buffer index - Input
TOOL_CHR    = $af               ; Current function (T_ASM, T_DIS)
OUTBUFFER   = $0200             ; Output buffer (48 bytes)
INBUFFER    = $0230             ; Input buffer transcription (22 bytes)
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
jInstall:   jmp Install         ; a000
jHexGet:    jmp HexGet          ; a003
jCharGet:   jmp CharGet         ; a006
jCharOut:   jmp CharOut         ; a009
jHexOut:    jmp HexOut          ; a00c
jIncAddr:   jmp IncAddr         ; a00f
jIncCP:     jmp IncCP           ; a012
jLookup:    jmp Lookup          ; a015
jPrintBuff: jmp PrintBuff       ; a018
jResetIn:   jmp ResetIn         ; a01b
jResetOut:  jmp ResetOut        ; a01e
jShowAddr:  jmp ShowAddr        ; a021
jShowCP:    jmp ShowCP          ; a024
jAddr2CP:   jmp Addr2CP         ; a027
jPrintStr:  jmp PrintStr        ; a02a
jNext:      jmp Next            ; a02d
jDirectMode:jmp DirectMode      ; a030
jSizeOf:    jmp SizeOf          ; a033
jDisasm:    jmp Disasm          ; a036
jSyntaxErr: jmp SyntaxErr       ; a039
jSetUserVar:jmp SetUserVar      ; a03c

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; INSTALLER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
Install:    lda #<Banner        ; Print introduction message
            ldy #>Banner        ; ,,
            jsr PrintStr        ; ,,
            jsr has_exp         ; Determine whether to introduce this software
            beq nonwAxpand      ;   as wAxpander, based on RAM in Block 3
            lda #<wAxpander     ;   ,,
            ldy #>wAxpander     ;   ,,
            jsr PrintStr        ;   ,,
nonwAxpand: jsr Rechain         ; Rechain BASIC program
            jsr SetupVec        ; Set up vectors (IGONE and BRK)
            lda #DEF_DEVICE     ; Set default device number
            sta DEVICE          ; ,,
            ldx #<uRelocate     ; If this is not a wAxpander, the default user
            ldy #>uRelocate     ;   tool is the Relocator
            jsr has_exp         ;   ,,
            beq defmenu         ;   ,,
            ldx #<uConfig       ; If this is a wAxpander, the default user tool
            ldy #>uConfig       ;   is MEM CONFIG
defmenu:    stx USER_VECT       ;   ,,
            sty USER_VECT+1     ;   ,,
            jmp (READY)         ; Warm start with READY prompt
has_exp:    sei                 ; Just in case there's an IRQ handler here
            lda $2000           ; Test lowest byte of Block 1 RAM to
            inc $2000           ;   determine whether this software should
            cmp $2000           ;   introduce itself as "WAXPANDER"
            php                 ;   ,,
            dec $2000           ;   ,,
            plp                 ;   ,,
            cli
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
Main:       jsr CHRGET          ; Get the character from input or BASIC
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
            jsr SyntaxErr       ; Mode-appropriate syntax error
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
            sta BYTE_MOD        ; Clear byte modifier
            jsr Transcribe      ; Transcribe from CHRGET to INBUFFER
            lda #$ef            ; $0082 BEQ $008a -> BEQ $0073 (maybe)
            sta $83             ; ,,
RefrAddr:   jsr ResetIn         ; Re-initialize for buffer read
            jsr HexGet          ; Convert 2 characters to a byte   
            bcc main_r          ; Fail if the byte couldn't be parsed
            sta W_ADDR+1        ; Save to the W_ADDR high byte
            jsr HexGet          ; Convert next 2 characters to byte
            bcc main_r          ; Fail if the byte couldn't be parsed
            sta W_ADDR          ; Save to the W_ADDR low byte
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

; Mode-appropriate Syntax Error
; In direct mode, show a question mark and return to           
SyntaxErr:  jsr DirectMode      ; In a BASIC program, respond to illegal
            beq cmd_err         ;   commands with SYNTAX ERROR
            jmp SYNTAX_ERR      ;   ,,
cmd_err:    lda #"?"            ; In direct mode, respond to illegal commands
            jsr CHROUT          ;   with a question mark
            jsr CReturn         ;   ,,
            jmp Return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; COMMON LIST COMPONENT
; Shared entry point for Disassembler and Memory Dump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Handle Starting DEF
; As a special case, in which the user enters something like
;     .DEF00
; DEF is tokenized by BASIC, and wAx's detokenization doesn't start until
; Transcribe, where other instances of DEF are detokenized. So this handles that
; by forcing the high byte to EF, and then grabbing the low byte
DEF:        lda #T_DIS          ; Change the tool from $96 (DEF_TOKEN)
            sta TOOL_CHR        ;   to Disassemble
            lda #$ef            ; Set the high byte to $EF
            sta W_ADDR+1        ; ,,
            jsr ResetIn         ; Technically, the low byte of the start is the
            jsr HexGet          ;   first byte on the command line.
            sta W_ADDR          ;   ,,
            ; Fall through to List. Carry flag from HexGet maintains its meaning

List:       bcs from_addr       ; If no start address, continue list at W_ADDR
            lda INBUFFER
            beq list_cont
            jmp SyntaxErr
from_addr:  lda #$ff            ; Default range to top of memory
            sta RANGE_END       ; ,,
            sta RANGE_END+1     ; ,,
            jsr HexGet          ; 
            bcc start_list      ; If an optional end address is provided, set
            sta RANGE_END+1     ;   the end-of-range address
            jsr HexGet          ;   ,,
            bcc start_list      ;   ,,
            sta RANGE_END       ;   ,,
            cmp W_ADDR          ; Bail out immediately if the end range is
            lda RANGE_END+1     ;   lower than the start range
            sbc W_ADDR+1        ;   ,,
            bcs lrange_ok       ;   ,,
lrange_bad: jmp list_r          ;   ,,
lrange_ok:  ldx #$80            ; When X=$80, list won't stop after LIST_NUM
            bne ListLine        ;   lines, but will go through range unless STOP
list_cont:  lda C_PT            ; Otherwise, set the working addresss to the
            sta W_ADDR          ;  Command Pointer to continue listing
            lda C_PT+1          ;  after the last address
            sta W_ADDR+1        ;  ,,
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
            cmp #T_USR          ; User list tool
            beq to_usr          ; ,,
            cmp #T_INT          ; Text list tool
            beq to_int          ; ,,
            jsr Space           ; Space goes after address for Disassembly
            jsr Disasm
            jmp Next
to_mem:     lda #":"            ; Memory editor character goes after address
            jsr CharOut         ; ,,
            jsr Memory          ; Do Memory display
            jmp Next
to_bin:     jsr CharOut         ; Binary editor character goes after address
            jsr BinaryDisp      ; Do Binary display
            jmp Next
to_int:     jsr TextDisp        ; Do text display
            jmp Next
to_usr:     jmp (USER_VECT)
Next:       jsr PrintBuff      
            pla
            tax
            bpl skip_range      ; If X bit 7 is clear, don't check range
            inx                 ; Compensate for dex to keep bit 7 set
            lda RANGE_END       ; Check for end of range
            cmp W_ADDR          ; ,,
            lda RANGE_END+1     ; ,,
            sbc W_ADDR+1        ; ,,
            bcc ch_shift        ; ,,
skip_range: jsr ISCNTC          ; Exit if STOP key is pressed
            beq list_stop       ; ,,          
            dex                 ; Exit if loop is done
            bne ListLine        ; ,,
            inx                 ; But if the loop is done, but a Shift key
ch_shift:   lda KEYCVTRS        ;   is engaged, then go back for one more
            and #$01            ;   ,,
            bne ListLine        ;   ,,
            lda TOOL_CHR        ; If the breakpoint was set, don't update
            cmp #T_BRK          ;   the Command Pointer or show a tool
            beq list_r          ;   prompt
list_stop:  lda #WEDGE          ; Provide a tool for the next page in the key-
            sta KEYBUFF         ;   board buffer
            lda TOOL_CHR        ;   ,,
            sta KEYBUFF+1       ;   ,,
            lda #CRSRLF         ;   ,,
            sta KEYBUFF+2       ;   ,,
            lda #$03            ;   ,,
            sta KBSIZE          ;   ,,
            jsr Addr2CP         ; Update Command Pointer with working addr
list_r:     jmp EnableBP        ; Re-enable breakpoint, if necessary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; DISASSEMBLER
; https://github.com/Chysn/VIC20-wAx2/wiki/6502-Disassembler
; https://github.com/Chysn/VIC20-wAx2/wiki/Undocumented-Instruction-Support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Disassemble
; Disassemble a single instruction at the working address
Disasm:     jsr IncAddr         ; Get opcode
            jsr Lookup          ; Look it up
            bcc Unknown         ; Clear carry indicates an unknown opcode
            pha                 ; Store addressing mode for later
            jsr DMnemonic       ; Display mnemonic
            jsr Space           ; Space after mnemonic
disasm_op:  pla                 ; Pass addressing mode to operand routine
            jmp DOperand        ; Display operand

; Unknown Opcode
Unknown:    lda #":"            ; Memory entry before an unknown byte
            jsr CharOut         ; ,,
            lda OPCODE          ; The unknown opcode is still here   
            jmp HexOut             
            
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
            adc #"@"            ; Get the PETSCII character and
            jsr CharOut         ;   output it
            dex                 ; Get the next five-bit chunk
            bne loop            ; ,,
            pla                 ; Restore the mnemonic code to its pre-
            sta MNEM            ;   un-encoded form
            pla                 ;   ,,
            sta MNEM+1          ;   ,,
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
            adc W_ADDR          ; ,,
            sta WORK            ; ,,
            lda WORK+1          ; ,,
            adc W_ADDR+1        ; ,,
            jsr HexOut          ; No need to save the high byte, just show it
            lda WORK            ; Show the low byte of the computed address
            jmp HexOut          ; ,,
                            
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
; MEMORY EDITOR
; https://github.com/Chysn/VIC20-wAx2/wiki/Memory-Editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Memory Editor               
MemEdit:    ldy #$0             ; Byte index
            sei                 ; Stop interrupts during memory update
-loop:      jsr HexGet   
            bcc edit_exit       ; Bail out on the first non-hex byte
            sta (W_ADDR),y      
            iny
            cpy #8
            bne loop
edit_exit:  cli                 ; Re-enable interrupts after memory update
            cpy #$00
            beq asm_error
            tya
            tax
            jsr Prompt          ; Prompt for the next address
            jsr ClearBP         ; Clear breakpoint if anything was changed
edit_r:     rts

; Screen Code Editor
; Like text editor, but writes screen codes
ScrEdit:    jsr CharGet         ; For a screen code editor, the character
            cmp #QUOTE          ;   after / must be a quote. After this, it
            bne asm_error       ;   mostly behaves like a text editor, except
            sec                 ;   that it converts PETSCII to screen code
            ror CHARAC          ;   ,,
            .byte $3c           ; Skip the clearing of CHARAC below
            ; Fall through to TextEdit

; Text Editor
; If the input starts with a quote, add characters until we reach another
; quote, or 0
TextEdit:   lsr CHARAC
            ldy #$00            ; Y=Data Index
-loop:      jsr CharGet
            beq edit_exit       ; Return to MemEditor if 0
            cmp #QUOTE          ; Is the character a quotation mark? 
            bne non_quote       ; ,,
            jsr CharGet         ; If so, check the next charater
            cmp #0              ; If that's the end, treat the last quote mark 
            beq edit_exit       ;   as a string delimiter
            dec IDX_IN          ; If anything is next, back up the index and add
            lda #QUOTE          ;   a real quotation mark
non_quote:  bit CHARAC          ; CHARAC bit 7 is high if this is a screen code
            bpl skip_conv       ;   editor
            bit main_r          ; BIT #$60. Check for control character and
            beq loop            ;   ignore control characters here
            jsr PETtoScr        ; Perform the conversion
skip_conv:  sta (W_ADDR),y      ; Populate data
            iny
            cpy #16             ; String size limit
            beq edit_exit
            jmp loop
            
; Binary Editor
; If the input starts with a %, get one binary byte and store it in memory                   
BinaryEdit: jsr BinaryByte      ; Get 8 binary bits
            ;bcc edit_r         ; If invalid, errors at BinaryByte
            ldy #$00            ; Store the valid byte to memory
            sta (W_ADDR),y      ; ,,
            iny                 ; Increment the byte count and return to
            jmp edit_exit       ;   editor            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; ASSEMBLER
; https://github.com/Chysn/VIC20-wAx2/wiki/6502-Assembler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Centrally-located error jump
asm_error:  jmp ASM_ERROR

; Assemble 6502 Instruction
; Or data
Assemble:   bcs ch_return       ; Bail if the address is no good
            lda INBUFFER        ; Permit comment at the beginning of a line
            beq asm_r           ;   of assembly
            bne asm_error       ;   ,,
ch_return:  lda INBUFFER+4      ; If user pressed RETURN at an assembly prompt,
            beq asm_r           ;   go back to wAx prompt
-loop:      jsr CharGet         ; Look through the buffer for either
            beq test            ;   0, which should indicate implied mode, or:
            ldy IDX_IN          ; If we've gone past the first character after
            cpy #$05            ;   the address, no longer pay attention to
            bne op_parts        ;   pre-data stuff
            cmp #SIGIL          ; @ = New symbol
            beq DefSymbol       ; ,,
            cmp #":"            ; Colon = Byte entry (route to hex editor)
            bne ch_txt          ; ,,
            jmp MemEdit         ; ,,
ch_txt:     cmp #QUOTE          ; " = Text entry (route to text editor)
            beq TextEdit        ; ,,
            cmp #T_BIN          ; % = Binary entry (route to binary editor)
            beq BinaryEdit      ; ,,
            cmp #"/"            ; / = Screen code entry (route to screen code
            bne op_parts        ;   editor)
            jmp ScrEdit         ;   ,,
op_parts:   cmp #"#"            ; # = Parse immediate operand (quotes and %)
            beq ImmedOp         ; ,,         
            cmp #"$"            ; $ = Parse the operand
            bne loop            ; ,,
main_op:    jsr GetOperand      ; Once $ is found, then grab the operand
test:       jsr Hypotest        ; Line is done; hypothesis test for a match
            bcc asm_error       ; Clear carry means the test failed
            ldy #$00            ; A match was found! Transcribe the good code
            lda OPCODE          ;   to the working address. The number of
            sta (W_ADDR),y      ;   bytes to transcribe is stored in the
            ldx INSTSIZE        ;   INTSIZE location.
            cpx #$02            ; Store the low operand byte, if indicated
            bcc nextline        ; ,,
            lda OPERAND         ; ,,
            iny                 ; ,,
            sta (W_ADDR),y      ; ,,
            cpx #$03            ; Store the high operand byte, if indicated
            bcc nextline        ; ,,
            lda OPERAND+1       ; ,,
            iny                 ; ,,
            sta (W_ADDR),y      ; ,,
nextline:   jsr ClearBP         ; Clear breakpoint on successful assembly
            jsr Prompt          ; Prompt for next line if in direct mode
asm_r:      rts

; Define Label
; Create a new symbol entry, and resolve any forward references to the
; new symbol.
DefSymbol:  jsr CharGet         ; Get the next character after the symbol sigil
            jsr SymbolIdx       ; Get an index for the symbol in A
have_sym:   jsr IsDefined       ; If this symbol is not yet defined, then
            bne is_def          ;   resolve the forward reference, if it
            sty IDX_SYM         ;   was used
            jsr ResolveFwd      ;   ,,
            ldy IDX_SYM
is_def:     lda W_ADDR          ; Set the symbol address
            sta SYMBOL_AL,y     ; ,,
            lda W_ADDR+1        ; ,,
            sta SYMBOL_AH,y     ; ,,
            jsr CharGet
            cmp #$00
            bne pull_code
            ldx #$00            ; Return to BASIC or prompt for the same
            jmp Prompt          ;   address again
pull_code:  ldy #$00            ; If there's code after the symbol, pull it
-loop:      iny                 ;   two spaces, replacing the symbol. This
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
try_slash:  cmp #"/"            ; If it's a quote preceeded by a slash, treat
            bne try_quote       ;   the next quoted string as a screen code
            ldy IDX_IN          ;   rather than a character code
            dey                 ;   ,,
            lda #1              ;   ,,
            sta INBUFFER,y      ;   ,,
            jsr CharGet         ;   ,,
            cmp #QUOTE          ;   ,, If the character after / is't a quote,
            bne ASM_ERROR       ;   ,,   that's an error
            jsr CharGet         ;   ,, Get the next character after the quote,
            jsr PETtoScr        ;   ,,   and convert it to a screen code
            jmp close_qu        ;   ,,
try_quote:  cmp #QUOTE          ; If it's a double quote, make sure it's a one
            bne try_binary      ;   character surrounded by quotes. If it is,
            jsr CharGet         ;   set it as the operand and convert it to
close_qu:   sta OPERAND         ;   hex for the hypotester
            jsr CharGet         ;   ,,
            cmp #QUOTE          ;   ,,
            bne ASM_ERROR       ;   ,, Error if the second quote isn't here
            beq insert_hex      ;   ,,
try_binary: cmp #"%"            ; If it's a binary prefix sigil %, convert
            bne try_base10      ;   the eight binary bits and, if valid,
            jsr BinaryByte      ;   set the operand and convert it to hex
            ;bcc ASM_ERROR      ;   ,, (errors at BinaryByte)
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
            bcs ASM_ERROR       ;   ,,
            jsr ASCFLT          ; Convert the buffer text into FAC1
            jsr MAKADR          ; Convert FAC1 to 16-bit unsigned integer
            cmp #$00            ; High byte from MAKADR is in A
            bne ASM_ERROR       ; Error if high byte is set; too big for immed
            sty OPERAND         ; Low byte from MAKADR is in Y
            pla                 ; Put the CHRGET buffer back so BASIC doesn't
            sta $7a             ;   freak out
            pla                 ;   ,,
            sta $7b             ;   ,,
            cpy #10             ; If the decimal operand was 10 or more, advance
            bcc insert_hex      ;   the index to account for the second digit.
            inc IDX_IN          ; Advance the index to the next location to
            cpy #100            ;   check for + or -. If the decimal operand was
            bcc insert_hex      ;   100 or more, advance the index again to
            inc IDX_IN          ;   account for the third digit.
            ; Fall through to insert_hex
insert_hex: lda IDX_IN          ; Save starting index for arithmetic operand
            sta PREV_IDX        ; ,,
            jsr Arithmetic      ; Perform arithmetic on operand and code
            jsr ResetOut        ; Store the hex value of the operand after the
            sta INBUFFER+11     ;   #, so it can be matched by Hypotest.
            lda #"$"            ;   End it with 0 as a line delimiter
            sta INBUFFER+8      ;   ,,
            lda OPERAND         ;   ,,
            jsr HexOut          ;   ,,
            lda OUTBUFFER       ;   ,,
            sta INBUFFER+9      ;   ,,
            lda OUTBUFFER+1     ;   ,,
            sta INBUFFER+10     ;   ,,
            jmp test
            
; Error Message
; Invalid opcode or formatting (ASSEMBLY)
; Failed boolean assertion (MISMATCH, borrowed from ROM)
ASM_ERROR:  ldx #$00            ; ?ASSMEBLY ERROR
            .byte $3c           ; TOP (skip word)
MIS_ERROR:  ldx #$01            ; ?MISMATCH ERROR
            .byte $3c           ; TOP (skip word)
SYM_ERROR:  ldx #$02            ; ?SYMBOL ERROR
            .byte $3c           ; TOP (skip word)
RES_ERROR:  ldx #$03            ; ?CAN'T RESOLVE ERROR 
            .byte $3c           ; TOP (skip word)
TOO_FAR_ER: ldx #$04            ; ?TOO FAR ERROR
            lda ErrAddr_L,x
            sta ERROR_PTR
            lda ErrAddr_H,x
            sta ERROR_PTR+1
            jmp CUST_ERR          

; Get Operand
; Populate the operand for an instruction
GetOperand: lda IDX_IN          ; Save starting index for arithmetic operand
            sta PREV_IDX        ; ,,
            lda #1              ; Operand size-1 for arithmetic conversion
            sta INSTSIZE        ; ,,
            jsr HexGet          ; Get the first byte
            bcc getop_r         ; If invalid, return
            sta OPERAND+1       ; Default to being high byte
            jsr HexGet   
            bcs high_byte       ; If an 8-bit operand is provided, the first
            lda OPERAND+1       ;   byte is the low byte
            dec INSTSIZE        ; Set to 0 if only one operand byte
            dec IDX_IN          ; If there's one byte, step back the input index
high_byte:  sta OPERAND         ;   set the low byte with the input
Arithmetic: jsr CharGet         ; Check character after operand
            cmp #"+"            ; Perform addition
            beq add_op          ; ,,
            cmp #"-"            ; Perform subtraction
            beq sub_op          ; ,,
getop_r:    rts
sub_op:     jsr HexGet
            bcc getop_r
            sta TEMP_CALC
            sec
            lda OPERAND
            sbc TEMP_CALC
            sta OPERAND
            bcs repl_hex
            dec OPERAND+1
            jmp repl_hex
add_op:     jsr HexGet
            bcc getop_r
            clc                 ; Add the number to the operand
            adc OPERAND         ; ,,
            sta OPERAND         ; ,,
            bcc repl_hex        ; ,,
            inc OPERAND+1       ; ,,
repl_hex:   ldx IDX_IN
            lda #" "            ; Place spaces where arithmetic operators and
            sta INBUFFER-3,x    ;   operands were, so they're ignored by the
            sta INBUFFER-2,x    ;   assembler matcher
            sta INBUFFER-1,x
            jsr ResetOut        ; Will be using the output buffer as tmp hex
            ldx PREV_IDX        ; Starting index of instruction operand
            stx IDX_IN
            lda INSTSIZE        ; If only one byte was provided (INSTSIZE=0),
            beq pl1             ;   then update only the low byte (OPERAND)
            lda OPERAND+1       ; Otherwise, there are two bytes, so start by
            jsr HexOut          ;   updating the high byte
            jsr CopyOp          ;   ,,
            jsr ResetOut        ; Same as above, reset the output buffer, which
pl1:        lda OPERAND         ;   holds the hex value of the low byte
            jsr HexOut          ;   ,,
            ; Fall through to CopyOp
 
; Copy Hex
; From the output buffer to the input buffer           
CopyOp:     lda OUTBUFFER       ; Get first hex digit of output buffer
            jsr AddInput        ;   Copy it to input
            lda OUTBUFFER+1     ; Get second hex digit of output buffer
            jmp AddInput        ;   Copy it to input
                       
; Hypothesis Test
; Search through the language table for each opcode and disassemble it using
; the opcode provided for the candidate instruction. If there's a match, then
; that's the instruction to assemble at the working address. If Hypotest tries
; all the opcodes without a match, then the candidate instruction is invalid.
Hypotest:   jsr ResetLang       ; Reset language table
reset:      ldy #$06            ; Offset disassembly by 6 bytes for buffer match   
            sty IDX_OUT         ;   b/c output buffer will be "$00AC INST"
            lda #OPCODE         ; Write location to working addr for hypotesting
            sta W_ADDR          ; ,,
            ldy #$00            ; Set the working address high byte
            sty W_ADDR+1        ; ,,
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
match:      lda W_ADDR          ; Set the INSTSIZE location to the number of
            sec                 ;   bytes that need to be programmed
            sbc #OPCODE         ;   ,,
            sta INSTSIZE        ;   ,,
            jmp RefrAddr        ; Restore the working address to target addr
test_rel:   lda #$0a            ; For relative branch instructions, first check
            sta IDX_OUT         ;   the name of the instruction. If that checks
            jsr IsMatch         ;   out, compute the relative branch offset and
            bcc reset           ;   insert it into memory, if it's within range
            jsr RefrAddr        ;   ,,
            jsr ComputeRB       ;   ,,
            sty OPERAND         ;   ,,
            lda #$02            ;   ,, 
            sta INSTSIZE        ;   ,,
            sec                 ; Set carry to indicate success
            rts
bad_code:   clc                 ; Clear carry flag to indicate failure
            rts
            
; Compute Relative Branch Offset
; With branch instruction in W_ADDR and target in OPERAND
; Return offset in Y if valid, or error message if too far
ComputeRB:  lda W_ADDR+1        ; Stash the working address, as the offset
            pha                 ;   is computed from the start of the next
            lda W_ADDR          ;   instruction
            pha                 ;   ,,
            jsr IncAddr         ; W_ADDR += 2
            jsr IncAddr         ; ,,
            lda OPERAND         ; Subtract operand from working address
            sec                 ;   to get offset
            sbc W_ADDR          ;   ,,
            tay                 ;   ,, (Y will be the RB offset)
            lda OPERAND+1       ;   ,,
            sbc W_ADDR+1        ;   ,,
            tax                 ;   ,,
            pla                 ; Put back the tool's working address
            sta W_ADDR          ; ,,
            pla                 ; ,,
            sta W_ADDR+1        ; ,,
            cpx #$ff            ; Check the range; the difference must be
            beq neg             ;   between $ff80 and $007f, inclusive
            cpx #$00            ;   ,,
            beq pos             ;   ,,
rb_err:     lda IGNORE_RB       ; Allow RB target to be out-of-range ($0000)
            bne compute_r       ;   if this is a forward reference
            jmp TOO_FAR_ER      ; ?TOO FAR error if out of range
neg:        cpy #$80
            bcc rb_err
            rts
pos:        cpy #$80
            bcs rb_err
compute_r:  rts            
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MEMORY DISPLAY
; https://github.com/Chysn/VIC20-wAx2/wiki/Memory-Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Memory:     ldy #$00            ; Y is the offset for the address start
-loop:      cpy #0              ; For the first location and third locations,
            beq r_on            ;   turn reverse on, otherwise, turn reverse
            cpy #2              ;   off.
            beq r_on            ;   ,,
            lda #RVS_OFF        ;   ,,
            .byte $3c           ;   ,, TOP (skip word) 
r_on:       lda #RVS_ON         ;   ,,
            jsr CharOut         ;   ,,
            lda (W_ADDR),y      ; Get the value at the offset
            sta CHARDISP,y      ;   Put that value in the right-hand display
            jsr HexOut          ;   and then show the value
            iny                 ; Move to the next value
            cpy #$04            ; Show the right-size characters if done
            beq show_char       ; ,,
            jmp loop            ; ,, otherwise continue
show_char:  lda #";"            ; Comment after hex values
            jsr CharOut         ; ,,
            jsr ReverseOn       ; Reverse interpreted characters
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
; BINARY DISPLAY
; https://github.com/Chysn/VIC20-wAx2/wiki/Memory-Display#binary-display
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
            jsr Semicolon
            pla
            jmp HexOut

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; ASSERTION TESTER/QUICK PEEK
; https://github.com/Chysn/VIC20-wAx2/wiki/Assertion-Tester
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Tester:     bcs get_bytes       ; If there's an address supplied, get more bytes
            jsr ResetIn         ; Otherwise, see if only a single byte was
            jsr HexGet          ;   provided for Quick Peek address
            bcs low_peek        ; If so, handle low-byte peek
            jmp SyntaxErr
low_peek:   sta W_ADDR          ; Treat the single address as the low byte,
            lda #0              ;   setting the high byte to 0
            sta W_ADDR+1        ;   ,,
get_bytes:  ldy #$00            ; Start with 0 index
            jsr HexGet          ; Is there a byte after the address?
            bcs add_test        ; If so, insert it into the test, look for more
            jsr ResetOut        ; If there's only an address, show the value
            jsr wAxPrompt       ; Show values in an editor command     
            jsr Comma           ; ,,
            jsr Space           ; ,,
            jsr ShowAddr        ; ,,
            lda #":"            ; ,,
            jsr CharOut         ; ,,
            lda (W_ADDR),y      ; Show two values
            jsr HexOut          ;   ,,
            jsr Space           ;   separated by a space
            iny                 ;   ,,
            lda (W_ADDR),y      ;   ,,
            jsr HexOut          ;   ,,
            jmp PrintBuff       ;   ,,
-loop:      jsr HexGet   
            bcc test_r          ; Bail out on the first non-hex byte
add_test:   cmp (W_ADDR),y      ; Compare byte at working address to hex
            bne test_err        ;   Error if wrong
            jsr IncAddr         ; Go to next address
            jmp loop            ; ,,
test_r:     jmp Addr2CP         ; Update CP and BASIC variable
test_err:   jmp MIS_ERROR       ; ?MISMATCH ERROR on failed test

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; GO
; https://github.com/Chysn/VIC20-wAx2/wiki/Go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Go:         bcs go_param        ; If address was provided, use that
            lda SYS_DEST        ; Otherwise, continue from the previous
            sta W_ADDR          ;   PC specified in the SYS destination
            lda SYS_DEST+1      ;   pointer
            sta W_ADDR+1        ;   ,,
            pla                 ; Pull return address to Return, to restore
            pla                 ;   stack to pre-BRK state
            jmp SYS_REG
go_param:   lda W_ADDR          ; Set the temporary INT storage to the program
            sta SYS_DEST        ;   counter. This is what SYS uses for its
            lda W_ADDR+1        ;   execution address, and I'm using that
            sta SYS_DEST+1      ;   system to borrow saved Y,X,A,P values
            jsr SetupVec        ; Make sure the BRK handler is enabled
            jsr SYS_REG         ; Call BASIC SYS, after the parameter parsing
            jsr SYS_TAIL        ; Save registers after return
            ; Fall through to register display
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; REGISTER EDITOR
; https://github.com/Chysn/VIC20-wAx2/wiki/Register-Editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Register Display            
RegDisp:    cld                 ; Escape hatch for accidentally-set Decimal flag
            ;Register display header
            lda #<Registers     ; Print register display bar
            ldy #>Registers     ; ,,
            jsr PrintStr        ; ,,
            jsr ResetOut        ; ,,
            
            ; Status flag display
            ldy #$07            ; Print processor flag statuses
            lda #$80            ; Bit value of flag (from bit 7 to 0)
-loop:      pha                 ; ,,
            bit ACC+3           ; Is this status flag on?
            beq pfoff           ; ,,
            lda #RVS_ON         ; If so, show it in reverse
            .byte $3c           ; TOP (skip word)
pfoff:      lda #RVS_OFF        ; Reverse off = flag is off
            jsr CharOut         ; ,,
            lda PFNames,y       ; If the flag isn't in the name list, don't show
            jsr CharOut         ;   flag (I and B are not shown, for example)
            pla                 ; Shift to the next flag's bit value
            lsr                 ; ,,
            dey                 ; Decrement the flag index
            bpl loop            ; If there's another flag, go back for it
            jsr PrintBuff       ; Otherwise, flush the status flags to display
            
            ; Show register values (A, X, Y, P), stack pointer, and PC
            jsr ResetOut        ; Reset output for register value line
            jsr wAxPrompt       ; ,,
            jsr Semicolon       ; ,,
            ldy #0              ; Y is the register index from the SYS locations
-loop:      lda ACC,y           ; Get register values from storage and add
            jsr HexOut          ;   each one to the buffer. These values came
            jsr Space           ;   from the hardware IRQ, and are A,X,Y,P
            iny                 ;   ,,
            cpy #$04            ;   ,,
            bne loop            ;   ,,
            tsx                 ; Add stack pointer to the buffer
            txa                 ; ,,
            jsr HexOut          ; ,,
            jsr Space
            lda SYS_DEST+1      ; Print high byte of SYS destination
            jsr HexOut          ; ,,
            lda SYS_DEST        ; Print low byte of SYS destination
            jsr HexOut          ; ,,
            jmp PrintBuff       ; Print the buffer
            
; Set register values
; In the order AA [XX] [YY] [PR] [ST] [PC]
; The stack pointer value is not actually set 
; Keep a register the same by entering == in its place
; Note that no syntax error is returned for invalid input. This is so that
; .; can be used as a comment within BASIC code.
Register:   jsr ResetIn         ; Reset in because allowing a single byte
            jsr CharGet         ; If alone on the line, show register display
            beq RegDisp         ; ,,
            jsr HexGet+3        ; Get A. +3 because CharGet has been done!
            bcc ch_x            ; If no A, check X
            sta ACC             ; Store A in SYS A
ch_x:       jsr HexGet          ; Get X
            bcc ch_y            ; If no X, check for Y
            sta XREG            ; Store X in SYS X
ch_y:       jsr HexGet          ; Get Y
            bcc ch_proc         ; If no Y, check for Processor Status
            sta YREG            ; Store Y in SYS Y
ch_proc:    jsr HexGet          ; Get Processor Status
            bcc ch_sp           ; If no Processor Status, check for Stack Ptr
            sta PROC            ; Store Status in SYS Status
ch_sp:      jsr HexGet          ; Stack pointer byte is pulled, but is not
            bcc ch_addr         ;   observed, because it messes stuff up
ch_addr:    jsr HexGet          ; Get and set SYS PC
            bcc register_r      ; ,,
            sta SYS_DEST+1      ; ,,
            jsr HexGet          ; ,,
            bcc register_r      ; ,,
            sta SYS_DEST
register_r: rts
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; BREAKPOINT MANAGER
; https://github.com/Chysn/VIC20-wAx2/wiki/Breakpoint-Manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetBreak:   bcs set_bp          ; Set breakpoint if a valid address is provided
            lda INBUFFER        ; If nothing is after the B, show the
            beq show_bp         ;   breakpoint
            cmp #"-"            ; If - is after the B, clear the breakpoint
            bne vec_r           ; ,,
            jsr ClearBP         ; ,,
show_bp:    jsr SetupVec        ; Turn on BRK trapping
            lda BREAKPOINT+2    ; Is a breakpoint set?
            beq vec_r           ; If not, just return
            lda BREAKPOINT      ; If so, populate working address with
            sta W_ADDR          ;   breakpoint, and show the line of code
            lda BREAKPOINT+1    ;   ,,
            sta W_ADDR+1        ;   ,,
            jmp showcode        ;   ,,
set_bp:     jsr ClearBP         ; Clear the old breakpoint, if it exists
            lda W_ADDR          ; Add a new breakpoint at the working address
            sta BREAKPOINT      ; ,,
            lda W_ADDR+1        ; ,,
            sta BREAKPOINT+1    ; ,,
            ;ldy #$00           ; (Y is already 0 from ClearBP)
            lda (W_ADDR),y      ; Stash it in the Breakpoint data structure,
            sta BREAKPOINT+2    ;   to be restored on the next break
            tya                 ; Write BRK to the breakpoint location
            sta (W_ADDR),y      ;   ,,
            lda (W_ADDR),y      ; If the byte isn't set to 0, do not continue
            beq bp_ok           ; ,,
            sty BREAKPOINT+2    ; Clear breakpoint but continue
bp_ok:      lda #CRSRUP         ; Cursor up to overwrite the command
            jsr CHROUT          ; ,,
showcode:   jsr DirectMode      ; When run inside a BASIC program, skip the
            bne vec_r           ;   BRK line display
            ldx #$01            ; List a single line for the user to review
            jsr ListLine        ; ,,
            ; Fall through to SetupVec

; Set Up Vectors
; Used by installation, and also by the breakpoint manager                    
SetupVec:   lda #<Main          ; Intercept GONE to process wedge
            sta IGONE           ;   tool invocations
            lda #>Main          ;   ,,
            sta IGONE+1         ;   ,,
            lda #<Break         ; Set the BRK interrupt vector
            sta CBINV           ; ,,
            lda #>Break         ; ,,
            sta CBINV+1         ; ,,
vec_r:      rts

; BRK Trapper
; Replaces the default BRK handler. Gets registers from hardware interrupt
; and puts them in the SYS register storage locations. Gets program counter
; and stores it in the Command Pointer location. Then falls through
; to register display.
Break:      pla                 ; Get values from stack and put them in the
            tay                 ;   proper registers
            pla                 ;   ,,
            tax                 ;   ,,
            pla                 ;   ,,
            plp                 ; Get the processor status
            jsr SYS_TAIL        ; Store regiters in SYS locations
            pla                 ; Get Program Counter from interrupt and put
            sta SYS_DEST        ;   it in the SYS destination
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
            lda (W_ADDR),y      ; ,,
            bne ind_r           ; If not, do nothing
            lda BREAKPOINT      ; If it is a BRK, is it our breakpoint?
            cmp W_ADDR          ; ,,
            bne ind_r           ; ,,
            lda BREAKPOINT+1    ; ,,
            cmp W_ADDR+1        ; ,,
            bne ind_r           ; ,,
            jsr ReverseOn       ; Reverse on for the breakpoint
            lda BREAKPOINT+2    ; Temporarily restore the breakpoint byte
            sta (W_ADDR),y      ;   for disassembly purposes
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
; DISK,TAPE,SD STORAGE
; https://github.com/Chysn/VIC20-wAx2/wiki/Disk-Tape-SD-Storage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MemSave:    bcc save_err        ; Bail if the address is no good
            jsr HexGet          ; Convert 2 characters to a byte   
            bcc save_err        ; Fail if the byte couldn't be parsed
            sta RANGE_END+1     ; Save to the range high byte
            jsr HexGet          ; Convert next 2 characters to byte
            bcc save_err        ; Fail if the byte couldn't be parsed
            sta RANGE_END       ; Save to the range low byte
            jsr FileSetup       ; SETLFS, get filename length, etc.  
            ldx #<INBUFFER+9    ; ,,
            ldy #>INBUFFER+9    ; ,,
            jsr SETNAM          ; ,,
            lda #W_ADDR         ; Set up SAVE call
            ldx RANGE_END       ; ,,
            ldy RANGE_END+1     ; ,,
            jsr SAVE            ; ,,
            bcs FileError
            jmp CReturn 
save_err:   jmp SyntaxErr  

; Show System Disk Error            
FileError:  bne show_error      ; Error in A will be $00 if a cassette save is
            lda #$1e            ;   stopped, so override that to ?BREAK ERROR
show_error: jmp ERROR_NO 
            
; Memory Load
MemLoad:    jsr DropDown        ; Find the bottom of a file list, for example
            jsr ResetIn         ; Reset the input buffer because there's no addr
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
            sta C_PT
            jsr CHRIN
            sta C_PT+1
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
            bne file_r          ;   direct mode
show_range: jsr ResetOut
            jsr CReturn 
            jsr AddrPrefix      ; Show address prefix with prompt
            ldx DEVICE          ; If the device numbr is 1, skip the start/end
            cpx #$01            ;   display
            bne disk
            ldx $033d           ; Update the Command Pointer with the start
            stx C_PT            ;   of memory loaded from cassette
            ldx $033e           ;   ,,
            stx C_PT+1          ;   ,,
disk:       jsr ShowCP          ; Show the Command Pointer
            jsr Space           ; Space between start and end
            lda $af             ; Show the end of the loaded range
            jsr HexOut          ; ,,
            lda $ae             ; ,,
            jsr HexOut          ; ,,
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
file_r:     rts
setup_err:  jmp save_err
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; SEARCH
; https://github.com/Chysn/VIC20-wAx2/wiki/Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Search:     bcs s_addr_ok       ; Syntax error if no good address
search_err: jmp SyntaxErr       ; ,,
s_addr_ok:  lda INBUFFER+4      ; Or nothing to search
            beq search_err      ; ,,
            lda #SEARCH_L       ; Move search limit to countdown
            sta SEARCH_C        ; ,,
            lda #0              ; Initialize search size (for memory and string
            sta SEARCH_S        ;   searches
            sta WORK+1          ; Set to 1 for screen code search
            jsr ResetOut        ; Reset output
            lda INBUFFER+4      ; What kind of search is this?
            cmp #":"            ;   If a hex search, do setup      
            bne s_ch_scc        ;   ,,
            jmp SetupHex        ;   ,,
s_ch_scc:   cmp #"/"            ;   If a screen code search, do setup
            bne s_ch_quote      ;   ,,
            inc WORK+1          ;   ,,
            lda INBUFFER+5      ;   ,, Quote must immediately follow /
s_ch_quote: cmp #QUOTE          ;   If a string search, do setup
            beq SetupText       ;   ,,
next_srch:  jsr ISCNTC          ; Check for STOP key
            beq srch_stop       ;   and finish search early if pressed
            lda W_ADDR+1        ; Store the working address high byte for
            pha                 ;   later comparison
            lda INBUFFER+4
            cmp #":"
            beq MemSearch
            cmp #QUOTE
            beq MemSearch
            cmp #"/"
            beq MemSearch
            jmp CodeSearch
check_end:  pla                 ; Has the working address high byte advanced?
            cmp W_ADDR+1        ;   ,,
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
            jmp Addr2CP         ; Update Command Pointer

; Setup Hex Search
; by converting a hex search into a memory search. Transcribe hex characters
; into the input as values.       
SetupHex:   lda #$05            ; Place the input index after the colon so
            sta IDX_IN          ;   it can get hex bytes
            ldy #$00            ; Count the number of hex bytes
-loop:      jsr HexGet          ; Is it a valid hex character?
            bcc setup_done      ; If not, the transcription is done
            sta INBUFFER+5,y    ; Store the byte in the buffer
            iny                 ; Grab up to eight bytes
            bne loop
setup_done: sty SEARCH_S        ; Set search size
            jmp next_srch  
            
; Setup Text Search
SetupText:  ldy WORK+1
-loop:      lda INBUFFER+5,y
            cmp #QUOTE
            beq su_txt_r
            iny
            cpy #17
            bne loop
su_txt_r:   sty SEARCH_S
            jmp next_srch                
            
; Memory Search
; Compare a sequence of bytes in memory to the input. If there's a match,
; indicate the starting address of the match.            
MemSearch:  ldy WORK+1          ; Start index for search
-loop:      lda INBUFFER+5,y    ; Get character to search
            ldx WORK+1          ; If start index is 1, it means we're ALSO
            beq no_scr_c        ;   looking for screen codes (b/c of the /)
            jsr PETtoScr        ; Perform PETSCII to screen code conversion
no_scr_c:   cmp (W_ADDR),y      ; Do the compare
            bne no_match        ; If this doesn't match, need to move on
            iny                 ; Otherwise, check for additional matches
            cpy SEARCH_S        ; Have we reached the search size?
            bne loop            ; ,,
            beq mem_found       ; If SEARCH_S matches, show found address
no_match:   jmp next_check      ; If no match, increment address, try again
mem_found:  jsr ResetOut        ; Show the match address on the screen
            jsr AddrPrefix      ; ,, Use the standard search prefix
            lda W_ADDR          ; ,,
            clc                 ; Add the screen code offset
            adc WORK+1          ; ,,
            sta W_ADDR          ; ,,
            bcc no_scr_c2       ; ,,
            inc W_ADDR+1        ; ,,
no_scr_c2:  jsr ShowAddr        ; Show the address where the query was
            jsr PrintBuff       ;   found.
next_check: jsr IncAddr         ; Increment the search address.
            jmp check_end                      
             
; Code Search
; Disassemble code from the working address. If the disassembly at that
; address matches the input, indicate the starting address of the match.
CodeSearch: jsr ResetOut
            jsr AddrPrefix+3    ; Show address display convention, w/o prompt
            jsr ShowAddr        ; ,,
            lda #" "            ; Found disassembly space
            jsr CharOut         ; ,,
            jsr Disasm          ; Disassmble the code at the working address
code_found: lda #WEDGE          ; Show the wedge character as a prefix
            jsr CHROUT          ; ,,
            jsr PrintBuff       ; Show the code disassembly at this address
            ldx #7              ; Check for match.
            jsr IsMatch+2       ; If match, go to next instruction
            bcs ch_end_a        ; ,,
            lda #CRSRUP         ; If no match, cursor up.
            jsr CHROUT          ; ,,
ch_end_a:   jmp check_end       ; Go back for more      
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; TRANSFER AND FILL
; https://github.com/Chysn/VIC20-wAx2/wiki/Transfer-and-Fill
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copy
MemCopy:    bcc copy_err        ; Get parameters as 16-bit hex addresses for
            jsr HexGet          ; Source end
            bcc copy_err        ; ,,
            sta RANGE_END+1     ; ,,
            jsr HexGet          ; ,,
            bcc copy_err        ; ,,
            sta RANGE_END       ; ,,
            jsr HexGet          ; Target
            bcc copy_err        ; ,,
            sta C_PT+1          ; ,,
            jsr HexGet          ; ,,    
            bcc copy_err        ; ,,
            sta C_PT            ; ,,
            ldx #$00            ; Copy memory from the start address...
-loop:      lda (W_ADDR,x)      ; ,,
            sta (C_PT,x)        ; ...To the target address
            lda W_ADDR+1        ; ,,
            cmp RANGE_END+1     ; Have we reached the end of the copy range?
            bne advance         ; ,,
            lda W_ADDR          ; ,,
            cmp RANGE_END       ; ,,
            beq copy_end        ; If so, leave the copy tool
advance:    jsr IncAddr         ; If not, advance the working address and the
            jsr IncCP           ;   Command Pointer
            jmp loop            ;   and copy the next byte
copy_end:   jsr IncCP           ; Advance Command Pointer
            jmp CPtoBASIC       ; Update CP variable and end  
copy_err:   jmp SyntaxErr       ; SYNTAX ERROR if invalid parameters
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; NUMERIC CONVERSION
; https://github.com/Chysn/VIC20-wAx2/wiki/Numeric-Conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Hex to Base-10
Hex2Base10: jsr ResetIn         ; Reset input buffer
            jsr HexGet   
            bcc hex_conv_r      ; There's no byte available, so bail
            sta W_ADDR+1
            jsr HexGet   
            sta W_ADDR
            bcs two_bytes       ; There are two good bytes
            lda W_ADDR+1        ; If there's only one good byte, then
            sta W_ADDR          ;   treat that as a low byte, and make the
            lda #$00            ;   high byte zero
            sta W_ADDR+1        ;   ,,
two_bytes:  lda #WEDGE
            jsr CHROUT
            lda #"#"
            jsr CHROUT
            ldx W_ADDR          ; Set up PRTFIX for base-10 integer output
            lda W_ADDR+1        ; ,,
            jsr PRTFIX          ; ,,
            jsr CReturn 
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
            jsr HexOut
only_low:   lda SYS_DEST
            jsr HexOut
b102h_r:    jmp PrintBuff
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; SYMBOLIC ASSEMBLER COMPONENTS
; https://github.com/Chysn/VIC20-wAx2/wiki/Symbol-Table-Manager
; https://github.com/Chysn/VIC20-wAx2/wiki/Symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Set Command Pointer
SetCP:      bcc CPtoBASIC       ; Set CP variable (in BASIC) if no address
            lda #0              ; Clear the unresolved reference overflow when
            sta OVERFLOW_F      ;   CP is set
            jsr DirectMode      ; If * addr was issued in BASIC, reset forward
            beq Addr2CP         ;   reference overflow counter, and set UR%
            jsr URtoBASIC       ;   ,,
Addr2CP:    lda W_ADDR          ; Move working address to Command Pointer
            sta C_PT            ; ,,
            lda W_ADDR+1        ; ,,
            sta C_PT+1          ; ,,
            ; Fall through to CPtoBASIC

; Store Command Pointer in CP variable
; If running in program mode          
CPtoBASIC:  jsr DirectMode      ; Do not set this variable in direct mode
            beq init_r          ; ,, (return via some random RTS)
            lda #"C"            ; Create the floating-point variable name CP
            sta $45             ;   for "Command Pointer"
            lda #"P"            ;   ,,
            sta $46             ;   ,,
            jsr FNDVAR          ;   Find or create variable
            ldy C_PT
            lda C_PT+1
            jsr ToFAC1
store_var:  ldx $47             ; Store the floating-point number in FAC1 to
            ldy $48             ;   the variable memory
            jmp STORFAC         ;   ,,
            
; Store 16-bit unsigned value in FAC1
; And set VL variable in program mode
; A = high, Y = low
SetUserVar: jsr ToFAC1          ; Convert A/Y to 16-bit unsigned FAC1
            jsr DirectMode      ; If in direct mode, do not assign the
            beq init_r          ;   variable
            lda #"U"            ; In program mode, find or create UU
            sta $45             ; ,,
            sta $46             ; ,,
            jsr FNDVAR          ; ,,
            jmp store_var       ; Store FAC1 in that variable
            
; Store 16-bit unsigned value in FAC1
; A = high, Y = low
ToFAC1:     jsr MAKFP           ; A/Y to FAC1
            jsr SGNFAC          ; Is the result negative?
            cmp #$ff            ; ,,
            bne init_r          ; If not, there's nothing that needs to be done
            lda #<F65536        ; If FAC1 is negative, it means that it's
            ldy #>F65536        ;   $8000 or more. Add 65536 to make CP the
            jmp LAPLUS          ;   actual address.
           
; Assign or Initialize Symbols
Symbols:    lda INBUFFER 
            bne set_lab         ; If the tool is alone, show the symbol table
            jmp SymbolList      ; ,,
set_lab:    jsr ResetIn
            jsr CharGet
            cmp #"-"            ; If - follows the tool, initialize the symbol
            beq init_clear      ;   table
            jsr SymbolIdx       ; Is this a valid symbol, and is there memory?
            jsr HexGet   
            bcs hex_ok
            jmp SYM_ERROR        
hex_ok:     sta SYMBOL_AH,y
            jsr HexGet          ; Get the low byte of the value
            bcs llow_ok         ; If there's no low byte provided, then the
            lda SYMBOL_AH,y     ;   only valid byte is treated as the low
            tax                 ;   byte, and the high byte is set to 0.
            lda #0              ;   ,,
            sta SYMBOL_AH,y     ;   This allows setting a symbol using an
            txa                 ;   8-bit value.
llow_ok:    sta SYMBOL_AL,y
            rts            
init_clear: lda #$00            ; Initialize bytes for the symbol table
            ldy #ST_SIZE-1      ;   See the Symbol Table section at the top for
-loop:      sta SYMBOL_D,y      ;   information about resizing or relocating the
            dey                 ;   symbol table
            bpl loop            ;   ,,
init_r:     rts
            
; Get Symbol Index
; Return symbol index in Y
; Error (all symbols used, or bad symbol) if symbol error
SymbolIdx:  cmp #FWD_NAME       ; If the reserved forward reference symbol,
            bne sym_range       ;   assign it to a specific symbol location
            ldy #MAX_SYM-1      ;   ,,
            lda #FWD_NAME
            pha
            lda IDX_IN
            cmp #5
            bcc sym_found
            lda #0
            sta SYMBOL_AL,y
            sta SYMBOL_AH,y
            jmp sym_found
sym_range:  cmp #"0"            ; Allowed symbol names are 0-9, A-Z, and @
            bcc bad_name        ; ,,
            cmp #"Z"+1          ; ,,
            bcs bad_name        ; ,,
            cmp #"9"+1          ; ,,
            bcc good_name       ; ,,
            cmp #"@"            ; ,, (includes A~)
            bcs good_name       ; ,,
bad_name:   jmp SYM_ERROR        
good_name:  pha
            ldy #MAX_SYM-2      ; See if the name is already in the table
-loop:      cmp SYMBOL_D,y      ; ,,
            beq sym_found       ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            ldy #MAX_SYM-2      ; If the symbol isn't already in use, look for
-loop:      lda SYMBOL_D,y      ;   an empty record
            beq sym_found       ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
            pla                 ; No empty symbol is found; all symbols are in        
            jmp bad_name        ;   use. Return for error
sym_found:  pla
            sta SYMBOL_D,y      ; Populate the symbol table with the name
            rts            
            
; Show Symbols List           
SymbolList: ldx #$00
-loop:      txa                 ; Save the iterator from PrintBuff, etc.
            pha                 ; ,,
            tay                 ; ,,
            lda SYMBOL_AL,y     ; Stash the current value in W_ADDR
            sta W_ADDR          ; ,,
            lda SYMBOL_AH,y     ; ,,
            sta W_ADDR+1        ; ,,
            lda W_ADDR          ; If this symbol is undefined (meaning, it is
            bne show_sym        ;   $0000, then skip it)
            lda W_ADDR+1        ;   ,,
            beq undefd          ; Undefined, but it might be a forward reference
show_sym:   jsr SymListCo       ; Add elements common to both listed item
            jsr ShowAddr
            jsr PrintBuff
next_sym:   pla
            tax
            inx
            cpx #MAX_SYM
            bne loop
            jsr ResetOut        ; Show the value of the Command Pointer
            jsr Space           ; ,,
            jsr Space           ; ,,
            lda #"*"            ; ,,
            jsr CharOut         ; ,,
            jsr Space           ; ,,
            jsr ShowCP          ; ,,
            lda OVERFLOW_F      ; Show the overflow forward reference count
            beq lablist_r       ;   (if any)
            pha                 ;   ,,
            jsr Space           ;   ,,
            jsr ReverseOn       ;   ,,
            lda #"?"            ;   ,,
            jsr CharOut         ;   ,,
            pla                 ;   ,,
            jsr HexOut          ;   ,,
lablist_r:  jmp PrintBuff       ;   ,, 

; Count forward references and return to the SymbolList loop
undefd:     stx IDX_SYM
            ldy #$00            ; Forward reference count for this symbol
            ldx #$00            ; Forward record index
-loop:      lda SYMBOL_F,x      ; Is this reference record in use?
            beq next_undef      ; ,,
            and #$3f            ; Mask off high byte flag (bit 6)
            cmp IDX_SYM
            bne next_undef
            iny
next_undef: inx                 ; If record is unused, advance the
            cpx #MAX_FWD        ;   forward reference index, and check
            bne loop            ;   again
            cpy #$00            ; If no forward references exist for the current
            beq next_sym        ;   symbol, try the next one
show_fwd:   tya                 ; If there is at least one forward reference,
            pha                 ;   then display the symbol's record
            ldx IDX_SYM         ;   ,,
            jsr SymListCo       ;   ,,
            jsr ReverseOn       ;   ,,
            lda #"?"            ;   ,,
            jsr CharOut         ;   ,,
            pla                 ;   ,,
            jsr HexOut          ;   ,, Number of unresolved references
fwd_d:      jsr PrintBuff       ;   ,,
            jmp next_sym

; Symbol List Common            
SymListCo:  jsr ResetOut
            jsr Space
            lda #SIGIL
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
            jsr HexOut          ;   insert the high byte of the address
            lda BYTE_MOD        ;   ,,
            cmp #HIGH_BYTE      ; If > has been specified, then skip the low
            beq do_expand       ;   byte
insert_lo:  lda SYMBOL_AL,y
            jsr HexOut            
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
            ldx #$00            ; First order of business is finding unresolved
-loop:      cmp SYMBOL_F,x      ;   records that match the symbol
            beq fwd_used
            ora #$40            ; Also check for high-byte specifier
            cmp SYMBOL_F,x
            beq fwd_used
            and #%10111111      ; Mask away high-byte specifier for next check
            inx
            cpx #MAX_FWD
            bne loop
            rts                 ; Label not found in forward reference table
fwd_used:   lda SYMBOL_FL,x     ; A forward reference for this symbol has been
            sta CHARAC          ;   found; store the address in zero page for
            lda SYMBOL_FH,x     ;   updating the code at this address.
            sta CHARAC+1        ;   ,,
            ldy #$00            ; Get the byte at the reference address, which
            lda (CHARAC),y      ;   should be an instruction opcode
            jsr Lookup          ; Look it up
            bcs get_admode      ; ,,
            jmp RES_ERROR       ; Not a valid instruction; ?CAN'T RESOLVE ERROR
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
            jmp RES_ERROR       ;   ,,
load_abs:   lda W_ADDR          ; For an absolute mode instruction, add the
            ldy #$01            ;   value of the reference to the existing
            clc                 ;   value of the operand, to account for
            adc (CHARAC),y      ;   arithmetic (+ or -) operations to the
            sta (CHARAC),y      ;   absolute address
            iny                 ;   ,,
            lda W_ADDR+1        ;   ,,
            adc (CHARAC),y      ;   ,,
            sta (CHARAC),y      ;   ,,
            jmp clear_back
load_rel:   lda W_ADDR          ; The target is the current working address
            sec                 ; Subtract the reference address and add
            sbc CHARAC          ;   two to get the offset
            sec                 ;   ,,
            sbc #$02            ;   ,,
            ldy #$01            ; Store the computed offset in the forward
            sta (CHARAC),y      ;   reference operand address
            jmp clear_back      ; Go back and see if there are more to resolve
load_immed: lda #$40            ; Check bit 6 of the symbol byte of the forward
            and SYMBOL_F,x      ;   reference symbol record. If it's set, it
            beq load_low        ;   means that the user wants the high byte
            lda W_ADDR+1        ;   of the symbol target
            .byte $3c           ; Skip word
load_low:   lda W_ADDR          ; For other instructions, add the reference
            ldy #$01            ;   value to the existing address, to account
            clc                 ;   for arithmetic operations (+ and -)
            adc (CHARAC),y      ;   ,,
            sta (CHARAC),y      ;   ,,
clear_back: lda #$00            ; Clear the forward reference table record for
            sta SYMBOL_F,x      ;   re-use, and go back for additional
            jmp ResolveFwd      ;   forward references
            
; Add Forward Record
; For symbol in Y            
; Each forward reference record consists of three bytes-
; Offset 0 - Label Index OR %10000000
AddFwdRec:  ldx #$00            ; Search the forward symbol table for a
-loop:      lda SYMBOL_FL,x     ;   record in use with the same address.
            cmp W_ADDR          ;   If such a record is found, re-use it
            bne next_used       ;   rather than searching the empty records
            lda SYMBOL_FH,x     ;   ,,
            cmp W_ADDR+1        ;   ,,
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
            bne URtoBASIC       ;   the Symbol Error. In BASIC, this condition
            jmp SYM_ERROR       ;   can be caught, so keep going for multi-pass
empty_rec:  tya
            ldy BYTE_MOD        ; If the symbol was prefixed with >, then mark
            cpy #HIGH_BYTE      ;   this forward record to use the high byte
            bne store_rec       ;   on resolution, by setting bit 6 of the
            ora #$40            ;   symbol forward reference entry
store_rec:  sta SYMBOL_F,x      ; Store the symbol index in the record
            lda W_ADDR          ; Store the current working address in the
            sta SYMBOL_FL,x     ;   forward reference record for (hopefully)
            lda W_ADDR+1        ;   later resolution
            sta SYMBOL_FH,x     ;   ,,
addfwd_r:   rts

; Set UR%
; To value of OVERFLOW_F
URtoBASIC:  lda #"U"+$80        ; During BASIC operation, set UR% to the
            sta $45             ;   number of unresolved forward references
            lda #"R"+$80        ;   on overflow condition. This allows
            sta $46             ;   multi-pass assembly
            jsr FNDVAR          ;   ,,
            ldy #1              ;   ,,
            lda OVERFLOW_F      ;   ,,
            sta ($47),y         ;   ,,
            dey
            lda #0
            sta ($47),y
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; BASIC Stage Manager
; https://github.com/Chysn/VIC20-wAx2/wiki/Change-BASIC-Stage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
BASICStage: jsr ResetIn         ; Reset the input buffer index
            sta W_ADDR          ; Set default end page
            jsr HexGet          ; Get the first hex byte
            bcc st_range        ; If no valid address was provided, show range
            sta W_ADDR+1        ; This is the stage's starting page number
            jsr HexGet          ; But the default can be overridden if a valid
            bcc ch_length       ;   starting page is provided
            sta W_ADDR          ;   ,,
ch_length:  lda W_ADDR+1        ; Make sure that the ending page isn't lower
            cmp W_ADDR          ;   in memory than the starting page. If it is,
            bcc set_ptrs        ;   default the stage size to 3.5K
            clc                 ;   ,,
            adc #$0e            ;   ,,
            sta W_ADDR          ;   ,,
set_ptrs:   lda W_ADDR+1        ; Set up the BASIC start of memory pointer
            sta $2c             ; ,, high
            ldy #$01            ; ,,
            sty $2b             ; ,, low=1
            lda W_ADDR          ; Set up the BASIC end of memory pointer
            sta $38             ; ,, high
            dey                 ; ,,
            sty $37             ; ,, low = 0
            ;ldy #$00
-loop:      lda INBUFFER,y      ; Look through the input buffer for an "N" 
            beq finish          ;   character. This indicates that it is a
            cmp #"N"            ;   new stage.
            beq new             ;   ,,
            iny
            cpy #$16            ; If we reach the end without seeing an "N",
            bne loop            ;   just rechain the area as if it were a BASIC
            beq finish          ;   program
new:        dec $2b             ; Zero out byte before start-of-BASIC memory,
            lda #0              ;   in order to avoid ?SYNTAX ERROR on RUN.
            tay                 ;   The NEW routine doesn't do this.
            sta ($2b),y         ;   ,,
            inc $2b             ; Restore start-of-BASIC low byte
            jsr $c644           ; Perform BASIC NEW
finish:     jsr Rechain
            jmp (READY)
st_range:   jsr ResetOut        ; Show the start and end pages of the current
            jsr wAxPrompt       ;   BASIC stage
            lda #$5e            ;   ,,
            jsr CharOut         ;   ,,
            jsr Space           ;   ,,
            lda $2c             ;   ,,
            jsr HexOut          ;   ,,
            jsr Space           ;   ,,
            lda $34             ;   ,,
            jsr HexOut          ;   ,,
            jmp PrintBuff       ;   ,,
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE TOOL
; https://github.com/Chysn/VIC20-wAx2/wiki/Disk-Tape-SD-Storage#file-listing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
QUOTE_FL    = $0247             ; Quote flag for filename
FIRST_REC   = $0248             ; First record flag

File:       lda INBUFFER        ; First character after the F tool
            beq show_dir        ; Nothing, show directory
            cmp #QUOTE          ; Quote, show directory
            beq show_dir        ; ,,
            cmp #"#"            ; Octothorpe, set device
            bne disk_cmd        ; Anything else, run disk command
            lda #1              ; Get one- or two-digit hex number for the
            sta IDX_IN          ;   device number
            jsr HexGet          ;   ,,
            bcs set_dev         ; If not valid, ?DEVICE NOT PRESENT
            jmp dFail           ; ,,
set_dev:    sta DEVICE          ; If so, set device number
            rts

            ; Execute disk command          
disk_cmd:   ldx DEVICE          ; Get current device number
            lda #15             ; Set file number and secondary address
            tay                 ; ,,
            jsr SETLFS          ; Set logical file
            jsr OPEN            ; And open it
            ldx #15             ; Redirect CHROUT to command
            jsr CHKOUT          ; ,,
            lda #"I"            ; Send initialize command to make sure
            jsr CHROUT          ;   status is updated
            jsr CLRCHN          ;   ,,
            ldx #15             ; Re-redirect CHROUT to command
            jsr CHKOUT          ; ,,
            ldy #0              ; Now send the specified command from the
-loop:      lda INBUFFER,y      ;   input buffer, terminated by 0
            beq show_st         ;   ,,
            jsr CHROUT          ;   ,,
            iny                 ;   ,,
            bne loop            ;   ,,
show_st:    jsr CLRCHN          ; Execute command         
            ldx #15             ; Show command status
            jsr CHKIN           ;   Set the channel for command status input
-loop:      jsr CHRIN           ; Get status text from output channel
            jsr CHROUT          ; OK to use CHROUT, input is back to normal
            cmp #CR             ; CR terminates status
            bne loop            ; ,,
            jsr CLRCHN          ; Close command channel out
            lda #15             ; ,,
            jmp CLOSE           ; ,,
            
            ; Display directory
show_dir:   lsr FIRST_REC       ; Clear first record flag
            lda #1              ; SETNAM - (1) Set name length
            ldx #<lfs+1         ; - Set name as the $ used below
            ldy #>lfs+1         ; ,,
            jsr SETNAM          ; ,,
lfs:        lda #"$"            ; SETLFS - Set file number as $
            pha                 ; - Keep the file number for CLOSE
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
-next_char: jsr CharIn          ; Get next character from line
            beq EOL             ; 0 indicates end-of-line
            cmp #QUOTE          ; If this is a quote, handle the quote mode.
            beq handle_qu       ;   There are three quote modes... Unstarted,
            bit QUOTE_FL        ;   Started, and Done.
            bvs post_name       ; Checks for quote mode Done
            bpl next_char       ; Checks for quote mode Unstarted
dir_char:   jsr CharOut         ; Quote mode is Started, so add character
            jmp next_char       ; ,,
post_name:  cmp #"D"            ; The file type is after the file name (PRG,
            bne next_char       ;   etc.). If a D is found, show it in reverse
            ldy IDX_OUT         ;   text by replacing the first quote mark with
            lda #"/"            ;   Reverse On, and the last quote mark with
            sta OUTBUFFER-1,y   ;   the line terminator.
            lda #RVS_ON         ;   ,,
            sta OUTBUFFER+3     ;   ,,
            bne dir_char        ;   ,,
handle_qu:  sec                 ; Handle the quote mode by shifting 1 into
            ror QUOTE_FL        ;   the high bit.
            bit QUOTE_FL        ; 00000000 is Unstarted. 10000000 is Started
            bvs get_qu          ;   and 11000000 is Done
            jsr AddrPrefix      ; If Started, add the prefix
get_qu:     lda #QUOTE          ; Feed a quote back to the directory name print
            bne dir_char        ;   it to the buffer
EOL:        bit QUOTE_FL        ; If a quote hasn't been finished, end of dir
            bvc EOF             ; ,,
            bit FIRST_REC       ; Is this the first record?
            bpl subseq          ; If so, skip the display
            jsr Match           ; Perform a text match, if necessary
            bcc subseq          ; ,,
            jsr PrintBuff       ; Flush buffer to screen
subseq:     sec                 ; Set the flag to display subsequent lines
            ror FIRST_REC       ; ,,
            jmp newline
EOF:        jsr CLRCHN          ; Reset channel destinations
            pla                 ; Pushed "$" from the beginning
            jmp CLOSE           ; Close directory
            
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
            ldx #3              ; Start of haystack (output buffer)
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
derror:     jmp SyntaxErr           

; Rechain BASIC program
Rechain:    jsr $c533           ; Re-chain BASIC program to set BASIC
            lda $22             ;   pointers as a courtesy to the user
            ;clc                ;   ,, ($c533 always exits with Carry clear)
            adc #$02            ;   ,,
            sta $2d             ;   ,,
            lda $23             ;   ,,
            jmp $c655           ;   ,,
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TEXT DISPLAY (INTERPRET)
; https://github.com/Chysn/VIC20-wAx2/wiki/Memory-Display#text-display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TextDisp:   jsr DQuote
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
            jmp DQuote
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; COMPARE
; https://github.com/Chysn/VIC20-wAx2/wiki/Compare
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compare tool workspace
STATUS      = $0247             ; $00 = Non-Matching, $80 = Matching
COUNT       = $0248             ; Count of unmatched/matched bytes so far

Compare:    bcc cerror          ; Error if the first address is no good
            jsr HexGet          ; Get high byte of range end
            bcc cerror          ; ,,
            sta RANGE_END+1     ; ,,
            jsr HexGet          ; Get low byte of range end
            bcc cerror          ; ,,
            sta RANGE_END       ; ,,
            jsr HexGet          ; Get high byte of compare start
            bcc cerror          ; ,,
            sta C_PT+1          ; ,,
            jsr HexGet          ; Get low byte of compare start
            bcc cerror          ; ,,
            sta C_PT            ; ,,
            lda #$00            ; Reset status and counter
            sta STATUS          ; ,,
            sta COUNT           ; ,,
            sta COUNT+1         ; ,,
            bcs Start           ; Setup OK, do compare
cerror:     jmp SyntaxErr

; Start comparison
Start:      jsr StartLine       ; Start address line
            inc RANGE_END       ; Increase the range by 1 for
            bne compare         ;   the comparison
            inc RANGE_END+1     ;   ,,
compare:    lda W_ADDR+1        ; Is the working address in 
            cmp RANGE_END+1     ;   the compare range?
            bcc in_range        ;   ,,
            lda W_ADDR          ;   ,,
            cmp RANGE_END       ;   ,,
            bcc in_range        ; If so, check for byte match
done:       jsr Toggle          ; Toggle to show the right color
            jsr EndLine         ; Show the last result count
            rts                 ; Done!
in_range:   ldx #$00            ; Compare EA to CP
            lda (W_ADDR,x)      ; ,,
            cmp (C_PT,x)        ; ,,
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
inc_mem:    jsr IncCP           ; Increment the comparison counters
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
            jsr AddrPrefix
            jsr ShowAddr
            lda #";"
            jsr CharOut
            jsr ShowCP
            jmp Space

; End Line
; Complete the line by showing the count in reverse (differs) or
; normal text (same)
EndLine:    lda COUNT           ; If the count is zero, don't
            bne report          ;   finish the buffer, because it's
            lda COUNT+1         ;   probably the first group of bytes
            bne report          ;   ,,
            rts                 ;   ,,
report:     lda STATUS
            beq same
differ:     jsr ReverseOn
            lda #"!"
            jsr CharOut
            jmp show_qty
same:       lda #"="
            jsr CharOut
show_qty:   lda COUNT+1         ; Show number of matches/no matches
            jsr HexOut          ;   before the change
            lda COUNT           ;   ,,
            jsr HexOut          ;   ,,
            jmp PrintBuff       ;   ,,
                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; HELP
; https://github.com/Chysn/VIC20-wAx2/wiki/wAx2-Tutorial#invoking-wax-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Help:       lda #<HelpScr1      ; Print help screen 1. It's broken into pieces
            ldy #>HelpScr1      ;   because it's longer than 256 bytes
            jsr PrintStr        ;   ,,
            lda #<HelpScr2      ; Print help screen 2
            ldy #>HelpScr2      ; ,,
            jmp PrintStr        ; ,,  
                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; USER PLUG-IN
; https://github.com/Chysn/VIC20-wAx2/wiki/User-Plug-In
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PlugIn:     php                 ; Push processor status, used by most tools
            lda INBUFFER        ; If the character is P (detokenized PRINT)
            cmp #"P"            ;   the user is asking for the usage text
            beq ShowUsage       ;   ,,
            ldy #3              ; Get plug-in type
            lda (USER_VECT),y   ; ,,
            bmi is_list         ; ,,
            plp                 ; Normal tool
            jmp (USER_VECT)     ; ,,
is_list:    plp                 ; List tool
            jmp List            ; ,,
            
; Show Usage for Plug-In
; This is also called from the Plug-In menu, but as ShowUsage+1 to skip PLP,
; so be careful if changing the top of this routine.            
ShowUsage:  plp                 ; Pop what was pushed in the main part
            lda USER_VECT       ; Add 4 to the user vector for the usage
            ldy USER_VECT+1     ;   text
            clc                 ;   ,,
            adc #4              ;   ,,
            bcc show_pr         ;   ,,
            iny                 ;   ,,
show_pr:    jsr PrintStr        ;   ,,
            jmp CReturn         ;   ,, CR
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PLUG-IN MANAGER
; https://github.com/Chysn/VIC20-wAx2/wiki/User-Plug-In#user-plug-in-manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CurChar     = $0247

PlugMenu:   bcs instaddr        ; First, try to install by address
            jsr ResetIn         ; Reset in to check for quote
            jsr CharGet         ; If the next character is ", then install by
            cmp #QUOTE          ;   name. Otherwise, show the menu
            beq get_name        ;   ,,
            bne ShowMenu        ;   ,,
instaddr:   lda W_ADDR+1        ; Install by address
            sta USER_VECT+1     ; ,,
            lda W_ADDR          ; ,,
            sta USER_VECT       ; ,,
            jmp sh_usage        ; Show usage, if in direct mode
get_name:   jsr CharGet         ; Get the next two characters after the quote
            sta CurChar         ; ,,
            jsr CharGet         ; ,,
            sta CurChar+1       ; ,,
            ldy #PLUGINS-1      ; Y = last menu index
-loop       lda MenuChar1,y     ; Check first character
            cmp CurChar         ; ,,
            bne mnext           ; ,,
            lda MenuChar2,y     ; If it's found, check second character
            cmp CurChar+1       ; ,,
            beq cfound          ; Both match, so the menu item is found
mnext:      dey                 ; Iterate
            bpl loop            ; ,,
            jmp SyntaxErr       ; Syntax error if plug-in not found
ShowMenu:   lda #"$"
            jsr CHROUT
            jsr ResetOut        ; Show the current address of the plug-in 
            lda USER_VECT+1     ;   ,,
            jsr HexOut          ;   ,,
            lda USER_VECT       ;   ,,
            jsr HexOut          ;   ,,
            jsr PrintBuff       ;   ,,
            jsr ShowUsage+1     ; Show the usage template
            ldx #0              ; Iterate through each plug-in
-loop:      lda #WEDGE          ; For each item in the list, show prompt,
            jsr CHROUT          ;   ,,
            lda TOOL_CHR        ;   then the current tool character
            jsr CHROUT          ;   ,,
            lda #" "            ;   then a space. This is a memory savings over
            jsr CHROUT          ;   putting these things in the menu table.
            lda USER_VECT       ; Compare the user vector to this plug-in's
            cmp MenuLoc_L,x     ;   address, reverse the text for the 
            bne m_next_it       ;   name if it's currently selected
            lda USER_VECT+1     ;   ,,
            cmp MenuLoc_H,x     ;   ,,
            bne m_next_it       ;   ,,
            lda #RVS_ON         ;   ,,
            jsr CHROUT          ;   ,,
m_next_it:  lda #QUOTE          ; Quote before the name
            jsr CHROUT          ; ,,
            lda MenuText_L,x    ; Show the template name to complete the
            ldy MenuText_H,x    ;   install string
            jsr PrintStr        ;   ,,
            inx                 ;   ,,
            cpx #PLUGINS        ; End of menu?
            bne loop            ; ,,
            rts
cfound:     lda MenuLoc_L,y     ; Found item, so set plug-in vector based on
            sta USER_VECT       ;   looked up address
            lda MenuLoc_H,y     ;   ,,
            sta USER_VECT+1     ;   ,,
sh_usage:   jsr DirectMode      ; Do not display usage in program mode
            bne menu_r          ; ,,
            jsr DropDown        ;
desc_r:     jmp ShowUsage+1     ; Show new tool's usage, but skip the PLP     
menu_r:     rts
                        
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
; Zero flag is set if there's no character available           
CharGet:    ldx IDX_IN
            lda INBUFFER,x
            php
            inc IDX_IN
            plp
            rts             
                   
; Is Buffer Match            
; Does the input buffer match the output buffer?
; Carry is set if there's a match, clear if not
IsMatch:    ldx #6              ; Offset for output after address
            ldy #6              ; Offset for input after address
-loop:      lda OUTBUFFER,x     ; If the disassembled code contains a space,
            cmp #" "            ;   skip it.
            bne nx_in           ;   ,,
            inx                 ;   ,,
nx_in:      lda INBUFFER-2,y    ; Compare the assembly with the disassembly
            cmp #" "            ;   But ignore spaces in assembly
            beq skip_match      ;   ,,
            cmp #WILDCARD       ; Handle wildcard character by advancing
            bne match_c         ;   both indexes
            lda OUTBUFFER,x 
            beq not_found
            bne match_ok
match_c:    cmp OUTBUFFER,x     ; Do input and output match at this index?
            bne not_found       ; (See Lookup subroutine above)
match_ok:   inx                 ; Match is good, advance both indexes
skip_match: iny                 ; If input space is skipped, advance only Y
            cpx IDX_OUT         ; Reached the end of output?
            bcc loop            ;   Loop until the buffer is done
            ;sec                ; Carry already set by CPX above
            rts

; Character to Nybble
; A is the character in the text buffer to be converted into a nybble
Char2Nyb:   cmp #"9"+1          ; Is the character in range 0-9?
            bcs not_b10dig      ; ,,
            cmp #"0"            ; ,,
            bcc not_b10dig      ; ,,
            sbc #"0"            ; If so, nybble value is 0-9
            rts
not_b10dig: cmp #"F"+1          ; Is the character in the range A-F?
            bcs not_found       ; See Lookup subroutine above
            cmp #"A"         
            bcc not_found       ; See Lookup subroutine above
            sbc #"A"-$0a        ; The nybble value is 10-15
            rts

; Buffer to Byte
; Get two characters from the buffer and evaluate them as a hex byte
HexGet:     jsr CharGet
            cmp #QUOTE          ; Ignore quotation marks in hex, to allow
            beq HexGet          ;   string interpolation of hex in commands
            cmp #WILDCARD       ; Skip two characters if the first is an
            bne nyb             ;   equal sign (see Register for why)
            jsr CharGet         ;   ,,
            clc                 ;   ,,
            rts                 ;   ,,
nyb:        jsr Char2Nyb        ; Is this valid hex [0-9A-F]?
            bcc hexget_r        ; Return with Carry clear if invalid
            asl                 ; Multiply high nybble by 16
            asl                 ;   ,,
            asl                 ;   ,,
            asl                 ;   ,,
            sta WORK
            jsr CharGet
            jsr Char2Nyb
            bcc one_digit       ; Clear Carry flag indicates invalid hex
            ora WORK            ; Combine high and low nybbles
            ;sec                ; Set Carry flag indicates success
hexget_r:   rts
one_digit:  lda WORK            ; Only a single hex character was provided,
            lsr                 ;   so divide the value by 16 to bring it
            lsr                 ;   back to a one-hex-digit value.
            lsr                 ;   ,,
            lsr                 ;   ,,
            dec IDX_IN          ; Back one input index. The only thing that will
            sec                 ;   work with the one-digit syntax is arithmetic
            rts
            
; Increment Working Address
; Get the working byte and advance working address by one
IncAddr:    ldx #$00
            lda (W_ADDR,x)
            inc W_ADDR
            bne addr_r
            inc W_ADDR+1
addr_r:     rts

; Incremenet Command Pointer
IncCP:      inc C_PT 
            bne pc_r 
            inc C_PT+1
pc_r:       rts

; Address Display
; For address-only displays, like the kids you'd find in searches and
; statuses, they should begin with an asterisk, and then a space,
; and then the address. This shows the semicolon and the space
AddrPrefix: jsr wAxPrompt
CPPlace:    lda #"*"
            jsr CharOut
            ; Fall through to Space

; Commonly-Used Characters
Space:      lda #" "
            .byte $3c           ; TOP (skip word)
DQuote:     lda #QUOTE
            .byte $3c           ; TOP (skip word)
Semicolon:  lda #";"            
            .byte $3c           ; TOP (skip word)
ReverseOn:  lda #RVS_ON
            .byte $3c           ; TOP (skip word)
CloseParen: lda #")"
            .byte $3c           ; TOP (skip word)
Comma:      lda #","
            .byte $3c           ; TOP (skip word)
HexPrefix:  lda #"$"
            .byte $3c           ; TOP (skip word)
wAxPrompt:  lda #WEDGE
            ; Fall through to CharOut
            
; Character to Output
; Add the character in A to the outut buffer            
CharOut:    sta CHARAC          ; Save temporary character
            tya                 ; Save registers
            pha                 ; ,,
            txa                 ; ,,
            pha                 ; ,,
            ldx IDX_OUT         ; Write to the next OUTBUFFER location
            cpx #$30            ; ,, Prevent buffer overwrite
            bcs charout_r       ; ,, 
            lda CHARAC          ; ,,
            sta OUTBUFFER,x     ; ,,
            inc IDX_OUT         ; ,,
charout_r:  pla                 ; Restore registers
            tax                 ; ,,
            pla                 ; ,,
            tay                 ; ,,
            lda CHARAC          ; ,,
            rts
                        
; Write hexadecimal character
HexOut:     pha                 ; Hex converter based on from WOZ Monitor,
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
bad_bin:    jmp ASM_ERROR
 
; Show Working Address
; 16-bit hex address at working address          
ShowAddr:   lda W_ADDR+1
            jsr HexOut
            lda W_ADDR
            jmp HexOut 

; Show Command Pointer
; 16-bit hex address at Command Pointer address          
ShowCP:     lda C_PT+1
            jsr HexOut
            lda C_PT
            jmp HexOut
            
; Show 8-bit Parameter           
Param_8:    jsr HexPrefix
            jsr IncAddr   
            jmp HexOut           
            
; Show 16-Bit Parameter            
Param_16:   jsr HexPrefix
            jsr IncAddr   
            pha
            jsr IncAddr   
            jsr HexOut
            pla
            jmp HexOut

; Interpolate Variable
; Replace 'V with hex(V)            
InterpVar:  ldy #0
            sty CHARAC          ; Set bit 7 if string
-loop:      lda #0
            sta $45,y
            jsr CHRGET          ; Get next character
            beq int_err         ; Error on nothing
            cmp #"$"            ; String variable?
            bne ch_eov          ; ,,
            ror CHARAC          ; Set string variable flag
            jsr CHRGET          ; The closing quote is expected immediately
            cmp #"'"            ;   after a $, or it's a syntax error
            bne int_err         ;   ,,
ch_eov:     cmp #"'"            ; If the closing quote is found,
            beq val_var         ;   then find the variable
            sta $45,y           ; Store next variable name character
next_vch:   iny
            cpy #4
            bne loop
int_err:    jmp SyntaxErr       ; Interpolation variable error                      
val_var:    lda $45             ; Validate the found variable name;
            jsr CHRTST          ;   if first character <A or >Z, then error
            bcc int_err         ;   ,,
            lda $46             ; Validate the second character. If it's 0,
            beq find_var        ;   then it's a one-variable name, and
            jsr CHRTST          ;   the name is OK
            bcs find_var        ;   ,,
            cmp #"9"+1          ; If the second character >9 then error
            bcs int_err         ; ,,
            cmp #"0"            ; If the second character <0 then error
            bcc int_err         ; ,,
find_var:   lda $46             ; The variable name is OK. Add bit 7 if a
            ora CHARAC          ;   string
            sta $46             ;   ,,
            jsr FNDVAR          ; Find variable
            bit $46             ; If the found variable is a string, then
            bmi is_string       ;   handle it, otherwise it's numeric...
            lda TOOL_CHR        ; If the interpolation is in the Assemble
            cmp #T_ASM          ;   tool, as an operand, add the $ in
            beq add_hexsig      ;   front of the hex digits.
            cmp #T_SRC          ; Same with code search
            beq add_hexsig      ; ,,
            cmp #T_ASM_AL       ; Same with assembly alias
            bne mov2fac         ; ,,
add_hexsig: lda IDX_IN          ; The hex sigil $ should only be added when
            cmp #7              ;   applied to an instruction operand, like
            beq sig             ;     JSR $nnnn or LDA ($nn, X) or LDA #$nn
            cmp #8              ;   which is the 7th or 8th position in the
            bne mov2fac         ;   buffer. For anything else, like arithmetic,
sig:        lda #"$"            ;   or the target address, the sigil is not
            jsr AddInput        ;   inserted.
mov2fac:    lda $47             ; Move found variable to FAC
            ldy $48             ; ,,
            jsr LODFAC          ; ,,
            jsr MAKADR          ; Convert floating point to address
            jsr ResetOut        ; Use output buffer for hex conversion
            lda $15
            beq int_low
            jsr HexOut
            jsr CopyOp
            jsr ResetOut
int_low:    lda $14
            jsr HexOut
            jsr CopyOp
            jmp Transcribe
            
is_string:  lda #QUOTE          ; This is a string variable, so add quote to
            jsr AddInput        ;   the input buffer
            ldx #0              ; Get the string size here
            lda ($47,x)         ; ,,
            beq int_err         ; Error if it's not set
            cmp #17             ; If it's bigger than wAx can handle, shorten it
            bcc str_sz_ok       ; ,,
            lda #16             ; ,,
str_sz_ok:  sta CHARAC          ; Store the string size for comparison
            ldy #2              ; Get the second byte after the variable
            lda ($47),y         ;   address, which is high byte of string
            pha                 ; Push high byte to stack
            dey                 ; Now get the low byte of the string address
            lda ($47),y         ; ,,
            sta $47             ; Store low byte, repurposing $47/$48 for a
            pla                 ;   little while; then, pull and store
            sta $48             ;   high byte.
            dey                 ; Set string index to 0
-loop:      lda ($47),y         ; Get the string byte at the (newly-designated)
            beq str_done        ;   pointer. It's zero-terminated
            jsr AddInput        ; AddInput doesn't use Y, so we're okay
            iny                 ; Increment the counter
            cpy CHARAC          ; For wAx, strings need to max out at 16
            bne loop            ; ,,
str_done:   lda #QUOTE          ; Add the ending quote, which is needed
            jsr AddInput        ;   to represent the string accurately
            jmp Transcribe
                        
; Expand Command Pointer
; Replace asterisk with the C_PT
ExpandCP:   jsr ResetOut
            jsr ShowCP
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
            cmp #$ac            ; Replace an asterisk with the Command
            bne ch_interp       ;   Pointer
            jmp ExpandCP        ;   ,,
ch_interp:  cmp #"'"            ; Replace a variable name with a hex value
            bne ch_comment      ; ,,
            ldy $83             ;   (If currently in quote mode, just add the
            cpy #$06            ;     apostrophe without interpolating
            beq x_add           ;     a variable)
            jmp InterpVar       ; ,,
ch_comment: cmp #";"            ; If it's a comment, then quit transcribing
            beq comment         ;   unless we're in quote mode
            cmp #HIGH_BYTE      ; If it's > or <, it modifies the next symbol
            bne ch_low          ; ,,
            sta BYTE_MOD        ; ,,
            jmp post_mx         ; ,, Get more, but don't clear the modifier
ch_low:     cmp #LOW_BYTE       ; ,,
            bne ch_sym          ; ,,
            sta BYTE_MOD        ; ,,
            jmp post_mx         ; ,, Get more, but don't clear the modifier
ch_sym:     cmp #SIGIL          ; Handle symbols
            beq handle_sym      ; ,,
            cmp #QUOTE          ; If a quote is found, modify CHRGET so that
            bne ch_token        ;   spaces are no longer filtered out
            lda #$06            ;   until the next quote. At an end quote,
            cmp $83             ;   put CHRGET back to normal.
            bne qu_on           ;   ,,
            lda #$ef            ;   ,,
            .byte $3c           ; DOP (skip byte)
qu_on:      lda #$06            ; $0082 BEQ $0073 -> BEQ $008a
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
handle_sym: ldy $83             ; Don't handle symbols if the name is in quotes
            cpy #$06            ;   (as in an immediate operand, or text entry)
            beq x_add           ;   ,,
            lda IDX_IN          ; If @ is the first character in the input
            cmp #$06            ;   buffer after the address, defer the
            bcs start_exp       ;   symbol for handling by the assembler
            lda #SIGIL          ;   ,,
            jsr AddInput        ;   ,,
            jmp Transcribe      ;   ,,
start_exp:  jsr CHRGET          ; Get the next character, the symbol name
            jsr SymbolIdx       ; Get the symbol index
get_s_name: jsr IsDefined
            bne go_expand
            lda IDX_IN          ; The symbol has not yet been defined; parse
            pha                 ;   the first hex numbers to set the working
            jsr RefrAddr        ;   address, then return the input index to
            pla                 ;   its original position
            sta IDX_IN          ;   ,,
            jsr AddFwdRec       ; Add forward reference record for symbol Y
            inc IGNORE_RB       ; Set relative branch ignore flag
go_expand:  jmp ExpandSym       ; Use $0000 as a placeholder
comment:    ldy $83
            cpy #$06
            beq add_only
            lda #$06            ; Move into quote mode so that symbol characters
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
            sec                 ; Get current cursor column to pad (most of)
            jsr PLOT            ;   the remainder of the line with spaces,
            lda #" "            ;   so that if the user cursors up to
-loop:      cpy #21             ;   execute a tool, the data send to the
            bcs CReturn         ;   screen remains clean
            jsr CHROUT          ;   ,,
            iny                 ;   ,,
            bne loop            ;   ,,

; Carriage return
; To output, not buffer
CReturn:    lda #CR
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
; X should be set to the number of bytes the working address should be
; advanced
Prompt:     txa                 ; Based on the incoming X register, advance
            clc                 ;   the effecive address and store in the
            adc W_ADDR          ;   Command Pointer. This is how wAx
            sta C_PT            ;   remembers where it was
            lda #$00            ;   ,,
            adc W_ADDR+1        ;   ,,
            sta C_PT+1          ;   ,,
            jsr CPtoBASIC       ;   Set the CP variable in BASIC
            jsr ResetOut        ; Reset the output buffer to generate the prompt
            jsr wAxPrompt       ; The prompt begins with the wedge character
            lda #T_ASM          ;   Followed by the assembler tool character
            jsr CharOut         ;   ,,
            jsr Space           ;   Followed by a space
            jsr ShowCP          ; Show Command Pointer
            lda #CRSRRT         ; Cursor right
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
;    jsr DirectMode
;    beq running_direct
;    bne running_basic
DirectMode: ldy CURLIN+1
            iny
            rts

; Instruction Size
; Given 6502 opcode in A, return its size (1-3) in X            
SizeOf:     ldx #$03            ; Determine instruction length
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
three:      rts

; Convert PETSCII to Screen Code
; In A
PETtoScr:   cmp #$ff            ; Is pi, which is an odd duck
            beq pi              ; ,,
            cmp #$c0
            bcs b_c0
            cmp #$a0
            bcs b_a0
            cmp #" "
            bcc pet_r
            cmp #$60
            bcc s_60
            and #$df
            bne pet_r
s_60:       and #$3f
pet_r:      rts
b_a0:       sbc #$40
b_c0:       and #$7f
            rts
pi:         lda #$5E
            rts   
            
; Drop to Blank
; Search for the last prompt character
DropDown:   ldx #0              ; Get character at screen position
-loop:      lda ($d1,x)         ; ,,
            cmp #WEDGE          ; Is it a . character?
            bne dd_r            ; If not, done positioning cursor
            lda #$11            ; Drop down one line
            jsr CHROUT          ; ,,
            bne loop            ; And look again
dd_r:       rts
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ToolTable contains the list of tools and addresses for each tool
ToolTable:  .byte T_DIS,T_ASM,T_MEM,T_REG,T_EXE,T_BRK,T_TST,T_SAV,T_LOA,T_BIN
            .byte T_XDI,T_SRC,T_CPY,T_H2T,T_T2H,T_SYM,T_BAS,T_USR
            .byte ",",";",T_FIL,T_INT,T_COM,T_HLP,T_MEN,SIGIL,DEF_TOKEN
ToolAddr_L: .byte <List-1,<Assemble-1,<List-1,<Register-1,<Go-1
            .byte <SetBreak-1,<Tester-1,<MemSave-1,<MemLoad-1,<List-1
            .byte <List-1,<Search-1,<MemCopy-1,<Hex2Base10-1,<Base102Hex-1
            .byte <SetCP-1,<BASICStage-1,<PlugIn-1
            .byte <Assemble-1,<Register-1,<File-1,<List-1,<Compare-1
            .byte <Help-1,<PlugMenu-1,<Symbols-1,<DEF-1
ToolAddr_H: .byte >List-1,>Assemble-1,>List-1,>Register-1,>Go-1
            .byte >SetBreak-1,>Tester-1,>MemSave-1,>MemLoad-1,>List-1
            .byte >List-1,>Search-1,>MemCopy-1,>Hex2Base10-1,>Base102Hex-1
            .byte >SetCP-1,>BASICStage-1,>PlugIn-1
            .byte >Assemble-1,>Register-1,>File-1,>List-1,>Compare-1
            .byte >Help-1,>PlugMenu-1,>Symbols-1,>DEF-1

; Plug-In Menu Data           
MenuText_L: .byte <MEtxt,<REtxt,<DEtxt,<MLtxt,<CYtxt,<BAtxt,<WAtxt
MenuText_H: .byte >MEtxt,>REtxt,>DEtxt,>MLtxt,>CYtxt,>BAtxt,>WAtxt
MEtxt:      .asc "MEM CONF",QUOTE,CR,$00
REtxt:      .asc "RELOC",QUOTE,CR,$00
DEtxt:      .asc "DE-BUG",QUOTE,CR,$00
MLtxt:      .asc "ML2BAS",QUOTE,CR,$00
CYtxt:      .asc "CYCLES",QUOTE,CR,$00
BAtxt:      .asc "BAS AID",QUOTE,CR,$00
WAtxt:      .asc "WAXFER",QUOTE,CR,$00
            
MenuChar1:  .asc "M","R","D","M","C","B","W"
MenuChar2:  .asc "E","E","E","L","Y","A","A"
MenuLoc_L:  .byte <uConfig,<uRelocate,<uDebug,<uML2BAS
            .byte <uCycle,<uBASIC,<uwAxfer
MenuLoc_H:  .byte >uConfig,>uRelocate,>uDebug,>uML2BAS
            .byte >uCycle,>uBASIC,>uwAxfer

; Addresses for error message text
ErrAddr_L:  .byte <AsmErrMsg,<MISMATCH,<LabErrMsg,<ResErrMsg,<RBErrMsg
ErrAddr_H:  .byte >AsmErrMsg,>MISMATCH,>LabErrMsg,>ResErrMsg,>RBErrMsg

; Text display tables  
wAxpander:  .asc CRSRUP,CRSRUP,CRSRRT,CRSRRT
            .asc CRSRRT,CRSRRT,CRSRRT,CRSRRT," +27K",CR,CR,$00
Banner:     .asc CR,$b0,CR
            .asc $dd," BEIGEMAZE.COM/WAX2",CR
            .asc $dd," V2.1       .? HELP",CR
            .asc $ad,CR,$00
            
Registers:  .asc CR,$c0,$c0,"A",$c0,$c0,"X",$c0,$c0,"Y",$c0,$c0,"P",$c0
            .asc $c0,"S",$ae,$00
PFNames:    .asc "C","Z",$01,"D",$01,$01,"V","N"            
BreakMsg:   .asc CR,RVS_ON,"BRK",RVS_OFF,$00
HelpScr1:   .asc T_DIS,176,"DISASSM",$dd,"A ASSEMBLE",CR
            .asc T_XDI,173,"+UNOFF.",$dd,"G GO",CR
            .asc T_MEM," MEMORY ",$dd,T_REG," REGISTER",CR
            .asc T_INT," TEXT   ",$dd,T_BRK," BRKPT",CR
            .asc T_BIN," BINARY ",$dd,"= TEST",CR
            .asc T_COM," COMPARE",$dd,T_LOA," LOAD",CR,$00
HelpScr2:   .asc T_SRC," SEARCH ",$dd,T_SAV," SAVE",CR
            .asc T_CPY," XFER   ",$dd,T_FIL," FILE",CR 
            .asc $5e," STAGE  ",$dd,T_EXI," EXIT",CR
            .asc "@ SYMBOLS",$dd,CR
            .asc "* SET CP ",171,192,"PLUG-IN",192,174,CR
            .asc T_H2T," HEX-DEC",$dd,T_USR," INVOKE",CR
            .asc T_T2H," DEC-HEX",$dd,T_MEN," INSTALL",CR,$00
                    
; Error messages
AsmErrMsg:  .asc "ASSEMBL",$d9
LabErrMsg:  .asc "SYMBO",$cc
ResErrMsg:  .asc "CAN",$27,"T RESOLV",$c5
RBErrMsg:   .asc "TOO FA",$d2

; FAC for 65536
F65536:     .byte $91,$00,$00,$00,$00

; 6502 Instruction Set
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
            
            ; 6502 Unofficial Instructions
            ; with alternate names. In many cases, there's no consensus on what
            ; the mnemonic for some instructions should be. If you have strong
            ; feelings, the comments above the standard instruction table 
            ; explain how a mnemonic is formatted. If there had been enough
            ; memory left, I would have included more aliases.
Extended:   .byte $9a,$ef       ; SKW               (see TOP)
            .byte $3c,$b0       ; * SKW implied (ersatz)
            .byte $9a,$c5       ; SKP               (see DOP)
            .byte $34,$b0       ; * SKB implied (ersatz)
            .byte $0b,$87       ; ANC   
            .byte $0b,$a0       ; * ANC immediate
            .byte $2b,$a0       ; * ANC immediate
            .byte $98,$71       ; SAX
            .byte $87,$70       ; * SAX zero page
            .byte $97,$90       ; * SAX zero page,y
            .byte $83,$20       ; * SAX (indirect,x)
            .byte $8f,$40       ; * SAX absolute
            .byte $0c,$a5       ; ARR
            .byte $6b,$a0       ; * ARR immediate
            .byte $0c,$e5       ; ASR               (aka ALR)
            .byte $4b,$a0       ; * ASR immediate
            .byte $66,$03       ; LXA               (aka ATX, OAL)
            .byte $ab,$a0       ; * LXA immediate            
            .byte $9a,$03       ; SHA               (aka AHX)
            .byte $9f,$60       ; * SHA absolute,y
            .byte $93,$30       ; * SHA (indirect),y
            .byte $98,$b1       ; SBX               (aka AXS)
            .byte $cb,$a0       ; * SBX immediate
            .byte $20,$e1       ; DCP               (aka DCM)
            .byte $c7,$70       ; * DCP zero page
            .byte $d7,$80       ; * DCP zero page,x
            .byte $cf,$40       ; * DCP absolute
            .byte $df,$50       ; * DCP absolute,x
            .byte $db,$60       ; * DCP absolute,y
            .byte $c3,$20       ; * DCP (indirect,x)
            .byte $d3,$30       ; * DCP (indirect),y
            .byte $23,$e1       ; DOP               (aka NOP, SKB)
            .byte $04,$70       ; * DOP zero page
            .byte $14,$80       ; * DOP zero page,x
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
            .byte $4c,$c5       ; ISB               (aka ISC)
            .byte $e7,$70       ; * ISB zero page
            .byte $f7,$80       ; * ISB zero page,x
            .byte $ef,$40       ; * ISB absolute
            .byte $ff,$50       ; * ISB absolute,x
            .byte $fb,$60       ; * ISB absolute,y
            .byte $e3,$20       ; * ISB (indirect,x)
            .byte $f3,$30       ; * ISB (indirect),y
            .byte $60,$4b       ; LAE               (aka LAR, LAS)
            .byte $bb,$60       ; * LAE absolute,y
            .byte $60,$71       ; LAX
            .byte $a7,$70       ; * LAX zero page
            .byte $b7,$90       ; * LAX zero page,y
            .byte $af,$40       ; * LAX absolute
            .byte $bf,$60       ; * LAX absolute,y
            .byte $a3,$20       ; * LAX (indirect,x)
            .byte $b3,$30       ; * LAX (indirect),y
            .byte $73,$e1       ; NOP               (aka NPO)
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
            .byte $9b,$1f       ; SLO               (aka ASO)
            .byte $07,$70       ; * SLO zero page
            .byte $17,$80       ; * SLO zero page,x
            .byte $0f,$40       ; * SLO absolute
            .byte $1f,$50       ; * SLO absolute,x
            .byte $1b,$60       ; * SLO absolute,y
            .byte $03,$20       ; * SLO (indirect,x)
            .byte $13,$30       ; * SLO (indirect),y
            .byte $9c,$8b       ; SRE               (aka LSE)
            .byte $47,$70       ; * SRE zero page
            .byte $57,$80       ; * SRE zero page,x
            .byte $4f,$40       ; * SRE absolute
            .byte $5f,$50       ; * SRE absolute,x
            .byte $5b,$60       ; * SRE absolute,y
            .byte $43,$20       ; * SRE (indirect,x)
            .byte $53,$30       ; * SRE (indirect),y
            .byte $9a,$31       ; SHX               (aka SXA, XAS)
            .byte $9e,$60       ; * SHX absolute,y
            .byte $9a,$33       ; SHY               (aka SYA, SAY)
            .byte $9c,$50       ; * SHY absolute,x
            .byte $a3,$e1       ; TOP               (aka NOP, SKW)
            .byte $0c,$40       ; * TOP absolute
            .byte $1c,$50       ; * TOP absolute,x
            .byte $5c,$50       ; * TOP absolute,x
            .byte $7c,$50       ; * TOP absolute,x
            .byte $dc,$50       ; * TOP absolute,x
            .byte $fc,$50       ; * TOP absolute,x
            .byte $0b,$8b       ; ANE               (aka XAA)
            .byte $8b,$a0       ; * ANE immediate
            .byte $9a,$27       ; SHS               (aka TAS, XAS)
            .byte $9b,$60       ; * SHS absolute,y
            .byte $50,$5b       ; JAM               (aka HLT, KIL)
            .byte $02,$b0       ; * JAM implied           
            .byte $43,$29       ; HLT               (aka HLT, JAM)
            .byte $02,$b0       ; * HLT implied
            .byte XTABLE_END,$00; End of 6502 extended table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; wAx USER PLUG-INS
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

uwAxfer:    jmp ph_waxfer
            ; Command template
            .asc $00,".U [B/T/ADDR]",$00
ph_waxfer:  php
            lsr TERMMODE        ; Default to not Terminal Mode
            lsr BASIC           ; Default to not BASIC
            lda #0              ; Initialize 2-byte header read
            sta HEADER          ; ,,
            plp
            bcc ch_pk           ; If no address is provided, check for T or B
            jsr Addr2CP         ; Set CP to starting addres
            lda #2              ; Pretend that the header has already been
            sta HEADER          ;   received
            jmp setup           ; Continue to hardware setup
ch_pk:      jsr ResetIn         ; Get first character
            jsr CharGet         ; ,,
            cmp #"T"            ; Terminal mode
            bne ch_basic        ; ,,
            lda #2              ; Consider the header done for terminal mode
            sta HEADER          ; ,,
            sec                 ; Set Terminal mode
            ror TERMMODE        ; ,,
            jmp setup           ; Continue setup
ch_basic:   cmp #"B"            ; Is BASIC specified?
            bne setup           ; If not, just treat incoming data as a PRG
            sec                 ; Turn on BASIC flag
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
standby:    lsr RUNNING         ; Clear RUNNING flag
wait:       bit TIMER           ; Has the timer advanced 64 jiffies?
            bvc ch_stop         ;   If not, check for STOP
            bit RUNNING         ;   If so, has the first byte come?
            bmi Complete        ;   If so, end and show info
ch_stop:    jsr ISCNTC          ; Check for STOP key
            beq Complete        ; If it's pressed, end data receive, show info
            bit RECEIVED        ; If data receive flag is off, just wait
            bpl wait            ; ,,
            lsr RECEIVED        ; Turn the flag off
            lda W_ADDR          ; Are we at a multiple of 64 bytes?
            and #%00111111      ; ,,
            bne wait            ; If not, just get more data
            jsr Progress        ; Show progress header  
            lda #CRSRUP         ; Cursor back up
            jsr CHROUT          ; ,,
            jmp wait            ; Back to start of wait loop

; Show Received Data Range            
Complete:   bit RUNNING         ; Was any data received?
            bpl comp_r          ;   If not, just end without showing anything
is_data:    jsr Progress        ; Show progress header
            bit BASIC           ; Rechain BASIC program, if specified with B
            bpl comp_r          ; ,,
            jsr Rechain         ; ,,
comp_r:     jmp (READY) 

; Show Progress Header
Progress:   jsr ResetOut        ; Show final locations...
            jsr AddrPrefix      ; Show address prefix
            jsr ShowCP          ;   Starting address        
            jsr Space           ;     to
            jsr ShowAddr        ;   end address
            jmp PrintBuff       ;   ,,            
  
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
            bit BASIC           ; If in BASIC mode, instead of setting starting
            bpl store_addr      ;   addresses from incoming data, set them
            lda $2b,x           ;   from start-of-BASIC Stage
store_addr: sta W_ADDR,x        ;   ,,
            sta C_PT,x          ;   ,,
            inc HEADER          ; Advance header
            bne ser_r           ; Return from interrupt
prg:        ldx #0              ; Store PRG data byte in current location
            sta (W_ADDR,x)      ; ,,
            stx TIMER           ; Reset time since last byte
            sec                 ; Set byte received flag for caller
            ror RECEIVED        ; ,,
            sec                 ; Set when first data is received
            ror RUNNING         ; ,,
next_addr:  jsr IncAddr         ; Increment working address
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
; RELOCATE
; https://github.com/Chysn/VIC20-wAx2/wiki/Plug-In:-Relocate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relocate Workspace
SRC_END     = $0247             ; End of source range (2 bytes)
DEST_END    = $0249             ; End of destination range (2 bytes)
OFFSET      = $024b             ; Offset (C_PT - W_ADDR, 2 bytes)
-OPERAND    = $024d             ; Instruction operand (2 bytes)           

            ; Parameter collection
uRelocate:  jmp ph_reloc
            ; Command template
            .asc $00,".U FROM TO TGT",$00
ph_reloc:   bcs okay            ; Error if invalid first argument (source start)
error:      jmp SyntaxErr       ; ?SYNTAX ERROR, warm start
okay:       jsr HexGet          ; Get high byte of source end
            bcc error           ; ,,
            sta SRC_END+1       ; ,,
            jsr HexGet          ; Get low byte of source end
            bcc error           ; ,,
            sta SRC_END         ; ,,
            jsr HexGet          ; Get high byte of destination start
            bcc error           ; ,,
            sta C_PT+1          ; ,,
            jsr HexGet          ; Get low byte of destination start
            bcc error           ; ,,
            sta C_PT            ; ,,
            sec                 ; Calculate default offset (C_PT - W_ADDR)
            sbc W_ADDR          ;   This is initially used to calculate an
            sta OFFSET          ;   end-of-destination address, but both end-of-
            lda C_PT+1          ;   destination and offset can be overridden by
            sbc W_ADDR+1        ;   additional optional 16-bit parameters.
            sta OFFSET+1        ;   ,,
            lda OFFSET          ; Calculate default end-of-destination 
            clc                 ;   address, for checking the end of the
            adc SRC_END         ;   relocate loop.
            sta DEST_END        ;   ,,
            lda OFFSET+1        ;   ,,
            adc SRC_END+1       ;   ,,
            sta DEST_END+1      ;   ,,
            jsr HexGet          ; Get high byte of destination end
            bcc Relocate        ;   This is optional, so begin if not provided
            sta DEST_END+1      ;   ,,
            jsr HexGet          ; Get low byte of destination end
            bcc error           ;   If only high byte was provided, but not the
            sta DEST_END        ;   low byte, it's a syntax error
            jsr HexGet          ; Get high byte of offset override
            bcc Relocate        ;   This is optional, so begin if not provided
            sta OFFSET+1        ;   ,,
            jsr HexGet          ; Get low byte of offset override
            bcc error           ;   If only high byte was provided, but not the
            sta OFFSET          ;   low byte, it's a syntax error
            ; Fall through to Relocate
            
            ; Relocation process
Relocate:   ldy #0              ; Load instruction opcode
            lda (C_PT),y        ; ,,
            jsr SizeOf          ; Get instruction size in X
            cpx #3              ; 1/2 bytes - Always advance to next instruction
            bne rnext           ; ,,
            iny                 ; 3 bytes - Something to potentially update.
            lda (C_PT),y        ;   Grab the operand from the two bytes after
            sta OPERAND         ;   the opcode...
            cmp W_ADDR          ;   ,,            
            iny                 ;   ,,
            lda (C_PT),y        ;   ,,
            sta OPERAND+1       ;   ,,
            sbc W_ADDR+1        ; ...then check whether the range of the operand
            bcc rnext           ;   is within the original code range
            lda OPERAND         ;   ,,
            cmp SRC_END         ;   ,,
            lda OPERAND+1       ;   ,,
            sbc SRC_END+1       ;   ,,
            bcs rnext           ;   If not, advance to the next instruction
            dey                 ;   ,,
            lda OPERAND         ; If the operand is within this code range,
            adc OFFSET          ;   add the offset and copy it
            sta (C_PT),y        ;   ,,
            iny                 ;   ,,
            lda OPERAND+1       ;   ,,
            adc OFFSET+1        ;   ,,
            sta (C_PT),y        ;   ,,
            jsr ResetOut        ; Show list of changed addresses
            jsr AddrPrefix      ; ,,
            jsr ShowCP          ; ,,
            jsr PrintBuff       ; ,,
            ldx #$03            ; ,, (reset X back to 3 for incrementing C_PT)
rnext:      txa                 ; Add X (instruction size) to the source pointer
            clc                 ; ,,
            adc C_PT            ; ,,
            sta C_PT            ; ,,
            bcc rcheck_end      ; ,,
            inc C_PT+1          ; ,,
rcheck_end: cmp DEST_END        ; Have we reached the end of the range?
            lda C_PT+1          ; ,,
            sbc DEST_END+1      ; ,,
            bcc Relocate        ; Get next instruction in the code
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DEBUG
; https://github.com/Chysn/VIC20-wAx2/wiki/Plug-In:-Debug
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BASIC Routines
UND_ERROR   = $c8e3             ; UNDEF'D STATEMENT ERROR

; Knapsack Generator
KNAPSACK    = $03ef             ; Knapsack storage (10 bytes)
BREAKPT     = KNAPSACK+10       ; Breakpoint address (2 bytes)
KNAPSIZE    = BREAKPT+2         ; Knapsack size (1 byte)

; Main routine entry point
; * If setting a breakpoint, its address is in W_ADDR vector
; * If clearing a breakpoint, the Carry flag is clear
uDebug:     jmp ph_debug
            ; Command template
            .asc $00,".U [ADDR]",$00 
ph_debug:   bcs NewKnap         ; A legal address has been provided in $a6/$a7
restore:    lda BREAKPT         ; Otherwise, restore the breakpoint to the
            sta W_ADDR          ;   original code by copying the
            lda BREAKPT+1       ;   bytes in the knapsack back to the code
            sta W_ADDR+1        ;   ,,
            ldy KNAPSIZE        ; Get knapsack code size (3-5, or 0)
            beq restore_r       ; Don't restore if KNAPSIZE isn't set
            dey
-loop       lda KNAPSACK+2,y    ; Move between 3 and 5 bytes back
            sta (W_ADDR),y      ;   to their original locations
            dey                 ;   ,,
            bpl loop            ;   ,,
            lda #$00            ; Reset the knapsack size so that doing it again
            sta KNAPSIZE        ;   doesn't mess up the original code
restore_r:  rts                 ;   ,,

; New Knapsack
; Generate a new knapsack at the working address
NewKnap:    lda KNAPSIZE        ; Don't install a knapsack if there's already
            bne knap_r          ;   one installed
            ldy #$00            ; (BRK)
            sty KNAPSACK        ; ,,
            lda #$ea            ; (NOP)
            sta KNAPSACK+1      ; ,,
next_inst:  tya                 ; Preserve Y against SizeLookup
            pha                 ; ,,
            lda (W_ADDR),y      ; A = Opcode of the breakpoint instruction
            jsr SizeLookup      ; X = Size of instruction (1-3)
            pla
            tay
            bcs xfer            ; Error if branch or unknown instruction
            jmp UND_ERROR        ; ?UNDEF'D STATEMENT ERROR     
xfer:       lda (W_ADDR),y      ; Move X bytes starting at Y index
            sta KNAPSACK+2,y    ; Y is a running count of knapsacked bytes
            iny                 ; ,,
            dex                 ; ,,
            bne xfer            ; ,,
            cpy #$03            ; If at least three bytes have been knapsacked
            bcc next_inst       ;   we're done
            lda W_ADDR          ; Stash pointer in breakpoint storage for
            sta BREAKPT         ;   later restoration
            lda W_ADDR+1        ;   ,,
            sta BREAKPT+1       ;   ,,
            sty KNAPSIZE        ; Save knapsack size for later
            lda #$ea            ; (NOP)
-loop:      cpy #$03            ; Pad code with more than three bytes with
            beq add_kjmp        ;   NOPs after the first three
            dey                 ;   ,,
            sta (W_ADDR),y      ;   ,,
            bne loop            ;   ,,
add_kjmp:   ldy #$00
            lda #$4c            ; (JMP) This is the JMP to the knapsack
            sta (W_ADDR),y      ; 
            lda #<KNAPSACK      ; Store knapsack JMP low byte
            iny                 ; ,,
            sta (W_ADDR),y      ; ,,
            lda #>KNAPSACK      ; Store knapsack JMP high byte
            iny                 ; ,,
            sta (W_ADDR),y      ; ,,
            lda KNAPSIZE        ; Calculate the return jump point (original
            tay                 ;   address + Y)
            clc                 ;   ,,
            adc W_ADDR          ;   ,,
            sta KNAPSACK+3,y    ; Store return JMP low byte
            lda #$00
            adc W_ADDR+1 
            sta KNAPSACK+4,y    ; Store return JMP high byte
            lda #$4c            ; (JMP) This is the JMP to the return point        
            sta KNAPSACK+2,y    ; ,,
knap_r:     rts 
    
; Size Of Instruction
; Given an opcode in A, return instruction size in X and set Carry flag
; Carry clear indicates an error (unknown or relative branch instruction)        
SizeLookup: jsr Lookup
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
; https://github.com/Chysn/VIC20-wAx2/wiki/Plug-In:-ML-to-BASIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ML2BAS Workspace
LINE_NUM    = $0247             ; BASIC Line Number (2 bytes)
MODIFIER    = $0249             ; Relocate or absolute
FAIL_POINT  = $024a             ; BASIC program end restore point (2 bytes)

uML2BAS:    jmp ph_ml2bas
            ; Command template
            .asc $00,".U FROM TO+1 [R/H/T]",$00
ph_ml2bas:  bcc merror          ; Error if the first address is no good
            jsr HexGet          ; Get high byte of range end
            bcc merror          ; ,,
            sta RANGE_END+1     ; ,,
            jsr HexGet          ; Get low byte of range end
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
            lda $2b             ; Set Command Pointer with start of
            sta C_PT            ;   BASIC
            lda $2c             ;   ,,
            sta C_PT+1          ;   ,,
            lda #$64            ; Start at line 100 by default
            sta LINE_NUM        ; ,,
            lda #$00            ; ,,
            sta LINE_NUM+1      ; ,,
            jsr NextLink        ; Is there an existing BASIC program?
            bcc found_end       ; If no existing program,
            jsr FindEnd         ; Find the last line number
            jsr IncLine         ; Increment it by 5
            lda C_PT            ; Set fail point, which preserves the existing
            sta FAIL_POINT      ;   BASIC program if the ML2BAS process results
            lda C_PT+1          ;   in an out of memory condition.
            sta FAIL_POINT+1    ;   ,,
found_end:  lda MODIFIER        ; If the code is not relocatable, skip the
            beq mStart          ;   PC setting up front
            jsr LinkBytes       ; Add link bytes to next line
            jsr LineNumber      ; Add line number to first line
            lda #WEDGE          ; Add wedge character to line
            jsr AddByte         ; ,,
            jsr AddBASCP		; Add * token to line
            jsr ResetOut        ; Add the start address to the output buffer
            jsr ShowAddr        ; ,,
            jsr AddBuffer       ; Add the output buffer to the BASIC line
            jsr mEndLine        ; Finish the first BASIC line
            jmp mStart          ; Start adding lines of 6502 code
merror:     jmp SyntaxErr       ; ?SYNTAX ERROR, warm start
mStart:     jsr mChRange
            bcc range_ok
mdone:      jsr mEnd            ; Add $00,$00 to the the program
            jsr Rechain         ; Rechain BASIC program
            jmp ($c002)         ; READY.
range_ok:   jsr LinkBytes
            jsr LineNumber
            lda #WEDGE          ; Add the wedge character
            jsr AddByte         ; ,,
            lda #T_ASM_AL       ; Default to the assembler tool, but...
            ldy MODIFIER        ; If the modifier is T (assertion test), then
            cpy #"T"            ;   switch the tool over to =
            bne show_tool       ;   ,,
            lda #$b2            ;   ,,
show_tool:  jsr AddByte         ; Add the selected tool to the buffer
            lda MODIFIER        ; If the user requested relocatable code,
            beq show_addr       ;   add the * instead of the address
            cmp #"T"            ;   ,,
            beq show_addr       ;   ,,
            jsr AddBASCP        ;   ,, Add * token
            jmp code_part2      ;   ,,
show_addr:  jsr ResetOut        ; Add the current address to the BASIC line
            jsr ShowAddr        ; ,,
            jsr AddBuffer       ; ,,
code_part:  lda #" "            ; Space after address or *
            jsr AddByte         ; ,,
code_part2: jsr ResetOut        ; Reset output for the code portion
            lda MODIFIER        ; If the disassembly is in relocate mode,
            beq gen_code        ;   
            cmp #"H"            ;   check for hex dump modifier and
            beq HexDump         ;   handle that, if necessary. Otherwise, check
            cmp #"T"            ;   for assertion test modified and
            beq HexDump         ;   handle that, if necessary. Otherwise, check
            jsr CheckRel        ;   for relative branch. If so, disassemble the
            bcs code2buff       ;   instruction with CP offset.
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
            jsr HexOut          ; ,,
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

AddBASCP:   lda #$ac            ; Add PC set tool
            ; Fall through to AddByte

 
; Add Byte
; Add the byte in Accumulator to the BASIC line            
AddByte:    pha
            ldx #$00
            sta (C_PT,x)
            jsr IncCP
            lda C_PT+1          ; Check memory for end of BASIC
            cmp $38             ; ,,
            bcc ok              ; ,,
            lda C_PT            ; ,,
            cmp $37             ; ,,
            beq OutOfMem        ; If at limit of memory, then ERROR
ok:         pla                 ; Who cares about PLA in case of error
            rts        

; Perform NEW, then show Out of Memory Error
OutOfMem:   jsr ResetOut        ; Show the current address, so the user
            jsr AddrPrefix      ;   knows where we ran out of BASIC
            jsr ShowAddr        ;   memory
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
            lda (C_PT),y        ;   it      
            sta LINE_NUM        ;   ,,
            iny                 ;   ,,
            lda (C_PT),y        ;   ,,
            sta LINE_NUM+1      ;   ,,
            lda $07             ; Get the next link pointer and update C_PT
            sta C_PT            ;   ,,
            lda $08             ;   ,,
            sta C_PT+1          ; Keep looking for the end
            jmp FindEnd

NextLink:   ldy #$00            ; Set locations $07 and $08 to the next
            lda (C_PT),y        ; BASIC line pointer. If both are $00, then
            sta $07             ; we're at the end of the BASIC program,
            iny                 ; otherwise, the BASIC program continues
            lda (C_PT),y        ; at the specified address
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
CheckRel:   ldx #$00            ; Check the instruction at the working address
            lda (W_ADDR,x)      ; ,,
            jsr Lookup          ; If it doesn't exist, exit
            bcc clc_r           ; ,,
            cmp #$c0            ; Is the instruction relative mode?
            bne clc_r           ; If not, exit
            jsr DMnemonic       ; Add branch instruction mnemonic
            jsr IncAddr         ; Increment to the operand
            jsr Space
            jsr HexPrefix
            lda #$ac            ; Add the * token
            jsr CharOut         ; ,,
            jsr IncAddr         ; Get the relative branch operand
            clc                 ; Add 2 to make it an offset from *
            adc #2              ; ,,
            tay                 ; Save offset in Y for later
            bpl pos_off         ; ,,
neg_off:    eor #$ff            ; If it's a negative offset, make absolute
            adc #1              ;   value
            tay                 ; Save offset in Y
            lda #$ab            ; Minus token
            .byte $3c           ; (TOP (skip word)
pos_off:    lda #$aa            ; Plus token
            jsr CharOut         ; ,,
            tya                 ; Get positive or negative offset back
            jsr HexOut          ;   and add to buffer
            sec                 ; Set carry indicates a relative instruction
            rts                 ;   has been handled
            
; Check Range
; Check to see if C_PT is greater than or equal to Range End  
; In range if Carry is clear; out of range if Carry is set    
mChRange:   lda RANGE_END+1
            cmp W_ADDR+1
            bcc out_range
            bne min_range
            lda W_ADDR
            cmp RANGE_END
            rts
out_range:  sec
            rts
min_range:  clc
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CYCLE COUNTER
; https://github.com/Chysn/VIC20-wAx2/wiki/Plug-In:-Cycle-Counter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
uCycle:     jmp ph_cycle
            .asc $00,".U ADDR",$00
ph_cycle:   bcs cyc_ok
            jmp SyntaxErr
cyc_ok:     sei
            jsr $fdf9           ; Initialize hardware registers
            lda #$fe            ; Reset timer
            sta $9114           ; ,,
            lda #$ff            ; ,,
            sta $9115           ; ,,
            lda W_ADDR          ; Set specified routine address for SYS
            sta $14             ;   ,,
            lda W_ADDR+1        ;   ,,
            sta $15             ;   ,,
            lda PROC            ; Make sure that SEI is still set when
            ora #$04            ;   SYS's processor flags are pulled
            sta PROC            ;   ,,
            jsr $e12d           ; SYS to set registers, run, and set memory
            lda $9114           ; Get current cycle counter
            ldy $9115           ; ,,
            sta W_ADDR          ; Put it in working address
            sty W_ADDR+1        ; ,,
            cli                 ; Reinstate interrupt
            jsr FNDVAR          ;   Find or create variable         
            lda #150            ; Compensate for preparatory code's cycles
            sec                 ; ,,
            sbc W_ADDR          ; ,,
            tay                 ; ,, Low byte Y for FAC conversion
            lda #$ff            ; ,,
            sbc W_ADDR+1        ; ,, High byte A for FAC conversion
            jsr SetUserVar      ; Put A/Y in FAC1 for display or variable set
            jsr DirectMode      ; If in direct mode, show the FAC1 value
            bne cyc_r           ;   ,,
show_cyc:   jsr CReturn         ;   ,, Put count alone on new line
            jsr $dddd           ;   Convert FAC1 to string
            jsr $cb1e           ;   Print it
            jmp CReturn         ;   Then carriage return
cyc_r:      rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BASIC AID
; https://github.com/Chysn/VIC20-wAx2/wiki/Plug-In:-BASIC-Aid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LINE_INC    = $0247             ; Line increment
CURR_NUM    = $0248             ; Current line number (2 bytes)
;FAIL_POINT = $024a             ; Fail point from ML2BASIC (2 bytes)
CONSEC_0S   = $024c             ; Consecutive zeroes

uBASIC:     jmp ph_basic
            ; Command templates
            .asc $00,".U R [L# [INC]]",$0d
            .asc ".U L S1 [S2..]",$0d
            .asc ".U O",$00
ph_basic:   jsr ResetIn         ; Route to utility
            jsr CharGet         ; ,,
            cmp #"O"            ; ,, Old utility
            beq Old             ; ,,
            cmp #"L"            ; ,, Link utility
            beq Link            ; ,,
            cmp #"R"            ; ,, Renumber utility
            bne basic_err       ; ,,
            jmp Renum           ; ,,
basic_err:  jmp SyntaxErr

; Old
; Restore a NEWed program
Old:        ldy #0              ; Index from start of memory
            sta ($2b),y         ; A is "O" here, but it doesn't matter
            iny                 ; Same as the next byte
            sta ($2b),y         ; ,,
            jsr Rechain         ; Fix the links
            jmp (READY)

; BASIC Link
; Copy the specified BASIC stage to the current stage
Link:       jsr HexGet          ; Get a hex byte, with syntax error if not
            bcc basic_err       ;   valid
            pha                 ; Store link source for later
            jsr BASIC2Addr      ; Set working address to BASIC stage
-loop:      jsr EndOfBASIC      ; Have we found the end of the program?
            beq found_eob       ; ,,
            jsr BASFollow       ; If not, follow to the next pointer
            jmp loop            ;   and see if that's the end
found_eob:  jsr Addr2CP         ; Destination address is now in CP
            lda C_PT            ; Set fail point, which preserves the existing
            sta FAIL_POINT      ;   BASIC program if the Link process results
            lda C_PT+1          ;   in an out of memory condition.
            sta FAIL_POINT+1    ;   ,,
next_prg:   pla                 ; Get back the link source and put it into
            sta W_ADDR+1        ;   the working address
            lda #$01            ;   ,,
            sta W_ADDR          ;   ,,
            ldy #0              ; Start copying code
            sty CONSEC_0S       ; Reset consecutive 0 counter
-loop:      lda (W_ADDR),y      ; Get source byte
            bne reset_0         ;   If it's a zero, then count it
            inc CONSEC_0S       ;   ,,
            bne copy_bas        ;   and add it to the copy
reset_0:    sty CONSEC_0S       ; When non-0, reset the consecutive 0 counter
copy_bas:   jsr AddByte         ; AddByte from ML2BASIC, checks out-of-memory
            jsr IncAddr         ; Increment the source
            lda #3              ; Have there been three consecutive zeroes
            cmp CONSEC_0S       ;   to end the source program?
            bne loop            ; If not, keep copying
            jsr HexGet          ; Is there another BASIC stage to link?
            bcc link_done       ; If so, do it
            pha                 ; Save the next stage page
            sec                 ; If there's another stage to link, subtract
            lda C_PT            ;   2 from the destination address, to
            sbc #$02            ;   compensate for the two program-ending
            sta C_PT            ;   zeroes
            bcs next_prg        ;   ,,
            dec C_PT+1          ;   ,,
            jmp next_prg        ;   ,,
link_done:  jsr Rechain         ; Fix all the broken links
            jmp (READY)         ; Return to BASIC READY prompt

; BASIC Renumber
; Renumber from specified line, with a specified increment
Renum:      lda #100            ; Set defaults for current line number
            sta CURR_NUM        ;   ,,
            lda #0              ;   ,,
            sta CURR_NUM+1      ;   ,,
            lda #10             ;   and line increment
            sta LINE_INC        ;   ,,
            jsr HexGet          ; If a line number is not provided, just start
            bcc start_ren       ;   ,,
            sta CURR_NUM        ; Set the starting line number (low byte)
            jsr HexGet          ; Now get the increment
            bcc start_ren       ; 
            sta LINE_INC        ; Store line increment
start_ren:  jsr BASIC2Addr      ; Set working address to BASIC stage
-loop:      jsr EndOfBASIC      ; Is this the end of the BASIC program?
            bne set_num         ; ,,
            jmp (READY)         ; If so, BASIC READY prompt
set_num:    ldy #2              ; Store the current line number after the line
            lda CURR_NUM        ;   pointer
            sta (W_ADDR),y      ;   ,,
            lda CURR_NUM+1      ;   ,,
            iny                 ;   ,,
            sta (W_ADDR),y      ;   ,,
            lda LINE_INC        ; Increment the next line number by the
            clc                 ;   line increment amount
            adc CURR_NUM        ;   ,,
            sta CURR_NUM        ;   ,,
            bcc adv_ptr         ;   ,,
            inc CURR_NUM+1      ;   ,,
adv_ptr:    jsr BASFollow       ; Follow working address to next pointer
            jmp loop            ; Check for completion and continue

; BASIC stage to working address
BASIC2Addr: lda $2b             ; Get the BASIC starting address into the
            sta W_ADDR          ;   working address
            lda $2c             ;   ,,
            sta W_ADDR+1        ;   ,,
            rts

; End of BASIC
; Based on pointer in working address
;     jsr EndOfBASIC
;     bne no
;     beq yes
EndOfBASIC: ldy #0              ; Look at pointer to the next line. If both
            tya                 ;   bytes are zero, the renumber is done
            cmp (W_ADDR),y      ;   ,,
            bne eob_r           ;   ,,
            iny                 ;   ,,
            cmp (W_ADDR),y      ;   ,,
            bne eob_r           ;   ,,
eob_r:      rts  

; BASIC Line Follow
; Use working address pointer to update to the next line
BASFollow:  ldy #0              ; Advance the working address to the pointer
            lda (W_ADDR),y      ;   specified at the current working address
            pha                 ;   ,,
            iny                 ;   ,,
            lda (W_ADDR),y      ;   ,,
            sta W_ADDR+1        ;   ,,
            pla                 ;   ,,
            sta W_ADDR          ;   ,,
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MEMORY CONFIG
; https://github.com/Chysn/VIC20-wAx2/wiki/About-wAxpander
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
uConfig:    jmp ph_conf
            ; Command template
            .asc $00,".U 0/3/8/16/24",$00
ph_conf:    jsr ResetIn         ; Get the character after the command
            jsr CharGet         ; ,,
            ldy #4              ; Look through the table for the character
-loop:      cmp ExpKey,y        ; ,,
            beq expfound        ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            jmp SyntaxErr       ; No valid character was found
expfound:   lda MemLo,y         ; A valid character was found, indicating a
            sta $0282           ;   memory configuration. So look up the values
            lda MemHi,y         ;   in the table, including mem low and high
            sta $0284           ;   bytes, and screen location
            lda ScrHi,y         ;   ,,
            sta $0288           ;   ,,
soft_reset: jsr $fd52           ; restore default I/O vectors
            jsr $fdf9           ; initialize I/O registers
            jsr $e518           ; initialise hardware
            cli                 ; enable interrupts            
            jsr $e45b           ; Initialise BASIC vector table
            jsr $e3a4           ; Initialise BASIC RAM locations
            jsr $e404           ; Print start up message and init pointers
            ldx #$fb            ; Value for start stack
            txs                 ; Set stack pointer
            jsr Install         ; Re-install wAx
            jmp $c474

; Memory settings table
; For             0K  3K  8K  16K 24K
ExpKey:     .asc  "0","3","8","1","2"
MemLo:      .byte $10,$04,$12,$12,$12
MemHi:      .byte $1e,$1e,$40,$60,$80
ScrHi:      .byte $1e,$1e,$10,$10,$10
