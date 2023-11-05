/Users/jej/personal/VIC20-wAx2/vic


;wax2test2.prg ==1201==
  100 print "{clr}{red}wax2 output test suite{blk}"
  105 gosub 670
  110 rem **** m ****
  120 print "m tool..";
  130 .u on
  140 .m c000 c400
  150 .u off
  160 .=0001 72da
  170 print ". {blu}[pass]{blk}"
  180 rem **** i ****
  190 print "i tool..";
  200 .u on
  210 .i d000 d400
  220 .u off
  230 .=0001 145d
  240 print ". {blu}[pass]{blk}"
  250 rem **** % ****
  260 print "%{$a0}tool..";
  270 .u on
  280 .% 8000 8100
  290 .u off
  300 .=0001 f6ff
  310 print ". {blu}[pass]{blk}"
  320 print "c tool..";
  330 .=0001 f6ff
  340 .u on
  350 .c 8000 8100 8800
  360 .u off
  370 .=0001 967e
  380 print ". {blu}[pass]{blk}"
  390 rem **** d ****
  400 print "d tool..";
  410 .u on
  420 .d eabf eb25
  430 .u off
  440 .=0001 ce12
  450 print ". {blu}[pass]{blk}"
  460 rem **** h ****
  470 print "h tool"
  480 print "  hex ..";
  490 .u on
  500 .h 8000:80 40 20
  510 .u off
  520 .=0001 070b
  530 print ". {blu}[pass]{blk}"
  540 print "  text..";
  550 .u on
  560 .h c000 "cbm"
  570 .u off
  580 .=0001 0749
  590 print ". {blu}[pass]{blk}"
  600 print "  code..";
  610 .u on
  620 .h d000 ldy #$==
  630 .u off
  640 .=0001 3c6f
  650 print ". {blu}[pass]{blk}"
  660 print:print "{grn}all tests pass!{blu}":end
  670 .@-
  680 v = peek(807)*256+peek(806)
  690 .@v 'v'
  700 a=1024
  720 .,'a' jmp @s
  730 .,*   :00
  740 .,*   ".u on/off"
  750 .,*   :00
  760 .,*@s jsr $a01b
  770 .,*   jsr $a006
  780 .,*   jsr $a006
  790 .,*   cmp #"n"
  800 .,*   beq @e
  810 .,*   cmp #"f"
  820 .,*   beq @u
  830 .,*   jmp $a039
  840 .,*@u lda #<@v
  850 .,*   sta $0326
  860 .,*   lda #>@v
  870 .,*   sta $0327
  880 .,*   lda #$1b
  890 .,*   sta $900f
  900 .,*   rts
  910 .,*@e lda #0
  920 .,*   sta $02
  930 .,*   sta $01
  935 .,*   sta $00
  940 .,*   lda #<@c
  950 .,*   sta $0326
  960 .,*   lda #>@c
  970 .,*   sta $0327
  980 .,*   rts
  990 .,*@c inc $00
  995 .,*   eor $00
  997 .,*   clc
 1000 .,*   adc $02
 1010 .,*   sta $02
 1020 .,*   lda #0
 1030 .,*   adc $01
 1040 .,*   sta $01
 1050 .,*   lda $900f
 1060 .,*   eor #$01
 1070 .,*   sta $900f
 1080 .,*   lda #0
 1090 .,*   jmp @v
 1100 .p 'a'
 1110 print"installed checksum..."
 1120 return

