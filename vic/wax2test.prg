! �"�**  WAX TEST SUITE  **" ? � "�* ASSEMBLE ALL INSTR" X
 �"  TESTING...     " d .� 6000 vd .A � ADC #$00 �e .A � ADC $10 �f .A � ADC $20,X �g .A � ADC $3030 �h .A � ADC $4040,X �i .A � ADC $5050,Y �j .A � ADC ($60,X) 	k .A � ADC ($70),Y 	l .A � � #$80  	m .A � � $90 1	n .A � � $10,X B	o .A � � $1111 U	p .A � � $1212,X h	q .A � � $1313,Y {	r .A � � ($14,X) �	s .A � � ($15),Y �	t .A � ASL A �	u .A � ASL $17 �	v .A � ASL $18,X �	w .A � ASL $1919 �	x .A � ASL $2020,X �	y .A � BCC $6031 
z .A � BCS $6033 "
{ .A � BEQ $6035 3
| .A � BIT $24 F
} .A � BIT $2525 Y
~ .A � BMI $603C l
 .A � BNE $603E 
� .A � BPL $6040 �
� .A � BRK  �
� .A � BVC $6043 �
� .A � BVS $6045 �
� .A � CLC  �
� .A � CLD  �
� .A � CLI  �
� .A � CLV  �
� .A � CMP #$36 � .A � CMP $37 !� .A � CMP $38,X 4� .A � CMP $3939 I� .A � CMP $4040,X ^� .A � CMP $4141,Y s� .A � CMP ($42,X) �� .A � CMP ($43),Y �� .A � CPX #$44 �� .A � CPX $45 �� .A � CPX $4646 �� .A � CPY #$47 �� .A � CPY $48 �� .A � CPY $4949 � .A � DEC $50 � .A � DEC $51,X +� .A � DEC $5252 @� .A � DEC $5353,X N� .A � DEX  \� .A � DEY  m� .A � E� #$56 }� .A � E� $57 �� .A � E� $58,X �� .A � E� $5959 �� .A � E� $6060,X �� .A � E� $6161,Y �� .A � E� ($62,X) �� .A � E� ($63),Y � .A � INC $64 � .A � INC $65,X (� .A � INC $6666 =� .A � INC $6767,X K� .A � INX  Y� .A � INY  l� .A � JMP $7070 �� .A � JMP ($7171) �� .A � JSR $7272 �� .A � LDA #$73 �� .A � LDA $74 �� .A � LDA $75,X �� .A � LDA $7676 �� .A � LDA $7777,X � .A � LDA $7878,Y � .A � LDA ($79,X) 1� .A � LDA ($80),Y C� .A � LDX #$81 T� .A � LDX $82 g� .A � LDX $83,Y z� .A � LDX $8484 �� .A � LDX $8585,Y �� .A � LDY #$86 �� .A � LDY $87 �� .A � LDY $88,X �� .A � LDY $8989 �� .A � LDY $9090,X �� .A � LSR A � .A � LSR $92  � .A � LSR $93,X 3� .A � LSR $9494 H� .A � LSR $9595,X V� .A � NOP  g� .A � �A #$97 w� .A � �A $98 �� .A � �A $99,X �� .A � �A $1000 �� .A � �A $1010,X �� .A � �A $2020,Y �� .A � �A ($30,X) �� .A � �A ($40),Y �� .A � PHA  � .A � PHP  � .A � PLA  #� .A � PLP  2� .A � ROL A C� .A � ROL $10 V� .A � ROL $11,X i� .A � ROL $1212 ~� .A � ROL $1313,X �� .A � R� A �� .A � R� $15 �� .A � R� $16,X �� .A � R� $1717 �� .A � R� $1818,X �� .A � RTI  �� .A � RTS  � .A � SBC #$21 � .A � SBC $22 &� .A � SBC $23,X 9� .A � SBC $2424 N� .A � SBC $2525,X c� .A � SBC $2626,Y x� .A � SBC ($27,X) �� .A � SBC ($28),Y �� .A � SEC  �� .A � SED  �� .A � SEI  �� .A � STA $32 �� .A � STA $33,X �� .A � STA $3434 � .A � STA $3535,X � .A � STA $3636,Y -� .A � STA ($37,X) B� .A � STA ($38),Y S� .A � STX $39 f� .A � STX $40,Y y� .A � STX $4141 �� .A � STY $42 �� .A � STY $43,X �� .A � STY $4444 �� .A � TAX  �� .A � TAY  �� .A � TSX  �� .A � TXA  �� .A � TXS  � .A � TYA  .� 6000 "-.� � 69006510 4..� � 75206D30 F/.� � 307D4040 X0.� � 79505061 j1.� � 60717029 |2.� � 80259035 �3.� � 102D1111 �4.� � 3D121239 �5.� � 13132114 �6.� � 31150A06 �7.� � 1716180E �8.� � 19191E20 �9.� � 2090FEB0 :.� � FEF0FE24 ;.� � 242C2525 0<.� � 30FED0FE B=.� � 10FE0050 T>.� � FE70FE18 f?.� � D858B8C9 x@.� � 36C537D5 �A.� � 38CD3939 �B.� � DD4040D9 �C.� � 4141C142 �D.� � D143E044 �E.� � E445EC46 �F.� � 46C047C4 �G.� � 48CC4949 H.� � C650D651 I.� � CE5252DE ,J.� � 5353CA88 >K.� � 49564557 PL.� � 55584D59 bM.� � 595D6060 tN.� � 59616141 �O.� � 625163E6 �P.� � 64F665EE �Q.� � 6666FE67 �R.� � 67E8C84C �S.� � 70706C71 �T.� � 71207272 �U.� � A973A574 V.� � B575AD76 W.� � 76BD7777 (X.� � B97878A1 :Y.� � 79B180A2 LZ.� � 81A682B6 ^[.� � 83AE8484 p\.� � BE8585A0 �].� � 86A487B4 �^.� � 88AC8989 �_.� � BC90904A �`.� � 46925693 �a.� � 4E94945E �b.� � 9595EA09 �c.� � 97059815  d.� � 990D0010 e.� � 1D101019 $f.� � 20200130 6g.� � 11404808 Hh.� � 68282A26 Zi.� � 1036112E lj.� � 12123E13 ~k.� � 136A6615 �l.� � 76166E17 �m.� � 177E1818 �n.� � 4060E921 �o.� � E522F523 �p.� � ED2424FD �q.� � 2525F926 �r.� � 26E127F1 s.� � 2838F878  t.� � 85329533 2u.� � 8D34349D Dv.� � 35359936 Vw.� � 36813791 hx.� � 38863996 zy.� � 408E4141 �z.� � 84429443 �{.� � 8C4444AA �|.� � A8BA8A9A �}.� � 98 �~� "�           [AWESOME]�" �� ** TEST 3 EDIT TESTS �� "* MEMORY EDITORS" *�� "  TESTING...     " B�., 6800:00 11 22 33 Z�., 6804:44 55 66 77 r�., 6808:88 99 AA BB ��., 680C:CC DD EE FF ��., 6810"TEXT1234" ��., 6818 �"ABCD" ��.� 6800 00 11 22 33 ��.� 6804 44 55 66 77 ��.� 6808 88 99 AA BB �.� 680C CC DD EE FF ,�.� 6810 54 45 58 54 D�.� 6814 31 32 33 34 \�.� 6818 01 02 03 04 |�� "�             [SWEET]�" ��� ** TEST 4 IMMED OPERANDS ��� "* IMMEDIATE OPERANDS" ���"  TESTING...     " ��.@� ��.A 6800 LDA #"�" �.A 6802 �A #%11010110 �.A �    LDX #$B2�3 4�.A �    LDY #100 J�.A �    LDA #�"J" k�.� 6800 A99309D6A2B5A064A910 ��� "�               [LIT]�" �X� "* SYMBOLIC ASSEMBLY" �Y� "  TESTING...     " �Z.@� �\.@A 02A5 ; TEST � �].� 6800 ; SET PC ^� TEST FWD REF _.A �    JSR @& /`.A �    LDA #�@& Da.A �    LDX #�@& Wb.A �    BCC @2 oc.A � @F LDA (�@D,X) {d.A � @2 �e.A � @&  ;RESOLVE FWD �f� TEST BACK �g.A � @X JMP (@@) �h.A �    JSR @X �i.A � @@ �j.A �    BCC @@  k.A � @F INY ;RE� l.A �    LDA @F *m.A �    E� �@A,X <n.A � @D "JEJ" Mo� UNRESOLVED `p.A �    JMP @U tq� VERIFY MEMORY �r.� 6800 200B68A9 �s.� 6804 0BA26890 �t.� 6808 02A1196C �u.� 680C 1168200B �v.� 6810 6890FEC8 �w.� 6814 AD136855 x.� 6818 024A454A y.� 681C 4C0000 :z� "�          [WONDROUS]�" V�� "* BASIC INTEGRATION" o��"  TESTING...     " w�.@� ��P � 0 ��P � P � 1 ��� P � 2 � � "PASS ERROR":� ��.� 6800 ��� I � 1 � 24 ��.A � BCC @F ��� I ��.A � @F ��� UR% � 715 �� P �� 2 � � "PASS ERROR":� B�� CP �� 26672 � � "CP ERROR":� b�� "�              [DOPE]�" |!� "* PARAM MODIFIERS" �#�"  TESTING...     " �%.A 6800 LDX #$50�1 �*.A �    LDA ($FA�2,X) �/.A �    LDY #100�5 �4.A �    CMP #%00001111�F  9.A �    LDA #"Z"�1 - >P � 65490 : CH � �("@") @ C.A �    JSR 'P T H., �    CPY 'CH d M.A �    BRK � R.� 6800 A251A1FCA05FC900 � W.� �    A95B20D2FFC44000 � \V � 100 : VR � 40 � a., 6800 LDA ('V,X) � f., �    LDA ('V),Y � k., �    LDA ('VR,X) !p., �    LDA ('VR),Y /!z.� 6800 A164B164A128B128 O!�� "�           [FAR OUT]�" k!��:� "ALL TESTS PASS!"   