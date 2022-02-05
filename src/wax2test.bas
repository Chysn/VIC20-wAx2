1 print"{clr}{red}**  wax test suite  **"
7 print "{black}* assemble all instr"
10 print"  testing...     ";
15 rem ** test 1 6502 assembly
20 .* 6000
100 .a * adc #$00
101 .a * adc $10
102 .a * adc $20,x
103 .a * adc $3030
104 .a * adc $4040,x
105 .a * adc $5050,y
106 .a * adc ($60,x)
107 .a * adc ($70),y
108 .a * and #$80
109 .a * and $90
110 .a * and $10,x
111 .a * and $1111
112 .a * and $1212,x
113 .a * and $1313,y
114 .a * and ($14,x)
115 .a * and ($15),y
116 .a * asl a
117 .a * asl $17
118 .a * asl $18,x
119 .a * asl $1919
120 .a * asl $2020,x
121 .a * bcc $6031
122 .a * bcs $6033
123 .a * beq $6035
124 .a * bit $24
125 .a * bit $2525
126 .a * bmi $603c
127 .a * bne $603e
128 .a * bpl $6040
129 .a * brk 
130 .a * bvc $6043
131 .a * bvs $6045
132 .a * clc 
133 .a * cld 
134 .a * cli 
135 .a * clv 
136 .a * cmp #$36
137 .a * cmp $37
138 .a * cmp $38,x
139 .a * cmp $3939
140 .a * cmp $4040,x
141 .a * cmp $4141,y
142 .a * cmp ($42,x)
143 .a * cmp ($43),y
144 .a * cpx #$44
145 .a * cpx $45
146 .a * cpx $4646
147 .a * cpy #$47
148 .a * cpy $48
149 .a * cpy $4949
150 .a * dec $50
151 .a * dec $51,x
152 .a * dec $5252
153 .a * dec $5353,x
154 .a * dex 
155 .a * dey 
156 .a * eor #$56
157 .a * eor $57
158 .a * eor $58,x
159 .a * eor $5959
160 .a * eor $6060,x
161 .a * eor $6161,y
162 .a * eor ($62,x)
163 .a * eor ($63),y
164 .a * inc $64
165 .a * inc $65,x
166 .a * inc $6666
167 .a * inc $6767,x
168 .a * inx 
169 .a * iny 
170 .a * jmp $7070
171 .a * jmp ($7171)
172 .a * jsr $7272
173 .a * lda #$73
174 .a * lda $74
175 .a * lda $75,x
176 .a * lda $7676
177 .a * lda $7777,x
178 .a * lda $7878,y
179 .a * lda ($79,x)
180 .a * lda ($80),y
181 .a * ldx #$81
182 .a * ldx $82
183 .a * ldx $83,y
184 .a * ldx $8484
185 .a * ldx $8585,y
186 .a * ldy #$86
187 .a * ldy $87
188 .a * ldy $88,x
189 .a * ldy $8989
190 .a * ldy $9090,x
191 .a * lsr a
192 .a * lsr $92
193 .a * lsr $93,x
194 .a * lsr $9494
195 .a * lsr $9595,x
196 .a * nop 
197 .a * ora #$97
198 .a * ora $98
199 .a * ora $99,x
200 .a * ora $1000
201 .a * ora $1010,x
202 .a * ora $2020,y
203 .a * ora ($30,x)
204 .a * ora ($40),y
205 .a * pha 
206 .a * php 
207 .a * pla 
208 .a * plp 
209 .a * rol a
210 .a * rol $10
211 .a * rol $11,x
212 .a * rol $1212
213 .a * rol $1313,x
214 .a * ror a
215 .a * ror $15
216 .a * ror $16,x
217 .a * ror $1717
218 .a * ror $1818,x
219 .a * rti 
220 .a * rts 
221 .a * sbc #$21
222 .a * sbc $22
223 .a * sbc $23,x
224 .a * sbc $2424
225 .a * sbc $2525,x
226 .a * sbc $2626,y
227 .a * sbc ($27,x)
228 .a * sbc ($28),y
229 .a * sec 
230 .a * sed 
231 .a * sei 
232 .a * sta $32
233 .a * sta $33,x
234 .a * sta $3434
235 .a * sta $3535,x
236 .a * sta $3636,y
237 .a * sta ($37,x)
238 .a * sta ($38),y
239 .a * stx $39
240 .a * stx $40,y
241 .a * stx $4141
242 .a * sty $42
243 .a * sty $43,x
244 .a * sty $4444
245 .a * tax 
246 .a * tay 
247 .a * tsx 
248 .a * txa 
249 .a * txs 
250 .a * tya 
260 rem ** test 2 verify 6502 assem
271 print "{blue}[ok]{black}"
272 print "* verify assembly"
273 print "  testing...     ";
274 .* 6000
301 .= * 69006510
302 .= * 75206d30
303 .= * 307d4040
304 .= * 79505061
305 .= * 60717029
306 .= * 80259035
307 .= * 102d1111
308 .= * 3d121239
309 .= * 13132114
310 .= * 31150a06
311 .= * 1716180e
312 .= * 19191e20
313 .= * 2090feb0
314 .= * fef0fe24
315 .= * 242c2525
316 .= * 30fed0fe
317 .= * 10fe0050
318 .= * fe70fe18
319 .= * d858b8c9
320 .= * 36c537d5
321 .= * 38cd3939
322 .= * dd4040d9
323 .= * 4141c142
324 .= * d143e044
325 .= * e445ec46
326 .= * 46c047c4
327 .= * 48cc4949
328 .= * c650d651
329 .= * ce5252de
330 .= * 5353ca88
331 .= * 49564557
332 .= * 55584d59
333 .= * 595d6060
334 .= * 59616141
335 .= * 625163e6
336 .= * 64f665ee
337 .= * 6666fe67
338 .= * 67e8c84c
339 .= * 70706c71
340 .= * 71207272
341 .= * a973a574
342 .= * b575ad76
343 .= * 76bd7777
344 .= * b97878a1
345 .= * 79b180a2
346 .= * 81a682b6
347 .= * 83ae8484
348 .= * be8585a0
349 .= * 86a487b4
350 .= * 88ac8989
351 .= * bc90904a
352 .= * 46925693
353 .= * 4e94945e
354 .= * 9595ea09
355 .= * 97059815
356 .= * 990d0010
357 .= * 1d101019
358 .= * 20200130
359 .= * 11404808
360 .= * 68282a26
361 .= * 1036112e
362 .= * 12123e13
363 .= * 136a6615
364 .= * 76166e17
365 .= * 177e1818
366 .= * 4060e921
367 .= * e522f523
368 .= * ed2424fd
369 .= * 2525f926
370 .= * 26e127f1
371 .= * 2838f878
372 .= * 85329533
373 .= * 8d34349d
374 .= * 35359936
375 .= * 36813791
376 .= * 38863996
377 .= * 408e4141
378 .= * 84429443
379 .= * 8c4444aa
380 .= * a8ba8a9a
381 .= * 98
382 print "{blue}[ok]{black}"
383 rem ** test 3 edit tests
385 print "* memory editors"
386 print "  testing...     ";
400 ., 6800:00 11 22 33
401 ., 6804:44 55 66 77
402 ., 6808:88 99 aa bb
403 ., 680c:cc dd ee ff
404 ., 6810"text1234"
410 .= 6800 00 11 22 33
411 .= 6804 44 55 66 77
412 .= 6808 88 99 aa bb
413 .= 680c cc dd ee ff
414 .= 6810 54 45 58 54
415 .= 6814 31 32 33 34
420 print "{blue}[ok]{black}"
500 rem ** test 4 immed operands
501 print "* immediate operands"
502 print"  testing...     ";
503 .@-
504 .a 6800 lda #"{clr}"
505 .a 6802 ora #%11010110
506 .a *    ldx #$b2+3
507 .a *    ldy #100
508 .= 6800 a99309d6a2b5a064
509 print"{blue}[ok]{black}"
599 rem ** test 5 symbolic assembly
600 print "* symbolic assembly"
601 print "  testing...     ";
602 .@-
604 .@a 02a5 ; test def
605 .* 6800 ; set pc
606 .,; test fwd ref
607 .a *    jsr @&
608 .a *    lda #<@&
609 .a *    ldx #>@&
610 .a *    bcc @2
611 .a * @f lda (<@d,x)
612 .a * @2
613 .a * @&  ;resolve fwd
614 .,; test back
615 .a * @x jmp (@@)
616 .a *    jsr @x
617 .a * @@
618 .a *    bcc @@ 
619 .a * @f iny ;redef
620 .a *    lda @f
621 .a *    eor >@a,x
622 .a * @d "jej"
623 .,; unresolved
624 .a *    jmp @u
625 .,; verify memory
626 .= 6800 200b68a9
627 .= 6804 0ba26890
628 .= 6808 02a1196c
629 .= 680c 1168200b
630 .= 6810 6890fec8
631 .= 6814 ad136855
632 .= 6818 024a454a
633 .= 681c 4c0000
634 print "{blue}[ok]{black}"
699 rem ** test 6 multipass
700 print "* ur% and cp"
705 print"  testing...     ";
710 .@-
712 p = 0
715 ur% = 0 : p = p + 1
717 if p > 2 then print "pass error":stop
720 .* 6800
725 for i = 1 to 24
730 .a * bcc @f
735 next i
737 .a * @f
740 if ur% goto 715
745 if p <> 2 then print "pass error":stop
747 if cp <> 26672 then print "cp error":stop
750 print "{blue}[ok]{black}"
800 rem ** arithmetic and substitution
801 print "* param modifiers"
803 print"  testing...     ";
805 .a 6800 ldx #$50+1
810 .a *    lda ($fa+2,x)
815 .a *    ldy #100-5
820 .a *    cmp #%00001111-f
825 .a *    lda #"z"+1
830 p = 65490 : c = asc("@")
835 .a *    jsr 'p
840 .a *    cpy 'c
845 .a *    brk
850 .= 6800 a251a1fca05fc900
855 .= *    a95b20d2ffc44000
860 print "{blue}[ok]{black}"
1000 print "{green}done!{blue}"


