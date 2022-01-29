10 print"{up}  testing...     ";
100 @1c00 adc #$00
101 @1c02 adc $10
102 @1c04 adc $20,x
103 @1c06 adc $3030
104 @1c09 adc $4040,x
105 @1c0c adc $5050,y
106 @1c0f adc ($60,x)
107 @1c11 adc ($70),y
108 @1c13 and #$80
109 @1c15 and $90
110 @1c17 and $10,x
111 @1c19 and $1111
112 @1c1c and $1212,x
113 @1c1f and $1313,y
114 @1c22 and ($14,x)
115 @1c24 and ($15),y
116 @1c26 asl a
117 @1c27 asl $17
118 @1c29 asl $18,x
119 @1c2b asl $1919
120 @1c2e asl $2020,x
121 @1c31 bcc $1c31
122 @1c33 bcs $1c33
123 @1c35 beq $1c35
124 @1c37 bit $24
125 @1c39 bit $2525
126 @1c3c bmi $1c3c
127 @1c3e bne $1c3e
128 @1c40 bpl $1c40
129 @1c42 brk 
130 @1c43 bvc $1c43
131 @1c45 bvs $1c45
132 @1c47 clc 
133 @1c48 cld 
134 @1c49 cli 
135 @1c4a clv 
136 @1c4b cmp #$36
137 @1c4d cmp $37
138 @1c4f cmp $38,x
139 @1c51 cmp $3939
140 @1c54 cmp $4040,x
141 @1c57 cmp $4141,y
142 @1c5a cmp ($42,x)
143 @1c5c cmp ($43),y
144 @1c5e cpx #$44
145 @1c60 cpx $45
146 @1c62 cpx $4646
147 @1c65 cpy #$47
148 @1c67 cpy $48
149 @1c69 cpy $4949
150 @1c6c dec $50
151 @1c6e dec $51,x
152 @1c70 dec $5252
153 @1c73 dec $5353,x
154 @1c76 dex 
155 @1c77 dey 
156 @1c78 eor #$56
157 @1c7a eor $57
158 @1c7c eor $58,x
159 @1c7e eor $5959
160 @1c81 eor $6060,x
161 @1c84 eor $6161,y
162 @1c87 eor ($62,x)
163 @1c89 eor ($63),y
164 @1c8b inc $64
165 @1c8d inc $65,x
166 @1c8f inc $6666
167 @1c92 inc $6767,x
168 @1c95 inx 
169 @1c96 iny 
170 @1c97 jmp $7070
171 @1c9a jmp ($7171)
172 @1c9d jsr $7272
173 @1ca0 lda #$73
174 @1ca2 lda $74
175 @1ca4 lda $75,x
176 @1ca6 lda $7676
177 @1ca9 lda $7777,x
178 @1cac lda $7878,y
179 @1caf lda ($79,x)
180 @1cb1 lda ($80),y
181 @1cb3 ldx #$81
182 @1cb5 ldx $82
183 @1cb7 ldx $83,y
184 @1cb9 ldx $8484
185 @1cbc ldx $8585,y
186 @1cbf ldy #$86
187 @1cc1 ldy $87
188 @1cc3 ldy $88,x
189 @1cc5 ldy $8989
190 @1cc8 ldy $9090,x
191 @1ccb lsr a
192 @1ccc lsr $92
193 @1cce lsr $93,x
194 @1cd0 lsr $9494
195 @1cd3 lsr $9595,x
196 @1cd6 nop 
197 @1cd7 ora #$97
198 @1cd9 ora $98
199 @1cdb ora $99,x
200 @1cdd ora $1000
201 @1ce0 ora $1010,x
202 @1ce3 ora $2020,y
203 @1ce6 ora ($30,x)
204 @1ce8 ora ($40),y
205 @1cea pha 
206 @1ceb php 
207 @1cec pla 
208 @1ced plp 
209 @1cee rol a
210 @1cef rol $10
211 @1cf1 rol $11,x
212 @1cf3 rol $1212
213 @1cf6 rol $1313,x
214 @1cf9 ror a
215 @1cfa ror $15
216 @1cfc ror $16,x
217 @1cfe ror $1717
218 @1d01 ror $1818,x
219 @1d04 rti 
220 @1d05 rts 
221 @1d06 sbc #$21
222 @1d08 sbc $22
223 @1d0a sbc $23,x
224 @1d0c sbc $2424
225 @1d0f sbc $2525,x
226 @1d12 sbc $2626,y
227 @1d15 sbc ($27,x)
228 @1d17 sbc ($28),y
229 @1d19 sec 
230 @1d1a sed 
231 @1d1b sei 
232 @1d1c sta $32
233 @1d1e sta $33,x
234 @1d20 sta $3434
235 @1d23 sta $3535,x
236 @1d26 sta $3636,y
237 @1d29 sta ($37,x)
238 @1d2b sta ($38),y
239 @1d2d stx $39
240 @1d2f stx $40,y
241 @1d31 stx $4141
242 @1d34 sty $42
243 @1d36 sty $43,x
244 @1d38 sty $4444
245 @1d3b tax 
246 @1d3c tay 
247 @1d3d tsx 
248 @1d3e txa 
249 @1d3f txs 
250 @1d40 tya 
999 print "{blue}[ok]{black}"
1000 print "* verify assembly"
1005 print "  {cyan}loading...{black}"
1010 load "verify.prg",8
