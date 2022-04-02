; SAM (Software Automatic Mouth) by Mark Barton
;
; (c)1982 Don't Ask Software
;
; Disassembled from the C64 version and released on GitHub with the
; author's permission:
;
; http://github.com/DLehenbauer/c64-sam
;
; Reverse engineered comments borrow liberally from the 'C' port by
; Sebastian Macke and refactorings by Vidar Hokstad and Aidan Dodds.

.include "c64.inc"

; Important memory addresses (see pg. 35 of the C64 manual).
.export L9500_install_wedge
.export L97e0_throat
.export L97e1_mouth
.export S97e2_SetMouthThroat
.export L9a03_SAM_ML_entry
.export L9a09_Reciter_ML_entry
.export L9a0e_speed
.export L9a0f_pitch
.export L9a10_lights_enabled
.export L9a11_interrupts_enabled
.export L9a15_input

; SAM uses the following zeropage region for variables.  The range mem19..mem76
; are backed up and restored by 'SaveZeropage' and 'RestoreZeropage'.
mem19 = $13
mem27 = $1b
mem29 = $1d
mem30 = $1e
mem31 = $1f
mem32 = $20
mem33 = $21
mem34 = $22
mem35 = $23
mem36 = $24
mem37 = $25
mem38 = $26
mem39 = $27
mem40 = $28
mem41 = $29
mem42 = $2a
mem43 = $2b
mem44 = $2c
mem45 = $2d
mem46 = $2e
mem46_ptr = $2e ; 16b ptr (originally mem46_ptr/47)
mem47 = $2f
mem48 = $30
mem49 = $31
mem50 = $32
mem51 = $33
mem52 = $34
mem53 = $35
mem54 = $36
mem55 = $37
mem56 = $38
mem57 = $39
mem58 = $3a
mem59 = $3b
mem60 = $3c
mem61 = $3d
mem62 = $3e
mem63 = $3f
mem64_sign2 = $40
mem65_sign1 = $41
mem66_pos = $42

; Note: These Zeropage locations are not backed up / restored, but are
;       typically unused by the C64 ROMS.
mem251 = $fb
mem252 = $fc
mem253 = $fd
mem254 = $fe

; ']LOAD' places 'RECITER' at $7d00 (low memory) and then optionally
; relocates to $C000 (high memory).
.segment "RECITER"

; Alphabetic rule sets are prefixed with "]{CH}" where '{CH}' is
; the uppercase (shifted) letter beginning the match (e.g., "]A")
.macro RULE_SET Arg
    .byte $5d
    .byte Arg
.endmacro

; Rule entries are in the form "{prefix}({match}){suffix}={phonemes}"
; where the high bit of the last character of the emitted phoneme is
; set to mark the end of the rule.
.macro RULE Arg
    .repeat .strlen(Arg) - 1, i
        ; Because of CA65's default PETSCII mapping, we need to "unshift"
        ; alphabetic characters.
        .if (.strat(Arg,i) < 'A')
            .byte .strat(Arg, i)
        .elseif (.strat(Arg,i) <= 'Z')
            .byte .strat(Arg, i) & $7f
        .else
            .byte .strat(Arg, i)
        .endif
    .endrepeat
    ; Mark end of rule by setting MSB of last character
    .byte .strat(Arg, .strlen(Arg) - 1) | $80
.endmacro

.feature string_escapes

; From: https://apps.dtic.mil/sti/pdfs/ADA021929.pdf
;
;   #   One or more vowels (A, E, I, O, U, Y)
;   .   One of B, D, V, G, J, L, M, N, R, W, and Z: a voiced consonant
;   %   One of (ER, E, ES, ED, ING, ELY): a suffix
;   &   One of (S, C, G, Z, X, J, CH, SH): a sibilant
;   @   One of (T, S, R, D, L, Z, N, J, TH, CH, SH): a consonant influencing the sound of a
;       following long u (cf. rule and mule)
;   ^   One consonant (B, C, D, F, G, H, J, K, L, M, N, P, Q, R, S, T, V, W, X, Z)
;   +   One of (E, I, Y): a front vowel
;   :   Zero or more consonants
;
; Note: '*' and '$' from paper are not used/implemented by SAM.

L7d00_a_rules:                  ; ($7d00 is the location where RECITER is loaded)
    RULE_SET "A"
    RULE " (A.)=EH4Y. "
    RULE "(A) =AH"
    RULE " (ARE) =AAR"
    RULE " (AR)O=AXR"
    RULE "(AR)#=EH4R"
    RULE " ^(AS)#=EY4S"
    RULE "(A)WA=AX"
    RULE "(AW)=AO5"
    RULE " :(ANY)=EH4NIY"
    RULE "(A)^+#=EY5"
    RULE "#:(ALLY)=ULIY"
    RULE " (AL)#=UL"
    RULE "(AGAIN)=AXGEH4N"
    RULE "#:(AG)E=IHJ"
    RULE "(A)^%=EY"
    RULE "(A)^+:#=AE"
    RULE " :(A)^+ =EY4"
    RULE " (ARR)=AXR"
    RULE "(ARR)=AE4R"
    RULE " ^(AR) =AA5R"
    RULE "(AR)=AA5R"
    RULE "(AIR)=EH4R"
    RULE "(AI)=EY4"
    RULE "(AY)=EY5"
    RULE "(AU)=AO4"
    RULE "#:(AL) =UL"
    RULE "#:(ALS) =ULZ"
    RULE "(ALK)=AO4K"
    RULE "(AL)^=AOL"
    RULE " :(ABLE)=EY4BUL"
    RULE "(ABLE)=AXBUL"
    RULE "(A)VO=EY4"
    RULE "(ANG)+=EY4NJ"
    RULE "(ATARI)=AHTAA4RIY"
    RULE "(A)TOM=AE"
    RULE "(A)TTI=AE"
    RULE " (AT) =AET"
    RULE " (A)T=AH"
    RULE "(A)=AE"
L7e95_b_rules:
    RULE_SET "B"
    RULE " (B) =BIY4"
    RULE " (BE)^#=BIH"
    RULE "(BEING)=BIY4IHNX"
    RULE " (BOTH) =BOW4TH"
    RULE " (BUS)#=BIH4Z"
    RULE "(BREAK)=BREY5K"
    RULE "(BUIL)=BIH4L"
    RULE "(B)=B"
L7ef7_c_rules:
    RULE_SET "C"
    RULE " (C) =SIY4"
    RULE " (CH)^=K"
    RULE "^E(CH)=K"
    RULE "(CHA)R#=KEH5"
    RULE "(CH)=CH"
    RULE " S(CI)#=SAY4"
    RULE "(CI)A=SH"
    RULE "(CI)O=SH"
    RULE "(CI)EN=SH"
    RULE "(CITY)=SIHTIY"
    RULE "(C)+=S"
    RULE "(CK)=K"
    RULE "(COMMODORE)=KAA4MAHDOHR"
    RULE "(COM)=KAHM"
    RULE "(CUIT)=KIHT"
    RULE "(CREA)=KRIYEY"
    RULE "(C)=K"
L7fa2_d_rules:
    RULE_SET "D"
    RULE " (D) =DIY4"
    RULE " (DR.) =DAA4KTER"
    RULE "#:(DED) =DIHD"
    RULE ".E(D) =D"
    RULE "#:^E(D) =T"
    RULE " (DE)^#=DIH"
    RULE " (DO) =DUW"
    RULE " (DOES)=DAHZ"
    RULE "(DONE) =DAH5N"
    RULE "(DOING)=DUW4IHNX"
    RULE " (DOW)=DAW"
    RULE "#(DU)A=JUW"
    RULE "#(DU)^#=JAX"
    RULE "(D)=D"
; Note: The C64 version originally contained a benign bug where
;       'e_rules' pointed to $8039 instead of $803f.
L803f_e_rules:
    RULE_SET "E"
    RULE " (E) =IYIY4"
    RULE "#:(E) ="
    RULE "':^(E) ="
    RULE " :(E) =IY"
    RULE "#(ED) =D"
    RULE "#:(E)D ="
    RULE "(EV)ER=EH4V"
    RULE "(E)^%=IY4"
    RULE "(ERI)#=IY4RIY"
    RULE "(ERI)=EH4RIH"
    RULE "#:(ER)#=ER"
    RULE "(ERROR)=EH4ROHR"
    RULE "(ERASE)=IHREY5S"
    RULE "(ER)#=EHR"
    RULE "(ER)=ER"
    RULE " (EVEN)=IYVEHN"
    RULE "#:(E)W="
    RULE "@(EW)=UW"
    RULE "(EW)=YUW"
    RULE "(E)O=IY"
    RULE "#:&(ES) =IHZ"
    RULE "#:(E)S ="
    RULE "#:(ELY) =LIY"
    RULE "#:(EMENT)=MEHNT"
    RULE "(EFUL)=FUHL"
    RULE "(EE)=IY4"
    RULE "(EARN)=ER5N"
    RULE " (EAR)^=ER5"
    RULE "(EAD)=EHD"
    RULE "#:(EA) =IYAX"
    RULE "(EA)SU=EH5"
    RULE "(EA)=IY5"
    RULE "(EIGH)=EY4"
    RULE "(EI)=IY4"
    RULE " (EYE)=AY4"
    RULE "(EY)=IY"
    RULE "(EU)=YUW5"
    RULE "(EQUAL)=IY4KWUL"
    RULE "(E)=EH"
L81c5_f_rules:
    RULE_SET "F"
    RULE " (F) =EH4F"
    RULE "(FUL)=FUHL"
    RULE "(FRIEND)=FREH5ND"
    RULE "(FATHER)=FAA4DHER"
    RULE "(F)F="
    RULE "(F)=F"
L8206_g_rules:
    RULE_SET "G"
    RULE " (G) =JIY4"
    RULE "(GIV)=GIH5V"
    RULE " (G)I^=G"
    RULE "(GE)T=GEH5"
    RULE "SU(GGES)=GJEH4S"
    RULE "(GG)=G"
    RULE " B#(G)=G"
    RULE "(G)+=J"
    RULE "(GREAT)=GREY4T"
    RULE "(GON)E=GAO5N"
    RULE "#(GH)="
    RULE " (GN)=N"
    RULE "(G)=G"
L827e_h_rules:
    RULE_SET "H"
    RULE " (H) =EY4CH"
    RULE " (HAV)=/HAE6V"
    RULE " (HERE)=/HIYR"
    RULE " (HOUR)=AW5ER"
    RULE "(HOW)=/HAW"
    RULE "(H)#=/H"
    RULE "(H)="
L82c7_i_rules:
    RULE_SET "I"
    RULE " (IN)=IHN"
    RULE " (I) =AY4"
    RULE "(I) =AY"
    RULE "(IN)D=AY5N"
    RULE "SEM(I)=IY"
    RULE " ANT(I)=AY"
    RULE "(IER)=IYER"
    RULE "#:R(IED) =IYD"
    RULE "(IED) =AY5D"
    RULE "(IEN)=IYEHN"
    RULE "(IE)T=AY4EH"
    RULE "(I')=AY5"
    RULE " :(I)^%=AY5"
    RULE " :(IE) =AY4"
    RULE "(I)%=IY"
    RULE "(IE)=IY4"
    RULE " (IDEA)=AYDIY5AH"
    RULE "(I)^+:#=IH"
    RULE "(IR)#=AYR"
    RULE "(IZ)%=AYZ"
    RULE "(IS)%=AYZ"
    RULE "I^(I)^#=IH"
    RULE "+^(I)^+=AY"
    RULE "#:^(I)^+=IH"
    RULE "(I)^+=AY"
    RULE "(IR)=ER"
    RULE "(IGH)=AY4"
    RULE "(ILD)=AY5LD"
    RULE " (IGN)=IHGN"
    RULE "(IGN) =AY4N"
    RULE "(IGN)^=AY4N"
    RULE "(IGN)%=AY4N"
    RULE "(ICRO)=AY4KROH"
    RULE "(IQUE)=IY4K"
    RULE "(I)=IH"
L8426_j_rules:
    RULE_SET "J"
    RULE " (J) =JEY4"
    RULE "(J)=J"
L8437_k_rules:
    RULE_SET "K"
    RULE " (K) =KEY4"
    RULE " (K)N="
    RULE "(K)=K"
L844e_l_rules:
    RULE_SET "L"
    RULE " (L) =EH4L"
    RULE "(LO)C#=LOW"
    RULE "L(L)="
    RULE "#:^(L)%=UL"
    RULE "(LEAD)=LIYD"
    RULE " (LAUGH)=LAE4F"
    RULE "(L)=L"
L8491_m_rules:
    RULE_SET "M"
    RULE " (M) =EH4M"
    RULE " (MR.) =MIH4STER"
    RULE " (MS.)=MIH5Z"
    RULE " (MRS.) =MIH4SIXZ"
    RULE "(MOV)=MUW4V"
    RULE "(MACHIN)=MAHSHIY5N"
    RULE "M(M)="
    RULE "(M)=M"
L84f1_n_rules:
    RULE_SET "N"
    RULE " (N) =EH4N"
    RULE "E(NG)+=NJ"
    RULE "(NG)R=NXG"
    RULE "(NG)#=NXG"
    RULE "(NGL)%=NXGUL"
    RULE "(NG)=NX"
    RULE "(NK)=NXK"
    RULE " (NOW) =NAW4"
    RULE "N(N)="
    RULE "(NON)E=NAH4N"
    RULE "(N)=N"
L8555_o_rules:
    RULE_SET "O"
    RULE " (O) =OH4W"
    RULE "(OF) =AHV"
    RULE " (OH) =OW5"
    RULE "(OROUGH)=ER4OW"
    RULE "#:(OR) =ER"
    RULE "#:(ORS) =ERZ"
    RULE "(OR)=AOR"
    RULE " (ONE)=WAHN"
    RULE "#(ONE) =WAHN"
    RULE "(OW)=OW"
    RULE " (OVER)=OW5VER"
    RULE "PR(O)V=UW4"
    RULE "(OV)=AH4V"
    RULE "(O)^%=OW5"
    RULE "(O)^EN=OW"
    RULE "(O)^I#=OW5"
    RULE "(OL)D=OW4L"
    RULE "(OUGHT)=AO5T"
    RULE "(OUGH)=AH5F"
    RULE " (OU)=AW"
    RULE "H(OU)S#=AW4"
    RULE "(OUS)=AXS"
    RULE "(OUR)=OHR"
    RULE "(OULD)=UH5D"
    RULE "(OU)^L=AH5"
    RULE "(OUP)=UW5P"
    RULE "(OU)=AW"
    RULE "(OY)=OY"
    RULE "(OING)=OW4IHNX"
    RULE "(OI)=OY5"
    RULE "(OOR)=OH5R"
    RULE "(OOK)=UH5K"
    RULE "F(OOD)=UW5D"
    RULE "L(OOD)=AH5D"
    RULE "M(OOD)=UW5D"
    RULE "(OOD)=UH5D"
    RULE "F(OOT)=UH5T"
    RULE "(OO)=UW5"
    RULE "(O')=OH"
    RULE "(O)E=OW"
    RULE "(O) =OW"
    RULE "(OA)=OW4"
    RULE " (ONLY)=OW4NLIY"
    RULE " (ONCE)=WAH4NS"
    RULE "(ON'T)=OW4NT"
    RULE "C(O)N=AA"
    RULE "(O)NG=AO"
    RULE " :^(O)N=AH"
    RULE "I(ON)=UN"
    RULE "#:(ON)=UN"
    RULE "#^(ON)=UN"
    RULE "(O)ST=OW"
    RULE "(OF)^=AO4F"
    RULE "(OTHER)=AH5DHER"
    RULE "R(O)B=RAA"
    RULE "^R(O):#=OW5"
    RULE "(OSS) =AO5S"
    RULE "#:^(OM)=AHM"
    RULE "(O)=AA"
L87a1_p_rules:
    RULE_SET "P"
    RULE " (P) =PIY4"
    RULE "(PH)=F"
    RULE "(PEOPL)=PIY5PUL"
    RULE "(POW)=PAW4"
    RULE "(PUT) =PUHT"
    RULE "(P)P="
    RULE "(P)S="
    RULE "(P)N="
    RULE "(PROF.)=PROHFEH4SER"
    RULE "(P)=P"
L87fe_q_rules:
    RULE_SET "Q"
    RULE " (Q) =KYUW4"
    RULE "(QUAR)=KWOH5R"
    RULE "(QU)=KW"
    RULE "(Q)=K"
L8824_r_rules:
    RULE_SET "R"
    RULE " (R) =AA5R"
    RULE " (RE)^#=RIY"
    RULE "(R)R="
    RULE "(R)=R"
L8845_s_rules:
    RULE_SET "S"
    RULE " (S) =EH4S"
    RULE "(SH)=SH"
    RULE "#(SION)=ZHUN"
    RULE "(SOME)=SAHM"
    RULE "#(SUR)#=ZHER"
    RULE "(SUR)#=SHER"
    RULE "#(SU)#=ZHUW"
    RULE "#(SSU)#=SHUW"
    RULE "#(SED)=ZD"
    RULE "#(S)#=Z"
    RULE "(SAID)=SEHD"
    RULE "^(SION)=SHUN"
    RULE "(S)S="
    RULE ".(S) =Z"
    RULE "#:.E(S) =Z"
    RULE "#:^#(S) =S"
    RULE "U(S) =S"
    RULE " :#(S) =Z"
    RULE "##(S) =Z"
    RULE " (SCH)=SK"
    RULE "(S)C+="
    RULE "#(SM)=ZUM"
    RULE "#(SN)'=ZUM"
    RULE "(STLE)=SUL"
    RULE "(S)=S"
L892d_t_rules:
    RULE_SET "T"
    RULE " (T) =TIY4"
    RULE " (THE) #=DHIY"
    RULE " (THE) =DHAX"
    RULE "(TO) =TUX"
    RULE " (THAT)=DHAET"
    RULE " (THIS) =DHIHS"
    RULE " (THEY)=DHEY"
    RULE " (THERE)=DHEHR"
    RULE "(THER)=DHER"
    RULE "(THEIR)=DHEHR"
    RULE " (THAN) =DHAEN"
    RULE " (THEM) =DHAEN"
    RULE "(THESE) =DHIYZ"
    RULE " (THEN)=DHEHN"
    RULE "(THROUGH)=THRUW4"
    RULE "(THOSE)=DHOHZ"
    RULE "(THOUGH) =DHOW"
    RULE "(TODAY)=TUXDEY"
    RULE "(TOMO)RROW=TUMAA5"
    RULE "(TO)TAL=TOW5"
    RULE " (THUS)=DHAH4S"
    RULE "(TH)=TH"
    RULE "#:(TED)=TIXD"
    RULE "S(TI)#N=CH"
    RULE "(TI)O=SH"
    RULE "(TI)A=SH"
    RULE "(TIEN)=SHUN"
    RULE "(TUR)#=CHER"
    RULE "(TU)A=CHUW"
    RULE " (TWO)=TUW"
    RULE "&(T)EN ="
    RULE "(T)=T"
L8aa7_u_rules:
    RULE_SET "U"
    RULE " (U) =YUW4"
    RULE " (UN)I=YUWN"
    RULE " (UN)=AHN"
    RULE " (UPON)=AXPAON"
    RULE "@(UR)#=UH4R"
    RULE "(UR)#=YUH4R"
    RULE "(UR)=ER"
    RULE "(U)^ =AH"
    RULE "(U)^^=AH5"
    RULE "(UY)=AY5"
    RULE " G(U)#="
    RULE "G(U)%="
    RULE "G(U)#=W"
    RULE "#N(U)=YUW"
    RULE "@(U)=UW"
    RULE "(U)=YUW"
L8b36_v_rules:
    RULE_SET "V"
    RULE " (V) =VIY4"
    RULE "(VIEW)=VYUW5"
    RULE "(V)=V"
L8b53_w_rules:
    RULE_SET "W"
    RULE " (W) =DAH4BULYUW"
    RULE " (WERE)=WER"
    RULE "(WA)SH=WAA"
    RULE "(WA)ST=WEY"
    RULE "(WA)S=WAH"
    RULE "(WA)T=WAA"
    RULE "(WHERE)=WHEHR"
    RULE "(WHAT)=WHAHT"
    RULE "(WHOL)=/HOWL"
    RULE "(WHO)=/HUW"
    RULE "(WH)=WH"
    RULE "(WAR)#=WEHR"
    RULE "(WAR)=WAOR"
    RULE "(WOR)^=WER"
    RULE "(WR)=R"
    RULE "(WOM)A=WUHM"
    RULE "(WOM)E=WIHM"
    RULE "(WEA)R=WEH"
    RULE "(WANT)=WAA5NT"
    RULE "ANS(WER)=ER"
    RULE "(W)=W"
L8c2e_x_rules:
    RULE_SET "X"
    RULE " (X) =EH4KR"
    RULE " (X)=Z"
    RULE "(X)=KS"
L8c47_y_rules:
    RULE_SET "Y"
    RULE " (Y) =WAY4"
    RULE "(YOUNG)=YAHNX"
    RULE " (YOUR)=YOHR"
    RULE " (YOU)=YUW"
    RULE " (YES)=YEHS"
    RULE " (Y)=Y"
    RULE "F(Y)=AY"
    RULE "PS(YCH)=AYK"
    RULE "#:^(Y)=IY"
    RULE "#:^(Y)I=IY"
    RULE " :(Y) =AY"
    RULE " :(Y)#=AY"
    RULE " :(Y)^+:#=IH"
    RULE " :(Y)^#=AY"
    RULE "(Y)=IH"
L8cda_z_rules:
    RULE_SET "Z"
    RULE " (Z) =ZIY4"
    RULE "(Z)=Z"

Padding0: .res $15 ; TODO: Remove?

L8d00_inputtemp: .res $100

Padding1: .res $18 ; TODO: Remove?

; TODO: Document - Appears to be flags used to help classify characters for reciter
L8e18_tab36376:
    .byte $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $02, $02, $02, $02, $02, $02, $82
    .byte $00, $00, $02, $02, $02, $02, $02, $02, $03, $03, $03, $03, $03, $03, $03, $03
    .byte $03, $03, $02, $02, $02, $02, $02, $02, $02, $c0, $a8, $b0, $ac, $c0, $a0, $b8
    .byte $a0, $c0, $bc, $a0, $ac, $a8, $ac, $c0, $a0, $a0, $ac, $b4, $a4, $c0, $a8, $a8
    .byte $b0, $c0, $bc, $00, $00, $00, $02, $00

L8e78_Reciter_from_BASIC:
    jsr S9b20_say_init
    jsr Sb9c0_SaveZeropage
    jsr Sa3cd
    jmp L8e8a

L8e84_Reciter_from_ML:
    jsr S9b20_say_init
    jsr Sb9c0_SaveZeropage
L8e8a:
    ; Prepend a space (' ') at the beginning of 'inputtemp[]'.  Spaces indicate
    ; a word break.  This prevents the rule matching from walking backwards
    ; past the beginning of 'inputtemp[]'.
    lda #$20
    sta L8d00_inputtemp                 ; L8d00_inputtemp[0] = $20

    ldx #$01                            ; X = 1
    ldy #$00                            ; Y = 0
L8e93:                                  ; do {
    lda L9a15_input,y                   ;   A = L9a15_input[Y]

    ; In PETSCII, the $80 flag indicates the character is shifted.
    and #$7f                            ;   A = A & $7f

    cmp #$70
    bcc L8ea1                           ;   if (A > $70) {
    and #$5f                            ;     A &= $5f
    jmp L8ea7                           ;   }
L8ea1:                                  ;   else {
    cmp #$60
    bcc L8ea7                           ;     if (A > $60)
    and #$4f                            ;       A &= $4f
L8ea7:                                  ;   }
    sta L8d00_inputtemp,x               ;   inputtemp[X] = A
    inx                                 ;   X++
    iny                                 ;   Y++
    cpy #$ff
    bne L8e93                           ; } while (Y != $FF)

    ; Place an '{ESC}' at the end of the input buffer.  This ensures that rule
    ; matching will not advance past the end of 'inputtemp[]'.
    ; TODO: Replace 'X' with 'Y', which is already $ff from the loop above?
    ldx #$ff
    lda #$1b
    sta L8d00_inputtemp,x               ; inputtemp[$ff] = '{ESC}'
    jmp L8ec2

; TODO: Inline or combine with below?
S8eba:
    jsr S9b61_say_main
    rts

; TODO: Inline or combine with above?
L8ebe:
    jsr S9b61_say_main
    rts

L8ec2:
    lda #$ff
    sta mem61                           ; mem61 = $ff
L8ec6:                                  ; loop {
    lda #$ff
    sta mem56                           ;   mem56 = $ff
L8eca_L_start_parse:                    ;   loop {
    inc mem61                           ;     mem61++
    ldx mem61                           ;     X = mem61
    lda L8d00_inputtemp,x               ;     A = inputtemp[X]
    sta mem64_sign2                     ;     mem64_sign2 = A

    ; If the next character is '{ESC}' say the phonemes and return.
    cmp #$1b
    bne L8ee3                           ;     if (A == {ESC}) {
    inc mem56                           ;       mem56++
    ldx mem56                           ;       X = mem56
    lda #$9b
    sta L9a15_input,x                   ;       input[X] = $9b
    jmp L8ebe                           ;       return S9b61_say_main()
L8ee3:                                  ;     }
    cmp #$2e
    bne L8eff                           ;     if (A != '.') break
    inx
    lda L8d00_inputtemp,x
    tay
    lda L8e18_tab36376,y                ;     A = tab36376[inputtemp[X + 1]]
    and #$01
    bne L8eff                           ;     if (A & 1) break
    inc mem56
    ldx mem56                           ;     X = mem56++
    lda #$2e
    sta L9a15_input,x                   ;     input[X] = $2e
    jmp L8eca_L_start_parse             ;   }
L8eff:
    lda mem64_sign2
    tay
    lda L8e18_tab36376,y
    sta mem57                           ;   mem57 = tab36376[mem64_sign2]

    ; If non-alphabetic character, use 'non_alpha_rules'
    ; TODO: mem62/63 are a pointer to the current rule
    and #$02
    beq L8f16                           ;   if (A & 2) {
    lda #<L92a5_non_alpha_rules ; (originally '#$a5')
    sta mem62
    lda #>L92a5_non_alpha_rules ; (originally '#$92')
    sta mem63                           ;     ptr = &L92a5_non_alpha_rules
    jmp L8f5c_L_nextrule                ;     goto L8f5c_L_nextrule     (run rules for non-alpha character)
L8f16:                                  ;   }
    lda mem57
    bne L8f45                           ;   if (mem57 != 0) break       (run rules for alpha character)
    lda #$20
    sta L8d00_inputtemp,x               ;   inputtemp[X] = ' '
    inc mem56
    ldx mem56                           ;   X = ++mem56
    cpx #$78
    bcs L8f2e                           ;   if (X <= 120) {
    sta L9a15_input,x                   ;     input[X] = ' '
    jmp L8eca_L_start_parse             ;     goto L_start_parse (~continue..  doesn't set mem56)

L8f2d_saved_pos: .res $1

L8f2e:                                  ;   }
    lda #$9b
    sta L9a15_input,x                   ;   input[X] = $9b

    ; TODO: 'mem61' doesn't appear to be overwritten, remove stashing?
    lda mem61
    sta L8f2d_saved_pos                 ;   L8f2d_saved_pos = mem61

    sta mem29                           ;   mem29 = mem61
    jsr S8eba                           ;   S9b61_say_main()

    ; Restore mem61 after jsr
    lda L8f2d_saved_pos
    sta mem61                           ;   mem61 = L8f2d_saved_pos
    jmp L8ec6                           ; }

    ; Run rules for alphabetic character
L8f45:
    lda mem57
    and #$80
    bne L8f4c
    brk                                 ; assert (mem57 & $80)

    ; Go to the right rules for this alphabetic character.
L8f4c:
    lda mem64_sign2
    sec
    sbc #$41
    tax                                 ; X = mem64_sign2 - 'A';
    lda L9271_alpha_rules_lo,x
    sta mem62
    lda L928b_alpha_rules_hi,x
    sta mem63                           ; ptr = alpha_rules[x]

    ; Advance 'ptr' until we find the beginning of the next rule.
L8f5c_L_nextrule:
    ldy #$00                            ; Y = 0
L8f5e:                                  ; do {
    clc
    lda mem62
    adc #$01
    sta mem62
    lda mem63
    adc #$00
    sta mem63                           ;  ptr++
    lda (mem62),y
    bpl L8f5e                           ; } while ((*ptr & $80) == 0)
    iny                                 ; Y++

    ; Advance 'Y' until we find '('
L8f70:                                  ; loop {
    lda (mem62),y
    cmp #$28
    beq L8f7a                           ;   if (ptr[Y] == '(') break
    iny                                 ;   Y++
    jmp L8f70                           ; }
L8f7a:
    sty mem66_pos                       ; mem66_pos = Y     (remember position of '(')

    ; Advance 'Y' until we find ')'
L8f7c:                                  ; do {
    iny                                 ;   Y++
    lda (mem62),y
    cmp #$29
    bne L8f7c                           ; } while (ptr[Y] == ')')
    sty mem65_sign1                     ; mem65_sign1 = Y     (remember position of ')')

    ; Advance 'Y' until we find '='
L8f85:                                  ; {
    iny                                 ;   Y++
    lda (mem62),y                       ;   A = ptr[Y]
    and #$7f
    cmp #$3d
    bne L8f85                           ; } while((ptr[Y] & $7f) != '=')
    sty mem64_sign2                     ; mem64_sign2 = Y     (remember position of '=')
    ldx mem61

    ; Move Y back to '('
    stx mem60                           ; mem60 = mem61
    ldy mem66_pos                       ; Y = mem66_pos

    ; Compare the string within the parentheses
    iny                                 ; Y++
L8f97:                                  ; do {
    lda L8d00_inputtemp,x
    sta mem57                           ;   mem57 = inputtemp[X]
    lda (mem62),y                       ;   A = ptr[Y]
    cmp mem57
    beq L8fa5                           ;   if (A != mem57) {
    jmp L8f5c_L_nextrule                ;     skip to next rule
L8fa5:                                  ;   }

    iny                                 ;   Y++

    ; Break out of the loop when we get to the ')'
    cpy mem65_sign1
    bne L8fad                           ;   if (Y == mem65_sign1)
    jmp L8fb3                           ;     break

L8fad:
    inx                                 ;   X++
    stx mem60                           ;   mem60 = X
    jmp L8f97                           ; }

    ; String in the parentheses is correct
L8fb3:
    lda mem61
    sta mem59                           ; mem59 = mem60
L8fb7_L_match_left:                     ; loop {
    ldy mem66_pos
    dey
    sty mem66_pos                       ;   mem66_pos--
    lda (mem62),y
    sta mem57                           ;   mem57 = ptr[mem66_pos]

    ; If end of LHS pattern
    bpl L8fc5                           ;   if (mem57 & $80) {
    jmp L913c                           ;     goto L913c
L8fc5:                                  ;   }
    and #$7f
    tax                                 ;   X = mem57 & $7f
    lda L8e18_tab36376,x
    and #$80
    beq L8fe1                           ;   if (!(tab36376[X] & $80)) break
    ldx mem59
    dex                                 ;   X = mem59 - 1
    lda L8d00_inputtemp,x
    cmp mem57
    beq L8fdc                           ;   if (inputtemp[mem59 - 1] == mem57) {
    jmp L8f5c_L_nextrule                ;     goto L8f5c_L_nextrule
L8fdc:                                  ;   }
    stx mem59                           ;   X = mem59   (mem59 - 1)
    jmp L8fb7_L_match_left              ; }

L8fe1:
    lda mem57
    cmp #$20
    bne L8fea                           ; if (mem57 == ' ') {
    jmp L901f                           ;   goto L901f          ; Match word break
L8fea:                                  ; }
    cmp #$23
    bne L8ff1                           ; if (mem57 == '#') {
    jmp L902e                           ;   goto L902e          ; Match one or more vowels
L8ff1:                                  ; }
    cmp #$2e
    bne L8ff8                           ; if (mem57 == '.') {
    jmp L9038                           ;   goto L9038          ; Match one voiced consonant
L8ff8:                                  ; }
    cmp #$26
    bne L8fff                           ; if (mem57 == '&') {
    jmp L9047                           ;   goto L9047          ; Match one sibilant
L8fff:                                  ; }
    cmp #$40
    bne L9006                           ; if (mem57 == '@') {   ; Match one consonant influencing long u
    jmp L9067                           ;   goto L9067
L9006:                                  ; }
    cmp #$5e
    bne L900d                           ; if (mem57 == '^') {   ; Match one consonant
    jmp L908c                           ;   goto L908c
L900d:                                  ; }
    cmp #$2b
    bne L9014                           ; if (mem57 == '+') {   ; Match one front vowel
    jmp L909b                           ;   goto L909b
L9014:                                  ; }
    cmp #$3a
    bne L901b                           ; if (mem57 == ':') {   ; Match zero or more consonants
    jmp L90b0                           ;   goto L90b0
L901b:                                  ; }
    ; Note: Originally the below 'jsr' would jump to $a439, which is mid-instruction.
    ;       (See commented-out Sa439 for more details.)
    jsr Sa43b_ErrorBeep ; (originally 'jsr Sa439')
    brk                                 ; FAIL (?)

; ' ': Match preceeding word break
L901f:
    jsr S90bf                           ; X = mem59 - 1, A = tab36376[tab36096[X]]
    and #$80
    beq L9029                           ; if (!(A & $80)) {
    jmp L8f5c_L_nextrule                ;   goto L8f5c_L_nextrule (??)
L9029:                                  ; }
    stx mem59                           ; mem59--
    jmp L8fb7_L_match_left              ; goto L8fb7_L_match_left (??)

; '#': Match one or more preceeding vowels
L902e:
    jsr S90bf                           ; X = mem59 - 1, A = tab36376[tab36096[X]]
    and #$40                            ; if (A & $40)
    bne L9029                           ;     goto L9029
    jmp L8f5c_L_nextrule                ; else goto L8f5c_L_nextrule

; '.': Match one preceeding voiced consonant
L9038:
    jsr S90bf                           ; X = mem59 - 1, A = tab36376[tab36096[X]]
    and #$08                            ; if (A & $08)
    bne L9042                           ;     goto L9042
    jmp L8f5c_L_nextrule                ; else goto L8f5c_L_nextrule

L9042:
    stx mem59                           ; mem59--
    jmp L8fb7_L_match_left              ; goto L8fb7_L_match_left

; '&': Match one preceeding sibilant
L9047:
    jsr S90bf                           ; X = mem59 - 1, A = tab36376[tab36096[X]]
    and #$10                            ; if (A & $10)
    bne L9042                           ;   goto L9042
    lda L8d00_inputtemp,x
    cmp #$48
    beq L9058                           ; if (inputtemp[mem59 - 1] != 'H') {
    jmp L8f5c_L_nextrule                ;   goto L8f5c_L_nextrule
L9058:                                  ; }
    dex                                 ; X--
    lda L8d00_inputtemp,x
    cmp #$43
    beq L9042                           ; if (inputtemp[mem59 - 2]) == 'C') goto L9042
    cmp #$53
    beq L9042                           ; if (inputtemp[mem59 - 2]) == 'S') goto L9042
    jmp L8f5c_L_nextrule                ; else goto L8f5c_L_nextrule

; '@': Match one preceeding consonant influencing the sound of long u
L9067:
    jsr S90bf                           ; X = mem59 - 1, A = tab36376[tab36096[X]]
    and #$04
    bne L9042                           ; if (A & $04) goto L9042
    lda L8d00_inputtemp,x
    cmp #$48
    beq L9078                           ; if (inputtemp[mem59 - 1] != 'H') {
    jmp L8f5c_L_nextrule                ;   goto L8f5c_L_nextrule
L9078:                                  ; }
    cmp #$54
    beq L9087                           ; if (inputtemp[mem59 - 1] == 'T') goto L9087
    cmp #$43
    beq L9087                           ; if (inputtemp[mem59 - 1] == 'C') goto L9087
    cmp #$53
    beq L9087                           ; if (inputtemp[mem59 - 1] == 'S') goto L9087
    jmp L8f5c_L_nextrule                ; goto L8f5c_L_nextrule

L9087:
    stx mem59                           ; mem59 = X
    jmp L8fb7_L_match_left              ; goto L8fb7_L_match_left

; '^': Match one consonant
L908c:
    jsr S90bf                           ; X = mem59 - 1, A = tab36376[tab36096[X]]
    and #$20
    bne L9096                           ; if (A & $20) {
    jmp L8f5c_L_nextrule                ;   goto L_nextrule
L9096:                                  ; }
    stx mem59                           ; mem59--
    jmp L8fb7_L_match_left              ; goto L_match_left

; '+': Match one front vowel
L909b:
    ldx mem59
    dex
    lda L8d00_inputtemp,x               ; A = inputtemp[mem59 - 1]
    cmp #$45
    beq L9096                           ; if (A == 'E') goto L9096
    cmp #$49
    beq L9096                           ; if (A == 'I') goto L9096
    cmp #$59
    beq L9096                           ; if (A == 'Y') goto L9096
    jmp L8f5c_L_nextrule                ; goto L_nextrule

; ':': Match zero or more consonants
L90b0:                                  ; loop {
    jsr S90bf                           ;   X = mem59 - 1, A = tab36376[tab36096[X]]
    and #$20
    bne L90ba                           ;   if (A & $20) {
    jmp L8fb7_L_match_left              ;     goto L_match_left
L90ba:                                  ;   }
    stx mem59                           ;   mem59--
    jmp L90b0                           ; }

; X = mem59 - 1, A = tab36376[tab36096[mem59 - 1]]
S90bf:
    ldx mem59                           ; X = mem59
    dex                                 ; X--
    lda L8d00_inputtemp,x               ; A = tab36096[X]
    tay                                 ; Y = A
    lda L8e18_tab36376,y                ; A = tab36376[Y]
    rts                                 ; return

; X = mem58 + 1, A = tab36376[tab36096[mem59 + 1]]
S90ca:
    ldx mem58                           ; X = mem58
    inx                                 ; X++
    lda L8d00_inputtemp,x               ; A = tab36096[X];
    tay                                 ; Y = A
    lda L8e18_tab36376,y                ; A = tab36376[Y];
    rts

; '%': Match one suffix
L90d5:
    ldx mem58
    inx                                 ; X = mem58 + 1
    lda L8d00_inputtemp,x               ; A = inputtemp[X]
    cmp #$45
    bne L9125_L_match_ing               ; if (inputtemp[X + 1] != 'E') goto L_match_ing
    inx
    lda L8d00_inputtemp,x
    tay                                 ; Y = inputtemp[X + 1]
    dex
    lda L8e18_tab36376,y                ; A = tab36376[Y]
    and #$80
    beq L90f4                           ; if (A & $80) {
    inx                                 ;   X++
    lda L8d00_inputtemp,x               ;   A = inputtemp[X]
    cmp #$52                            ;   if (A != 'R')
    bne L90f9                           ;     goto L90f9
L90f4:                                  ; }
    stx mem58                           ; mem58 = X
    jmp L9140_L_match_right             ; goto L_match_right

L90f9:
    cmp #$53
    beq L90f4                           ; if (A == 'S') goto L90f4
    cmp #$44
    beq L90f4                           ; if (A == 'D') goto L90f4
    cmp #$4c
    bne L910f                           ; if (A != 'L') goto L910f
    inx                                 ; X++
    lda L8d00_inputtemp,x               ; A = inputtemp[X]
    cmp #$59
    bne L9139                           ; if (A != 'Y') goto L_nextrule
    beq L90f4                           ; goto L90f4        (near jump)
L910f:
    ; match FUV
    cmp #$46
    bne L9139                           ; if (A != 'F') goto L_nextrule
    inx                                 ; X++
    lda L8d00_inputtemp,x               ; A = inputtemp[X]
    cmp #$55
    bne L9139                           ; if (A != 'U') goto L_nextrule
    inx                                 ; X++
    lda L8d00_inputtemp,x               ; A = inputtemp[X]
    cmp #$4c
    beq L90f4                           ; if (A == 'V') goto L90f4
    bne L9139                           ; goto L_nextrule
L9125_L_match_ing:
    cmp #$49
    bne L9139                           ; if (A != 'I') goto L9139
    inx                                 ; X++
    lda L8d00_inputtemp,x               ; A = inputtemp[X]
    cmp #$4e
    bne L9139                           ; if (A != 'N') goto L9139
    inx                                 ; X++
    lda L8d00_inputtemp,x               ; A = inputtemp[X]
    cmp #$47
    beq L90f4                           ; if (A == 'G') goto L90f4
L9139:
    jmp L8f5c_L_nextrule

L913c:
    lda mem60
    sta mem58                           ; mem58 = mem60
L9140_L_match_right:                    ; loop {
    ldy mem65_sign1                     ;   Y = mem65_sign1
    iny                                 ;   Y++
    cpy mem64_sign2
    bne L914a                           ;   if (Y == mem64_sign2) {
    jmp L924f_L_emitrule                ;     goto L_emitrule
L914a:                                  ;   }
    sty mem65_sign1                     ;   mem65_sign1++
    lda (mem62),y
    sta mem57
    tax                                 ;   X = mem57 = mem62[Y]
    lda L8e18_tab36376,x                ;   A = tab36376[X]
    and #$80
    beq L916a                           ;   if (!(A & $80)) goto L916a
    ldx mem58                           ;   X = mem58
    inx                                 ;   X++
    lda L8d00_inputtemp,x               ;   A = inputtemp[X]
    cmp mem57
    beq L9165                           ;   if (A != mem57) {
    jmp L8f5c_L_nextrule                ;     goto nextrule
L9165:                                  ;   }
    stx mem58                           ;   mem58++
    jmp L9140_L_match_right             ; }

L916a:
    lda mem57                           ; A = mem57
    cmp #$20
    bne L9173                           ; if (A == ' ') {   ; Match word break
    jmp L91af                           ;   goto L91af
L9173:                                  ; }
    cmp #$23
    bne L917a                           ; if (A == '#') {   ; Match one or more vowels
    jmp L91be                           ;   goto L91be
L917a:                                  ; }
    cmp #$2e
    bne L9181                           ; if (A == '.') {   ; Match one voiced consonant
    jmp L91c8                           ;   goto L91c8
L9181:                                  ; }
    cmp #$26
    bne L9188                           ; if (A == '&') {   ; Match one sibilant
    jmp L91d7                           ;   goto L91d7
L9188:                                  ; }
    cmp #$40
    bne L918f                           ; if (A == '@') {   ; Match one consonant influencing long u
    jmp L91f7                           ;   goto L91f7
L918f:                                  ; }
    cmp #$5e
    bne L9196                           ; if (A == '^') {   ; Match one consonant
    jmp L921c                           ;   goto L921c
L9196:                                  ; }
    cmp #$2b
    bne L919d                           ; if (A == '+') {   ; Match a front vowel
    jmp L922b                           ;   goto L922b
L919d:                                  ; }
    cmp #$3a
    bne L91a4                           ; if (A == ':') {   ; Match zero or more consonants
    jmp L9240                           ;   goto L9240
L91a4:                                  ; }
    cmp #$25
    bne L91ab                           ; if (A == '%') {   ; Match a suffix
    jmp L90d5                           ;   goto L90d5
L91ab:                                  ; }
    ; Note: Originally the below 'jsr' would jump to $a439, which is mid-instruction.
    ;       (See commented-out Sa439 for more details.)
    jsr Sa43b_ErrorBeep ; (originally 'jsr Sa439')
    brk                                 ; FAIL (?)

; ' ': Match one following word break
L91af:
    jsr S90ca                           ; X = mem58 + 1, A = tab36376[tab36096[mem59 + 1]]
    and #$80
    beq L91b9                           ; if (!(A & $80)) {
    jmp L8f5c_L_nextrule                ;   goto nextrule
L91b9:                                  ; }
    stx mem58                           ; mem58++
    jmp L9140_L_match_right             ; goto L_match_right

; '#': Match one or more following vowels
L91be:
    jsr S90ca                           ; X = mem58 + 1, A = tab36376[tab36096[mem59 + 1]]
    and #$40
    bne L91b9                           ; if (A & $80) goto L91b9
    jmp L8f5c_L_nextrule                ; else goto nextrule

; '.': Match one following voiced consonant
L91c8:
    jsr S90ca                           ; X = mem58 + 1, A = tab36376[tab36096[mem59 + 1]]
    and #$08
    bne L91d2                           ; if (A & $08) {
    jmp L8f5c_L_nextrule                ;   goto nextrule
L91d2:                                  ; }
    stx mem58                           ; mem58++
    jmp L9140_L_match_right             ; goto L_match_right

; '&': Match one following sibilant
L91d7:
    jsr S90ca                           ; X = mem58 + 1, A = tab36376[tab36096[mem59 + 1]]
    and #$10
    bne L91d2                           ; if (A & 10) goto L91d2
    lda L8d00_inputtemp,x
    cmp #$48
    beq L91e8                           ; if (inputtemp[X] != 'H') {
    jmp L8f5c_L_nextrule                ;   goto L_nextrule
L91e8:                                  ; }
    inx                                 ; X++
    lda L8d00_inputtemp,x
    cmp #$43
    beq L91d2                           ; if (inputtemp[X] == 'C') goto L91d2
    cmp #$53
    beq L91d2                           ; if (inputtemp[X] == 'S') goto L91d2
    jmp L8f5c_L_nextrule                ; goto L_nextrule

; '@': Match one following consonant influencing the sound of long u
L91f7:
    jsr S90ca                           ; X = mem58 + 1, A = tab36376[tab36096[mem59 + 1]]
    and #$04
    bne L91d2                           ; if (A & $04) goto L91d2
    lda L8d00_inputtemp,x
    cmp #$48
    beq L9208                           ; if (inputtype[X] == 'H') {
    jmp L8f5c_L_nextrule                ;   goto nextrule
L9208:                                  ; }
    cmp #$54
    beq L9217                           ; if (inputtype[X] == 'T') goto L9217
    cmp #$43
    beq L9217                           ; if (inputtype[X] == 'C') goto L9217
    cmp #$53
    beq L9217                           ; if (inputtype[X] == 'S') goto L9217
    jmp L8f5c_L_nextrule                ; goto L_nextrule
L9217:
    stx mem58                           ; mem58++
    jmp L9140_L_match_right             ; goto L_match_right

; '^': Match one following consonant
L921c:
    jsr S90ca                           ; X = mem58 + 1, A = tab36376[tab36096[mem59 + 1]]
    and #$20
    bne L9226                           ; if (!(A & $20)) {
    jmp L8f5c_L_nextrule                ;   goto L_nextrule
L9226:                                  ; }
    stx mem58                           ; mem58++
    jmp L9140_L_match_right             ; goto L_match_right

; '+': Match one following front vowel
L922b:
    ldx mem58
    inx                                 ; X = mem58 + 1
    lda L8d00_inputtemp,x               ; A = inputtemp[X]
    cmp #$45
    beq L9226                           ; if (A == 'E') goto L9226
    cmp #$49
    beq L9226                           ; if (A == 'I') goto L9226
    cmp #$59
    beq L9226                           ; if (A == 'Y') goto L9226
    jmp L8f5c_L_nextrule                ; goto L_nextrule

; ':': Match zero or more consonants
L9240:                                  ; loop {
    jsr S90ca                           ;   X = mem58 + 1, A = tab36376[tab36096[mem59 + 1]]
    and #$20
    bne L924a                           ;   if (!(A & $20)) {
    jmp L9140_L_match_right             ;     goto L_match_right
L924a:                                  ;   }
    stx mem58                           ;   mem58++
    jmp L9240                           ; }

L924f_L_emitrule:
    ldy mem64_sign2                     ; Y = mem64_sign2
    lda mem60
    sta mem61                           ; mem61 = mem60
L9255:                                  ; loop {
    lda (mem62),y
    sta mem57                           ;   mem57 = A = mem62[Y]
    and #$7f
    cmp #$3d
    beq L9266                           ;   if ((A & $7f) != $3d) {
    inc mem56
    ldx mem56
    sta L9a15_input,x                   ;     input[++mem56] = A
L9266:                                  ;   }
    bit mem57
    bpl L926d                           ;   if (mem57 & $80) {
    jmp L8eca_L_start_parse             ;     goto L_start_parse
L926d:                                  ;   }
    iny                                 ;   Y++
    jmp L9255                           ; }

; Pointer table used to index into alphabetic rules
L9271_alpha_rules_lo:   ; (Low byte)
    .byte <L7d00_a_rules, <L7e95_b_rules, <L7ef7_c_rules, <L7fa2_d_rules
    .byte <L803f_e_rules, <L81c5_f_rules, <L8206_g_rules, <L827e_h_rules
    .byte <L82c7_i_rules, <L8426_j_rules, <L8437_k_rules, <L844e_l_rules
    .byte <L8491_m_rules, <L84f1_n_rules, <L8555_o_rules, <L87a1_p_rules
    .byte <L87fe_q_rules, <L8824_r_rules, <L8845_s_rules, <L892d_t_rules
    .byte <L8aa7_u_rules, <L8b36_v_rules, <L8b53_w_rules, <L8c2e_x_rules
    .byte <L8c47_y_rules, <L8cda_z_rules
L928b_alpha_rules_hi:   ; (High byte)
    .byte >L7d00_a_rules, >L7e95_b_rules, >L7ef7_c_rules, >L7fa2_d_rules
    .byte >L803f_e_rules, >L81c5_f_rules, >L8206_g_rules, >L827e_h_rules
    .byte >L82c7_i_rules, >L8426_j_rules, >L8437_k_rules, >L844e_l_rules
    .byte >L8491_m_rules, >L84f1_n_rules, >L8555_o_rules, >L87a1_p_rules
    .byte >L87fe_q_rules, >L8824_r_rules, >L8845_s_rules, >L892d_t_rules
    .byte >L8aa7_u_rules, >L8b36_v_rules, >L8b53_w_rules, >L8c2e_x_rules
    .byte >L8c47_y_rules, >L8cda_z_rules

L92a5_non_alpha_rules:
    RULE "(A)="
    RULE "(!)=."
    RULE "(\") =-AH5NKWOWT-"
    RULE "(\")=KWOW4T-"
    RULE "(#)= NAH4MBER"
    RULE "($)= DAA4LER"
    RULE "(%)= PERSEH4NT"
    RULE "(&)= AEND"
    RULE "(')="
    RULE "(*)= AE4STERIHSK"
    RULE "(+)= PLAH4S"
    RULE "(,)=,"
    RULE " (-) =-"
    RULE "(-)="
    RULE "(.)= POYNT"
    RULE "(/)= SLAE4SH"
    RULE "(0)= ZIY4ROW"
    RULE " (1ST)=FER4ST"
    RULE " (10TH)=TEH4NTH"
    RULE "(1)= WAH4N"
    RULE " (2ND)=SEH4KUND"
    RULE "(2)= TUW4"
    RULE " (3RD)=THER4D"
    RULE "(3)= THRIY4"
    RULE "(4)= FOH4R"
    RULE " (5TH)=FIH4FTH"
    RULE "(5)= FAY4V"
    RULE " (64) =SIH4KSTIY FOHR"
    RULE "(6)= SIH4KS"
    RULE "(7)= SEH4VUN"
    RULE " (8TH)=EY4TH"
    RULE "(8)= EY4T"
    RULE "(9)= NAY4N"
    RULE "(:)=."
    RULE "(;)=."
    RULE "(<)= LEH4S DHAEN"
    RULE "(=)= IY4KWULZ"
    RULE "(>)= GREY4TER DHAEN"
    ; Note: The C64 version originally contained a bug where '?' did not cause
    ;       a rising inflection when using RECITER.
    RULE "(?)=?" ; (originally '(?)=.')
    RULE "(@)= AE6T"
    RULE "(^)= KAE4RIXT"

; Trackloader in SAM.PRG loads SAM to $9500
.segment "SAM"

; Address of CHRGET routine, which fetches next character of BASIC program text
CHRGET = $0073

L9500_install_wedge:
    ; Install wedge by overwriting first 3 bytes of CHRGET with 'jmp L950d_wedge'.
    lda #$4c
    sta CHRGET
    lda #<L950d_wedge ; (originally '#$0d')
    sta CHRGET + 1
    lda #>L950d_wedge ; (originally '#$95')
    sta CHRGET + 2
    rts

; 16b pointer used by original CHRGET to read the BASIC text.
basic_text_ptr = CHRGET + 7

L950d_wedge:
    inc basic_text_ptr
    bne L9513
    inc basic_text_ptr + 1              ; basic_text_ptr++
L9513:
    ; Save existing X/Y values before overwriting them
    sty L9934_saved_y
    stx L9933_saved_x

    ldy #$00                            ; Y = 0
    tsx                                 ; X = SP

    ; Probe stack to see if caller is BASIC ($a7e4 or $a48a).  Note that $a78a and $a4e4
    ; are known not to call CHRGET, and therefore there's no need to discriminate them.
    lda $0101,x
    cmp #$e6
    beq L952a
    cmp #$8c
    beq L952a                           ; if (stack[SP] != $e6 or stack[SP] != $8c) {
    jmp L962d_run_old_chrget            ;   goto L962d_run_old_chrget
L952a:                                  ; }

    ; Probe next stack frame for $a7 or $a4
    lda $0102,x
    cmp #$a7
    beq L9538
    cmp #$a4
    beq L9538                           ; if (stack[SP + 1] != $a7 || stack[SP + 1] != $a4) {
    jmp L962d_run_old_chrget            ;   goto L962d_run_old_chrget
L9538:                                  ; }

    ; Check if next character is 'S' or ']'
    lda (basic_text_ptr),y              ; A = basic_text_ptr[Y]
    cmp #$53
    beq L9548                           ; if (A == 'S') goto L9548
    cmp #$5d
    bne L9545                           ; if (A != ']') goto run_old_chrget()
    jmp L9636                           ; else goto L9636

L9545:
    jmp L962d_run_old_chrget

; Found 'S'
L9548:
    ; Check if next letter is 'A'
    ldy #$01
    lda (basic_text_ptr),y              ; A = basic_text_ptr[1]
    cmp #$41
    beq L9553                           ; if (A != 'A') {
    jmp L962d_run_old_chrget            ;   goto run_old_chrget
L9553:                                  ; }

    ; Check if next letter is 'Y'
    iny                                 ; Y++
    lda (basic_text_ptr),y              ; A = basic_text_ptr[2]
    cmp #$59
    beq L955d                           ; if (A != 'Y') {
    jmp L962d_run_old_chrget            ;   goto run_old_chrget
L955d:                                  ; }

    ldx #$00                            ; X = 0

    ; Skip spaces
L955f:                                  ; do {
    iny                                 ;   Y++
    lda (basic_text_ptr),y              ;   A = basic_text_ptr[Y]
    cmp #$20
    beq L955f                           ; while(A == ' ')
    cmp #$3a
    bne L956d                           ; if (A == ':') {
    jmp L9603_parse_next_stmt           ;   goto L9603_parse_next_stmt
L956d:                                  ; }
    cmp #$00
    bne L9574                           ; if (A == '\0') {
    jmp L9618_skip_and_run_old_chrget   ;   goto L9618_skip_and_run_old_chrget
L9574:                                  ; }
    cmp #$22
    beq L957b                           ; if (A != '"')
    jmp L95a4                           ;   goto L95a4
                                        ; }

; Copy from basic text to 'input[]' until we find the matching quote (").
L957b:                                  ; do {
    iny                                 ;   Y++
    lda (basic_text_ptr),y              ;   A = basic_text_ptr[Y]
    cmp #$22
    beq L9589                           ;   if (A != '"') {
    sta L9a15_input,x                   ;     L9a15_input[X]
    inx                                 ;     X++
                                        ;   }
    jmp L957b                           ; }
L9589:
    ; Terminate 'input[]' with $9b
    lda #$9b
    sta L9a15_input,x                   ; L9a15_input[X] = $9b

    ; Save position in 'basic_text_ptr'
    sty L9935                           ; L9935 = Y

    ; Invoke SAM or RECITER ML entry point (target address is modified)
L9592 = * + 1
    jsr L9a03_SAM_ML_entry

    ; Restore Y to last position in 'basic_text_ptr'
    ldy L9935
L9597:                                  ; loop {
    iny                                 ;   Y++
    lda (basic_text_ptr),y              ;   A = basic_text_ptr[Y]
    cmp #$3a
    beq L9603_parse_next_stmt           ;   if (A == ':') goto L9603_parse_next_stmt
    cmp #$00
    beq L9618_skip_and_run_old_chrget   ;   if (A == '\0') goto L9618_skip_and_run_old_chrget
    bne L9597                           ; }  (near-jump)

L95a4:
    cmp #$41
    bcs L95ab                           ; if (A < 'A') {
    jmp L962d_run_old_chrget            ;   goto run_old-chrget
L95ab:                                  ; }

    cmp #$5b
    bcs L962d_run_old_chrget            ; if (A >= '[') goto run_old-chrget
    sta L9936                           ; L9936 = A
    lda #$80
    sta L9937                           ; L9937 = $80
    iny                                 ; Y++
    lda (basic_text_ptr),y              ; A = basic_text_ptr[Y]
    cmp #$24
    beq L95d3                           ; if (A != '$') {
    sta L9937                           ;   L9937 = A
L95c1:                                  ;   loop {
    iny                                 ;     Y++
    lda (basic_text_ptr),y              ;     A = basic_text_ptr[Y]
    cmp #$24
    beq L95d3                           ;     if (A == '$') break
    cmp #$00
    beq L962d_run_old_chrget            ;     if (A == '\0') goto run_old_chrget
    cmp #$3a
    beq L962d_run_old_chrget            ;     if (A == ':') goto run_old_chrget
    jmp L95c1                           ;   }
L95d3:                                  ; }

    lda L9936                           ; A = L9936
    sta La3da                           ; La3da = A
    lda L9937
    ora #$80
    sta La3e1                           ; La3e1 = L9937 | $80
    sty L9935                           ; Y = La3e1

    ; Invoke SAM or RECITER BASIC entry (target address is modified)
L95e5 = * + 1
    jsr L9a00_SAM_BASIC_entry
    ldy L9935                           ; Y = L9935
    lda #$53
    sta La3da                           ; La3da = $53
    lda #$c1
    sta La3e1                           ; La3e1 = $c1
L95f4:                                  ; do {
    iny                                 ;   Y++
    lda (basic_text_ptr),y              ;   A = basic_text_ptr[Y]
    bne L95fc                           ;   if (A == '\0') {
    jmp L9618_skip_and_run_old_chrget   ;     goto L9618_skip_and_run_old_chrget
L95fc:                                  ;   }
    cmp #$3a
    bne L95f4                           ; } while (A != ':')
    jmp L9603_parse_next_stmt ; TODO: Remove and fall-through?

L9603_parse_next_stmt:
    ; Advance 'basic_text_ptr' past the portion of text handled by SAM wedge
    tya
    clc
    adc basic_text_ptr
    sta basic_text_ptr
    lda basic_text_ptr + 1
    adc #$00
    sta basic_text_ptr + 1              ; basic_text_ptr += Y

    ; Restore previous X/Y values before invoking original CHRGET
    ldx L9933_saved_x
    ldy L9934_saved_y
    jmp CHRGET                          ; Invoke wedged CHRGET, potentially handling another SAM command

L9618_skip_and_run_old_chrget:
    ; Advance 'basic_text_ptr' past the portion of text handled by SAM wedge
    tya
    clc
    adc basic_text_ptr
    sta basic_text_ptr
    lda basic_text_ptr + 1
    adc #$00
    sta basic_text_ptr + 1              ; basic_text_ptr += Y

    ; TODO: Remove and fall through to 'L962d_run_old_chrget' below?
    ; Restore previous X/Y values before invoking original CHRGET
    ldx L9933_saved_x
    ldy L9934_saved_y
    jmp CHRGET + 6                      ; Invoke original CHRGET, just after 'basic_text_ptr' has been incremented.

L962d_run_old_chrget:
    ; Restore previous X/Y values before invoking original CHRGET
    ldx L9933_saved_x
    ldy L9934_saved_y
    jmp CHRGET + 6                      ; Invoke original CHRGET, just after 'basic_text_ptr' has been incremented.

; Found ']'
L9636:
    iny                                 ; Y++
    lda (basic_text_ptr),y              ; A = basic_text_ptr[Y]
    cmp #$45                            ; switch() {
    beq L965e                           ;   case 'E': goto L965e
    cmp #$4b
    beq L9661                           ;   case 'K': goto L9661
    cmp #$4c
    beq L968f                           ;   case 'L': goto L968f
    cmp #$50
    beq L969d                           ;   case 'P': goto L969d
    cmp #$52
    beq L96cb                           ;   case 'R': goto L96cb
    cmp #$53
    bne L9654                           ;   default: goto L9654
    jmp L96d8                           ;   case 'S': goto L96d8
                                        ; }
L9654:
    cmp #$51
    bne L965b                           ; if (A == 'Q') {
    jmp L9707_restore_chrget            ;   goto restore_chrget     (']Q' uninstalls SAM wedge)
L965b:                                  ; }
    jmp L962d_run_old_chrget            ; else goto run_old_chrget

L965e:
    jmp L9716                           ; goto L9716

; Handle ']K*': Set mouth/throat knobs and recalculate F1 and F2 tables
L9661:
    jsr S9779_parse_number              ; parse_number()
    lda L9939_found_number
    bne L966c                           ; if (!found_number) {
    jmp L962d_run_old_chrget            ;   goto run_old_chrget
L966c:                                  ; }
    lda L9938_number
    sta L97e0_throat                    ; throat = number
    jsr S9779_parse_number              ; parse_number()
    lda L9939_found_number
    bne L967d                           ; if (!found_number) {
    jmp L962d_run_old_chrget            ;   goto run_old_chrget
L967d:                                  ; }
    lda L9938_number
    sta L97e1_mouth                     ; mouth = number
    sty L9935
    jsr S97e2_SetMouthThroat            ; SetMouthThroat();
    ldy L9935
    jmp L97cc_expect_end_of_stmt        ; goto L97cc_expect_end_of_stmt

; Handle ']L*': Distinguish between LOAD and LIGHTS
L968f:
    iny                                 ; Y++
    lda (basic_text_ptr),y              ; basic_text_ptr[Y]
    cmp #$49
    beq L96b1                           ; if (A == 'I') goto L96b1
    cmp #$4f
    beq L96c5                           ; if (A == 'O') goto L96c5
    jmp L962d_run_old_chrget

; Handle ']P*': Set 'PITCH' value
L969d:
    jsr S9779_parse_number              ; parse_number()
    lda L9939_found_number
    bne L96a8                           ; if (!found_number) {
    jmp L962d_run_old_chrget            ;   goto run_old_chrget
L96a8:                                  ; }
    lda L9938_number
    sta L9a0f_pitch                     ; pitch = number
    jmp L97cc_expect_end_of_stmt        ; goto L97cc_expect_end_of_stmt

; Handle "]LI*": Set 'LIGHTS' on/off
L96b1:
    jsr S9779_parse_number              ; parse_number()
    lda L9939_found_number
    bne L96bc                           ; if (!found_number) {
    jmp L962d_run_old_chrget            ;   goto run_old_chrget
L96bc:                                  ; }
    lda L9938_number
    sta L9a10_lights_enabled            ; lights_enabled = number
    jmp L97cc_expect_end_of_stmt        ; goto L97cc_expect_end_of_stmt

; Handle "]LO*": Load RECITER
L96c5:
    jsr S9940_load_reciter              ; load_reciter
    jmp L97cc_expect_end_of_stmt        ; goto expect_end_of_stmt

; Handle "]R*": Switch to RECITER (TTS mode)
L96cb:
    ; Modifies the target address of the 'jsr's used by "SAY" to call RECITER
    ; to generate phonemes from text.
    ;
    ; TODO: This only modifies the low byte of the pointer since the SAM/RECITER
    ;       entry points are known to have the same high byte in the C64 memory
    ;       layout.
    lda #<L9a09_Reciter_ML_entry ; (originally #$09)
    sta L9592
    lda #<L9a06_Recicter_BASIC_entry ; (originally #$06)
    sta L95e5
    jmp L97cc_expect_end_of_stmt        ; goto expect_end_of_stmt

; Handle "]S": Distinguish between SAM and SPEED
L96d8:
    iny                                 ; Y++
    lda (basic_text_ptr),y              ; basic_text_ptr[Y]
    cmp #$41
    beq L96e6                           ; if (A == 'A') goto L96e6
    cmp #$50
    beq L96f3                           ; if (A == 'P') goto L96f3
    jmp L962d_run_old_chrget            ; goto run_old_chrget

; Handle "]SA*": Switch to SAM (phoneme mode)
L96e6:
    ; Modifies the target address of the 'jsr's used by "SAY" to bypass reciter
    ; and go directly to phoneme parsing and rendering.
    ;
    ; TODO: This only modifies the low byte of the pointer since the SAM/RECITER
    ;       entry points are known to have the same high byte in the C64 memory
    ;       layout.
    lda #<L9a03_SAM_ML_entry ; (originally #$03)
    sta L9592
    lda #<L9a00_SAM_BASIC_entry ; (originally #$00)
    sta L95e5
    jmp L97cc_expect_end_of_stmt        ; goto expect_end_of_stmt

; Handle "]SP*": Set SPEED value
L96f3:
    jsr S9779_parse_number              ; parse_number()
    lda L9939_found_number
    bne L96fe                           ; if (!found_number) {
    jmp L962d_run_old_chrget            ;   goto run_old_chrget
L96fe:                                  ; }
    lda L9938_number
    sta L9a0e_speed                     ; speed = number
    jmp L97cc_expect_end_of_stmt        ; goto expect_end_of_stmt

; Handle "]Q*": QUIT (uninstall wedge)
L9707_restore_chrget:
    ; Restore first 3 bytes with original values.
    ; TODO: Single caller: inline at $9658?
    lda #$e6
    sta CHRGET
    lda #$7a
    sta CHRGET + 1                      ; Restore 'inc $7a'
    lda #$d0
    sta CHRGET + 2                      ; Restore 'bne' opcode of 'bne $0079'
    jmp L97cc_expect_end_of_stmt

; Matched "]E*": Print last ERROR
L9716:
    lda L9a14_error_pos                 ; A = error_pos

    ; Check if an error occured:
    cmp #$ff
    bne L9720                           ; if (A == 255) {
    jmp L97cc_expect_end_of_stmt        ;   goto L97cc_expect_end_of_stmt
L9720:                                  ; }

    dec L9a14_error_pos                 ; L9a14_error_pos--
    jsr $ffcc                           ; Close I/O channel to ensure output goes to screen
    ldx #$00                            ; X = 0
L9728:                                  ; do {
    lda L9a15_input,x                   ;   A = L9a15_input[X]
    cpx L9a14_error_pos
    bne L973a                           ;   if (X == error_pos) {
    jsr S974d                           ;     S974d()
    lda L993a                           ;     A = L993a
    cmp #$9b
    beq L976e                           ;     if (A == $9b) goto L976e
L973a:                                  ;   }
    cmp #$9b
    bne L9741                           ;   if (A != $9b) {
    jmp L976e                           ;     goto L976e
L9741:                                  ;   }

    jsr $ffd2                           ;   CHROUT(A)
    inx                                 ;   X++
    bne L9728                           ; }
    inc L9a14_error_pos                 ; error_pos++
    jmp L97cc_expect_end_of_stmt        ; goto L97cc_expect_end_of_stmt

; Print one character in reverse video and read next from 'input[]'
S974d:
    ; Enable reverse video, preserving register A to L993a
    sta L993a
    lda #$12
    jsr $ffd2                           ; CHROUT({RVS ON})
    lda L993a
    cmp #$9b
    bne L975e                           ; if (A == $9b) {
    lda #$20                            ;   A = ' '
L975e:                                  ; }
    jsr $ffd2                           ; CHROUT(A)

    ; Disable reverse video
    lda #$92
    jsr $ffd2                           ; CHROUT({RVS OFF})

    ; Read next character from 'input[]'
    inx                                 ; X++
    lda L9a15_input,x                   ; A = L9a15_input[X]
    sta L993a                           ; L993a = A
    rts                                 ; return

L976e:
    lda #$0d
    jsr $ffd2                           ; CHROUT({RETURN})
    inc L9a14_error_pos                 ; error_pos++
    jmp L97cc_expect_end_of_stmt

; Parses a decimal number at the current position (Y) in 'basic_text_ptr[]'
; On success, 'found_number' == 1 and 'number' contains the parsed value.
; TODO: Return success via 'A' instead?  (All callers 'lda' as next instr.)
S9779_parse_number:
    lda #$00
    sta L9938_number                    ; L9938_number = 0
    sta L9939_found_number              ; L9939_found_number = 0

L9781:                                  ; loop {
    iny                                 ;   Y++
    lda (basic_text_ptr),y              ;   A = basic_text_ptr[Y]
    cmp #$3b
    bcs L9781                           ;   if (A >= ';') continue
    cmp #$3a
    beq L97cb_return                    ;   if (A == ':') return
    cmp #$00
    beq L97cb_return                    ;   if (A == '\0') return
    cmp #$30
    bcc L9781                           ;   if (A >= '0') break;
                                        ; }

    ; Found a digit.  Set the 'found_number' flag.
    lda #$01
    sta L9939_found_number              ; L9939_found_number = 1

    ; Parse the number, converting from decimal as we go.
    lda (basic_text_ptr),y              ; A = basic_text_ptr[Y]
    sec
    sbc #$30
    sta L9938_number                    ; L9938_number = A - '0'
L97a1:                                  ; loop {
    iny                                 ;   Y++
    lda (basic_text_ptr),y              ;   A = basic_text_ptr[Y]
    cmp #$30
    bcs L97a9                           ;   if (A < '0') {
    rts                                 ;     return
L97a9:                                  ;   }
    cmp #$3a
    bcc L97ae                           ;   if (A > '9') {
    rts                                 ;     return
L97ae:                                  ;   }

    lda L9938_number                    ;   A = L9938_number
    asl L9938_number
    asl L9938_number
    asl L9938_number                    ;   L9938_number <<= 3
    asl a                               ;   A <<= 1
    clc
    adc L9938_number                    ;   A += L9938_number
    clc
    adc (basic_text_ptr),y              ;   A += basic_text_ptr[y]
    sec
    sbc #$30                            ;   A -= '0'
    sta L9938_number                    ;   L9938_number = A
    jmp L97a1                           ; }

L97cb_return:
    rts

L97cc_expect_end_of_stmt:               ; loop {
    lda (basic_text_ptr),y              ;   A = basic_text_ptr[Y]

    bne L97d3                           ;   if (A == 0) {
    jmp L9618_skip_and_run_old_chrget   ;     goto L9618_skip_and_run_old_chrget
L97d3:                                  ;   }
    cmp #$3a
    bne L97da                           ;   if (A == ':') {
    jmp L9603_parse_next_stmt           ;     goto L9603_parse_next_stmt
L97da:                                  ;   }

    iny                                 ;   Y++
    jmp L97cc_expect_end_of_stmt        ; }

Padding3: .res $2 ; Remove?

L97e0_throat:  .byte $80
L97e1_mouth:   .byte $80

; SAM's voice can be altered by changing the frequencies of the
; mouth formant (F1) and the throat formant (F2). Only the voiced
; phonemes (5-29 and 48-53) are altered.
.proc S97e2_SetMouthThroat
    ; Switch $A000-$BFFF to RAM. (TODO: Merge with L9b54?)
    lda LORAM
    and #$fe
    sta LORAM

    ; One-time initialization of mouth and throat formants tables

    ; TODO: Mouth/throat formant tables are const after initialization.
    ;       Statically initialize and skip copy at runtime?

    lda L9932_formants_initialized      ; if (!is_voice_initialized) {
    bne L9819

    ; On initialization, copy the original mouth and throat frequences
    ; from 'freq1data' and 'freq2data' to 'mouthFormants5_29' and
    ; 'throatFormants5_29'

    ; Copy:
    ;     mouthFormants5_29[$5..$1d] = freq1data[$5..$1d]
    ;    throatFormants5_29[$5..$1d] = freq2data[$5..$1d]

    ldx #$05                            ;   X = 5
L97ef:                                  ;   do {
    lda Lb000_freq1data,x               ;     mouthFormants5_29[X] = freq1data[X]
    sta L98e4_throatFormants5_29,x
    lda Lb050_freq2data,x               ;     throatFormants5_29[X] = freq2data[X]
    sta L9902_mouthFormants5_29,x
    inx                                 ;     X++
    cpx #$1e
    bne L97ef                           ;   } while (X != $1e)

    ; Copy:
    ;    mouthFormants48_53[$0..$5] = freq1data[$30..$35]
    ;   throatFormants48_53[$0..$5] = freq2data[$30..$35]

    ldx #$30                            ;   X = $30
    ldy #$00                            ;   Y = $0
L9804:                                  ;   do {
    lda Lb000_freq1data,x
    sta L9920_throatFormants48_53,y     ;     mouthFormants5_29[Y] = freq1data[X]
    lda Lb050_freq2data,x
    sta L9926_mouthFormants48_53,y      ;     throatFormants5_29[Y] = freq2data[X]
    inx                                 ;     X++
    iny                                 ;     Y++
    cpx #$36
    bne L9804                           ;   } while (X != $36)

    inc L9932_formants_initialized      ;   L9932_formants_initialized = true
L9819:                                  ; }

    ; Recalculate formant frequencies 5..29 for the mouth (F1) and throat (F2)
    lda #$05
    sta L9930_pos                       ; pos = 5
L981e_loop_5_29:                        ; loop {
    ldx L9930_pos
    cpx #$1e
    beq L9893                           ;   if (pos == $1e) break
    lda #$00                            ;   A = 0
    sta L9931_ret_dispatch              ;   ret_dispatch = 0
    lda L97e0_throat
    sta L992c                           ;   L992c = throat_knob
    lda L98e4_throatFormants5_29,x
    sta L992d_initialFrequency          ;   initialFrequency = mouthFormants5_29[x]
    bne L9860_trans                     ;   if (initialFrequency != 0) trans(throat, initialFrequency);
L9838_cont_0:
    lda L992f_trans_result
    ldx L9930_pos
    sta Lb000_freq1data,x               ;   freq1data[pos] = L992f_trans_result
    inc L9931_ret_dispatch              ;   ret_dispatch = 1
    lda L97e1_mouth
    sta L992c                           ;   L992c = mouth_knob
    lda L9902_mouthFormants5_29,x
    sta L992d_initialFrequency          ;   initialFrequency = throatFormants5_29[x]
    bne L9860_trans                     ;   if (initialFrequency != 0) trans(mouth, initialFrequency);
L9852_cont_1:
    lda L992f_trans_result
    ldx L9930_pos
    sta Lb050_freq2data,x               ;   Lb050_freq2data[pos] = L992f_trans_result
    inc L9930_pos                       ;   pos++
    bne L981e_loop_5_29                 ; } // NOTE: near-jmp (A always != 0)

    ; Computes 'result = (L992C*initialFrequency) >> 1' (?)
    ; Multiply by adding 'initialFrequency' and ror in a loop
    .proc L9860_trans
        lda #$00
        sta L992f_trans_result          ; result = 0
        sta L992e                       ; L992e = 0
        ldx #$08                        ; X = 8
    L986a:                              ; do {
        lsr L992c                       ;   lsr(L992c)
        bcc L9879                       ;   if (carry) {
        clc                             ;     clear carry
        lda L992f_trans_result
        adc L992d_initialFrequency      ;
        sta L992f_trans_result          ;     trans_result += initialFrequency
    L9879:                              ;   }
        ror L992f_trans_result          ;   ror(result)
        dex                             ;   X--
        bne L986a                       ; } while (X != 0)
        rol L992e                       ; rol(L992e)
        rol L992f_trans_result          ; rol(result)

        ; TODO: Convert to jsr/rts instead of using 'ret_dispatch' to jump?
        ldx L9931_ret_dispatch          ; X = ret_dispatch
        beq L9838_cont_0                ; if (X == 0) return
        dex                             ; X--
        beq L9852_cont_1                ; if (X == 0) return
        dex                             ; X--
        beq L98b4_cont_2                ; if (X == 0) return
        dex                             ; X--
        beq L98ce_cont_3                ; if (X == 0) return
        ; TODO: Does fallthrough happen?
    .endproc

L9893:
    lda #$30
    sta L9930_pos                       ; pos = $30
    ldy #$00                            ; Y = 0
L989a:
    ldx L9930_pos
    cpx #$36
    beq L98dd                           ; while (pos < 36) {
    lda #$02
    sta L9931_ret_dispatch              ;   ret_dispatch = 2
    lda L97e0_throat
    sta L992c                           ;   L992c = throat_knob
    lda L9920_throatFormants48_53,y
    sta L992d_initialFrequency          ;   initialFrequency = mouthFormants48_53[Y]
    bne L9860_trans                     ;   if (initialFrequency) { trans(L992c, initialFrequency) }
L98b4_cont_2:
    lda L992f_trans_result
    ldx L9930_pos
    sta Lb000_freq1data,x               ;   freq1data[x] = trans_result
    inc L9931_ret_dispatch              ;   ret_dispatch++
    lda L97e1_mouth
    sta L992c                           ;   L992c = L97e1_mouth
    lda L9926_mouthFormants48_53,y
    sta L992d_initialFrequency          ;   initialFrequency = L9926_mouthFormants48_53[Y]
    bne L9860_trans                     ;   if (initialFrequency) { trans(L992c, initialFrequency) }
L98ce_cont_3:
    lda L992f_trans_result
    ldx L9930_pos
    sta Lb050_freq2data,x               ;   freq2data[x] = trans_result
    iny                                 ;   Y++
    inc L9930_pos                       ;   pos++
    bne L989a                           ; } // NOTE: near-jmp (A always != 0)
L98dd:
    ; Restore $A000-$BFFF to BASIC ROM (see 'SetMouthThroat').
    ; TODO: Merge with L9bcd?
    lda LORAM
    ora #$01
    sta LORAM
    rts
.endproc

; Formant 1 frequencies (throat) 5..29
; TODO: Could 0..4 be elided?
L98e4_throatFormants5_29:
    .byte $00, $00, $00, $00, $00, $0a, $0e, $13, $18, $1b, $17, $15
    .byte $10, $14, $0e, $12, $0e, $12, $12, $10, $0d, $0f, $0b, $12, $0e, $0b, $09, $06
    .byte $06, $06

; Formant 2 frequencies (mouth) 5..29
; TODO: Could 0..4 be elided?
L9902_mouthFormants5_29:
    .byte $ff, $ff, $ff, $ff, $ff, $54, $49, $43, $3f, $28, $2c, $1f, $25, $2d
    .byte $49, $31, $24, $1e, $33, $25, $1d, $45, $18, $32, $1e, $18, $53, $2e, $36, $56

; Formant 1 frequencies (throat) 48..53 (NOTE: Table should not contain zeros.)
L9920_throatFormants48_53:
    .byte $13, $1b, $15, $1b, $12, $0d

; Formant 2 frequencies (mouth) 48..53 (NOTE: Table should not contain zeros.)
L9926_mouthFormants48_53:
    .byte $48, $27, $1f, $2b, $1e, $22

L992c:                      .byte $00   ; TODO: Review if .res?
L992d_initialFrequency:     .byte $22   ; TODO: Review if .res?
L992e:                      .byte $00   ; TODO: Review if .res?
L992f_trans_result:         .byte $22   ; TODO: Review if .res?
L9930_pos:                  .byte $36   ; TODO: Review if .res?
L9931_ret_dispatch:         .byte $03   ; TODO: Review if .res?

; Non-zero if mouth/throat formant tables have been initialized.
; TODO: Remove since tables are statically initialized?
L9932_formants_initialized: .byte $01

L9933_saved_x:              .byte $01   ; TODO: Review if .res?
L9934_saved_y:              .byte $44   ; TODO: Review if .res?
L9935:                      .byte $06
L9936:                      .byte $53
L9937:                      .byte $41
L9938_number:               .byte $48   ; TODO: Review if .res?
L9939_found_number:         .byte $01   ; TODO: Review if .res?

L993a: ; TODO: Review if .res?
    .byte $45, $ff, $ff, $ff, $ff, $ff

; Loads 'RECITER.PRG' from drive 8 to $7d00 (low memory), then
; given the user the option to relocate to $c000 (high memory).
S9940_load_reciter:
    lda #$08
    ldx #$08
    ldy #$00
    jsr $ffba                           ; SETLFS(logical = 8, device = 8, secondary = 0)
    lda #$07
    ldx #<L99e0_filename ; (originally #$e0)
    ldy #>L99e0_filename ; (originally #$99)
    jsr $ffbd                           ; SETNAM(length = 7, name = "reciter")
    lda #$00
    ldx #<L7d00_a_rules ; (originally #$00)
    ldy #>L7d00_a_rules ; (originally #$7d)
    clc
    jsr $ffd5                           ; LOAD(load, address = $7d00)
    bcc L9979                           ; if (C != 0) {
    jsr $ffcc                           ;   CLRCHN();

    ; Print "FILE ERROR"
    ldx #$0c                            ;   X = $C
L9963:                                  ;   do {
    lda L996c_file_err_msg,x            ;     A = L996C_file_err_msg[X]
    jsr $ffd2                           ;     CHROUT(A)
    dex                                 ;     X--
    bne L9963                           ;   } while (X)
L996c_file_err_msg:
    rts                                 ;   return

.byte "\nrorre elif\n"

L9979:                                  ; }
    jsr $ffcc                           ; CLRCHN();

    ; Print "LOW OR HIGH MEMORY?"
    ldx #$15                            ; X = 15
L997e:                                  ; do {
    lda L99e6_load_prompt,x             ;   A = L99e6_load_prompt[X]
    jsr $ffd2                           ;   CHROUT(A)
    dex                                 ;   X--
    bne L997e                           ; } while (X)
    lda #$00
    sta mem51                           ; mem51 = 0
    sta mem53                           ; mem53 = 0
    sta mem55                           ; mem55 = 0
L998f:                                  ; loop {
    jsr $ffe4                           ;   GETIN();
    and #$7f                            ;   A &= $7f      (Unshift key input)
    cmp #$4c
    beq L999f                           ;   if (A == 'L') goto L999f
    cmp #$48
    beq L99a8                           ;   if (A == 'H') goto L99a8
    jmp L998f                           ; }

; Handle 'L': Load reciter into low memory
; TODO: Call from 'startup.s'?
L999f:
    lda #$7d
    sta mem52                           ; mem52 = $7d (<FRETOP: top of string stack)
    sta mem54                           ; mem54 = $7d (<FRESPC: utility pointer for strings)
    sta mem56                           ; mem56 = $7d (<MEMSIZ: highest BASIC RAM address / bottom of string stack)
    rts                                 ; return

; Handle 'H': Move reciter to high memory
L99a8:
    ldx #$1a                            ; X = 26
L99aa:                                  ; do {
    dex                                 ;   X--
    lda L928b_alpha_rules_hi,x
    clc
    adc #$43
    sta L928b_alpha_rules_hi,x          ;   L928b_alpha_rules_hi[X] += $43
    cpx #$00
    bne L99aa                           ; } while (X != 0)
    stx mem251                          ; mem251 = 0
    stx mem253                          ; mem253 = 0
    lda #$7d
    sta mem252                          ; mem252 = $7d
    lda #$c0
    sta mem254                          ; mem254 = $c0

    ldy #$00                            ; Y = 0
L99c6:                                  ; do {
    lda (mem251),y
    sta (mem253),y                      ;   mem252[Y] = mem251[Y]
    iny                                 ;   Y++
    bne L99c6                           ;   if (Y != 0) continue;
    inc mem252                          ;   mem252++
    inc mem254                          ;   mem254++
    lda mem254
    cmp #$d0
    bne L99c6                           ; } while (mem254 != $d0)
    lda #$8d
    sta mem52                           ; mem52 = $8d (<FRETOP: top of string stack)
    sta mem54                           ; mem54 = $8d (<FRESPC: utility pointer for strings)
    sta mem56                           ; mem56 = $8d (<MEMSIZ: highest BASIC RAM address / bottom of string stack)
    rts                                 ; return

L99e6_load_prompt = L99e0_filename + 6
L99e0_filename:
    .byte "reciter\n?yromem hgih ro wol\n"
    .byte $00, $00, $00, $00

L9a00_SAM_BASIC_entry:
    jmp L9b15_SAM_from_BASIC

L9a03_SAM_ML_entry:
    jmp L9b5b_SAM_from_ML

L9a06_Recicter_BASIC_entry:
    jmp L8e78_Reciter_from_BASIC

L9a09_Reciter_ML_entry:
    jmp L8e84_Reciter_from_ML

Padding6: .res $2; TODO: Remove?

L9a0e_speed:                .byte $48
L9a0f_pitch:                .byte $40
L9a10_lights_enabled:       .byte $00
L9a11_interrupts_enabled:   .byte $00
L9a12_saved_vic_ctrl1:      .res $1
L9a13_saved_vic_spr_ena:    .res $1
L9a14_error_pos:            .byte $ff ; TODO: Review if .res?

L9a15_input: .res $100

L9b15_SAM_from_BASIC:
    jsr L9b39_ConfigureSid
    jsr Sa3cd

    ; Check for error and invoke SAM on success
    lda mem27
    beq L9b5b_SAM_from_ML   ; if (mem27 == 0) SAM_from_ML
    rts                     ; return

; Invoked at the beginning of 'SAMMain' to configure the C64 hardware
; for playing digital audio:
;
;   - If 'lights_enabled' is zero (default), disables video to prevent
;     audio artifacts from the VIC-II stealing cycles from the CPU.
;
;   - If 'interrupts_enabled' is zero (default), disables interrupts
;     for the same reason.
;
;   - Configures the SID to emit a pulse wave with maxmimum duty cycle
;     so that the audio output can be controlled via the volume register.
;
; TODO: Inline or extract to platform-specific 'c64.s'?
S9b20_say_init:
    lda L9a10_lights_enabled            ; if (L9a10_lights_enabled)
    bne L9b39_ConfigureSid              ;   goto ConfigureSid

    lda VIC_CTRL1                       ; Save original values of VIC-II registers
    sta L9a12_saved_vic_ctrl1
    lda VIC_SPR_ENA
    sta L9a13_saved_vic_spr_ena
    lda #$00                            ; Disable video
    sta VIC_CTRL1
    sta VIC_SPR_ENA
L9b39_ConfigureSid:
    lda #$ff                            ; Configure SID:
    sta SID_PB1Lo                       ; Pulse waveform duty cycle = $ffff
    sta SID_PB1Hi
    sta SID_SUR1                        ; Sustain = $ff, Release = $f
    lda #$00
    sta SID_AD1                         ; Attack = $0, Release = $0
    lda #$41
    sta SID_Ctl1                        ; Pulse = 1, Gate = 1

    lda L9a11_interrupts_enabled        ; if (!L9a11_interrupts_enabled) {
    bne L9b54
    sei                                 ;   disable interrupts
L9b54:                                  ; }
    ; Switch $A000-$BFFF to RAM.  This is a vestage from when code/data was
    ; located in LORAM.  (TODO: Merge with 'SetMouthThroat'?)
    lda LORAM
    and #$fe
    sta LORAM
    rts

; Main entry point for 'say'.
.proc L9b5b_SAM_from_ML
    jsr S9b20_say_init                  ; Configure SID and (optionally) disable video/interrupts
    jsr Sb9c0_SaveZeropage              ; Save zeropage area clobbered by mem19..mem66_pos
    ; fall through to say_main
.endproc

.proc S9b61_say_main
    lda #$ff
    sta L9a14_error_pos                 ; error_pos = $ff (no error)
    jsr La068_Parser1                   ; Parse 'input[]' and populate 'phonemeIndex[]'
    lda L9a14_error_pos
    cmp #$ff
    bne L9bbc_say_uninit                ; if (error_pos != $ff) goto say_uninit
    jsr Sa1b5_Parser2                   ; Rules base replacement of certain phoneme patterns
    jsr Sa39b_CopyStress                ; Rules based adjustment of stress
    jsr Sa0f3_SetPhonemeLength          ; Change phonemeLength depedendent on stress
    jsr Sbdeb_AdjustLengths             ; Rules based length adjustments
    jsr Sa118_Code41240

    lda #$00                            ; TODO: remove?

    lda #$07                            ; These control the length of a busy wait
    sta unvoiced_length                 ; when playing samples.
    lda #$06
    sta voiced_length

    lda L9a0f_pitch
    sta Lba3f_pitch                     ; pitch = pitch_knob
    lda L9a0e_speed
    sta Lbc18_speed                     ; speed = speed_knob

L9b97:                                  ; do {
    lda L9be0_phonemeIndex,x            ;   A = phonemeIndex[X];
    cmp #$50
    bcs L9ba3                           ;   if (A > $50) goto found_error
    inx                                 ;   X++
    bne L9b97                           ; } while (X != 0)
    beq L9ba8                           ; goto insert_breath
L9ba3: ; 'found_error'
    lda #$ff
    sta L9be0_phonemeIndex,x            ; phonemeIndex[X] = $ff

L9ba8:
    jsr Sbd2f_InsertBreath

    ; Ensure there is a $ff at the end of 'phonemeIndex[]'
    lda #$ff
    sta L9be0_phonemeIndex + $fe


    jsr Sbda3_PrepareOutput

    ldx #$00
    cpx mem29                           ; was_zero = mem29 == 0
    stx mem29                           ; mem29 = 0

    beq L9bbc_say_uninit                ; if (was_zero) goto say_uninit()
    rts
.endproc

L9bbc_say_uninit:
    ; Restore VIC II settings
    lda L9a10_lights_enabled
    bne L9bcd
    lda L9a12_saved_vic_ctrl1
    sta VIC_CTRL1
    lda L9a13_saved_vic_spr_ena
    sta VIC_SPR_ENA
L9bcd:
    jsr Sb9cb_RestoreZeropage
L9bd0:
    ; Restore $A000-$BFFF to BASIC ROM (See L9b54).
    ; TODO: Merge with L98dd?
    lda LORAM
    ora #$01
    sta LORAM
    cli             ; Enable interrupts
    rts

Padding7: .res $8 ; TODO: Remove?

L9be0_phonemeIndex:     .res $100
L9ce0_phonemeLength:    .res $100
L9de0_stress:           .res $100

; TODO: Review if first byte of stressInputTable unused?
L9ee0_stressInputTable:
    .byte '*', '1', '2', '3', '4', '5', '6', '7', '8', '9'

L9eea_signInputTable1:
    .byte ' ', '.', '?', ',', '-', 'i', 'i', 'e'
    .byte 'a', 'a', 'a', 'a', 'u', 'a', 'i', 'e'
    .byte 'u', 'o', 'r', 'l', 'w', 'y', 'w', 'r'
    .byte 'l', 'w', 'y', 'm', 'n', 'n', 'd', 'q'
    .byte 's', 's', 'f', 't', '/', '/', 'z', 'z'
    .byte 'v', 'd', 'c', '*', 'j', '*', '*', '*'
    .byte 'e', 'a', 'o', 'a', 'o', 'u', 'b', '*'
    .byte '*', 'd', '*', '*', 'g', '*', '*', 'g'
    .byte '*', '*', 'p', '*', '*', 't', '*', '*'
    .byte 'k', '*', '*', 'k', '*', '*', 'u', 'u'
    .byte 'u'

L9f3b_signInputTable2:
    .byte '*', '*', '*', '*', '*', 'y', 'h', 'h'
    .byte 'e', 'a', 'h', 'o', 'h', 'x', 'x', 'r'
    .byte 'x', 'h', 'x', 'x', 'x', 'x', 'h', '*'
    .byte '*', '*', '*', '*', '*', 'x', 'x', '*'
    .byte '*', 'h', '*', 'h', 'h', 'x', '*', 'h'
    .byte '*', 'h', 'h', '*', '*', '*', '*', '*'
    .byte 'y', 'y', 'y', 'w', 'w', 'w', '*', '*'
    .byte '*', '*', '*', '*', '*', '*', '*', 'x'
    .byte '*', '*', '*', '*', '*', '*', '*', '*'
    .byte '*', '*', '*', 'x', '*', '*', 'l', 'm'
    .byte 'n'

.enum Flags1
    None            = $00
    Plosive         = $01
    StopConsonant   = $02
    Voiced          = $04
    Unknown         = $08   ; TODO: M* N* NX DX Q* CH J* B* D* G* GX P* T* K* KX
    Dipthong        = $10
    DipthongYX      = $20   ; Dipthong ending in YX
    Consonant       = $40
    Vowel           = $80
.endenum

.feature c_comments

L9f8c_flags:
    /*  0:  '*' */ .byte Flags1::None                                                                   ; = $00
    /*  1: '.*' */ .byte Flags1::None                                                                   ; = $00
    /*  2: '?*' */ .byte Flags1::None                                                                   ; = $00
    /*  3: ',*' */ .byte Flags1::None                                                                   ; = $00
    /*  4: '-*' */ .byte Flags1::None                                                                   ; = $00
    /*  5: 'IY' */ .byte Flags1::Voiced | Flags1::DipthongYX | Flags1::Vowel                            ; = $a4
    /*  6: 'IH' */ .byte Flags1::Voiced | Flags1::DipthongYX | Flags1::Vowel                            ; = $a4
    /*  7: 'EH' */ .byte Flags1::Voiced | Flags1::DipthongYX | Flags1::Vowel                            ; = $a4
    /*  8: 'AE' */ .byte Flags1::Voiced | Flags1::DipthongYX | Flags1::Vowel                            ; = $a4
    /*  9: 'AA' */ .byte Flags1::Voiced | Flags1::DipthongYX | Flags1::Vowel                            ; = $a4
    /* 10: 'AH' */ .byte Flags1::Voiced | Flags1::DipthongYX | Flags1::Vowel                            ; = $a4
    /* 11: 'AO' */ .byte Flags1::Voiced | Flags1::Vowel                                                 ; = $84
    /* 12: 'UH' */ .byte Flags1::Voiced | Flags1::Vowel                                                 ; = $84
    /* 13: 'AX' */ .byte Flags1::Voiced | Flags1::DipthongYX | Flags1::Vowel                            ; = $a4
    /* 14: 'IX' */ .byte Flags1::Voiced | Flags1::DipthongYX | Flags1::Vowel                            ; = $a4
    /* 15: 'ER' */ .byte Flags1::Voiced | Flags1::Vowel                                                 ; = $84
    /* 16: 'UX' */ .byte Flags1::Voiced | Flags1::Vowel                                                 ; = $84
    /* 17: 'OH' */ .byte Flags1::Voiced | Flags1::Vowel                                                 ; = $84
    /* 18: 'RX' */ .byte Flags1::Voiced | Flags1::Vowel                                                 ; = $84
    /* 19: 'LX' */ .byte Flags1::Voiced | Flags1::Vowel                                                 ; = $84
    /* 20: 'WX' */ .byte Flags1::Voiced | Flags1::Vowel                                                 ; = $84
    /* 21: 'YX' */ .byte Flags1::Voiced | Flags1::Vowel                                                 ; = $84
    /* 22: 'WH' */ .byte Flags1::Voiced | Flags1::Consonant                                             ; = $44
    /* 23: 'R*' */ .byte Flags1::Voiced | Flags1::Consonant                                             ; = $44
    /* 24: 'L*' */ .byte Flags1::Voiced | Flags1::Consonant                                             ; = $44
    /* 25: 'W*' */ .byte Flags1::Voiced | Flags1::Consonant                                             ; = $44
    /* 26: 'Y*' */ .byte Flags1::Voiced | Flags1::Consonant                                             ; = $44
    /* 27: 'M*' */ .byte Flags1::Voiced | Flags1::Unknown | Flags1::Consonant                           ; = $4c
    /* 28: 'N*' */ .byte Flags1::Voiced | Flags1::Unknown | Flags1::Consonant                           ; = $4c
    /* 29: 'NX' */ .byte Flags1::Voiced | Flags1::Unknown | Flags1::Consonant                           ; = $4c
    /* 30: 'DX' */ .byte Flags1::Unknown | Flags1::Consonant                                            ; = $48
    /* 31: 'Q*' */ .byte Flags1::Voiced | Flags1::Unknown | Flags1::Consonant                           ; = $4c
    /* 32: 'S*' */ .byte Flags1::Consonant                                                              ; = $40
    /* 33: 'SH' */ .byte Flags1::Consonant                                                              ; = $40
    /* 34: 'F*' */ .byte Flags1::Consonant                                                              ; = $40
    /* 35: 'TH' */ .byte Flags1::Consonant                                                              ; = $40
    /* 36: '/H' */ .byte Flags1::Consonant                                                              ; = $40
    /* 37: '/X' */ .byte Flags1::Consonant                                                              ; = $40
    /* 38: 'Z*' */ .byte Flags1::Voiced | Flags1::Consonant                                             ; = $44
    /* 39: 'ZH' */ .byte Flags1::Voiced | Flags1::Consonant                                             ; = $44
    /* 40: 'V*' */ .byte Flags1::Voiced | Flags1::Consonant                                             ; = $44
    /* 41: 'DH' */ .byte Flags1::Voiced | Flags1::Consonant                                             ; = $44
    /* 42: 'CH' */ .byte Flags1::Unknown | Flags1::Consonant                                            ; = $48
    /* 43: '**' */ .byte Flags1::Consonant                                                              ; = $40
    /* 44: 'J*' */ .byte Flags1::Voiced | Flags1::Unknown | Flags1::Consonant                           ; = $4c
    /* 45: '**' */ .byte Flags1::Voiced | Flags1::Consonant                                             ; = $44
    /* 46: '**' */ .byte Flags1::None                                                                   ; = $00
    /* 47: '**' */ .byte Flags1::None                                                                   ; = $00
    /* 48: 'EY' */ .byte Flags1::Voiced | Flags1::Dipthong | Flags1::DipthongYX | Flags1::Vowel         ; = $b4
    /* 49: 'AY' */ .byte Flags1::Voiced | Flags1::Dipthong | Flags1::DipthongYX | Flags1::Vowel         ; = $b4
    /* 50: 'OY' */ .byte Flags1::Voiced | Flags1::Dipthong | Flags1::DipthongYX | Flags1::Vowel         ; = $b4
    /* 51: 'AW' */ .byte Flags1::Voiced | Flags1::Dipthong | Flags1::Vowel                              ; = $94
    /* 52: 'OW' */ .byte Flags1::Voiced | Flags1::Dipthong | Flags1::Vowel                              ; = $94
    /* 53: 'UW' */ .byte Flags1::Voiced | Flags1::Dipthong | Flags1::Vowel                              ; = $94
    /* 54: 'B*' */ .byte Flags1::StopConsonant | Flags1::Voiced | Flags1::Unknown | Flags1::Consonant   ; = $4e
    /* 55: '**' */ .byte Flags1::StopConsonant | Flags1::Voiced | Flags1::Unknown | Flags1::Consonant   ; = $4e
    /* 56: '**' */ .byte Flags1::StopConsonant | Flags1::Voiced | Flags1::Unknown | Flags1::Consonant   ; = $4e
    /* 57: 'D*' */ .byte Flags1::StopConsonant | Flags1::Voiced | Flags1::Unknown | Flags1::Consonant   ; = $4e
    /* 58: '**' */ .byte Flags1::StopConsonant | Flags1::Voiced | Flags1::Unknown | Flags1::Consonant   ; = $4e
    /* 59: '**' */ .byte Flags1::StopConsonant | Flags1::Voiced | Flags1::Unknown | Flags1::Consonant   ; = $4e
    /* 60: 'G*' */ .byte Flags1::StopConsonant | Flags1::Voiced | Flags1::Unknown | Flags1::Consonant   ; = $4e
    /* 61: '**' */ .byte Flags1::StopConsonant | Flags1::Voiced | Flags1::Unknown | Flags1::Consonant   ; = $4e
    /* 62: '**' */ .byte Flags1::StopConsonant | Flags1::Voiced | Flags1::Unknown | Flags1::Consonant   ; = $4e
    /* 63: 'GX' */ .byte Flags1::StopConsonant | Flags1::Voiced | Flags1::Unknown | Flags1::Consonant   ; = $4e
    /* 64: '**' */ .byte Flags1::StopConsonant | Flags1::Voiced | Flags1::Unknown | Flags1::Consonant   ; = $4e
    /* 65: '**' */ .byte Flags1::StopConsonant | Flags1::Voiced | Flags1::Unknown | Flags1::Consonant   ; = $4e
    /* 66: 'P*' */ .byte Flags1::Plosive | Flags1::StopConsonant | Flags1::Unknown | Flags1::Consonant  ; = $4b
    /* 67: '**' */ .byte Flags1::Plosive | Flags1::StopConsonant | Flags1::Unknown | Flags1::Consonant  ; = $4b
    /* 68: '**' */ .byte Flags1::Plosive | Flags1::StopConsonant | Flags1::Unknown | Flags1::Consonant  ; = $4b
    /* 69: 'T*' */ .byte Flags1::Plosive | Flags1::StopConsonant | Flags1::Unknown | Flags1::Consonant  ; = $4b
    /* 70: '**' */ .byte Flags1::Plosive | Flags1::StopConsonant | Flags1::Unknown | Flags1::Consonant  ; = $4b
    /* 71: '**' */ .byte Flags1::Plosive | Flags1::StopConsonant | Flags1::Unknown | Flags1::Consonant  ; = $4b
    /* 72: 'K*' */ .byte Flags1::Plosive | Flags1::StopConsonant | Flags1::Unknown | Flags1::Consonant  ; = $4b
    /* 73: '**' */ .byte Flags1::Plosive | Flags1::StopConsonant | Flags1::Unknown | Flags1::Consonant  ; = $4b
    /* 74: '**' */ .byte Flags1::Plosive | Flags1::StopConsonant | Flags1::Unknown | Flags1::Consonant  ; = $4b
    /* 75: 'KX' */ .byte Flags1::Plosive | Flags1::StopConsonant | Flags1::Unknown | Flags1::Consonant  ; = $4b
    /* 76: '**' */ .byte Flags1::Plosive | Flags1::StopConsonant | Flags1::Unknown | Flags1::Consonant  ; = $4b
    /* 77: '**' */ .byte Flags1::Plosive | Flags1::StopConsonant | Flags1::Unknown | Flags1::Consonant  ; = $4b

.enum Flags2
    None            = $00
    Punctuation     = $01
    ; NotUsed         = $02
    Alveolar        = $04
    Nasal           = $08
    LiquidConsonant = $10
    Fricative       = $20
    Consonant       = $40
    Vowel           = $80
.endenum

L9fda_flags2:
    /*  0: '*'  */ .byte Flags2::Vowel                                           ; = $80
    /*  1: '.*' */ .byte Flags2::Punctuation | Flags2::Consonant | Flags2::Vowel ; = $c1
    /*  2: '?*' */ .byte Flags2::Punctuation | Flags2::Consonant | Flags2::Vowel ; = $c1
    /*  3: ',*' */ .byte Flags2::Punctuation | Flags2::Consonant | Flags2::Vowel ; = $c1
    /*  4: '-*' */ .byte Flags2::Punctuation | Flags2::Consonant | Flags2::Vowel ; = $c1
    /*  5: 'IY' */ .byte Flags2::None                                            ; = $00
    /*  6: 'IH' */ .byte Flags2::None                                            ; = $00
    /*  7: 'EH' */ .byte Flags2::None                                            ; = $00
    /*  8: 'AE' */ .byte Flags2::None                                            ; = $00
    /*  9: 'AA' */ .byte Flags2::None                                            ; = $00
    /* 10: 'AH' */ .byte Flags2::None                                            ; = $00
    /* 11: 'AO' */ .byte Flags2::None                                            ; = $00
    /* 12: 'UH' */ .byte Flags2::None                                            ; = $00
    /* 13: 'AX' */ .byte Flags2::None                                            ; = $00
    /* 14: 'IX' */ .byte Flags2::None                                            ; = $00
    /* 15: 'ER' */ .byte Flags2::None                                            ; = $00
    /* 16: 'UX' */ .byte Flags2::None                                            ; = $00
    /* 17: 'OH' */ .byte Flags2::None                                            ; = $00
    /* 18: 'RX' */ .byte Flags2::None                                            ; = $00
    /* 19: 'LX' */ .byte Flags2::None                                            ; = $00
    /* 20: 'WX' */ .byte Flags2::None                                            ; = $00
    /* 21: 'YX' */ .byte Flags2::None                                            ; = $00
    /* 22: 'WH' */ .byte Flags2::None                                            ; = $00
    /* 23: 'R*' */ .byte Flags2::LiquidConsonant                                 ; = $10
    /* 24: 'L*' */ .byte Flags2::LiquidConsonant                                 ; = $10
    /* 25: 'W*' */ .byte Flags2::LiquidConsonant                                 ; = $10
    /* 26: 'Y*' */ .byte Flags2::LiquidConsonant                                 ; = $10
    /* 27: 'M*' */ .byte Flags2::Nasal                                           ; = $08
    /* 28: 'N*' */ .byte Flags2::Alveolar | Flags2::Nasal                        ; = $0c
    /* 29: 'NX' */ .byte Flags2::Nasal                                           ; = $08
    /* 30: 'DX' */ .byte Flags2::Alveolar                                        ; = $04
    /* 31: 'Q*' */ .byte Flags2::Consonant                                       ; = $40
    /* 32: 'S*' */ .byte Flags2::Alveolar | Flags2::Fricative                    ; = $24
    /* 33: 'SH' */ .byte Flags2::Fricative                                       ; = $20
    /* 34: 'F*' */ .byte Flags2::Fricative                                       ; = $20
    /* 35: 'TH' */ .byte Flags2::Alveolar | Flags2::Fricative                    ; = $24
    /* 36: '/H' */ .byte Flags2::None                                            ; = $00
    /* 37: '/X' */ .byte Flags2::None                                            ; = $00
    /* 38: 'Z*' */ .byte Flags2::Alveolar | Flags2::Fricative                    ; = $24
    /* 39: 'ZH' */ .byte Flags2::Fricative                                       ; = $20
    /* 40: 'V*' */ .byte Flags2::Fricative                                       ; = $20
    /* 41: 'DH' */ .byte Flags2::Alveolar | Flags2::Fricative                    ; = $24
    /* 42: 'CH' */ .byte Flags2::Fricative                                       ; = $20
    /* 43: '**' */ .byte Flags2::Fricative                                       ; = $20
    /* 44: 'J*' */ .byte Flags2::None                                            ; = $00
    /* 45: '**' */ .byte Flags2::Fricative                                       ; = $20
    /* 46: '**' */ .byte Flags2::None                                            ; = $00
    /* 47: '**' */ .byte Flags2::None                                            ; = $00
    /* 48: 'EY' */ .byte Flags2::None                                            ; = $00
    /* 49: 'AY' */ .byte Flags2::None                                            ; = $00
    /* 50: 'OY' */ .byte Flags2::None                                            ; = $00
    /* 51: 'AW' */ .byte Flags2::None                                            ; = $00
    /* 52: 'OW' */ .byte Flags2::None                                            ; = $00
    /* 53: 'UW' */ .byte Flags2::None                                            ; = $00
    /* 54: 'B*' */ .byte Flags2::None                                            ; = $00
    /* 55: '**' */ .byte Flags2::None                                            ; = $00
    /* 56: '**' */ .byte Flags2::None                                            ; = $00
    /* 57: 'D*' */ .byte Flags2::Alveolar                                        ; = $04
    /* 58: '**' */ .byte Flags2::Alveolar                                        ; = $04
    /* 59: '**' */ .byte Flags2::Alveolar                                        ; = $04
    /* 60: 'G*' */ .byte Flags2::None                                            ; = $00
    /* 61: '**' */ .byte Flags2::None                                            ; = $00
    /* 62: '**' */ .byte Flags2::None                                            ; = $00
    /* 63: 'GX' */ .byte Flags2::None                                            ; = $00
    /* 64: '**' */ .byte Flags2::None                                            ; = $00
    /* 65: '**' */ .byte Flags2::None                                            ; = $00
    /* 66: 'P*' */ .byte Flags2::None                                            ; = $00
    /* 67: '**' */ .byte Flags2::None                                            ; = $00
    /* 68: '**' */ .byte Flags2::None                                            ; = $00
    /* 69: 'T*' */ .byte Flags2::Alveolar                                        ; = $04
    /* 70: '**' */ .byte Flags2::Alveolar                                        ; = $04
    /* 71: '**' */ .byte Flags2::Alveolar                                        ; = $04
    /* 72: 'K*' */ .byte Flags2::None                                            ; = $00
    /* 73: '**' */ .byte Flags2::None                                            ; = $00
    /* 74: '**' */ .byte Flags2::None                                            ; = $00
    /* 75: 'KX' */ .byte Flags2::None                                            ; = $00
    /* 76: '**' */ .byte Flags2::None                                            ; = $00
    /* 77: '**' */ .byte Flags2::None                                            ; = $00

; TODO: Inline?
.proc La028_SaveAXY
    sta mem63
    stx mem62
    sty mem61
    rts
.endproc

; TODO: Inline?
.proc La02f_RestoreAXY
    lda mem63
    ldx mem62
    ldy mem61
    rts
.endproc

.proc La036_Insert
    jsr La028_SaveAXY
    ldx #$ff                            ; X = $fe (after dex below)
    ldy #$00                            ; Y = $ff (after dey below)
La03d:                                  ; loop {
    dex
    dey
    lda L9be0_phonemeIndex,x
    sta L9be0_phonemeIndex,y            ;   phonemeIndex[i + 1] = phonemeIndex[i];
    lda L9ce0_phonemeLength,x
    sta L9ce0_phonemeLength,y           ;   phonemeLength[i + 1] = phonemeLength[i];
    lda L9de0_stress,x
    sta L9de0_stress,y                  ;   stress[i + 1] = stress[i];
    cpx mem57
    bne La03d                           ; } while (X != mem57)
    lda mem60
    sta L9be0_phonemeIndex,x            ; phonemeIndex[position] = mem60
    lda mem59
    sta L9ce0_phonemeLength,x           ; phonemeLength[position] = mem59
    lda mem58
    sta L9de0_stress,x                  ; stress[position] = mem58
    jsr La02f_RestoreAXY
    rts
.endproc

; The input[] buffer contains a string of phonemes and stress markers along
; the lines of:
;
;     DHAX KAET IHZ AH5GLIY. <0x9B>
;
; The byte 0x9B marks the end of the buffer. Some phonemes are 2 bytes
; long, such as "DH" and "AX". Others are 1 byte long, such as "T" and "Z".
; There are also stress markers, such as "5" and ".".
;
; The first character of the phonemes are stored in the table signInputTable1[].
; The second character of the phonemes are stored in the table signInputTable2[].
; The stress characters are arranged in low to high stress order in stressInputTable[].
;
; The following process is used to parse the input[] buffer:
;
; Repeat until the <0x9B> character is reached:
;
;        First, a search is made for a 2 character match for phonemes that do not
;        end with the '*' (wildcard) character. On a match, the index of the phoneme
;        is added to phonemeIndex[] and the buffer position is advanced 2 bytes.
;
;        If this fails, a search is made for a 1 character match against all
;        phoneme names ending with a '*' (wildcard). If this succeeds, the
;        phoneme is added to phonemeIndex[] and the buffer position is advanced
;        1 byte.
;
;        If this fails, search for a 1 character match in the stressInputTable[].
;        If this succeeds, the stress value is placed in the last stress[] table
;        at the same index of the last added phoneme, and the buffer position is
;        advanced by 1 byte.
;
;        If this fails, return a 0.
;
; On success:
;
;    1. phonemeIndex[] will contain the index of all the phonemes.
;    2. The last index in phonemeIndex[] will be 255.
;    3. stress[] will contain the stress value for each phoneme

; input[] holds the string of phonemes, each two bytes wide
; signInputTable1[] holds the first character of each phoneme
; signInputTable2[] holds te second character of each phoneme
; phonemeIndex[] holds the indexes of the phonemes after parsing input[]
;
; The parser scans through the input[], finding the names of the phonemes
; by searching signInputTable1[] and signInputTable2[]. On a match, it
; copies the index of the phoneme into the phonemeIndexTable[].
;
; The character <0x9B> marks the end of text in input[]. When it is reached,
; the index 255 is placed at the end of the phonemeIndexTable[], and the
; function returns with a 1 indicating success.
.proc La068_Parser1
    ldx #$00                            ; X = 0
    txa                                 ; A = 0
    tay                                 ; Y = 0
    sta mem66_pos                       ; mem66_pos = 0

    ; Clear the stress table
La06e:                                  ; do {
    sta L9de0_stress,y                  ;   L9de0_stress[Y] = 0
    iny                                 ;   Y++
    cpy #$ff
    bne La06e                           ; } while (Y != $ff)

    ; This code matches the phoneme letters to the table
La076_next_input:                       ; while (input[X] != $9b) {
    ; Get the first character from the phoneme buffer
    lda L9a15_input,x

    ; Test for $9b end of line marker
    cmp #$9b
    beq La0eb

    sta mem65_sign1                     ;   sign1 = input[X]

    ; Get the next character from the buffer
    inx                                 ;   X++
    lda L9a15_input,x
    sta mem64_sign2                     ;   sign2 = input[X]

    ; Now sign1 = first character of phoneme, and sign2 = second character of phoneme

    ; Try to match phonemes on two two-character name
    ; Ignore phonemes in table ending with wildcards

    ; Set index to 0
    ldy #$00                            ;   Y = 0
La087:                                  ;   do {
    ; First character matches?
    lda L9eea_signInputTable1,y
    cmp mem65_sign1
    bne La099                           ;     if (signInputTable1[Y] != sign1) continue;

    ; Not a special and matches second character?
    lda L9f3b_signInputTable2,y
    cmp #$2a
    beq La099                           ;     if (signInputTable2[Y] == '*') continue;
    cmp mem64_sign2
    beq La0a0                           ;     if (signInputTable2[Y] == sign2) break;
La099:
    iny
    cpy #$51
    bne La087                           ;   } while (++Y < $51)
    beq La0ac                           ; goto (near-jmp: A always == 0)

; Found match
; TODO: Pre-increment X and jmp to La0c3?
La0a0:
    ; Store the index of the phoneme into the 'phomeneIndex[]' table
    tya
    ldy mem66_pos
    sta L9be0_phonemeIndex,y            ;   phonemeIndex[pos] = Y

    ; Advance the pointer to the 'phonemeindex[]'
    inc mem66_pos                       ;   mem66_pos++
    ; Advance the pointer to the 'input[]'
    inx                                 ;   X++

    ; Continue parsing
    jmp La076_next_input                ;   continue

    ; Reached end of table without an exact (2 character) match.
    ; This time, search for a 1 character match against the wildcards

    ; Reset the index to point to the start of the phoneme name table
La0ac:
    ldy #$00                            ; Y = 0
La0ae:                                  ; do {
    ; Does the phoneme in the table end with '*'?
    lda L9f3b_signInputTable2,y
    cmp #$2a
    bne La0bc                           ;   if (signInputTable2[Y] != '*') not wildcard

    ; Does the first character match the first letter of the phoneme
    lda L9eea_signInputTable1,y
    cmp mem65_sign1
    beq La0c3                           ;   if (signInputTable1[Y] == sign1) found match

    ; Not wildcard match
La0bc:
    iny
    cpy #$51
    bne La0ae                           ; } while (++Y != $51)

    beq La0ce                           ; goto find_stress

La0c3:
    ; Save the position and move ahead
    tya
    ldy mem66_pos
    sta L9be0_phonemeIndex,y            ; phonemeIndex[mem66_pos] = Y

    ; Advance the pointer
    inc mem66_pos                       ; mem66_pos++

    ; Continue through the loop
    jmp La076_next_input                ; continue

    ; Failed to match with a wildcard. Assume this is a stress
    ; character. Search through the stress table
La0ce:
    lda mem65_sign1

    ; Set index to position 8 (end of stress table)
    ldy #$08

    ; Walk back through table looking for a match
    ; TODO: could be simple calculation instead of looping over table?
La0d2:                                  ; do {
    cmp L9ee0_stressInputTable,y
    beq La0e1                           ;   if (sign1 == stressInputTable[Y]) goto La0e1
    dey                                 ;   Y--
    bne La0d2                           ; } while (Y != 0)
    stx L9a14_error_pos
    jsr Sa43b_ErrorBeep                 ; TODO: tail call?
    rts

    ; Set the stress for the prior phoneme
La0e1:
    tya
    ldy mem66_pos
    dey
    sta L9de0_stress,y                  ; stress[position - 1] = Y

    jmp La076_next_input                ; continue

La0eb:
    lda #$ff
    ldy mem66_pos
    sta L9be0_phonemeIndex,y
    rts
.endproc

; Change phonemeLength depedendent on stress
.proc Sa0f3_SetPhonemeLength
    ldy #$00                                ; position = 0
La0f5:
    lda L9be0_phonemeIndex,y                ; A = phonemeIndex[position]
    cmp #$ff
    beq La117                               ; if (A == $ff) goto La117 (done)
    tax                                     ; X = A
    lda L9de0_stress,y                      ; A = stress[position]
    beq La10d                               ; if (A == 0) goto La10d
    bmi La10d                               ; if (A & 128) goto La10d
    lda Lb1e0_phonemeStressedLength,x
    sta L9ce0_phonemeLength,y               ; phonemeLength[position] = phonemeLengthTable2[phonemeIndex[position]];
    jmp La113

La10d:
    lda Lb230_phonemeNormalLength,x
    sta L9ce0_phonemeLength,y               ; phonemeLength[position] = phonemeLengthTable1[phonemeIndex[position]];
La113:
    iny                                     ; Y++
    jmp La0f5                               ; continue

La117:
    rts
.endproc

.proc Sa118_Code41240
    lda #$00
    sta mem66_pos                           ; pos = 0

La11c:                                      ; loop {
    ldx mem66_pos
    lda L9be0_phonemeIndex,x
    cmp #$ff
    bne La126                               ;   if (phonemeIndex[pos] == $ff) return
    rts

La126:
    sta mem60                               ;   A = index = phonemeIndex[pos]
    tay
    lda L9f8c_flags,y
    tay                                     ;   Y = flags[index]
    and #Flags1::StopConsonant
    bne La136                               ;   if (!(flags[index] & 2)) {
    inc mem66_pos                           ;     pos++
    jmp La11c                               ;     continue;
                                            ;   }
La136:
    tya
    and #Flags1::Plosive
    bne La167                               ;   if (!(flags[index] & 1)) {

    ; TODO: Can this be merged with La188?

    inc mem60                               ;     mem60 = index + 1
    ldy mem60
    lda L9de0_stress,x
    sta mem58                               ;     mem58 = stress[pos]
    lda Lb230_phonemeNormalLength,y
    sta mem59                               ;     mem59 = Lb230_phonemeNormalLength[index + 1]
    inx
    stx mem57                               ;     mem57 = pos + 1
    jsr La036_Insert                        ;     Insert()
    inc mem60                               ;     mem60 = index + 2
    ldy mem60
    lda Lb230_phonemeNormalLength,y
    sta mem59                               ;     mem59 = phonemeLengthTable[index + 2]
    inx
    stx mem57                               ;     mem57 = pos + 2
    jsr La036_Insert                        ;     Insert()
    inc mem66_pos
    inc mem66_pos
    inc mem66_pos                           ;     pos += 3
    jmp La11c                               ;     continue
                                            ;   }
La167:                                      ;   do {
    inx                                     ;     X++
    lda L9be0_phonemeIndex,x                ;     A = phonemeIndex[X];
    beq La167                               ;   } while (A == 0)
    sta mem56                               ;   mem56 = A
    cmp #$ff
    bne La176                               ;   if (A != $ff) {
    jmp La188

La176:
    tay
    lda L9f8c_flags,y
    and #Flags1::Unknown
    bne La1b0                               ;     if (flags[A] & $8) goto La1b0

    ; TODO: Understand why we couldn't use the ::Unknown flag for '/H' and '/X' as well?
    lda mem56
    cmp #$24
    beq La1b0                               ;     if (A == '/H') goto La1b0
    cmp #$25
    beq La1b0                               ;     if (A == '/X') goto La1b0
La188:
    ldx mem66_pos
    lda L9de0_stress,x
    sta mem58                               ;     mem58 = stress[pos]
    inx
    stx mem57                               ;     mem57 = pos + 1
    ldx mem60
    inx
    stx mem60                               ;     mem60 = index + 1
    lda Lb230_phonemeNormalLength,x
    sta mem59                               ;     mem59 = phonemeLengthTable[index + 1]
    jsr La036_Insert                        ;     Insert()
    inc mem57                               ;     mem57 = pos + 2
    inx
    stx mem60                               ;     mem60 = index + 2
    lda Lb230_phonemeNormalLength,x
    sta mem59                               ;     mem59 = phonemeLengthTable[index + 2]
    jsr La036_Insert                        ;     Insert()
    inc mem66_pos
    inc mem66_pos                           ;     pos += 2
La1b0:
    inc mem66_pos                           ;     pos++
    jmp La11c                               ;     continue
.endproc                                    ; } }

; Rewrites the phonemes using the following rules:
;
;       <DIPHTONG ENDING WITH WX> -> <DIPHTONG ENDING WITH WX> WX
;       <DIPHTONG NOT ENDING WITH WX> -> <DIPHTONG NOT ENDING WITH WX> YX
;       UL -> AX L
;       UM -> AX M
;       <STRESSED VOWEL> <SILENCE> <STRESSED VOWEL> -> <STRESSED VOWEL> <SILENCE> Q <VOWEL>
;       T R -> CH R
;       D R -> J R
;       <VOWEL> R -> <VOWEL> RX
;       <VOWEL> L -> <VOWEL> LX
;       G S -> G Z
;       K <VOWEL OR DIPHTONG NOT ENDING WITH IY> -> KX <VOWEL OR DIPHTONG NOT ENDING WITH IY>
;       G <VOWEL OR DIPHTONG NOT ENDING WITH IY> -> GX <VOWEL OR DIPHTONG NOT ENDING WITH IY>
;       S P -> S B
;       S T -> S D
;       S K -> S G
;       S KX -> S GX
;       <ALVEOLAR> UW -> <ALVEOLAR> UX
;       CH -> CH CH' (CH requires two phonemes to represent it)
;       J -> J J' (J requires two phonemes to represent it)
;       <UNSTRESSED VOWEL> T <PAUSE> -> <UNSTRESSED VOWEL> DX <PAUSE>
;       <UNSTRESSED VOWEL> D <PAUSE>  -> <UNSTRESSED VOWEL> DX <PAUSE>
.proc Sa1b5_Parser2
    lda #$00
    sta mem66_pos                       ; pos = 0

    ; Loop through phonemes
La1b9:
    ; Set X to the current position
    ldx mem66_pos                       ; X = pos

    ; Get the phoneme at the current position
    lda L9be0_phonemeIndex,x            ; A = phonemeIndex[pos];

    ; Is phoneme pause?
    bne La1c5                           ; if (A == 0) {
    inc mem66_pos                       ;   pos++
    jmp La1b9                           ;   continue
                                        ; }
    ; If end of phonemes flag reached, exit routine
La1c5:
    cmp #$ff
    bne La1ca                           ; if (A = $ff)
    rts                                 ;   return

La1ca:
    ; Copy the current phoneme index to Y
    tay

    ; RULE:
    ;       <DIPHTONG ENDING WITH WX> -> <DIPHTONG ENDING WITH WX> WX
    ;       <DIPHTONG NOT ENDING WITH WX> -> <DIPHTONG NOT ENDING WITH WX> YX
    ; Example: OIL, COW

    ; Check for DIPHTONG
    lda L9f8c_flags,y
    and #Flags1::Dipthong
    beq La1f1                           ; if (!(flags[A] & $10)) skip

    ; Not a DIPTHONG. Get the stress
    lda L9de0_stress,x
    sta mem58                           ; mem58 = stress[pos];
    inx
    stx mem57                           ; mem57 = pos + 1

    ; If ends with IY, use YX, else use WX
    lda L9f8c_flags,y
    and #Flags1::DipthongYX
    beq La1ed                           ; if (flags[Y] & $20)
    lda #$15                            ;   A = 'YX'

    ; Insert at WX or YX following, copying the stress
La1e3:
    sta mem60                           ; mem60 = A ('WX' or 'YX')
    jsr La036_Insert                    ; Insert()
    ldx mem66_pos                       ; X = pos
    jmp La315                           ; continue with ALVEOLAR rules

La1ed:
    lda #$14                            ; else A = 'WX'
    bne La1e3                           ; Insert WX and continue with ALVEOLAR rules

La1f1:
    ; Get phoneme
    lda L9be0_phonemeIndex,x

    ; RULE:
    ;       UL -> AX L
    ; Example: MEDDLE

    cmp #$4e
    bne La20f                           ; if (A != 'UL') skip;
    lda #$18                            ; Change 'UL' to 'AX L'
La1fa:
    sta mem60                           ; mem60 = A

    ; Get current phoneme stress
    lda L9de0_stress,x
    sta mem58                           ; mem58 = stress[X]

    ; Change 'UL' to 'AX'
    lda #$0d
    sta L9be0_phonemeIndex,x            ; phonemeIndex[X] = 'AX'
    inx
    stx mem57                           ; mem57 = X + 1
    jsr La036_Insert
    jmp La396                           ; Move to next phoneme

    ; RULE:
    ;       UM -> AX M
    ; Example: ASTRONOMY
La20f:
    cmp #$4f
    bne La217                           ; if (A != 'UM') skip
    lda #$1b
    bne La1fa                           ; Change 'UM' to 'AX M'

    ; RULE:
    ;       UN -> AX N
    ; Example: FUNCTION
La217:
    cmp #$50
    bne La21f                           ; if (A != 'UN') skip
    lda #$1c
    bne La1fa                           ; Change 'UN' to 'AX N'

    ; RULE:
    ;       <STRESSED VOWEL> <SILENCE> <STRESSED VOWEL> -> <STRESSED VOWEL> <SILENCE> Q <VOWEL>
    ; EXAMPLE: AWAY EIGHT
La21f:
    tay

    lda L9f8c_flags,y                   ; Skip if not a vowel
    and #Flags1::Vowel
    beq La252                           ; if (!(flags[A] & 128)) skip

    lda L9de0_stress,x                  ; Skip if not stressed
    beq La252                           ; if (!stress[X]) skip

    inx                                 ; Skip if following phoneme is not a pause
    lda L9be0_phonemeIndex,x
    bne La252                           ; if (phonemeIndex[X + 1] != 0) skip
    inx
    ldy L9be0_phonemeIndex,x            ; Skip if phoneme after pause is not a vowel
    lda L9f8c_flags,y
    and #Flags1::Vowel
    beq La252                           ; if (!(phonemeIndex[X + 2] & 0x80)) skip
    lda L9de0_stress,x                  ; Skip if phoneme after pause is not stressed
    beq La252                           ; if (!(stress[X + 2])) skip
    stx mem57                           ; mem57 = stress[X + 2]
    lda #$00
    sta mem58                           ; mem58 = 0
    lda #$1f
    sta mem60                           ; mem60 = 'Q'
    jsr La036_Insert                    ; Insert a glottal stop
    jmp La396                           ; Move to next phoneme

    ; RULES FOR PHONEMES BEFORE R
    ;        T R -> CH R
    ; Example: TRACK
La252:
    ldx mem66_pos
    lda L9be0_phonemeIndex,x
    cmp #$17
    bne La28b                           ; if (phonemeIndex[pos] != 'R') skip
    dex                                 ; X = pos - 1
    lda L9be0_phonemeIndex,x            ; A = phonemeIndex[pos - 1]
    cmp #$45
    bne La26b                           ; if (phonemeIndex[pos - 1] == 'T') skip
    lda #$2a
    sta L9be0_phonemeIndex,x            ; Change 'T' to 'CH'
    jmp La333                           ; Change 'CH' to 'CH CH'

    ; RULES FOR PHONEMES BEFORE R
    ;        D R -> J R
    ; Example: DRY
La26b:
    cmp #$39
    bne La277                           ; if (phonemeIndex[pos - 1] != 'D') skip
    lda #$2c
    sta L9be0_phonemeIndex,x            ; Change 'D' to 'J'
    jmp La33c                           ; Change 'J' to 'J J'

    ; RULES FOR PHONEMES BEFORE R
    ;        <VOWEL> R -> <VOWEL> RX
    ; Example: ART
La277:
    tay                                 ; Y = (phonemeIndex[pos - 1]
    inx                                 ; X = pos
    lda L9f8c_flags,y                   ; If vowel flag is set change R to RX
    and #Flags1::Vowel
    bne La283                           ; if (flags[Y] & $80) change 'R' to 'RX'
    jmp La396

La283:
    lda #$12
    sta L9be0_phonemeIndex,x            ; phonemeIndex[pos] = 'RX'
    jmp La396

    ; RULE:
    ;       <VOWEL> L -> <VOWEL> LX
    ; Example: ALL
La28b:
    cmp #$18
    bne La2a6                           ; if (phonemeIndex[pos] != 'L') skip

    ; If prior phoneme does not have VOWEL flag set, move to next phoneme
    dex
    ldy L9be0_phonemeIndex,x
    inx
    lda L9f8c_flags,y
    and #Flags1::Vowel
    bne La29e
    jmp La396                           ; if (!(flags[phonemeIndex[pos - 1]] & 128)) continue

La29e:
    ; Prior phoneme has VOWEL flag set, so change L to LX and move to next phoneme
    lda #$13
    sta L9be0_phonemeIndex,x            ; phonemeIndex[X] = 'LX'
    jmp La396                           ; continue

    ; RULE:
    ;       G S -> G Z
    ;
    ; TODO: Can't get to fire:
    ;       1. The G -> GX rule intervenes
    ;       2. Reciter already replaces GS -> GZ
La2a6:
    cmp #$20
    bne La2be                           ; if (A != 'S') skip
    dex
    lda L9be0_phonemeIndex,x
    cmp #$3c
    beq La2b5                           ; if (phonemeIndex[pos - 1] != 'G')
    jmp La396                           ;   continue

La2b5:                                  ; Replace S with Z and move on
    inx
    lda #$26
    sta L9be0_phonemeIndex,x            ; phonemeIndex[pos] = 'Z'
    jmp La396                           ; continue

    ; RULE:
    ;       K <VOWEL OR DIPHTONG NOT ENDING WITH IY> -> KX <VOWEL OR DIPHTONG NOT ENDING WITH IY>
    ; Example: COW
La2be:
    cmp #$48
    bne La2d9                           ; if (A != 'K') skip
    inx
    ldy L9be0_phonemeIndex,x            ; Y = phonemeIndex[pos + 1]
    dex
    lda L9f8c_flags,y
    and #Flags1::DipthongYX
    beq La2d1                           ; if (!(flags[Y] & $20)) replace with KX
    jmp La2f4

La2d1:
    lda #$4b
    sta L9be0_phonemeIndex,x            ; phonemeIndex[pos] = 'KX'
    jmp La2f4

    ; RULE:
    ;       G <VOWEL OR DIPHTONG NOT ENDING WITH IY> -> GX <VOWEL OR DIPHTONG NOT ENDING WITH IY>
    ; Example: GO
La2d9:
    cmp #$3c
    bne La2f4                           ; if (A != 'G') skip
    ; If diphtong ending with YX, continue processing next phoneme
    inx
    ldy L9be0_phonemeIndex,x            ; Y = phonemeIndex[pos + 1]
    dex
    lda L9f8c_flags,y
    and #Flags1::DipthongYX
    beq La2ec                           ; if (flags[Y] & $20) change 'G' to 'GX'
    jmp La396                           ; else continue

La2ec:
    lda #$3f
    sta L9be0_phonemeIndex,x            ; phonemeIndex[pos] = 'GX'
    jmp La396                           ; continue

    ; RULE:
    ;      S P -> S B
    ;      S T -> S D
    ;      S K -> S G
    ;      S KX -> S GX
    ; Examples: SPY, STY, SKY, SCOWL
La2f4:
    ldy L9be0_phonemeIndex,x            ; Y = phonemeIndex[pos]
    lda L9f8c_flags,y
    and #Flags1::Plosive
    beq La315                           ; if (!(flags[Y] & $1)) skip rule
    dex
    lda L9be0_phonemeIndex,x            ; A = phonemeIndex[pos - 1]
    inx
    cmp #$20
    beq La30b                           ; if (A != 'S') {
    tya                                 ;   A = Y
    jmp La354                           ;   skip several rules
                                        ; }
La30b:
    sec
    tya
    sbc #$0c
    sta L9be0_phonemeIndex,x            ; phonemeIndex[pos] = Y - 12
    jmp La396                           ; continue

    ; RULE:
    ;      <ALVEOLAR> UW -> <ALVEOLAR> UX
    ;
    ; Example: NEW, DEW, SUE, ZOO, THOO, TOO
La315:
    lda L9be0_phonemeIndex,x            ; A = phonemeIndex[X];
    cmp #$35
    bne La333                           ; if (A != 'UW') skip rule
    dex
    ldy L9be0_phonemeIndex,x            ; Y = phonemeIndex[X - 1]
    inx
    lda L9fda_flags2,y                  ; If ALVEOLAR not set, continue with next phoneme
    and #Flags2::Alveolar
    bne La32b                           ; if (!(flags2[Y] & $4))
    jmp La396                           ;   continue

La32b:
    lda #$10
    sta L9be0_phonemeIndex,x            ; phonemeIndex[X] = 'UX';
    jmp La396                           ; continue

    ; RULE:
    ;       'CH' -> 'CH CH+' (CH requires two phonemes to represent it)
    ; Example: CHEW
La333:
    cmp #$2a
    bne La33c                           ; if (A != 'CH') skip rule
La337:
    tay
    iny                                 ; Y = 'CH+'
    jmp La343                           ; Insert

    ; RULE:
    ;       J -> J J+ (J requires two phonemes to represent it)
    ; Example: JAY
La33c:
    cmp #$2c
    beq La337                           ; if (A == 'J') Insert J+
    jmp La354                           ; else skip rule

La343:
    sty mem60                           ; mem60 = Y
    inx
    stx mem57                           ; mem57 = pos + 1
    dex
    lda L9de0_stress,x
    sta mem58                           ; mem58 = stress[pos]
    jsr La036_Insert                    ; Insert
    jmp La396                           ; continue

    ; RULE: Soften T following vowel
    ; NOTE: This rule fails for cases such as "ODD"
    ;       <UNSTRESSED VOWEL> T <PAUSE> -> <UNSTRESSED VOWEL> DX <PAUSE>
    ;       <UNSTRESSED VOWEL> D <PAUSE>  -> <UNSTRESSED VOWEL> DX <PAUSE>
    ; Example: PARTY, TARDY
La354:
    ; Past this point, only process if phoneme is T or D
    cmp #$45
    bne La35a                           ; if (A != 'T') check for 'D'
    beq La361                           ;   goto found T or D
La35a:
    cmp #$39                            ; if (A == 'D')
    beq La361                           ;   goto found T or D
    jmp La396                           ; else continue

    ; If prior phoneme is not a vowel, continue processing phonemes
La361:
    dex
    ldy L9be0_phonemeIndex,x
    inx
    lda L9f8c_flags,y
    and #Flags1::Vowel
    beq La396                           ; if (!(flags[phonemeIndex[X - 1]] & $80)) continue
    inx                                 ; X++
    lda L9be0_phonemeIndex,x            ; A = phonemeIndex[X];
    beq La38a                           ; if (A == 0) ??
    tay
    lda L9f8c_flags,y
    and #Flags1::Vowel
    beq La396                           ; if (!(flags[A] & 128)) continue
    lda L9de0_stress,x
    bne La396                           ; if (!stress[X]) continue
La380:
    ldx mem66_pos                       ; Change 'T' or 'D' with 'DX'
    lda #$1e
    sta L9be0_phonemeIndex,x            ; phonemeIndex[pos] = 'DX'
    jmp La396                           ; continue

La38a:
    inx
    lda L9be0_phonemeIndex,x            ; A = phonemeIndex[X + 1];
    tay
    lda L9f8c_flags,y
    and #Flags1::Vowel
    bne La380                           ; if (flags[A] & $80) change 'T' or 'D' to 'DX'
La396:
    inc mem66_pos                       ; pos++
    jmp La1b9                           ; loop
.endproc

; Iterates through the phoneme buffer, copying the stress value from
; the following phoneme under the following circumstance:
;
;     1. The current phoneme is voiced, excluding plosives and fricatives
;     2. The following phoneme is voiced, excluding plosives and fricatives, and
;     3. The following phoneme is stressed
;
;  In those cases, the stress value+1 from the following phoneme is copied.
;
; For example, the word LOITER is represented as LOY5TER, with as stress
; of 5 on the diphtong OY. This routine will copy the stress value of 6 (5+1)
; to the L that precedes it.
.proc Sa39b_CopyStress
    lda #$00
    sta mem66_pos                       ; pos = 0
La39f:                                  ; loop {
    ldx mem66_pos                       ;   X = pos
    ldy L9be0_phonemeIndex,x            ;   Y = phonemeIndex[pos];
    cpy #$ff
    bne La3a9                           ;   if (X = $ff) return
    rts

La3a9:
    ; Skip if CONSONANT_FLAG set.  Only vowels get stress.
    lda L9f8c_flags,y
    and #Flags1::Consonant
    beq La3c8                           ;   if (flags[Y] & $40) continue

    ; Skip if the following phoneme is a vowel.
    inx
    ldy L9be0_phonemeIndex,x            ;   Y = phonemeIndex[pos + 1]
    lda L9f8c_flags,y
    and #Flags1::Vowel
    beq La3c8                           ;   if (!(flags[Y] & $80)) continue

    ; If next phoneme is not stressed, skip
    ldy L9de0_stress,x                  ;   Y = stress[pos + 1]
    beq La3c8                           ;   if (Y == 0) continue

    ; If next phoneme is not a VOWEL OR ER, skip
    bmi La3c8

    ; Copy stress from prior phoneme to this one
    iny
    dex
    tya
    sta L9de0_stress,x                  ;   stress[pos] = Y + 1;
La3c8:
    inc mem66_pos                       ;   pos++
    jmp La39f                           ; }
.endproc

; Invoked by BASIC routines just before SAM_ML or Reciter_ML
Sa3cd:
    lda mem45
    sta mem34                           ; mem34 = mem45
    lda mem46
    sta mem35                           ; mem35 = mem46
    ldy #$00
La3d7:                                  ; loop {
    lda (mem34),y                       ;   A = mem34[Y]
La3da = * + 1
    cmp #$53
    bne La3e6                           ;   if (A == 'S')
    iny                                 ;     Y++
    lda (mem34),y                       ;     A = mem34[Y]
La3e1 = * + 1
    cmp #$c1
    bne La3e6                           ;     if (A == 'A')     (shifted)
    beq La411                           ;       goto La411      (near-jmp)
La3e6:                                  ;   }
    ldy #$00                            ;   Y = 0
    clc
    lda mem34
    adc #$07
    sta mem34
    lda mem35
    adc #$00
    sta mem35                           ;   mem34/35 += 7
    cmp mem48
    bcc La3d7                           ;   if (mem35 < mem48) goto La3d7
    bne La405                           ;   if (mem35 > mem48) goto La405
    lda mem34
    cmp mem47
    bcs La405
    beq La405                           ;   if (mem34 <= mem47) goto La405
    bne La3d7                           ; }  (near-jmp)
La405:
    jsr Sa43b_ErrorBeep                 ; ErrorBeep()

    ; Set 'mem27' indicating there was an error
    lda #$01
    sta mem27                           ; mem27 = 1
    pla                                 ; pop A
    pla                                 ; pop A
    jmp L9bd0                           ; goto L9bd0

La411:
    ldy #$04
    lda (mem34),y
    sta mem32                           ; mem32 = mem34[4]
    dey
    lda (mem34),y
    sta mem31                           ; mem31 = mem34[3]
    dey
    lda (mem34),y
    sta mem33                           ; mem33 = mem34[2]

    ldy #$00                            ; Y = 0
La423:                                  ; loop {
    lda (mem31),y
    and #$7f
    sta L9a15_input,y                   ;   L9a15_input[Y] = mem31[Y] & $7f
    iny                                 ;   Y++
    cpy mem33
    bne La423                           ; } while (Y != mem33)
    lda #$9b
    sta L9a15_input,y                   ; L9a15_input[Y] = $9b

    ; Clear 'mem27' indicating success
    lda #$00
    sta mem27                           ; mem27 = 0

La438:
    ; TODO: Purpose of setting mem29 is unclear?
    sta mem29                           ; mem29 = 0
    rts                                 ; return

; Note: Originally, there were two places where code would 'jsr' to $a439
;       and issue a 'brk' on return.  As $a439 is mid-instruction and results
;       in an illegal opcode, I'm guessing the intent was to call the nearby
;       'ErrorBeep'.
;
; Sa439 = La438 + 1
    ; ora La960,x                       ; La960 is inside La900_frequency1
    ; .byte $02                         ; ILLEGAL
    ; (continues at La43d)

; This routine is called to emit two short beeps when an error is encountered.
.proc Sa43b_ErrorBeep
    lda #$02
; La43d:                    (See comment note above)
    sta mem30                           ; A = 2
    cli                                 ; Disable interrupts

La440:                                  ; loop {
    ; Compute end time of beep from lowest byte of 60 HZ clock
    lda TIME + 2
    clc
    adc #$08
    tax                                 ;   X = *(TIME + 2) + 8
La446:                                  ;   do {
    lda #$0f
    sta SID_Amp                         ;     Amp = $f
    lda #$00                            ;     A = 0             (next value for Amp)

    ; Busy wait for 1/2 period
    ldy #$d0                            ;     Y = $d0
La44f:                                  ;     do {
    dey                                 ;       Y--
    bne La44f                           ;     } while (Y != 0)

    sta SID_Amp                         ;     Amp = $0

    ; Busy wait for 1/2 period
    ldy #$d0                            ;     Y = $d0
La457:                                  ;     do {
    dey                                 ;       Y--
    bne La457                           ;     } while (Y != 0)

    ; Check if enough time has elapsed
    cpx TIME + 2
    bne La446                           ;   } while(*(TIME + 2) != X)

    ; Exit after 2 beeps
    dec mem30                           ;   mem30--
    beq La46e                           ;   if (mem30 == 0) break

    ; Compute end of pause between beeps
    txa
    clc
    adc #$05
    tax                                 ;   X += 5

    ; Busy wait for duration of pause.  Note that 'Amp = $0'.
La467:                                  ;   do {
    cpx TIME + 2
    bne La467                           ;   } while (*(TIME + 2) != X)
    jmp La440                           ; }

La46e:
    rts                                 ; return
.endproc

La46f_saved_zero_page: .res $30

Padding8: .res $61 ; TODO: Remove?

La500_sinus:
    .byte $00, $00, $00, $10, $10, $10, $10, $10, $10, $20, $20, $20, $20, $20, $20, $30
    .byte $30, $30, $30, $30, $30, $30, $40, $40, $40, $40, $40, $40, $40, $50, $50, $50
    .byte $50, $50, $50, $50, $50, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60
    .byte $60, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70
    .byte $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70
    .byte $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $50, $50, $50, $50
    .byte $50, $50, $50, $50, $40, $40, $40, $40, $40, $40, $40, $30, $30, $30, $30, $30
    .byte $30, $30, $20, $20, $20, $20, $20, $20, $10, $10, $10, $10, $10, $10, $00, $00
    .byte $00, $00, $00, $f0, $f0, $f0, $f0, $f0, $f0, $e0, $e0, $e0, $e0, $e0, $e0, $d0
    .byte $d0, $d0, $d0, $d0, $d0, $d0, $c0, $c0, $c0, $c0, $c0, $c0, $c0, $b0, $b0, $b0
    .byte $b0, $b0, $b0, $b0, $b0, $a0, $a0, $a0, $a0, $a0, $a0, $a0, $a0, $a0, $a0, $a0
    .byte $a0, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90
    .byte $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90
    .byte $a0, $a0, $a0, $a0, $a0, $a0, $a0, $a0, $a0, $a0, $a0, $a0, $b0, $b0, $b0, $b0
    .byte $b0, $b0, $b0, $b0, $c0, $c0, $c0, $c0, $c0, $c0, $c0, $d0, $d0, $d0, $d0, $d0
    .byte $d0, $d0, $e0, $e0, $e0, $e0, $e0, $e0, $f0, $f0, $f0, $f0, $f0, $f0, $00, $00

La600_rectangle:
    .byte $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90
    .byte $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90
    .byte $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90
    .byte $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90
    .byte $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90
    .byte $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90
    .byte $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90
    .byte $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90
    .byte $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70
    .byte $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70
    .byte $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70
    .byte $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70
    .byte $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70
    .byte $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70
    .byte $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70
    .byte $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70

La700_multtable:
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $01, $01, $02, $02, $03, $03, $04, $04, $05, $05, $06, $06, $07, $07
    .byte $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b, $0c, $0d, $0e, $0f
    .byte $00, $01, $03, $04, $06, $07, $09, $0a, $0c, $0d, $0f, $10, $12, $13, $15, $16
    .byte $00, $02, $04, $06, $08, $0a, $0c, $0e, $10, $12, $14, $16, $18, $1a, $1c, $1e
    .byte $00, $02, $05, $07, $0a, $0c, $0f, $11, $14, $16, $19, $1b, $1e, $20, $23, $25
    .byte $00, $03, $06, $09, $0c, $0f, $12, $15, $18, $1b, $1e, $21, $24, $27, $2a, $2d
    .byte $00, $03, $07, $0a, $0e, $11, $15, $18, $1c, $1f, $23, $26, $2a, $2d, $31, $34
    .byte $00, $fc, $f8, $f4, $f0, $ec, $e8, $e4, $e0, $dc, $d8, $d4, $d0, $cc, $c8, $c4
    .byte $00, $fc, $f9, $f5, $f2, $ee, $eb, $e7, $e4, $e0, $dd, $d9, $d6, $d2, $cf, $cb
    .byte $00, $fd, $fa, $f7, $f4, $f1, $ee, $eb, $e8, $e5, $e2, $df, $dc, $d9, $d6, $d3
    .byte $00, $fd, $fb, $f8, $f6, $f3, $f1, $ee, $ec, $e9, $e7, $e4, $e2, $df, $dd, $da
    .byte $00, $fe, $fc, $fa, $f8, $f6, $f4, $f2, $f0, $ee, $ec, $ea, $e8, $e6, $e4, $e2
    .byte $00, $fe, $fd, $fb, $fa, $f8, $f7, $f5, $f4, $f2, $f1, $ef, $ee, $ec, $eb, $e9
    .byte $00, $ff, $fe, $fd, $fc, $fb, $fa, $f9, $f8, $f7, $f6, $f5, $f4, $f3, $f2, $f1
    .byte $00, $ff, $ff, $fe, $fe, $fd, $fd, $fc, $fc, $fb, $fb, $fa, $fa, $f9, $f9, $f8

.align $100 ; $a800-$af00 must be page aligned
La800_pitches:              .res $100
La900_frequency1:           .res $100
Laa00_frequency2:           .res $100
Lab00_frequency3:           .res $100
Lac00_amplitude1:           .res $100
Lad00_amplitude2:           .res $100
Lae00_amplitude3:           .res $100
Laf00_sampledConsonantFlag: .res $100

; TODO: The next several tables of length 80 are indexed by phoneme.
;       Comment with phoneme indices like flags tables?
Lb000_freq1data:
    .byte $00, $13, $13, $13, $13, $0a, $0e, $12, $18, $1a, $16, $14, $10, $14, $0e, $12
    .byte $0e, $12, $12, $10, $0c, $0e, $0a, $12, $0e, $0a, $08, $06, $06, $06, $06, $11
    .byte $06, $06, $06, $06, $0e, $10, $09, $0a, $08, $0a, $06, $06, $06, $05, $06, $00
    .byte $12, $1a, $14, $1a, $12, $0c, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06
    .byte $06, $06, $06, $06, $06, $06, $06, $06, $06, $0a, $0a, $06, $06, $06, $2c, $13

Lb050_freq2data:
    .byte $00, $43, $43, $43, $43, $54, $48, $42, $3e, $28, $2c, $1e, $24, $2c, $48, $30
    .byte $24, $1e, $32, $24, $1c, $44, $18, $32, $1e, $18, $52, $2e, $36, $56, $36, $43
    .byte $49, $4f, $1a, $42, $49, $25, $33, $42, $28, $2f, $4f, $4f, $42, $4f, $6e, $00
    .byte $48, $26, $1e, $2a, $1e, $22, $1a, $1a, $1a, $42, $42, $42, $6e, $6e, $6e, $54
    .byte $54, $54, $1a, $1a, $1a, $42, $42, $42, $6d, $56, $6d, $54, $54, $54, $7f, $7f

Lb0a0_freq3data:
    .byte $00, $5b, $5b, $5b, $5b, $6e, $5d, $5b, $58, $59, $57, $58, $52, $59, $5d, $3e
    .byte $52, $58, $3e, $6e, $50, $5d, $5a, $3c, $6e, $5a, $6e, $51, $79, $65, $79, $5b
    .byte $63, $6a, $51, $79, $5d, $52, $5d, $67, $4c, $5d, $65, $65, $79, $65, $79, $00
    .byte $5a, $58, $58, $58, $58, $52, $51, $51, $51, $79, $79, $79, $70, $6e, $6e, $5e
    .byte $5e, $5e, $51, $51, $51, $79, $79, $79, $65, $65, $70, $5e, $5e, $5e, $08, $01

Lb0f0_ampl1data:
    .byte $00, $00, $00, $00, $00, $0d, $0d, $0e, $0f, $0f, $0f, $0f, $0f, $0c, $0d, $0c
    .byte $0f, $0f, $0d, $0d, $0d, $0e, $0d, $0c, $0d, $0d, $0d, $0c, $09, $09, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $0b, $0b, $0b, $0b, $00, $00, $01, $0b, $00, $02
    .byte $0e, $0f, $0f, $0f, $0f, $0d, $02, $04, $00, $02, $04, $00, $01, $04, $00, $01
    .byte $04, $00, $00, $00, $00, $00, $00, $00, $00, $0c, $00, $00, $00, $00, $0f, $0f

Lb140_ampl2data:
    .byte $00, $00, $00, $00, $00, $0a, $0b, $0d, $0e, $0d, $0c, $0c, $0b, $09, $0b, $0b
    .byte $0c, $0c, $0c, $08, $08, $0c, $08, $0a, $08, $08, $0a, $03, $09, $06, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $03, $05, $03, $04, $00, $00, $00, $05, $0a, $02
    .byte $0e, $0d, $0c, $0d, $0c, $08, $00, $01, $00, $00, $01, $00, $00, $01, $00, $00
    .byte $01, $00, $00, $00, $00, $00, $00, $00, $00, $0a, $00, $00, $0a, $00, $00, $00

Lb190_ampl3data:
    .byte $00, $00, $00, $00, $00, $08, $07, $08, $08, $01, $01, $00, $01, $00, $07, $05
    .byte $01, $00, $06, $01, $00, $07, $00, $05, $01, $00, $08, $00, $00, $03, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $01, $0e, $01
    .byte $09, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, $00, $00, $05, $00, $13, $10

Lb1e0_phonemeStressedLength:
    .byte $00, $12, $12, $12, $08, $0b, $09, $0b, $0e, $0f, $0b, $10, $0c, $06, $06, $0e
    .byte $0c, $0e, $0c, $0b, $08, $08, $0b, $0a, $09, $08, $08, $08, $08, $08, $03, $05
    .byte $02, $02, $02, $02, $02, $02, $06, $06, $08, $06, $06, $02, $09, $04, $02, $01
    .byte $0e, $0f, $0f, $0f, $0e, $0e, $08, $02, $02, $07, $02, $01, $07, $02, $02, $07
    .byte $02, $02, $08, $02, $02, $06, $02, $02, $07, $02, $04, $07, $01, $04, $05, $05

Lb230_phonemeNormalLength:
    .byte $00, $12, $12, $12, $08, $08, $08, $08, $08, $0b, $06, $0c, $0a, $05, $05, $0b
    .byte $0a, $0a, $0a, $09, $08, $07, $09, $07, $06, $08, $06, $07, $07, $07, $02, $05
    .byte $02, $02, $02, $02, $02, $02, $06, $06, $07, $06, $06, $02, $08, $03, $01, $1e
    .byte $0d, $0c, $0c, $0c, $0e, $09, $06, $01, $02, $05, $01, $01, $06, $01, $02, $06
    .byte $01, $02, $08, $02, $02, $04, $02, $02, $06, $01, $04, $06, $01, $04, $c7, $ff

; The number of frames at the end of the phoneme used to transition to the following phoneme.
Lb280_outBlendLength:
    .byte $00, $02, $02, $02, $02, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
    .byte $04, $04, $03, $02, $04, $04, $02, $02, $02, $02, $02, $01, $01, $01, $01, $01
    .byte $01, $01, $01, $01, $01, $01, $02, $02, $02, $01, $00, $01, $00, $01, $00, $05
    .byte $05, $05, $05, $05, $04, $04, $02, $00, $01, $02, $00, $01, $02, $00, $01, $02
    .byte $00, $01, $02, $00, $02, $02, $00, $01, $03, $00, $02, $03, $00, $02, $a0, $a0

; The number of frames of the following phoneme used to transition into that phoneme.
Lb2d0_inBlendLength:
    .byte $00, $02, $02, $02, $02, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
    .byte $04, $04, $03, $03, $04, $04, $03, $03, $03, $03, $03, $01, $02, $03, $02, $01
    .byte $03, $03, $03, $03, $01, $01, $03, $03, $03, $02, $02, $03, $02, $03, $00, $00
    .byte $05, $05, $05, $05, $04, $04, $02, $00, $02, $02, $00, $03, $02, $00, $04, $02
    .byte $00, $03, $02, $00, $02, $02, $00, $02, $03, $00, $03, $03, $00, $03, $b0, $a0

; Determines which phoneme's blend values are used.
Lb320_blendRank:
    .byte $00, $1f, $1f, $1f, $1f, $02, $02, $02, $02, $02, $02, $02, $02, $02, $05, $05
    .byte $02, $0a, $02, $08, $05, $05, $0b, $0a, $09, $08, $08, $a0, $08, $08, $17, $1f
    .byte $12, $12, $12, $12, $1e, $1e, $14, $14, $14, $14, $17, $17, $1a, $1a, $1d, $1d
    .byte $02, $02, $02, $02, $02, $02, $1a, $1d, $1b, $1a, $1d, $1b, $1a, $1d, $1b, $1a
    .byte $1d, $1b, $17, $1d, $17, $17, $1d, $17, $17, $1d, $17, $17, $1d, $17, $17, $17

Lb370_sampledConsonantFlags:
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $f1, $e2, $d3, $bb, $7c, $95, $01, $02, $03, $03, $00, $72, $00, $02, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $1b, $00, $00, $19, $00, $00, $00, $00, $00, $00, $00, $00, $00

; TODO: Label beginning of each sample?
Lb3c0_sampleTable:
    .byte $38, $84, $6b, $19, $c6, $63, $18, $86, $73, $98, $c6, $b1, $1c, $ca, $31, $8c
    .byte $c7, $31, $88, $c2, $30, $98, $46, $31, $18, $c6, $35, $0c, $ca, $31, $0c, $c6
    .byte $21, $10, $24, $69, $12, $c2, $31, $14, $c4, $71, $08, $4a, $22, $49, $ab, $6a
    .byte $a8, $ac, $49, $51, $32, $d5, $52, $88, $93, $6c, $94, $22, $15, $54, $d2, $25
    .byte $96, $d4, $50, $a5, $46, $21, $08, $85, $6b, $18, $c4, $63, $10, $ce, $6b, $18
    .byte $8c, $71, $19, $8c, $63, $35, $0c, $c6, $33, $99, $cc, $6c, $b5, $4e, $a2, $99
    .byte $46, $21, $28, $82, $95, $2e, $e3, $30, $9c, $c5, $30, $9c, $a2, $b1, $9c, $67
    .byte $31, $88, $66, $59, $2c, $53, $18, $84, $67, $50, $ca, $e3, $0a, $ac, $ab, $30
    .byte $ac, $62, $30, $8c, $63, $10, $94, $62, $b1, $8c, $82, $28, $96, $33, $98, $d6
    .byte $b5, $4c, $62, $29, $a5, $4a, $b5, $9c, $c6, $31, $14, $d6, $38, $9c, $4b, $b4
    .byte $86, $65, $18, $ae, $67, $1c, $a6, $63, $19, $96, $23, $19, $84, $13, $08, $a6
    .byte $52, $ac, $ca, $22, $89, $6e, $ab, $19, $8c, $62, $34, $c4, $62, $19, $86, $63
    .byte $18, $c4, $23, $58, $d6, $a3, $50, $42, $54, $4a, $ad, $4a, $25, $11, $6b, $64
    .byte $89, $4a, $63, $39, $8a, $23, $31, $2a, $ea, $a2, $a9, $44, $c5, $12, $cd, $42
    .byte $34, $8c, $62, $18, $8c, $63, $11, $48, $66, $31, $9d, $44, $33, $1d, $46, $31
    .byte $9c, $c6, $b1, $0c, $cd, $32, $88, $c4, $73, $18, $86, $73, $08, $d6, $63, $58
    .byte $07, $81, $e0, $f0, $3c, $07, $87, $90, $3c, $7c, $0f, $c7, $c0, $c0, $f0, $7c
    .byte $1e, $07, $80, $80, $00, $1c, $78, $70, $f1, $c7, $1f, $c0, $0c, $fe, $1c, $1f
    .byte $1f, $0e, $0a, $7a, $c0, $71, $f2, $83, $8f, $03, $0f, $0f, $0c, $00, $79, $f8
    .byte $61, $e0, $43, $0f, $83, $e7, $18, $f9, $c1, $13, $da, $e9, $63, $8f, $0f, $83
    .byte $83, $87, $c3, $1f, $3c, $70, $f0, $e1, $e1, $e3, $87, $b8, $71, $0e, $20, $e3
    .byte $8d, $48, $78, $1c, $93, $87, $30, $e1, $c1, $c1, $e4, $78, $21, $83, $83, $c3
    .byte $87, $06, $39, $e5, $c3, $87, $07, $0e, $1c, $1c, $70, $f4, $71, $9c, $60, $36
    .byte $32, $c3, $1e, $3c, $f3, $8f, $0e, $3c, $70, $e3, $c7, $8f, $0f, $0f, $0e, $3c
    .byte $78, $f0, $e3, $87, $06, $f0, $e3, $07, $c1, $99, $87, $0f, $18, $78, $70, $70
    .byte $fc, $f3, $10, $b1, $8c, $8c, $31, $7c, $70, $e1, $86, $3c, $64, $6c, $b0, $e1
    .byte $e3, $0f, $23, $8f, $0f, $1e, $3e, $38, $3c, $38, $7b, $8f, $07, $0e, $3c, $f4
    .byte $17, $1e, $3c, $78, $f2, $9e, $72, $49, $e3, $25, $36, $38, $58, $39, $e2, $de
    .byte $3c, $78, $78, $e1, $c7, $61, $e1, $e1, $b0, $f0, $f0, $c3, $c7, $0e, $38, $c0
    .byte $f0, $ce, $73, $73, $18, $34, $b0, $e1, $c7, $8e, $1c, $3c, $f8, $38, $f0, $e1
    .byte $c1, $8b, $86, $8f, $1c, $78, $70, $f0, $78, $ac, $b1, $8f, $39, $31, $db, $38
    .byte $61, $c3, $0e, $0e, $38, $78, $73, $17, $1e, $39, $1e, $38, $64, $e1, $f1, $c1
    .byte $4e, $0f, $40, $a2, $02, $c5, $8f, $81, $a1, $fc, $12, $08, $64, $e0, $3c, $22
    .byte $e0, $45, $07, $8e, $0c, $32, $90, $f0, $1f, $20, $49, $e0, $f8, $0c, $60, $f0
    .byte $17, $1a, $41, $aa, $a4, $d0, $8d, $12, $82, $1e, $1e, $03, $f8, $3e, $03, $0c
    .byte $73, $80, $70, $44, $26, $03, $24, $e1, $3e, $04, $4e, $04, $1c, $c1, $09, $cc
    .byte $9e, $90, $21, $07, $90, $43, $64, $c0, $0f, $c6, $90, $9c, $c1, $5b, $03, $e2
    .byte $1d, $81, $e0, $5e, $1d, $03, $84, $b8, $2c, $0f, $80, $b1, $83, $e0, $30, $41
    .byte $1e, $43, $89, $83, $50, $fc, $24, $2e, $13, $83, $f1, $7c, $4c, $2c, $c9, $0d
    .byte $83, $b0, $b5, $82, $e4, $e8, $06, $9c, $07, $a0, $99, $1d, $07, $3e, $82, $8f
    .byte $70, $30, $74, $40, $ca, $10, $e4, $e8, $0f, $92, $14, $3f, $06, $f8, $84, $88
    .byte $43, $81, $0a, $34, $39, $41, $c6, $e3, $1c, $47, $03, $b0, $b8, $13, $0a, $c2
    .byte $64, $f8, $18, $f9, $60, $b3, $c0, $65, $20, $60, $a6, $8c, $c3, $81, $20, $30
    .byte $26, $1e, $1c, $38, $d3, $01, $b0, $26, $40, $f4, $0b, $c3, $42, $1f, $85, $32
    .byte $26, $60, $40, $c9, $cb, $01, $ec, $11, $28, $40, $fa, $04, $34, $e0, $70, $4c
    .byte $8c, $1d, $07, $69, $03, $16, $c8, $04, $23, $e8, $c6, $9a, $0b, $1a, $03, $e0
    .byte $76, $06, $05, $cf, $1e, $bc, $58, $31, $71, $66, $00, $f8, $3f, $04, $fc, $0c
    .byte $74, $27, $8a, $80, $71, $c2, $3a, $26, $06, $c0, $1f, $05, $0f, $98, $40, $ae
    .byte $01, $7f, $c0, $07, $ff, $00, $0e, $fe, $00, $03, $df, $80, $03, $ef, $80, $1b
    .byte $f1, $c2, $00, $e7, $e0, $18, $fc, $e0, $21, $fc, $80, $3c, $fc, $40, $0e, $7e
    .byte $00, $3f, $3e, $00, $0f, $fe, $00, $1f, $ff, $00, $3e, $f0, $07, $fc, $00, $7e
    .byte $10, $3f, $ff, $00, $3f, $38, $0e, $7c, $01, $87, $0c, $fc, $c7, $00, $3e, $04
    .byte $0f, $3e, $1f, $0f, $0f, $1f, $0f, $02, $83, $87, $cf, $03, $87, $0f, $3f, $c0
    .byte $07, $9e, $60, $3f, $c0, $03, $fe, $00, $3f, $e0, $77, $e1, $c0, $fe, $e0, $c3
    .byte $e0, $01, $df, $f8, $03, $07, $00, $7e, $70, $00, $7c, $38, $18, $fe, $0c, $1e
    .byte $78, $1c, $7c, $3e, $0e, $1f, $1e, $1e, $3e, $00, $7f, $83, $07, $db, $87, $83
    .byte $07, $c7, $07, $10, $71, $ff, $00, $3f, $e2, $01, $e0, $c1, $c3, $e1, $00, $7f
    .byte $c0, $05, $f0, $20, $f8, $f0, $70, $fe, $78, $79, $f8, $02, $3f, $0c, $8f, $03
    .byte $0f, $9f, $e0, $c1, $c7, $87, $03, $c3, $c3, $b0, $e1, $e1, $c1, $e3, $e0, $71
    .byte $f0, $00, $fc, $70, $7c, $0c, $3e, $38, $0e, $1c, $70, $c3, $c7, $03, $81, $c1
    .byte $c7, $e7, $00, $0f, $c7, $87, $19, $09, $ef, $c4, $33, $e0, $c1, $fc, $f8, $70
    .byte $f0, $78, $f8, $f0, $61, $c7, $00, $1f, $f8, $01, $7c, $f8, $f0, $78, $70, $3c
    .byte $7c, $ce, $0e, $21, $83, $cf, $08, $07, $8f, $08, $c1, $87, $8f, $80, $c7, $e3
    .byte $00, $07, $f8, $e0, $ef, $00, $39, $f7, $80, $0e, $f8, $e1, $e3, $f8, $21, $9f
    .byte $c0, $ff, $03, $f8, $07, $c0, $1f, $f8, $c4, $04, $fc, $c4, $c1, $bc, $87, $f0
    .byte $0f, $c0, $7f, $05, $e0, $25, $ec, $c0, $3e, $84, $47, $f0, $8e, $03, $f8, $03
    .byte $fb, $c0, $19, $f8, $07, $9c, $0c, $17, $f8, $07, $e0, $1f, $a1, $fc, $0f, $fc
    .byte $01, $f0, $3f, $00, $fe, $03, $f0, $1f, $00, $fd, $00, $ff, $88, $0d, $f9, $01
    .byte $ff, $00, $70, $07, $c0, $3e, $42, $f3, $0d, $c4, $7f, $80, $fc, $07, $f0, $5e
    .byte $c0, $3f, $00, $78, $3f, $81, $ff, $01, $f8, $01, $c3, $e8, $0c, $e4, $64, $8f
    .byte $e4, $0f, $f0, $07, $f0, $c2, $1f, $00, $7f, $c0, $6f, $80, $7e, $03, $f8, $07
    .byte $f0, $3f, $c0, $78, $0f, $82, $07, $fe, $22, $77, $70, $02, $76, $03, $fe, $00
    .byte $fe, $67, $00, $7c, $c7, $f1, $8e, $c6, $3b, $e0, $3f, $84, $f3, $19, $d8, $03
    .byte $99, $fc, $09, $b8, $0f, $f8, $00, $9d, $24, $61, $f9, $0d, $00, $fd, $03, $f0
    .byte $1f, $90, $3f, $01, $f8, $1f, $d0, $0f, $f8, $37, $01, $f8, $07, $f0, $0f, $c0
    .byte $3f, $00, $fe, $03, $f8, $0f, $c0, $3f, $00, $fa, $03, $f0, $0f, $80, $ff, $01
    .byte $b8, $07, $f0, $01, $fc, $01, $bc, $80, $13, $1e, $00, $7f, $e1, $40, $7f, $a0
    .byte $7f, $b0, $00, $3f, $c0, $1f, $c0, $38, $0f, $f0, $1f, $80, $ff, $01, $fc, $03
    .byte $f1, $7e, $01, $fe, $01, $f0, $ff, $00, $7f, $c0, $1d, $07, $f0, $0f, $c0, $7e
    .byte $06, $e0, $07, $e0, $0f, $f8, $06, $c1, $fe, $01, $fc, $03, $e0, $0f, $00, $fc

Lb8c0_phonemeIndexOutput:   .res $3c
Lb8fc_stressOutput:         .res $3c
Lb938_phonemeLengthOutput:  .res $3c

Lb974_amplitudeRescale:
    .byte $00, $01, $02, $02, $02, $03, $03, $04, $04, $05, $06, $08
    .byte $09, $0b, $0d, $0f

Lb984_tab47492:
    .byte $00, $00, $e0, $e6, $ec, $f3, $f9, $00, $06, $0c, $06

; TODO: Document...  Computes division with modulus?
.proc Sb98f_Div
    ldy #$00
    bit mem53
    bpl Lb99e
    sec
    lda #$00
    sbc mem53
    sta mem53
    ldy #$80
Lb99e:
    sty mem50
    lda #$00
    ldx #$08
Lb9a4:
    asl mem53
    rol a
    cmp mem52
    bcc Lb9af
    sbc mem52
    inc mem53
Lb9af:
    dex
    bne Lb9a4
    sta mem51
    bit mem50
    bpl Lb9bf
    sec
    lda #$00
    sbc mem53
    sta mem53
Lb9bf:
    rts
.endproc

; Save clobbered Zeropage area
; TODO: mem19 doesn't appear to otherwise be used?
.proc Sb9c0_SaveZeropage
    ldx #$2f
Lb9c2:
    lda mem19,x
    sta La46f_saved_zero_page,x
    dex
    bne Lb9c2
    rts
.endproc

; Restore clobbered Zeropage area
.proc Sb9cb_RestoreZeropage
    ldx #$2f
Lb9cd:
    lda La46f_saved_zero_page,x
    sta mem19,x
    dex
    bne Lb9cd
    rts
.endproc

; RENDER THE PHONEMES IN THE LIST
;
; The phoneme list is converted into sound through the steps:
;
; 1. Copy each phoneme <length> number of times into the frames list,
;    where each frame represents 10 milliseconds of sound.
;
; 2. Determine the transitions lengths between phonemes, and linearly
;    interpolate the values across the frames.
;
; 3. Offset the pitches by the fundamental frequency.
;
; 4. Render the each frame.
Sb9d6_Render:
    lda Lb8c0_phonemeIndexOutput
    cmp #$ff
    bne Lb9de                           ; if (phonemeIndexOutput[0] == 255) {
    rts                                 ;   return
Lb9de:                                  ; }

    lda #$00                            ; A = 0
    tax                                 ; X = 0
    sta mem44                           ; mem44 = 0

    ; CREATE FRAMES
    ;
    ; The length parameter in the list corresponds to the number of frames
    ; to expand the phoneme to. Each frame represents 10 milliseconds of time.
    ; So a phoneme with a length of 7 = 7 frames = 70 milliseconds duration.
    ;
    ; The parameters are copied from the phoneme to the frame verbatim.

Lb9e3:                                  ; do {
    ; get the index
    ldy mem44                           ;   Y = mem44

    ; get the phoneme at the index
    lda Lb8c0_phonemeIndexOutput,y      ;   A = phonemeIndexOutput[mem44];
    sta mem56                           ;   mem56 = A

    ; if terminal phoneme, exit the loop
    cmp #$ff
    bne Lb9f1                           ;   if (A == $ff) {
    jmp Lba4e                           ;     break
Lb9f1:                                  ;   }
    ; Add falling inflection if ends in period ('.')
    cmp #$01
    bne Lb9f8                           ;   if (A == 1) {
    jmp Lbcee_FallingInflection         ;     FallingInflection()
Lb9f8:                                  ;   }
    ; Add rising inflection if ends in question mark ('?')
    cmp #$02
    bne Lb9ff                           ;   if (A == 2) {
    jmp Lbcf4_RisingInflection          ;     RisingInflection()
Lb9ff:                                  ;   }

    ; get the stress amount (more stress = higher pitch)
    lda Lb8fc_stressOutput,y
    sta mem43                           ;   phase1 = tab47492[stressOutput[Y] + 1];

    ; get number of frames to write
    lda Lb938_phonemeLengthOutput,y
    sta mem42                           ;   phase2 = phonemeLengthOutput[Y]
    ldy mem43                           ;   Y = mem43

    ; TODO: What is this?
    iny                                 ;   Y++
    lda Lb984_tab47492,y                ;   A = Lb984_tab47492[Y]
    sta mem43                           ;   mem43 = A
    ldy mem56                           ;   Y = mem56

    ; copy from the source to the frames list
Lba13:                                  ;   do {
    lda Lb000_freq1data,y
    sta La900_frequency1,x              ;     frequency1[X] = freq1data[Y]; // F1 frequency
    lda Lb050_freq2data,y
    sta Laa00_frequency2,x              ;     frequency2[X] = freq2data[Y]; // F2 frequency
    lda Lb0a0_freq3data,y
    sta Lab00_frequency3,x              ;     frequency3[X] = freq3data[Y]; // F3 frequency
    lda Lb0f0_ampl1data,y
    sta Lac00_amplitude1,x              ;     amplitude1[X] = ampl1data[Y]; // F1 amplitude
    lda Lb140_ampl2data,y
    sta Lad00_amplitude2,x              ;     amplitude2[X] = ampl2data[Y]; // F2 amplitude
    lda Lb190_ampl3data,y
    sta Lae00_amplitude3,x              ;     amplitude3[X] = ampl3data[Y]; // F3 amplitude
    lda Lb370_sampledConsonantFlags,y
    sta Laf00_sampledConsonantFlag,x    ;    sampledConsonantFlag[X] = sampledConsonantFlags[Y];
    clc
Lba3f_pitch = * + 1
    lda #$40
    adc mem43
    sta La800_pitches,x                 ;     pitches[X] = pitch + phase1
    inx                                 ;     X++
    dec mem42                           ;     phase2--
    bne Lba13                           ;   } while (phase2 != 0)
    inc mem44                           ;   mem44++
    bne Lb9e3                           ; } while (mem44 != 0)

    ; CREATE TRANSITIONS
    ;
    ; Linear transitions are now created to smoothly connect the
    ; end of one sustained portion of a phoneme to the following
    ; phoneme.
    ;
    ; To do this, three tables are used:
    ;
    ;  Table         Purpose
    ;  =========     ==================================================
    ;  blendRank     Determines which phoneme's blend values are used.
    ;
    ;  blendOut      The number of frames at the end of the phoneme that
    ;                will be used to transition to the following phoneme.
    ;
    ;  blendIn       The number of frames of the following phoneme that
    ;                will be used to transition into that phoneme.
    ;
    ; In creating a transition between two phonemes, the phoneme
    ; with the HIGHEST rank is used. Phonemes are ranked on how much
    ; their identity is based on their transitions. For example,
    ; vowels and diphthongs are identified by their sustained portion,
    ; rather than the transitions, so they are given low values. In contrast,
    ; stop consonants (P, B, T, K) and glides (Y, L) are almost entirely
    ; defined by their transitions, and are given high rank values.
    ;
    ; Here are the rankings used by SAM:
    ;
    ;     Rank    Type                         Phonemes
    ;     2       All vowels                   IY, IH, etc.
    ;     5       Diphthong endings            YX, WX, ER
    ;     8       Terminal liquid consonants   LX, WX, YX, N, NX
    ;     9       Liquid consonants            L, RX, W
    ;     10      Glide                        R, OH
    ;     11      Glide                        WH
    ;     18      Voiceless fricatives         S, SH, F, TH
    ;     20      Voiced fricatives            Z, ZH, V, DH
    ;     23      Plosives, stop consonants    P, T, K, KX, DX, CH
    ;     26      Stop consonants              J, GX, B, D, G
    ;     27-29   Stop consonants (internal)   **
    ;     30      Unvoiced consonants          /H, /X and Q*
    ;     160     Nasal                        M
    ;
    ; To determine how many frames to use, the two phonemes are
    ; compared using the blendRank[] table. The phoneme with the
    ; higher rank is selected. In case of a tie, a blend of each is used:
    ;
    ;      if blendRank[phoneme1] ==  blendRank[phomneme2]
    ;          // use lengths from each phoneme
    ;          outBlendFrames = outBlend[phoneme1]
    ;          inBlendFrames = outBlend[phoneme2]
    ;      else if blendRank[phoneme1] > blendRank[phoneme2]
    ;          // use lengths from first phoneme
    ;          outBlendFrames = outBlendLength[phoneme1]
    ;          inBlendFrames = inBlendLength[phoneme1]
    ;      else
    ;          // use lengths from the second phoneme
    ;          // note that in and out are SWAPPED!
    ;          outBlendFrames = inBlendLength[phoneme2]
    ;          inBlendFrames = outBlendLength[phoneme2]
    ;
    ; Blend lengths can't be less than zero.
    ;
    ; Transitions are assumed to be symetrical, so if the transition
    ; values for the second phoneme are used, the inBlendLength and
    ; outBlendLength values are SWAPPED.
    ;
    ; For most of the parameters, SAM interpolates over the range of the last
    ; outBlendFrames-1 and the first inBlendFrames.
    ;
    ; The exception to this is the Pitch[] parameter, which interpolates the
    ; pitch from the CENTER of the current phoneme to the CENTER of the next
    ; phoneme.
    ;
    ; Here are two examples. First, consider the word "SUN" (S AH N)
    ;
    ;    Phoneme   Duration    BlendWeight    OutBlendFrames    InBlendFrames
    ;    S         2           18             1                 3
    ;    AH        8           2              4                 4
    ;    N         7           8              1                 2
    ;
    ; The formant transitions for the output frames are calculated as follows:
    ;
    ;     flags ampl1 freq1 ampl2 freq2 ampl3 freq3 pitch
    ;    ------------------------------------------------
    ; S
    ;    241     0     6     0    73     0    99    61   Use S (weight 18) for transition instead of AH (weight 2)
    ;    241     0     6     0    73     0    99    61   <-- (OutBlendFrames-1) = (1-1) = 0 frames
    ; AH
    ;      0     2    10     2    66     0    96    59 * <-- InBlendFrames = 3 frames
    ;      0     4    14     3    59     0    93    57 *
    ;      0     8    18     5    52     0    90    55 *
    ;      0    15    22     9    44     1    87    53
    ;      0    15    22     9    44     1    87    53
    ;      0    15    22     9    44     1    87    53   Use N (weight 8) for transition instead of AH (weight 2).
    ;      0    15    22     9    44     1    87    53   Since N is second phoneme, reverse the IN and OUT values.
    ;      0    11    17     8    47     1    98    56 * <-- (InBlendFrames-1) = (2-1) = 1 frames
    ; N
    ;      0     8    12     6    50     1   109    58 * <-- OutBlendFrames = 1
    ;      0     5     6     5    54     0   121    61
    ;      0     5     6     5    54     0   121    61
    ;      0     5     6     5    54     0   121    61
    ;      0     5     6     5    54     0   121    61
    ;      0     5     6     5    54     0   121    61
    ;      0     5     6     5    54     0   121    61
    ;
    ; Now, consider the reverse "NUS" (N AH S):
    ;
    ;     flags ampl1 freq1 ampl2 freq2 ampl3 freq3 pitch
    ;    ------------------------------------------------
    ; N
    ;     0     5     6     5    54     0   121    61
    ;     0     5     6     5    54     0   121    61
    ;     0     5     6     5    54     0   121    61
    ;     0     5     6     5    54     0   121    61
    ;     0     5     6     5    54     0   121    61
    ;     0     5     6     5    54     0   121    61   Use N (weight 8) for transition instead of AH (weight 2)
    ;     0     5     6     5    54     0   121    61   <-- (OutBlendFrames-1) = (1-1) = 0 frames
    ; AH
    ;     0     8    11     6    51     0   110    59 * <-- InBlendFrames = 2
    ;     0    11    16     8    48     0    99    56 *
    ;     0    15    22     9    44     1    87    53   Use S (weight 18) for transition instead of AH (weight 2)
    ;     0    15    22     9    44     1    87    53   Since S is second phoneme, reverse the IN and OUT values.
    ;     0     9    18     5    51     1    90    55 * <-- (InBlendFrames-1) = (3-1) = 2
    ;     0     4    14     3    58     1    93    57 *
    ; S
    ;   241     2    10     2    65     1    96    59 * <-- OutBlendFrames = 1
    ;   241     0     6     0    73     0    99    61
Lba4e:
    lda #$00                            ; A = 0
    sta mem44                           ; mem44 = 0
    sta mem49                           ; mem49 = 0
    tax                                 ; X = 0
Lba55:                                  ; while (true) {
    ; Get the current and following phoneme
    ldy Lb8c0_phonemeIndexOutput,x      ;   Y = phonemeIndexOutput[X];
    inx                                 ;   X++
    lda Lb8c0_phonemeIndexOutput,x      ;   A = phonemeIndexOutput[X];

    ; Exit loop at end token
    cmp #$ff
    bne Lba63                           ;   if (A == $ff)
    jmp Lbb62                           ;     break

    ; Get the ranking of each phoneme
Lba63:
    tax                                 ; X = A
    lda Lb320_blendRank,x
    sta mem56                           ; mem56 = blendRank[A]
    lda Lb320_blendRank,y               ; A = blendRank[Y]

    ; Compare the rank - lower rank value is stronger
    cmp mem56
    beq Lba8c                           ; if (A == mem56) goto equal
    bcc Lba7f                           ; else if (A < mem56) goto less_than

    ; Second phoneme is stronger, so use it's blend lengths
    ; note the out/in are swapped
    lda Lb280_outBlendLength,y          ; phase1 = outBlendLength[Y]
    sta mem43
    lda Lb2d0_inBlendLength,y           ; phase2 = inBlendLength[Y]
    sta mem42
    jmp Lba96

Lba7f:
    ; First phoneme is stronger, so us it's blend lengths
    lda Lb2d0_inBlendLength,x
    sta mem43                           ; phase1 = inBlendLength[X];
    lda Lb280_outBlendLength,x
    sta mem42                           ; phase2 = outBlendLength[X];
    jmp Lba96

Lba8c:
    ; Same rank, so use out blend lengths from each phoneme
    lda Lb280_outBlendLength,y
    sta mem43                           ; phase1 = outBlendLength[Y];
    lda Lb280_outBlendLength,x
    sta mem42                           ; phase2 = outBlendLength[X];
Lba96:
    clc
    lda mem49                           ; A = mem49
    ldy mem44                           ; Y = mem44;
    adc Lb938_phonemeLengthOutput,y
    sta mem49                           ; mem49 += phonemeLengthOutput[mem44]
    adc mem42                           ; A += phase2
    sta mem45                           ; speedcounter = A
    lda #<La800_pitches ; (originally '#$00')
    sta mem46_ptr
    lda #>La800_pitches ; (originally '#$a8')
    sta mem46_ptr + 1                   ; ptr = &La800_pitches
    sec
    lda mem49                           ; A = mem49
    sbc mem43                           ; A -= mem43
    sta mem41                           ; mem41 = A
    clc
    lda mem43                           ; A = mem43
    adc mem42                           ; A += mem42
    sta mem38                           ; mem38 = A
    tax                                 ; X = A
    dex
    dex                                 ; X -= 2
    bpl Lbac2                           ; if ((x & $80) == 0) {
    jmp Lbb5b

Lbac2:                                  ;   do {
    lda mem38
    sta mem40                           ;     mem40 = mem38
    lda mem46_ptr + 1
    cmp #>La800_pitches ; (originally '#$a8')
    bne Lbb09                           ;     if (hi(ptr) == hi(&La800_pitches)) {

    ; Unlike the other values, 'pitches[]'' interpolates from
    ; the middle of the current phoneme to the middle of the
    ; next phoneme

    ; Half the width of the current phoneme
    ldy mem44
    lda Lb938_phonemeLengthOutput,y
    lsr a
    sta mem36                           ;       mem36 = phonemeLengthOutput[mem44] >> 1

    ; Half the width of the next phoneme
    iny
    lda Lb938_phonemeLengthOutput,y
    lsr a
    sta mem37                           ;       mem37 = phonemeLengthOutput[mem44 + 1] >> 1

    ; Sum the values
    ; Length of both halves
    clc
    lda mem36
    adc mem37
    sta mem40                           ;       mem40 = mem36 + mem37

    ; Center of next phoneme
    clc
    lda mem49
    adc mem37
    sta mem37                           ;       mem37 += mem49

    ; Center index of current phoneme
    sec
    lda mem49
    sbc mem36
    sta mem36                           ;       mem36 = mem49 - mem36

    ; Value at center of next phoneme - end interpolation value
    ldy mem37
    lda (mem46_ptr),y                   ;       A = ptr[mem37]

    ; Start index of interpolation
    sec
    ldy mem36                           ;       Y = mem36;

    ; Value to center of current phoneme
    sbc (mem46_ptr),y
    sta mem53                           ;       mem53 = A - ptr[mem36]
    lda mem40
    sta mem52                           ;       mem52 = mem40
    jsr Sb98f_Div                       ;       Div()
    ldx mem40                           ;       X = mem40

    ; Start index of interpolation
    ldy mem36                           ;       Y = mem36
    jmp Lbb1f

Lbb09:                                  ;     } else {
    ; Value to interpolate to
    ldy mem45
    sec
    lda (mem46_ptr),y                   ;       A = ptr[mem45]

    ; Position to start interpolation from
    ldy mem41

    ; Value to interpolate from
    sbc (mem46_ptr),y
    sta mem53                           ;       mem53 = A - ptr[phase3];
    lda mem40
    sta mem52                           ;       mem52 = mem40
    jsr Sb98f_Div                       ;       Div()
    ldx mem40                           ;       X = mem40
    ldy mem41                           ;       Y = mem41
Lbb1f:                                  ;     }
    ; Linearly interpolate values
    lda #$00
    sta mem56                           ;   mem56 = 0
    clc
Lbb24:                                  ;   while {
    lda (mem46_ptr),y
    adc mem53
    sta mem48                           ;     mem48 = ptr[Y] + mem53;
    iny                                 ;     Y++
    dex                                 ;     X--
    beq Lbb50                           ;     if (X == 0) break;
    clc
    lda mem56
    adc mem51
    sta mem56                           ;     mem56 += mem51
    cmp mem40
    bcc Lbb49                           ;     if (mem56 >= mem40) {
    lda mem56
    sbc mem40
    sta mem56                           ;       mem56 -= mum40

    ; Note that the '&& mem48 != 0' below is implemented by falling through to
    ; to 'dec mem48' when mem48 == 0, which componsates for the 'inc mem48'.

    bit mem50
    bmi Lbb47                           ;       if ((mem50 & 128) == 0 && mem48 != 0) {   ; (see below)
    inc mem48                           ;          mem48++
    bne Lbb49                           ;       } else {
Lbb47:
    dec mem48                           ;         mem48--
Lbb49:                                  ;       }

    lda mem48
    sta (mem46_ptr),y                   ;       ptr[Y] = mem48
    clc
    bcc Lbb24                           ;     }   (near-jmp: carry always 0)
Lbb50:
    inc mem46_ptr + 1                   ;     ptr += $100
    lda mem46_ptr + 1
    cmp #>Laf00_sampledConsonantFlag ; (originally '#$af')
    beq Lbb5b                           ;
    jmp Lbac2                           ;   } while (hi(ptr) != hi(&Laf00_sampledConsonantFlag))

Lbb5b:
    inc mem44                           ;   mem44++
    ldx mem44                           ;   X = mem44
    jmp Lba55                           ; }

Lbb62:
    ; Add the length of this phoneme
    lda mem49
    clc
    ldy mem44
    adc Lb938_phonemeLengthOutput,y
    sta mem48                           ; mem48 = mem49 + phonemeLengthOutput[mem44]

    ; ASSIGN PITCH CONTOUR
    ;
    ; This subtracts the F1 frequency from the pitch to create a
    ; pitch contour. Without this, the output would be at a single
    ; pitch level (monotone).

    ; TODO: Implement "sing mode", which skips pitch contour adjustment?

    ldx #$00                            ; X = 0
Lbb6e:                                  ; do {
    ; Subtract half the frequency of the formant 1.
    ; This adds variety to the voice

    lda La900_frequency1,x
    lsr a
    sta mem56
    sec
    lda La800_pitches,x
    sbc mem56
    sta La800_pitches,x                 ;   pitches[X] -= frequency1[X] >> 1
    dex
    bne Lbb6e                           ; } while (X != 0)

    lda #$00                            ; A = 0
    sta mem43                           ; mem43 = 0     TODO: phase1?
    sta mem42                           ; mem42 = 0     TODO: phase2?
    sta mem41                           ; mem41 = 0     TODO: phase3?
    sta mem49                           ; mem49 = 0
    lda #$48
    sta mem45                           ; mem45 = $48   TODO: speedcounter?

    ; RESCALE AMPLITUDE
    ;
    ; Rescale volume from a linear scale to decibels.
    ;
    ; amplitude rescaling

    ; mem56 tracks how many amplitude tables are remaining for rescale
    lda #$03
    sta mem56                           ; mem56 = $03

    lda #<Lac00_amplitude1 ; (originally '#$00')
    sta mem46_ptr
    lda #>Lac00_amplitude1 ; (originally '#$ac')
    sta mem46_ptr + 1                   ; ptr = &Lac00_amplitude1
Lbb9a:                                  ; do {
    ldy #$00                            ;   Y = 0
Lbb9c:                                  ;   do {
    lda (mem46_ptr),y
    tax
    lda Lb974_amplitudeRescale,x
    sta (mem46_ptr),y                   ;     ptr[Y] = amplitudeRescale[ptr[Y]]
    dey                                 ;     Y--
    bne Lbb9c                           ;   } while (Y != 0)

    ; Move to next amplitude table
    inc mem46_ptr + 1                   ;   ptr += $100

    ; Check if there are remaining amplitude tables to rescale
    dec mem56
    bne Lbb9a                           ; } while (mem56 != 0)

    ldy #$00                            ; Y = 0

    lda La800_pitches,y                 ; A = pitches[0]
    sta mem44                           ; mem44 = pitches[0]

    ; Calculate 3/4 * pitches[0]
    tax                                 ; X = pitches[0]
    lsr a
    lsr a
    sta mem56                           ; A = A >> 2
    sec
    txa
    sbc mem56
    sta mem38                           ; mem38 = A - (A >> 2)
    jmp Lbbce_soundout2

    ; PROCESS THE FRAMES
    ;
    ; In traditional vocal synthesis, the glottal pulse drives filters, which
    ; are attenuated to the frequencies of the formants.
    ;
    ; SAM generates these formants directly with sin and rectangular waves.
    ; To simulate them being driven by the glottal pulse, the waveforms are
    ; reset at the beginning of each glottal pulse.

    ; finally the loop for sound output
Lbbc2:
    jsr Sbc63_RenderSample

    ; Skip ahead two in the phoneme buffer (?)
    iny                                     ; Y++
    iny                                     ; Y++
    dec mem48                               ; mem48--
    dec mem48                               ; mem48--
    jmp Lbc14

; CombineGlottalAndFormants?
Lbbce_soundout2:
    lda Laf00_sampledConsonantFlag,y
    sta mem39                               ; mem39 = sampledConsonantFlag[Y]
    and #$f8
    bne Lbbc2                               ; if (mem39 & $f8) continue (??)

    ldx mem43
    clc
    lda La500_sinus,x
    ora Lac00_amplitude1,y
    tax
    lda La700_multtable,x
    sta mem56                               ;   mem56 = multtable[sinus[mem43] | amplitude1[Y]]

    ldx mem42
    lda La500_sinus,x
    ora Lad00_amplitude2,y
    tax
    lda La700_multtable,x
    adc mem56
    sta mem56                               ;   mem56 += multtable[sinus[mem42] | amplitude2[Y]]

    ldx mem41
    lda La600_rectangle,x
    ora Lae00_amplitude3,y
    tax
    lda La700_multtable,x
    adc mem56                               ;   A = mem56 += multtable[rectangle[mem41] | amplitude3[Y]];

    adc #$88                                ;   A += $88

    lsr a
    lsr a
    lsr a
    lsr a                                   ;   A >>= 4

    sta SID_Amp                             ;   Amp = A

    dec mem45                               ;   mem45--
    bne Lbc1b                               ;   if (mem45 != 0) goto ??

    iny                                     ;   Y++
    dec mem48                               ;   mem48--

 Lbc14:
    bne Lbc17                               ; if (mem48)
    rts                                     ;   return

Lbc18_speed = * + 1
Lbc17:
    lda #$48
    sta mem45                               ; mem45 = speed
Lbc1b:
    dec mem44                               ; mem44--
    bne Lbc3a                               ; if (mem44) {
Lbc1f:
    lda La800_pitches,y
    sta mem44                               ;   mem44 = pitches[Y]    (glottal_pulse?)
    tax                                     ;   X = A
    lsr a
    lsr a
    sta mem56                               ;   A = mem44 >> 2
    sec
    txa
    sbc mem56
    sta mem38                               ;   mem38 = mem44 - (mem44 >> 2)      (mem44 * 0.75)
    lda #$00                                ;   A = 0
    sta mem43                               ;   mem43 = 0
    sta mem42                               ;   mem42 = 0
    sta mem41                               ;   mem41 = 0
    jmp Lbbce_soundout2                     ;   goto (??)

Lbc3a:                                      ; }
    ; Within the first 75% of the glottal pulse?
    dec mem38                               ; mem38--
    bne Lbc48                               ; if (mem38) {

    ; Is the count is non-zero and the sampled flag is zero?
    lda mem39                               ;   A = mem39
    beq Lbc48                               ;   if (!A) {

    jsr Sbc63_RenderSample                  ;     RenderSample()
    jmp Lbc1f                               ;     goto (??)

Lbc48:                                      ; } }

    ; reset the phase of the formants to match the pulse
    clc
    lda mem43
    adc La900_frequency1,y
    sta mem43                               ; phase1 += frequency1[Y];
    clc
    lda mem42
    adc Laa00_frequency2,y
    sta mem42                               ; phase2 += frequency2[Y];
    clc
    lda mem41
    adc Lab00_frequency3,y
    sta mem41                               ; phase3 += frequency3[Y];
    jmp Lbbce_soundout2

; Render a sampled sound from the sampleTable.
;
;   Phoneme   Sample Start   Sample End
;   32: S*    15             255
;   33: SH    257            511
;   34: F*    559            767
;   35: TH    583            767
;   36: /H    903            1023
;   37: /X    1135           1279
;   38: Z*    84             119
;   39: ZH    340            375
;   40: V*    596            639
;   41: DH    596            631
;
;   42: CH
;   43: **    399            511
;
;   44: J*
;   45: **    257            276
;   46: **
;
;   66: P*
;   67: **    743            767
;   68: **
;
;   69: T*
;   70: **    231            255
;   71: **
;
; The SampledPhonemesTable[] holds flags indicating if a phoneme is
; voiced or not. If the upper 5 bits are zero, the sample is voiced.
;
; Samples in the sampleTable are compressed, with bits being converted to
; bytes from high bit to low, as follows:
;
;   unvoiced 0 bit   -> X
;   unvoiced 1 bit   -> 5
;
;   voiced 0 bit     -> 6
;   voiced 1 bit     -> 24
;
; Where X is a value from the table:
;
;   { 0x18, 0x1A, 0x17, 0x17, 0x17 };
;
; The index into this table is determined by masking off the lower
; 3 bits from the SampledPhonemesTable:
;
;        index = (SampledPhonemesTable[i] & 7) - 1;
;
; For voices samples, samples are interleaved between voiced output.
Sbc63_RenderSample:
    sty mem49                           ; mem49 = Y     TODO: Is this always the current phoneme's index?
    lda mem39                           ; A = mem39     TODO: Is this always the consonant flag?

  	; mask low three bits and subtract 1 get value to
	; convert 0 bits on unvoiced samples.
    tay
    and #$07
    tax
    dex
    stx mem56                           ; X = mem56 = (consonantFlag & 7) - 1

	; determine which offset to use from table { 0x18, 0x1A, 0x17, 0x17, 0x17 }
	; T, S, Z                0          0x18
	; CH, J, SH, ZH          1          0x1A
	; P, F*, V, TH, DH       2          0x17
	; /H                     3          0x17
	; /X                     4          0x17

    lda Lbd2a_tab48426,x
    sta mem53                           ; mem53 = Lbd2a_tab48426[X]

    clc
    lda #>Lb3c0_sampleTable ; (originally '#$b3')
    adc mem56
    sta mem46_ptr + 1
    lda #<Lb3c0_sampleTable ; (originally '#$c0')
    sta mem46_ptr                       ; ptr = &randomtable[mem56 << 8]

    ; If the upper 5 bits are non-zero, the sample is unvoiced
    tya
    and #$f8
    bne Lbc8f_RenderUnvoicedSample      ; if (A & $f8) goto soundout3

    ; Else the sample is voiced: Z*, ZH, V*, DH
    ldy mem49                           ; Y = mem49
    lda La800_pitches,y
    lsr a
    lsr a
    lsr a
    lsr a                               ; A = pitches[mem49] >> 4
    jmp Lbcbb_RenderVoicedSample

Lbc8f_RenderUnvoicedSample:
    eor #$ff                            ; A                 (pitch?)
    tay                                 ; Y = A ^ $ff       (offset?)
Lbc92:                                  ; do {
    lda #$08
    sta mem56                           ;   mem56 = 8       (bits remaining)
    lda (mem46_ptr),y                   ;   A = ptr[Y]      (sample)
Lbc98:                                  ;   do {
    asl a                               ;     A <<= 1
    bcc Lbca2                           ;     if (!(A & $100)) {
    ldx mem53
    stx SID_Amp                         ;       Amp = mem53
    bne Lbca8                           ;     } else {
Lbca2:
    ldx #$05
    stx SID_Amp                         ;       Amp = $5
    nop
Lbca8:                                  ;     }
unvoiced_length = * + 1
    ldx #$07
Lbcaa:
    dex
    bne Lbcaa                           ;     (busy wait)
    dec mem56                           ;     mem56--
    bne Lbc98                           ;   } while (mem56 != 0)
    iny                                 ;   Y++
    bne Lbc92                           ; } while (Y != 0)

    lda #$01                            ; A = $1
    sta mem44                           ; mem44 = $1
    ldy mem49                           ; Y = mem49
    rts

Lbcbb_RenderVoicedSample:
    eor #$ff                            ; A ^= $ff
    sta mem43                           ; mem43 = A
    ldy mem66_pos                       ; Y = pos

Lbcc1:                                  ; do {
    lda #$08
    sta mem56                           ;   mem56 = $8    (bits remaining)
    lda (mem46_ptr),y                   ;   A = ptr[Y]    (sample)
Lbcc7:                                  ;   do {
    asl a                               ;     A <<= 1
    bcc Lbcd1                           ;     if (!(sample & $100)) {
    ldx #$1a
    stx SID_Amp                         ;       Amp=$a + Enable LP filter
    bne Lbcd7                           ;       (near-jmp: we only enter this branch when A == 0)
Lbcd1:                                  ;     } else {
    ldx #$06
    stx SID_Amp                         ;       Amp=$6
    nop
Lbcd7:                                  ;     }
voiced_length = * + 1
    ldx #$06
Lbcd9:
    dex
    bne Lbcd9                           ;     (busy wait)
    dec mem56                           ;     mem56--
    bne Lbcc7                           ;   } while (mem56 != 0)
    iny                                 ;   Y++
    inc mem43                           ;   mem43++
    bne Lbcc1                           ; } while (mem43 != 0)

    lda #$01                            ; A = 1
    sta mem44                           ; mem44 = 1
    sty mem66_pos                       ; pos = 1
    ldy mem49                           ; Y = mem49
    rts                                 ; return

.proc Lbcee_FallingInflection
    lda #$01
    sta mem48
    bne Lbcf8_AddInflection
.endproc

.proc Lbcf4_RisingInflection
    lda #$ff
    sta mem48
.endproc

; Create a rising or falling inflection 30 frames prior to
; index X. A rising inflection is used for questions, and
; a falling inflection is used for statements.
.proc Lbcf8_AddInflection
    stx mem49                           ; X = mem49             (end)
    txa
    sec
    sbc #$1e                            ; A = mem49 - 30
    bcs Lbd02                           ; if (A < 30) {
    lda #$00                            ;   A = 0
Lbd02:                                  ; }
    tax                                 ; X = A                 (X = start)

    ; Advance start position while pitches[X] == $7f
Lbd03:                                  ; loop {
    lda La800_pitches,x                 ;   A = pitches[X]
    cmp #$7f
    bne Lbd0e                           ;   if (A != $7f) break
    inx                                 ;   X++
    jmp Lbd03                           ; }

Lbd0e:                                  ; loop {
    clc
    adc mem48
    sta mem43                           ;   mem43 = A + mem48   (A is pitch, mem48 is delta)
    sta La800_pitches,x                 ;   pitches[X] = A
Lbd16:                                  ;   loop {
    inx                                 ;     X++
    cpx mem49
    beq Lbd27                           ;     if (X == mem49) return
    lda La800_pitches,x                 ;     A = pitches[X]
    cmp #$ff
    beq Lbd16                           ;   } while (A == $ff)
    lda mem43                           ;   A = mem43
    jmp Lbd0e                           ; }

Lbd27:
    jmp Lb9ff
.endproc

Lbd2a_tab48426:
    .byte $18, $1a, $17, $17, $17

.proc Sbd2f_InsertBreath
    ldx #$ff
    stx mem54                           ; mem54 = $ff
    inx                                 ; X = 0
    stx mem55                           ; mem55 = 0
    stx mem66_pos                       ; pos = 0

Lbd38_continue:                         ; loop {
    ldx mem66_pos                       ;   X = pos
    ldy L9be0_phonemeIndex,x            ;   Y = phonemeIndex[X];
    cpy #$ff
    bne Lbd42                           ;   if (Y == $ff)
    rts                                 ;     return

Lbd42:
    clc
    lda mem55
    adc L9ce0_phonemeLength,x
    sta mem55                           ;   mem55 += phonemeLength[X]
    cmp #$e8
    bcc Lbd51                           ;   if (mem55 < $e8) {
    jmp Lbd7a

Lbd51:
    lda L9fda_flags2,y
    and #Flags2::Punctuation
    beq Lbd6f                           ;     if (flags2[index] & 1) {
    inx                                 ;       X++
    stx mem57                           ;       mem57 = X
    lda #$00
    sta mem55                           ;       mem55 = 0
    sta mem58                           ;       mem58 = 0
    lda #$fe
    sta mem60                           ;       mem60 = $fe
    jsr La036_Insert                    ;       Insert()
    inc mem66_pos
    inc mem66_pos                       ;       pos += 2
    jmp Lbd38_continue                  ;       continue
                                        ;     }
Lbd6f:
    cpy #$00
    bne Lbd75                           ;     if (Y == $0)
    stx mem54                           ;       mem54 = X
Lbd75:
    inc mem66_pos                       ;     pos++
    jmp Lbd38_continue                  ;     continue

Lbd7a:                                  ;   }
    ldx mem54
    lda #$1f
    sta L9be0_phonemeIndex,x            ;   phonemeIndex[X] = 'Q*' (glottal stop)
    lda #$04
    sta L9ce0_phonemeLength,x           ;   phonemeLength[X] = 4
    lda #$00
    sta L9de0_stress,x                  ;   stress[X] = 0
    inx                                 ;   X++
    stx mem57                           ;   mem57 = X
    lda #$fe
    sta mem60                           ;   mem60 = $fe
    lda #$00
    sta mem55                           ;   mem55 = $0
    sta mem58                           ;   mem58 = $0
    jsr La036_Insert                    ;   Insert()
    inx                                 ;   X++
    stx mem66_pos                       ;   pos = X
    jmp Lbd38_continue                  ; }
.endproc

Padding9: .res $1 ; TODO: Remove?

Lbda2_temp: .res $1

.proc Sbda3_PrepareOutput
    lda #$00                            ; A = 0
    tax                                 ; X = 0
    tay                                 ; Y = 0
Lbda7_continue:                         ; loop {
    lda L9be0_phonemeIndex,x            ;   A = phonemeIndex[X]
    cmp #$ff
    bne Lbdb7                           ;   if (A == $ff) {
    lda #$ff
    sta Lb8c0_phonemeIndexOutput,y      ;     phonemeIndexOutput[Y] = $ff
    jsr Sb9d6_Render                    ;     Render()
    rts                                 ;     return
Lbdb7:                                  ;   }
    cmp #$fe
    bne Lbdcf                           ;   if (A == $fe) {
    inx                                 ;     X++
    stx Lbda2_temp                      ;     temp = X
    lda #$ff
    sta Lb8c0_phonemeIndexOutput,y      ;     phonemeIndexOutput[Y] = $ff;
    jsr Sb9d6_Render                    ;     Render()
    ldx Lbda2_temp                      ;     X = temp
    ldy #$00                            ;     Y = $0
    jmp Lbda7_continue                  ;     continue
Lbdcf:                                  ;   }
    cmp #$00
    bne Lbdd7                           ;   if (A == 0) {
    inx                                 ;     X++
    jmp Lbda7_continue                  ;     continue
Lbdd7:                                  ;   }

    sta Lb8c0_phonemeIndexOutput,y      ;   phonemeIndexOutput[Y] = A
    lda L9ce0_phonemeLength,x
    sta Lb938_phonemeLengthOutput,y     ;   phonemeLengthOutput[Y] = phonemeLength[X];
    lda L9de0_stress,x
    sta Lb8fc_stressOutput,y            ;   stressOutput[Y] = stress[X];
    inx                                 ;   X++
    iny                                 ;   Y++
    jmp Lbda7_continue                  ; }
.endproc

; Applies various rules that adjust the lengths of phonemes
;
;       Lengthen <FRICATIVE> or <VOICED> between <VOWEL> and <PUNCTUATION> by 1.5
;       <VOWEL> <RX | LX> <CONSONANT> - decrease <VOWEL> length by 1
;       <VOWEL> <UNVOICED PLOSIVE> - decrease vowel by 1/8th
;       <VOWEL> <UNVOICED CONSONANT> - increase vowel by 1/2 + 1
;       <NASAL> <STOP CONSONANT> - set nasal = 5, consonant = 6
;       <VOICED STOP CONSONANT> {optional silence} <STOP CONSONANT> - shorten both to 1/2 + 1
;       <LIQUID CONSONANT> <DIPHTONG> - decrease by 2
.proc Sbdeb_AdjustLengths
    ; LENGTHEN VOWELS PRECEDING PUNCTUATION
    ;
    ; Search for punctuation. If found, back up to the first vowel, then
    ; process all phonemes between there and up to (but not including) the punctuation.
    ; If any phoneme is found that is a either a fricative or voiced, the duration is
    ; increased by (length * 1.5) + 1

    ldx #$00                            ; X = 0
Lbded:                                  ; loop {
    ldy L9be0_phonemeIndex,x            ;   Y = phonemeIndex[X];
    cpy #$ff
    bne Lbdf7                           ;   if (Y = $ff)
Lbdf4:
    jmp Lbe39                           ;     break

Lbdf7:
    lda L9fda_flags2,y                  ; Skip if not punctuation
    and #Flags2::Punctuation
    bne Lbe02                           ;   if (!(flags2[Y] & Punctuation)) {
    inx                                 ;     X++
    jmp Lbded                           ;     continue
                                        ;   }
Lbe02:
    stx mem66_pos                       ;   pos = X
    ; Walk backwards until we find the preceeding vowel
Lbe04:                                  ;   do {
    dex                                 ;     X--
    beq Lbdf4                           ;     if (X == 0) break
    ldy L9be0_phonemeIndex,x            ;     Y = phonemeIndex[X]
    lda L9f8c_flags,y
    and #Flags1::Vowel
    beq Lbe04                           ;   } while (!(flags[Y] & Vowel))
Lbe11:                                  ;   loop {
    ldy L9be0_phonemeIndex,x            ;     Y = phonemeIndex[X]
    lda L9fda_flags2,y
    and #Flags2::Fricative
    beq Lbe22                           ;     if (flags2[Y] & Fricative) {
    lda L9f8c_flags,y
    and #Flags1::Voiced
    beq Lbe30                           ;       if (!(flags2[Y] & Voiced)) continue
Lbe22:                                  ;     }
    ; Change phoneme length to (length * 1.5) + 1
    lda L9ce0_phonemeLength,x
    sta mem56
    lsr a
    clc
    adc mem56
    adc #$01
    sta L9ce0_phonemeLength,x           ;     phonemeLength[X] += (phonemeLength[X] >> 1) + 1;
Lbe30:
    inx                                 ;     X++
    cpx mem66_pos                       ;
    bne Lbe11                           ;   } while (pos != X)
    inx                                 ;   X++
    jmp Lbded                           ; }

    ; Similar to the above routine, but shorten vowels under some circumstances
Lbe39:
    ldx #$00
    stx mem66_pos                       ; pos = 0
Lbe3d:
    ldx mem66_pos                       ; X = pos
    ldy L9be0_phonemeIndex,x            ; Y = phonemeIndex[X];
    cpy #$ff                            ; if (Y == $ff)
    bne Lbe47
    rts                                 ;   return

Lbe47:
    lda L9f8c_flags,y
    and #Flags1::Vowel
    bne Lbe51                           ; if (!(flags[Y] & Vowel)) {
    jmp Lbeb5                           ;   skip rule
Lbe51:                                  ; }
    ; Get next phoneme
    inx                                 ; X++
    ldy L9be0_phonemeIndex,x            ; Y = phonemeIndex[X];
    lda L9f8c_flags,y
    sta mem56                           ; mem56 = flags[Y]
    and #Flags1::Consonant
    beq Lbe91                           ; if (flags[index] & $40)) goto not a consonant (??)

    ; Got here if not <VOWEL>
    lda mem56
    and #Flags1::Voiced
    beq Lbe77                           ; if ((flags[index] & $4) == 0) goto unvoiced
    dex                                 ; X--
    lda L9ce0_phonemeLength,x           ; A = phonemeLength[X]
    sta mem56                           ; mem56 = A
    lsr a
    lsr a
    clc
    adc mem56
    adc #$01
    sta L9ce0_phonemeLength,x           ; phonemeLength[X] += (phonemeLength[X] >> 2) + 1
Lbe74_continue:
    jmp Lbf21_continue

Lbe77:
    ; Unvoiced
    ; *, .*, ?*, ,*, -*, DX, S*, SH, F*, TH, /H, /X, CH, P*, T*, K*, KX

    ; Not an unvoiced plosive?
    lda mem56
    and #$01
    beq Lbe74_continue                  ; if (!(mem56 & $1)) continue

    ; P*, T*, K*, KX

    ; RULE: <VOWEL> <UNVOICED PLOSIVE>
    ; <VOWEL> <P*, T*, K*, KX>

    ; Move back
    dex

    ; Decrease length by 1/8th
    lda L9ce0_phonemeLength,x
    tay
    lsr a
    lsr a
    lsr a
    sta mem56                           ; mem56 = phonemeLength[X] >> 3
    sec
    tya
    sbc mem56
    sta L9ce0_phonemeLength,x           ; phonemeLength[X] -= mem56
    jmp Lbf21_continue                  ; continue

Lbe91: ; not_a_consonant
    cpy #$12
    beq Lbe9c                           ; if (Y == 'RX') goto found_rx_or_lx
    cpy #$13
    beq Lbe9c                           ; if (Y == 'LX') goto found_rx_or_lx
Lbe99:
    jmp Lbf21_continue                  ; continue

Lbe9c:                                  ; found_rx_or_lx
    ; Is next phoneme a consonant?
    inx                                 ; X++
    ldy L9be0_phonemeIndex,x            ; Y = phonemeIndex[X];
    lda L9f8c_flags,y
    and #Flags1::Consonant
    beq Lbe99                           ; if (!(flags[index] & $40)) continue

    ; RULE: <VOWEL> RX | LX <CONSONANT>

    ; Decrease length of vowel by 1 frame
    ldx mem66_pos
    lda L9ce0_phonemeLength,x
    sec
    sbc #$01
    sta L9ce0_phonemeLength,x           ; phonemeLength[X]--
    jmp Lbf21_continue                  ; continue

    ; RULE: <NASAL> <STOP CONSONANT>
    ;       Set punctuation length to 6
    ;       Set stop consonant length to 5
Lbeb5:
    lda L9fda_flags2,y
    and #Flags2::Nasal
    beq Lbed8                           ; if (!(flags2[index] & $8) skip rule
    ; Is nasal.  Check if next phoneme a stop consonant?
    inx                                 ; X++
    ldy L9be0_phonemeIndex,x            ; Y = phonemeIndex[X];
    lda L9f8c_flags,y
    and #Flags1::StopConsonant
    bne Lbeca                           ; if (!flags[index] & $2)
Lbec7:
    jmp Lbf21_continue                  ;   continue

    ; B*, D*, G*, GX, P*, T*, K*, KX
Lbeca:
    ; Set stop consonant length to 6
    lda #$06
    sta L9ce0_phonemeLength,x           ; phonemeLength[X] = $6

    ; Set nasal length to 5
    dex
    lda #$05
    sta L9ce0_phonemeLength,x           ; phonemeLength[X - 1] = $5
    jmp Lbf21_continue                  ; continue

    ; WH, R*, L*, W*, Y*, Q*, Z*, ZH, V*, DH, J*, B*, D*, G*, GX

    ; RULE: <VOICED STOP CONSONANT> {optional silence} <STOP CONSONANT>
    ;       Shorten both to (length/2 + 1)
Lbed8:
    ; (voiced) stop consonant?
    lda L9f8c_flags,y
    and #Flags1::StopConsonant
    beq Lbf05                           ; if (!(flags[index] & 2)) goto ??

    ; B*, D*, G*, GX

    ; Move past silence
Lbedf:                                  ; do {
    inx                                 ;   X++
    ldy L9be0_phonemeIndex,x            ;   Y = phonemeIndex[X]
    beq Lbedf                           ; } while(Y == 0)

    ; If another stop consonant, move ahead
    lda L9f8c_flags,y
    and #Flags1::StopConsonant
    beq Lbec7                           ; if ((flags[Y] & $2) == 0) continue

    ; Shorten the prior phoneme length to (length/2 + 1)
    lda L9ce0_phonemeLength,x
    lsr a
    clc
    adc #$01
    sta L9ce0_phonemeLength,x           ; phonemeLength[X] = (phonemeLength[X] >> 1) + 1;

    ; Also shorten this phoneme length to (length/2 +1)
    ldx mem66_pos                       ; X = pos
    lda L9ce0_phonemeLength,x
    lsr a
    clc
    adc #$01
    sta L9ce0_phonemeLength,x           ; phonemeLength[X] = (phonemeLength[X] >> 1) + 1;
Lbf02_continue:
    jmp Lbf21_continue

    ; WH, R*, L*, W*, Y*, Q*, Z*, ZH, V*, DH, J*, **

    ; RULE: <VOICED NON-VOWEL> <DIPHTONG>
    ;       Decrease <DIPHTONG> by 2

Lbf05:
    ; liquid consonant?
    lda L9fda_flags2,y
    and #Flags2::LiquidConsonant
    beq Lbf02_continue                  ; if (!(flags2[Y] & $10)) continue

    ; prior phoneme a stop consonant?
    dex
    ldy L9be0_phonemeIndex,x            ; Y = phonemeIndex[X - 1]
    lda L9f8c_flags,y
    and #Flags1::StopConsonant
    beq Lbf02_continue                  ; if (!(flags[Y] & $2)) continue

    ; Rule: <LIQUID CONSONANT> <DIPHTONG>
    ; decrease the phoneme length by 2 frames (20 ms)
    inx
    lda L9ce0_phonemeLength,x
    sec
    sbc #$02
    sta L9ce0_phonemeLength,x           ; phonemeLength[X] -= 2
Lbf21_continue:
    inc mem66_pos                       ; pos++
    jmp Lbe3d                           ; loop
.endproc
