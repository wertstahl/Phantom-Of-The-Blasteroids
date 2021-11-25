;-----------------------------------------------------------------------------
; Intro
;-----------------------------------------------------------------------------

i_scanline  = #$07
i_scanline2 = #$9a

*=$4000
incbin      "/res/phantom-map.bin"
*=$40c8
incbin      "/res/otblasteroids-map.bin"

*=$5000     ;emptysprite 40 -- 9f
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

*=$5040
i_sprite    ;first sprite 41 --- a0
incbin      "/res/blasteroids_2_spr.prg",2

*=$6000     
incbin      "/res/phantom.bin"

*=$6800
incbin      "/res/otblasteroids.bin"

*=$7000
muzak
incbin      "glitch47000.sid",$7e


;-----------------------------------------------------------------------------

*=$4400
            ;--------------------------------------------------
            ; simple polling rastersync routine
            ; align to some page so branches do not cross a page boundary and fuck up the timing
i_rsync
i_rlp1      cpx $d012
            bne i_rlp1
            jsr i_rcycles
            bit $ea
            nop
            cpx $d012
            beq i_rskip1
            nop
            nop
i_rskip1    jsr i_rcycles
            bit $ea
            nop
            cpx $d012
            beq i_rskip2
            bit $ea
i_rskip2    jsr i_rcycles
            nop
            nop
            nop
            cpx $d012
            bne i_ronecycle
i_ronecycle rts

i_rcycles   ldy #$06
i_rlp2      dey
            bne i_rlp2
            inx
            nop
            nop
            rts

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

            ; begin intro

i_intro     sei
            cld

            lda   #$00
            sta   $d011       

            lda   #$00
            sta   $d015

i_rsmsetup  lda   #$35              ;gibe all di ram!
            sta   $01

            jsr   i_cls

            ;-----------------------------
            ; display credits

            lda   #$01
i_textcol   sta   $da58,x
            sta   $dae7,x
            inx
            bne   i_textcol

            ldx   #$27
i_showmsg   lda   i_msg1,x
            sta   $06a8,x
            lda   i_msg2,x
            sta   $06d0,x
            lda   i_msg3,x
            sta   $0720,x
            dex
            bpl   i_showmsg

            jsr   i_gmode          ;show gamemode


            ;-----------------------------

            jsr   i_nitsprites
            jsr   removeshot1
            jsr   removeshot2

            ;-----------------------------


            lda   #$01        
            jsr   muzak

            lda   #$01
            sta   $d01a
            sta   $dc0d

            lda   #<i_mainloop
            sta   $FFFE
            ldy   #>i_mainloop
            sty   $FFFF

            lda   #$00
            sta   $d011 
            lda   i_scanline        ;first interrupt line
            sta   $d012

            lda   #$7f
            sta   $dc0d
            lda   $dc0d

            lda   #$00
            sta   i_state
            sta   i_trigfire1
            sta   i_trigfire2

            lda   #$01
            sta   intromode

            lda   i_min_x
            sta   min_x
            lda   i_max_x
            sta   max_x

            lda   i_blinkspeed
            sta   i_1upblink
            sta   i_2upblink

            lda   #$00
            sta   i_curt_stp

            lda   #$01
            sta   i_curt_act
            sta   i_lohz

            lda   #$00
            sta   $3fff

            lda   #$1b
            sta   $d011

            lda   #$01
            sta   shotrspeed
            
            cli

            ;--------------------------------------------------------------------------------------

i_idle      jsr   i_colorfx   ;roll title curtain + blink players

            lda   $dc01
            sta   keynew
            and   #%00010000      ;joy1 fire
            bne   i_nojf1
            lda   i_trigfire1
            bne   i_nojf1
            lda   #$01
            sta   i_trigfire1
            ldx   #$0b
i_oneup     lda   i_msg4,x          ;display -one up-
            ora   #$80
            sta   $07c0,x
            dex
            bpl   i_oneup

i_nojf1     lda   $dc00
            and   #%00010000        ;joy1 fire
            bne   i_contidle
            lda   i_trigfire2
            bne   i_contidle
            lda   #$01
            sta   i_trigfire2
            ldx   #$0b
i_twoup     lda   i_msg5,x          ;display -two up-
            ora   #$80
            sta   $07dc,x
            dex
            bpl   i_twoup

            ;-----------------------
            ;modechoice

i_contidle  lda   keynew
            cmp   i_dc0old
            beq   i_endidle
            sta   i_dc0old
            cmp   #$df
            bne   i_endidle

            lda   i_gamestyle
            eor   #$01
            sta   i_gamestyle

            jsr   i_gmode
      
            ;----------------------

i_endidle  jmp   i_idle

keynew      byte  0
i_dc0old    byte  1
i_gamestyle byte  0
i_trigfire1 byte  0
i_trigfire2 byte  0
i_state     byte  0
i_fwait     byte  $0f
i_mv25hz    byte $0
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------

i_mainloop  pha
            txa
            pha
            tya
            pha
            inc   $d019

            ;----------------------

            lda   i_state     ;which irq are we supposed to be?

            beq   i_upper     ;0 = upper

            cmp   #$01
            beq   i_middle    ;1 = middle

            jmp   i_lower     ;2 = lower

            ;-----------------------------------------------------------------------------
            ;-----------------------------------------------------------------------------
            ; upper half of logo


i_upper     lda   #$00
            sta   $d020
            sta   $d021

            lda   i_sprmulcol1_1
            sta   $d026

            lda   #%00000010  ;<- your desired VIC bank value
            sta   $DD00
            lda   #%00001000  ;screenmem at $4000, char1 at $6000
            sta   $d018  
            lda   #%11001000  ;hiresmode
            sta   $d016

            lda   $d011
            and   #%10000111
            ora   #%00011000  ;charmode 40 chars
            sta   $d011

                  ;------------------------------------------------


                  dec   i_fwait    ;delay triggering fire a bit
                  bne   i_reup

                  lda   #$40
                  sta   i_fwait

                  lda   #$01       ;trigger fire
                  sta   trig1_fire 
                  sta   trig2_fire

i_reup            
                  jsr   shoz1fired  ;work the shots
                  jsr   shoz2fired

                  jsr   i_spr_update  ;update sprite positions

                  jsr   i_12blink

                  ;-----------------------------------------------

            lda   #$01
            sta   i_state

            lda   #$5a
            sta   $d012

            jmp   i_rtirq

            ;-----------------------------------------------------------------------------
            ;-----------------------------------------------------------------------------
            ; second half of logo

i_middle    lda   #%00001010 ;screenmem at $4000, char1 at $6000
            sta   $d018  

            ;---------------------------------

                  ldx   #$07
i_sprbnkcpy       lda   screen_01+1016,x
                  sec
                  sbc   #$5f
                  sta   $4000+1016,x
                  dex
                  bpl   i_sprbnkcpy
                  
                  ;---------------------------------------

                  
                  jsr   orb1_rotl   ;move orb1
                  jsr   orb2_rotr   ;move orb2


                ;  lda   i_mv25hz    ;slow down movement 
                ;  eor   #$01
                ;  sta   i_mv25hz
                ;  beq   i_skipshipmove

                  jsr   do_pl1_left ;move ship1
                  jsr   do_pl2_rite ;move ship2

;i_skipshipmove
                  jsr   st1nomov    ;update player1 position
                  jsr   st2nomov    ;update player2 position

                  jsr   spr1anim    ;animate ship
                  jsr   orb1_anim   ;animate targeting orb

                  jsr   spr2anim    ;animate ship
                  jsr   orb2_anim   ;animate targeting orb

                  jsr   i_checkboth ;firebuttons

                  ;---------------------------------------

            lda   #$02
            sta   i_state     ;which irq phase

            lda   i_scanline2
            sta   $d012

            jmp   i_rtirq

            ;-----------------------------------------------------------------------------
            ;-----------------------------------------------------------------------------
            ;-----------------------------------------------------------------------------
            ; blue area text screen
     
i_lower     ldx   $d012
            inx   
            inx   
            jsr   i_rsync

            lda   #%00000011
            sta   $DD00
            lda   #%00011110 ;screenmem at $0400, font at $3800
            sta   $d018

            ldx   $d012
            inx   
            inx   
            jsr   i_rsync

            nop
            nop
            nop
            nop
            nop
            nop

            ldx   #$0b
            stx   $d020
            stx   $d021

            lda   i_sprmulcol1_2
            sta   $d026

            ;------------------- end sync 

            lda   $d01e
            sta   coll_reg

            jsr   collcheck1
            jsr   collcheck2

            lda   #$00
            sta   $d01e

            jsr   roomboom

            ;--------------------

            jsr   muzak+3

            lda   i_scanline   ;set the rasterline again
            sta   $d012

            lda   #$00
            sta   i_state     ;which irq phase

            ;-----------------------------------------------------------------------------
            ;-----------------------------------------------------------------------------
            ;-----------------------------------------------------------------------------

            ; end of irq

i_rtirq     pla
            tay
            pla
            tax
            pla

            rti

;---------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------
; check if both buttons have been pressed, if so : go game

i_checkboth lda   i_trigfire1
            asl
            ora   i_trigfire2
            cmp   #$03
            beq   i_run
i_w8trdy    rts
            
            ;------------------------------------------------

i_rdydone   byte  $0
i_run       lda   i_rdydone
            bne   i_do_run
            jmp   i_get_ready

i_do_run    sei

            lda   #$00
            sta   i_rdydone
            sta   i_grdycnt
            lda   i_grddlval
            sta   i_grdydly

            lda   #$00
            jsr   muzak
            lda   #$00
            sta   $d418

            lda   #$00
            sta   trig1_fire
            sta   trig2_fire

            jsr   removeshot1
            jsr   removeshot2

            lda   shotrspeed  ;reset shot recharge time counter
            sta   shot1rechrg
            sta   shot2rechrg

            lda   #$00
            sta   shot1avail
            sta   shot2avail

            jmp   appstart  


;---------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
; sprite position update, intro version, avoids certain y positions

i_wround    byte $0

i_spr_update  ldx   #$00
            ldy   #$00

i_s_u_lp    lda   sprx,x     
            sta   $d000,y     
            
            lda   spry,x
            stx   i_wround
            tax
                  ;sprite fucks timing up? just dont let it be there! haha!
            lda   i_wr_exclude,x
            sta   $d001,y

            txa
            ldx   i_wround

     
            iny
            iny
            inx
            cpx   #$08        
            bne   i_s_u_lp

            lda   players8
            sta   $d010

            rts

;--------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------

i_sprxini   byte  $40,$40,$40,$00, $30,$30,$30,$00
i_spryini   byte  $50,$50,$50,$00, $ca,$ca,$ca,$00
i_sprcol    byte  $0f,$01,$0f,$01, $0f,$01,$0f,$01

i_sprmulcol0   =     #$0c ;light color
i_sprmulcol1_1 =     #$0b ;dark color
i_sprmulcol1_2 =     #$00 ;dark color
; cols
; 00 blk  01 wht  02 red  03 cyn  04 pur  05 grn  06 blu  07 yel 
; 08 lbr  09 brn  0A pnk  0B dgr  0C mgr  0D lgn  0E lbl  0F lgr

;--------------------------------------------------------------------------------------
; sprite setup, intro version


i_nitsprites ldx   #$0f

i_niini     lda   i_sprxini,x
            sta   sprx,x
            dex
            bpl   i_niini

i_scolini   ldx   #$07
i_scolilp   lda   i_sprcol,x
            sta   sprcol,x
            dex
            bpl   i_scolilp

            ;-----------------------

            ldx   #$00
            ldy   #$00

i_spr_stuplp  lda   sprx,x     
            sta   $d000,y     
            
            lda   spry,x
            sta   $d001,y
     
            lda   sprcol,x
            sta   $d027,x     
            
            lda   initbank,x
            sta   screen_01+1016,x     

            lda   i_sprmulcol0        
            sta   $d025       
            lda   i_sprmulcol1_1        
            sta   $d026
            
            iny
            iny
            inx
            cpx   #$08        
            bne   i_spr_stuplp

            lda   sprx+1
            sta   player1x
            lda   spry+1
            sta   player1y

            lda   sprx+5
            sta   player2x
            lda   spry+5
            sta   player2y


            lda   players8    ;sprite x bit 8 
            sta   $d010                  

            lda   #%00000000  ;sprites infront gfx     
            sta   $d01b 
            lda   #%01100110  ;sprites multicol
            sta   $d01c       
            lda   #%00000000  ;xstretch
            sta   $d01d       
            lda   #%00000000  ;ystretch
            sta   $d017       
            lda   #%11111111  ;sprite enable
            sta   $d015       
            rts


;--------------------------------------------------------------------------------------
; intro clear screen

i_clscol1   =#$00
i_clscol2   =#$0f
i_clschar   =#$20

i_cls       ldy   #$00        ; clear screen & color

            lda   i_clschar
i_clr       sta   screen_01,y     
            sta   screen_01+256,y     
            sta   screen_01+512,y     
            iny
            bne   i_clr        

            lda   i_clscol1
i_clr1      sta   cram_01,y     
            sta   cram_01+256,y     
            sta   cram_01+512,y     
            iny
            bne   i_clr1

            ldx   #$e8
            lda   i_clschar
i_clr2      sta   screen_01+768,y
            iny
            dex
            bne   i_clr2        

            ldx   #$e9
            lda   i_clscol2
i_clr3      sta   cram_01+768,y
            dey
            dex
            bne   i_clr3        

            rts

;--------------------------------------------------------------------------------------
            ; intro color fx

            ; pull up color curtain for game title

i_curt_stp  byte $0
i_ccurtain  byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
            byte $0,$0,$0,$0,$0,$0,$0,$0,$b,$6,$e,$f,$f,$f,$f,$f,$f,$f,$f
            byte $f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f
            byte $f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f
            byte $f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f
            byte $f,$f,$f,$f,$f,$ff
i_curt_act  byte $0

i_1upblink  byte $0
i_2upblink  byte $0
i_blinkspeed = #$05
i_lohz      byte $1

i_odd       rts
i_colorfx   dec   i_lohz
            bne   i_odd
            lda   #$7f
            sta   i_lohz

            lda   i_curt_act        ;curtain in action
            beq   i_odd             ; no 

            ldx   i_curt_stp        ;check which curtainstep
            lda   i_ccurtain,x
            cmp   #$ff              ; last one?
            bne   i_rollcurt        ; not yet

            lda   #$00              ; last one, deactivate
            sta   i_curt_act
            rts                     

            ;--------

i_rollcurt  tay
            inc   i_curt_stp

            ldx   #$00
i_scrollcol lda   $d801,x           ;brutascroll
            sta   $d800,x
            lda   $d901,x
            sta   $d900,x
            lda   $da01,x
            sta   $da00,x
            inx
            bne   i_scrollcol

            tya                     ;insert color

            sta   $d823  ;
            sta   $d84a ; inserts tilted
            sta   $d871   ;
            sta   $d89b  ;
            sta   $d8c2    ;
            sta   $d8ec   ; 
            sta   $d913     ;+
            sta   $d93d    ;+
            sta   $d965      ;+
            sta   $d98f     ;+
            sta   $d9b6       ;+
            sta   $d9df      ;+
            sta   $da06        ;
            
            lda   #$00
            sta   $da2f
            sta   $da57


            rts

;----------------------------------------------------------------------------------------

            ;------------------ blink player 1 if fire was pressed

i_12blink   lda   i_trigfire1  ; 1up triggered? 
            beq   i_2bldo      ; nope check 2

            dec   i_1upblink
            bne   i_2bldo
            lda   i_blinkspeed
            sta   i_1upblink
            
            lda   i_clflp1+1
            eor   #$01
            sta   i_clflp1+1
i_clflp1    ldx   #$01
            lda   i_blcol,x

            ldx   #$0b
i_1bllp     sta   $dbc0,x
            dex
            bpl   i_1bllp

            ;------------------ blink player 2 if fire was pressed

i_2bldo     lda   i_trigfire2
            beq   i_bl_end
            
            dec   i_2upblink
            bne   i_bl_end
            lda   i_blinkspeed
            sta   i_2upblink

            lda   i_clflp2+1
            eor   #$01
            sta   i_clflp2+1
i_clflp2    ldx   #$01
            lda   i_blcol,x

            ldx   #$0b
i_2bllp     sta   $dbdc,x
            dex
            bpl   i_2bllp
            
i_bl_end    rts
i_blcol     byte  $0b,$0f

;--------------------------------------------------------------------------------------

i_gmode     lda   i_gamestyle
            bne   i_mode1

i_mode0     ldx   #$0b
i_modedsp0  lda   i_msg6,x          ;display -one up-
            ora   #$80
            sta   $07ce,x
            dex
            bpl   i_modedsp0
            rts

i_mode1     ldx   #$0b
i_modedsp1  lda   i_msg7,x          ;display -one up-
            ora   #$80
            sta   $07ce,x
            dex
            bpl   i_modedsp1
            rts

;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
                 ;                            .
                 ;0123456789abcdef0123456789abcdef01234567
                 ;0123456789012345678901234567890123456789
i_msg1      text ' (c)2018  code and design by wertstahl  '
i_msg2      text '         music by celticdesign          '
i_msg3      text '        press fire both players         '
i_msg4      text '** one up **'
i_msg5      text                             '** two up **'

i_msg6      text ' c',$3d,'  match  '
i_msg7      text ' c',$3d,'  1kill  '
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------


            ; after press fire display get ready (scroll in from right)

i_grddlval  = #$50
i_grdydly   byte $50
i_grdycnt   byte $0
i_get_ready ldx   i_grdycnt
            lda   i_msgrdy1,x
            cmp   #$ff
            bne   i_mk_rdy

            dec   i_grdydly
            bne   i_grdex

            lda   #$01
            sta   i_rdydone
i_grdex     rts

i_mk_rdy    ldx   #$00
i_grdlp1    lda   $06a9,x
            sta   $06a8,x
            lda   $06d1,x
            sta   $06d0,x
            lda   $06f9,x
            sta   $06f8,x
            lda   $0721,x
            sta   $0720,x
            lda   $0749,x
            sta   $0748,x
            inx
            cpx   #$27
            bne   i_grdlp1

            ldx   i_grdycnt
            lda   i_msgrdy1,x
            sta   $06cf
            lda   i_msgrdy2,x
            sta   $06f7
            lda   i_msgrdy3,x
            sta   $071f
            lda   i_msgrdy4,x
            sta   $0747
            lda   i_msgrdy5,x
            sta   $076f

            inc   i_grdycnt

            rts


;--------------------------------------------------------------------------------------
                 ;                            .
                 ;0123456789abcdef0123456789abcdef01234567
                 ;0123456789012345678901234567890123456789
i_msgrdy1   byte  $20,$20,$20,$20,$20,$20,$E9,$DF,$DF,$20,$20,$20,$20,$E9,$DF,$DF,$20,$20,$20,$E9,$DF,$DF,$20,$20,$20,$20,$E9,$DF,$DF,$20,$20,$DF,$DF,$20,$E9,$DF,$20,$E9,$E0,$20
            byte  $ff
i_msgrdy2   byte  $20,$20,$20,$20,$20,$E9,$E0,$69,$5F,$DF,$20,$20,$E9,$E0,$69,$5F,$DF,$20,$E9,$E0,$69,$5F,$DF,$20,$20,$E9,$E0,$69,$5F,$DF,$20,$E0,$69,$E9,$E0,$69,$20,$E0,$69,$20
i_msgrdy3   byte  $20,$20,$20,$20,$E9,$E0,$E0,$E0,$E0,$69,$20,$E9,$E0,$E0,$69,$20,$20,$E9,$E0,$69,$20,$E9,$E0,$20,$E9,$E0,$69,$20,$E9,$E0,$20,$E0,$E0,$E0,$69,$20,$E9,$69,$20,$20
i_msgrdy4   byte  $20,$20,$20,$E9,$E0,$69,$5F,$E0,$DF,$20,$E9,$E0,$69,$20,$20,$20,$E9,$E0,$E0,$E0,$E9,$E0,$69,$E9,$E0,$69,$20,$E9,$E0,$69,$E9,$E0,$69,$20,$20,$20,$20,$20,$20,$20
i_msgrdy5   byte  $20,$20,$E9,$E0,$69,$20,$E9,$E0,$69,$E9,$E0,$69,$E0,$E0,$69,$E9,$E0,$69,$20,$E9,$E0,$69,$E9,$E0,$69,$E0,$E0,$E0,$69,$E9,$E0,$69,$20,$20,$E9,$69,$20,$20,$20,$20

;-----------------------------------------------------------------------------------------
;EOF