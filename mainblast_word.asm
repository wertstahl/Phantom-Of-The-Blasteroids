;------ MAIN ------------------------------------------------------------- CBMprgStudio
;
;       (c)wertstahl 02/2018 #it is a beautiful day, and then you die.
;
;       Phantom of the Blasteroids
;
;------ BASIC - bootloader ------------------------------------------------------------

*=$0801
            byte  $0c,$08,$01,$00,$9e,$20,$32,$30,$36,$32,$00,$00,$00; 0 sys2062 ($080e)

;--------------------------------------------------------------------------------------

*=$080e
            jmp appstart

;--------------------------------------------------------------------------------------

scanline1   =     #$fe
border_col  =     #$00
screen_col  =     #$00
cls_col     =     #$00
clschar     =     #$20
clscol      =     #$01
screen_01   =     $0400
cram_01     =     $d800

multi0_col  =     #$04
multi1_col  =     #$08
sprexcol0   =     #$03
sprexcol1   =     #$04

; cols
; 00 blk  01 wht  02 red  03 cyn  04 pur  05 grn  06 blu  07 yel 
; 08 lbr  09 brn  0A pnk  0B dgr  0C mgr  0D lgn  0E lbl  0F lgr

;--------------------------------------------------------------------------------------

appstart    cld
            sei

            lda   #$01
            sta   $d01a
            sta   $dc0d

            lda   #$35              ;gibe all di ram!
            sta   $01

            lda   #$00
            sta   $d011       
            lda   #$02
            sta   $d020       
            sta   $d021

            jsr   gfxtools          ;--> gfxtools

            jsr   mkshottab

;            lda   #$01        
;            jsr   music

            lda   #$1b
            sta   $d011

            lda   #<mainloop
            sta   $FFFE
            ldy   #>mainloop
            sty   $FFFF

            lda   scanline1   ;first interrupt line
            sta   $d012
            lda   #$7f
            sta   $dc0d
            lda   $dc0d

            cli

idle        jmp   idle        

;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------

mainloop    pha
            txa
            pha
            tya
            pha
            inc   $d019

            lda   #$01
            sta   $d020
            sta   $d021

            ;-----------------------------------------------------------------------------
            ;-----------------------------------------------------------------------------
            ;-----------------------------------------------------------------------------

            jsr   joycontrol2    ;check joystick port 2 + update coordinates

            jsr   spranim     ;animate ship

            jsr   orb1_anim   ;animate targeting orb

            jsr   spr_update  ;update sprite positions

            jsr   shozfired

            ;-----------------------------------------------------------------------------
            ;-----------------------------------------------------------------------------
            ;-----------------------------------------------------------------------------

            lda   screen_col
            sta   $d021
            lda   #$0f;border_col
            sta   $d020


            lda   scanline1   ;set the rasterline again
            sta   $d012

            lda   $d011       
            and   #%01111111  ;make sure the rasterline is not assigned to 
                              ;the lower part of the screen.
            sta   $d011       


            pla
            tay
            pla
            tax
            pla

            rti
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------

;----------- screen init

gfxtools    jsr   cls

            jsr   scrcol

            jsr   setfont

            jsr   initsprites

            jsr   removeshot

            rts

;----------------------------------------------------------------------

scrcol      ;-------------------------
            
            lda   border_col
            sta   $d020  
            lda   screen_col     
            sta   $d021       

            lda   multi0_col
            sta   $d022
            lda   multi1_col
            sta   $d023

            rts

;-----------------------------------------------------

cls         ldy   #$00        ; clear screen & color

            lda   clschar
clr         sta   screen_01,y     
            sta   screen_01+256,y     
            sta   screen_01+512,y     
            iny
            bne   clr        

            lda   clscol
clr1        sta   cram_01,y     
            sta   cram_01+256,y     
            sta   cram_01+512,y     
            iny
            bne   clr1

            ldx   #$e8
            lda   clschar
clr2        sta   screen_01+768,y
            iny
            dex
            bne   clr2        

            ldx   #$e9
            lda   clscol
clr3        sta   cram_01+768,y
            dey
            dex
            bne   clr3        

            rts

;-----------------------------------------------------

setfont     lda   $DD00
            and   #%11111100
            ora   #%00000011 ;<- your desired VIC bank value
            sta   $DD00

            lda   #%00011110 ;screenmem at $0400, font at $3800
            sta   $d018      

            lda   #%11011000 ;multicolor mode is on (bit4 of 0-7)
            sta   $d016
            rts            

;-----------------------------------------------------

initsprites ldx   #$00
            ldy   #$00

spr_stuplp  lda   sprx,x     
            sta   $d000,y     
            
            lda   spry,x
            sta   $d001,y
     
            lda   sprcol,x
            sta   $d027,x     
            
            lda   initbank,x
            sta   screen_01+1016,x     

            lda   sprexcol0        
            sta   $d025       
            lda   sprexcol1        
            sta   $d026
            
            iny
            iny
            inx
            cpx   #$08        
            bne   spr_stuplp

            lda   sprx+01
            sta   playerx
            lda   spry+1
            sta   playery

            lda   #%00000000  ;sprite x bit 8 
            sta   $d010                  

            lda   #%00000000  ;sprites infront gfx     
            sta   $d01b 
            lda   #%00000110  ;sprites multicol
            sta   $d01c       
            lda   #%00000000  ;xstretch
            sta   $d01d       
            lda   #%00000000  ;ystretch
            sta   $d017       
            lda   #%00001111  ;sprite enable
            sta   $d015       
            rts


sprx        byte  $80,$80,$80,$30, $00,$00,$00,$00
spry        byte  $50,$50,$50,$30, $00,$00,$00,$00
sprcol      byte  $05,$01,$01,$02, $05,$06,$07,$08
initbank    byte  $a8,$a0,$c8,$d0, $9f,$9f,$9f,$9f

;spriteslots:
; 00 = turret ($a8-c7)
; 01 = ship   ($a0-a7)
; 02 = orb    ($c8-cf)
; 03 = shot   ($d0)

;-----------------------------------------------------------------------------

ship1count  byte  $00
orb1count   byte  $00
25hz        byte  $00

endanim     rts
spranim     lda   25hz
            eor   #$01
            sta   25hz
            beq   endanim

            ;-------------------
ship1_anim  ; ship gfx animation

            inc   ship1count
            lda   ship1count
            cmp   #$08
            bne   nsh1rs
            lda   #$00
            sta   ship1count
nsh1rs      clc
            adc   initbank+1
            sta   screen_01+1017

            ;-------------------
orb1_anim   ; orb gfx animation

            inc   orb1count
            lda   orb1count
            cmp   #$08
            bne   nor1rs
            lda   #$00
            sta   orb1count
nor1rs      clc
            adc   initbank+2
            sta   screen_01+1018

            ;-----------------------
            ; orb position animation

                  ;--------------------------
                  ; paused when shot is fired

                  lda shotstarre
                  beq goonorbit
                  dec shotstarre
                  jmp doturret

                  ;--------------------------

goonorbit   ldx   orbpospnt
            dex
            cpx   #$ff
            bne   oxpnrs
            ldx   #$3f
oxpnrs      stx   orbpospnt
            
            txa
            ldx   #$01
            jsr   hex2screen
            ldx   orbpospnt



            lda   orbx+16,x   ;+16 = sin cosine offset
            sec
            sbc   #$18
            clc
            adc   spry+1 
            sta   spry+2 


            ;-----------------------------------------

            lda   orbx,x
            sec
            sbc   #$18
            clc   
            adc   sprx+1 
            sta   sprx+2

            ;-----------------------

            lda   player8
            and   #%11111011        ;initially remove ext bit 
            sta   player8

            lda   player8
            and   #$01
            beq   shiplo

            lda   #$90
            sec
            sbc   sprx+2
            bcc   doturret

            lda   player8
            ora   #%00000100        ;set orb x-ext
            sta   player8
            jmp   doturret

shiplo      lda   sprx+1
            sec
            sbc   #$c0        ;don`t ask me why.
            bcc   doturret

            lda   sprx+2
            bmi   doturret

            lda   player8
            ora   #%00000100        ;set orb x-ext
            sta   player8
            
            ;-----------------------
            ; turret angle animation
            ; (relative to orb position)

doturret    lda   orbpospnt
            eor   #$3f
            lsr

            sec
            sbc   #$08
            and   #$1f

            clc
            adc   initbank
 
            sta   screen_01+1016

            rts

orbbit      byte  $0

;-----------------------------------------------------------------------------

shotx       byte  $00
shothi      byte  $00
shoty       byte  $00
shotavail   byte  $00
shotrechrg  byte  $03
shotflies   byte  $00
shotdir     byte  $00
shotstep    byte  $00
shotrspeed  = #$01
shotavailt  = #$08
shotspeed   byte  $03
shotstarre  byte  $00
shotstarreval = #$40



shotex      rts

shozfired   lda   shotflies
            bne   shot_do

            dec   shotrechrg
            bne   shotex      
            lda   shotrspeed
            sta   shotrechrg

            lda   shotavail
            cmp   shotavailt
            beq   tryfire

            inc   shotavail
            lda   shotavail
            cmp   shotavailt
            bne   shotex

tryfire     lda   trig_fire
            beq   shotex

            ;--------------------------------------------------------
            ; shot can be fired, setup bullet

            ; get angle

            lda   orbpospnt
            eor   #$3f
            lsr                     ;fetch shot direction

            sec
            sbc   #$07              ;correct angle a bit 
            bcs   notfpullup
            adc   #$1f              ;correct the correction maybe 

notfpullup  sta   shotdir           ;store value

            ;---------

            ;get shot origin position

            lda   player8
            and   #%0000001        ;get hibit of turret (shot originates from turret!)
            asl
            asl
            asl
            sta   shothi            ;shothi stores initial hi-ext value
            ora   player8
            sta   player8

            lda   sprx            ;get turret position
            clc
            adc   #$03
            sta   shotx
            bcc   getshoty

            lda   #%00001000
            sta   shothi            ;shothi stores initial hi-ext value
            ora   player8
            sta   player8

getshoty    lda   spry
            sta   shoty

            ;-----
            
            ; finish up

            lda   #$00
            sta   shotavail
            inc   shotflies

            lda   shotstarreval
            sta   shotstarre

            lda   #$d0
            sta   initbank+3
            sta   screen_01+1019
            jmp   shotex            ; done


            ;----------------------------------------------------

            ; process the shot 

shot_do     lda   shotdir     ;load shot angle value
            asl               ;/2
            asl               ;/4
            asl               ;/8

            clc
            adc   shotstep    ;add current step in shotladder

            tax

            ;---------

            ; do complicated evaluation of direction and set hibit accordingly

            lda   shottabaddrhi
            
lda   shottabx,x  ;fetch respective step value
            jsr   turbobullet

            clc

            adc   shotx       ;CARRY IS BEING SET OR CLEARED NORMALLY
            sta   shotx       ;calculate new shot position
            tay

            lda   shottabx,x  ;fetch respective step value again
            bpl   xadd        ;is positve? -> addition
        ;   bmi   xsub        ;is negative? -> subtraction

xsub        lda   shothi      ;is the shot in extended space?
            bne   xsubinhigh  ;yes, check hi values
                              ;no, check low values

                              ;are we in low and the carry is set? 
                              ;underflow -> should never happen

xsubinlow   lda   shotx
            sec
            sbc   #$09        ;are we below f? (inside border) 
            bcc   removeshot  ;yes removeshot
         ;  bcs   doshoty     ;no, just continue 
            ;--------

xsubinhigh  bcs   doshoty     ;did we wrap to low? no - just shoot
flipxhi     lda   shothi      ;else flip the hi bit
            eor   #%00001000
            sta   shothi

            lda   player8
            and   #%11110111
            ora   shothi
            sta   player8
            jmp   doshoty     ;and go on

            ;---------

xadd        lda   shothi      ;are we in high?
            bne   xaddinhi    ;yes
       ;    beq   xaddinlo    ;no 

xaddinlo    bcs   flipxhi     ;if we wrapped to high, flip the bit!
            bcc   doshoty     ;didnt wrap, just go on

            inc   $d021

xaddinhi    lda   shotx       ;get shot x value
            sec
            sbc   #$4f        ;are we in valid space?
            bcs   removeshot  ;nope, remove 
        ;   bcc   doshoty     ;yes, go on
            ;-----------

            ;vertical shot flight processing

doshoty     lda   shottaby,x
            jsr   turbobullet
            clc   
            adc   shoty
            sta   shoty

            sec
            sbc   #$f0        ;shot position still valid?
            bcc   endshotfl   ;yes, so we're done

            ;--------------
                              ;carry is set, position seems higher than f0
                              ;thus remove the shot 
removeshot  lsr   shotflies
            lda   #$00
            sta   shotx
            sta   shoty
            sta   sprx+3
            sta   spry+3
            sta   shotstep
            lda   #$9f
            sta   initbank+3
            sta   screen_01+1019

            lda   player8
            and   #%11110111
            sta   player8

            rts

            ;-----

endshotfl   lda   shotx
            sta   sprx+3
            lda   shoty
            sta   spry+3

            inc   shotstep
            lda   shotstep
            cmp   #$08
            bne   endshot
            lda   #$00
            sta   shotstep
endshot     rts

            ;-----


            ;----------------------------------------------------

            ; make the shot faster.

turbobullet bpl   fastplus

fastminus   sec
            sbc   shotspeed
            rts
fastplus    clc
            adc   shotspeed
            rts




;-----------------------------------------------------------------------------

spr_update  ldx   #$00
            ldy   #$00

s_u_lp      lda   sprx,x     
            sta   $d000,y     
            
            lda   spry,x
            sta   $d001,y
     
            iny
            iny
            inx
            cpx   #$08        
            bne   s_u_lp

            lda   player8
            sta   $d010

            rts

;-----------------------------------------------------------------------------

orbpospnt   byte  $3f

orbx        byte  $00,$00,$01,$01, $02,$03,$05,$07
            byte  $09,$0b,$0d,$0f, $11,$13,$15,$17
            byte  $19,$1b,$1d,$1f, $21,$23,$25,$27
            byte  $29,$2b,$2c,$2d, $2e,$2e,$2f,$2f

            byte  $2f,$2f,$2e,$2e, $2d,$2c,$2b,$29
            byte  $27,$25,$23,$21, $1f,$1d,$1b,$19
            byte  $17,$15,$13,$11, $0f,$0d,$0b,$09
            byte  $07,$05,$03,$02, $01,$01,$00,$00

            byte  $00,$00,$01,$01, $02,$03,$05,$07
            byte  $09,$0b,$0d,$0f, $11,$13,$15,$17
            byte  $19,$1b,$1d,$1f, $21,$23,$25,$27
            byte  $29,$2b,$2c,$2d, $2e,$2e,$2f,$2f

;-----------------------------------------------------------------------------
; number to ascii

            ; pass hex number in a
            ; pass x offset for printing in x
            ; y is unaffected
            ; a returned as is

            ; A = *SCII-code
hex2screen  pha ;1
            stx h2s_tmp1
            sty h2s_tmp2

            ldy #$2f
            ldx #$3a
            sec
h2s_lp1     iny
            sbc #100
            bcs h2s_lp1
h2s_lp2     dex
            adc #10
            bmi h2s_lp2
            adc #$2f

            pha ;2
            txa
            pha ;3
            tya
            pha ;4

            ldx h2s_tmp1
            pla ;3
            sta screen_01,x
            pla ;2
            sta screen_01+1,x
            pla ;1
            sta screen_01+2,x

            ldy h2s_tmp2
            pla ;0
            rts
            text  'tiny .a to ascii by white flame / codebase64'            
h2s_tmp1    byte 0
h2s_tmp2    byte 0

;------------------------------------------------------  

; on-screen debug output

xy_deb_ug   

            jsr   hex2screen


            rts               ;a,x and y unaffected


;--------------------------------------------------------------------------------------
; Freudenstab

joy2last    byte  $00
trig_fire   byte  $00
stick_l     byte  $00
stick_r     byte  $00
stick_d     byte  $00
stick_u     byte  $00
player8     byte  $00
playerx     byte  $00
playery     byte  $00
min_x       byte  $1c
max_x       byte  $40   ; maximum x position respecting x-pos hi bit (=+$ff)
min_y       byte  $33
max_y       byte  $e6
posdown     byte  $00
posup       byte  $00
posleft     byte  $00
posright    byte  $00
player_dir  byte  $00
movespeed   byte  $03

;--------------------------------------------------------------------------------------
; CHECK JOYSTICK PORT II
;--------------------------------------------------------------------------------------

joycontrol2 lda   #$00
            sta   stick_l
            sta   stick_r
            sta   stick_u
            sta   stick_d
            sta   trig_fire

            jsr   do_chkj2

            lda   stick_r     ;order is inverted because originally the map was scrolled
            bne   st2r        ;opposite to the player movement
            jsr   do_pl_left
st2r        lda   stick_l
            bne   st2d
            jsr   do_pl_rite
st2d        lda   stick_u
            bne   st2u
            jsr   do_pl_down
st2u        lda   stick_d
            bne   stnomov
            jsr   do_pl_up
stnomov

            lda   playerx
            sta   sprx+1      ;ship x
            sta   sprx        ;cannon x
            lda   playery
            sta   spry+1      ;ship y
            sta   spry        ;cannon y

            rts

hackd       inc   $d020
            jmp   hackd

;--------------------------------------------------------------------------------------

do_chkj2    lda   $dc00           ;check joystick II
            sta   joy2last
            tax

            and   #%00010000      ;joy2 fire
            bne   nojf  
            inc   trig_fire

nojf        txa 
            and   #%00000100      ;joy2 left
            bne   nojl
            inc   stick_l  

nojl        txa
            and   #%00001000      ;joy2 right
            bne   nojr
            inc   stick_r  

nojr        txa
            and   #%00000010      ;joy2 down
            bne   nojd
            inc   stick_d 

nojd        txa
            and   #%00000001      ;joy2 up
            bne   chkj2_exit
            inc   stick_u    

chkj2_exit  rts



;--------------------------------------------------------------------------------------

do_pl_left  lda   player8     ;check if xext bit is set     
            and   #%00000011  ;player xext is bit 2
            bne   px_do_dec   ;it is set! just decrease!

            lda   playerx     ;it is not set! check if we hit left border!
            sec
            sbc   min_x       
            bcs   px_do_dec   ;workaround to avoid BPL
            lda   playerx     
            jmp   px_done_dec ;minimum reached= no action (direction must be set anyways!)
            
px_do_dec   lda   playerx
            sec
            sbc   movespeed   ;if we go to minus, carry will be cleared 
            bcs   px_done_dec ;no minus, all good.

            pha               ;we went minus. xext must be unset
            lda   player8     
            and   #%11111100  ;kill xext bit
            sta   player8
            pla
            
px_done_dec sta   playerx

c_j_l       lda   posleft        
            sta   player_dir  

            rts

;--------------------------------------------------------------------------------------

do_pl_rite  lda   player8     ;check if xext bit is set     
            and   #%00000011  ;player xext is bit 2
            beq   px_do_inc   ;its not set! just increase!

            ldx   playerx     ;attention
            txa               ;this is an important workaround to some
            sec               ;crazy bmi behaviour
            sbc   max_x       
            bcc   px_do_inc   
            jmp   px_done_inc ;maximum reached= no action (direction must be set anyways!)
            
px_do_inc   lda   playerx
            clc
            adc   movespeed   ;if we go to minus-wrap, carry will be set 
            tax               ;workaround
            bcc   px_done_inc ;no wrap, all good.

            lda   player8     
            ora   #%00000011  ;set xext bit
            sta   player8            
            
px_done_inc stx   playerx

c_j_r       lda   posright       
            sta   player_dir

            rts

;--------------------------------------------------------------------------------------

do_pl_down  ldx   playery     ;attention
            txa               ;this is an important workaround to some
                              ;
            sec               ;crazy bmi behaviour
            sbc   max_y       ;        
            bcc   py_do_inc   ;
            jmp   py_done_inc
            
py_do_inc   lda   playery
            clc
            adc   movespeed   
            tax               ;workaround

py_done_inc stx   playery     
            
c_j_d       lda   posdown     
            sta   player_dir  
            rts   

;--------------------------------------------------------------------------------------

do_pl_up    lda   playery
            sec
            sbc   min_y       
            bcs   py_do_dec   ;workaround to avoid BPL 

            lda   playery     ;prior sbc
            jmp   py_done_dec
            
py_do_dec   lda   playery     
            sec
            sbc   movespeed   

py_done_dec sta   playery

c_j_u       lda   posup     
            sta   player_dir  
            rts   

;--------------------------------------------------------------------------------------

*=$2200

shottabaddrhi     byte $00,$00,$00,$00,$00,$00,$00,$00
                  byte $00,$00,$00,$00,$00,$00,$00,$00
                  byte $00,$00,$00,$00,$00,$00,$00,$00
                  byte $00,$00,$00,$00,$00,$00,$00,$00
                  byte $00,$00,$00,$00,$00,$00,$00,$00
                  byte $00,$00,$00,$00,$00,$00,$00,$00
                  byte $00,$00,$00,$00,$00,$00,$00,$00
                  byte $00,$00,$00,$00,$00,$00,$00,$00

shottabaddrlo     byte $00,$00,$00,$00,$00,$00,$00,$00
                  byte $00,$00,$00,$00,$00,$00,$00,$00
                  byte $00,$00,$00,$00,$00,$00,$00,$00
                  byte $00,$00,$00,$00,$00,$00,$00,$00
                  byte $00,$00,$00,$00,$00,$00,$00,$00
                  byte $00,$00,$00,$00,$00,$00,$00,$00
                  byte $00,$00,$00,$00,$00,$00,$00,$00
                  byte $00,$00,$00,$00,$00,$00,$00,$00

;--------------------------------------------------------------------------------------

mkshottab   ldx   #$00

            lda   #>shottabx        ;shottab hi adress
            sta   shottabaddrhi,x   ;first placement
            tay

            lda   #<shottabx        ;shottab lo adress
            sta   shottabaddrlo,x   ;first placement

            inx

mkaddrloop  clc
            adc   #$08
            bcc   mkshtt_nup

            iny

mkshtt_nup  sta   shottabaddrhi,x

            inx   
            cpx   #$40
            bne   mkaddrloop

            rts


;--------------------------------------------------------------------------------------
; EOF

*=$2400
shottabx    

            byte $00,$00,$00,$00,$00,$00,$00,$00 
            byte $00,$00,$00,$00,$00,$01,$00,$00
            byte $00,$00,$00,$01,$00,$00,$00,$00
            byte $00,$00,$00,$00,$01,$00,$00,$01
            byte $00,$00,$01,$00,$00,$01,$00,$00
            byte $01,$00,$00,$01,$00,$00,$01,$00
            byte $00,$01,$00,$01,$00,$00,$01,$00
            byte $01,$00,$01,$00,$00,$01,$00,$01
            byte $01,$00,$00,$01,$01,$00,$00,$01
            byte $00,$01,$01,$00,$01,$01,$00,$01
            byte $00,$01,$01,$00,$01,$00,$01,$01
            byte $01,$01,$00,$01,$01,$01,$00,$01
            byte $01,$01,$00,$01,$01,$00,$01,$01
            byte $01,$00,$01,$01,$01,$00,$01,$01
            byte $01,$01,$01,$00,$01,$01,$01,$00
            byte $01,$01,$01,$01,$00,$01,$01,$01 ;
            byte $01,$01,$01,$01,$01,$01,$01,$01
            byte $01,$01,$01,$01,$00,$01,$01,$01
            byte $01,$01,$01,$00,$01,$01,$01,$00
            byte $01,$00,$01,$01,$01,$00,$01,$01
            byte $01,$01,$00,$01,$01,$01,$00,$01
            byte $01,$01,$00,$01,$01,$00,$01,$01
            byte $00,$01,$01,$00,$01,$00,$01,$01
            byte $00,$01,$01,$00,$01,$01,$00,$01
            byte $01,$00,$00,$01,$01,$00,$00,$01
            byte $01,$00,$01,$00,$00,$01,$00,$01
            byte $00,$01,$00,$01,$00,$00,$01,$00
            byte $01,$00,$00,$01,$00,$00,$01,$00
            byte $00,$00,$01,$00,$00,$01,$00,$00
            byte $01,$00,$00,$00,$01,$00,$00,$00
            byte $00,$00,$00,$01,$00,$00,$00,$00
shottaby    byte $00,$00,$00,$00,$00,$00,$00,$00 ;
            byte $00,$00,$00,$00,$00,$ff,$00,$00
            byte $00,$00,$00,$ff,$00,$00,$00,$00
            byte $00,$00,$00,$00,$ff,$00,$00,$ff
            byte $00,$00,$ff,$00,$00,$ff,$00,$00
            byte $ff,$00,$00,$ff,$00,$00,$ff,$00
            byte $00,$ff,$00,$ff,$00,$00,$ff,$00
            byte $ff,$00,$ff,$00,$00,$ff,$00,$ff
            byte $ff,$00,$00,$ff,$ff,$00,$00,$ff
            byte $00,$ff,$ff,$00,$ff,$ff,$00,$ff
            byte $00,$ff,$ff,$00,$ff,$00,$ff,$ff
            byte $ff,$ff,$00,$ff,$ff,$ff,$00,$ff
            byte $ff,$ff,$00,$ff,$ff,$00,$ff,$ff
            byte $ff,$00,$ff,$ff,$ff,$00,$ff,$ff
            byte $ff,$ff,$ff,$00,$ff,$ff,$ff,$00
            byte $ff,$ff,$ff,$ff,$00,$ff,$ff,$ff
            byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ;
            byte $ff,$ff,$ff,$00,$ff,$ff,$ff,$ff
            byte $ff,$ff,$ff,$ff,$00,$ff,$ff,$ff
            byte $ff,$ff,$ff,$00,$ff,$ff,$ff,$00
            byte $ff,$00,$ff,$ff,$ff,$00,$ff,$ff
            byte $ff,$ff,$00,$ff,$ff,$ff,$00,$ff
            byte $ff,$ff,$00,$ff,$ff,$00,$ff,$ff
            byte $00,$ff,$ff,$00,$ff,$00,$ff,$ff
            byte $00,$ff,$ff,$00,$ff,$ff,$00,$ff
            byte $ff,$00,$00,$ff,$ff,$00,$00,$ff
            byte $ff,$00,$ff,$00,$00,$ff,$00,$ff
            byte $00,$ff,$00,$ff,$00,$00,$ff,$00
            byte $ff,$00,$00,$ff,$00,$00,$ff,$00
            byte $00,$00,$ff,$00,$00,$ff,$00,$00
            byte $ff,$00,$00,$00,$ff,$00,$00,$00
            byte $00,$00,$00,$ff,$00,$00,$00,$00
            byte $00,$00,$00,$00,$00,$ff,$00,$00 ;

;------------------------------------------------------------------------


*=$27c0     ;emptysprite 9f
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

*=$2800
sprites     ;first sprite a0
incbin      "blasteroids_2_spr.prg",2

*=$3800
font        
incbin      "blasterfont.bin"


