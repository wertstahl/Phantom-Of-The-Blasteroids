;----------------------------------------------------------------------------
; Game Over Sequence
;----------------------------------------------------------------------------

gameover1   sei
            jsr   cls
            jsr   plotstat2

            ldx   #$27
gmovprnt    lda   pl2a,x
            sta   $0540,x
            lda   pl2b,x
            sta   $0568,x
            lda   pl2c,x
            sta   $0590,x
            lda   pl2d,x
            sta   $05b8,x
            lda   pl2e,x
            sta   $05e0,x
            lda   pl2f,x
            sta   $0608,x

            lda   pl1g,x
            sta   $0658,x

            lda   pl1h,x
            sta   $06a8,x

            dex
            bpl   gmovprnt

            lda   #$00
            sta   $d005
            lda   #$00
            sta   $d00d
            lda   #%11110000  ;xstretch
            sta   $d01d       
            lda   #%11110000  ;ystretch
            sta   $d017

            jmp   kaudaun


;----------------------------------------------------------------------------

gameover2   sei
            jsr   cls
            jsr   plotstat1

            ldx   #$27
gmov2prnt   lda   pl1a,x
            sta   $0540,x
            lda   pl1b,x
            sta   $0568,x
            lda   pl1c,x
            sta   $0590,x
            lda   pl1d,x
            sta   $05b8,x
            lda   pl1e,x
            sta   $05e0,x
            lda   pl1f,x
            sta   $0608,x

            lda   pl1g,x
            sta   $0658,x

            lda   pl1h,x
            sta   $06a8,x

            dex
            bpl   gmov2prnt

            lda   #$00
            sta   $d005
            lda   #$00
            sta   $d00d
            lda   #%00001111  ;xstretch
            sta   $d01d       
            lda   #%00001111  ;ystretch
            sta   $d017

            ldx   #$ff
            ldy   #$05
            jmp   kaudaun

;----------------------------------------------------------------------------
;----------------------------------------------------------------------------


kaudaun     lda   #$c  ;25 frames
            sta   timersecs

            lda   #$30
            sta   phase1secs

            lda   #$40
            sta   phase2secs

            ;---------------------------------
kauloop     lda   #$e0
vwaitg1     cmp   $d012
            bne   vwaitg1


            jsr   spr1anim    ;animate ship
            jsr   spr2anim    ;animate ship

            lda   #$00
            sta   spry+2 
            sta   sprx+2
            sta   spry+6
            sta   sprx+6

            jsr   spr_update  ;update sprite positions
            jsr   gamemuz+3

            ;-------------------------------

            dec   timersecs    ;$25 frames
            bne   vwaitg1

            lda   #$0c
            sta   timersecs

            lda   $dc01
            and   #%00010000      ;joy1 fire
            beq   bamagain

            lda   $dc00
            and   #%00010000      ;joy1 fire
            beq   bamagain
            

            ;-------------------------------
            ; phase 1 : for 8 seconds only display screen
            lda   phase1secs
            cmp   #$00
            bne   decph1

            jsr   colorfade
            jmp   overdecph1

decph1      dec   phase1secs

            ;-------------------------------
            ; phase 2 : fade screen out

overdecph1  dec   phase2secs
            bne   kauloop

            ;-------------------------------
            ; phase 3 : game over

            jmp   gover_ex

bamagain    lda   #$00
            sta   shaketheroom
            jmp   appstart


timersecs   byte  $0

phase1secs  byte  $0

phase2secs  byte  $0


;----------------------------------------------------------------------------


gover_ex    lda   #$01
            jsr   gamemuz
            lda   #$00
            sta   $d418

            lda   #0  ;xstretch
            sta   $d01d       
            lda   #0  ;ystretch
            sta   $d017

            jsr   boomreset

            jmp   i_intro


;----------------------------------------------------------------------------
;----------------------------------------------------------------------------

pl1a  byte  $20,$20,$E0,$E0,$E0,$20,$20,$20,$E0,$E0,$20,$20,$20,$20,$E0,$E0,$20,$20,$E0,$E0,$20,$20,$E0,$E0,$20,$E0,$E0,$E0,$E0,$20,$E0,$E0,$E0,$20,$20,$20,$20,$E0,$A0,$20
pl1b  byte  $20,$20,$E0,$20,$E0,$E0,$20,$20,$E0,$E0,$20,$20,$20,$A0,$A0,$A0,$20,$20,$E0,$E0,$20,$E0,$E0,$20,$20,$E0,$A0,$20,$20,$20,$E0,$20,$E0,$E0,$20,$20,$A0,$A0,$A0,$20
pl1c  byte  $20,$E0,$E0,$20,$E0,$E0,$20,$E0,$E0,$20,$20,$20,$E0,$A0,$20,$E0,$A0,$20,$E0,$E0,$20,$E0,$E0,$20,$E0,$E0,$20,$20,$20,$E0,$E0,$20,$E0,$E0,$20,$20,$20,$A0,$A0,$20
pl1d  byte  $20,$E0,$E0,$E0,$E0,$20,$20,$E0,$E0,$20,$20,$20,$E0,$E0,$A0,$E0,$A0,$20,$20,$E0,$E0,$E0,$20,$20,$E0,$E0,$E0,$20,$20,$E0,$E0,$E0,$20,$20,$20,$20,$20,$E0,$A0,$20
pl1e  byte  $E0,$E0,$20,$20,$20,$20,$E0,$E0,$20,$20,$20,$E0,$E0,$20,$A0,$E0,$20,$20,$E0,$E0,$20,$20,$20,$E0,$E0,$20,$20,$20,$E0,$E0,$20,$E0,$E0,$20,$20,$20,$20,$A0,$A0,$20
pl1f  byte  $E0,$E0,$20,$20,$20,$20,$E0,$E0,$E0,$E0,$20,$E0,$E0,$20,$E0,$E0,$20,$20,$E0,$E0,$20,$20,$20,$E0,$E0,$E0,$E0,$20,$E0,$E0,$20,$20,$E0,$E0,$20,$20,$E0,$E0,$E0,$A0
pl1g  text  '**** congratulations - you triumph *****'
pl1h  text  '        hold fire to play again        '


pl2a  byte  $20,$20,$E0,$E0,$E0,$20,$20,$20,$E0,$E0,$20,$20,$20,$20,$E0,$E0,$20,$20,$E0,$E0,$20,$20,$E0,$E0,$20,$E0,$E0,$E0,$E0,$20,$E0,$E0,$E0,$20,$20,$20,$E0,$E0,$E0,$20
pl2b  byte  $20,$20,$E0,$20,$E0,$E0,$20,$20,$E0,$E0,$20,$20,$20,$A0,$A0,$A0,$20,$20,$E0,$E0,$20,$E0,$E0,$20,$20,$E0,$A0,$20,$20,$20,$E0,$20,$E0,$E0,$20,$E0,$E0,$20,$E0,$E0
pl2c  byte  $20,$E0,$E0,$20,$E0,$E0,$20,$E0,$E0,$20,$20,$20,$E0,$A0,$20,$E0,$A0,$20,$E0,$E0,$20,$E0,$E0,$20,$E0,$E0,$20,$20,$20,$E0,$E0,$20,$E0,$E0,$20,$20,$20,$20,$E0,$E0
pl2d  byte  $20,$E0,$E0,$E0,$E0,$20,$20,$E0,$E0,$20,$20,$20,$E0,$E0,$A0,$E0,$A0,$20,$20,$E0,$E0,$E0,$20,$20,$E0,$E0,$E0,$20,$20,$E0,$E0,$E0,$20,$20,$20,$20,$E0,$E0,$E0,$20
pl2e  byte  $E0,$E0,$20,$20,$20,$20,$E0,$E0,$20,$20,$20,$E0,$E0,$20,$A0,$E0,$20,$20,$E0,$E0,$20,$20,$20,$E0,$E0,$20,$20,$20,$E0,$E0,$20,$E0,$E0,$20,$20,$E0,$E0,$20,$20,$20
pl2f  byte  $E0,$E0,$20,$20,$20,$20,$E0,$E0,$E0,$E0,$20,$E0,$E0,$20,$E0,$E0,$20,$20,$E0,$E0,$20,$20,$20,$E0,$E0,$E0,$E0,$20,$E0,$E0,$20,$20,$E0,$E0,$20,$E0,$E0,$E0,$E0,$E0

;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
