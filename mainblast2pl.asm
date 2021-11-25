;------ MAIN ------------------------------------------------------------- CBMprgStudio
;
;       (c)wertstahl 02/2018 #it is a beautiful day, and then you die.
;
;       Phantom of the Blasteroids 2Player
;
;------ BASIC - bootloader ------------------------------------------------------------

*=$0801
            byte  $0c,$08,$01,$00,$9e,$20,$32,$30,$36,$32,$00,$00,$00; 0 sys2062 ($080e)

;--------------------------------------------------------------------------------------

*=$080e
            sei

            jsr   firstrun

            jsr   genfont

            lda   intro 
            beq   nointro

            jmp   i_intro
nointro     jmp   appstart

;--------------------------------------------------------------------------------------

intro       =     #$01
scanline1   =     #$fe
border_col  =     #$00
screen_col  =     #$00
cls_col     =     #$00
clschar     =     #$20
clscol      =     #$01
screen_01   =     $0400
cram_01     =     $d800

multi0_col  =     #$07
multi1_col  =     #$0d

;--------------------------------------------------------------------------------------

appstart    sei
            cld

            lda   #$01
            sta   $d01a
            sta   $dc0d

            lda   #$35              ;gibe all di ram!
            sta   $01

            lda   #$05
ast_vwait   cmp   $d012
            bne   ast_vwait

            lda   #$00
            sta   $d011       
            lda   #$00
            sta   $d020       
            sta   $d021

            jsr   gfxtools          ;--> gfxtools

            lda   #$01        
            jsr   gamemuz

            jsr   initcolfade

deeleven    lda   #$1b
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

            lda   #$00
            sta   intromode
            lda   g_min_x
            sta   min_x
            lda   g_max_x
            sta   max_x

            lda   #$01
            sta   dfwait      ;first time, fight must be displayed directly
            sta   dfghtdo 

            ldx     #$07
            lda     #$ff
insertful   sta     $3ff8,x
            dex
            bpl     insertful

            lda   #$10
            sta   shotrspeed

            lda   i_gamestyle
            bne   style2

            lda   #$08
            sta   p1energy
            sta   p2energy
            jmp   style1

style2      lda   #$01
            sta   p1energy
            sta   p2energy

style1


            cli
            
idle        jsr   colorfade

            jsr   disp_fight

            jmp   idle        

;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------

mainloop    pha
            txa
            pha
            tya
            pha
            inc   $d019

           ; lda   #$01
           ; sta   $d020
           ; sta   $d021

            ;-----------------------------------------------------------------------------
            ;-----------------------------------------------------------------------------
            ;-----------------------------------------------------------------------------

            jsr   joycontrol1 ;check joystick port 1 + update coordinates
            jsr   spr1anim    ;animate ship
            jsr   orb1_anim   ;animate targeting orb
            jsr   spr_update  ;update sprite positions

            ;--------------------

            jsr   joycontrol2 ;check joystick port 2 + update coordinates
            jsr   spr2anim    ;animate ship
            jsr   orb2_anim   ;animate targeting orb
            jsr   spr_update  ;update sprite positions

            ;--------------------

            jsr   shoz1fired
            jsr   shoz2fired

            jsr   shipmarker1
            jsr   shipmarker2

            ;--------------------

            jsr   plotstat1
            jsr   plotstat2

            ;--------------------

            lda   $d01e
            sta   coll_reg

            jsr   collcheck1
            jsr   collcheck2

            lda   #$00
            sta   $d01e

            jsr   roomboom

            ;--------------------

            jsr   gamemuz+3

            ;-----------------------------------------------------------------------------
            ;-----------------------------------------------------------------------------
            ;-----------------------------------------------------------------------------

            lda   screen_col
            sta   $d021
            lda   border_col
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


coll_reg    byte  0

;--------------------------------------------------------------------------------------

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

            lda   players8
            sta   $d010

            rts

;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------

;----------- graphics tools

gfxtools    jsr   dk_cls

            jsr   scrcol

            jsr   setfont

            jsr   initsprites

            jsr   removeshot1

            jsr   removeshot2

            rts

;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------

shaketheroom      byte 0
shakecount        byte 0
shakevalz         byte $01,$06,$03,$07,$01,$04,$02,$06,$07,$05,$00,$03,$04,$06,$01,$00,$05,$06,$02,$05,$01,$07,$02,$04,$03,$ff

noboom      rts
roomboom    lda   shaketheroom
            beq   noboom
            ldx   shakecount
            lda   shakevalz,x
            cmp   #$ff
            beq   boomreset

            inc   shakecount

            lda   $d016
            and   #$f8
            ora   shakevalz,x
            sta   $d016
            lda   $d011
            and   #$f8
            ora   shakevalz,x
            sta   $d011            

            lda   intromode
            bne   skipmusical
            jsr   muzak+3
skipmusical

            rts

boomreset   lda   #$00
            sta   shakecount
            sta   shaketheroom
            lda   desixteen+1
            sta   $d016
            lda   $d011
            and   #%10000000
            ora   #%00011011
            sta   $d011
            rts
      
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------

dfghtimes   byte  $0a
dfghtdo     byte  $0
dfwait      byte  $01   ;will be set as 1 at first launch, then 80 for repeats


disp_fight  lda   dfghtdo
            bne   dodfght
dfreturn    rts

dodfght     dec   dfwait
            bne   dfreturn
            lda   #$08
            sta   dfwait

            dec   dfghtimes
            bne   dfdisplay

            ; else -> done:

dfdisable   lda   #$10  ;reset for next call
            sta   dfghtimes
            lda   #$00  ;disable until game restart
            sta   dfghtdo


            ldx     #$07
insertbg    lda     bginsert,x
            sta     $3ff8,x
            dex
            bpl     insertbg

            rts

dfdisplay   ldx   #$21
dspfghtlp   
            lda   msg_fight1,x
            sta   $d91b,x

            lda   msg_fight2,x
            sta   $d943,x

            lda   msg_fight3,x
            sta   $d96b,x

            lda   msg_fight4,x
            sta   $d993,x

            lda   msg_fight5,x
            sta   $d9bb,x

            lda   msg_fight6,x
            sta   $d9e3,x

            lda   msg_fight7,x
            sta   $da0b,x

            dex
            bpl   dspfghtlp

            rts

            

;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------

sprxini     byte  $40,$40,$40,$00, $30,$30,$30,$00
spryini     byte  $40,$40,$40,$00, $c0,$c0,$c0,$00
sprcolini   byte  $0a,$07,$0a,$0a, $05,$0d,$05,$08

; cols
; 00 blk  01 wht  02 red  03 cyn  04 pur  05 grn  06 blu  07 yel 
; 08 lbr  09 brn  0A pnk  0B dgr  0C mgr  0D lgn  0E lbl  0F lgr

     ;slots    ;   0   1   2   3    4   5   6   7
sprx        byte  $40,$40,$40,$00, $30,$30,$30,$00
spry        byte  $40,$40,$40,$00, $c0,$c0,$c0,$00
sprcol      byte  $0a,$07,$0a,$0a, $05,$0d,$05,$08
initbank    byte  $a8,$a0,$c8,$d0, $a8,$a0,$c8,$d0

sprmulcol0   =     #$0c ;light color
sprmulcol1   =     #$0b ;dark color

p1energy    byte  $8
p2energy    byte  $8

;sprite1slots:
; 00 = turret1 ($a8-c7) d000,d001
; 01 = ship1   ($a0-a7) d002,d003
; 02 = orb1    ($c8-cf) d004,d005
; 03 = shot1   ($d0)    d006,d007

;sprite2slots:
; 04 = turret2 ($a8-c7) d008,d009
; 05 = ship2   ($a0-a7) d00a,d00b
; 06 = orb2    ($c8-cf) d00c,d00d
; 07 = shot2   ($d0)    d00e,d00f


;--------------------------------------------------------------------------------------
; General Player related values
;--------------------------------------------------------------------------------------

intromode   byte  $0

min_x       byte  $1b   ; objects min x position
max_x       byte  $3e   ; objects maximum position respecting x-pos hi bit (=+$ff)
min_y       byte  $36   ; objects min y position
max_y       byte  $e3   ; objects max y position


g_min_x     byte  $1b   ; objects min x position
g_max_x     byte  $3e   ; objects maximum position respecting x-pos hi bit (=+$ff)

i_min_x     byte  $08   ; objects min x position
i_max_x     byte  $50   ; objects maximum position respecting x-pos hi bit (=+$ff)

movespeed   byte  $02

primerval   = #$0c

players8    byte  $00   

            ;      -p2--p1-
players8g   byte  %11110000   ; players combined x-ext bit for $d010 (x hi bit of sprites)
            ;slot  76543210

;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------

incasm      "turretorb.asm"

;--------------------------------------------------------------------------------------

incasm      "turretorb2.asm"

;--------------------------------------------------------------------------------------

incasm      "shots.asm"

;--------------------------------------------------------------------------------------

incasm      "shots2.asm"

;--------------------------------------------------------------------------------------

incasm      "player1joy.asm"

;--------------------------------------------------------------------------------------

incasm      "player2joy.asm"

;--------------------------------------------------------------------------------------

incasm      "status1.asm"

;--------------------------------------------------------------------------------------

incasm      "status2.asm"

;--------------------------------------------------------------------------------------

incasm      "collision1to2.asm"

;--------------------------------------------------------------------------------------

incasm      "collision2to1.asm"

;--------------------------------------------------------------------------------------

incasm      "gameover.asm"

;--------------------------------------------------------------------------------------

incasm      "colorfading.asm"

;--------------------------------------------------------------------------------------

incasm      "sfx.asm"

;--------------------------------------------------------------------------------------

incasm      "toolsandtables.asm"

;--------------------------------------------------------------------------------------

incasm      "firstrun.asm"

;--------------------------------------------------------------------------------------
*=$27c0     ;emptysprite 9f
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

*=$2800
sprites     ;first sprite a0
incbin      "/res/blasteroids_2_spr.prg",2

;--------------------------------------------------------------------------------------

*=$3800 ; to $3fff
font       
incbin      "goplay_bmp.bin"

;--------------------------------------------------------------------------------------

;*=$4000
incasm      "intro.asm"

;--------------------------------------------------------------------------------------

*=$8800     
goplaymap
incbin      "goplay_map.bin"


;--------------------------------------------------------------------------------------

*=$9000
gamemuz
incbin      "glitchbeatz9000.sid",$7e

;--------------------------------------------------------------------------------------
; EOF
