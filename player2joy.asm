;--------------------------------------------------------------------------------------
; CHECK JOYSTICK PORT II = PLAYER 2
;--------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------
; Freudenstab2

joy2last    byte  $00
trig2_fire  byte  $00
stick2_l    byte  $00
stick2_r    byte  $00
stick2_d    byte  $00
stick2_u    byte  $00
player2x    byte  $00
player2y    byte  $00

;--------------------------------------------------------------------------------------

joycontrol2 lda   #$00
            sta   stick2_l
            sta   stick2_r
            sta   stick2_u
            sta   stick2_d
            sta   trig2_fire

            jsr   do_chkj2

            lda   stick2_r     
            beq   st2r
            jsr   orb2_rotr        
            jsr   do_pl2_rite

st2r        lda   stick2_l
            beq   st2d
            jsr   orb2_rotl
            jsr   do_pl2_left

st2d        lda   stick2_u
            beq   st2u
            jsr   do_pl2_up

st2u        lda   stick2_d
            beq   st2nomov
            jsr   do_pl2_down

st2nomov    lda   player2x
            sta   sprx+5      ;ship x
            sta   sprx+4      ;cannon x
            lda   player2y
            sta   spry+5      ;ship y
            sta   spry+4      ;cannon y
            rts

            ;-----------------------------------

do_chkj2    lda   $dc00           ;check joystick II
            sta   joy2last
            tax

            and   #%00010000      ;joy2 fire
            bne   nojf2  
            inc   trig2_fire

nojf2       txa 
            and   #%00000100      ;joy2 left
            bne   nojl2
            inc   stick2_l  

nojl2       txa
            and   #%00001000      ;joy2 right
            bne   nojr2
            inc   stick2_r  

nojr2       txa
            and   #%00000010      ;joy2 down
            bne   nojd2
            inc   stick2_d 

nojd2       txa
            and   #%00000001      ;joy2 up
            bne   chkj2_exit
            inc   stick2_u    

chkj2_exit  rts

            ;---------------------------------------------
            ; Player 1 Movement Mechanics

do_pl2_left lda   players8    ;check if xext bit is set     
            and   #%00100000  ;player xext is bit 2
            bne   p2x_do_dec  ;it is set! just decrease!
            lda   player2x    ;it is not set! check if we hit left border!
            tay               ;not set, first store value
            sec
            sbc   min_x       ;limit reached?
            bcs   p2x_do_dec  ;no, decrease
            lda   player2x    ;limit reached, hold position
            bcc   p2x_dundec   ;and exit

p2x_do_dec  lda   player2x    ;load value (first time here)
            sec
            sbc   movespeed   ;if we go to minus, carry will be cleared 
            bcs   p2x_dundec  ;no minus, all good.
            tay               ;backup value
            lda   players8    ;we went minus. xext must be unset   
            and   #%11001111  ;kill xext bit
            sta   players8       
            tya               ;get value back
p2x_dundec  sta   player2x    ;store!
            rts

            ;---------------------------------------------------------

do_pl2_rite lda   players8    ;check if xext bit is set     
            and   #%00110000  ;player xext is bit 2
            beq   p2x_do_inc  ;its not set! just increase!
            lda   player2x     
            tax
            sec               
            sbc   max_x       
            bcc   p2x_do_inc

                  lda   intromode
                  beq   p2x_halt
                  jmp   p2x_wrap

p2x_halt    txa
            bcs   p2x_done_inc ;maximum reached= no action

p2x_do_inc  lda   player2x
            tax
            clc
            adc   movespeed    ;if we go to minus-wrap, carry will be set 
            bcc   p2x_done_inc ;no wrap, all good.
            tax                ;oh pardon, sind sie der fuerst von cleverstein      
            lda   players8     
            ora   #%00110000  ;set xext bit
            sta   players8            
            txa
p2x_done_inc sta   player2x
            rts

            ;--------------------------------------------------------------

do_pl2_down lda   player2y     
            tax
            sec               
            sbc   max_y              
            bcc   p2y_do_inc   
            txa
            bcs   p2y_done_inc
            
p2y_do_inc  lda   player2y
            clc
            adc   movespeed   
p2y_done_inc sta   player2y     
            rts   

            ;---------------------------------------------------------------

do_pl2_up   lda   player2y
            tax
            sec
            sbc   min_y       
            bcs   p2y_do_dec    
            txa    
            bcc   p2y_done_dec

p2y_do_dec  txa     
            sec
            sbc   movespeed   
p2y_done_dec sta   player2y
            rts   

;--------------------------------------------------------------------------------------

p2x_wrap    lda   i_min_x
            sta   player2x

            lda   players8    ;we went minus. xext must be unset   
            and   #%11001111  ;kill xext bit
            sta   players8          

            rts

;--------------------------------------------------------------------------------------

orb2_rotr   lda   #$01
            sta   rotdir2
            rts

;--------------------------------------------------------------------------------------

orb2_rotl   lda   #$00
            sta   rotdir2
            rts

;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
