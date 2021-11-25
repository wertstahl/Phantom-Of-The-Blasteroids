;-----------------------------------------------------------------------
; collision shot 1 with player 2 check
;-----------------------------------------------------------------------

collisionx1 byte  0
collisionx2 byte  0
collisiony1 byte  0
collisiony2 byte  0
stor1       byte  0

xsize1      = #$11 ;The area of the drawn sprite on the left
xsize2      = #$11 ;The area of the drawn sprite on the right
ysize1      = #$11 ;The area of the drawn sprite at the top
ysize2      = #$11 ;The area of the drawn sprite at the bottom



armed1      byte  0

collcheck1  lda   armed1
            beq   nothit1
            
checkimp1   lda   coll_reg
            tay
            and   #%00001000
            beq   nothit1

            tya
            and   #%10001000
            cmp   #%10001000
            beq   nothit1

hit1        jsr   removeshot1
            inc   shaketheroom

            lda   intromode
            bne   nothit1

            jsr   shm2_hit
            jsr   sfx_smash
            dec   p2energy
            bne   nothit1

death       sei            
            jmp   gameover2

nothit1     rts

;---------------------------------------------