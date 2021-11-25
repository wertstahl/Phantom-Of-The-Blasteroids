

firstrun    lda   #$00
            sta   $d011
            jsr   cls
            jsr   initcolfade

            ldx   #$00
copygpmp    lda   goplaymap,x
            sta   $0400,x
            lda   goplaymap+256,x
            sta   $0500,x
            lda   goplaymap+512,x
            sta   $0600,x
            lda   goplaymap+768,x
            sta   $0700,x
            inx
            bne   copygpmp

            jsr   setfont

            lda   #$ff
fvwait1     cmp   $d012
            bne   fvwait1

            lda   #$01
            sta   $d020
            sta   $d021

            lda   #$1b
            sta   $d011

frcnt       lda   #$1d
            cmp   #$00
            beq   endfirstrun

ffwait      lda   #$e0
frvwait     cmp   $d012
            bne   frvwait

hzcount     lda   #$19
            dec   hzcount+1
            bne   ffwait

            lda   #$19
            sta   hzcount+1

            jsr   colorfade

            dec   frcnt+1
            jmp   frcnt

endfirstrun lda   #$ff
fvwait3     cmp   $d012
            bne   fvwait3

            lda   #$00
            sta   $d020
            sta   $d021

            lda   #$00
            sta   $d011

            rts