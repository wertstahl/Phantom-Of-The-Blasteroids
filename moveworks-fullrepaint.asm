;==================================================================================================================================================


*=$7000

starcolor   byte $01    ;color of items
starshape   byte $01    ;shape of items

;--------------------------------------------------------------------------

            ;required startups

mw_init     jsr maketab             ; adresstabellen anlegen
            jsr createyvectors      ; vektortabelleadressen anlegen
            rts

;--------------------------------------------------------------------------

            ; usage (idle loop recommended)

            ;---------- announce new pixel --------------------

;            ldx #$13                ; screenpos x (40 positionen #$0-#$28) in x
;            ldy #$0e                ; screenpos y (28 positionen #$0-#$1c) in y
;            lda #$01                ; vector in accu
;            jsr mw_push             ; translates from xy in adresscoordinate & writes a to currentvec 
                                     ; pushes new pixel to stack
                                     ; if stack reaches max size, oldest pixel will die
                                     ; target coordinate bg will be rescued
                                     ; pixel will be painted

            ;---------- move stuff around ----------------------


;            jsr restore_bg          ; first remove all pixels and restore background
;            jsr mw_scan             ; then process movement of all pixels
;            jsr copy_bg             ; now rescue background for all pixels
;            jsr paint_all           ; and paint the whole stack to the screen
;            rts


;--------------------------------------------------------------------------------------
;----------------------- stackscan ----------------------------------------------------

mw_scan     ldx #$00                ;stackpointer
            stx pointer             ;zurücksetzen

checkstack  ldx pointer
            cpx stacksize          ;stack 0 bytes gross?
            bne contstack
safetyend   ldx #$00
            rts                     ;wenn ja, abbruch, kein pixel auf screen

contstack   ldx pointer
            lda matrxval,x          ;vektorinhalt holen
            sta currentvec     
            lda coordx,x
            sta currentx
            lda coordy,x
            cmp #$ff
            beq safetyend
            sta currenty

;-------------------------------------------------------------------------------------
;-------------------------- bewegung -------------------------------------------------           

           ;------------- y berechnen ------------------------

process     ldx currentvec          ;aktuellen winkelwert
            lda translatey,x        ;in unser winkelmass uebersetzen
                                    ;360 grad auf 100 (4x25 -> kreisteile) und dann auf 250 von 256 schritten verteilt
            tax 
            lda vectslo,x           ;dies verweist uns auf die entsprechende
            sta usage1+1           ;steigungssequenz-zeile
            lda vectshi,x           ;in der vektortabelle
            sta usage1+2

           ; --------------------------------- y berechnen ------------------------------

            ldy currentx            ;alten X WERT (!) laden fuer steigungsoffset um abweichungen zu bekommen
            ldx currenty            ;alten Y WERT (!) in x zum berechnen laden

usage1      lda $1234,y       ;steigungswert holen (0 oder 1) aus 24 werten via y aus $40/2
            beq noplus              ;keine steigung? dann y nicht veraendern

            ldy currentvec          ;winkel laden
            lda commandsy,y         ;entsprechendes kommando aus tabelle laden
            sta selfmody            ;und nachfolgend reinschreiben
selfmody    nop                     ;wird mit dex inx oder nop aus tabelle ersetzt

noplus      stx newy                ;und ziel - y schreiben
            lda stopy,x             ;stopbittabelle lesen                
            beq noystop             ;rand noch nicht erreicht?
            jsr pull                ;doch, abbruch
            jmp EOS

            ; --------------------------------- x berechnen ------------------------------

noystop     ldx currentvec          ;winkel laden
            lda translatex,x        ;Uebersetzung von 360(Grad)->256(Positionen)->25(Steigungszeilen)
            tax
            lda vectslo,x           ;dies verweist uns auf die entsprechende
            sta usage2+1            ;steigungssequenz-zeile
            lda vectshi,x           ;in der vektortabelle
            sta usage2+2

            ldy currenty            ;y für x als steigungstabellen-offset laden
            ldx currentx            ;x alt laden für berechnung

usage2      lda $1234,y             ;steigungswert holen (0 oder 1) aus 24 werten via y aus $40/2 aber tabelle von hinten
            beq noplus2             ;keine steigung? dann x nicht veraendern

            ldy currentvec          ;winkel laden
            lda commandsx,y         ;entsprechendes kommando aus tabelle laden
            sta selfmodx            ;und nachfolgend reinschreiben
selfmodx    nop                     ;wird mit DEX, INX oder nop aus tabelle ersetzt

noplus2     stx newx                ;ziel - x schreiben
            lda stopx,x             ;zeilenende erreicht?
            beq noxstop             ;rand noch nicht erreicht?
            jsr pull                ;doch, abbruch
            jmp EOS

           ;------------------------ alte durch neue werte ersetzen  ------------------------

noxstop     ldx pointer

            lda newx
            sta coordx,x

            lda newy
            sta coordy,x

            lda currentvec 
            sta matrxval,x

            ;----------------------------------------------------------------------------------------------------------------------------
            ;----------------------------------------------------------------------------------------------------------------------------

            inc pointer
EOS         jmp checkstack      ;############ END of stackscan

;----------------------------------------------------------------------------------------------------------------------------
;-----------------------------sprungmodule mit rts --------------------------------------------------------------------------

;------------------ koordinaten auf den stack schieben, pixel anmelden ------------------------

            ; x contains numeric x
            ; y contains numeric y
            ; a contains vector (direction) (#$01-#$ff, split to 360 degres. #$00 = off)

mw_push     stx currentx
            sty currenty            
            sta currentvec          ;store angle in currentvec
            
            ldx stacksize

pushit      lda matrxval,x          ;copy loop to the right (insert)
            sta matrxval+1,x
            lda coordy,x
            sta coordy+1,x
            lda coordx,x
            sta coordx+1,x
            lda whatwasthere,x
            sta whatwasthere+1,x
            lda whichcolorwasit,x
            sta whichcolorwasit+1,x

            dex                     ;descend from max
            cpx #$ff                ;number of values
            bne pushit

            ldx stacksize
            cpx maxlivepix          ;max amount of pixels reached?
            beq sizelimit           ;indeed - so drop the last one
                                    ;by not increasing stack size
            inc stacksize           ;else increase stack size

sizelimit   lda currentvec
            sta matrxval            ;update cell contents stack
            lda currentx            
            sta coordx              ;update numeric x stack
            lda currenty
            sta coordy              ;update numeric y stack

            ;------- rescue the background for the last added pixel --------

rescueone   ldy coordy              ;load y line start
            lda wherehi,y
            sta usage3+2            ;this is the interim hi addr      
            lda wherelo,y           ;lowbyte from y
            sta usage3+1            ;this is the final low addr

            sta usage4+1
            ldy coordx              ;load x-offset
usage3      lda $1234,y             ;rescue
            sta whatwasthere        ;background gfx

            lda usage3+2               
            clc
            adc offsetfromscreen    ;climb to color rom
            sta usage4+2
usage4      lda $1234,y             ;and rescue
            and #$0f
            sta whichcolorwasit     ;original color
            rts
;--------------------- pull stack, destroy pixel ----------------------------------

error       inc $d020
            jmp error

pull        ldx stacksize
            cpx #$00
            beq error

            ldx pointer             ; stackpointer laden
            cpx stacksize           ; ende des stacks erreicht?
            beq pullready           ; yup, dort killen und raus

pullit      lda matrxval+1,x        ; werte umkopieren
            sta matrxval,x
            lda coordx+1,x
            sta coordx,x
            lda coordy+1,x
            sta coordy,x
            lda whatwasthere+1,x
            sta whatwasthere,x
            lda whichcolorwasit+1,x
            sta whichcolorwasit,x

            inx
            cpx stacksize           ; stack ende erreicht?
            bne pullit

pullready   ldx stacksize
            lda #$ff                ; stack ende                        -
            sta coordx,x            ; opt kontrolle, kann später raus   -
            cpx #$00                ; stackgroesse war schon 0?
            beq stfinished          ; ja, raus
            dec stacksize           ; nein, um 1 verkleinern

stfinished  rts

;--------------------------------------------------------------------------------------
;-------------- new method: calculation and painting are separated --------------------

            ;--------------------------------------------------------
            ; erase all pixels and restore background

restore_bg  ldx #$00
getorig     lda coordx,x            ;end of values?
            cmp #$ff
            bne or_do               ;no, go on
            rts                     ;yes, goodbye
or_do       ldy coordy,x            ;load y line start
            lda wherehi,y
            sta ko_hi               ;this is the interim hi addr      
            lda wherelo,y           ;lowbyte from y
            sta ko_low              ;this is the final low addr
            ldy coordx,x            ;x-offset laden
            lda whatwasthere,x      ;restore 
            sta (ko_low),y          ;original graphics
            lda ko_hi               
            clc
            adc offsetfromscreen    ;climb to color rom
            sta ko_hi
            lda whichcolorwasit,x   ;restore
            sta (ko_low),y          ;original colour            
            inx
            jmp getorig


;------------------------------------------------------------------------------------------------------------

            ;------------------------------------------------------
            ; alle pixel zeichnen. dazu zuerst hintergrund retten

copy_bg     ldx #$00
getrepaint  lda coordx,x
            cmp #$ff
            bne copy_on
            rts
copy_on     ldy coordy,x            ;load y line start
            lda wherehi,y
            sta ko_hi               ;this is the interim hi addr      
            lda wherelo,y           ;lowbyte from y
            sta ko_low              ;this is the final low addr
            ldy coordx,x            ;load x-offset
            lda (ko_low),y          ;rescue the  
            sta whatwasthere,x      ;grafix
            lda ko_hi               
            clc
            adc offsetfromscreen    ;climb to color rom
            sta ko_hi
            lda (ko_low),y          ;rescue the
            and #$0f                ;(destroy bus noise)
            sta whichcolorwasit,x   ;color
            inx
            jmp getrepaint

            ;-- und dann alle pixel zeichnen ---------------------

paint_all   ldx #$00
do_paint    lda coordx,x            ;end of values?
            cmp #$ff
            bne paint_on            ;no, paint on
            rts                     ;yes, goodbye.
paint_on    ldy coordy,x            ;load y line start
            lda wherehi,y
            sta ko_hi               ;this is the interim hi addr      
            lda wherelo,y           ;lowbyte from y
            sta ko_low              ;this is the final low addr
            ldy coordx,x            ;load x-offset
            lda starshape           ;shape (char)
            sta (ko_low),y          ;paint it
            lda ko_hi               
            clc
            adc offsetfromscreen    ;climb to color rom
            sta ko_hi
            lda starcolor           ;color
            sta (ko_low),y          ;paint it
            inx
            jmp do_paint


;----------------------------------------------------------------------------------------------------------------------
;----------------------------- init routinen --------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------

            ;--------- matrix adresstabelle herstellen

maketab     lda #$00
            ldy screenbaseB         ;screen base adress hi
            ldx #$00                ;screen base adress lo
            sta wherelo
            sty wherehi
            inx

makmor      clc
            adc #$28                ;+28 = next line
            sta wherelo,x
            bcc ynextl              ;no address-break, continue
            iny
ynextl      pha                     ;akku retten
            tya
            sta wherehi,x
            pla                     ;akku rueckholen
            inx
            cpx #$20                ;max lines reached?
            bne makmor
            rts

            ;-----------vektortabelle adresstabelle herstellen

createyvectors
            ldx #$00
            lda #<vects
            sta vectslo
            ldy #>vects
            sty vectshi
            inx

makmor3     clc
            adc #$28
            sta vectslo,x
            bcc yster3
            iny
yster3      sta abak                ;akku retten
            tya
            sta vectshi,x
            lda abak                ;akku rueckholen
            inx
            cpx #$1a
            bne makmor3
            rts

abak        byte 0

;-------------------------------------------------------------------------------------------------------------------
;-----------------------------variablen ----------------------------------------------------------------------------

stacksize       byte $00            ;groesse des stack
pointer         byte $00            ;stackpointer

currenthi       byte $00            ;aktuelle adresse hi byte
currentlo       byte $00            ;aktuelle adresse lo byte
currentvec      byte $00            ;aktueller zelleninhalt/vektor

currenty        byte $00
currentx        byte $00

curwaspix       byte $00
curwascol       byte $00

newx            byte $00
newy            byte $00
newvec          byte $00


;==================================================================================================================================================
;==================================================================================================================================================
;==================================================================================================================================================
;==================================================================================================================================================
;-------------------------------------------------------------------------------------------------------------------
;----------------------------------- tabellen ----------------------------------------------------------------------


*=$7300
            ;xy koordinaten x=0 bis $27 y=0 bis $19
coordx      byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
coordy      byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

            ;vektorwerte jeder zelle, 360 grad verteilt auf 256 werte
matrxval    byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

            ;backup table mit dem pixel das auf dem screen war, bevor wir ein pixel hinschruben
whatwasthere byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

            ;backup table mit der farbe die auf dem screen war, bevor wir ein pixel hinschruben
whichcolorwasit byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
*=$7800

            ; adresstabelle fuer bewegungsvektorzeilen ( also grad )
vectslo     byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
vectshi     byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

*=$7900
            ;Steigungstabelle (0 = keine steigung, 1 = steigung), aus mehreren richtungen ausgelesen
vects       byte    $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte    $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00
            byte    $01,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00
            byte    $01,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$01,$00,$00,$00
            byte    $01,$00,$00,$00,$01,$00,$00,$00,$00,$01,$00,$00,$00,$00,$01,$00,$00,$00,$00,$01,$00,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$00,$01,$00,$00,$00,$00,$01,$00
            byte    $01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00
            byte    $01,$00,$00,$01,$00,$00,$01,$00,$00,$00,$01,$00,$00,$01,$00,$00,$00,$01,$00,$00,$01,$00,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$00,$01,$00,$00,$01,$00,$00
            byte    $01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$01
            byte    $01,$00,$01,$00,$00,$01,$00,$00,$01,$00,$01,$00,$00,$01,$00,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$00,$01,$00,$00,$01,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00
            byte    $01,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00
            byte    $01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$00,$01,$00,$01
            byte    $01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00
            byte    $01,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$01,$00,$01,$00
            byte    $01,$01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$01,$00,$01,$00,$01,$00,$01,$01,$00,$01
            byte    $01,$01,$00,$01,$01,$00,$01,$00,$01,$01,$00,$01,$01,$00,$01,$00,$01,$01,$00,$01,$01,$00,$01,$00,$01,$01,$00,$01,$01,$00,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01
            byte    $01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01
            byte    $01,$01,$01,$00,$01,$01,$00,$01,$01,$01,$00,$01,$01,$00,$01,$01,$01,$00,$01,$01,$00,$01,$01,$00,$01,$01,$01,$00,$01,$01,$00,$01,$01,$01,$00,$01,$01,$00,$01,$01
            byte    $01,$01,$01,$00,$01,$01,$01,$00,$01,$01,$01,$01,$00,$01,$01,$01,$00,$01,$01,$00,$01,$01,$01,$00,$01,$01,$01,$00,$01,$01,$01,$00,$01,$01,$01,$00,$01,$01,$01,$01
            byte    $01,$01,$01,$01,$00,$01,$01,$01,$01,$00,$01,$01,$01,$01,$00,$01,$01,$01,$01,$00,$01,$01,$01,$00,$01,$01,$01,$01,$00,$01,$01,$01,$01,$00,$01,$01,$01,$01,$00,$01
            byte    $01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01
            byte    $01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$00
            byte    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01
            byte    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
            byte    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01

*=$7d00


            ;Uebersetzung von 360(Grad)->256(Positionen)->25(Steigungszeilen)
translatey  byte    $00,$00,$01,$02,$03,$03,$04,$05,$06,$07,$07,$08,$09,$0a,$0b,$0c
            byte    $0d,$0e,$0e,$0f,$10,$11,$11,$12,$13,$14,$15,$15,$16,$17,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
translatex  byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$17,$16,$15,$15,$14,$13,$12,$11,$11,$10,$0f,$0e,$0e,$0d
            byte    $0c,$0b,$0a,$0a,$09,$08,$07,$07,$06,$05,$04,$03,$03,$02,$01,$00
            byte    $00,$01,$01,$02,$03,$03,$04,$05,$06,$07,$07,$08,$09,$0a,$0b,$0c
            byte    $0d,$0e,$0e,$0f,$10,$11,$11,$12,$13,$14,$15,$15,$16,$17,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$17,$16,$15,$15,$14,$13,$12,$11,$11,$10,$0f,$0e,$0e,$0d
            byte    $0c,$0b,$0a,$0a,$09,$08,$07,$07,$06,$05,$04,$03,$03,$02,$01,$00
            byte    $00,$01,$01,$02,$03,$03,$04,$05,$06,$07,$07,$08,$09,$0a,$0b,$0c
            byte    $0d,$0e,$0e,$0f,$10,$11,$11,$12,$13,$14,$15,$15,$16,$17,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
            byte    $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18


            ;Stoptabelle für Displayrand
stopx       byte    $1,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
stopy       byte    $0
            byte    $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
            byte    $0,$0,$0,$0,$0,$0,$0,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
            byte    $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1


            ; $ca = dex  $e8 = inx selfmodifying code table
commandsy   byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8

commandsx   byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8

            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca
            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca
            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca
            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca

            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca
            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca
            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca
            byte    $ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca,$ca

            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            byte    $e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8,$e8
            
            byte    $ea

*=$8100
            ;adresstabelle zeilenanfaenge pixelmatrix
wherelo     byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
            byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
            byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
            byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
            byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
            byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
            byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
            byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44

wherehi     byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
            byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
            byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
            byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
            byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
            byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
            byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
            byte    $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44

;==================================================================================================================================================
;==================================================================================================================================================
;==================================================================================================================================================