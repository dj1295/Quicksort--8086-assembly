        PAGE 60,80
        TITLE quick_omerktn
        
STACKSG SEGMENT PARA STACK 'STACK'
        DW 64 DUP(?)
STACKSG ENDS

DATASG  SEGMENT PARA 'DATA'
DIZI    DB 100 dup(0)
ELEMAN  DB 100
MSG2	DB	'Dizinin uzunlugu: ','$'
MSG_neg	DB	'-','$'
MSG_val	DB	'. eleman: ','$'
MSG_err DB	'Hata! Girilen deger -128 ile 127 arasinda olmali.', '$'
MSG4 	DB	'-- Orijinal dizi: --', '$'
MSG5 	DB	'-- Siralandirilmis dizi: --', '$'
MSG_voi	DB	'  ','$'
the_string db 26         ;MAKSIMUM KARAKTER: 25
           db ?          ;GIRILEN KARAKTER UZUNLUGU
           db 26 dup (?) ;GIRILEN KARAKTERLER
DATASG ENDS

CODESG  SEGMENT PARA 'CODE'
        ASSUME CS:CODESG, DS:DATASG, SS:STACKSG

BASLA   PROC FAR
        PUSH DS
        XOR AX,AX
        PUSH AX
        MOV AX, DATASG
        MOV DS, AX
		;----------;
		
		LEA DX,MSG2	 ; ikinci cikti
		MOV AH,9
		INT 21H
		
		CALL read_number ; Dizinin uzunlgunu iste ve BX'e at
		CALL new_line
		PUSH BX ; Eleman sayini stack'e at
		
		XOR CX,CX
		MOV CL,BL ; Döngü sayınını hazırla, eleman sayısı kadar dönecek.
		
		XOR SI,SI ; KULLANICIDAN ELEMANLARI ALAN DONGU
getElement:
		MOV BX,SI
		INC BL
		CALL print	; BL+1 değerini ekrana yazdırır

		LEA DX,MSG_val	; ". eleman:" stringini bas.
		MOV AH,9
		
		INT 21H

		CALL read_number ; oku ve BX'e at
		
		CMP BX,128
		JGE	hata1    ; 128'e eşit veya büyükse hata
		CMP BX,-128
		JL hata1	; -128'den küçükse hata
		JMP hatayok
		hata1:
			LEA DX,MSG_err ; HATA MESAJI
			MOV AH,9
			INT 21H
			DEC SI ; SI degeri bir artmaması için burada azaltalım.
			INC CX ; döngünün bir ileriye gitmemesi için burada arttıralım.
			JMP hataDevam
		hatayok:
		
		MOV dizi[SI],BL ; girilen degeri diziye ekle.
		hataDevam:
		CALL new_line
		INC SI
		LOOP getElement
		
		CALL new_line	; yeni satır
		LEA DX,MSG4	; "Orijinal dizi" stringini yazdır.
		MOV AH,9
		INT 21H
		CALL new_line	; yeni satır
		
		POP CX
		PUSH CX ; Stack'e tekrar at, n sayısı lazım olacak.
		XOR SI,SI
showArray:
		XOR BH,BH
		MOV BL, dizi[SI]
		CALL print	; BL değerini ekrana yazdırır
		
		LEA DX,MSG_voi	; boşluk yazdır.
		MOV AH,9
		INT 21H
		
		INC SI
		LOOP showArray
		;;;;;;;;;;;;;;;;;;; QUICKSORT BAŞLA ;;;;;;;;;;;;;;;;;;;;;;
		CALL new_line
		CALL new_line
		LEA DX,MSG5	; "Sıralanmış dizi" stringini yazdır.
		MOV AH,9
		INT 21H
		
		; Quicksort iki parametre alacak; LEFT:AX, RIGHT:BX
		XOR AX,AX ; left = 0
		POP BX ; right = BX = n
		PUSH BX ; aşağıda kullanılacak (n)
		DEC BX ; right = n-1
		CALL  quicksort
		
		CALL new_line
		
		;Sıralanmış diziyi yazdır
		POP CX  ; cx = n
		XOR SI,SI
showArray2:
		XOR BH,BH
		MOV BL, dizi[SI]
		CALL print	; BL değerini ekrana yazdırır
		
		LEA DX,MSG_voi	; boşluk yazdır.
		MOV AH,9
		INT 21H
		
		INC SI
		LOOP showArray2
		
        RETF
BASLA   ENDP

quicksort	PROC
	; LEFT:AX, RIGHT:BX
PUSH BX
PUSH AX
	CMP AX,BX
	JGE bitti
	CALL arrange_pivot ; DX uzerinden pivot'un yeni yerini dondurur
	
POP AX
	MOV BX,DX ; pivotu aktar
	DEC BX ; right = pivot - 1
	CALL quicksort ; (left:left, right:pivot-1)
POP BX
	MOV AX,DX ; pivotu aktar
	INC AX ; left = pivot + 1
	CALL quicksort ; (left:pivot+1, right:right)
	JMP devamEdiyor
	bitti:
	POP AX
	POP BX
	devamEdiyor:
	RET
quicksort	ENDP

arrange_pivot PROC
	XOR DH,DH
	; DX: pivot, AX: left, BX: right
	MOV DL, dizi[BX] ; DX = dizi[right]
	MOV SI,AX
	DEC SI ; ind = SI = left - 1
	
	MOV CX,BX
	SUB CX,AX ; dongu sayimiz: right-left
	
	MOV DI,AX ; j = left
dongu1:
	PUSH BX
	XOR BH,BH
	MOV BL, dizi[DI]
	CMP BL,DL
	JG devam2
	INC SI
	
	;;; dizi[SI] ile dizi[DI] yı swap yap
	PUSH DX
	XOR DH,DH
	MOV DL,dizi[SI]
	XCHG DL,dizi[DI]
	MOV dizi[SI],DL
	POP DX
	
	devam2:
	POP BX
	INC DI
	LOOP dongu1

	 ;;; dizi[SI+1] ile dizi[BX] yı swap yap
	PUSH DX
	XOR DH,DH
	INC SI ; SI+1
	MOV DL,dizi[SI]
	XCHG DL,dizi[BX]
	MOV dizi[SI],DL
	POP DX
	
	MOV DX,SI ;; DX = SI+1 döndür

	RET
arrange_pivot ENDP


read_number	PROC
PUSH SI
	; isimlerin ingilizce olmasina karar verdim.
	; kullanıcıdan sayı ister. BX değerine atar
	MOV AH, 0ah  ; karakter oku
	mov dx,offset the_string  ; burada tut
	INT 21H
	
	MOV SI, offset the_string ;string'i sayiya cevir (SI'ya aktar)
	CALL string2num	;sayiyi BX'e at
POP SI
	RET
read_number ENDP


new_line	PROC
		PUSH DX
		PUSH AX
		
		MOV dl, 10	; YENI SATIR \n
		MOV ah, 02h
		INT 21h
		MOV dl, 13
		MOV ah, 02h
		INT 21h
		
		POP AX
		POP DX
		RET
new_line ENDP

print		PROC
push ax
push dx
push cx
push bx
	mov  ax, bx ; BL değerini kullanılıyor
	CMP bl,0
	JGE devam ; Eger negatif ise
		LEA DX,MSG_neg ; - sembolünü bas
		MOV AH,9
		INT 21H
		MOV AX,BX ; bastirilacak degeri tekrar ax'e at
		NEG Al ; -1 ile çarp, pozitif gibi göster	
	devam:
	
   MOV BX, 10     ;bx'i bölen olarak kullanacağız
   XOR DX,DX
   XOR CX,CX
    
          ;bölme işlemleri
bol1:  
	XOR DX,DX
    DIV BX      ;AX/BX
    PUSH DX     
    INC CX      ;rakamlari takip edecek dongu sayisi
    CMP AX, 0     ;bolecek bir sey kaldı mı?
    JNE bol1     ; sayma bittiyse
    
bol2:  POP DX      
   ADD DX, 30H     ;ascii esdegerine cevir
   MOV AH, 02H     
   INT 21H      ;karakteri goster
   LOOP bol2    

pop bx
pop cx
pop dx
pop ax
  RET
print ENDP

string2num	PROC
push cx ; ana prosedürde değerinin değişmesi istemiyoruz.
push si
;SI'yı en dusuk anlamlı basamagın adresi yapalım
  inc  si ;SI'ya almisti, girilen karakter sayisinin adresi
  mov  cl, [ si ] ;girilen karakter sayisi                                         
  xor ch,ch ;sifirlandı
  add  si, cx ; SI: en dusuk anlamlı basamagi gosteriyor
;string donusumu
  xor bx,bx
  mov  bp, 1 ;gerçek degerine ulasabilmek için 10 ile çarpacağız MULTIPLE OF 10 TO MULTIPLY EVERY DIGIT.

  xor ax,ax
repeat:
;karakterleri donustur                    
  mov  al, [ si ] ;rakami al'ye aktar
  CMP AL,'-'
  JNE positive
		; Sayı negatif ise ( '-' karakterinde isek)
		NEG BX
		JMP negative  ; döngüyü bitir
  positive:
  sub  al, 48 ;ascii karakterini rakama cevir

  xor ah,ah
  mul  bp ;AX*BP = DX:AX
  add  bx,ax ;sonucu bx'e at
;10'un katlarını arttır 1, 10, 100 diye ..
  mov  ax, bp
  mov  bp, 10
  mul  bp ;AX*10 = DX:AX
  mov  bp, ax ;10'un yeni bir katı
;bitti mi kontrol et
  dec  si ;islenecek bir sonraki rakam
  loop repeat
negative:

pop si
pop cx
  RET
string2num ENDP

CODESG  ENDS
        END BASLA
