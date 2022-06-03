IDEAL
MODEL small
STACK 100h
DATASEG
	randhelper dw 0
	x1 dw 70
	y1 dw 150
	x2 dw 230
	y2 dw 150
	yremover dw 150
	yfinish dw 0
	y2finish dw 0
	color db 7
	colorremover db 0
	LenghLine db 25
	WidthLine db 10
	LenghLine2 db 25
	WidthLine2 db 10
	yOriginal dw 150
	y2Original dw 150
	square1dir db 0
	square2dir db 0
	delay dw 15
	arrayofycoords dw 20, 30, 10, 40, 80, 10, 40, 90, 25, 70 ,30, 85, 40, 93 ,45 
	arrayofxcoords dw 10, 112, 300, 282, 53, 218, 73, 94, 243 , 135, 150, 178, 223, 165, 300
	xmeteor dw 10
	ymeteor dw ?
	meteorId dw 0
	movemeteorId dw 0
	xLine dw 149
	yLine dw 80
	score dw 0
	score2 dw 0
	P1win db 'WinP.bmp',0
	P2win db 'WinP2.bmp',0
	startScreen db 'startScr.bmp',0
	instructionScr db 'instScr.bmp',0
	filename db ? ,0
	filehandle dw ?
	Header db 54 dup (0)
	Palette db 256*4 dup (0)
	ScrLine db 320 dup (0)
	ErrorMsg db 'Error', 13, 10 ,'$'
	clearscrX dw 0
	clearscrY dw 0
	widthscr dw 320
	lenghscr dw 200
	notes dw 17E6h, 2394h,1E11h , 1AC9h, 17E6h, 2394h,1E11h , 1AC9h, 17E6h, 2394h,1E11h , 1AC9h, 17E6h, 2394h,1E11h, 1AC9h , 2F8Fh, 47B4h, 3C87h, 3592h, 2F8Fh, 47B4h, 3C87h, 3592h, 1FECh, 2F8Fh, 282Eh, 2394h, 1FECh, 2F8Fh, 282Eh, 2394h, 1FECh,  2F8Fh, 282Eh, 2394h, 1FECh, 2F8Fh, 282Eh, 3592h, 505Ch, 3C87h, 3FD8h, 3592h, 505Ch, 3C87h, 3FD8h, 47B4h, 2D40h, 282Eh, 2394h, 3592h, 2D40h, 282Eh, 2394h, 3592h, 2D40h, 282Eh, 2394h, 3592h, 2D40h, 17E6h, 2394h, 1E11h ,1AC9h ,17E6h, 2394h, 1E11h ,1AC9h, 1FECh, 2F8Fh, 282Eh,2394h, 1FECh, 2F8Fh, 282Eh,2394h, 1FECh, 2F8Fh, 282Eh,2394h, 1FECh, 2F8Fh, 282Eh, 1AC9h, 282Eh, 1E11h, 1FECh, 1AC9h, 282Eh, 1E11h ,1FECh, 2394h,2D40h , 282Eh ,2394h ,2F8Fh,2D40h ,282Eh, 2394h,2F8Fh,2D40h ,282Eh, 2394h, 2F8Fh, 2394h, 3043d, 4571d, 3836d, 3418d, 3043d, 4571d, 3836d, 3418d, 4072d, 6087d, 5120d, 4571d, 4072d, 6087d, 5120d, 4571d, 4072d, 6087d, 5120d, 4571d, 4072d, 6087d, 5120d, 3418d, 5121d, 4072d, 3836d, 4072d, 5120d, 4307d, 6087d, 5764d, 5120d, 4307d, 6087d, 5764d, 5120d, 4307d
	notes2 dw 6087d, 5764d, 5120d, 4307d, 6087d, 5764d, 5120d, 4307d
	notes_len dw 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 59, 59, 9, 9, 49, 44, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 19, 59, 59, 9, 9, 49, 59, 9, 9, 19, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 19, 59, 59, 9, 9, 44, 49, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 19, 59, 59, 9, 9, 44, 44, 9, 9, 25, 9, 9, 19, 19, 9, 9, 19,19, 9, 9, 19, 19, 19, 49, 49, 9, 9, 39, 39, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 19, 49, 49, 24, 24, 24, 24, 19, 19, 9, 9 , 19, 19, 9, 9 , 19, 19, 9, 9 , 19, 19, 9, 9 , 59
	notes_len_Copy dw 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 59, 59, 9, 9, 49, 44, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 19, 59, 59, 9, 9, 49, 59, 9, 9, 19, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 19, 59, 59, 9, 9, 44, 49, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 19, 59, 59, 9, 9, 44, 44, 9, 9, 25, 9, 9, 19, 19, 9, 9, 19,19, 9, 9, 19, 19, 19, 49, 49, 9, 9, 39, 39, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 9, 9, 19, 19, 19, 49, 49, 24, 24, 24, 24, 19, 19, 9, 9 , 19, 19, 9, 9 , 19, 19, 9, 9 , 19, 19, 9, 9 , 59
	mutespeak dw 60000
	notesId dw 210
	bool db 0
	notesId2 dw 0
	
CODESEG
proc OpenFile
	mov ah, 3Dh
	xor al, al
	int 21h
	jc openerror
	mov [filehandle], ax
	ret
	openerror :
		mov dx, offset ErrorMsg
		mov ah, 9h
		int 21h
		ret
endp OpenFile

proc ReadHeader
	; Read BMP file header, 54 bytes
	mov ah,3fh
	mov bx, [filehandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	ret
endp ReadHeader

proc ReadPalette
	; Read BMP file color palette, 256 colors * 4 bytes (400h)
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	ret
endp ReadPalette

proc CopyPal
	; Copy the colors palette to the video memory
	; The number of the first color should be sent to port 3C8h
	; The palette is sent to port 3C9h
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0
	; Copy starting color to port 3C8h
	out dx,al
	; Copy palette itself to port 3C9h
	inc dx
	PalLoop:
		; Note: Colors in a BMP file are saved as BGR values rather than RGB .
		mov al,[si+2] ; Get red value .
		shr al,2 ; Max. is 255, but video palette maximal
		; value is 63. Therefore dividing by 4.
		out dx,al ; Send it .
		mov al,[si+1] ; Get green value .
		shr al,2
		out dx,al ; Send it .
		mov al,[si] ; Get blue value .
		shr al,2
		out dx,al ; Send it .
		add si,4 ; Point to next color .
		; (There is a null chr. after every color.)
		loop PalLoop
		ret
endp CopyPal

proc CopyBitmap
	; BMP graphics are saved upside-down .
	; Read the graphic line by line (200 lines in VGA format),
	; displaying the lines from bottom to top.
	mov ax, 0A000h
	mov es, ax
	mov cx,200
	PrintBMPLoop :
		push cx
		; di = cx*320, point to the correct screen line
		mov di,cx
		shl cx,6
		shl di,8
		add di,cx
		; Read one line
		mov ah,3fh
		mov cx,320
		mov dx,offset ScrLine
		int 21h
		; Copy one line into video memory
		cld ; Clear direction flag, for movsb
		mov cx,320
		mov si,offset ScrLine
		rep movsb ; Copy line to the screen
					 ;rep movsb is same as the following code :
					 ;mov es:di, ds:si
					 ;inc si
					 ;inc di
					 ;dec cx
					 ;loop until cx=0
		pop cx
		loop PrintBMPLoop
		ret
endp CopyBitmap

proc drawpic
	call OpenFile
	call ReadHeader
	call ReadPalette
	call CopyPal
	call CopyBitmap
	ret
endp drawpic

start:
	mov ax, @data
	mov ds, ax
	mov ax, 13h
	int 10h ;graphic mode
	mov dx, offset startScreen
	call drawpic
	WaitForDesicion:
		mov ah, 1
		int 16h
		jz WaitForDesicion
		mov ah, 0
		int 16h
		cmp al, 103
		je clearTheScreen
		cmp al, 102  
		je instructionScreen
		jmp WaitForDesicion
	instructionScreen:
		mov dx, offset instructionScr
		call drawpic
		mov ah,1
		int 21h
	clearTheScreen:
		delay3:
			dec [delay]
			cmp [delay], 0
			jne delay3
		mov [delay], 15
		mov bh,0h
		mov cx, [clearscrX]
		mov dx, [clearscrY]
		mov al,[colorremover]
		mov ah,0ch
		int 10h
		cmp [widthscr], 0
		je scrLineDown
		inc [clearscrX]
		dec [widthscr]
		jmp clearTheScreen
	scrLineDown:
		cmp [lenghscr], 0
		je startTheGame
		inc [clearscrY]
		mov [clearscrX], 0
		mov [widthscr], 320
		dec [lenghscr]
		jmp clearTheScreen
	
	startTheGame:
		;jmp nextnote
		jmp checkfinish ;starting the game
		

checkfinish:
	mov bx, offset arrayofxcoords
	add bx, [meteorId]
	cmp [bx], 310
	jne resumerest
	mov [bx], 10
	mov bx, offset arrayofycoords
	add bx, [meteorId]
	mov bh,0h
	mov cx, 310
	mov dx, [bx]
	mov al,[colorremover]
	mov ah,0ch
	int 10h
	mov bh,0h
	mov cx, 309
	mov dx, [bx]
	mov al,[colorremover]
	mov ah,0ch
	int 10h
	resumerest:
		add [meteorId], 2
		cmp [meteorId], 30
		jne checkfinish
		mov [meteorId], 0
		call movemeteorfunc


incmetId:
	add [meteorId], 2 
	call buildMeteor
	
incmovemetId:
	add [moveMeteorId], 2
	call movemeteorfunc


proc movemeteorfunc
	mov bx, offset arrayofxcoords
	add bx, [movemeteorId]
	inc [bx]
	cmp [movemeteorId], 30
	jne incmovemetId
	call buildMeteor
endp movemeteorfunc
	
proc buildMeteor
	mov bx, offset arrayofycoords
	add bx, [meteorId]
	mov ax, [bx]
	mov [ymeteor], ax
	mov bx, offset arrayofxcoords
	add bx, [meteorId]
	mov ax, [bx]
	mov [xmeteor], ax
	mov bh,0h
	mov cx,[xmeteor]
	mov dx,[ymeteor]
	mov al,[color]
	mov ah,0ch
	int 10h
	dec [xmeteor]
	mov bh,0h
	mov cx,[xmeteor]
	mov dx,[ymeteor]
	mov al,[color]
	mov ah,0ch
	int 10h
	dec [xmeteor]
	mov bh,0h
	mov cx,[xmeteor]
	mov dx,[ymeteor]
	mov al,[colorremover]
	mov ah,0ch
	int 10h
	cmp [meteorId], 28
	je finish
	call incmetId
	finish:
		mov [meteorId], 0
		mov [moveMeteorId], 0
endp buildMeteor
	
buileWhiteLine:	
	mov bh,0h
	mov cx,[xLine]
	mov dx,[yLine]
	mov al,[color]
	mov ah,0ch
	int 10h
	cmp [xLine], 151
	je yLineDown
	cmp [yLine], 200
	je checkcollisions
	xLineInc:
		inc [xLine]
		jmp buileWhiteLine
	yLineDown:
		mov [xLine], 149
		inc [yLine]
		jmp buileWhiteLine	
	
checkcollisions:
	mov [xLine], 149
	mov [yLine], 80
	label1:
		mov bx, offset arrayofxcoords
		add bx, [meteorId]
		cmp [bx], 70
		jl skiprest
		cmp [bx], 81
		jg skiprest
		mov bx, offset arrayofycoords
		add bx, [meteorId]
		mov ax, [yOriginal]
		cmp [bx], ax
		jl skiprest
		add ax, 25
		cmp [bx], ax
		jg skiprest
		call collisionteleport
		skiprest:
			add [meteorId], 2
			cmp [meteorId], 30
			jne label1
			jmp checkcollisions2
					
checkcollisions2:
	mov [meteorId], 0
	label2:
		mov bx, offset arrayofxcoords
		add bx, [meteorId]
		cmp [bx], 230
		jl skiprest2
		cmp [bx], 241
		jg skiprest2
		mov bx, offset arrayofycoords
		add bx, [meteorId]
		mov ax, [y2Original]
		cmp [bx], ax
		jl skiprest2
		add ax, 25
		cmp [bx], ax
		jg skiprest2
		call collisionteleport2
		skiprest2:
			add [meteorId], 2
			cmp [meteorId], 30
			jne label2
			jmp scorelabel	
				
proc collisionteleport
	cmp [yOriginal], 0
	jne nonfinish
	inc [score]
	cmp [score], 5
	je P1Wins
	nonfinish:
		mov [widthline], 10
		mov [lenghLine], 26
		mov [x1], 70
		mov ax, [yOriginal]
		mov [yfinish], ax
		mov [y1], 150
		mov [yOriginal], 150
		cmp [square1dir], 2
		jne resetdir
		dec [yfinish]
	resetdir:
		mov [square1dir], 0
	emptysquare3:
		mov bh,0h
		mov cx,[x1]
		mov dx, [yfinish]
		mov al,[colorremover]
		mov ah,0ch
		int 10h
		cmp [widthline], 0
		je proceed8
		call addxremover
		jmp emptysquare3
	proceed8:
		cmp [lenghLine], 0
		je teleportfinish
		call linefinishdown
		jmp emptysquare3	
endp collisionteleport


teleportfinish:
	mov [x1], 70
	mov [widthline] , 10
	mov [lenghLine], 25
	jmp teleportfinish2


P1Wins:
	mov dx, offset P1Win
	call drawpic
	jmp WaitForRestart
	
WaitForRestart:
	mov ah, 1
	int 16h
	jz WaitForRestart
	mov ah, 0
	int 16h
	cmp al, 114
	jne WaitForRestart
	mov [score], 0
	mov [score2], 0
	mov [notesId], 0
	mov [meteorId], 0
	mov [widthline], 10
	mov [widthline2], 10
	mov [lenghLine], 25
	mov [lenghLine2], 25
	mov [x1], 70
	mov [y1], 150
	mov [x2], 230
	mov [y2], 150
	mov [yOriginal], 150
	mov [y2Original], 150
	mov [clearscrX], 0
	mov [clearscrY], 0
	mov [widthscr], 320
	mov [lenghscr], 200
	mov [square1dir], 0
	mov [square2dir], 0
	jmp clearTheScreen

	
P2Wins:
	mov dx, offset P2Win
	call drawpic
	jmp WaitForRestart

proc collisionteleport2
	cmp [y2Original], 0
	jne nonfinish2
	inc [score2]
	cmp [score2], 5
	je P2Wins
	nonfinish2:
		mov [widthline2], 10
		mov [lenghLine2], 26
		mov [x2], 230
		mov ax, [y2Original]
		mov [y2finish], ax
		mov [y2], 150
		mov [y2Original], 150
		cmp [square2dir], 2
		jne resetdir2
		dec [y2finish]
	resetdir2:
		mov [square2dir], 0
	emptysquare4:
		mov bh,0h
		mov cx,[x2]
		mov dx, [y2finish]
		mov al,[colorremover]
		mov ah,0ch
		int 10h
		cmp [widthline2], 0
		je proceed9
		call addxremover2
		jmp emptysquare4
	proceed9:
		cmp [lenghLine2], 0
		je teleportfinish2
		call linefinishdown2
		jmp emptysquare4	
endp collisionteleport2
		
teleportfinish2:
	mov [x2], 230
	mov [widthline2] , 10
	mov [lenghLine2], 25	

scorelabel:
	mov  dl, 1   ;Column
	mov  dh, 20  ;Row
	mov  bh, 0    ;Display page
	mov  ah, 02h  ;SetCursorPosition
	int  10h	
	mov ax, [score] 	
	add  ax, '00'		
	mov  bl, 7  ;Color of the string - white
	mov  bh, 0    ;Display page
	mov  ah, 0Eh  ;Teletype
	int  10h

scorelabel2:
	mov  dl, 38   ;Column
	mov  dh, 20   ;Row
	mov  bh, 0    ;Display page
	mov  ah, 02h  ;SetCursorPosition
	int  10h
	mov ax, [score2] 
	add  ax, '00'		
	mov  bl, 7  ;Color of the string - white
	mov  bh, 0    ;Display page
	mov  ah, 0Eh  ;Teletype
	int  10h

buildsquare1:
	mov [meteorId], 0
	mov bh,0h
	mov cx,[x1]
	mov dx,[y1]
	mov al,[color]
	mov ah,0ch
	int 10h
	cmp [WidthLine], 0
	jne addx
	cmp [LenghLine], 0
	jne linedown
	mov [widthline], 10
	mov [lenghLine], 25
	mov [x1], 70
	cmp [square1dir], 1
	je removeBehindDown
	cmp [square1dir], 2
	je removeBehindUp
	jmp buildsquare2

addx:
	inc [x1]
	dec [WidthLine]
	jmp buildsquare1

linedown:
	mov [x1], 70
	inc [y1]
	dec [LenghLine]
	mov [WidthLine], 10
	jmp buildsquare1

removeBehindDown:
	mov cx, [yOriginal]
	add cx, 26
	mov [yremover], cx
	mov bh,0h
	mov cx,[x1]
	mov dx,[yremover]
	mov al,[colorremover]
	mov ah,0ch
	int 10h
	cmp [widthline], 0
	je proceed
	call addxremover
	jmp removeBehindDown
proceed:
	mov [LenghLine], 25
	mov [WidthLine], 10
	mov [x1], 70
	jmp buildsquare2

removeBehindUp:
	mov cx, [yOriginal]
	dec cx
	mov [yremover], cx
	mov bh,0h
	mov cx,[x1]
	mov dx,[yremover]
	mov al,[colorremover]
	mov ah,0ch
	int 10h
	cmp [widthline], 0
	je proceed2
	call addxremover
	jmp removeBehindUp
proceed2:
	mov [LenghLine], 25
	mov [WidthLine], 10
	mov [x1], 70
	jmp buildsquare2

buildsquare2:
	mov bh,0h
	mov cx,[x2]
	mov dx,[y2]
	mov al,[color]
	mov ah,0ch
	int 10h
	cmp [WidthLine2], 0
	jne addx2
	cmp [LenghLine2], 0
	jne linedown2
	mov [LenghLine2], 25
	mov [WidthLine2], 10
	mov [x2], 230
	cmp [square2dir], 1
	je removeBehindDown2
	cmp [square2dir], 2
	je removeBehindUp2
	helplabel:
		jmp WaitForData
	
addx2:
	inc [x2]
	dec [WidthLine2]
	jmp buildsquare2

linedown2:
	mov [x2], 230
	inc [y2]
	dec [LenghLine2]
	mov [WidthLine2], 10
	jmp buildsquare2

removeBehindDown2:
	mov cx, [y2Original]
	add cx, 26
	mov [yremover], cx
	mov bh,0h
	mov cx,[x2]
	mov dx,[yremover]
	mov al,[colorremover]
	mov ah,0ch
	int 10h
	cmp [widthline2], 0
	je proceed3
	call addxremover2
	jmp removeBehindDown2
	proceed3:
		mov [LenghLine2], 25
		mov [WidthLine2], 10
		mov [x2], 230
		jmp WaitForData

removeBehindUp2:
	mov cx, [y2Original]
	dec cx
	mov [yremover], cx
	mov bh,0h
	mov cx,[x2]
	mov dx,[yremover]
	mov al,[colorremover]
	mov ah,0ch
	int 10h
	cmp [widthline2], 0
	je proceed4
	call addxremover2
	jmp removeBehindUp2
	proceed4:
		mov [LenghLine2], 25
		mov [WidthLine2], 10
		jmp WaitForData
	
proc addxremover2
	inc [x2]
	dec [widthline2]
	ret
endp addxremover2

proc addxremover
	inc [x1]
	dec [widthline]
	ret
endp addxremover

;mutesounds:
;	in al, 61h
	;and al, 11111100b
	;out 61h, al
	;jmp resume

copynoteslen:
	mov bx, offset notes_len
	mov si, offset notes_len_Copy
	add bx, [notesId]
	add si, [notesId]
	mov ax, [si]
	mov [bx], ax
	add [notesId], 2
	cmp [notesId], 306
	jne copynoteslen
	mov [notesId], 0
	mov [meteorId], 0
	mov [widthline], 10
	mov [widthline2], 10
	mov [lenghLine], 25
	mov [lenghLine2], 25
	jmp startTheGame

;nextnote:
	;cmp [notesId], 290
	;jg nextnote2label
	;mov [mutespeak], 60000
	;mov bx, offset notes
	;add bx, [notesId]
	;in al, 61h
	;or al, 00000011b
	;out 61h, al
	;mov al, 0B6h
	;out 43h, al
	;play frequency 
	;mov ax, [bx]
	;out 42h, al 
	;mov al, ah
	;out 42h, al		
	;add [notesId], 2
	;mov [meteorId], 0
	;mov [widthline], 10
	;mov [widthline2], 10
	;mov [lenghLine], 25
	;mov [lenghLine2], 25
	;call checkfinish
	
	
	
nextnote2label:
	;mov [mutespeak], 60000
	;mov bx, offset notes2
	;add bx, [notesId2]
	;in al, 61h
	;or al, 00000011b
	;out 61h, al
	;mov al, 0B6h
	;out 43h, al
	;play frequency 
	;mov ax, [bx]
	;out 42h, al 
	;mov al, ah
	;out 42h, al		
	;add [notesId2], 2
	;add [notesId], 2
	;cmp [notesId2], 18
	;jne finishsong
	;mov [notesId], 0
	;mov [notesId2], 0
	;jmp copynoteslen
	;finishsong:
	;	mov [meteorId], 0
	;	mov [widthline], 10
	;	mov [widthline2], 10
	;	mov [lenghLine], 25
	;	mov [lenghLine2], 25
	;	call checkfinish	
	
	
;nextnote2:
	;jmp nextnote
	
WaitForData:
	mov si, offset notes_len
	sub [notesId], 2
	add si, [notesId]
	add [notesId], 2
	mov [x2], 230
	mov [x1], 70
	mov [delay], 15000 
	mov ax, [y2Original]
	mov [y2], ax
	mov ax, [yOriginal]
	mov [y1], ax
	cmp [y2Original], 0
	jne labehelper
	call collisionteleport2
	labehelper:
		cmp [yOriginal], 0
		jne labehelper2
		call collisionteleport
	labehelper2:
		cmp [yOriginal], 175
		jne continue
		call collisionteleport
	continue:
		cmp [y2Original], 175
		jne delay2
		call collisionteleport2
	delay2:
		dec [delay]
		cmp [delay], 0
		jne delay2
		;dec [si]
		;cmp [si], 3
		;jg resume
		;cmp [si], 0
		;je nextnote2
		;jmp mutesounds
		
	resume:
		mov ah, 1
		int 16h
		jz samedirs
		mov ah, 0
		int 16h
		cmp al, 111
		je square2stop
		cmp al, 105
		je square2up
		cmp al, 107
		je square2down
		cmp al, 113
		je square1stop
		cmp al, 119
		je square1up
		cmp al, 115
		je square1down
		jmp WaitForData
	

samedirs:
	mov [delay], 15000
	cmp [square2dir], 1
	je helpaddsquare2
	cmp [square2dir], 2
	je helpsubsquare2
	jmp samedirs2
		
samedirs2: 
	mov [delay], 15000
	cmp [square1dir], 1
	je addsquare
	cmp [square1dir], 2
	je subsquare	
	call checkfinish
	
square1up:
	mov [square1dir], 1
	jmp resume	

helpaddsquare2:
	jmp addsquare2
	
square2stop:
	mov [square2dir], 0
	jmp resume
	
square2up:
	mov [square2dir], 1
	jmp resume

square2down:
	mov [square2dir], 2
	jmp resume	

square1stop:
	mov [square1dir], 0
	jmp resume	
	
square1down:
	mov [square1dir], 2
	jmp resume

helpsubsquare2:
	jmp subsquare2

addsquare:
	mov [x1], 70
	dec [yOriginal]
	mov ax, [yOriginal]
	mov [y1], ax
	mov [widthline], 10
	call checkfinish

subsquare:
	mov [x1], 70
	inc [yOriginal]
	mov ax, [yOriginal]
	mov [y1], ax
	mov [widthline], 10
	call checkfinish

addsquare2:
	mov [x2], 230
	dec [y2Original]
	mov ax, [y2Original]
	mov [y2], ax
	mov [widthline2], 10
	jmp samedirs2
	
subsquare2:
	mov [x2], 230
	inc [y2Original]
	mov ax, [y2Original]
	mov [y2], ax
	mov [widthline2], 10
	jmp samedirs2


proc linefinishdown2
	mov [widthline2], 10
	mov [x2], 230
	inc [y2finish]
	dec [lenghLine2]
	ret
endp linefinishdown2

proc linefinishdown
	mov [widthline], 10
	mov [x1], 70
	inc [yfinish]
	dec [lenghLine]
	ret
endp linefinishdown	
exit:
	mov ax, 4c00h
	int 21h
END start