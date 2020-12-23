


dseg segment 'DATA'
	BLOCK_X DW 80h ; x pozicija (kolona) bloka
	BLOCK_Y DW 0BCh ; y pozicija (linija) bloka
	BLOCK_SIZE_X DW 40h ; velicina bloka (koliko piksela u sirinu)
	BLOCK_SIZE_Y DW 0Ch ; velicina bloka (koliko piksela u visinu)
	BLOCK_VELOCITY DW 07h
	
	
	COLOR db 0Fh
	
	DUZINA_NIZA1 dw 6h ;broj blokova u prvom nizu
	DUZINA_NIZA2 dw 5h ;broj blokova u drugom nizu
	DUZINA_NIZA3 dw 4h ;broj blokova u trecem nizu
	 
	NIZ1 dw 0Fh, 41h, 73h, 0A5h, 0D7h, 109h ;x pozicije 1. niza
	VELICINA_KOCKE1 dw 28h ;duzina bloka
	POZICIJA_Y1 dw 5Ah ;y pozicija 1. niza
	BOJA1 db 2h ;boja blokova 1
	
	NIZ2 dw 1Eh, 56h, 8Eh, 0C6h, 0FEh ;x pozicije 2. niza
	VELICINA_KOCKE2 dw 24h ;duzina bloka
	POZICIJA_Y2 dw 3Ch ;y pozicija 1. niza
	BOJA2 db 0Eh ;boja blokova 1
	
	NIZ3 dw 2Dh, 6Eh, 0AFh, 0F0h ;x pozicije 2. niza
	VELICINA_KOCKE3 dw 23h ;duzina bloka
	POZICIJA_Y3 dw 1Eh ;y pozicija 3. niza
	BOJA3 db 4h ;boja blokova 1
	
	
	 
	
	
	WINDOW_WIDTH DW 140h   ;the width of the window (320 pixels)
	WINDOW_HEIGHT DW 0C8h  ;the height of the window (200 pixels)
	WINDOW_BOUNDS DW 0h
	TIME_AUX DB 0 ; vremenska promenljiva 
	BALL_X DW 80h ; X pozicija (kolona) lopte
	BALL_Y DW 0AFh ; Y pozicija (linija) lopte
	BALL_SIZE DW 04h ; velicina lopte (koliko piksela u sirinu i u visinu)
	BALL_VELOCITY_X DW 04h ; x (horizontalna) komponenta brzine
	BALL_VELOCITY_Y DW 02h ; Y (vertikalna) komponenta brzine

dseg ends

cseg segment 'CODE'

	
	
assume cs:cseg, ds:dseg, ss:sseg
	draw:
	
	
	call CLEAR_SCREEN
	
	
	CALL DRAW_BLOCK
	mov ax, dseg
    mov ds, ax
	
	CHECK_TIME:
	
		mov ah,2Ch ; prekid kojim se dobija sistemsko vreme
		int 21h    ; CH = sati, CL = minuti, DH = sekunde, DL = stotinke
		
		cmp dl,TIME_AUX  ; da li je trenutno vreme jednako prethodnom (TIME_AUX)?
		je CHECK_TIME    ; ako je isto, proveri ponovo; inace ucrtaj loptu, pomeri je....
		
		mov TIME_AUX,dl ; azuriraj vreme
		
		call CLEAR_SCREEN ; obrisi sadrzaj ekrana
		
		call MOVE_BALL ; pomeri loptu
		call DRAW_BALL  ; ucrtaj  je
		call CHANGE_COLOR ;promeni boju
		
		call MOVE_BLOCK ;pomeri blok
		call DRAW_BLOCK ;ucrtaj blok
		
		call DRAW_BRICKS ;ucrtaj cigle
		
		jmp CHECK_TIME ; proveri vreme ponovo
	jmp kraj
		
	
	DRAW_BLOCK PROC NEAR
		
		mov cx,BLOCK_X ; postavi inicijalnu kolonu (X)
		mov dx,BLOCK_Y ; postavi inicijalni red (Y)
		
		DRAW_BLOCK_HORIZONTAL:
			mov ah,0Ch ; podesi konfiguraciju za ispis piksela
			mov al,01h ; izaberi plavu boju
			mov bh,00h ; 
			int 10h    ; izvrsi konfiguraciju
			
			inc cx     ;cx = cx + 1
			mov ax,cx  
			sub ax,BLOCK_X ;cx - BLOCK_X > BLOCK_SIZE_X (ako jeste, iscrtali smo za taj red sve kolone; inace nastavljamo dalje)
			cmp ax,BLOCK_SIZE_X
			jng DRAW_BLOCK_HORIZONTAL
			
			mov cx,BLOCK_X ; vrati cx na inicijalnu kolonu
			inc dx        ; idemo u sledeci red
			
			mov ax,dx    ; dx - BLOCK_Y > BLOCK_SIZE_Y (ako jeste, iscrtali smo sve redove piksela; inace nastavljamo dalje)
			sub ax,BLOCK_Y
			cmp ax,BLOCK_SIZE_Y
			jng DRAW_BLOCK_HORIZONTAL
		
		ret
	DRAW_BLOCK ENDP
	
	
	
	DRAW_BRICKS PROC NEAR
		
		
		mov di,0h
		lea bx,NIZ1 ;bx = adresa prvog niza
		
		DRAW_BRICKS1:
			mov dx,POZICIJA_Y1 ; postavi inicijalni red (Y)
			mov cx,[bx][di]	; postavi inicijalnu kolonu (X)
			
			cmp cx,0h ;proveravamo da li je blok obrisan, ukoliko jeste nastavi sa crtanjem sledeceg
			je NASTAVAK_CRTANJA1
			
			mov ax, cx
			sub ax, BALL_SIZE
			cmp ax, BALL_X	;proveravamo da li je lopta levo od bloka
			jg DRAW_BRICK1_HORIZONTAL
			mov ax,cx
			add ax,VELICINA_KOCKE2
			cmp ax,BALL_X ;proveravamo da li je lopta desno od bloka
			jl DRAW_BRICK1_HORIZONTAL
			mov ax, dx
			sub ax, BALL_SIZE
			cmp ax, BALL_Y ;proveravamo da li je lopta iznad od bloka
			jg DRAW_BRICK1_HORIZONTAL
			mov ax,dx
			add ax,0Ah
			cmp ax,BALL_Y ;proveravamo da li je lopta ispod od bloka
			jl DRAW_BRICK1_HORIZONTAL
			
			
			;ako smo dosli dovde znaci da je lopta udarila blok
			
			mov [bx][di],0h ;brisemo blok (nula za x poziciju znaci da ne treba da se crta)
			jmp NASTAVAK_CRTANJA1
		
		DRAW_BRICK1_HORIZONTAL:
			mov ah,0Ch ; podesi konfiguraciju za ispis piksela
			mov al,BOJA1 ; zelena boja
			mov bh,00h ; 
			int 10h; izvrsi konfiguraciju
			
			
			
			inc cx     ;cx = cx + 1
			mov ax,cx  
			sub ax,[bx][di] ;
			cmp ax,VELICINA_KOCKE1
			jng DRAW_BRICK1_HORIZONTAL
			
			mov cx,[bx][di] ; vrati cx na inicijalnu kolonu
			inc dx        ; idemo u sledeci red
			
			mov ax,dx    
			sub ax,POZICIJA_Y1
			cmp ax,0Ah
			jng DRAW_BRICK1_HORIZONTAL
			
			NASTAVAK_CRTANJA1:
				inc di
				inc di
				mov ax,di ;povecali smo di i smestili ga u ax
				shr ax,1h ;ax=ax/2 (zato sto je dw 2 bajta)
				cmp ax,DUZINA_NIZA1
				jl DRAW_BRICKS1
			
			
			
			
		
		mov di,0h
		lea bx,NIZ2 ;bx = adresa prvog niza
		
		DRAW_BRICKS2:
			mov dx,POZICIJA_Y2 ; postavi inicijalni red (Y)
			mov cx,[bx][di]	; postavi inicijalnu kolonu (X)
			
			cmp cx,0h	;proveravamo da li je blok obrisan, ukoliko jeste nastavi sa crtanjem sledeceg
			je NASTAVAK_CRTANJA2
			
			mov ax, cx
			sub ax, BALL_SIZE
			cmp ax, BALL_X 		;proveravamo da li je lopta levo od bloka
			jg DRAW_BRICK2_HORIZONTAL
			mov ax,cx
			add ax,VELICINA_KOCKE2
			cmp ax,BALL_X		;proveravamo da li je lopta desno od bloka
			jl DRAW_BRICK2_HORIZONTAL
			mov ax, dx
			sub ax, BALL_SIZE
			cmp ax, BALL_Y		;proveravamo da li je lopta iznad od bloka
			jg DRAW_BRICK2_HORIZONTAL
			mov ax,dx
			add ax,0Ah
			cmp ax,BALL_Y		;proveravamo da li je lopta ispod od bloka
			jl DRAW_BRICK2_HORIZONTAL
			
			mov [bx][di],0h
			jmp NASTAVAK_CRTANJA2
		
		
		DRAW_BRICK2_HORIZONTAL:
			mov ah,0Ch ; podesi konfiguraciju za ispis piksela
			mov al,BOJA2 ; zuta boja
			mov bh,00h ; 
			int 10h    ; izvrsi konfiguraciju
			
			inc cx     ;cx = cx + 1
			mov ax,cx  
			sub ax,[bx][di] ;cx - BALL_X > BALL_SIZE (ako jeste, iscrtali smo za taj red sve kolone; inace nastavljamo dalje)
			cmp ax,VELICINA_KOCKE2
			jng DRAW_BRICK2_HORIZONTAL
			
			mov cx,[bx][di] ; vrati cx na inicijalnu kolonu
			inc dx        ; idemo u sledeci red
			
			mov ax,dx    ; dx - BALL_Y > BALL_SIZE (ako jeste, iscrtali smo sve redove piksela; inace nastavljamo dalje)
			sub ax,POZICIJA_Y2
			cmp ax,0Ah
			jng DRAW_BRICK2_HORIZONTAL
			
			NASTAVAK_CRTANJA2:
				inc di
				inc di
				mov ax,di ;povecali smo di i smestili ga u ax
				shr ax,1h ;ax=ax/2 (zato sto je dw 2 bajta)
				cmp ax,DUZINA_NIZA2
				jl DRAW_BRICKS2
			
			
			
			
			
			
		mov dx,POZICIJA_Y3 ; postavi inicijalni red (Y)
		mov di,0h
		lea bx,NIZ3 ;bx = adresa prvog niza
		
		DRAW_BRICKS3:
			mov dx,POZICIJA_Y3 ; postavi inicijalni red (Y)
			mov cx,[bx][di]	; postavi inicijalnu kolonu (X)
		
			cmp cx,0h	;proveravamo da li je blok obrisan, ukoliko jeste nastavi sa crtanjem sledeceg
			je NASTAVAK_CRTANJA3
			
			mov ax, cx
			sub ax, BALL_SIZE
			cmp ax, BALL_X 		;proveravamo da li je lopta levo od bloka
			jg DRAW_BRICK3_HORIZONTAL
			mov ax,cx
			add ax,VELICINA_KOCKE2
			cmp ax,BALL_X		;proveravamo da li je lopta desno od bloka
			jl DRAW_BRICK3_HORIZONTAL
			mov ax, dx
			sub ax, BALL_SIZE
			cmp ax, BALL_Y		;proveravamo da li je lopta iznad od bloka
			jg DRAW_BRICK3_HORIZONTAL
			mov ax,dx
			add ax,0Ah
			cmp ax,BALL_Y		;proveravamo da li je lopta ispod od bloka
			jl DRAW_BRICK3_HORIZONTAL
			
			mov [bx][di],0h
			jmp NASTAVAK_CRTANJA3
			
		
		DRAW_BRICK3_HORIZONTAL:
			mov ah,0Ch ; podesi konfiguraciju za ispis piksela
			mov al,BOJA3 ; crvena boja
			mov bh,00h ; 
			int 10h    ; izvrsi konfiguraciju
			
			
			
			inc cx     ;cx = cx + 1
			mov ax,cx  
			sub ax,[bx][di] ;cx - BALL_X > BALL_SIZE (ako jeste, iscrtali smo za taj red sve kolone; inace nastavljamo dalje)
			cmp ax,VELICINA_KOCKE3
			jng DRAW_BRICK3_HORIZONTAL
			
			mov cx,[bx][di] ; vrati cx na inicijalnu kolonu
			inc dx        ; idemo u sledeci red
			
			mov ax,dx    ; dx - BALL_Y > BALL_SIZE (ako jeste, iscrtali smo sve redove piksela; inace nastavljamo dalje)
			sub ax,POZICIJA_Y3
			cmp ax,0Ah
			jng DRAW_BRICK3_HORIZONTAL
			
			NASTAVAK_CRTANJA3:
				inc di
				inc di
				mov ax,di ;povecali smo di i smestili ga u ax
				shr ax,1h ;ax=ax/2 (zato sto je dw 2 bajta)
				cmp ax,DUZINA_NIZA3
				jl DRAW_BRICKS3
			
			
			
			

		
		ret
	
	
	
	DRAW_BRICKS ENDP
	
	
	
	
	
	
	
	
	
	
	MOVE_BALL PROC NEAR
		
		mov ax,BALL_VELOCITY_X    
		add BALL_X,ax             ; pomeri lopticu horizontalno
		
		mov ax,WINDOW_BOUNDS
	
		cmp BALL_X,ax                         
		jle NEG_VELOCITY_X         ; BALL_X < 0 + WINDOW_BOUNDS (sudar - leva ivica)
		
		mov ax,WINDOW_WIDTH
		sub ax,BALL_SIZE
		sub ax,WINDOW_BOUNDS
		dec ax
		cmp BALL_X,ax	          ;BALL_X > WINDOW_WIDTH - BALL_SIZE  - WINDOW_BOUNDS (sudar - desna ivica)
		jg NEG_VELOCITY_X
		
		
		mov ax,BALL_VELOCITY_Y
		add BALL_Y,ax             ; pomeri lopticu vertikalno
		
		mov ax,WINDOW_BOUNDS
		dec ax
		cmp BALL_Y,ax   		;BALL_Y < 0 + WINDOW_BOUNDS (sudar - gornja ivica)
		jl NEG_VELOCITY_Y                          
		
		mov ax,WINDOW_HEIGHT	
		sub ax,BALL_SIZE
		sub ax,BLOCK_SIZE_Y
		sub ax,WINDOW_BOUNDS
		cmp BALL_Y,ax
		jge PROVERA_X_POZICIJE
		
		ret
		
		PROVERA_X_POZICIJE:
		
			mov ax, BLOCK_X
			sub ax, BALL_SIZE
			cmp ax, BALL_X
			jg KR
			mov ax,BLOCK_X
			add ax,BLOCK_SIZE_X
			cmp ax,BALL_X
			jl KR
			jmp NEG_VELOCITY_Y
		
		
		KR:
			jmp kraj
		
				  
		
		ret
		
		NEG_VELOCITY_X:
			neg BALL_VELOCITY_X   ;BALL_VELOCITY_X = - BALL_VELOCITY_X
			ret
			
		NEG_VELOCITY_Y:
			neg BALL_VELOCITY_Y   ;BALL_VELOCITY_Y = - BALL_VELOCITY_Y
			ret
		
	MOVE_BALL ENDP
	
	
	
	CHANGE_COLOR PROC NEAR
	
		
		mov ax,WINDOW_BOUNDS
	
		cmp BALL_X,ax                         
		jle CHANGE_R         ; BALL_X < 0 + WINDOW_BOUNDS (sudar - leva ivica)
		
		mov ax,WINDOW_WIDTH
		sub ax,BALL_SIZE
		sub ax,WINDOW_BOUNDS
		dec ax
		cmp BALL_X,ax	          ;BALL_X > WINDOW_WIDTH - BALL_SIZE  - WINDOW_BOUNDS (sudar - desna ivica)
		jg CHANGE_G
		
		mov ax,WINDOW_BOUNDS
		dec ax
		cmp BALL_Y,ax   		;BALL_Y < 0 + WINDOW_BOUNDS (sudar - gornja ivica)
		jl CHANGE_B
		
			ret
		
		CHANGE_R:
		
			mov COLOR, 04h
				ret
	
		CHANGE_G:
			
			mov COLOR,02h
				
				ret
		
		CHANGE_B:
			
			mov COLOR, 09h
				
			
		
		RET
		
	CHANGE_COLOR ENDP
	
	
	
	MOVE_BLOCK PROC NEAR
		
		mov ah,01h
		int 16h
		
		jz EXIT_BLOCK_MOVEMENT
		
		mov ah,00h
		int 16h
		
		
		cmp ah,4Bh ;ako je leva strelica
		je MOVE_BLOCK_LEFT
		
		
		cmp ah,4Dh
		je MOVE_BLOCK_RIGHT
		jmp EXIT_BLOCK_MOVEMENT
		
		MOVE_BLOCK_LEFT:
			mov ax,BLOCK_VELOCITY
			sub BLOCK_X,ax
			
			
			cmp BLOCK_X,0h
			jl FIX_BLOCK_LEFT
			jmp EXIT_BLOCK_MOVEMENT
			
			FIX_BLOCK_LEFT:
				mov BLOCK_X,0h
				jmp EXIT_BLOCK_MOVEMENT
				
		
		MOVE_BLOCK_RIGHT:
			mov ax,BLOCK_VELOCITY
			add BLOCK_X,ax
			mov ax,WINDOW_WIDTH
			sub ax,WINDOW_BOUNDS
			sub ax,BLOCK_SIZE_X
			dec ax
			cmp BLOCK_X,ax
			jg FIX_BLOCK_RIGHT
			jmp EXIT_BLOCK_MOVEMENT
			
			FIX_BLOCK_RIGHT:
				mov BLOCK_X,ax
				jmp EXIT_BLOCK_MOVEMENT
				
	EXIT_BLOCK_MOVEMENT:
	
		ret
	
	MOVE_BLOCK ENDP
	
	DRAW_BALL PROC NEAR
		
		mov cx,BALL_X ; postavi inicijalnu kolonu (X)
		mov dx,BALL_Y ; postavi inicijalni red (Y)
		
		DRAW_BALL_HORIZONTAL:
			mov ah,0Ch ; podesi konfiguraciju za ispis piksela
			mov al,COLOR ; izaberi belu boju
			mov bh,00h ; 
			int 10h    ; izvrsi konfiguraciju
			
			inc cx     ;cx = cx + 1
			mov ax,cx  
			sub ax,BALL_X ;cx - BALL_X > BALL_SIZE (ako jeste, iscrtali smo za taj red sve kolone; inace nastavljamo dalje)
			cmp ax,BALL_SIZE
			jng DRAW_BALL_HORIZONTAL
			
			mov cx,BALL_X ; vrati cx na inicijalnu kolonu
			inc dx        ; idemo u sledeci red
			
			mov ax,dx    ; dx - BALL_Y > BALL_SIZE (ako jeste, iscrtali smo sve redove piksela; inace nastavljamo dalje)
			sub ax,BALL_Y
			cmp ax,BALL_SIZE
			jng DRAW_BALL_HORIZONTAL
		
		ret
	DRAW_BALL ENDP
	
	
	
		CLEAR_SCREEN PROC NEAR
			mov ah,00h ; postaviti konfiguraciju za video mod
			mov al,13h ;
			int 10h    ; izvrsi konfiguraciju
		
			mov ah,0bh ; postavi konfiguraciju  za boju pozadine
			mov bh,00h ;
			mov bl,04h ; boja pozadine = crvena
			int 10h    ; izvrsi konfiguraciju
			
			
			
			ret
	CLEAR_SCREEN ENDP
	
	
kraj:   mov ax, 4c00h		 ; exit
		int 21h
cseg 	ends
	


sseg segment stack 'STACK'

     dw 64 dup(?)

sseg ends
end draw