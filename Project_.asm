.MODEL COMPACT
.STACK 64

;Macroos
DisplayString MACRO STR
    PushALL
    mov Ah,09h
    mov dx, offset STR
    int 21h    
    PopALL
ENDM DisplayString

DisplayStringRead MACRO STR
    mov Ah,09h
    mov dx, offset STR+2
    int 21h    
ENDM DisplayString

ReadString MACRO PromptMessege
    mov Ah,0Ah 
    mov Dx, Offset PromptMessege
    int 21h        
ENDM ReadString     

MoveCursor MACRO X,Y
    mov Ah,02H
    Mov Dl,X
    Mov Dh,Y
    int 10h    
ENDM MoveCursor

;New Fixed line with constant start X and variable Y
NewFLine MACRO X,Step
    PushALL
    ;Geting cursor position   
    mov Ah,03h
    mov bh,0h
    int 10h
    ;Dl=X-Position Dh=Y-Position

    Add DH,Step ;To reach new line

    ;Set Cursor at new line
    mov Ah,02H
    Mov Dl,X
    int 10h  
    PopAll  
ENDM NewFLine  

PushALL MACRO
    Push AX
    Push BX
    Push CX
    Push DX
ENDM PushALL

PopALL MACRO
    Pop DX
    Pop CX
    Pop BX
    Pop Ax
ENDM PopALL

Clear MACRO 
    mov Ax,3
    int 10h
ENDM Clear  

StringToNumber Macro  InputNumber 
    Local MyLoop,Num,Continue
    Mov SI,offset InputNumber
    Mov Cl,[SI+1]
    MyLoop:
    Mov Al,[SI+2]
    CMP Al,40H   ;If less (Number)   // More (Letter)
    JC Num
    Sub Al,37H
    JMP Continue
    Num: Sub Al,30H
    Continue:
    Mov [SI+2],Al
    INC SI
    DEC Cl
    JNZ MyLoop
ENDM StringToNumber

NumberToString Macro InputNumber,DispNumber
    Local LOOPX,Num,ContinueFirst,ContinueSecond,Num1
    PushALL
        Mov SI,offset InputNumber
        Mov DI,offset DispNumber+3
        Mov CL,2
        LOOPX:
        Mov Al,[SI]
        Mov DL,00001111B
        AND Al,Dl   ;To get the first 4 bits
        CMP Al,1010B
        JB Num
        Add Al,37H
        JMP ContinueFirst
        Num: Add AL,30H
        ContinueFirst:
        Mov [DI],Al  ;To put the first byte
        DEC DI
        Mov Al,[SI]
        SHR Al,4    ;to get the second 4 bits
        CMP Al,1010B
        JB Num1
        Add Al,37H
        JMP ContinueSecond
        Num1: Add AL,30H
        ContinueSecond:
        Mov [DI],Al
        DEC DI
        INC SI
        DEC CL
        CMP Cl,0
        JNZ LOOPX
    PopAll
ENDM NumberToString

MemoryToString Macro DispNumber
    Local LOOPX,Num,ContinueFirst,ContinueSecond,Num1
    PushALL
    PUSH SI 
    PUSH DI
        Mov DI,offset DispNumber+1
        Mov Al,[SI]
        Mov DL,00001111B
        AND Al,Dl   ;To get the first 4 bits
        CMP Al,1010B
        JB Num
        Add Al,37H
        JMP ContinueFirst
        Num: Add AL,30H
        ContinueFirst:
        Mov [DI],Al  ;To put the first byte
        DEC DI
        Mov Al,[SI]
        SHR Al,4    ;to get the second 4 bits
        CMP Al,1010B
        JB Num1
        Add Al,37H
        JMP ContinueSecond
        Num1: Add AL,30H
        ContinueSecond:
        Mov [DI],Al
    POP DI 
    POP SI              
    POPALL
ENDM MemoryToString

STRINGCOPY Macro M1,M2,Size
    PushALL
    mov AX,DS 
    mov ES,AX
    mov SI,offset M1 
    mov DI,offset M2
    Mov Cx,0
    mov cl,Size
    REP MOVSB
    PopALL
ENDM STRINGCOPY

STRINGCOMPARE Macro M1,M2,Size,Indicator
    Local LOP,X
    PushALL
    mov AX,DS 
    mov ES,AX
    mov SI,offset M1 
    mov DI,offset M2 + 2
    Mov Cx,Size
    REPE CMPSB

    CMP CX,0
    JZ LOP
    Mov Indicator,0
    JMP X
    LOP: Mov Indicator,1
    X:
    PopALL
ENDM STRINGCOMPARE

DrawPixel Macro X,Y,Color
    mov bh,0
    mov al, Color
    mov cx, X
    mov dx, Y
    mov ah, 0ch
    int 10h 
ENDM DrawPixel

DisplayCharacter Macro Letter,Color
    ;bh=Page Number, al=Letter ASCII ,CX=Number of times,bl=Colour
    mov ah,9 
    mov bh,0
    mov al,letter
    mov cx,1
    mov bl,Color
    int 10h
ENDM DisplayCharacter

DisplayCharacterSTR Macro Color,X,Y,Size
    Local STRLOOP 
    PushALL
    Mov DI,Size
    Mov DL,X   
    STRLOOP:   
        mov Ah,02H
        Mov Dh,Y
        int 10h  
        
        
        ;bh=Page Number, al=Letter ASCII ,CX=Number of times,bl=Colour
        mov ah,9 
        mov bh,0
        mov al,[SI]
        mov cx,1
        mov bl,Color
        int 10h

        
        INC SI
        INC DL

    DEC DI
    JNZ STRLOOP
    PopALL
ENDM DisplayCharacterSTR

ReadCharacter Macro x
    PushALL
    mov ah, 1
    int 21h 
    Mov x,al
    PopALL
ENDM ReadCharacter

DrawVLinePixel Macro X,Y1,Y2,Color
    Local LineLoop
    PushALL
    Mov SI,Y1
    LineLoop:
    DrawPixel X,SI,Color
    INC SI
    CMP SI,Y2
    JNZ LineLoop
    PopALL
ENDM DrawVLinePixel

DrawHLinePixel Macro X1,X2,Y,Color
    Local LineLoop1
    PushALL
    Mov SI,X1
    LineLoop1:
    DrawPixel SI,Y,Color
    INC SI
    CMP SI,X2
    JNZ LineLoop1
    PopALL
ENDM DrawHLinePixel

DrawDiagonalRLinePixel Macro X1,X2,Y1,Color
    Local LineLoop1
    PushALL
    Mov SI,X1
    Mov DI,Y1
    LineLoop1:
    DrawPixel SI,DI,Color
    INC SI
    INC DI
    CMP SI,X2
    JNZ LineLoop1
    PopALL
ENDM DrawDiagonalRLinePixel

DrawDiagonalPLinePixel Macro X1,X2,Y1,Color
    Local LineLoop1
    PushALL
    Mov SI,X1
    Mov DI,Y1
    LineLoop1:
    DrawPixel SI,DI,Color
    INC SI
    Dec DI
    CMP SI,X2
    JNZ LineLoop1
    PopALL
ENDM DrawDiagonalPLinePixel

DrawSquarePixel Macro X1,X2,Y1,Y2,Color
    Local SquareLoop
    PushALL
    Mov DI,Y1
    SquareLoop:
    DrawHLinePixel X1,X2,DI,Color
    INC DI
    CMP DI,Y2
    JNZ SquareLoop
    PopALL
ENDM DrawSquarePixel

ClearString Macro STRING,Size
    Local ZLOOPO
    PushALL
    Mov SI,Offset STRING+2
    Mov CX,Size
    ZLOOPO:
    Mov Al,SDollar
    Mov [SI],Al
    INC SI
    DEC CX
    JNZ ZLOOPO
    POPALL
ENDM ClearString

MakeCapital Macro STRING,Size
    Local M,BIG
    PushALL
    Mov SI,Offset STRING+2
    Mov CX,Size
    M:
    Mov AH,[SI]
    CMP AH,61H
    JB BIG
    CMP Ah,7AH
    JA BIG
    SUB AH,20H
    BIG:
    Mov [SI],AH
    INC SI
    DEC CX
    JNZ M
    POPALL
ENDM MakeCapital

ReverseCapital Macro STRING,Size
    Local M,BIG
    PushALL
    Mov SI,Offset STRING+2
    Mov CX,Size
    M:
    Mov AH,[SI]
    CMP AH,61H
    JC BIG
    Add AH,20H
    BIG:
    Mov [SI],AH
    INC SI
    DEC CX
    JNZ M
    POPALL
ENDM ReverseCapital

FCharacterCheck1 Macro STR,Size,CharFound  ;To check (if present) forbidden character of P1
    Local X
    mov AX,DS 
    mov ES,AX
    Mov CharFound,0
    Mov DI,offset STR + 2
    Mov AL, P2char
    Mov Cx, Size
    REPNE SCASB 
    CMP CL,0
    JZ X 
    Mov CharFound,1
    X:
ENDM FCharacterCheck1

FCharacterCheck2 Macro STR,Size,CharFound ;To check (if present) forbidden character of P2
    Local X
    mov AX,DS 
    mov ES,AX
    Mov CharFound,0
    Mov DI,offset STR + 2
    MakeCapital P1Char,1
    Mov AL, P1char
    Mov Cx, Size
    REPNE SCASB 
    CMP CL,0
    JZ X 
    Mov CharFound,1
    X:
ENDM FCharacterCheck2

HexaToDecimal Macro InputNumber,DispNumber
    Local LOOPX,Num,Continue
        PushALL
        Push DI
        ClearString DispNumber,4
        mov ax, InputNumber
        Mov DI,offset DispNumber
        Mov BX,10
        mov cx, 0
        LOOPX:
        Mov DX,0
        Div BX 
        push dx
        inc cx
        cmp ax,0
        jnz loopx

        Continue:
        pop dx
        add dx,30h
        mov [di], dl
        inc di
        DEC cx
        JNZ Continue
        pop DI
        PopALL
ENDM HexaToDecimal


.DATA
;First Screen
NameX db 'Please enter your Name: ','$'
Points db 'Enter your points: ','$'
Continue db 'Press any key to continue: ','$'
Spam db 2,?,2 dup('$')
XPoint EQU 20

;First Player
P1Name db 30,?,30 dup('$')
P1Points db 5,?,5 dup('$')

;Second Player
P2Name db 30,?,30 dup('$')
P2Points db 5,?,5 dup('$')



;Second Screen (Main Screen)
PlayMessege db 'Your other party is: ','$'
ChatInvitationRecieved db '- Chat invitation recieved (F1) ','$'  
GameInvitationRecieved db '- Game invitation recieved (F2) ','$'

ChatInvitationSent db '- You sent a chat invitation','$'  
GameInvitationSent db '- You sent a Game invitation','$'

Chat db 'To start Chatting press F1','$'
Game db 'To start the Game F2','$'
EndProgram db 'To End the program press ESC','$'

ChoiceP1 db ?
ChoiceP2 db ? 
Flag db 0

;Third Screen (Chat Mode)
P1Chat db 50,?,50 dup('$')
P2Chat db 50,?,50 dup('$')
SDollar db '$'


;;GAME MODE SCREEENS;;
;CHOSE POINTS SCREEN:
    InitialPoints db 'Enter initial points ?','$'
    P1_Game_Points db 5,?,5 dup('$')
    P2_Game_Points db 5,?,5 dup('$')
    P1_Score DW ?
    P2_Score DW ?
    P1_ScoreP DB 5 dup('$')
    P2_ScoreP DB 5 dup('$')
   

;Choose Level Screen
    ChooseLevel db 'C','h','o','o','s','e','_','t','h','e','_','l','e','v','e','l','?'
    Level db ?
    Loading db 'L','O','A','D','I','N','G'

;Choose forbidden character screen
    ChosenLevel db 'Level:' ,'$'
    ChooseCharacter1 db 'Choose forbidden','$'
    ChooseCharacter2 db 'character', '$'
    P1char db ?
    P2char db ?

;Game Main Game Screen
Target dw 105EH
Winner db ?
Scores db 'Score','$'
CharF db 0
ToggleFlag db 0
Case db 0

R1  db 'A','X'
    db 'B','X'
    db 'C','X'
    db 'D','X'
    db 'S','I'
    db 'D','I'
    db 'S','P'
    db 'B','P'

    Trial db 5 dup('$')
    MTrial db 3 dup('$')
    P1Memory    DB 12H, 34H, 10H, 00, 0A0H, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
    P2Memory    DB 00, 90H, 66H, 00, 80H, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
    P1Registers dw 5F11H ;P1AX
                dw 1255H ;P1BX
                dw 1455H ;P1CX
                dw 1800H ;P1DX
                dw 1452H ;P1SI
                dw 1999H ;P1DI
                dw 2000H ;P1SP
                dw 9911H ;P1BP

    P2Registers dw 1111H ;P2AX
                dw 1255H ;P2BX
                dw 1455H ;P2CX
                dw 1800H ;P2DX
                dw 1452H ;P2SI
                dw 1999H ;P2DI
                dw 2000H ;P2SP
                dw 9911H ;P2BP


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    P1Command db 5,?,5  dup('$')
    P2Command db 5,?,5  dup('$') 
    CommandNumber db ?

    P1Source db 7,?,7  dup('$')
    P2Source db 7,?,7  dup('$') 
    SourceNumber dw ?

    P1Destination db 5,?,5  dup('$') 
    P2Destination db 5,?,5  dup('$')  
    DestinationNumber dw ?    

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;Commands
        ComMOV db 'MOV'
        ComADD db 'ADD'
        ComADC db 'ADC'
        ComSUB db 'SUB'
        ComSBB db 'SBB'
        ComDIV db 'DIV'
        ComMUL db 'MUL'
        ComXOR db 'XOR'
        ComAND db 'AND'
        ComNOP db 'NOP'
        ComSHR db 'SHR'
        ComSHL db 'SHL'
        ComSAR db 'SAR'
        ComCLC db 'CLC'
        ComROR db 'ROR'
        ComRCL db 'RCL'
        ComRCR db 'RCR'
        ComROL db 'ROL'
        ComINC db 'INC'
        ComDEC db 'DEC'
        ComOR db 'OR'
        ;Registers
        RAX db 'AX'
        RBX db 'BX'
        RCX db 'CX'
        RDX db 'DX'
        RSI db 'SI'
        RDI db 'DI'
        RSP db 'SP'
        RBP db 'BP'
        RAL db 'AL'
        RAH db 'AH'
        RBL db 'BL'
        RBH db 'BH'
        RCLL db 'CL'
        RCH db 'CH'
        RDL db 'DL'
        RDH db 'DH'
        SBracket db '['
        ;MEMORY ADDRESSES
        Address db '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;POWER UPS
    PorC db ?
    PowerUp db ?
    P1Stock db 1, 1, 1
    P2Stock db 1, 1, 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NumberHolder dw ? 
ChooseRegister db ?
Initially db 0

;Sores Screen
PWinner db 'W','I','N','N','E','R'


;Shooting
mai db ?
x1 dw ?
y1 dw ?
x2 dw ?
y2 dw ?
yay dw ?
Shooted db ?

;ENDSCREEN
FINISHED db 'Game Stopped','$'



.CODE
;..................................
;           First Screen          ;
;..................................
JMP Main
FirstS PROC
 ReEnterN1:  clear
    ;For Displaying Lines
    MoveCursor 20D,5D 
    DisplayString NameX
    NewFLine XPoint,4
    DisplayString Points
    NewFLine XPoint,4
    DisplayString Continue
    
    ;For Reading lines 
                      
        MoveCursor 20D,7D 
        ReadString P1Name
        CMP P1Name+1, 15D
        JB C11
        JMP ReEnterN1
        C11:
        CMP P1Name+2 , 41H
        JA C12
        JMP ReEnterN1
        C12:
        CMP P1Name+2 , 7AH
        JB C13
        JMP ReEnterN1
        C13:

    NewFLine XPoint,4
    ReadString P1Points
    NewFLine XPoint,4
    ReadString Spam

   ReEnterN2:  Clear
        
    ;For Displaying Lines
    MoveCursor 20D,5D 
    DisplayString NameX
    NewFLine XPoint,4
    DisplayString Points
    NewFLine XPoint,4
    DisplayString Continue
    
    ;For Reading lines                                           
        MoveCursor 20D,7D 
        ReadString P2Name
        CMP P2Name+1, 15D
        JB C21
        JMP ReEnterN2
        C21:
        CMP P2Name+2 , 41H
        JA C22
        JMP ReEnterN2
        C22:
        CMP P2Name+2 , 7AH
        JB C23
        JMP ReEnterN2
        C23:

    NewFLine XPoint,4
    ReadString P2Points
    NewFLine XPoint,4
    ReadString Spam
    RET       
FirstS ENDP

;..................................
;          Second Screen          ;
;.................................. 
SecondS Proc
    Clear
    Mov Ah,0
    Mov Al,03h
    INT 10H
    ;Designing the screen 
    ;Drawing a vertical line at the middle if the Screen (Main Screens) 
    Mov Cl,24D
    Mov Bl,0
    VLine: 
    MoveCursor 40,Bl
        Mov Ah,2
        Mov Dl, '|'
        int 21H
    INC Bl
    DEC Cl
    JNZ VLine

    ;Drawing a horizontal line at the bottom of the screen (Notification Bars)
    Mov Cl,80D
    Mov Bl,0
    HLine: 
    MoveCursor Bl,20
        Mov Ah,2
        Mov Dl, '-'
        int 21H
    INC Bl
    DEC Cl
    JNZ HLine

    ;Printing each other names 
    MoveCursor 0,0
    DisplayString PlayMessege
    DisplayStringRead P2Name  

    MoveCursor 41,0
    DisplayString PlayMessege
    DisplayStringRead P1Name

    ;Priting the Main for both Screens 
        MoveCursor 4,8
        DisplayString Chat
        NewFline 4,2 
        DisplayString Game
        NewFline 4,2
        DisplayString EndProgram
        NewFline 4,2
        
        MoveCursor 45,8
        DisplayString Chat
        NewFline 45,2 
        DisplayString Game
        NewFline 45,2
        DisplayString EndProgram
        NewFline 45,2 
        


    Mov ChoiceP1,0
    Mov ChoiceP2,0

    Player1:
    Mov Ah,0h
    int 16H
    Mov ChoiceP1,AH


    ;Choices are Stored in (ChoiceP1)
    ;1) If choose ESC with Scan Code 01
    Mov Al,01
    CMP ChoiceP1,Al
    JNZ NoExit
    JMP Ex
    NoExit:

    ;2) If choose F1 with Scan Code 3D  
    Mov Al,3DH
    CMP ChoiceP1,Al
    JZ P1StartChat


    ;3) If choose F2 with Scan Code 3C
    Mov Al,3CH
    CMP ChoiceP1,Al
    JZ P1StartGame  
        

    P1StartChat:
    ;Check if P2 Invited Go to Chat
    Mov Al,ChoiceP1
    CMP Al,ChoiceP2
    JZ Chatting 

    Mov Flag,0
    ;Messege in player1 Notification that invitation is sent
    MoveCursor 0,21
    DisplayString ChatInvitationSent  
    ;Messege in PLAYER2 Notification that invitation is recieved (No player writes recieve to himself)
    MoveCursor 41,21
    DisplayString ChatInvitationRecieved
    JMP Player2

    P1StartGame:
    ;Check if P2 Invited Go to Game
    Mov Al,ChoiceP1
    CMP Al,ChoiceP2
    JZ PlayGame 

    Mov Flag,0
    ;Messege in player1 Notification that invitation is sent
    MoveCursor 0,21
    DisplayString GameInvitationSent  
    ;Messege in PLAYER2 Notification that invitation is recieved (No player writes recieve to himself)
    MoveCursor 41,21
    DisplayString GameInvitationRecieved
    JMP Player2  

    PlayGame: Call GameMode
    Chatting: Call ChatMode
    Ex: Call BYE

    Player2:

    Mov Ah,0h
    int 16H
    Mov ChoiceP2,AH

    ;SCROLL THE SCREEN
    PushALL
        mov ax,0701h 
        mov bh,07 
        mov cx,1500H 
        mov dx,1627H
        int 10H

        mov ax,0701h 
        mov bh,07 
        mov cx,1529H 
        mov dx,1650H
        int 10H
    PopAll

    ;Choices are Stored in (ChoiceP2)
    ;1) If choose ESC with Scan Code 01
    Mov Al,01
    CMP ChoiceP2,Al
    JZ Ex

    ;2) If choose F1 with Scan Code 3D  
    Mov Al,3DH
    CMP ChoiceP2,Al
    JZ P2StartChat


    ;3) If any user choose F2 with Scan Code 3C
    Mov Al,3CH
    CMP ChoiceP2,Al
    JZ P2StartGame  
    

    P2StartGame:
    ;Check if P2 Invited Go to Game
    Mov Al,ChoiceP1
    CMP Al,ChoiceP2
    JZ PlayGame 

    Mov Flag,1
    ;Messege in player1 Notification that invitation is sent
    MoveCursor 0,21
    DisplayString GameInvitationRecieved  
    ;Messege in PLAYER2 Notification that invitation is recieved (No player writes recieve to himself)
    MoveCursor 41,21
    DisplayString GameInvitationSent
    JMP Player1   

    P2StartChat:
    ;Check if P2 Invited Go to Chat
    Mov Al,ChoiceP1
    CMP Al,ChoiceP2
    JNZ NoChatting
    JMP Chatting 
    NoChatting:
    Mov Flag,1
    ;Messege in player1 Notification that invitation is sent
    MoveCursor 0,21
    DisplayString ChatInvitationRecieved  
    ;Messege in PLAYER2 Notification that invitation is recieved (No player writes recieve to himself)
    MoveCursor 41,21
    DisplayString ChatInvitationSent
    JMP Player1 
    RET
SecondS ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         Shooting Game           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ShootingS Proc
;Designing the mini game screen
;Change to video mode
    mov AH,00h     ;set the configuration to video mode
    mov AL,13h    ;choose to video mode
    mov bh,0
    int 10h       ;execute the configuration
    mov AH,0Bh    ;set the configuration
    mov Bx,0000h  ;set the configuration to background colour
    int 10h       ;execute the configuration
    
    DrawSquarePixel 125,135,165,175,9

    mov x1, 0
    mov x2, 5
    mov cx,1000
    DrawSquarePixel x1,x2,0,5,9

    mov y1,168
    mov y2,172
    mov cx,1000
    DrawSquarePixel 128,132,y1,y2,0FH

    
    LOOPFLying2:
    DrawSquarePixel x1,x2,0,5,0
    add x1, 10
    add x2, 10
    DrawSquarePixel x1,x2,0,5,9
    dec cx
    hlt
    mov ah,1
    int 16H
    cmp ah,57
    jnz LoopFlying2 
    ;cmp x2, 315
    jnz LOOPFLying2

    tany:
    mov ah,1
    int 16H
    cmp ah,57
    jnz tany
    LOOPFLying1:
    cmp x1,300
    cmp x1,301
    cmp x1,302
    cmp x1,303
    cmp x1,304
    cmp x1,305
    cmp x1,300
    cmp x1,306
    cmp x1,307
    cmp x1,308
    cmp x1,309
    cmp x1,310
    jz tel3oEqual
    jmp ayhaga
    tel3oEqual:
    mov yay,1
    ayhaga:
    DrawSquarePixel 128,132,y1,y2,0
    sub y1, 10
    sub y2, 10
    DrawSquarePixel 128,132,y1,y2,0FH
    dec cx
    hlt
    DrawSquarePixel x1,x2,0,5,0
    add x1, 10
    add x2, 10
    DrawSquarePixel x1,x2,0,5,9
    dec cx
    hlt
    cmp y2, 164
    ja barra
    jmp far ptr tany
    barra:
    cmp yay,1
    jz win
    jmp ayhagatanya
    win:
    MoveCursor 135,165
    ;DisplayString PWinner
    Mov Shooted,1
    NewFLine 136,170
    ayhagatanya:
    RET
ShootingS ENDP 
  

;..................................
;              EXIT               ;
;..................................  

EXIT PROC
    ReadString Spam
    clear
    MoveCursor 10,10
    DisplayString FINISHED
    ReadString Spam
    Call SecondS
EXIT ENDP

;..................................
;           Game Screen           ;
;.................................. 
 
;Calling different screens like the Main Procedure
GameMode Proc
    Clear
    Call GInitialPoints
    Clear
    Call GLevel
    Clear
    Call GFCharacter
    Clear
    ;For Clearing All registers
        PushALL
        Mov Bl,1
        Call ClearALLRegisters
        PopAll
    Call GMainScreen
GameMode ENDP

;1]] Choose Initial Points
GInitialPoints Proc
    ;Designing the screen 
    ;Drawing a vertical line at the middle if the Screen (Main Screens) 
    Mov Cl,24D
    Mov Bl,0
    VpLine: 
    MoveCursor 40,Bl
        Mov Ah,2
        Mov Dl, '|'
        int 21H
    INC Bl
    DEC Cl
    JNZ VpLine

    ;Priting the Main for both Screens 
        MoveCursor 10,8
        DisplayString InitialPoints

        MoveCursor 50,8
        DisplayString InitialPoints

        MoveCursor 10,8
        NewFLine 10,2
        ReadString P1_Game_Points

        MoveCursor 50,8
        NewFLine 50,2
        ReadString P2_Game_Points


    ;;For Getting the Mininum points => (GamePoints)
        ;Copare the Number of Digits
        Mov Al,P1_Game_Points+1
        Mov Bl,P2_Game_Points+1

        CMP Al,Bl
        JNC TryP2 ;If P1Points is less in digits
        JMP MinPoints1

        TryP2:
        CMP Al,Bl
        JNZ MinPoints2 ;If P2Points is less in digits
        
        ;IF BOTH Digits have equal size
        Mov Cl,P1_Game_Points+1 ;Equal digit size
        Mov SI, offset P1_Game_Points
        Mov DI, offset P2_Game_Points
        MINLoop:
        Mov Al,[SI+2]
        Mov Bl,[DI+2]
        INC SI
        INC DI
        ;If Both numbers are the same
            DEC CL
            CMP CL,0
            JNZ SameD 
            JMP MinPoints1 
        CMP Al,Bl

        SameD:
        JZ MINLoop ;If they are the same number in the digit
        CMP Al,Bl
        JNC Comp ;if digit of the first is smaller
        JMP MinPoints1
        Comp:
        CMP Al,Bl
        JNC MinPoints2


        MinPoints2:
        Push SI
        Push DI
        StringToNumber P2_Game_Points      ;31 30 30 --> 1 0 0              7,3,1,0,0
        STRINGCOPY P2_Game_Points ,P2Source,5
        POP DI
        POP SI
        Call DecNumberIntoBX
        Mov AX,NumberHolder
        MOV P1_Score, AX
        MOV P2_Score, AX
        RET

        MinPoints1:
        Push SI
        Push DI 
        StringToNumber P1_Game_Points
        STRINGCOPY P1_Game_Points ,P2Source,5 
        POP DI
        POP SI
        Call DecNumberIntoBX
        Mov AX,NumberHolder   ; 0010 0110 ; 4 6 ; 7 0 
        MOV P1_Score, AX
        MOV P2_Score, AX
        RET
GInitialPoints ENDP

;;2]] ChooseLevel
GLevel Proc
    ;Change to video mode
    mov ah,0
    mov bh,0 
    mov al,13h
    int 10h

    mov ax, 1003h
    mov bx, 0
    int 10h

    Mov DX,200
    PixelVLine:
    DrawPixel 160,DX,9
    Dec DX
    JNZ PixelVLine

    CMP Flag,1
    JNZ P1_Started
    JMP P2_Started
    P1_Started:
    PushALL
    Mov SI,Offset Loading
    DisplayCharacterSTR 040H,27,10,7
    DrawHLinePixel 180,300,100,35H
    DrawHLinePixel 200,280,110,34H
    DrawHLinePixel 220,260,120,33H
    PopAll

    Mov SI,Offset ChooseLevel
    DisplayCharacterSTR 050H,1,8,17
    MoveCursor 8,10
    ReadCharacter Level
    ;Validation Choose only level 1 or 2
    CMP Level,'1'
    JNZ CheckP2Level
    JMP ENDLevel
    CheckP2Level:
    CMP Level,'2'
    JNZ WrongLevel
    JMP ENDLevel
    WrongLevel: Call GLevel
    RET

    P2_Started:
    PushALL
    DrawHLinePixel 20,140,100,35H
    DrawHLinePixel 40,120,110,34H
    DrawHLinePixel 60,100,120,33H
    Mov SI,Offset Loading
    DisplayCharacterSTR 040H,7,10,7
    PopAll
    Mov SI,Offset ChooseLevel
    DisplayCharacterSTR 050H,21,8,17
    MoveCursor 28,10
    ReadCharacter Level
    ;Validation Choose only level 1 or 2
    CMP Level,'1'
    JNZ CheckP2Level1
    JMP ENDLevel
    CheckP2Level1:
    CMP Level,'2'
    JNZ WrongLevel1
    JMP ENDLevel
    WrongLevel1: Call GLevel
    
    ENDLevel:RET
GLevel ENDP

;;3]] ChooseCharacter
GFCharacter  PROC
    ;Change to video mode
    mov ah,0
    mov bh,0 
    mov al,13h
    int 10h

    mov ax, 1003h
    mov bx, 0
    int 10h

    Mov DX,200
    PixelVLine1:
    DrawPixel 160,DX,9
    Dec DX
    JNZ PixelVLine1


    CMP Flag,1
    JZ P2Control
    MoveCursor 21,1 ;Player One in Control
    DisplayString ChosenLevel
    MoveCursor 28,1
    JMP PrintLevel

    P2Control: ;Player two in control
    MoveCursor 1,1
    DisplayString ChosenLevel
    MoveCursor 8,1

    PrintLevel: ;Print the Level after assigned position
    DisplayCharacter Level, 20H

    ;Choosing forbidden character
    MoveCursor 2,5
    DisplayString ChooseCharacter1

    MoveCursor 5,6
    DisplayString ChooseCharacter2

    MoveCursor 22,5
    DisplayString ChooseCharacter1

    MoveCursor 25,6
    DisplayString ChooseCharacter2

    ;Input Forbidden character
    MoveCursor 9,10
    ReadCharacter P1char

    MoveCursor 30,10
    ReadCharacter P2char
    RET
GFCharacter  ENDP

;4]] Game Screen 
GMainScreen Proc
    Mov ToggleFlag,0

    ;Check if Target Value is found
    Call CheckTarget
    CMP Winner,1
    JB NoWinner
    Call ShowScore

    NoWinner:
        ;Change to video mode
        mov ah,0
        mov bh,0 
        mov al,13h
        int 10h

        mov ax, 1003h
        mov bx, 0
        int 10h

    ;For Screen Splitting
    DrawVLinePixel 161D,0D,180D,0EH
    DrawVLinePixel 162D,0D,180D,0EH
    DrawVLinePixel 163D,0D,180D,0EH
    DrawHLinePixel 0D,320D,180,0EH  

    ;For Memory Splitting
    DrawVLinePixel 140D,5D,140D,0EH
    DrawVLinePixel 185D,5D,140D,0EH
    DrawHLinePixel 140D,185D,5D,0EH  
    DrawHLinePixel 140D,185D,140D,0EH  

    ;Displaying Memory Values  ;Works by moving SI
    ;For Printing P1
    Mov CL, 16D
    Mov SI, offset P1Memory
    MoveCursor 18,1
    Printing:
    MemoryToString MTrial
    DisplayString MTrial
    NewFLine  18,1 
    INC SI
    DEC CL 
    JNZ Printing


    ;Displaying Memory Values  ;Works by moving SI
    ;For Printing P2
    Mov CL, 16D
    Mov SI, offset P2Memory
    MoveCursor 21,1
    Printing1:
    MemoryToString MTrial
    DisplayString MTrial
    NewFLine  21,1 
    INC SI
    DEC CL 
    JNZ Printing1

    ;PRINTING MEMORY ADDRESSES FOR P1
    MoveCursor 16,1
    DisplayCharacter Address,20H
    NewFLine 16,1
    DisplayCharacter Address+1,20H
    NewFLine 16,1
    DisplayCharacter Address+2,20H
    NewFLine 16,1
    DisplayCharacter Address+3,20H
    NewFLine 16,1
    DisplayCharacter Address+4,20H
    NewFLine 16,1
    DisplayCharacter Address+5,20H
    NewFLine 16,1
    DisplayCharacter Address+6,20H
    NewFLine 16,1
    DisplayCharacter Address+7,20H
    NewFLine 16,1
    DisplayCharacter Address+8,20H
    NewFLine 16,1
    DisplayCharacter Address+9,20H
    NewFLine 16,1
    DisplayCharacter Address+10,20H
    NewFLine 16,1
    DisplayCharacter Address+11,20H
    NewFLine 16,1
    DisplayCharacter Address+12,20H
    NewFLine 16,1
    DisplayCharacter Address+13,20H
    NewFLine 16,1
    DisplayCharacter Address+14,20H
    NewFLine 16,1
    DisplayCharacter Address+15,20H
    NewFLine 16,1

    ;PRINTING MEMORY ADDRESSES FOR P1
    MoveCursor 24,1
    DisplayCharacter Address,20H
    NewFLine 24,1
    DisplayCharacter Address+1,20H
    NewFLine 24,1
    DisplayCharacter Address+2,20H
    NewFLine 24,1
    DisplayCharacter Address+3,20H
    NewFLine 24,1
    DisplayCharacter Address+4,20H
    NewFLine 24,1
    DisplayCharacter Address+5,20H
    NewFLine 24,1
    DisplayCharacter Address+6,20H
    NewFLine 24,1
    DisplayCharacter Address+7,20H
    NewFLine 24,1
    DisplayCharacter Address+8,20H
    NewFLine 24,1
    DisplayCharacter Address+9,20H
    NewFLine 24,1
    DisplayCharacter Address+10,20H
    NewFLine 24,1
    DisplayCharacter Address+11,20H
    NewFLine 24,1
    DisplayCharacter Address+12,20H
    NewFLine 24,1
    DisplayCharacter Address+13,20H
    NewFLine 24,1
    DisplayCharacter Address+14,20H
    NewFLine 24,1
    DisplayCharacter Address+15,20H
    NewFLine 24,1


    ;For Displaying the User Names
    DrawHLinePixel 0,320,160,0EH
    MoveCursor 1,21
    DisplayStringRead P1Name
    MoveCursor 21,21
    DisplayStringRead P2Name

    ;For Displaying the Score
    MoveCursor 8,21
    DisplayString Scores
    MoveCursor 29,21
    DisplayString Scores


    CMP Level,'2'
    JZ DoNotShowF
    ;For Displaying the Forbidden Character
    MoveCursor 1,0
    DisplayCharacter P2Char,33H
    MoveCursor 38,0
    DisplayCharacter P1Char,33H

    DoNotShowF:
    HexaToDecimal P1_Score, P1_ScoreP 
    MoveCursor 14,21 
    DisplayString P1_ScoreP

    HexaToDecimal P2_Score, P2_ScoreP 
    MoveCursor 35,21 
    DisplayString P2_ScoreP


    ;Displaying Register !!!!Names FOR PLAYER 1!!!!!
    Mov SI,Offset R1
    DisplayCharacterSTR 040,4,1,2
    DisplayCharacterSTR 040,4,5,2
    DisplayCharacterSTR 040,4,9,2
    DisplayCharacterSTR 040,4,13,2
    DisplayCharacterSTR 040,10,1,2
    DisplayCharacterSTR 040,10,5,2
    DisplayCharacterSTR 040,10,9,2
    DisplayCharacterSTR 040,10,13,2

    ;FOR Initializing Registers Player1
    CMP Level,'2'
    JZ Level2Initialize1
    JMP NoInitialization1

        Level2Initialize1:
        CMP Initially,0
        JZ Init1
        JMP NoInitialization1
        Init1:
        ;For the first player in Level (2)
        Mov Cl,8
        Mov DI,0
        Mov Initially,1
        InitializeP1:
        PushALL
        MoveCursor 4,18
        ReadString P2Source
        StringToNumber P2Source
        Call NumberIntoBX
        Mov Bx,NumberHolder ;BX Contains the Number
        CMP BX,Target       ;Check if target is present (Mov Zero's)
        JNZ TargetNotFound
        Mov BX,0
        TargetNotFound:
        Mov SI,offset P1Registers
        Add SI,DI
        Mov [SI],BX
        Add DI,2
        Push SI
        Push DI
        DrawSquarePixel 20,100,130,155,0FFH
        Pop DI
        Pop SI
        PopALL
        DEC CL
        JZ NoInitialization1
        JMP InitializeP1
        
    NoInitialization1:
    MoveCursor 3,2
    NumberToString P1Registers,Trial    ;Printing AX
    DisplayString Trial
    NewFLine  3,4 
    NumberToString P1Registers+2,Trial  ;Printing BX
    DisplayString Trial 
    NewFLine  3,4 
    NumberToString P1Registers+4,Trial  ;Printing CX
    DisplayString Trial
    NewFLine  3,4 
    NumberToString P1Registers+6,Trial  ;Printing DX
    DisplayString Trial

    MoveCursor 9,2
    NumberToString P1Registers+8,Trial  ;Printing SI
    DisplayString Trial
    NewFLine  9,4 
    NumberToString P1Registers+10,Trial ;Printing DI
    DisplayString Trial
    NewFLine  9,4 
    NumberToString P1Registers+12,Trial ;Printing SP
    DisplayString Trial
    NewFLine  9,4 
    NumberToString P1Registers+14,Trial ;Printing BP
    DisplayString Trial




    ;Displaying Register !!!!Names FOR PLAYER 2!!!!!
    Mov SI,Offset R1
    DisplayCharacterSTR 040,28,1,2
    DisplayCharacterSTR 040,28,5,2
    DisplayCharacterSTR 040,28,9,2
    DisplayCharacterSTR 040,28,13,2
    DisplayCharacterSTR 040,34,1,2
    DisplayCharacterSTR 040,34,5,2
    DisplayCharacterSTR 040,34,9,2
    DisplayCharacterSTR 040,34,13,2

    ;FOR Initializing Registers Player1
        CMP Level,'2'
        JZ Level2Initialize2
        JMP NoInitialization2

        Level2Initialize2:
        CMP Initially,1
        JZ Init2
        JMP NoInitialization2
        Init2:
        ;For the first player in Level (2)
        Mov Cl,8
        Mov DI,0
        Mov Initially,2
        InitializeP2:
        PushALL
        MoveCursor 27,18
        ReadString P2Source
        StringToNumber P2Source
        Call NumberIntoBX
        Mov Bx,NumberHolder ;BX Contains the Number
        CMP BX,Target       ;Check if target is present (Mov Zero's)
        JNZ TargetNotFound0
        Mov BX,0
        TargetNotFound0:
        Mov SI,offset P2Registers
        Add SI,DI
        Mov [SI],BX
        Add DI,2
        Push SI
        Push DI
        DrawSquarePixel 200,260,130,155,0FFH
        Pop DI
        Pop SI
        PopALL
        DEC CL
        JZ NoInitialization2
        JMP InitializeP2
        
    NoInitialization2:
    MoveCursor 27,2
    NumberToString P2Registers,Trial    ;Printing AX
    DisplayString Trial
    NewFLine  27,4 
    NumberToString P2Registers+2,Trial  ;Printing BX
    DisplayString Trial 
    NewFLine  27,4 
    NumberToString P2Registers+4,Trial  ;Printing CX
    DisplayString Trial
    NewFLine  27,4 
    NumberToString P2Registers+6,Trial  ;Printing DX
    DisplayString Trial

    MoveCursor 33,2
    NumberToString P2Registers+8,Trial  ;Printing SI
    DisplayString Trial
    NewFLine  33,4 
    NumberToString P2Registers+10,Trial ;Printing DI
    DisplayString Trial
    NewFLine  33,4 
    NumberToString P2Registers+12,Trial ;Printing SP
    DisplayString Trial
    NewFLine  33,4 
    NumberToString P2Registers+14,Trial ;Printing BP
    DisplayString Trial

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
    CMP Flag,1
    JZ P2Action
    JMP Player1Game
    P2Action: JMP FAR PTR Player2Game

    Player1Game:
    DrawHLinePixel 20,110,140,050H
    DrawHLinePixel 20,110,155,050H
    DrawVLinePixel 20,140,155,050H
    DrawVLinePixel 110,140,155,050H

    DrawHLinePixel 1,19,140,060H
    DrawHLinePixel 1,19,155,060H
    DrawVLinePixel 1,140,155,060H
    DrawVLinePixel 19,140,155,060H
    WrongInput1: MoveCursor 1,18
    ReadCharacter PorC     ;P=>1)Clear 2)Change_Forbidden_Character 3)Command    C=>Command
    ;IF P (Read the Power up character)
    CMP PorC,'E'  ;Scancode of F4
    JNZ NoMainMenu
    JMP Far ptr SecondS  ;Call the main menue
    NoMainMenu:
    CMP PorC,'P'
    JZ Power1
    CMP PorC, 'p'
    JZ Power1
    CMP PorC, 'C'
    JNZ WrongInput1
    JMP C_1

    Power1:
    MoveCursor 1,18
    ReadCharacter PowerUp   ;If 1)Clear   2)Change_Forbidden_Character  3)Which_register_to_play_on
    CMP PowerUp,'1' ;Clear 30 points
    JNZ ChangeForbidden1
    CMP P1Stock,1 ;Check if power up is used before
    JZ P1Clear
    JMP C_1
        P1Clear:
        Mov BX,P1_Score
        CMP BX,1EH
        JAE P1SufficientC     ;Check if points are suifficient
        JMP C_1
        P1SufficientC:
            SUB BX,1EH
            MOV P1_Score,BX
            Mov P1Stock,0   ;To update Stock
            Mov Bx,0        ;To perform the clear
            MOV BL,2
            Call ClearALLRegisters
            Call ShootingS
            CMP Shooted,1
            JZ Increase1
            JMP C_1
            Increase1: Add P1_Score,5
            JMP C_1

    ChangeForbidden1:
    CMP PowerUp,'2' ;Forbidden  8 points
    JNZ ChangeOwnReg1
    CMP P1Stock+1,1 ;Check if power up is used before
    JZ P1Forbidden
    JMP C_1
        P1Forbidden:
        Mov BX,P1_Score
        CMP BX,8H
        JAE P1SufficientF     ;Check if points are suifficient
        JMP C_1
        P1SufficientF:
            SUB BX,8H
            MOV P1_Score,BX
            Mov P1Stock+1,0
            MoveCursor 1,18
            ReadCharacter P1Char
            JMP C_1

    ChangeOwnReg1:    ;  5 points     
    CMP PowerUp,'3'
    JNZ ChangeOnBoth1
    Mov BX,P1_Score
        CMP BX,5H
        JAE P1ChangeOwn     ;Check if points are suifficient
        JMP C_1
        P1ChangeOwn:
            SUB BX,5H
            MOV P1_Score,BX
            Mov ToggleFlag,1   ;To Change his own register
            JMP C_1

    ChangeOnBoth1:  ; 3 points
    CMP PowerUp,'4'
    JNZ ChangeTValue1
    Mov BX,P1_Score
        CMP BX,3H
        JAE P1ChangeBoth     ;Check if points are suifficient
        JMP C_1
        P1ChangeBoth:
            SUB BX,3H
            MOV P1_Score,BX
            Mov ToggleFlag,3

    ChangeTValue1:  
    CMP Level, '2'
    JZ COMP1 
    JMP C_1
    COMP1:
    CMP PowerUp,'5'
    JZ COMP2
    JMP C_1
    COMP2:
    CMP P1Stock+2,1 ;Check if power up is used before
        JZ COMP3
        JMP C_1
        COMP3:
            MOV P1Stock+2,0 
            PushALL
            MoveCursor 3,18
            ReadString P2Source
            Push SI
            Push DI
            DrawSquarePixel 20,100,142,155,0FFH
            POP DI
            POP SI
            PopALL
            StringToNumber P2Source
            Call NumberIntoBX
            MOV BX, NumberHolder
            MOV Ax, Target
            MOV Target, BX
            Call CheckTarget
            CMP Winner, 0
            JNZ NoChange1
            Mov Flag,1
            Call GMainScreen

    NoChange1:
        MOV Target, AX
        MOV Winner, 0
        Mov Flag,1
        Call GMainScreen

    C_1:
    MoveCursor 3,18
    ReadString P1Command
    MoveCursor 7,18
    ReadString P1Destination
    MoveCursor 10,18
    ReadString P1Source
    Mov Flag,1
    CMP Level,2
    JNZ Here
    ReadCharacter ChooseRegister
    CMP ChooseRegister,'1'
    JNZ Here
    Mov ToggleFlag,1
    Here: JMP GameProcedure



    Player2Game:
    DrawHLinePixel 210,300,140,050H
    DrawHLinePixel 210,300,155,050H
    DrawVLinePixel 210,140,155,050H
    DrawVLinePixel 300,140,155,050H

    DrawHLinePixel 191,209,140,060H
    DrawHLinePixel 191,209,155,060H
    DrawVLinePixel 191,140,155,060H
    DrawVLinePixel 209,140,155,060H
    WrongInput2: MoveCursor 25,18
    ReadCharacter PorC
    ;IF P (Read the Power up character)
    CMP PorC,'E'  ;Scancode of F4
    JNZ NoMainMenu1
    JMP Far ptr SecondS  ;Call the main menue
    NoMainMenu1:
    CMP PorC,'P'
    JZ Power2
    CMP PorC,'p'
    Jz Power2
    CMP PorC,'C'
    JNZ WrongInput2
    JMP C_2

    Power2:
    MoveCursor 25,18
    ReadCharacter PowerUp   ;If 1)Clear   2)Change_Forbidden_Character  3)Which_register_to_play_on
    CMP PowerUp,'1' ;Clear 30 points
    JNZ ChangeForbidden2
    CMP P2Stock,1 ;Check if power up is used before
    JZ P2Clear
    JMP C_2
        P2Clear:
        Mov BX,P2_Score
        CMP BX,1EH
        JAE P2SufficientC     ;Check if points are suifficient
        JMP C_2
        P2SufficientC:
            SUB BX,1EH
            MOV P2_Score,BX
            Mov P2Stock,0   ;To update Stock
            Mov Bx,0        ;To perform the clear
            MOV BL,3
            Call ClearALLRegisters
            Call ShootingS
            CMP Shooted,1
            JZ Increase2
            JMP C_2
            Increase2: Add P2_Score,5
            JMP C_2

    ChangeForbidden2:
    CMP PowerUp,'2' ;Forbidden  8 points
    JNZ ChangeOwnReg2
    CMP P2Stock+1,1 ;Check if power up is used before
    JZ P2Forbidden
    JMP C_2
        P2Forbidden:
        Mov BX,P2_Score
        CMP BX,8H
        JAE P2SufficientF     ;Check if points are suifficient
        JMP C_2
        P2SufficientF:
            SUB BX,8H
            MOV P2_Score,BX
            Mov P2Stock+1,0
            MoveCursor 1,18
            ReadCharacter P1Char
            JMP C_2

    ChangeOwnReg2:
    CMP PowerUp,'3'
    JNZ ChangeOnBoth2
    Mov BX,P2_Score
        CMP BX,5H
        JAE P2ChangeOwn     ;Check if points are suifficient
        JMP C_2
        P2ChangeOwn:
            SUB BX,5H
            MOV P2_Score,BX
            Mov ToggleFlag,1   ;To Change his own register
            JMP C_2

    ChangeOnBoth2:
    CMP PowerUp,'4'
    JNZ ChangeTValue2
    Mov BX,P2_Score
        CMP BX,3H
        JAE P2ChangeBoth     ;Check if points are suifficient
        JMP C_2
        P2ChangeBoth:
            SUB BX,3H
            MOV P2_Score,BX
            Mov ToggleFlag,3

    ChangeTValue2:  
    CMP Level, '2'
    JZ COMP11 
    JMP C_2
    COMP11:
    CMP PowerUp,'5'
    JZ COMP22
    JMP C_2
    COMP22:
        CMP P2Stock+2,1 ;Check if power up is used before
        JZ COMP33
        JMP C_2
        COMP33:
            MOV P2Stock+2,0 
            PushALL
            MoveCursor 27,18
            ReadString P1Source
            Push SI
            Push DI
            DrawSquarePixel 210,250,142,155,0FFH
            POP DI
            POP SI
            PopALL
            StringToNumber P1Source
            Call NumberIntoBX
            MOV BX, NumberHolder
            MOV Ax, Target
            MOV Target, BX
            Call CheckTarget
            CMP Winner, 0
            JNZ NoChange2
            Mov Flag,0
            Call GMainScreen

    NoChange2:
        MOV Target, AX
        MOV Winner, 0
        Mov Flag,0
        Call GMainScreen

    C_2:
    MoveCursor 27,18
    ReadString P2Command
    MoveCursor 31,18
    ReadString P2Destination
    MoveCursor 34,18
    ReadString P2Source
    Mov Flag,0
    CMP Level,2
    JNZ Here0
    ReadCharacter ChooseRegister
    CMP ChooseRegister,'1'
    JNZ Here0
    Mov ToggleFlag,2
    Here0: JMP GameProcedure

        GameProcedure:
        Call MakeALLCapital
        ;IFF Forrbiden Character Present
        CMP Flag,1   ;Flag=1 Player 1 Lessa me5alas
        JZ FP1
        JMP FP2

        FP1:
            FCharacterCheck1 P1Command,3,CharF
            CMP CharF,1
            JZ Back1
            FCharacterCheck1 P1Source,2,CharF
            CMP CharF,1
            Jz Back1
            FCharacterCheck1 P1Destination,2,CharF
            CMP CharF,1
            JZ Back1
            JMP NOFChar 
            Back1: Call GMainScreen
        FP2:
            FCharacterCheck2 P2Command,3,CharF
            CMP CharF,1
            JZ Back2
            FCharacterCheck2 P2Source,2,CharF
            CMP CharF,1
            Jz Back2
            JMP XX
            Back2: Call GMainScreen
            XX: FCharacterCheck2 P2Destination,2,CharF
            CMP CharF,1
            JZ Back2
            JMP NOFChar
            
        NOFChar:               ;If No Forbidden Char Found
        Call FindRegisterDestination  ;Destination number assigned
        Call FindRegisterSource       ;Source number assigned

        ;;;;;;;; Working Registers ;;;;;; FlagToggle=0    FlagToggle=1 Write on player1 registers   FlagToggle=2 Write on player2 registers
        CMP ToggleFlag,1           ;;;;;; FlagToggle=3 Write on both registers
        JZ ToP2
        CMP ToggleFlag,2
        JZ XP1
        CMP ToggleFlag,3
        JZ XP1

    CMP Flag,1
        JZ XP1
        ToP2:JMP YP2

            XP1: 
            CMP DestinationNumber,16
            JB S1
            INC case

            S1:
            CMP SourceNumber,16  ;in when SN>16
            JB Mismatch1
            CMP SourceNumber,29
            Jz Mismatch1
            CMP SourceNumber,30
            Jz Mismatch1
            CMP SourceNumber,31
            Jz Mismatch1
            INC case

            Mismatch1:
            CMP case,1
            JNZ NoMismatch1
            MOV case,0
            DEC P1_Score
            Call GMainScreen

            NoMismatch1:
            MOV case,0
            CMP DestinationNumber,29
            JNZ  DefaultDest
                StringToNumber P1Destination
                MOV AX,0
                Mov AL,P1Destination+3    ;7, size, [, 1, ]
                MOV SI, offset P2Memory
                ADD SI, AX
                MOV AX, [SI]
                JMP CONTI
                    DefaultDest:
                    Mov SI,Offset P2Registers
                    ADD SI,DestinationNumber
                    MOV AX,[SI]

            CONTI:
            CMP SourceNumber,30   ;IF Value is present
            JNZ NoVal
            JMP ValueX
            
            NoVal:
            CMP SourceNumber,31
            JNZ CAddress
            JMP NoValueX

            CAddress:
            CMP SourceNumber,29
            JNZ Cont
            StringToNumber P1Source
            MOV AX, 0
            Mov AL,P1Source+3    ;7, size, [, 1, ]
            MOV DI, offset P2Memory
            ADD DI, AX
            MOV BX, [DI]
            JMP Fcommand


            Cont:
            Mov DI,Offset P2Registers
            ADD DI,SourceNumber
            MOV BX,[DI]
            
            Fcommand:
            Call FindCommand 
            
            CMP DestinationNumber, 29
            JNZ DefaultDest1
            Mov SI,Offset P2Memory
            MOV CX,0
            Mov CL,P1Destination+3 
            ADD SI, CX
            MOV [SI],AX
            JMP  HERE3

            DefaultDest1:
            Mov SI,Offset P2Registers
            ADD SI,DestinationNumber
            MOV [SI],AX

            HERE3:
            CMP ToggleFlag,3
            JNZ NoPowerUp
            STRINGCOPY P1Command,P2Command,3
            JMP YP2
            NoPowerUp: Call GMainScreen

            ValueX:
            PushALL
            Call NumberIntoBX
            PopALL
            Mov BX,NumberHolder
            Call FindCommand 
            Mov SI,Offset P2Registers
            ADD SI,DestinationNumber
            MOV [SI],AX
            CMP ToggleFlag,3
            JNZ NoPowerUp
            STRINGCOPY P1Command,P2Command,3
            JMP YP2
            Call GMainScreen

            NoValueX: 
            Mov Bx,0
            Call FindCommand 
            Mov SI,Offset P2Registers
            ADD SI,DestinationNumber
            MOV [SI],AX
            CMP ToggleFlag,3
            JNZ NoPowerUp
            STRINGCOPY P1Command,P2Command,3
            JMP YP2
            Call GMainScreen

        
            
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

            YP2:
            CMP DestinationNumber,16
            JB S2
            INC case

            S2:
            CMP SourceNumber,16
            JB Mismatch2
            CMP SourceNumber,29
            Jz Mismatch2
            CMP SourceNumber,30
            Jz Mismatch2
            CMP SourceNumber,31
            Jz Mismatch2
            INC case

            Mismatch2:
            CMP case,1
            JNZ NoMismatch2
            MOV case,0
            DEC P2_Score
            Call GMainScreen

            NoMismatch2:
            Mov SI,Offset P1Registers
            ADD SI,DestinationNumber
            MOV AX,[SI]

            CMP SourceNumber,30
            JNZ NoVal2
            JMP ValueY

            NoVal2:
            CMP SourceNumber,31
            JNZ CAddress2
            JMP NoValueY

            CAddress2:
            CMP SourceNumber,29
            JNZ Cont2
            StringToNumber P2Source
            MOV AX, 0
            Mov AL,P2Source+3    ;7, size, [, 1, ]
            MOV DI, offset P1Memory
            ADD DI, AX
            MOV BX, [DI]
            JMP Fcommand2

            Cont2:
            Mov DI,Offset P1Registers
            ADD DI,SourceNumber
            MOV BX,[DI]
            
            Fcommand2:
            Call FindCommand 

            Mov SI,Offset P1Registers
            ADD SI,DestinationNumber
            MOV [SI],AX
            Call GMainScreen

            ValueY:
            PushALL
            Call NumberIntoBX
            PopALL
            Mov BX,NumberHolder
            Call FindCommand 
            Mov SI,Offset P1Registers
            ADD SI,DestinationNumber
            MOV [SI],AX
            Call GMainScreen

            NoValueY:
            Mov Bx,0
            Call FindCommand 
            Mov SI,Offset P1Registers
            ADD SI,DestinationNumber
            MOV [SI],AX

            ENDHERE:
            Call GMainScreen
GMainScreen ENDP 

;5]] Show results
ShowScore Proc
    ;Change to video mode
    mov ah,0
    mov bh,0 
    mov al,13h
    int 10h

    mov ax, 1003h
    mov bx, 0
    int 10h

    ;For Screen Splitting
    DrawVLinePixel 160D,0D,200D,0EH
    ;Names
    MoveCursor 4,6
    DisplayStringRead P1Name
    MoveCursor 24,6
    DisplayStringRead P2Name
    
    ;Printing the screen
    CMP Winner,1
    JZ P2_Winner
    JMP P1_Winner
    
    P2_Winner:
    PushALL
    DrawDiagonalRLinePixel 240,280,60,33H
    DrawDiagonalPLinePixel 200,240,100,33H
    DrawDiagonalRLinePixel 240,280,70,34H
    DrawDiagonalPLinePixel 200,240,110,34H
    DrawDiagonalRLinePixel 240,280,80,35H
    DrawDiagonalPLinePixel 200,240,120,35H
    DrawDiagonalRLinePixel 240,270,90,35H
    DrawDiagonalPLinePixel 210,240,120,35H
    Mov SI,Offset PWinner
    DisplayCharacterSTR 040H,27,13,6

    MoveCursor 24,18
    DisplayString Scores
    MoveCursor 24,20
    HexaToDecimal P2_Score,P2_ScoreP
    DisplayString P2_ScoreP
    PopAll

    MoveCursor 5,10
    DisplayString Scores
    MoveCursor 5,12
    HexaToDecimal P1_Score,P1_ScoreP
    DisplayString P1_ScoreP
   
    ReadCharacter Spam
    Call Exit

    P1_Winner:
    PushALL
    DrawDiagonalRLinePixel 80,120,60,33H
    DrawDiagonalPLinePixel 40,80,100,33H
    DrawDiagonalRLinePixel 80,120,70,34H
    DrawDiagonalPLinePixel 40,80,110,34H
    DrawDiagonalRLinePixel 80,120,80,35H
    DrawDiagonalPLinePixel 40,80,120,35H
    DrawDiagonalRLinePixel 80,110,90,35H
    DrawDiagonalPLinePixel 50,80,120,35H
    Mov SI,Offset PWinner
    DisplayCharacterSTR 040H,7,13,6

    MoveCursor 5,18
    DisplayString Scores
    MoveCursor 5,20
    HexaToDecimal P1_Score,P1_ScoreP
    DisplayString P1_ScoreP
    PopAll

    MoveCursor 24,10
    DisplayString Scores
    MoveCursor 24,12
    HexaToDecimal P2_Score,P2_ScoreP
    DisplayString P2_ScoreP  
    Call Exit
ShowScore ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           REGISTER             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FindRegisterDestination Proc
    CMP Flag,1
    JZ P1REGLBL
    JMP P2REGLBL

    P1REGLBL:
        Check1_AX:
        STRINGCOMPARE RAX,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_BX
        Mov DestinationNumber,0
        RET

        Check1_BX:
        STRINGCOMPARE RBX,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_CX
        Mov DestinationNumber,2
        RET

        Check1_CX:
        STRINGCOMPARE RCX,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_DX
        Mov DestinationNumber,4
        RET

        Check1_DX:
        STRINGCOMPARE RDX,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_SI
        Mov DestinationNumber,6
        RET

        Check1_SI:
        STRINGCOMPARE RSI,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_DI
        Mov DestinationNumber,8
        RET

        Check1_DI:
        STRINGCOMPARE RDI,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_SP
        Mov DestinationNumber,0AH
        RET

        Check1_SP:
        STRINGCOMPARE RSP,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_BP
        Mov DestinationNumber,0CH
        RET

        Check1_BP:
        STRINGCOMPARE RBP,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_AL
        Mov DestinationNumber,0EH
        RET
        

        ;Mai
        
        Check1_AL:
        STRINGCOMPARE RAL,P1Destination,2,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_AH
        Mov DestinationNumber,011H
        RET
        

        
        Check1_AH:
        STRINGCOMPARE RAH,P1Destination,2,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_BL
        Mov DestinationNumber,010H
        RET


        
        Check1_BL:
        STRINGCOMPARE RBL,P1Destination,2,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_BH
        Mov DestinationNumber,013H
        RET


        Check1_BH:
        STRINGCOMPARE RBH,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_CL
        Mov DestinationNumber,012H
        RET


        Check1_CL:
        STRINGCOMPARE RCLL,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_CH
        Mov DestinationNumber,015H
        RET


        Check1_CH:
        STRINGCOMPARE RCH,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_DL
        Mov DestinationNumber,014H
        RET

        
        Check1_DL:
        STRINGCOMPARE RDL,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_DH
        Mov DestinationNumber,017H
        RET
        
        
        Check1_DH:
        STRINGCOMPARE RDH,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_Bracket
        Mov DestinationNumber,016H
        RET

        Check1_Bracket:
        STRINGCOMPARE SBracket,P1Destination,2,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_TYPO
        MOV DestinationNumber,29
        RET
        
        
        Check1_TYPO:  
        Mov BX,P1_Score
        CMP BX,1
        JAE P11Sufficient     ;Check if points are suitable
        RET
        P11Sufficient:
        DEC P1_Score
        RET
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    P2REGLBL:
        Check2_AX:
        STRINGCOMPARE RAX,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_BX
        Mov DestinationNumber,0
        RET

        Check2_BX:
        STRINGCOMPARE RBX,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_CX
        Mov DestinationNumber,2
        RET

        Check2_CX:
        STRINGCOMPARE RCX,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_DX
        Mov DestinationNumber,4
        RET

        Check2_DX:
        STRINGCOMPARE RDX,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_SI
        Mov DestinationNumber,6
        RET

        Check2_SI:
        STRINGCOMPARE RSI,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_DI
        Mov DestinationNumber,8
        RET

        Check2_DI:
        STRINGCOMPARE RDI,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_SP
        Mov DestinationNumber,0AH
        RET

        Check2_SP:
        STRINGCOMPARE RSP,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_BP
        Mov DestinationNumber,0CH
        RET

        Check2_BP:
        STRINGCOMPARE RBP,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_AL
        Mov DestinationNumber,0EH
        RET

        Check2_AL:
        STRINGCOMPARE RAL,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_AH
        Mov DestinationNumber,011H
        RET
                
        Check2_AH:
        STRINGCOMPARE RAH,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_BL
        Mov DestinationNumber,010H
        RET

        Check2_BL:
        STRINGCOMPARE RBL,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_BH
        Mov DestinationNumber,013H
        RET

        Check2_BH:
        STRINGCOMPARE RBH,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_CL
        Mov DestinationNumber,012H
        RET

        Check2_CL:
        STRINGCOMPARE RCLL,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_CH
        Mov DestinationNumber,015H
        RET

        Check2_CH:
        STRINGCOMPARE RCH,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_DL
        Mov DestinationNumber,014H
        RET
        
        Check2_DL:
        STRINGCOMPARE RDL,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_DH
        Mov DestinationNumber,017H
        RET
                
        Check2_DH:
        STRINGCOMPARE RDH,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_Bracket
        Mov DestinationNumber,016H
        RET

        Check2_Bracket:
        STRINGCOMPARE SBracket,P2Destination,2,CommandNumber
        CMP CommandNumber,1
        JNZ Check22_TYPO
        MOV DestinationNumber,29
        RET

        Check22_TYPO:  
        Mov BX,P2_Score
        CMP BX,1
        JAE P22Sufficient     ;Check if points are suitable
        RET
        P22Sufficient:
        DEC P2_Score
        RET

FindRegisterDestination ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FindRegisterSource Proc
   CMP Flag,1
    JZ P1REGLBL1
    JMP P2REGLBL1

    P1REGLBL1:
        Check3_AX:
        STRINGCOMPARE RAX,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_BX
        Mov SourceNumber,0
        RET

        Check3_BX:
        STRINGCOMPARE RBX,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_CX
        Mov SourceNumber,2
        RET

        Check3_CX:
        STRINGCOMPARE RCX,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_DX
        Mov SourceNumber,4
        RET

        Check3_DX:
        STRINGCOMPARE RDX,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_SI
        Mov SourceNumber,6
        RET

        Check3_SI:
        STRINGCOMPARE RSI,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_DI
        Mov SourceNumber,8
        RET

        Check3_DI:
        STRINGCOMPARE RDI,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_SP
        Mov SourceNumber,0AH
        RET

        Check3_SP:
        STRINGCOMPARE RSP,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_BP
        Mov SourceNumber,0CH
        RET

        Check3_BP:
        STRINGCOMPARE RBP,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_AL
        Mov SourceNumber,0EH
        RET


        Check3_AL:
        STRINGCOMPARE RAL,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_AH
        Mov SourceNumber,11H
        RET

        Check3_AH:
        STRINGCOMPARE RAH,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_BL
        Mov SourceNumber,10H
        RET


        Check3_BL:
        STRINGCOMPARE RBL,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_BH
        Mov SourceNumber,13H
        RET

        Check3_BH:
        STRINGCOMPARE RBH,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_CL
        Mov SourceNumber,12H
        RET

        Check3_CL:
        STRINGCOMPARE RCLL,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_CH
        Mov SourceNumber,15H
        RET

        Check3_CH:
        STRINGCOMPARE RCH,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_DL
        Mov SourceNumber,14H
        RET

        Check3_DL:
        STRINGCOMPARE RDL,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_DH
        Mov SourceNumber,17H
        RET

        Check3_DH:
        STRINGCOMPARE RDH,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_Bracket
        Mov SourceNumber,16H
        RET

        Check3_Bracket:
        STRINGCOMPARE SBracket,P1Source,2,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_Number
        MOV SourceNumber,29
        RET

        Check3_Number:
        Mov SI,Offset P1Source + 1 
        Mov Dl,[SI]
        CMP Dl,0
        JZ NOSource
        MOV SourceNumber,30
        StringToNumber P1Source
        RET

        NOSource:   
        Mov SourceNumber,31
        RET

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    P2REGLBL1:
        Check4_AX:
        STRINGCOMPARE RAX,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_BX
        Mov SourceNumber,0
        RET

        Check4_BX:
        STRINGCOMPARE RBX,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_CX
        Mov SourceNumber,2
        RET

        Check4_CX:
        STRINGCOMPARE RCX,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_DX
        Mov SourceNumber,4
        RET

        Check4_DX:
        STRINGCOMPARE RDX,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_SI
        Mov SourceNumber,6
        RET

        Check4_SI:
        STRINGCOMPARE RSI,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_DI
        Mov SourceNumber,8
        RET

        Check4_DI:
        STRINGCOMPARE RDI,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_SP
        Mov SourceNumber,0AH
        RET

        Check4_SP:
        STRINGCOMPARE RSP,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_BP
        Mov SourceNumber,0CH
        RET

        Check4_BP:
        STRINGCOMPARE RBP,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_AL
        Mov SourceNumber,0EH
        RET

        Check4_AL:
        STRINGCOMPARE RAL,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_AH
        Mov SourceNumber,11H
        RET

        Check4_AH:
        STRINGCOMPARE RAH,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_BL
        Mov SourceNumber,10H
        RET


        Check4_BL:
        STRINGCOMPARE RBL,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_BH
        Mov SourceNumber,13H
        RET

        Check4_BH:
        STRINGCOMPARE RBH,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_CL
        Mov SourceNumber,12H
        RET

        Check4_CL:
        STRINGCOMPARE RCLL,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_CH
        Mov SourceNumber,15H
        RET

        Check4_CH:
        STRINGCOMPARE RCH,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_DL
        Mov SourceNumber,14H
        RET

        Check4_DL:
        STRINGCOMPARE RDL,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_DH
        Mov SourceNumber,17H
        RET

        Check4_DH:
        STRINGCOMPARE RDH,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_Bracket
        Mov SourceNumber,16H
        RET

        Check4_Bracket:
        STRINGCOMPARE SBracket,P2Source,2,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_Number
        MOV SourceNumber,29
        RET

        Check4_Number:
        Mov SI,Offset P2Source + 1 
        Mov Dl,[SI]
        CMP Dl,0
        JZ  NOSource1
        MOV SourceNumber,30
        StringToNumber P2Source
        RET

        NOSource1:   
        Mov SourceNumber,31
        RET

FindRegisterSource ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                ;
;           Command              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FindCommand Proc
    ;;PLAAAAYER (  1  ) COMMAAND;;
    CMP Flag,1
    JZ P1COMLBL
    JMP P2COMLBL

    P1COMLBL:
    Check1_MOV:
    STRINGCOMPARE ComMOV,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_SUB
    Mov AX,BX
    RET

    Check1_SUB:
    STRINGCOMPARE ComSUB,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_Add
    SUB AX,BX
   
    RET

    Check1_ADD:
    STRINGCOMPARE ComADD,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_ADC
    ADD AX,BX
   
    RET

    Check1_ADC:
    STRINGCOMPARE ComADC,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_SBB
    ADC AX,BX
   
    RET

    Check1_SBB:
    STRINGCOMPARE ComSBB,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_DIV
    SBB AX,BX
   
    RET

    Check1_DIV:
    STRINGCOMPARE ComDIV,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_MUL
    DIV BX
   
    RET

    Check1_MUL:
    STRINGCOMPARE ComMUL,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_XOR
    MUL BX
   
    RET

    Check1_XOR:
    STRINGCOMPARE ComXOR,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_AND
    XOR AX,BX
   
    RET

    Check1_AND:
    STRINGCOMPARE ComAND,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_NOP
    AND AX,BX
   
    RET

    Check1_NOP:
    STRINGCOMPARE ComNOP,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_SHR
    NOP
   
    RET

    Check1_SHR:
    STRINGCOMPARE ComSHR,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_SHL
    Mov CH,0
    Mov Cl,Bl
    SHR AX,Cl
   
    RET

    Check1_SHL:
    STRINGCOMPARE ComSHL,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_SAR
    Mov CH,0
    Mov Cl,Bl
    SHL AX,CL
   
    RET

    Check1_SAR:
    STRINGCOMPARE ComSAR,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_CLC
    Mov CH,0
    Mov Cl,Bl
    SAR AX,Cl
   
    RET

    Check1_CLC:
    STRINGCOMPARE ComCLC,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_ROR
    CLC
   
    RET

    Check1_ROR:
    STRINGCOMPARE ComROR,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_RCL
    Mov CH,0
    Mov Cl,Bl
    ROR AX,Cl
   
    RET

    Check1_RCL:
    STRINGCOMPARE ComRCL,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_RCR
    Mov CH,0
    Mov Cl,Bl
    RCL AX,Cl
   
    RET

    Check1_RCR:
    STRINGCOMPARE ComRCR,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_ROL
    Mov CH,0
    Mov Cl,Bl
    RCR AX,Cl
   
    RET

    Check1_ROL:
    STRINGCOMPARE ComROL,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_INC
    Mov CH,0
    Mov Cl,Bl
    ROL AX,Cl
   
    RET

    Check1_INC:
    STRINGCOMPARE ComINC,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_DEC
    INC AX
   
    RET

    Check1_DEC:
    STRINGCOMPARE ComDEC,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_OR
    DEC AX
   
    RET

    Check1_OR:
    STRINGCOMPARE ComOR,P1Command,2,CommandNumber
    CMP CommandNumber,1
    JNZ Check_TYPO
    OR AX,BX
   
    RET

    Check_TYPO:
    Mov BX,P1_Score
    CMP BX,1
    JAE C11Sufficient     ;Check if points are suitable
    RET
    C11Sufficient:
    DEC P1_Score
    RET
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    P2COMLBL:
    Check2_MOV:
    STRINGCOMPARE ComMOV,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_SUB
    Mov AX,BX
    RET

    Check2_SUB:
    STRINGCOMPARE ComSUB,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_Add
    SUB AX,BX
    RET

    Check2_ADD:
    STRINGCOMPARE ComADD,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_ADC
    ADD AX,BX
    RET

    Check2_ADC:
    STRINGCOMPARE ComADC,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_SBB
    ADC AX,BX
    RET

    Check2_SBB:
    STRINGCOMPARE ComSBB,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_DIV
    SBB AX,BX
    RET

    Check2_DIV:
    STRINGCOMPARE ComDIV,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_MUL
    DIV BX
    RET

    Check2_MUL:
    STRINGCOMPARE ComMUL,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_XOR
    MUL BX
    RET

    Check2_XOR:
    STRINGCOMPARE ComXOR,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_AND
    XOR AX,BX
    RET

    Check2_AND:
    STRINGCOMPARE ComAND,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_NOP
    AND AX,BX
    RET

    Check2_NOP:
    STRINGCOMPARE ComNOP,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_SHR
    NOP
    RET

    Check2_SHR:
    STRINGCOMPARE ComSHR,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_SHL
    Mov CH,0
    Mov Cl,Bl
    SHR AX,Cl
    RET

    Check2_SHL:
    STRINGCOMPARE ComSHL,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_SAR
    Mov CH,0
    Mov Cl,Bl
    SHL AX,Cl
    RET

    Check2_SAR:
    STRINGCOMPARE ComSAR,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_CLC
    Mov CH,0
    Mov Cl,Bl
    SAR AX,Cl
    RET

    Check2_CLC:
    STRINGCOMPARE ComCLC,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_ROR
    CLC
    RET

    Check2_ROR:
    STRINGCOMPARE ComROR,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_RCL
    Mov CH,0
    Mov Cl,Bl
    ROR AX,Cl
    RET

    Check2_RCL:
    STRINGCOMPARE ComRCL,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_RCR
    Mov CH,0
    Mov Cl,Bl
    RCL AX,Cl
    RET

    Check2_RCR:
    STRINGCOMPARE ComRCR,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_ROL
    Mov CH,0
    Mov Cl,Bl
    RCR AX,Cl
    RET

    Check2_ROL:
    STRINGCOMPARE ComROL,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_INC
    Mov CH,0
    Mov Cl,Bl
    ROL AX,Cl
    RET

    Check2_INC:
    STRINGCOMPARE ComINC,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_DEC
    INC AX
    RET

    Check2_DEC:
    STRINGCOMPARE ComDEC,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_OR
    DEC AX
    RET

    Check2_OR:
    STRINGCOMPARE ComOR,P2Command,2,CommandNumber
    CMP CommandNumber,1
    JNZ Check_TYPOS
    OR AX,BX
    RET

    Check_TYPOS:
    Mov BX,P2_Score
    CMP BX,1
    JAE C22Sufficient     ;Check if points are suitable
    RET
    C22Sufficient:
    DEC P2_Score
    RET
FindCommand ENDP

;..................................
;           Chat Screen           ;
;..................................   
ChatMode Proc 
    Clear
    ;Designing the screen 
    ;Drawing a vertical line at the middle if the Screen 
    Mov Cl,25D
    Mov Bl,0
    VrLine: 
    MoveCursor 40,Bl
        Mov Ah,2
        Mov Dl, '|'
        int 21H
        INC Bl
        DEC Cl
        JNZ VrLine
        
    MoveCursor 0,0
    DisplayStringRead P1Name
    MoveCursor 41,0
    DisplayStringRead P2Name

    ;BL will contain the Y-Coordinates of messeges in chat 
    Mov BL,1

    ;Player1 1)Read then 2)Print on screen
    ChatP1:
    ;To Chech that F3 Is Not pressed 
        ;mov ah,1
        ;int 16h
    ;CMP AH,3Dh
    ;JZ ReturntoMain   
    MoveCursor 0,24 ;Place to read from
    ReadString P1Chat
    SHR P1Chat,4
    Mov P1Chat,AL ;Move First Charachter  
    MoveCursor 2,BL
    DisplayStringRead P1Chat
    PushALL
    ;TO Clear String 
    Mov SI,Offset P1Chat+2
    Mov CX,50
    ZLOOP:
    Mov Al,SDollar
    Mov [SI],Al
    INC SI
    DEC CX
    JNZ ZLOOP
        ;Clear the TextBox
        mov ax,0600h 
        mov bh,07 
        mov cx,1800H 
        mov dx,1927H
        int 10h    
    PopALL
    INC BL
    JMP ChatP2

    ReturntoMain: Call SecondS

    ;Player2 1)Read then 2)Print on screen
    ChatP2: 
    ;To Chech that F3 Is Not pressed 
        ;mov ah,1
        ;int 16h
    ;CMP AH,3Dh
    ;JZ ReturntoMain  
    MoveCursor 41,24 ;Place to read from
    ReadString P2Chat
    MoveCursor 43,BL
    DisplayStringRead P2Chat
    PushALL
    ;TO Clear String 
    Mov SI,Offset P2Chat+2
    Mov CX,50
    MLOOP:
    Mov Al,SDollar
    Mov [SI],Al
    INC SI
    DEC CX
    JNZ MLOOP
    ;Clear the TextBox
        mov ax,0600h 
        mov bh,07 
        mov cx,1829h 
        mov dx,1950h
        int 10h
    PopALL
    INC BL

    ;To Scroll screen after chat
    CMP Bl,16H
    JNC ScrollChat
    JMP ChatP1


    ScrollChat:
    PushALL
        mov ax,060Ah 
        mov bh,07 
        mov cx,0100H 
        mov dx,1927H
        int 10H
        
        mov ax,060Ah 
        mov bh,07 
        mov cx,0129h 
        mov dx,1950h
        int 10h

    PopALL

        Sub Bl,10
        JMP ChatP1
ChatMode ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         FUNCTION  COMMAND                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MakeALLCapital Proc
    PushALL
    MakeCapital P1Command,3
    MakeCapital P2Command,3

    MakeCapital P1Source,2
    MakeCapital P2Source,2

    MakeCapital P1Destination,2
    MakeCapital P2Destination,2
    PopALL
    RET
MakeALLCapital ENDP

ClearALLRegisters Proc    ;Works on BL => 1)ClearAll, 2)ClearP1Registers, 3)ClearP2Registers
    CMP BL,1
    JNZ Clear1
    Mov SI,Offset P1Registers
    Mov Ax,0
    Mov CX,16
    JMP LOOPC

    Clear1:
    CMP Bl,2
    JNZ Clear2
    Mov SI,Offset P1Registers
    Mov Ax,0
    Mov CX,8
    JMP LOOPC

    Clear2:
    CMP Bl,3
    JNZ LOOPC
    Mov SI,Offset P2Registers
    Mov Ax,0
    Mov CX,8

    LOOPC: 
    Mov [SI],AX
    ADD SI,2
    DEC CX
    JNZ LOOPC
    RET
ClearALLRegisters ENDP

DecNumberIntoBX Proc
    CMP Flag,1
    JZ NP1D
    JMP NP2D

    NP1D:
    Mov SI,offset P1Source + 1      
    Mov Cl,[SI]
    AD:
    CMP Cl,1
    JNZ BD
    Mov AX,0
    Mov SI,Offset P1Source + 2
    Mov Al,[SI]
    Mov NumberHolder,AX
    RET
    BD:
    CMP Cl,2     ;1 2   ;AX=0012   AX=0102
    JNZ CD
    Mov AX,0
    Mov SI,Offset P1Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    Mov NumberHolder,AX
    RET
    CD:
    CMP Cl,3
    JNZ DD1        ;1  2  3
    Mov AX,0
    Mov SI,Offset P1Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,100
    MUL Ch
    Mov DX,AX
    Mov AX,0
    Mov SI,Offset P1Source + 3
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    ADD DX,AX
    Mov NumberHolder,DX
    RET
    DD1:
    CMP Cl,4   ;1 2 3 4
    JNZ LOOPENDD
    Mov AX,0
    Mov SI,Offset P1Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    Mov Dh,AL
    Mov Dl,0  ;DX=1200

    Mov AX,0
    Mov SI,Offset P1Source + 4
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    Mov Dl,Al
    Mov NumberHolder,DX

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LOOPENDD:RET

    NP2D:
    Mov SI,offset P2Source + 1      
    Mov Cl,[SI]
    A1D:
    CMP Cl,1
    JNZ B1D
    Mov AX,0
    Mov SI,Offset P2Source + 2
    Mov Al,[SI]
    Mov NumberHolder,AX
    RET
    B1D:
    CMP Cl,2     ;1 2   ;AX=0012   AX=0102
    JNZ C1D
    Mov AX,0
    Mov SI,Offset P2Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    Mov NumberHolder,AX
    RET
    C1D:
    CMP Cl,3
    JNZ D1D        ;1  2  3
    Mov AX,0
    Mov SI,Offset P2Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,100
    MUL Ch
    Mov DX,AX
    Mov AX,0
    Mov SI,Offset P2Source + 3
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    ADD DX,AX
    Mov NumberHolder,DX
    RET
    D1D:
    CMP Cl,4   ;1 2 3 4
    JNZ LOOPENDD
    Mov AX,0
    Mov SI,Offset P2Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    Mov Dh,AL
    Mov Dl,0  ;DX=1200

    Mov AX,0
    Mov SI,Offset P2Source + 4
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    Mov Dl,Al
    Mov NumberHolder,DX 
DecNumberIntoBX ENDP

CheckTarget Proc
    PushALL
    PUSH SI 
    
    Mov SI, Offset P1Registers
    Mov CL,8
    Target1_loop:
    Mov Ax,[SI]
    CMP AX,Target
    JZ FoundinP1
    Add SI,2
    Dec CL
    JZ Target1_loop
    JMP NotFoundinP1
    FoundinP1: Mov Winner,1

    NotFoundinP1:
    Mov SI, Offset P2Registers
    Mov CL,8
    Target2_loop:
    Mov Ax,[SI]
    CMP AX,Target
    JZ FoundinP2
    Add SI,2
    Dec CL
    JZ Target1_loop
    JMP NotFoundinP2
    FoundinP2: Mov Winner,2
    NotFoundinP2:
    POP SI
    PopALL
    RET
CheckTarget ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NumberIntoBX Proc
    CMP Flag,1
    JZ NP1
    JMP NP2

    NP1:
    Mov SI,offset P1Source + 1      
    Mov Cl,[SI]
    A:
    CMP Cl,1
    JNZ B
    Mov AX,0
    Mov SI,Offset P1Source + 2
    Mov Al,[SI]
    Mov NumberHolder,AX
    RET
    B:
    CMP Cl,2     ;1 2   ;AX=0012   AX=0102
    JNZ C
    Mov AX,0
    Mov SI,Offset P1Source + 2    ;Al=0000 0001    Bl=0000 0010
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    SHL Al,4                      ;Al=0001 0000
    ADD Al,Bl                     ;Al=0001 0010
    Mov NumberHolder,AX
    RET
    C:
    CMP Cl,3
    JNZ D         
    Mov AX,0
    Mov SI,Offset P1Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    SHL AX,4
    Add Al,Bl
    INC SI
    Mov Bl,[SI]
    SHL AX,4
    Add Al,Bl
    Mov NumberHolder,AX
    RET
    D:
    PushALL
    PUSH SI
    CMP Cl,4   ;1 2 3 4
    JNZ LOOPEND         
    Mov AX,0
    Mov SI,Offset P1Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    SHL AX,4
    Add Al,Bl
    INC SI
    Mov Bl,[SI]
    SHL AX,4
    Add Al,Bl
    Inc SI
    Mov Bl,[SI]
    SHL AX,4
    Add Al,Bl
    Mov NumberHolder,AX
    POP SI 
    POPALL
    RET

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LOOPEND:RET

    NP2:
    Mov SI,offset P2Source + 1      
    Mov Cl,[SI]
    A1:
    CMP Cl,1
    JNZ B1
    Mov AX,0
    Mov SI,Offset P2Source + 2
    Mov Al,[SI]
    Mov NumberHolder,AX
    RET
    B1:
    CMP Cl,2     ;1 2   ;AX=0012   AX=0102
    JNZ C1
    Mov AX,0                     
    Mov SI,Offset P2Source + 2     ;;0001 0010
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    SHL Al,4
    ADD Al,Bl
    Mov NumberHolder,AX
    RET
    C1:
    CMP Cl,3
    JNZ D1        
    Mov AX,0
    Mov SI,Offset P2Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    SHL AX,4
    Add Al,Bl
    INC SI
    Mov Bl,[SI]
    SHL AX,4
    Add Al,Bl
    Mov NumberHolder,AX
    RET
    D1:
    PushALL
    PUSH SI
    CMP Cl,4 
    JNZ LOOPEND         
    Mov AX,0
    Mov SI,Offset P2Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    SHL AX,4
    Add Al,Bl
    INC SI
    Mov Bl,[SI]
    SHL AX,4
    Add Al,Bl
    Inc SI
    Mov Bl,[SI]
    SHL AX,4
    Add Al,Bl
    Mov NumberHolder,AX
    POP SI 
    POPALL
    RET
NumberIntoBX ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Main   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


MAIN PROC FAR    
MOV AX,@DATA 
MOV DS,AX
;Change to text Mode
Mov Ah,0
Mov Al,03h
INT 10H

CALL FirstS ;The first screen to input Name & points
Clear
Call SecondS ;The Second screen (Main)

;Call ShootingS
;MOV CX, 00H
;MOV DX, 3240H
;MOV AH, 86H
;INT 15H

BYE:
Main ENDP
END Main