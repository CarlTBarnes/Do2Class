                    MEMBER()
!--------------------------
! CBTxa2QClass 
!
!--------------------------
    INCLUDE('EQUATES.CLW')
    INCLUDE('CBTxa2Q.INC'),ONCE
Glo:String1 STRING(1),PRIVATE

  MAP 
    MODULE('WINAPI')
       CreateFile(*CSTRING,ULONG,ULONG,LONG,ULONG,ULONG,UNSIGNED=0),UNSIGNED,RAW,PASCAL,NAME('CreateFileA'),DLL(1)
       GetFileSize(UNSIGNED,<*ULONG>),ULONG,PASCAL,DLL(1)
       ReadFile(UNSIGNED,LONG,ULONG,*ULONG,LONG),BOOL,PASCAL,RAW,DLL(1)
       CloseHandle(UNSIGNED),BOOL,PASCAL,PROC,DLL(1)
       GetLastError(),LONG,PASCAL,DLL(1)
    END 
  END

!----------------------------------------
CBTxa2QClass.Construct        PROCEDURE()
!----------------------------------------
    CODE
    IF (SELF.LinesQ &= NULL) THEN SELF.LinesQ &= NEW CbTxaLineQueueType . 
    IF (SELF.SourceQ &= NULL) THEN SELF.SourceQ &= NEW CbTxaSourceQueueType . 
    RETURN
!---------------------------------------
CBTxa2QClass.Destruct PROCEDURE()
!---------------------------------------
    CODE
    IF NOT (SELF.LinesQ &= NULL)  THEN 
       FREE(SELF.LinesQ)   ! SELF.LinesQ_Free()  
       !TODO Leak Fix
       DISPOSE(SELF.LinesQ)    
    END
    IF NOT (SELF.SourceQ &= NULL)  THEN 
       FREE(SELF.SourceQ)   ! SELF.LinesQ_Free()  
       !TODO Leak Fix
       DISPOSE(SELF.SourceQ)    
    END
    DISPOSE(SELF.TxtTXA)
    DISPOSE(SELF.TxtUPR)
    SELF.TxtSize=0    
    RETURN
!-----------------------------------
CBTxa2QClass.Init     PROCEDURE()
!-----------------------------------
    CODE
    RETURN
!-----------------------------------
CBTxa2QClass.Kill     PROCEDURE()
!-----------------------------------
    CODE
    RETURN

!!-----------------------------------
!CBTxa2QClass.Proc1     PROCEDURE() !,VIRTUAL
!!-----------------------------------
!    CODE
!    RETURN

!-----------------------------------
CBTxa2QClass.CopyLinesQ  PROCEDURE(CbTxaLineQueueType OutLinesQ)
!-----------------------------------
X LONG
    CODE
    LOOP X=1 TO RECORDS(SELF.LinesQ) 
        GET(SELF.LinesQ,X)
        OutLinesQ = SELF.LinesQ
        ADD(OutLinesQ)
    END
    RETURN
CBTxa2QClass.CopyLinesQ  PROCEDURE(CbTxaLineQViewType OutViewQ) 
X LONG  !The &STRING are not showng in LIST so Try this
    CODE
    LOOP X=1 TO RECORDS(SELF.LinesQ) 
        GET(SELF.LinesQ,X)
        CLEAR(OutViewQ)
        OutViewQ.LineNo = SELF.LinesQ.LineNo
        OutViewQ.LenWO2 = SELF.LinesQ.LenWO2
        OutViewQ.TxtTxa = SELF.LinesQ.TxtTxa
        OutViewQ.TxtUpr = SELF.LinesQ.TxtUpr
        OutViewQ.PosBeg = SELF.LinesQ.PosBeg
        OutViewQ.PosEnd = SELF.LinesQ.PosEnd
        OutViewQ.TxtTxaRef &= SELF.LinesQ.TxtTxa
        OutViewQ.TxtUprRef &= SELF.LinesQ.TxtUpr
        ADD(OutViewQ)
    END
    RETURN
!-----------------------------------
CBTxa2QClass.CopySourceQ  PROCEDURE(CbTxaSourceQueueType OutSrcQ)
!-----------------------------------
X LONG
    CODE
    FREE(OutSrcQ)
    LOOP X=1 TO RECORDS(SELF.SourceQ) 
        GET(SELF.SourceQ,X)
        OutSrcQ = SELF.SourceQ
        ADD(OutSrcQ)
    END
    RETURN
    
!---------------------
CBTxa2QClass.ReadFile  PROCEDURE(STRING pFilename)!,LONG !Bytes   
FileHandle   SIGNED
BytesRead    ULONG
FileName     CSTRING(261)
FileSize     ULONG
HSizeHigh    ULONG   !>4GB 
!Api_GenericRead     EQUATE(80000000h)
!us_Share_Read  EQUATE(1h)
!us_Share_Write EQUATE(2h)
Api_DenyNone EQUATE(3)   !Api_Read  EQUATE(1h) + Api_Write EQUATE(2h)
Api_Open_Existing EQUATE(3)
    CODE
    IF ~EXISTS(pFilename) THEN Message('CBTxa2QClass.ReadFile |File not found|' & pFilename).
    SELF.Destruct()
    SELF.Construct()
    SELF.FileName=pFilename    
    FileName   =  CLIP(pFilename) 
!HANDLE CreateFileA(     https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilea
!  LPCSTR                lpFileName,
!  DWORD                 dwDesiredAccess,
!  DWORD                 dwShareMode,
!  LPSECURITY_ATTRIBUTES lpSecurityAttributes, 0
!  DWORD                 dwCreationDisposition,  was 4 now 4
!  DWORD                 dwFlagsAndAttributes,   80h = normal
!  HANDLE                hTemplateFile
!); 
    ! FileHandle = CreateFile(DOSFileName,80000000h,1h,0,3,80h,0)   
    FileHandle =  CreateFile(FileName,Api_DenyNone,Api_DenyNone,0,Api_Open_Existing,0,0)  
    IF FileHandle = -1 !Invalid_Handle_Value
        SELF.TxtTXA &=  NEW STRING(1)  
        SELF.TxtUPR &=  NEW STRING(1)
        Message('CBTxa2QClass.ReadFile CreateFile Invalid Handle ' & GetLastError())
    ELSE
        FileSize    =  GetFileSize(FileHandle) !,HSizeHigh) 
      !  Message('GetFileSize=' & FileSize & '|FileHandle=' & FileHandle &'|Error=' & GetLastError())
        SELF.TxtSize=CHOOSE(FileSize<1,1,FileSize)
        SELF.TxtTXA &=  NEW STRING(SELF.TxtSize)  
        SELF.TxtUPR &=  NEW STRING(SELF.TxtSize)  
        IF ~ReadFile(FileHandle,ADDRESS(SELF.TxtTXA),FileSize,BytesRead,0)
            Message('CBTxa2QClass.ReadFile Failed Error ' & GetLastError(),'CBTxa2QClass.ReadFile')
        END          
        CloseHandle(FileHandle)            
    END
    SELF.TxtUPR=UPPER(SELF.TxtTXA)    
    RETURN FileSize
!-----------------------------------
CBTxa2QClass.SplitLines     PROCEDURE() !,VIRTUAL
!-----------------------------------
LQ   &CbTxaLineQueueType
Chr BYTE,AUTO
Tx  &STRING
LenTxt &LONG
LineCnt LONG
B LONG,AUTO
X LONG,AUTO 
E LONG,AUTO 
    CODE 
    FREE(SELF.LinesQ)   !TODO Free queues
    IF SELF.TxtSize < 2 THEN RETURN.
    LQ &= SELF.LinesQ 
    Tx &= SELF.TxtTXA ; LenTxt &= SELF.TxtSize

    B=1 ; LineCnt=0 
    LOOP X=1 TO LenTxt+1
        IF X<LenTxt AND VAL(Tx[X])<>13 THEN CYCLE.
        LineCnt += 1  
        E=X+1                !E=End with 10 after 13
        IF X=LenTxt+1 THEN   !The last byte, might not end with 13,10
           E=X-1
           IF E<B THEN BREAK.  !Hit the end and
        END 
                        !   Message('x=' & x &'|B=' & B &'|E=' & E & '|LineCnt=' & LineCnt )
        LQ.LineNo = LineCnt    ! LineNo      LONG      !LQ:LineNo
        LQ:LenWO2 = E-B+1 -2 !w/o 13,10    ! LenTxt      LONG      !LQ:LenTxt
        LQ:PosBeg = B    ! PosBeg      LONG      !LQ:PosBeg
        LQ:PosEnd = E    ! PosEnd      LONG      !LQ:PosEnd
                         IF B > E THEN Message('B << E ' & B & ' << '& E &'  LineCnt=' & LineCnt,'Bug CBTxa2QClass.SplitLines').
        IF LQ:LenWO2 > 0 THEN  !some text & 13,10
                         IF B > E-2 THEN Message('B>E-2 ' & B & ' > '& E &' -2  LineCnt=' & LineCnt,'Bug CBTxa2QClass.SplitLines').
           LQ:TxtTxa &= Tx[B : E-2]  
                        !     Setclipboard('"' &  Tx[B : E-2] & '"<<-end') ; stop('Tx[] line on clip')
                        !     Setclipboard('"' &  LQ:TxtTxa & '"<<-end') ; stop('LQ:TxtTxa line on clip - ' & '"' &  LQ:TxtTxa & '"<<-end')
           LQ:TxtUPR &= Self.TxtUPR[B : E-2]
        ELSE !Just 13,10
           LQ:TxtTxa &= Glo:String1
           LQ:TxtUPR &= Glo:String1        
        END
        ADD(LQ)
        X=E ; B=X+1 
    END           
    RETURN
!-----------------------------------
CBTxa2QClass.EmbedSourceQLoad     PROCEDURE() !,VIRTUAL
!-----------------------------------
LQ   &CbTxaLineQueueType
SQ   &CbTxaSourceQueueType
B LONG,AUTO
X LONG,AUTO 
E LONG,AUTO 
EmbedName   STRING(64)
EmbedNameUpr STRING(64),AUTO
EmbedLine   LONG 
EmbedType   STRING(1) 
InSource    BOOL
!LastSection STRING(16)  !last [xxxx] does not PUSH/POP. Not used 
Terms EQUATE('[END][ADDITION][FORMULA][SOURCE][EMBED][PROMPTS][DATA][WINDOW] [COMMON][CALLS][PROCEDURE][REPORT][MODULE][PROGRAM][FILES][PERSIST][SECONDARY][PRIMARY]')
    CODE 
    LQ &= SELF.LinesQ 
    SQ &= SELF.SourceQ 
    FREE(SQ) ; CLEAR(SQ)
    SELF.ProcCount=0
    SELF.ProcName=''
    SELF.FromTemplate=''
!    GET(LQ,336)    ; SETCLIPBOARD( '=' & LQ:TxtUpr & '=' )
!    Message('source Q read lines ' & RECORDS(LQ) &'||' & LQ:TxtUpr & |
!            '||' & CHOOSE(LQ:TxtUpr='[EMBED]','Is Embed','Is Not') )
    LOOP X=1 TO RECORDS(LQ) 
        GET(LQ,X)
!        IF SUB(LQ:TxtUpr,1,1)='[' THEN
!           LastSection=LQ:TxtUpr
!        END
        IF InSource AND SUB(LQ:TxtUpr,1,1)='[' THEN  ![SOURCE] Ends with any [xxx]
           E=INSTRING(']',LQ:TxtUpr,1)        !Does it end "]", not done by 
!TODO this still did not catch an exta [END]            
           IF E THEN  !Does it end "]", not done by
              IF ~INSTRING(LQ:TxtUpr[1: E],Terms,1) THEN 
                  Message('Problem parsing TXA found "[" in column 1 which ends a [SOURCE]' & |
                          '|but "' & LQ:TxtUPR[1: E] &'" is not a documented TXA [Section].' & |
                          '|This is probably in an Embed with OMIT.' & |
                          '||This TXA probably WILL NOT import. Try it, have a backup APP.' & |
                          '||Line: ' & X &'|Code: ' & CLIP(CLIP(LQ:TxtTXA)) ,|
                          'TXA Load CBTxa2Q Class',ICON:Exclamation,'Close',,MSGMODE:CANCOPY)
              END
              InSource=0
              SQ:SourceEnd=X-1
              PUT(SQ)
              CLEAR(SQ)  ; SQ:Embed='after end ' & X
           ELSE
                  Message('Problem parsing TXA found "[" in column 1 which ends a [SOURCE]' & |
                          '|but no closing "]" was found.' & | 
                          '|This is probably in an Embed with OMIT.' & |
                          '||This TXA probably WILL NOT import. Try it, have a backup APP.' & |
                          '||Line: ' & X &'|Code: ' & CLIP(CLIP(LQ:TxtTXA)) ,|
                          'TXA Load CBTxa2Q Class',ICON:Exclamation,'Close',,MSGMODE:CANCOPY)
           
           END
        END
        IF SUB(LQ:TxtUpr,1,7)='EMBED %' THEN
           EmbedName = LQ:TxtTXA 
           EmbedNameUpr=UPPER(EmbedName)
           EmbedLine = X 
           CASE EmbedNameUpr
           OF   'EMBED %DATASECTION'                !Not in Report nor Process Template incl CPCS :(
           OROF 'EMBED %DATASECTIONBEFOREWINDOW'
           OROF 'EMBED %DATASECTIONENDWINDOW'
           OROF 'EMBED %DATASECTIONAFTERWINDOW'
           OROF 'EMBED %DECLARATIONSECTION'         !Appears in all except Source
                                                EmbedType=CbTxaEmbedType:Data
           OF   'EMBED %PROCEDUREROUTINES'
           OROF 'EMBED %PROCROUTINES'
                                                EmbedType=CbTxaEmbedType:Routine
           OF   'EMBED %LOCALPROCEDURES' 
                                                EmbedType=CbTxaEmbedType:LocalPro
           ELSE 
                IF INSTRING('METHODCODESECTION',EmbedNameUpr,1) THEN                   !#EMBED(%NewMethodDataSection
                                                EmbedType=CbTxaEmbedType:MethodCode
                ELSIF INSTRING('METHODDATASECTION',EmbedNameUpr,1) THEN
                                                EmbedType=CbTxaEmbedType:MethodData
                ELSE
                                                EmbedType=CbTxaEmbedType:Unknown
                END
                
           END           
        END
        CASE LQ:TxtUpr
!Not every EMBED % has a [EMBED]        
!        OF '[EMBED]' 
!           X += 1
!           GET(LQ,X)
!           EmbedName = LQ:TxtTXA
!           EmbedLine = X

        OF '[PROCEDURE]' !Can also see in Calls but does not have NAME
            GET(LQ,X+1)
            IF SUB(LQ:TxtUpr,1,5)='NAME ' THEN
               SELF.ProcCount+=1
               IF SELF.ProcCount=1 THEN SELF.ProcName=SUB(LQ:TxtTXA,6,999).
            END
        OF '[SOURCE]'
            CLEAR(SQ)
            SQ:Type=EmbedTYPE
            SQ:Embed=EmbedName
            SQ:EmbedLnNo=EmbedLine
            SQ:SourceBeg=X
            SQ:CodeBeg=X+1
            ADD(SQ)
            InSource=1

        OF '[COMMON]'                  !Find FROM Template by looking ahead rather than trust LastSection
            IF SELF.ProcCount=1 THEN   !In [PROCEDURE] not in [MODULE], and not Proc #2 (unsupported)
               LOOP E=1 TO 99          !Look ahead ... expect DESCRIPTION  LONG (multi) FROM MODIFIED
                    GET(LQ,X+E) ; IF ERRORCODE() THEN BREAK.
                    IF SUB(LQ:TxtUpr,1,5)='FROM ' THEN
                       SELF.FromTemplate=SUB(LQ:TxtTXA,6,128)
                    ELSIF SUB(LQ:TxtUpr,1,1)='[' THEN
                       BREAK 
                    END
               END
               GET(LQ,X) 
            END
        OF 'PROPERTY:END'
            IF InSource AND SQ:CodeBeg=SQ:SourceBeg+1 THEN
                SQ:CodeBeg=X+1
                GET(LQ,X+1) 
                SQ:CodeLine1=LQ:TxtTXA
                PUT(SQ)            
            END
!        OF '[END]'    [Source] ends with Any [
!            IF ~InSource THEN CYCLE. 
!            InSource=0
!            SQ:SourceEnd=X
!            PUT(SQ)
!            CLEAR(SQ)  ; SQ:Embed='after end ' & X
        END
    END
    RETURN 

!-----------------------------------
CBTxa2QClass.LoadEmbedLines  PROCEDURE(LONG SourceQRecNo, CbTxaLineQViewType OutViewQ)
!-----------------------------------
X LONG  !The &STRING are not showng in LIST so Try this
    CODE
    FREE(OutViewQ)
    GET(SELF.SourceQ,SourceQRecNo) 
    IF ERRORCODE() THEN Message('Get Source Q failed ' & SourceQRecNo ) ;  RETURN.
   ! Message('Src load ' & SELF.SourceQ.SourceBeg &' TO '& SELF.SourceQ.SourceEnd )
    LOOP X=SELF.SourceQ.SourceBeg TO SELF.SourceQ.SourceEnd 
        GET(SELF.LinesQ,X)
        CLEAR(OutViewQ)
        OutViewQ.LineNo = SELF.LinesQ.LineNo
        OutViewQ.LenWO2 = SELF.LinesQ.LenWO2
        OutViewQ.TxtTxa = SELF.LinesQ.TxtTxa
        OutViewQ.TxtUpr = SELF.LinesQ.TxtUpr
        OutViewQ.PosBeg = SELF.LinesQ.PosBeg
        OutViewQ.PosEnd = SELF.LinesQ.PosEnd
        ADD(OutViewQ)
    END
    RETURN
    OMIT('**END**')
CbTxaSourceQueueType  QUEUE,TYPE
Embed       STRING(64)  !SQ:Embed      EMBED %Name  1+ after [EMBED]
EmbedLnNo   LONG        !SQ:EmbedLnNo  [EMBED] line
SourceBeg   LONG        !SQ:SourceBeg  [SOURCE] Line#
CodeBeg     LONG        !SQ:CodeBeg    PROPERTY:END +1 Line#
SourceEnd   LONG        !SQ:SourceEnd  [END] Line#
CodeLine1   STRING(128) !SQ:CodeLine1
               END    
               
CbTxaLineQueueType     QUEUE,TYPE
LineNo      LONG      !LQ:LineNo
LenTxt      LONG      !LQ:LenTxt
TxtTxa      &STRING   !LQ:TxtTxa
TxtUpr      &STRING   !LQ:TxtUpr
PosBeg      LONG      !LQ:PosBeg
PosEnd      LONG      !LQ:PosEnd
               END    
    !end of OMIT('**END**') 

    OMIT('** TERMS.CLW **')

  !Format of group is:
  !Section Header
  !Termination sections

Terminators GROUP()
Number        USHORT(18)
              PSTRING('[APPLICATION]')
              PSTRING('<<ALL>')
              PSTRING('[ADDITION]')
              PSTRING('[ADDITION][PERSIST][PROGRAM][PROCEDURE][CALLS][WINDOW][REPORT][FORMULA][MODULE]')
              PSTRING('[FORMULA]')
              PSTRING('<<HEADER>')
              PSTRING('[SOURCE]')
              PSTRING('<<HEADER>')   !<HEADER> means  any [xxx]
              PSTRING('[EMBED]')
              PSTRING('[ADDITION][PERSIST][PROGRAM][PROCEDURE][CALLS][WINDOW][REPORT][FORMULA]')
              PSTRING('[PROMPTS]')
              PSTRING('<<HEADER>')
              PSTRING('[DATA]')
              PSTRING('[FILES][PROMPTS][EMBED][ADDITION][PERSISTS][PROGRAM][PROCEDURE][CALLS][WINDOW][REPORT][FORMULA][MODULE]')
              PSTRING('[WINDOW]')
              PSTRING('<<HEADER>')
              PSTRING('[COMMON]')
              PSTRING('<<HEADER>')
              PSTRING('[CALLS]')
              PSTRING('<<HEADER>')
              PSTRING('[PROCEDURE]')
              PSTRING('[PROCEDURE][MODULE]')
              PSTRING('[REPORT]')
              PSTRING('<<HEADER>')
              PSTRING('[MODULE]')
              PSTRING('[MODULE]')
              PSTRING('[PROGRAM]')
              PSTRING('[MODULE]')
              PSTRING('[FILES]')
              PSTRING('[EMBED][ADDITION][DATA][FILES][CALLS][WINDOW][REPORT][FORMULA][END][PROCEDURE][PERSIST][PROGRAM][MODULE][PROJECT]')
              PSTRING('[PERSIST]')
              PSTRING('[PROGRAM][MODULE]')
              PSTRING('[SECONDARY]')
              PSTRING('<<HEADER>')
              PSTRING('[PRIMARY]')
              PSTRING('[OTHERS][PROMPTS][EMBED][ADDITION][CALLS][WINDOW][REPORT][FORMULA]')
            END
    !end of OMIT('** TERMS.CLW **')

    