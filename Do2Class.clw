![ ] Write Option ONE Method per Embed to split big embeds with many routines 
!
  PROGRAM 
  INCLUDE('KEYCODES.CLW'),ONCE
  INCLUDE('CBTxa2Q.INC'),ONCE     

OmitWndPrv EQUATE(0)                !Set to (1) if you do not have CBWndPreviewClass
    OMIT('**END**', OmitWndPrv)     !WndPreview secret button hover upper left corner and pops up
  INCLUDE('CBWndPreview.INC'),ONCE  !Download from https://github.com/CarlTBarnes/WindowPreview
WndPrvCls   CBWndPreviewClass       !At least download the LibSrc files and put in this folder
         **END**

  MAP
Do2Class        PROCEDURE()
PopupUnder      PROCEDURE(LONG CtrlFEQ, STRING PopMenu),LONG
DB              PROCEDURE(STRING xMessage) 
    MODULE('RTL')
LenFastClip         PROCEDURE(CONST *STRING Text2Measure),LONG,NAME('Cla$FASTCLIP') 
OutputDebugString   PROCEDURE(*Cstring),RAW,PASCAL,DLL(1),NAME('OutputDebugStringA')
    END
  END
DbgIt      BYTE(1)          !Output Debug  
DosFile    FILE,DRIVER('DOS'),PRE(DF),CREATE
    RECORD
Block   STRING(8000)
    . .
CfgFileIni STRING('.\Do2Class.INI')    
    CODE  
    Do2Class()
    RETURN
Do2Class        PROCEDURE()
  INCLUDE('Do2Class_DATA.clw')
  INCLUDE('Do2Class_Window.clw')

  CODE
  TxaLoadFile='Carlbase_Main.txa' 
  SYSTEM{PROP:PropVScroll}=1
  SYSTEM{PROP:MsgModeDefault}=MSGMODE:CANCOPY
  OPEN(Window)
  DDD.PrepareWindow()
  
  POST(EVENT:Accepted,?IsTXAinput)
  ACCEPT
    CASE EVENT() 
    OF EVENT:OpenWindow   ; IF OrigCode THEN POST(EVENT:Accepted, ?ProcessBtn).
    OF EVENT:NewSelection ; IF ?{PROP:Type}=CREATE:Spin THEN POST(EVENT:Accepted, ?).
    OF EVENT:Sized ; POST(EVENT:DoResize,0,THREAD())
    OF EVENT:DoResize ; DDD.ResizeWindow()
    OF EVENT:AlertKey
       CASE KEYCODE()
       OF DeleteKey 
          CASE FIELD()
          OF ?LIST:ChangeQ ; GET(ChangeQ,CHOICE(?LIST:ChangeQ)) ; DELETE(ChangeQ) ; DISPLAY
          END
       OF EnterKey
          IF ?{PROP:Type}=CREATE:list THEN SETKEYCODE(MouseLeft2) ; POST(EVENT:Accepted,?). 
       END
    OF EVENT:Drop
       TxaLoadFile=DROPID() ; SELECT(?TxaLoadFile) ; DISPLAY
    END
    CASE ACCEPTED()
    OF ?DooClassName OROF ?DooClassName:2 ; DooClassName=LEFT(DooClassName) ; IF ~DooClassName THEN DooClassName='DOO'.
                                            p2ClassName=CLIP(DooClassName) ; p2ClassDot=p2ClassName &'.' ! e.g. DOO / DOO.
 
    OF ?IsTXAinput ; DDD.IsTXAinputChanged()
    OF ?LoadTxaBtn
       IF DDD.LoadTxaFile() THEN 
          IF ParseOnly THEN 
              ENABLE(?ProcessTxaBtn) ;  ENABLE(?BuildTxaBtn) 
          END
          MruClass.PushFile(TxaLoadFile)
          DDD.ConfigLoadSave(2) 
          0{PROP:Text}=0{'OrigCap'} &' - ' & CLIP(TxaCls.ProcName) &' - ' &TxaLoadFile
          DISPLAY
       END     
    OF ?ProcessTxaBtn ; DDD.ProcessCodeQ2DoClass() ; DISPLAY ; SELECT(?TabCodeQ)
    OF ?TxaSaveBtn    ; DDD.WriteTXAFile() ; SETCLIPBOARD(TxaSaveFile)
    OF ?CopySaveFnBtn ; SETCLIPBOARD(TxaSaveFile)
    OF ?EditSaveFnBtn    ; DDD.EditSaveFn()
    OF ?CompareSaveFnBtn ; DDD.CompareSaveFn()
    OF ?ExploreSaveFnBtn ; IF TxaSaveFile THEN RUN('Explorer.exe /select,"' & CLIP(TxaSaveFile) &'"').
    OF ?PickTXAbtn    ; IF FILEDIALOG('Select a TXA file',TxaLoadFile, |
                                      'TXA & APV Files|*.TXA;*.APV|TXA Files|*.TXA|APV Files|*.APV|All Files|*.*' , |
                                      File:LongName + FILE:KeepDir) 
                           SELECT(?TxaLoadFile) ; DISPLAY ; POST(EVENT:Accepted,?LoadTxaBtn) 
                        END
    OF ?PasteTXAbtn   ; TxaLoadFile=CLIPBOARD() ; DISPLAY ; IF EXISTS(TxaLoadFile) THEN POST(EVENT:Accepted,?LoadTxaBtn).
    OF ?BuildTxaBtn   ; DDD.BuildFixdCode()        ; DISPLAY ; SELECT(?TabClassCode)
   
    OF ?PasteBtn   ; LnTxt = LEN(CLIPBOARD()) ; LineX=SIZE(OrigCode) * .9
                     IF LnTxt > LineX THEN 
                        CASE Message('The pasted text is ' & LnTxt & ' bytes.' & |
                                    '|The maximum size is ' & LineX & ' bytes.' & |
                                    '||' & LnTxt-LineX & ' bytes will NOT be processed',|
                                    'Paste too large',ICON:Asterisk,BUTTON:CANCEL+BUTTON:IGNORE,BUTTON:CANCEL) 
                        OF BUTTON:CANCEL ; CYCLE
                        END
                     END
                     OrigCode=CLIPBOARD() ; POST(EVENT:Accepted, ?ProcessBtn)
    OF ?ProcessBtn ; DDD.ProcessOrigCodeToCodeQ() ; IF ParseOnly THEN CYCLE.
                     DDD.ProcessCodeQ2DoClass()
                     SELECT(?TabCodeQ) ; DISPLAY
                     DDD.BuildFixdCode()
                     SELECT(?TabClassCode) ; DISPLAY
    OF ?ClearBtn ; CLEAR(OrigCode) ; DISPLAY ; SELECT(?OrigCode)
    OF ?CopyOrigBtn ; SETCLIPBOARD(OrigCode) ; SELECT(?OrigCode)
    OF ?CopyBtn  ; SETCLIPBOARD(FixdCode) 
    OF ?CopyChangesBtn   ; DDD.ChangesCodeCopy()
    OF ?CopyImplicitsBtn ; DDD.ImplicitCodeCopy() 
    OF ?RunAgainBtn OROF ?RunAgainBtn2 ; RUN(Command('0'))
    OF ?Font10pt ; DDD.Font10ptChange()

    OF ?CodeBackRtn OROF ?CodeNextRtn ; DDD.CodeQBackNext(CHOOSE(?=?CodeBackRtn,-1,1)) 
    OF ?Sample:Routine ; DDD.CodeQBackNext(0, StyleA:Routine)  
    OF ?Sample:Change  ; DDD.CodeQBackNext(0, StyleA:Change)  
    OF ?Sample:Problem ; DDD.CodeQBackNext(0, StyleA:Problem)  
    OF ?Sample:Impli   ; DDD.CodeQBackNext(0, StyleA:Implicit)      
    OF ?Sample:OMIT    ; DDD.CodeQBackNext(0, StyleA:Omit)      
    OF ?XRefExpandBtn OROF ?XRefContractBtn ; A=CHOOSE(?=?XRefContractBtn)
        DDD.TreeExpandContract(XRefQ,  XRefQ.Level,  A)
        DDD.TreeExpandContract(XRef2Q, XRef2Q.Level, A)
    OF ?XRefCopyBtn ; DDD.XRefCopy()
    OF ?XRefDbgBtn ; UNHIDE(?TabXRefDebug) ; SELECT(?TabXRefDebug)
    OF ?LIST:ChangeQ ; GET(ChangeQ,CHOICE(?LIST:ChangeQ)) ; DDD.RightClickLine(ChangeQ:LineNo)
                       IF KEYCODE()=MouseLeft2 
                          SETKEYCODE(0) 
                          ?LIST:ChangeQ:SbS{PROP:Selected}=?LIST:ChangeQ{PROP:Selected}
                          SourceQ:LineNo=ChangeQ:LineNo ; GET(SourceQ,SourceQ:LineNo) ; SELECT(?List:SourceQ,POINTER(SourceQ))                              
                          CodeQ:LineNo=ChangeQ:LineNo ; GET(CodeQ,CodeQ:LineNo) ; SELECT(?List:CodeQ,POINTER(CodeQ)) ; display
!                          SourceQ:LineNo=ChangeQ:LineNo ; GET(SourceQ,SourceQ:LineNo) ; SELECT(?List:SourceQ,POINTER(SourceQ))    
                       END
    OF ?LIST:ChangeQ:SbS  ; GET(ChangeQ,CHOICE(?LIST:ChangeQ:SbS)) ; POST(EVENT:Accepted,?LIST:ChangeQ)                      
    OF ?LIST:ClassQ   ; GET(ClassQ,CHOICE(?LIST:ClassQ)) ; DDD.RightClickLine(ClassQ:LineNo)
                        IF KEYCODE()=MouseLeft2 THEN 
                           SETKEYCODE(0) 
                           ChangeQ:LineNo=ClassQ:LineNo 
                           GET(ChangeQ,ChangeQ:LineNo) 
                           IF ~ERRORCODE() THEN 
                               ?LIST:ChangeQ:SbS{PROP:Selected}=POINTER(ChangeQ)
                               SELECT(?LIST:ChangeQ,POINTER(ChangeQ))
                               CYCLE
                           END
                           SourceQ:LineNo=ClassQ:LineNo  ; GET(SourceQ,SourceQ:LineNo) ; SELECT(?List:SourceQ,POINTER(SourceQ))                            
                        END
    OF ?LIST:CodeQ    ; GET(CodeQ,CHOICE(?LIST:CodeQ)) ; DDD.RightClickLine(CodeQ:LineNo)
                        DDD.SelectSourceQLine(CodeQ:LineNo)

    OF ?LIST:CodeOU   ; GET(CodeQ,CHOICE(?LIST:CodeOU)) ; DDD.RightClickLine(CodeQ:LineNo)
                        DDD.SelectSourceQLine(CodeQ:LineNo)

    OF ?LIST:ProblmQ  ; GET(ProblmQ,CHOICE(?LIST:ProblmQ)) ; DDD.RightClickLine(ProblmQ:LineNo)
                        IF KEYCODE()=MouseLeft2 THEN  
                           !DDD.SelectSourceQLine(ImpliQ:LineNo)
                           CodeQ:LineNo=ProblmQ:LineNo ; GET(CodeQ,CodeQ:LineNo) ; SELECT(?List:CodeQ,POINTER(CodeQ))                          
                        END

    OF ?LIST:ImpliQ   ; GET(ImpliQ,CHOICE(?LIST:ImpliQ)) ; DDD.RightClickLine(ImpliQ:LineNo)
                        IF KEYCODE()=MouseLeft2 THEN  
                           !DDD.SelectSourceQLine(ImpliQ:LineNo)
                           CodeQ:LineNo=ImpliQ:LineNo ; GET(CodeQ,CodeQ:LineNo) ; SELECT(?List:CodeQ,POINTER(CodeQ))                          
                        END

    OF ?LIST:OmitQ   ; GET(OmitQ,CHOICE(?LIST:OmitQ)) ; DDD.RightClickLine(OmitQ:LineNo)
                        IF KEYCODE()=MouseLeft2 THEN  
                           CodeQ:LineNo=OmitQ:LineNo ; GET(CodeQ,CodeQ:LineNo) ; SELECT(?List:CodeQ,POINTER(CodeQ))                          
                        END
                        
    OF ?LIST:EmbedQ   ; GET(EmbedQ,CHOICE(?LIST:EmbedQ)) ; DDD.RightClickLine(EmbedQ:CodeBeg)
                        IF KEYCODE()=MouseLeft2 THEN
                           ?List:TxaQ{PROP:YOrigin}=EmbedQ:SourceBeg
                           TxaQ:LineNo=CHOOSE(~EmbedQ:CodeBeg,EmbedQ:SourceBeg,EmbedQ:CodeBeg)
                           GET(TxaQ,TxaQ:LineNo) ; SELECT(?List:TxaQ,POINTER(TxaQ))    
                           SETKEYCODE(0)
                        END
    OF ?LIST:SourceQ  ; GET(SourceQ,CHOICE(?LIST:SourceQ))  ; DDD.RightClickLine(SourceQ:LineNo)
                        IF KEYCODE()=MouseLeft2 AND IsTXAinput 
                           TxaQ:LineNo=SourceQ:LineNo ; GET(TxaQ,TxaQ:LineNo) ; SELECT(?List:TxaQ,POINTER(TxaQ))    
                           SETKEYCODE(0)
                        END
    OF ?LIST:TxaQ     ; GET(TxaQ,CHOICE(?LIST:TxaQ))  ; DDD.RightClickLine(TxaQ:LineNo) 
                        IF KEYCODE()=MouseLeft2 
                           SETKEYCODE(0) 
                           SourceQ:LineNo=TxaQ:LineNo 
                           GET(SourceQ,SourceQ:LineNo) 
                           IF ERRORCODE() THEN GET(SourceQ,POSITION(SourceQ)).  !Find next closest SourceQ, must have SORT(
                           IF ~ERRORCODE() THEN SELECT(?List:SourceQ,POINTER(SourceQ)).
                         END  
    OF ?LIST:SxaQ     ; GET(SxaQ,CHOICE(?LIST:SxaQ))  ; DDD.RightClickLine(SxaQ:LineNo) 
                        IF KEYCODE()=MouseLeft2 
                           SETKEYCODE(0) 
                           TxaQ:LineNo=SxaQ:LineNo 
                           GET(TxaQ,TxaQ:LineNo) 
                           SELECT(?List:TxaQ,POINTER(TxaQ))
                         END 
    OF ?LIST:XRefQ   ; GET(XRefQ,CHOICE(?LIST:XRefQ)) ; DDD.RightClickLine(XRefQ:LineNo)
                        IF KEYCODE()=MouseLeft2 THEN 
                           DDD.SelectSourceQLine(XRefQ:LineNo)
                           CodeQ:LineNo=XRefQ:LineNo ; GET(CodeQ,CodeQ:LineNo) ; SELECT(?List:CodeQ,POINTER(CodeQ))                           
                        END 
    OF ?LIST:XRef2Q   ; GET(XRefQ,CHOICE(?LIST:XRef2Q)) ; DDD.RightClickLine(XRef2Q:LineNo)
                        IF KEYCODE()=MouseLeft2 THEN 
                           DDD.SelectSourceQLine(XRef2Q:LineNo)
                           CodeQ:LineNo=XRef2Q:LineNo ; GET(CodeQ,CodeQ:LineNo) ; SELECT(?List:CodeQ,POINTER(CodeQ))                           
                        END
    OF ?Jump:Omit ; SELECT(?TabOmitQ)                        
    OF ?Jump:Implicit ; SELECT(?TabImpliQ)
    OF ?Jump:Problems ; SELECT(?TabProblmQ)
    OF ?Jump:SaveTxa ; SELECT(?TabClassCode)
    END
  END
  DDD.ConfigLoadSave(2)
  RETURN
!==========================================  
DDD.PrepareWindow PROCEDURE()
    CODE
  DDD.ResizeWindow(1)
  0{'OrigCap'}=0{PROP:Text}
  0{PROP:MinWidth}=440 ; 0{PROP:MinHeight}=200
  ?Sheet1{PROP:TabSheetStyle}=1 ; ?Sheet_TXA{PROP:TabSheetStyle}=1
  DDD.LineHt(?LIST:ChangeQ,2)  !  ?LIST:ChangeQ{PROP:LineHeight} = 2 + ?LIST:ChangeQ{PROP:LineHeight} 
  DDD.LineHt(?List:EmbedQ)  
  DDD.LineHt(?TxaLoadFile)  
  !?TxaLoadFile{PROP:FontName}='Consolas'
  DDD.StyleSetCodeQLists() 
  DDD.ConfigLoadSave(1)
  MruClass.Init()

    OMIT('**END**', OmitWndPrv)   !Not required in 11.13505, but below does show Queue Name in WndPreview
  WndPrvCls.Init(1) 
  WndPrvCls.InitList(?LIST:TxaQ        ,TxaQ    ,'TxaQ   ')     !WndPreview secret button hover upper left corner and pops up
  WndPrvCls.InitList(?LIST:EmbedQ      ,EmbedQ  ,'EmbedQ ')     !In WndPreview main window right click on a LIST
  WndPrvCls.InitList(?LIST:SourceQ     ,SourceQ ,'SourceQ')     !On Popup select "LIST FORMAT() and PropList: Properties
  WndPrvCls.InitList(?LIST:SxaQ        ,SxaQ    ,'SxaQ   ')     !On PROPLIST window click the From(Q) button
  WndPrvCls.InitList(?LIST:ChangeQ     ,ChangeQ ,'ChangeQ')     !On From(Q) Tab can "View From(Q)
  WndPrvCls.InitList(?LIST:ChangeQ:SbS ,ChangeQ ,'ChangeQ') 
  WndPrvCls.InitList(?LIST:ClassQ      ,ClassQ  ,'ClassQ ') 
  WndPrvCls.InitList(?LIST:CodeQ       ,CodeQ   ,'CodeQ  ') 
  WndPrvCls.InitList(?LIST:CodeOU      ,CodeQ   ,'CodeQ  ') 
  WndPrvCls.InitList(?LIST:ProblmQ     ,ProblmQ ,'ProblmQ') 
  WndPrvCls.InitList(?LIST:ImpliQ      ,ImpliQ  ,'ImpliQ ') 
  WndPrvCls.InitList(?LIST:OmitQ       ,OmitQ   ,'OmitQ  ') 
  WndPrvCls.InitList(?LIST:XRefQ       ,XRefQ   ,'XRefQ  ') 
  WndPrvCls.InitList(?LIST:XRef2Q      ,XRef2Q  ,'XRef2Q ') 
  WndPrvCls.InitList(?LIST:XRefQ__     ,XRefQ   ,'XRefQ  ') 
  WndPrvCls.InitList(?LIST:XRef2Q__    ,XRef2Q  ,'XRef2Q ') 
    !end of OMIT('**END**', OmitWndPrv)


  ?ProbReturnFix{PROP:Tip}='The DOO.Method() could Return a BOOL False to indicate Return.' & |
                           '<13,10>Method() could Return BYTE like an ABC like LEVEL:Fatal.' & |
                           '<13,10>Caller could be refactored to not rely on Method() doing Return.' & |
                           ''
  ?ProbReturnDPR{PROP:Tip}='The DOO.Method() performing DO ProcedureReturn will cause a Compiler error.' & |
                           '<13,10>The possible fixes are the same as RETURN fixes.'

  ?ProbOmittedFix{PROP:Tip}='A Routine checking IF OMITTED(VarName) will be for the Procedure(<<VarName>).' & |
                           '<13,10>When changed to DOO.Method() this will NOT work correctly.' & |
                           '<13,10>If this is ABC this must still be a ROUTINE or change the stupid Window template to allow an Embed after CODE.' & |
                           '<13,10>If this is Legacy simply move the OMITTED() code outside the Method and maybe assign to a variable.' &|
                           '<13,10>Do NOT use old syntax OMITTED(#). Change the (#) to the (VarName).' &|
                           ''
  RETURN
DDD.LineHt PROCEDURE(LONG FEQ, SHORT HtBump=1)
  CODE
   FEQ{PROP:LineHeight} = HtBump + FEQ{PROP:LineHeight} 
!-------------------------  
DDD.IsTXAinputChanged  PROCEDURE()  !Choose betweeen RADIO('TXA')=1 and RADIO('Source')=0
    CODE 
    ?SaveTxaGroup{PROP:Hide}=CHOOSE(IsTXAinput=0)  ! BUTTON('&Save TXA with Classes'),USE(?TxaSaveBtn)
    ?TabTxaLoad{PROP:Hide}  =CHOOSE(IsTXAinput=0)  ! TAB(' TXA Load '),USE(?TabTxaLoad)
    ?TabRoutineCode{PROP:Hide}=CHOOSE(IsTXAinput=1)  !  TAB(' ROUTINE Code '),USE(?TabRoutineCode)
    SELECT(CHOOSE(IsTXAinput=1,?TabTxaLoad,?TabRoutineCode))
    RETURN 
!-------------------------  
DDD.LoadTxaFile  PROCEDURE()
  CODE 
  FREE(TxaQ) ; FREE(EmbedQ) ; FREE(SourceQ) ; FREE(ProblmQ) ; FREE(ImpliQ) ; Clear(Counts)
  IF ~EXISTS(TxaLoadFile) THEN 
     Message('File not found||' & TxaLoadFile)  ; SELECT(?TxaLoadFile) ; RETURN 0
  END
  IF INSTRING('_Do2Class_',TxaLoadFile,1) THEN
     IF 1=Message('That file name contains "_Do2Class_"|so has already been processed.||'&TxaLoadFile, |
                  'Do2Class',,'Cancel|Process') THEN RETURN 0.
  END 
  IF ~TxaCls.ReadFile(TxaLoadFile) THEN 
     Message('File did not load') ; SELECT(?TxaLoadFile) ;  RETURN 0
  END
  X=LenFastClip(TxaLoadFile)
  TxaSaveFile=SUB(TxaLoadFile,1,X-4) &'_Do2Class_.TXA' ; ENABLE(?SaveTxaGroup)
  TxaCls.SplitLines()
  TxaCls.EmbedSourceQLoad() 
  IF TxaCls.ProcCount > 1 THEN 
     IF 2<>Message('Not Supported.... TXA contains ' & TxaCls.ProcCount & ' Procedures.',|
                   'Do2Class',ICON:Hand,'Close|Ignore') THEN 
        RETURN False
     END
  END
  TxaCls.CopyLinesQ(TxaQ)  
  TxaCls.CopySourceQ(EmbedQ)
  SELF.LoadSxaQFromTxaQ()
  SELF.LoadSourceQFromTxaQ()    !Load the Local Source Q from the TXA Q 
  !TODO Break this into separate procedure ? Above Loads, below Processes
  SELF.ProcessSourceQToCodeQ()  !Copy SourceQ into CodeQ
  IF ParseOnly THEN RETURN 1.
  DDD.ProcessCodeQ2DoClass()    !Read
  SELECT(?TabCodeQ) 
  DDD.BuildFixdCode()
  SELECT(?TabClassCode)
  DDD.TxaAddChangesQEmbeds() 
  DDD.TabCountQ()
  DDD.LogFileAppend() 
  IF RECORDS(ClassQ) < 3 THEN
     IF Message('There were ZERO Routines found.||Maybe they are not in the %ProcedureRoutines nor %ProcRoutines?' & |
             '||If Routine code is in the %LocalProcedures embed, you can check|the box on the TXA tab to convert those embeds.' , |
             'No Routines Found',ICON:Asterisk,'Close|Check %LP Embeds') = 2 THEN SELECT(?Cfg:Do2LocalProc).
  END
!TODO Finish method  
  RETURN 1
!-------------------------
DDD.TxaAddChangesQEmbeds PROCEDURE()
EPX LONG      
    CODE 
    !If the TXA has a %LocalProcedures then adding more the original EMBED gets lost
    !TODO try moving Routines to %Local but that will be very disruptive to TXA for Compare ... Optional?
    DidNotMoveRoutineEmbeds=1
    IF Cfg:NoMoveRoutines THEN RETURN. 
    LOOP EPX=1 TO RECORDS(EmbedQ)
        GET(EmbedQ,EPX) 
        IF EmbedQ:Type = CbTxaEmbedType:LocalPro THEN
!           IF Message('This procedure already has EMBED %LocalProcedures ' & |
!                    '||so CANNOT change Routines to Local procedures or the ' & |
!                    '|existing embeds will be lost.' & |
!                    '||Eventually you should Copy/Paste Routines to Loc Procs.', |
!                    'Do2Class',,'Close|Do It Anyway')=1 THEN 
!              NoMoveRoutineEmbeds=1 
!              ?NoMoveRoutineEmbeds{PROP:Color}=COLOR:Yellow ;  DISPLAY       
              RETURN
!           END
!           BREAK 
        END
    END
    DidNotMoveRoutineEmbeds=0
    CLEAR(ChangeQ) 
    LOOP EPX=1 TO RECORDS(EmbedQ)
        GET(EmbedQ,EPX)        
        IF EmbedQ:Type = CbTxaEmbedType:Routine THEN  !'EMBED %ProcedureRoutines'  'EMBED %ProcRoutines'
            IF ChgQ:NLine=  'EMBED %LocalProcedures' THEN CYCLE.!Already in Q?
            CLEAR(ChangeQ)
            ChgQ:LineNo= EmbedQ:EmbedLnNo
            ChgQ:ALine=  EmbedQ:Embed
            ChgQ:NLine=  'EMBED %LocalProcedures'
            ADD(ChangeQ) 
        END
   END
   RETURN 
!-------------------------
DDD.LoadSxaQFromTxaQ PROCEDURE() !Load SXA queue of [Sections] from TXA 
TX  LONG   
Nxt BOOL
    CODE
    FREE(SxaQ)
    LOOP TX=1 TO RECORDS(TxaQ)
        GET(TxaQ,TX)
        IF TxaQ:TxtTxa[1]='[' THEN
           Nxt=1 ; SxaQ=TxaQ ; SxaQ:TxtUpr=''  
                   ADD(SxaQ)
        ELSIF Nxt 
           SxaQ:TxtUpr=CHOOSE(~SxaQ:TxtUpr,'',CLIP(SxaQ:TxtUpr) &' | ') & TxaQ:TxtTXA
         !  Nxt=0 ; 
         !  SxaQ:TxtUpr=TxaQ:TxtTXA
                      !SxaQ=TxaQ ; SxaQ:TxtUpr=SxaQ:TxtTXA ; SxaQ:TxtTXA='' ; ADD(SxaQ)
           PUT(SxaQ)
        END
    END 
    RETURN
!-------------------------
DDD.LoadSourceQFromTxaQ PROCEDURE()
EPX LONG   
SX  LONG   
Blanks  LONG   
    CODE
        LOOP SX=1 TO RECORDS(TXAQ) 
            GET(TxaQ,SX)
            TxaQ:StyleNo=StyleA:Txa:NotSource
            IF TxaQ:TxtTxa[1]='[' THEN TxaQ:StyleNo=StyleA:Txa:Bracket.
            PUT(TxaQ)
        END 
    FREE(SourceQ) ; CLEAR(SourceQ)
    LOOP EPX=1 TO RECORDS(EmbedQ)
        GET(EmbedQ,EPX)
        LOOP SX=EmbedQ:SourceBeg TO EmbedQ:SourceEnd  
            IF EmbedQ:CodeBeg AND SX < EmbedQ:CodeBeg THEN CYCLE.
            GET(TxaQ,SX)
            TxaQ:StyleNo=StyleA:Txa:Source
            PUT(TxaQ)
        END 

        CASE EmbedQ:Type
        OF CbTxaEmbedType:DATA    !e.g. 'EMBED %DataSection*' 'EMBED %DeclarationSection'
              IF ~CFG:Do2DataEmbeds AND EmbedQ:Type = CbTxaEmbedType:DATA THEN
                  EmbedQ:CodeLine1 ='--SKIP-DATA->' & EmbedQ:CodeLine1
                  PUT(EmbedQ)
                  CYCLE
              END
        OF CbTxaEmbedType:LocalPro
              IF ~Cfg:Do2LocalProc AND EmbedQ:Type = CbTxaEmbedType:LocalPro THEN
                  EmbedQ:CodeLine1 ='--SKIP-LocPro->' & EmbedQ:CodeLine1
                  PUT(EmbedQ)
                  CYCLE
              END
        OF CbTxaEmbedType:MethodData  !05/24/20 KSS testes
              IF ~Cfg:Do2MethodCode THEN
                 EmbedQ:CodeLine1 ='--SKIP-Method->' & EmbedQ:CodeLine1
                 PUT(EmbedQ)
                 CYCLE
              END

        OF CbTxaEmbedType:MethodCode  !05/24/20 KSS testes
            !Need to look in these for a DO ROUTINE we changed to DOO. 
            !May have scoping issues, i.e. Method Routine may be same name 
        END
        
        LOOP SX=EmbedQ:SourceBeg TO EmbedQ:SourceEnd  
            IF EmbedQ:CodeBeg AND SX < EmbedQ:CodeBeg THEN CYCLE.
            GET(TxaQ,SX)
          !  IF TxaQ:LenTxt <= 2 AND ~SUB(TxaQ:TxtTxa,1,TxaQ:LenTxt-2) THEN 
            IF TxaQ:LenWO2 <= 0 OR ~TxaQ:TxtTxa THEN 
                Blanks += 1 ; CYCLE  !1172
            END
            IF TxaQ:TxtTxa[1]='!' OR LEFT(TxaQ:TxtTxa,1)='!' THEN CYCLE. 
            SourceQ = TxaQ 
            SourceQ:EmbedType = EmbedQ:Type
            ADD(SourceQ)
        END
        SORT(SourceQ,SourceQ:LineNo)
    END 
    ?SourceQRecs{PROP:Text}=RECORDS(SourceQ) & ' Source lines out of ' & |
                RECORDS(TxaQ) &' TXA file lines, skipped ' & Blanks & ' blanks'              
    RETURN                
!------------------------- 
DDD.WriteTagImplicit    PROCEDURE(LONG TXQ_LineNo,*STRING CodeLn, <*LONG Out_LenWO2>)
    CODE
    ImpliQ:LineNo=TXQ_LineNo 
    GET(ImpliQ,ImpliQ:LineNo)
    IF ~ERRORCODE() AND ImpQ:StyleA<>StyleA:Routine THEN
        CodeLn=CLIP(CodeLn) &'  !#$"'
        IF ~OMITTED(Out_LenWO2) THEN Out_LenWO2=LenFastClip(CodeLn).
    END
    RETURN
!------------------------- 
DDD.WriteTXAFile PROCEDURE()  !Write the changed TXA by read TXAQ and merge ChangesQ lines
TXQ  &CbTxaLineQueueType  ! &= TxaCls.LinesQ so can TXQ:
B   LONG
ClsTxtFile  STRING(260) 
DataEmbedLineNo1   LONG     !Line with EMBED %DATASECTION
EmbedSectionLineNo LONG     !Line with [EMBED] to add Data after           
    CODE
    IF ~TxaSaveFile THEN RETURN.
    CLOSE(DosFile)
    DosFile{PROP:Name}=TxaSaveFile
    CREATE(DosFile)
    IF ~ERRORCODE() THEN SHARE(DosFile).
    IF ERRORCODE() THEN 
       Message('Unable to Create and Open TXA to Write with Do2Class|Error: ' & ErrorCode()&' '&Error()&'||' &  TxaSaveFile)
       RETURN
    END
    IF Cfg:WriteClass2Data THEN DO FindFirstDataEmbedRtn. 
    TXQ &= TxaCls.LinesQ
    LOOP LineX=1 TO RECORDS(TXQ)
        IF LineX=DataEmbedLineNo1 AND Cfg:WriteClass2Data THEN DO AddClassQToDataRtn.
        GET(TXQ,LineX)
        ChgQ:LineNo=TXQ:LineNo
        GET(ChangeQ,ChgQ:LineNo) 
        IF ~ERRORCODE() AND ~WriteNoChanges THEN
            IF Cfg:OmitDATAline THEN
               IF ChgQ:NLine[1]='!' AND LEFT(SUB(ChgQ:NLine,2,9999))='DATA' THEN CYCLE.
            END 
            IF Cfg:TagImplicitLines THEN DDD.WriteTagImplicit(TXQ:LineNo,ChgQ:NLine).
            DF:Block=ChgQ:NLine
            ADD(DosFile,LenFastClip(DF:Block))
        ELSE !No change
            DF:Block=TXQ:TxtTxa   !FYI TXQ:TxtTxa is &STRING of exact size
            IF Cfg:TagImplicitLines THEN DDD.WriteTagImplicit(TXQ:LineNo,DF:Block,TXQ:LenWO2).
            IF TXQ:LenWO2 THEN ADD(DosFile,TXQ:LenWO2).
        END
        DF:Block[1:2]='<13,10>' ; ADD(DosFile,2)
        IF LineX=EmbedSectionLineNo AND Cfg:WriteClass2Data AND ~DataEmbedLineNo1 THEN DO AddClassQAsNewDataEmbedRtn.        
    END
    CLOSE(DosFile) 
    Do WriteClass_FixdCode_ToTextFileRtn
    SETCLIPBOARD(TxaSaveFile)
    Message('TXA with DO to Class written to below file (name on clipboard):||     ' & LONGPATH(TxaSaveFile) & |
            '||Class Declaration in (for paste into Data)||     ' & ClsTxtFile)

WriteClass_FixdCode_ToTextFileRtn ROUTINE
    ClsTxtFile=CLIP(TxaSaveFile) &'.Class.TXT'
    DosFile{PROP:Name}=ClsTxtFile
    CREATE(DosFile)
    IF ~ERRORCODE() THEN SHARE(DosFile).
    IF ERRORCODE() THEN 
       Message('Unable to Create and Open Class TXT file to Write|Error: ' & ErrorCode()&' '&Error()&'||' &  ClsTxtFile)
       RETURN
    END
    B=SIZE(DF:Block)
    LOOP X=1 TO LenFastClip(FixdCode) BY B
         DF:Block=SUB(FixdCode,X,B)
         ADD(DosFile,LenFastClip(DF:Block))  !TODO could lose a space
    END
    CLOSE(DosFile)
    RUN('Notepad ' & CLIP(ClsTxtFile))
    EXIT 
FindFirstDataEmbedRtn ROUTINE 
    LOOP X=1 TO RECORDS(EmbedQ)
        GET(EmbedQ,X)
        IF EmbedQ:Type=CbTxaEmbedType:Data AND UPPER(EmbedQ:Embed)=EMBED_DATASECTION THEN  !'EMBED %DECLARATIONSECTION' was 'EMBED %DATASECTION'
           DataEmbedLineNo1 = EmbedQ:CodeBeg
           EXIT
        END 
    END
    !Did not find an EMBED_DATASECTION .. so find [EMBED]
    LOOP X=1 TO RECORDS(SxaQ)   !Loop my sections
        GET(SxaQ,X)
        IF SxaQ:TxtTxa='[EMBED]' THEN
           EmbedSectionLineNo = SxaQ:LineNo !; DB('[EMBED] Line=' & EmbedSectionLineNo ) 
           EXIT 
        END
    END

AddClassQAsNewDataEmbedRtn ROUTINE  !We are after [EMBED]. These was no %DataESection so make one
    ![EMBED]  has been written
    DDD.WriteDosLine(EMBED_DATASECTION)  !Now EMBED %DECLARATIONSECTION was EMBED %DataSection
    DDD.WriteDosLine('[DEFINITION]')
    DDD.WriteDosLine('[SOURCE]')
    DDD.WriteDosLine('PROPERTY:BEGIN')
!    DDD.WriteDosLine('PRIORITY 4000')  !leave out hoping IDE picks middle
    DDD.WriteDosLine('PROPERTY:END')
    DO AddClassQToDataRtn   !DOO CLASS 
    DDD.WriteDosLine('[END]')
    
AddClassQToDataRtn ROUTINE
    IF Cfg:WriteClassSorted THEN SORT(ClassQ, ClassQ:NameUPR, ClassQ:LineNo) .
    LOOP X=1 TO RECORDS(ClassQ)
        GET(ClassQ,X)
        DF:Block=CLIP(ClassQ:Declare) &'<13,10>'
        ADD(DosFile,LenFastClip(DF:Block))        
    END        
    DF:Block='<13,10,13,10>' ; ADD(DosFile,4)
    IF Cfg:TagImplicitLines AND RECORDS(ImpliQ) THEN
        DF:Block='![ ] Find and Fix Implicits Tagged on end of line with !#$" <13,10>'
        ADD(DosFile,LenFastClip(DF:Block))
    END
    IF Cfg:WriteClassSorted THEN SORT(ClassQ, ClassQ:LineNo) .
    EXIT
!---------------    
DDD.WriteDosLine PROCEDURE(STRING Block, BYTE Add1310=1)  !Write line to DosFile to output txa
LnB LONG,AUTO 
    CODE
    LnB=LenFastClip(Block)
    IF LnB THEN 
       DF:Block=Block[1 : LnB]
       ADD(DosFile,LnB)        
    END
    LOOP Add1310 TIMES
       DF:Block='<13,10>' ; ADD(DosFile,2)        
    END 
    RETURN
!===============================================================
DDD.ProcessSourceQToCodeQ  PROCEDURE()  !Load code from TXA SourceQ into CodeQ
ALeft   STRING(128)
LastRtnLine    LONG
Rtn1    LONG
Rtn2    LONG
Spc1    LONG
!Spc2    LONG 
Routine7    EQUATE(7)   !The word Routine is 7 bytes
SrcX  LONG
    CODE 
    FixdCode='' 
    FREE(CodeQ)  ; CLEAR(CodeQ); FREE(ProblmQ) ; FREE(ImpliQ) ; FREE(XRefQ)
!    LOOP LineX=1 TO ?OrigCode{PROP:LineCount}   !Original code is ROUTINE tab
!         CodeQ:LineNo  = LineX
!         CodeQ:ALine   = ?OrigCode{PROP:Line, LineX}     
    LOOP SrcX=1 TO RECORDS(SourceQ)
         GET(SourceQ,SrcX) 
         LineX=SourceQ:LineNo
         CodeQ:LineNo  = LineX
         CodeQ:ALine   = SourceQ:TxtTxaRef               
         !05/24/20 IF SourceQ:EmbedType=CbTxaEmbedType:Routine THEN
         CASE SourceQ:EmbedType
         OF CbTxaEmbedType:Routine    ; CodeQ:Type  = Type:InRoutine
         OF CbTxaEmbedType:MethodCode ; CodeQ:Type  = Type:InMethod
         ELSE
            CodeQ:Type  = 0
         END
         CodeQ:RtnPos  = 0 
         DDD.MakeCodeOnlyCoLine(CodeQ:ALine, CodeQ:CoLine )  
         
         ADD(CodeQ)
         ALeft = LEFT(CodeQ:CoLine) !Is Upper  ALeft = LEFT(CodeQ:ALine) ; ALeft = UPPER(ALeft)
         IF ALeft[1]='!' OR ~ALeft THEN ! OR ALeft[1]='?' THEN        !Comment or Blank line OR ?debug
            CodeQ:Type = Type:Comment
            PUT(CodeQ) 
            CYCLE
         END
         IF CodeQ:Type  = Type:InMethod THEN CYCLE.     !05/24/20 KSS
         IF ~CodeQ:ALine[1] THEN        !Not a Label    
!TODO Count OMIT and COMPILE         
            CASE ALeft  ! [1:5]  ALeft=CoLine so no exra chars
            OF 'DATA ' ! OROF 'DATA!' ; 
                CodeQ:Type=Type:DATAline ;  PUT(CodeQ) 
                !Type:RoutineDC  EQUATE(16)   !Routine with DATA CODE
                IF LastRtnLine
                   GET(CodeQ,LastRtnLine)
                   CodeQ:Type = Type:RoutineDC  !Routine with DATA CODE 
                   PUT(CodeQ) 
                END
                CYCLE
            OF 'CODE ' !OROF 'CODE!' ; 
                CodeQ:Type=Type:CODEline ;  PUT(CodeQ) ; CYCLE
            END !Case  
            
            CYCLE !No labels below here
         END  
         
         !Is this?  LABEL ROUTINE
         Rtn1 = INSTRING(' ROUTINE ',ALeft,1)   !Simple Find  TODO Bette Code     
         IF Rtn1=0 THEN CYCLE.
         Rtn1 += 1                      !ROUTINE
         Rtn2 = Rtn1 + Routine7         !01234567  
         Spc1 = INSTRING(' ',ALeft,1)               !End of Label at space 1
         
!no         IF ALeft[Rtn2]='!' THEN ALeft[Rtn2]=' '.   !Could be ROUTINE!comment
         IF ALeft[Rtn2]<>' ' THEN
            SETCLIPBOARD(ALeft)
            Message('Odd? Line ' & LineX & ' has ROUTINE @ ' & Rtn1 &' but no space @' & Rtn2 &'||' & ALeft)
            CYCLE            !Could be ROUTINEprocedure() 
         END   
         Spc1 = INSTRING(' ',ALeft,1)               !End of Label at space 1
         IF ALeft[Spc1 : Rtn1-1]<>' ' THEN !CYCLE.   !Want: Label   ROUTINE
            SETCLIPBOARD(ALeft)
            Message('Odd? Line ' & LineX & ' has ROUTINE @ ' & Rtn1 &', ends @' & Rtn2 &|
                     '|But not leading spaces from Spc1='& Spc1 &'||' & ALeft)
            CYCLE            !Could be ROUTINEprocedure() 
         END          
         !TODO - could be better
         CodeQ:Type = Type:Routine
         CodeQ:RtnPos = Rtn1          
         PUT(CodeQ)
         LastRtnLine = POINTER(CodeQ) !before TXA was --. LineX
    END
        !Debug look at just diff lines
        IF false
            LOOP LineX=RECORDS(CodeQ) TO 1 BY -1
                 GET(CodeQ,LineX)
                 IF UPPER(CLIP(CodeQ:ALine)) = CodeQ:CoLine OR CodeQ:Type=Type:Comment THEN 
                    DELETE(CodeQ)
                    ! CLEAR(CodeQ:CoLine).   
                 END    
            END 
        END !Dbg    
    RETURN

!################################################################### 
CaTxt.Init    PROCEDURE()
    CODE
    Self.Txt=''
CaTxt.Kill    PROCEDURE()   
    CODE
    CLEAR(Self.Txt) 
CaTxt.Add     PROCEDURE(*STRING Str2Add, SHORT CRLFs=1, BYTE Clip=1)
    CODE
    IF Clip THEN 
       SELF.Txt=SELF.Txt & CLIP(Str2Add) & CHOOSE(CRLFs<1,'','<13,10>') 
    ELSE   
       SELF.Txt=SELF.Txt & Str2Add & CHOOSE(CRLFs<1,'','<13,10>') 
    END
    IF CRLFs>1 THEN SELF.AddCR(CRLFs-1) .
    RETURN
CaTxt.Add     PROCEDURE(STRING Txt, SHORT CRLFs=1)   
    CODE
    SELF.Add(Txt,CRLFs,0)
    !SELF.Txt=SELF.Txt & Txt & CHOOSE(CRLFs<1,'','<13,10>')
CaTxt.AddCR   PROCEDURE(SHORT CRLFs=1)
    CODE
    SELF.Txt=SELF.Txt & ALL('<13,10>',2*CRLFs)
    RETURN     
!###################################################################
DDD.BuildFixdCode       PROCEDURE() 
ClsDeclHdr STRING('!----- Paste Class Declaration into Data Section ----- ')  
    CODE
    DDD.MakeClassQPretty()
    CaTxt.Init()
    DO AddTextClassQRtn 
    IF Cfg:TagImplicitLines AND RECORDS(ImpliQ) THEN
       CaTxt.Add('! [ ] Find and Fix Implicits Tagged on end of line with !#$" ')
       CaTxt.AddCR()
    END    
    IF IsTXAinput THEN  !TXA will be written so do not need the code
       CaTxt.Add('Click to Save to TXA, then import that TXA into your APP.')  
       CaTxt.Add('Output File: ' & TxaSaveFile,2) 
       CaTxt.Add('You will need to add to a DATA Section the above Class Declaration.') 
       CaTxt.Add('If a ' & p2ClassDot &'.Xxx() throws error "Unknown Procedure Label" you probably forgot to paste the Class Declaration.',2)
       CaTxt.Add('Do2Class: ' & COMMAND('0') ) 
       CaTxt.Add('Input File: ' & TxaLoadFile) 
       CaTxt.AddCR(2)
       SORT(ClassQ,Classq.NameUPR,ClassQ.LineNo)
       ClsDeclHdr='!-----  Class Declaration Sorted by Name ----- <13,10>'
       DO AddTextClassQRtn
       SORT(ClassQ,ClassQ.LineNo) 
    ELSE
        LOOP LineX=1 TO RECORDS(CodeQ)
            GET(CodeQ,LineX) 
            CaTxt.Add(CodeQ:ALine) 
        END
    END    
    FixdCode=CaTxt.Txt 
    CaTxt.Kill()
    RETURN
AddTextClassQRtn ROUTINE
    CaTxt.Add(ClsDeclHdr)  !; setclipboard(CaTxt.Txt) ; message('On CB is line 1 with: ' & ClsDeclHdr )  
    LOOP LineX=1 TO RECORDS(ClassQ)
        GET(ClassQ,LineX) 
        CaTxt.Add(CLassQ:Declare) 
    END  
    CaTxt.Add('!'& ALL('=',SIZE(ClsDeclHdr)),2 ) 
    EXIT
!----------------------------------------
DDD.MakeClassQPretty   PROCEDURE()  !Align the Name PROCEDURE() to look nice
FixIt   LONG         !Sample:
Spc LONG             !DefaultRtn          PROCEDURE() !Get default values from an INI file
Prc LONG             !SaveDefaultRtn      PROCEDURE()
IndMax  LONG(10)
TooBig  EQUATE(40)
Declare &STRING
    CODE
    LOOP FixIt=0 TO 1
        LOOP X=1 TO RECORDS(ClassQ)
            GET(ClassQ,X)  ; Declare &= ClassQ:Declare
            Spc=INSTRING(' ',Declare,1) 
            Prc=INSTRING('PROCEDURE',UPPER(Declare),1)
            IF Spc <=1 OR ~Prc THEN CYCLE.
            IF ~FixIt  THEN !Measure 1st
                IF Spc > IndMax AND Spc <= TooBig THEN IndMax=Spc.
            ELSE            !Align 2nd
                Declare=SUB(Declare,1,Spc) & ALL(' ',IndMax-Spc) &  LEFT(SUB(Declare,Spc,9999))
                PUT(ClassQ)                   
            END            
        END 
    END
    RETURN       
!----------------------------------------    
DDD.ProcessCodeQ2DoClass       PROCEDURE() 
InX         LONG,AUTO
!RNamX       LONG,AUTO
NmBeg       LONG,AUTO
NmEnd       LONG,AUTO
LnCode      LONG,AUTO
ClsNmMax    LONG           
InDATA      BOOL
PassNo      BYTE
CQX         LONG 
RtnNameNow  STRING(128)   
RtnNameQ    QUEUE,PRE(RtnQ)
LineNo          LONG            !RtnQ:LineNo
Name            STRING(128)     !RtnQ:Name
            END
    CODE 
    DO ChangeQSaveRtn
    FREE(ClassQ) ; CLEAR(ClassQ) ; CodeQProcLine1=0
    ClassQ:Declare = p2ClassName &' CLASS {20}  !Created ' & FORMAT(TODAY(),@d01) &' '& FORMAT(Clock(),@t3) &' by Do2Class by Carl Barnes'
    ClassQ:NameUPR='.' ; ClassQ.LineNo=0
    ADD(ClassQ) 
    LOOP PassNo=1 TO 2
       InDATA=0
       LOOP CQX= 1 TO RECORDS(CodeQ)
           GET(CodeQ,CQX) 
           LineX = CodeQ:LineNo
           IF CodeQ:Type=Type:Comment THEN CYCLE.
       
           CASE PassNo
           OF 1 !--------------- Pass 1 ----- Process Routine Declarations ------ Pass 1
                CASE CodeQ:Type
                OF Type:Routine   ; DO TypeRoutineRtn ; InDATA=0 !; CYCLE
                        CodeQ:ALine = CLIP(CodeQ:ALine) & '<13,10>  CODE'
                        PUT(CodeQ)
                        CYCLE
                OF Type:RoutineDC ; DO TypeRoutineRtn ; InDATA=0 ; CYCLE
                OF Type:DATAline  ; CodeQ:ALine='! ' & CodeQ:ALine ; PUT(CodeQ) ; InDATA=1 ; CYCLE
                OF Type:CODEline                                                ; InDATA=0 ; CYCLE
                OF Type:InMethod  ; InDATA=0 ; CYCLE !05/24/20 Routines in Method are NOT changed to DOO.
                END                 
                IF InDATA=1 THEN CYCLE.
                IF ~CodeQProcLine1 AND CodeQ:Type=0 THEN 
                    CodeQProcLine1=CodeQ:LineNo
                END
                !pass2--> DO ExitFixRtn
                !pass2--> DO DoFixRtn
    
           OF 2 !--------------- Pass 2 ----- Process CODE DOs ------ Pass 2
                CASE CodeQ:Type
                OF   Type:Routine   
                OROF Type:RoutineDC 
                     ClassQ:LineNo  = CodeQ:LineNo
                     GET(ClassQ,ClassQ:LineNo) ; IF ERRORCODE() THEN Message('Pass 2 Get ClsQ Err ').  
                     RtnNameNow=ClassQ:Name
                     InDATA=0 ; CYCLE                
                OF Type:DATAline  ; InDATA=1 ; CYCLE
                OF Type:CODEline  ; InDATA=0 ; CYCLE
                OF Type:InMethod  ; InDATA=0 ; DO DoFixRtn ; CYCLE !05/24/20 Look in Method for DO to DOO.
                END 
                IF InDATA=1 THEN CYCLE. 
                IF CodeQ:Type=Type:InRoutine THEN DO ReturnInRoutineCheckRtn.
                DO ExitFixRtn
                DO DoFixRtn
                DO ProblemCheckRtn
           END  !Case Pass No

       END !LOOP LineX
    END!LOOP PassNo
  
    CLEAR(ClassQ) ; 
    ClassQ:Declare = ALL(' ',LEN(p2ClassName)) &' END  !' & RECORDS(ClassQ)-1 & ' Routines Found'
    ClassQ:NameUPR='~' ; CLEAR(ClassQ.LineNo,1)
    ADD(ClassQ) 
    DO ClassQAlignRtn
    DO ChangeQCompareRtn   
    DO XRef2QBuildRtn   !Flip XRef for a Caller-Callee implosion
    RETURN 

  !IF IsTXAinput 
  ! Problem with TXA: Embed code in file is at Column 1, but is indented by the #EMBED
  !So I cannot assume 
  
DoFixRtn ROUTINE   !FInd DO ROutine and make DOO.Routine()
    LOOP              !012345
        InX=INSTRING(' DO ',CodeQ:CoLine,1)
        IF ~InX THEN
            IF IsTXAinput AND CodeQ:CoLine[1:3]='DO ' THEN !TXA Embeds have code in Column 1
               InX=0  !The space before DO is at 0
            ELSE
               BREAK
            END
        END                       ! 12345678
        IF SUB(CodeQ:CoLine,Inx,8)=' DO NOT ' THEN !English? Please do not drink bleach to prevent Corona 
           DB('?{9} DoFixRtn "DO" in English: ' & CodeQ:CoLine)
           CodeQ:CoLine[Inx : Inx+8-1]=' do not '   !Case sensitive, prevent
           CYCLE                   
        END
        LnCode=LEN(CLIP(CodeQ:CoLine)) !Assume space on end of CoLine
        NmBeg=0
        LOOP NmEnd = InX+3 TO LnCode+1          !Scan forward for next none Space
             IF NmEnd=LnCode+1 THEN EXIT.       !should never happen, hit the end
             IF ~NmBeg AND CLIP(CodeQ:CoLine[NmEnd])>' ' THEN
                NmBeg=NmEnd
               ! CYCLE
             END    
             IF CodeQ:CoLine[NmEnd+1] <=' ' THEN BREAK.
        END

     IF DbgIt
            DB('-{20} DoFixRtn<13,10>DoFixRtn InX=' & InX & '  LnCode=' & LnCode &'  NmBeg=' & NmBeg &'  NmEnd=' & NmEnd & |
                    '<13,10>Part 1:' & SUB(CodeQ:ALine ,1,InX) & |   !
                    '<13,10>Part 2:' & CLIP(SUB(CodeQ:ALine ,NmEnd-NmBeg+1,9999)) & |
                    '<13,10>Name: NmBeg=' & NmBeg &' NmEnd=' & NmEnd & ' Name:' & SUB(CodeQ:ALine ,NmBeg,NmEnd-NmBeg+1) & |
                    '<13,10>DoNm: ' & p2ClassDot & SUB(CodeQ:ALine ,NmBeg,NmEnd-NmBeg+1) &'()' &|
                    '<13,10>Line:' & CLIP(CodeQ:ALine) & |
                    '-{20}<13,10>')
     END
        IF ~NmBeg OR ~NmEnd OR NmBeg>NmEnd THEN 
            DB('DoFixRtn Should NOT happen  IF ~NmBeg OR ~NmEnd OR NmBeg>NmEnd  EXIT')
            EXIT   !should not happen 
        END
        
        ClassQ:NameUPR = UPPER(SUB(CodeQ:ALine ,NmBeg,NmEnd-NmBeg+1))
        GET(ClassQ,ClassQ:NameUPR)  !If Routine not see then must be TPL e.g. RefreshWindow
        IF ERRORCODE() THEN 
           CodeQ:CoLine[Inx+1 : Inx+2]='do'                               
           DB('     N/A ClassQ:' & CLIP(ClassQ:NameUPR) &'<13,10>     N/A CoLine:' & CLIP(CodeQ:CoLine) )
           PUT(CodeQ)
           CYCLE
        END
        CodeQ:ALine =CHOOSE(InX=0,'',SUB(CodeQ:ALine ,1,InX)) & |   ! InX=0 =DO in column 1 Embed
                     CHOOSE(Cfg:SELFinClass AND CodeQ:Type=Type:InRoutine,'SELF.',p2ClassDot) & |
                         SUB(CodeQ:ALine ,NmBeg,NmEnd-NmBeg+1) &'()'& |
                         SUB(CodeQ:ALine ,NmEnd+1,9999)
        DDD.MakeCodeOnlyCoLine(CodeQ:ALine,CodeQ:CoLine)
        !CodeQ:CoLine=SUB(CodeQ:CoLine,1,InX) & p2ClassDot & SUB(CodeQ:CoLine,RNamX,9999) 
        !stop('len(CoLine=' & Len(CodeQ:CoLine)) !< this proves no need to CLIP C string
        PUT(CodeQ)
        
        CLEAR(XRefQ) 
        XRQ:Name    = CHOOSE(CodeQ:Type=Type:InRoutine,RtnNameNow,'*PROCEDURE*')
        XRQ:Level   = 2
        XRQ:LineNo  = CodeQ:LineNo
        XRQ:ALine   = LEFT(CodeQ:ALine)
        XRQ:UprName = ClassQ:NameUPR 
        XRQ:Callee  = ClassQ:Name     !Who are we DO'in
        XRQ:Caller  = XRQ:Name        !Routine name we are inside is caller
        ADD(XRefQ,XRQ:UprName,XRQ:Level,XRQ:LineNo)        
    END 
    
ExitFixRtn ROUTINE 
    LOOP             !012345
        InX=INSTRING(' EXIT ',' '&CodeQ:CoLine,1)  
!           IF LEFT(CodeQ:ALine)='EXIT' THEN
!                 Message('ExitFixRtn|LineX=' & LineX &'|Exit InX=' & InX & |
!                         '|CoLine Len=' & LEN(CodeQ:CoLine) & |
!                         '|CoLine=' & CodeQ:CoLine )
!           END
        IF ~InX THEN BREAK. 
        InX -= 1             !  Instr +1 ' '&
        CodeQ:ALine =SUB(CodeQ:ALine ,1,InX) &'RETURN' & SUB(CodeQ:ALine ,InX+5,9999)
        CodeQ:CoLine=SUB(CodeQ:CoLine,1,InX) &'RETURN' & SUB(CodeQ:CoLine,InX+5,9999)
        PUT(CodeQ)
    END

ReturnInRoutineCheckRtn ROUTINE 
    InX=INSTRING(' RETURN',' ' & CodeQ:CoLine,1)  !Note leading  ' ' so Inx is +1
    IF Inx AND INLIST(CodeQ:CoLine[Inx-1+7],' ','(') THEN  ! Can be RETURN X  or  RETURN(X)
       CodeQ:ALine =' ? Return was in Routine ?<13,10>' & CodeQ:ALine
       CodeQ:StyleA=StyleA:Problem ; Cnts:Problem += 1
       PUT(CodeQ) 
       EXIT
    END

    InX=INSTRING('PROCEDURERETURN',CodeQ:CoLine,1)  !Legacy Do ProcedureReturn cannot be done from Class Method
    IF Inx THEN  ! Can be RETURN X  or  RETURN(X)
       CodeQ:ALine =' ? Return was in Routine ?<13,10>' & CodeQ:ALine
       CodeQ:StyleA=StyleA:Problem ; Cnts:Problem += 1
       PUT(CodeQ) 
       EXIT
    END

    InX=INSTRING(' OMITTED(',' ' & CodeQ:CoLine,1)            !FYI Omitted() Paramater, NOT Omit() code
!!!    DB('Omitted InX=' & CodeQ:LineNo &' - '& CodeQ:CoLine )
    IF Inx THEN 
       CodeQ:ALine =' ? Omitted() was in Routine ?<13,10>' & CodeQ:ALine
       CodeQ:StyleA=StyleA:Problem  ; Cnts:Problem += 1
       PUT(CodeQ)
       EXIT
    END 
ProblemCheckRtn ROUTINE
    IF CodeQ:StyleA THEN EXIT.    
    InX=INSTRING(' OMIT(',' ' & CodeQ:CoLine,1)
    IF ~Inx THEN InX=INSTRING(' COMPILE(',' ' & CodeQ:CoLine,1).
    IF Inx THEN
       CodeQ:StyleA=StyleA:Omit     !May or may not be problem, like implicit
       PUT(CodeQ)
       EXIT
    END
        
TypeRoutineRtn ROUTINE 
    CLEAR(XRefQ)
    XRQ:ALine   = CodeQ:ALine

    !CodeQ:RtnPos >|123456           |< CodeQ:RtnPos 
    !RoutName      ROUTINE
    CodeQ:ALine = SUB(CodeQ:ALine,1,CodeQ:RtnPos-1) & 'PROCEDURE() ' & LEFT(SUB(CodeQ:ALine,CodeQ:RtnPos+7,9999))
    CLEAR(ClassQ)
    ClassQ:Name    = SUB(CodeQ:ALine,1,CodeQ:RtnPos-1)
    ClassQ:NameUPR = UPPER(ClassQ:Name)
    ClassQ:LineNo  = CodeQ:LineNo ! LineX
    ClassQ:Declare = CodeQ:ALine        !Save the above for Declare    
    ADD(ClassQ)
    
    InX=INSTRING(' ',ClassQ:Declare,1) ; IF ClsNmMax < InX THEN ClsNmMax = InX.

    CodeQ:ALine=p2ClassDot & CodeQ:ALine    !Make DOO.RoutName
    PUT(CodeQ)    

    XRQ:Name    = ClassQ:Name
    XRQ:Level   = 1
    XRQ:LineNo  = ClassQ:LineNo
!    XRQ:ALine  = CodeQ: ALine       !set above
    XRQ:Callee  =  ClassQ:Name
    XRQ:Caller  = '' ! ClassQ:Name
    XRQ:UprName = UPPER(XRQ:Name)
    ADD(XRefQ,XRQ:UprName,XRQ:Level,XRQ:LineNo)
    EXIT

ClassQAlignRtn  ROUTINE
    LOOP LineX=2 TO RECORDS(ClassQ)-1
        GET(ClassQ,LineX) 
        InX=INSTRING(' ',ClassQ:Declare,1) 
        IF InX < ClsNmMax THEN 
           ClassQ:Declare=SUB(ClassQ:Declare,1,InX) & ALL(' ',ClsNmMax-InX) & SUB(ClassQ:Declare,InX+1,9999)
        END
        PUT(ClassQ)
    END

ChangeQSaveRtn ROUTINE
    FREE(ChangeQ) ; CLEAR(ChangeQ)
    LOOP LineX= 1 TO RECORDS(CodeQ)
        GET(CodeQ,LineX)
        ChangeQ :=: CodeQ
        ADD(ChangeQ)
    END
ChangeQCompareRtn ROUTINE   !CodeQ was saved to
    DATA   
AddNextRtn2Impli  BYTE   !doing in reverse
    CODE
    
    FREE(ImpliQ) ; CLEAR(ImpliQ)
    LOOP LineX=RECORDS(CodeQ) TO 1 BY -1
        GET(CodeQ,LineX) 
        ChgQ:LineNo = CodeQ:LineNo
        GET(ChangeQ,ChgQ:LineNo)
        !IF ERRORCODE() THEN  
        !    TODO Add
        !END 
       
        IF CodeQ:ALine <> ChgQ:ALine THEN
           ChgQ:ALine = ChgQ:ALine       !LEFT(ChgQ:ALine)
           ChgQ:NLine = CodeQ:ALine      !LEFT(CodeQ:ALine)
           PUT(ChangeQ)
           IF ~CodeQ:StyleA THEN 
               CASE CodeQ:Type
               OF Type:Routine OROF Type:RoutineDC  
                   CodeQ:StyleA=StyleA:Routine
                   IF AddNextRtn2Impli THEN 
                      ImpliQ=CodeQ 
                      ADD(ImpliQ, ImpQ:LineNo)
                      AddNextRtn2Impli=0 
                   END
               ELSE
                   CodeQ:StyleA=StyleA:Change
               END    
           END
           PUT(CodeQ)
        ELSE
           DELETE(ChangeQ)
        END 
        IF MATCH(UPPER(CodeQ:CoLine),'[_A-Z][_A-Z0-9]*[#"$]', Match:Regular) THEN 
           ImpliQ=CodeQ
           ImpQ:CoLine=CLIP(ChgQ:ALine)
           ADD(ImpliQ, ImpQ:LineNo)
           IF ~CodeQ:StyleA THEN CodeQ:StyleA=StyleA:Implicit ; PUT(CodeQ).
           AddNextRtn2Impli=1 
        ELSE
           CASE CodeQ:StyleA     !Put these in Implicits to draw attenention
           OF   StyleA:Implicit  !Line got tagged implicit elsewhere
          ! OROF StyleA:Problem 
                   ImpliQ=CodeQ
                   ImpQ:CoLine=CLIP(ChgQ:ALine)
                   IF ImpQ:StyleA=StyleA:Implicit THEN ImpQ:StyleA=0.
                   ADD(ImpliQ, ImpQ:LineNo)
                   AddNextRtn2Impli=1 
           END
        END 
    END 
    !---------- Build OMIT Q finding CodeQ lines
    AddNextRtn2Impli=0
    FREE(OmitQ)
    LOOP LineX=RECORDS(CodeQ) TO 1 BY -1
        GET(CodeQ,LineX) 
        ChgQ:LineNo = CodeQ:LineNo
        GET(ChangeQ,ChgQ:LineNo)
        IF CodeQ:StyleA=StyleA:Routine AND AddNextRtn2Impli THEN 
           OmitQ=CodeQ 
           ADD(OmitQ, OmtQ:LineNo)
           AddNextRtn2Impli=0 
        ELSIF CodeQ:StyleA=StyleA:OMIT THEN
           OmitQ=CodeQ 
           OmtQ:StyleA=0
           OmtQ:CoLine=CLIP(ChgQ:ALine)
           ADD(OmitQ, OmtQ:LineNo)
           AddNextRtn2Impli=1
           TxaQ:LineNo=CodeQ:LineNo     !In TXA view color OMIT to make easy to see
           GET(TxaQ,TxaQ:LineNo)
           IF ~ERRORCODE() THEN
              TxaQ:StyleNo=StyleA:Omit
              PUT(TxaQ)
           END

        ELSIF CodeQ:StyleA=StyleA:Problem OR CodeQ:StyleA=StyleA:Implicit THEN
           TxaQ:LineNo=CodeQ:LineNo     !In TXA view color OMIT to make easy to see
           GET(TxaQ,TxaQ:LineNo)
           IF ~ERRORCODE() THEN
              TxaQ:StyleNo=CodeQ:StyleA
              PUT(TxaQ)
           END

        END 
    END
!#########
    !---------- Build ProblmQ finding CodeQ lines
    AddNextRtn2Impli=0
    FREE(ProblmQ)
    LOOP LineX=RECORDS(CodeQ) TO 1 BY -1
        GET(CodeQ,LineX) 
        ChgQ:LineNo = CodeQ:LineNo
        GET(ChangeQ,ChgQ:LineNo)
        IF CodeQ:StyleA=StyleA:Routine AND AddNextRtn2Impli THEN 
           ProblmQ=CodeQ 
           ADD(ProblmQ, ProbQ:LineNo)
           AddNextRtn2Impli=0 
        ELSIF CodeQ:StyleA=StyleA:Problem  THEN
           ProblmQ=CodeQ 
           ProbQ:StyleA=0
           ProbQ:CoLine=CLIP(ChgQ:ALine)
           ADD(ProblmQ, ProbQ:LineNo)
           AddNextRtn2Impli=1
        END 
    END !Loop LineX CodeQ
    
    EXIT

XRef2QBuildRtn ROUTINE    !Take XRef that is Class calls Method, and flip to Method Calls Class for a Caller-Callee implosion
    FREE(XRef2Q) 
    CodeQ:LineNo=CodeQProcLine1
    GET(CodeQ,CodeQ:LineNo)    ;    IF ERRORCODE() THEN GET(CodeQ,1).
    CLEAR(XRef2Q)
    XR2Q:Name    = '*PROCEDURE*'
    XR2Q:Level   = 1
    XR2Q:LineNo  = CodeQ:LineNo
    XR2Q:ALine   = CLIP(TxaCls:ProcName) &'   Procedure()'
    XR2Q:Callee  =  ClassQ:Name
    XR2Q:Caller  = '' ! ClassQ:Name
    XR2Q:UprName = UPPER(XR2Q:Name)
    ADD(XRef2Q,XR2Q:UprName,XR2Q:Level,XR2Q:LineNo)
    
    LOOP X=1 TO RECORDS(XRefQ)
        GET(XRefQ,X)
        XRef2Q = XRefQ
        IF XRQ:Level=1 THEN  
        ELSE !IF XRQ:Level=2 THEN   
           XR2Q:Name      =  XRQ:Callee
           XR2Q:Level     =  2  
           XR2Q:UprName   =  UPPER(XRQ:Caller)
        END      
        ADD(XRef2Q,XR2Q:UprName,XR2Q:Level,XR2Q:LineNo)
    END        
    
!--------------------------------------------------   
DDD.ParseOrigCodeToSourceQ PROCEDURE()  !Put TEXT OrigCode lines into SourceQ to have sommon code with TXA
    CODE
    FREE(SourceQ)  ; CLEAR(SourceQ)  
    LOOP LineX=1 TO ?OrigCode{PROP:LineCount}
         CLEAR(SourceQ)
         SourceQ:LineNo    = LineX
         SourceQ:TxtTxa    = ?OrigCode{PROP:Line, LineX} 
         SourceQ:TxtUpr    = UPPER(SourceQ:TxtTxa)
         SourceQ:LenWO2    = LenFastClip(SourceQ:TxtTxa)
        ! SourceQ:PosBeg    =     !  LONG      !SourceQ:PosBeg                      
        ! SourceQ:PosEnd    =     !  LONG      !SourceQ:PosEnd                      
         SourceQ:TxtTxaRef &= SourceQ:TxtTxa   
         SourceQ:TxtUprRef &= SourceQ:TxtUprRef
         ADD(SourceQ)
    END
    SORT(SourceQ,SourceQ:LineNo)
!-------------------------   
DDD.ProcessOrigCodeToCodeQ  PROCEDURE()  !Load code from TEXT OrigCode into CodeQ
Routine7    EQUATE(7)   !The word Routine is 7 bytes
    CODE
  DDD.ParseOrigCodeToSourceQ()
  DDD.ProcessSourceQToCodeQ()  !Read SourceQ to find chnage and save into CodeQ  
  RETURN

DDD.MakeCodeOnlyCoLine  PROCEDURE(*STRING ALine, *CSTRING CoLine) !CodeOnly = UPPER w/o 'String Literals' !Comments
LenLn       LONG
InQuote     BOOL
    CODE
    LenLn = LenFastClip(ALine) 
    IF ~LenLn THEN
        CoLine=''
        RETURN
    END
    CoLine = UPPER(ALine[1 : LenLn] & ' ')     !Leave trailing space
    LOOP X=1 TO LenLn
         IF VAL(CoLine[x])=39 THEN 
            InQuote=1-InQuote
            CYCLE
         END   
         IF InQuote THEN CoLine[x]=' '.      !Keep nothig between quotes
         CASE CoLine[x] 
         OF CHR(9)       ; CoLine[x]=' '
         OF '.' OROF ';' ; CoLine[x]=' '     !blank . ; to avoid no space aftef THEH EXIT. 
         OF '!' OROF '|'                     !Comment or Continue ends line
            CoLine[x]=' '
            CoLine=SUB(CoLine,1,X)
            BREAK
         END
    END
    RETURN
!--------------------------------------------- 
DDD.ConfigLdSv1 PROCEDURE(BYTE LdSv,STRING EntName, *? EntValue)
    CODE
    IF LdSv=1 THEN 
        EntValue=GETINI('Cfg',EntName,EntValue,CfgFileIni) 
    ELSE    
        PUTINI('Cfg',EntName,EntValue,CfgFileIni) 
    END
    RETURN    
DDD.ConfigLoadSave PROCEDURE(BYTE LdSv)
    CODE
    SELF.ConfigLdSv1(LdSv,'TxaFile',    TxaLoadFile)
    SELF.ConfigLdSv1(LdSv,'OmitDATA',   Cfg:OmitDATAline)
    SELF.ConfigLdSv1(LdSv,'WriteClass', Cfg:WriteClass2Data)
    SELF.ConfigLdSv1(LdSv,'WriteClsSorted', Cfg:WriteClassSorted)
!no    SELF.ConfigLdSv1(LdSv,'LocalProEmb',Cfg:Do2LocalProc)
    SELF.ConfigLdSv1(LdSv,'NoMoveRtnEmb',Cfg:NoMoveRoutines)
!no    SELF.ConfigLdSv1(LdSv,'DoDataEmb',  Cfg:Do2DataEmbeds)
    SELF.ConfigLdSv1(LdSv,'DoSELF',     Cfg:SELFinClass)
    SELF.ConfigLdSv1(LdSv,'TagImplicitLines', Cfg:TagImplicitLines)

    DooClassName=LEFT(DooClassName) ; IF ~DooClassName THEN DooClassName='DOO'.
    p2ClassName=CLIP(DooClassName) ; p2ClassDot=p2ClassName &'.' ! e.g. DOO / DOO. 
    RETURN
!---------------------------------------------
DDD.CodeQBackNext  PROCEDURE(SHORT BackNext, LONG FindStyleA=0)
Wait4NoStyle LONG    
C1 LONG    
R1 LONG    
R2 LONG
YOrg LONG
Itms LONG    
    CODE
    IF BackNext=0 THEN BackNext=CHOOSE(~BAND(KEYSTATE(),CtrlKeyPressed),1,-1).
    C1=CHOICE(?LIST:CodeQ)  
    GET(CodeQ,C1) ; IF CodeQ:StyleA THEN Wait4NoStyle=1.
    R1=C1+BackNext
    R2=CHOOSE(BackNext=-1,1,RECORDS(CodeQ)) 
    LOOP X=R1 TO R2 BY BackNext
        GET(CodeQ,X)
        IF Wait4NoStyle AND CodeQ:StyleA THEN CYCLE.       
        IF CodeQ:StyleA AND ~Wait4NoStyle THEN 
           IF FindStyleA AND CodeQ:StyleA<>FindStyleA THEN CYCLE.
           SELECT(?LIST:CodeQ,X)
           YOrg=?LIST:CodeQ{PROP:YOrigin} ; Itms=?LIST:CodeQ{PROP:Items}
           IF (X>Itms AND X-YOrg < 4) |                 !Is this almost TOP
           OR (X>Itms-5 AND X-YOrg > Itms - 4) THEN
              ?LIST:CodeQ{PROP:YOrigin}=X-3
           END
           BREAK
        ELSIF ~CodeQ:StyleA THEN 
           Wait4NoStyle=0
        END
    END
    RETURN 
!-------------------- 
DDD.StyleSetCodeQLists  PROCEDURE()
    CODE
  DDD.StyleSetCodeQList(StyleA:Routine,  COLOR:Black, 080C0FFH, 00AAD5FFh, COLOR:Black )   !Orange 
  DDD.StyleSetCodeQList(StyleA:Change,   COLOR:Navy,  05DFFFFH ) !080FFFFH)   !Yellowish
  DDD.StyleSetCodeQList(StyleA:Problem,  COLOR:White, COLOR:Red)
  DDD.StyleSetCodeQList(StyleA:Implicit, COLOR:Black, 0C0FFC0H)  !COLOR:Orange)  !
  DDD.StyleSetCodeQList(StyleA:Omit,     COLOR:White, COLOR:Fuchsia, COLOR:White, COLOR:Fuchsia)
  DDD.StyleSetCodeQList(StyleA:Txa:NotSource,  Color:Black, 0efefefh)
  DDD.StyleSetCodeQList(StyleA:Txa:Bracket,    Color:Black, 0f3f3f3h,             ,              , FONT:Bold)
  DDD.StyleSetCodeQList(StyleA:Txa:Source,     Color:Black, COLOR:InfoBackground)

DDD.StyleSetCodeQList  PROCEDURE(LONG StyleNo, LONG TextClr, LONG BackClr, LONG SelTextClr=-1, LONG SelBackClr=-1, LONG FontStyle=-1)
FEQ LONG
    CODE
    FEQ=?LIST:CodeQ  ; DO SetRtn
    FEQ=?LIST:CodeOU ; DO SetRtn
    FEQ=?LIST:ProblmQ ; DO SetRtn
    FEQ=?LIST:ImpliQ ; DO SetRtn
    FEQ=?LIST:OmitQ  ; DO SetRtn
    FEQ=?LIST:TxaQ   ; DO SetRtn
    CASE StyleNo
    OF StyleA:Routine  ; FEQ=?Sample:Routine 
    OF StyleA:Change   ; FEQ=?Sample:Change  
    OF StyleA:Problem  ; FEQ=?Sample:Problem 
    OF StyleA:Implicit ; FEQ=?Sample:Impli
    OF StyleA:Omit     ; FEQ=?Sample:Omit     
    ELSE ; RETURN
    END 
    FEQ{PROP:FontColor}=TextClr 
    FEQ{PROP:Background}=BackClr 
  
SetRtn ROUTINE
   FEQ{PROPSTYLE:TextColor,StyleNo}=TextClr 
   FEQ{PROPSTYLE:BackColor,StyleNo}=BackClr
   IF SelTextClr <> -1 THEN FEQ{PROPSTYLE:TextSelected,StyleNo}=SelTextClr. 
   IF SelBackClr <> -1 THEN FEQ{PROPSTYLE:BackSelected,StyleNo}=SelBackClr. 
   IF FontStyle <> -1  THEN FEQ{PROPSTYLE:FontStyle   ,StyleNo}=FontStyle. 
   
!--------------------
DDD.Font10ptChange  PROCEDURE()
S LONG 
F LONG(0)
    CODE  
    LOOP ; F=0{PROP:NextField,F} ; IF ~F THEN BREAK.
         !IF INLIST(F{PROP:Type},CREATE:List,CREATE:Text) THEN F{PROP:FontSize}=Font10pt.
         CASE F{PROP:Type}
         OF CREATE:Text ; F{PROP:FontSize}=Font10pt
         OF CREATE:List ; F{PROP:FontSize}=Font10pt 
            LOOP S=1 TO StyleA:LAST
                IF F{PROPSTYLE:FontSize,S}>5 THEN F{PROPSTYLE:FontSize,S}=Font10pt.
            END
         END                        
    END !Loop   
    RETURN
!--------------------
DDD.RightClickLine PROCEDURE(LONG LineNo)
  CODE
  IF KEYCODE()<>MouseRight THEN RETURN.
  SETKEYCODE(0)
  X=POPUP('Line '& LineNo &' Jump to Tab' & |  !#1                                
            '|-' & |                                        
            '|TXA Q Load' & |   !#2    ?TabTxaLoad                               
            '|Source Embed Q' & |      !#3    ?TabTxaEmbeds                               
            '|Source Lines Q' & |     !#4    ?TabTxaSource                             
            '|Changes' & |      !#5    ?TabCChangeQ                               
            '|~Class Q' & |      !#6    ?TabClassQ                                 
            '|Code Q' & |       !#7    ?TabCodeQ                                   
            '|Code Over-Under' & |   !#8    ?TabCodeOU                              
            '|Implicit #$"' & | !#9    ?TabImpliQ                            
            '')                                             
                                                            
  CASE X                                                  
  !OF 0 TO 1 ; RETURN ! # 1  Line # Jump
  OF  2                              ! # 2  TXA Load       ?TabTxaLoad
        TxaQ:LineNo=LineNo ; GET(TxaQ,TxaQ:LineNo) ; SELECT(?List:TxaQ,POINTER(TxaQ))    
  OF  3                              ! # 3  Embeds Q       ?TabTxaEmbeds 
        LOOP X=1 TO RECORDS(EmbedQ)-1 ; GET(EmbedQ,X)  
            IF EmbedQ:SourceBeg >= LineNo  THEN BREAK. 
        END ; SELECT(?LIST:EmbedQ,X)
  OF  4                              ! # 4  Source Q       ?TabTxaSource
        SourceQ:LineNo=LineNo 
        GET(SourceQ,SourceQ:LineNo) 
        IF ERRORCODE() THEN GET(SourceQ,POSITION(SourceQ)).  !Find next closest SourceQ, must have SORT(
        IF ~ERRORCODE() THEN SELECT(?List:SourceQ,POINTER(SourceQ)).
  OF  5                              ! # 5  Changes        ?TabCChangeQ
        ChangeQ:LineNo=LineNo 
        GET(ChangeQ,ChangeQ:LineNo) 
        IF ERRORCODE() THEN GET(ChangeQ,POSITION(ChangeQ)).
        IF ~ERRORCODE() THEN SELECT(?LIST:ChangeQ,POINTER(ChangeQ)) .
        ?LIST:ChangeQ:SbS{PROP:Selected}=?LIST:ChangeQ{PROP:Selected}
  OF  6                              ! # 6  Class Q        ?TabClassQ           
  OF  7                              ! # 7  Code Q         ?TabCodeQ
        CodeQ:LineNo=LineNo ; GET(CodeQ,CodeQ:LineNo) ; IF ~ERRORCODE() THEN SELECT(?List:CodeQ,POINTER(CodeQ)).
  OF  8                              ! # 8  CoLINE O/U     ?TabCodeOU           
        CodeQ:LineNo=LineNo ; GET(CodeQ,CodeQ:LineNo) ; IF ~ERRORCODE() THEN SELECT(?TabCodeOU,POINTER(CodeQ)).
  OF  9                              ! # 9  Implicit #$"   ?TabImpliQ           
        ImpliQ:LineNo=LineNo ; GET(ImpliQ,ImpliQ:LineNo) ; IF ~ERRORCODE() THEN SELECT(?LIST:ImpliQ,POINTER(ImpliQ)).                             
  END !CASE Popup
  RETURN
!--------------------
DDD.SelectSourceQLine PROCEDURE(LONG SourceQ_LineNo)!,BOOL,PROC
RetOk BOOL
    CODE  
    IF KEYCODE()=MouseLeft2 THEN 
       SETKEYCODE(0)
       SourceQ:LineNo=SourceQ_LineNo 
       GET(SourceQ,SourceQ:LineNo) 
       IF ~ERRORCODE() THEN 
           RetOk=1
           SELECT(?List:SourceQ,POINTER(SourceQ)) 
       END
    END
    RETURN RetOk
!--------------------
DDD.ChangesCodeCopy PROCEDURE()
CB ANY
    CODE
    CB='!Lines with Changes from ' & CLIP(TxaCls.ProcName) & |
       '<13,10>Orignal line / Changed Line. Line# is TXA Line' 
    LOOP X=1 TO RECORDS(ChangeQ) ; GET(ChangeQ,X)
         CB=CB&'<13,10>!- '& ChgQ:LineNo &' -{20}<13,10>' & CLIP(ChgQ:ALine) &'<13,10>' & CLIP(ChgQ:ALine)
    END
    SETCLIPBOARD(CB)
    RETURN   
!        ChangeQ   QUEUE,PRE(ChgQ)       !put all in Q to make easier to figure out
!    LineNo      LONG                !ChgQ:LineNo
!    !Type        SHORT              !ChgQ:Type
!    ALine       STRING(2048)        !ChgQ:ALine
!    NLine       STRING(2048)        !ChgQ:NLine 
!--------------------
DDD.ImplicitCodeCopy PROCEDURE()
CB ANY
L  BYTE
    CODE
    L=POPUPunder(?CopyImplicitsBtn,'Copy Modified Lines|Copy Original Lines') ; IF ~L THEN RETURN.
    CB='!Lines with Implicits from ' & CLIP(TxaCls.ProcName) 
    LOOP X=1 TO RECORDS(ImpliQ) ; GET(ImpliQ,X)
         CB=CB&'<13,10>'& CLIP(CHOOSE(L,ImpQ:ALine,ImpQ:CoLine))
    END
    SETCLIPBOARD(CB)
    RETURN
!----------------------
DDD.TabCountQ PROCEDURE()
    CODE
    DDD.TabCountQ(?TabChangeQ, ChangeQ)
    DDD.TabCountQ(?TabProblmQ, ProblmQ) 
    DDD.TabCountQ(?TabImpliQ, ImpliQ) 
    DDD.TabCountQ(?TabOmitQ,  OmitQ) 
    DDD.TabCountQ(?TabClassQ, ClassQ, -2) 
    RETURN
DDD.TabCountQ PROCEDURE(LONG TabFEQ, QUEUE Q, LONG Adjust=0) !Put (#) Count on Tab
TabNm   PSTRING(32),AUTO
R       LONG,AUTO
    CODE
    TabNm=TabFEQ{'UPROP_TabNm'}
    IF ~TabNm THEN 
        TabNm=CLIP(TabFEQ{PROP:Text})
        TabFEQ{'UPROP_TabNm'}=TabNm
    END 
    R=RECORDS(Q)+Adjust
    TabFEQ{PROP:Text}=TabNm & CHOOSE(R<1,'',' (' & R & ') ')
    RETURN 
!------------
DDD.ResizeWindow PROCEDURE(BYTE IsOpen=0) 
ListXr1    LONG(?LIST:XRefQ)  !Left
ListXr2    LONG(?LIST:XRef2Q) !RIght 
Xr1Wd      LONG,STATIC
Xr2Wd      LONG,STATIC
Xr2RMar    LONG,STATIC    
WnWd       LONG,STATIC
WW  LONG,AUTO
WH  LONG,AUTO
LX  LONG,AUTO
LW  LONG,AUTO
    CODE
    GETPOSITION(0,,,WW,WH)
    IF IsOpen THEN
        WnWd=WW
        GETPOSITION(ListXr1,  ,,Xr1Wd)
        GETPOSITION(ListXr2,LX,,Xr2Wd) ; Xr2RMar=WW-(LX+Xr2Wd)
        0{PROP:MinWidth} = WW / 2
        0{PROP:MinHeight} = WH / 2
        RETURN
    END
    LW=Xr2Wd+(WW-WnWd)/2 ; ListXr2{PROP:Width}=LW ; ListXr2{PROP:XPos}=WW-LW-Xr2RMar
    LW=Xr1Wd+(WW-WnWd)/2 ; ListXr1{PROP:Width}=LW
    RETURN  
!------------
DDD.TreeExpandContract PROCEDURE(QUEUE TreeQ, *LONG Level, BYTE Contract=0)
Prv LONG
    CODE
    LOOP X=RECORDS(TreeQ) TO 1 BY -1
        GET(TreeQ,X) 
        Level=ABS(Level) 
        IF Level=1 THEN
           IF Contract THEN 
              IF Prv=2 THEN Level=-1.
           END
           PUT(TreeQ)
        END
        Prv=Level
    END
!------------
DDD.XRefCopy           PROCEDURE() !Copy XRef Qs to Clipboard
XLine GROUP,PRE(XL)
Proc1       STRING(40)
Proc2G      GROUP,OVER(Proc1)
Ind2            STRING(11)
Proc2           STRING(29)
            END
Spacer1     STRING(2)
LineNo      STRING(@n6)
LineNoStr   STRING(6),OVER(LineNo)
Spacer2     STRING(2)
CodeLine    STRING(80)
        END
XLineUL GROUP(XLine),PRE(XLUL).
CB      ANY
XRQR    &XRefQ
P2      BYTE
CRLF    EQUATE('<13,10>')
QX      LONG        
    CODE
    XLineUL='' ; XLUL:Proc1=ALL('=') ; XLUL:LineNoStr=ALL('=') ; XLUL:CodeLine=ALL('=',30)
    CB='XReference'
    LOOP P2=1 TO 2
         XLine='' ; XL:LineNoStr='LineNo' ; XL:CodeLine='Code'
         CASE P2
         OF 1
            CB='X-Reference of Procedure and Who Calls Proc '
            XL:Proc1='Callee  /  Caller Name'
            XRQR &= XRefQ
         OF 2
            XRQR &= XRef2Q
            CB=CB&'<13,10,13,10>X-Reference of Procedure and What Procs it Calls '
            XL:Proc1='Caller  /  Callee Name'
         END
         CB=CB & CRLF & CLIP(XLine) & CRLF & CLIP(XLineUL) 
         LOOP QX=1 TO RECORDS(XRQR)
            GET(XRQR,QX)
            CLEAR(XLine)
            IF XRQR.Level=1 THEN 
               XL:Proc1 = XRQR.Name
            ELSE
               XL:Proc2 = XRQR.Name
            END
            XL:LineNo   = XRQR.LineNo
            XL:CodeLine = XRQR.ALine
            CB=CB & CRLF & CLIP(XLine) 
         END !Loop XR
         CB=CB & CRLF & CLIP(XLineUL) & CRLF
    END !Loop P2  2 times
    SETCLIPBOARD(CB)

!------------
DDD.LogFileAppend PROCEDURE()  !Add to the LogFile
BS LONG
    CODE
    BS=INSTRING('\',TxaLoadFile,-1,SIZE(TxaLoadFile)) ; IF ~BS THEN BS=SIZE(TxaLoadFile).
    SHARE(LogFile)
    IF ERRORCODE()=2 THEN 
       CREATE(LogFile)
       SHARE(LogFile)
       Log:Line='Date,Time,File,Folder,TxaLns,Embeds,Source,Routines,Changes,Implicit,Omits,Problems <13,10>'
       ADD(LogFile,LenFastClip(Log:Line))
    END
    Log:Line=FORMAT(TODAY(),@D10) &','& FORMAT(CLOCK(),@T1) |
              &',"'& CLIP(SUB(TxaLoadFile,BS+1,255)) &'","'&  SUB(TxaLoadFile,1,BS-1) &'"' |
              &','& RECORDS(TxaQ) &','& RECORDS(EmbedQ) &','& RECORDS(SourceQ) &',' |
              & RECORDS(ClassQ)-2 &','& RECORDS(ChangeQ) &',' |
              & RECORDS(ImpliQ) &','& RECORDS(OmitQ) &','& Cnts:Problem  |
              &'<13,10>'
    ADD(LogFile,LenFastClip(Log:Line))
    CLOSE(LogFile)

!------------
DDD.EditSaveFn PROCEDURE()  
EditorEXE STRING(260)
Switches  PSTRING(64)
RunLine CSTRING(1000)  
    CODE
    IF ~EXISTS(TxaSaveFile) THEN 
        Message('Please save the TXA file first') ; SELECT(?TxaSaveBtn) ; RETURN 
    END 
    EditorEXE=GETINI('Editor','EXE','Notepad.EXE','.\Editor.INI')
    Switches =CLIP(' '&GETINI('Editor','Switches','','.\Editor.INI'))
    RunLine='"' & CLIP(EditorEXE) &'"' & Switches & ' "'&  CLIP(TxaSaveFile) &'"'
    RUN(RunLine)
    IF ERRORCODE() THEN Message('Run error ' & ErrorCode()&' '& ERROR() &'||Run('& RunLine ).
    RETURN
!------------
DDD.CompareSaveFn PROCEDURE()  
WinMergeEXE STRING(260)  ! F:\Tools\WinMerge_v216.4\WinMergeU.exe  Maybe make EXE and swicthes a INI setting or config 
WMDir_RegKey EQUATE('TypeLib\{{06029E17-28B5-456A-B866-4E79D98612FD}\1.0\HELPDIR') !has just the path so
RunLine CSTRING(1000)  
    CODE
    IF ~EXISTS(TxaSaveFile) THEN 
        Message('Please save the TXA file first') ; SELECT(?TxaSaveBtn) ; RETURN 
    END 
    WinMergeEXE=GetReg(REG_CLASSES_ROOT,WMDir_RegKey)
    IF EXISTS(WinMergeEXE) THEN WinMergeEXE=CLIP(WinMergeEXE) &'\WinMergeU.EXE'.
    IF ~WinMergeEXE OR ~EXISTS(WinMergeEXE) THEN 
        Message('Did not find WinMerge||Key: HKCR ' & WMDir_RegKey &'||'& WinMergeEXE )
        RETURN 
    END 
    
    RunLine='"' & CLIP(WinMergeEXE) &'" /wl /wr "' & CLIP(TxaLoadFile) &'" "'&  CLIP(TxaSaveFile) &'"'
    RUN(RunLine)
    IF ERRORCODE() THEN Message('Run error ' & ErrorCode()&' '& ERROR() &'||'& RunLine ).
    RETURN    
!------------
DDD.LastProcedureInClass PROCEDURE()  !I just add to the end before this 
    CODE
    Stop('Last?')
!==============================================
MruClass.Init PROCEDURE()
L USHORT
    CODE
    FREE(MruQ) 
    CLEAR(MruQ) 
    SELF.Letters=SELF.IniGet('List')
    LOOP L=1 TO LenFastClip(SELF.Letters)
         MruQ:Letter=SELF.Letters[L] 
         MruQ:FileName=SELF.IniGet(MruQ:Letter)
         MruQ:FileUPR=UPPER(MruQ:FileName)
         ADD(MRUQ)
    END 
MruClass.PushFile PROCEDURE(STRING FileName) 
L USHORT
    CODE
    MruQ:FileUPR=UPPER(FileName)
    GET(MRUQ,MruQ:FileUPR)
    IF ~ERRORCODE() THEN 
        L=POINTER(MruQ) 
        IF L=1 THEN RETURN.     !File is first
        DELETE(MRUQ)
        SELF.Letters=SUB(SELF.Letters,1,L-1) & SUB(SELF.Letters,L+1,26)
        !MruQ:Letter stays same
    ELSIF RECORDS(MRUQ)=26 THEN 
       GET(MRUQ,26)
       DELETE(MRUQ)
       !MruQ:Letter is last, falls off the end and added 1st
    ELSE !New Name < 26
       CLEAR(MruQ)
       MruQ:Letter=CHR(65+LenFastClip(SELF.Letters)) 
    END
    MruQ:FileName=FileName
    MruQ:FileUPR=UPPER(FileName)
    ADD(MRUQ,1)
    SELF.Letters= MruQ:Letter & SELF.Letters
    SELF.IniPut('List',SELF.Letters)
    SELF.IniPut(MruQ:Letter,MruQ:FileName)

MruClass.IniGet PROCEDURE(STRING Entry)!,STRING
MruFile STRING('.\MruDo2C.INI')
    CODE
    RETURN GETINI('MRU',Entry,,MruFile) 

MruClass.IniPut PROCEDURE(STRING Entry,*STRING Value)
MruFile STRING('.\MruDo2C.INI')
    CODE
    PUTINI('MRU',Entry,Value,MruFile) 

!==============================================
PopupUnder PROCEDURE(LONG CtrlFEQ, STRING PopMenu)!,LONG
X LONG,AUTO
Y LONG,AUTO
H LONG,AUTO
    CODE
    GETPOSITION(CtrlFEQ,X,Y,,H)
    RETURN POPUP(PopMenu,X,Y+H,1)
!==============================================
DB  PROCEDURE(STRING xMessage)
Prfx EQUATE('Do2Class: ') 
sz   CSTRING(SIZE(Prfx)+SIZE(xMessage)+1),AUTO
  CODE 
  sz  = Prfx & CLIP(xMessage)
  OutputDebugString( sz )
  RETURN
!---------------------------------------------
