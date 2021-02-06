!Do2Class Data Include
DooClassName STRING('DOO {26}')   !<--- Set your desired class name
p2ClassName PSTRING(32) !('DOO')   !<--- Set your desired class name
p2ClassDot  PSTRING(33) !('DOO.')  !     TODO inout on window and save to INI

EMBED_DATA_DataSection  EQUATE('EMBED %DATASECTION')            !Default Data Embed
EMBED_DATA_Declaration  EQUATE('EMBED %DECLARATIONSECTION')     !Report or Process or UnivReport or UnivProcess
 
TxaCls  CBTxa2QClass
TxaLoadFile STRING(260)
TxaSaveFile STRING(260)
 
A           LONG
X           LONG
LineX       LONG
LnTxt       LONG

DDD   CLASS
PrepareWindow           PROCEDURE()  !After Open(Window) code to make it right
LineHt                  PROCEDURE(LONG ListFEQ, SHORT LineHtBump=1)
IsTXAinputChanged       PROCEDURE()  !Choose betweeen RADIO('TXA')=1 and RADIO('Source')=0
LoadTxaFile             PROCEDURE(),BOOL 
LoadSxaQFromTxaQ        PROCEDURE()  !Load SXA queue of [Sections] from TXA
LoadSourceQFromTxaQ     PROCEDURE()
ProcessSourceQToCodeQ   PROCEDURE()  !Read SourceQ to find chnage and save into CodeQ
TxaAddChangesQEmbeds    PROCEDURE()
WriteTXAFile            PROCEDURE() !Write the changed TXA by read TXAQ and merge ChangesQ lines
WriteDosLine            PROCEDURE(STRING Block, BYTE Add1310Count=1)  !Write line to DosFile to output txa
WriteTagImplicit        PROCEDURE(LONG TXQ_LineNo,*STRING CodeLn, <*LONG Out_LenWO2>)
IsImplicit1PoundOnly    PROCEDURE(STRING CodeUpperClip),BOOL !Call UPPER(CLIP(code)) Implicits only X# ?
IsImplicitInCodeLine    PROCEDURE(STRING CodeUpperClip),BOOL !Call UPPER(CLIP())     Any Implcit

ParseOrigCodeToSourceQ  PROCEDURE()  !Put TEXT OrigCode lines into SourceQ to have sommon code with TXA
ProcessOrigCodeToCodeQ  PROCEDURE()  !Parse Origcode into CodeQ
ProcessCodeQ2DoClass    PROCEDURE()  !Make changes. Scan CodeQ for ROUTINE then make a DOO. Procedure. Scan for DO and change to DOO.
MakeCodeOnlyCoLine      PROCEDURE(*STRING InCodeLine, *CSTRING OutCoLine)  !CodeOnly = UPPER w/o 'String Literals' !Comments
BuildFixdCode           PROCEDURE()
MakeClassQPretty        PROCEDURE()   !Align the procedure names to look nice
!ConfigLoad              PROCEDURE()
!ConfigSave              PROCEDURE() 
!ConfigLoad1             PROCEDURE(STRING EntName, *? EntValue)
!ConfigSave1             PROCEDURE(STRING EntName, *? EntValue) 
ConfigLoadSave  PROCEDURE(BYTE Load1Save2)
ConfigLdSv1     PROCEDURE(BYTE LdSv,STRING EntName, *? EntValue)
CodeQBackNext           PROCEDURE(SHORT BackNext, LONG FindStyleA=0) !Select next List CodeQ with a Change style
StyleSetCodeQLists      PROCEDURE() 
StyleSetCodeQList       PROCEDURE(LONG StyleNo, LONG TextClr, LONG BackClr, LONG SelTextClr=-1, LONG SelBackClr=-1, LONG FontStyle=-1) 
RightClickLine          PROCEDURE(LONG LineNo, <STRING CopyText>)
!RightClickLine          PROCEDURE(LONG ListFEQ, QUEUE ListQ, *LONG 
SelectSourceQLine       PROCEDURE(LONG SourceQ_LineNo),BOOL,PROC   !GET(SourceQ then SELECT(?List,Pointer 
ChangesCodeCopy         PROCEDURE()
ImplicitCodeCopy        PROCEDURE()
TabCountQ               PROCEDURE() !Put (#) Count on all Tabs
TabCountQ               PROCEDURE(LONG TabFEQ, QUEUE Q, LONG Adjust=0, <STRING IconIfCount>) !Put (#) Count on Tab
ResizeWindow            PROCEDURE(BYTE IsOpen=0)
TreeExpandContract      PROCEDURE(QUEUE TreeQ, *LONG TreeLevel, BYTE Contract=0) 
Font10ptChange          PROCEDURE() 
XRefCopy                PROCEDURE() !Copy XRef Qs to Clipboard
LastProcedureInClass    PROCEDURE()  !I just add to the end before this 
LogFileAppend           PROCEDURE()  !Add to the LogFile
EditFileName            PROCEDURE(STRING FileName2Edit)  !Open Load or Save TXA in an Editor (Notepad)
CompareSaveFn           PROCEDURE()  !Compare Before and After TXAs using WinMerge
        END
        
!Region Queues 
CodeQProcLine1      LONG
CodeQ   QUEUE,PRE(CodeQ)        !put all in Q to make easier to figure out
LineNo      LONG                !CodeQ:LineNo
Type        SHORT               !CodeQ:Type
RtnPos      SHORT               !CodeQ:RtnPos
ALine       STRING(2048)        !CodeQ:ALine
StyleA      LONG                !CodeQ:StyleA   !See StyleA: Equates beow
CoLine      CSTRING(2050)       !CodeQ:CoLine  !Code Only Line 
        END 
ImpliQ  QUEUE(CodeQ),PRE(ImpQ)
        END 
OmitQ  QUEUE(CodeQ),PRE(OmtQ)   !OMIT() lines in Code
        END        
ProblmQ QUEUE(CodeQ),PRE(ProbQ) !StyleA:Problem lines in Code
        END 
Type:Comment    EQUATE(-1)   !or blank   For CodeQ:Type     
Type:Routine    EQUATE(10)   !XXXX ROUTINE w/o  Data Code line
Type:RoutineDC  EQUATE(20)   !XXXX Routine with DATA CODE line
Type:DATAline   EQUATE(21)   !Routine's "DATA" line
Type:CODEline   EQUATE(22)   !Routine's "CODE" line 
Type:InRoutine  EQUATE(3)    !Source is in a Routine want to check it for Implicits and RETURNs
Type:InMethod   EQUATE(4)    !05/24/20 KSS Source is in a MethodCode want to check for DO 
Type:Routine_Do2Class_Stop  EQUATE(90)  !07/07/20 

StyleA:Routine  EQUATE(1)    !          For CodeQ:StyleA
StyleA:Change   EQUATE(2)
!StyleA:ChangeDO EQUATE(3)   !could tag these separate to mark on Implicits
StyleA:Problem  EQUATE(4)
StyleA:Implicit EQUATE(5)
StyleA:Omit     EQUATE(6)    !An OMIT() that contains ROUTINEs is a Problem. Can tel If type 'R'
StyleA:Txa:Source     EQUATE(7)
StyleA:Txa:NotSource  EQUATE(8)
StyleA:Txa:Bracket    EQUATE(9)
StyleA:LAST           EQUATE(9)

ClassQ  QUEUE,PRE(ClassQ)
Name        STRING(128)         !ClassQ:Name
LineNo      LONG                !ClassQ:LineNo
Declare     LIKE(CodeQ:ALine)   !ClassQ:Declare
NameUPR     STRING(128)         !ClassQ:NameUPR
        END
        
ChangeQ   QUEUE,PRE(ChgQ)       !put all in Q to make easier to figure out
LineNo      LONG                !ChgQ:LineNo
!Type        SHORT              !ChgQ:Type
ALine       STRING(2048)        !ChgQ:ALine
NLine       STRING(2048)        !ChgQ:NLine     !new line
        END

XRefQ   QUEUE,PRE(XRQ)     !Cross-Ref Explosion Who Calls Routines
Name        STRING(128)    !XRQ:Name  
Level       LONG           !XRQ:Level 
LineNo      LONG           !XRQ:LineNo
ALine       STRING(1000)   !XRQ:ALine
UprName     STRING(128)    !XRQ:UprName   
Callee   STRING(128)       !XRQ:Callee
Caller   STRING(128)       !XRQ:Caller   
!ClassName   STRING(128)    !XRQ:ClassName   
!ProcName    STRING(128)    !XRQ:ProcName   
        END
XRef2Q  QUEUE(XRefQ),PRE(XR2Q)    !Implosion
        END

TxaQ        CbTxaLineQViewType
SxaQ        CbTxaLineQViewType      ![Sections] view
EmbedQ      CbTxaSourceQueueType
SourceQ     QUEUE(CbTxaLineQViewType),PRE(SourceQ) 
EmbedType   STRING(1)   !R=Rountine so can do SELF.
            END
 
 
!EndRegion    

!Region Queues from TXA Class        
!    EmbedQ  QUEUE,TYPE      !EmbedQ   CbTxaSourceQueueTyp
!    EmbedLnNo   LONG        !EmbedQ:EmbedLnNo  [EMBED] line   TxaQ:LineNo=xx ; GET(TxaQ,TxaQ:LineNo) ; SELECT(?List:TxaQ,POINTER(TxaQ))    
!    Type       STRING(1)  !EmbedQ:Type      D R L
!    Embed       STRING(64)  !EmbedQ:Embed      EMBED %Name  1+ after [EMBED] 
!    SourceBeg   LONG        !EmbedQ:SourceBeg  [SOURCE] Line#
!    SourceEnd   LONG        !EmbedQ:SourceEnd  [END] Line#
!    CodeBeg     LONG        !EmbedQ:CodeBeg    PROPERTY:END +1 Line#
!    CodeLine1   STRING(128) !EmbedQ:CodeLine1  Line after  PROPERTY:END
!                   END
!                   
!    SourceQ     QUEUE     !SourceQ     CbTxaLineQViewType
!    LineNo      LONG      !SourceQ:LineNo   SourceQ:LineNo=xx ; GET(SourceQ,SourceQ:LineNo) ; SELECT(?List:SourceQ,POINTER(SourceQ))    
!    LenTxt      LONG      !SourceQ:LenTxt
!    TxtTxa    STRING(255) !SourceQ:TxtTxa  !Could do ANY?
!    TxtUpr    STRING(255) !SourceQ:TxtUpr
!    PosBeg      LONG      !SourceQ:PosBeg
!    PosEnd      LONG      !SourceQ:PosEnd
!    TxtTxaRef   &STRING   !SourceQ:TxtTxaRef   Ptr to full data
!    TxtUprRef   &STRING   !SourceQ:TxtUprRef 
!    EmbedType   STRING(1) !SourceQ:EmbedType  !R=Rountine so can do SELF. 
!                END
!                
!    TxaQ     QUEUE        !TxaQ   CbTxaLineQViewType
!    LineNo      LONG      !TxaQ:LineNo   TxaQ:LineNo=xx ; GET(TxaQ,TxaQ:LineNo) ; SELECT(?List:TxaQ,POINTER(TxaQ))    
!    LenTxt      LONG      !TxaQ:LenTxt
!    TxtTxa    STRING(255) !TxaQ:TxtTxa  !Could do ANY?
!    TxtUpr    STRING(255) !TxaQ:TxtUpr
!    PosBeg      LONG      !TxaQ:PosBeg
!    PosEnd      LONG      !TxaQ:PosEnd
!    TxtTxaRef   &STRING   !TxaQ:TxtTxaRef   Ptr to full data
!    TxtUprRef   &STRING   !TxaQ:TxtUprRef 
!    EmbedType   STRING(1) !TxaQ:EmbedType  !R=Rountine so can do SELF. 
!                END 
!EndRegion Queues from TXA Class
   
!CatCls  CLASS
CaTxt  CLASS
Txt     ANY
Init    PROCEDURE()   
Kill    PROCEDURE()
Add     PROCEDURE(*STRING Str2Add, SHORT CRLFs=1, BYTE Clip=1)  
Add     PROCEDURE(STRING Txt2Add, SHORT CRLFs=1)   
AddCR   PROCEDURE(SHORT CRLFs=1)   
       END

Counts  GROUP,PRE(Cnts)
Problem   LONG
        END 
LogFile FILE,DRIVER('DOS'),NAME('LogDo2Cls.CSV'),PRE(Log),CREATE
    RECORD
Line    STRING(512)
    . .

MruQ QUEUE,PRE(MruQ)
FileName STRING(260)
Letter   STRING(1)
FileUPR  STRING(260)
    END

MruClass CLASS
Letters    STRING(26)
Init       PROCEDURE()
PushFile   PROCEDURE(STRING FileName) 
IniGet     PROCEDURE(STRING Entry),STRING
IniPut     PROCEDURE(STRING Entry,*STRING Value)
         END 
