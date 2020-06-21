    OMIT('_EndOfInclude_',_IFDef_CBTxa2Q_)
_IFDef_CBTxa2Q_  EQUATE(1)

![ ] Add Procedure Q to can tell if more than 1 ? but then the other queues should reference it. So mayb v2
 
!This is faily simlpy TXA parser that is only for finding [SOURCE]
!It would be confused by a EMBED %xxx in [SOURCE]

CbTxaLineQueueType     QUEUE,TYPE
LineNo      LONG      !LQ:LineNo
LenWO2      LONG      !LQ:LenWO2  !Does not include 13,10
TxtTxa      &STRING   !LQ:TxtTxa    !Does not include 13,10
TxtUpr      &STRING   !LQ:TxtUpr    !Does not include 13,10
PosBeg      LONG      !LQ:PosBeg
PosEnd      LONG      !LQ:PosEnd
               END
CbTxaLineQViewType     QUEUE,TYPE
LineNo      LONG      !1 LQ:LineNo
LenWO2      LONG     !2 LQ:LenWO2 !Does not include 13,10
TxtTxa    STRING(255) !3 LQ:TxtTxa  !Just to see, process using LQ:TxtTxaRef
StyleNo     LONG      !4 LQ:StyleNo     !in case want Style in LIST
TxtUpr    STRING(255) !5 LQ:TxtUpr
PosBeg      LONG      !6 LQ:PosBeg
PosEnd      LONG      !7 LQ:PosEnd
TxtTxaRef   &STRING   !8 LQ:TxtTxaRef   Ptr to full data
TxtUprRef   &STRING   !9 LQ:TxtUprRef  
               END
               
CbTxaEmbedType:Unknown   EQUATE(' ')
CbTxaEmbedType:Data      EQUATE('D')
CbTxaEmbedType:Routine   EQUATE('R')
CbTxaEmbedType:LocalPro  EQUATE('L')
CbTxaEmbedType:MethodData EQUATE('P')  !also XxxMethodDataSection like NewMethodDataSection
CbTxaEmbedType:MethodCode EQUATE('M')  !also XxxMethodCodeSection like NewMethodCodeSection

CbTxaSourceQueueType  QUEUE,TYPE 
EmbedLnNo   LONG        !SQ:EmbedLnNo  [EMBED] line
Type        STRING(1)   !SQ:Type       D,R,L see CbTxaEmbedType:Xxxx
Embed       STRING(64)  !SQ:Embed      EMBED %Name  1+ after [EMBED] 
!EmbedLnNo   LONG        !SQ:EmbedLnNo  [EMBED] line
SourceBeg   LONG        !SQ:SourceBeg  [SOURCE] Line#
SourceEnd   LONG        !SQ:SourceEnd  [END] Line#
CodeBeg     LONG        !SQ:CodeBeg    PROPERTY:END +1 Line#
CodeLine1   STRING(128) !SQ:CodeLine1  Line after  PROPERTY:END
               END    
!----------------- CBTxa2QClass -----------------
CBTxa2QClass    CLASS,TYPE,MODULE('CBTxa2Q.CLW'),LINK('CBTxa2Q.CLW')   !,_ABCLinkMode_),DLL(_ABCDllMode_)
!
!Properties
!----------
FileName    STRING(260)
TxtTXA      &STRING
TxtUPR      &STRING
TxtSize     LONG
LinesQ      &CbTxaLineQueueType
LineCnt     LONG
SourceQ     &CbTxaSourceQueueType
ProcName    STRING(128)
FromTemplate  STRING(128)    !FROM Clairon Report or FROM CPCSRpts UnivReport
ProcCount   USHORT
!Methods
!-------
ReadFile    PROCEDURE(STRING pFilename),LONG            !Read file from Disk into .TxtTXA / .TxtUPR
SplitLines  PROCEDURE()                                 !Build LinesQ by parsing TxtTXA
EmbedSourceQLoad PROCEDURE()                            !Scan TXA LinesQ to Build SourceQ
CopyLinesQ  PROCEDURE(CbTxaLineQueueType OutLinesQ)     !Copy .LinesQ to passed Out LinesQ
CopyLinesQ  PROCEDURE(CbTxaLineQViewType OutViewQ)      !Copy .LinesQ to passed Out View Q
CopySourceQ  PROCEDURE(CbTxaSourceQueueType OutSrcQ)    !Copy Embed Source Q to Out Embeds Source Q
LoadEmbedLines  PROCEDURE(LONG SourceQRecNo, CbTxaLineQViewType OutViewQ) !Load 1 Embed lines into Out View Q
Construct               PROCEDURE()
Destruct                PROCEDURE()
Init                    PROCEDURE()
Kill                    PROCEDURE()

!Proc1                  PROCEDURE(),VIRTUAL
                    END

    _EndOfInclude_
