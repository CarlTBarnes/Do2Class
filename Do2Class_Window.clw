!Do2Class Data Window
OrigCode     STRING(64000)
FixdCode     LIKE(OrigCode)

IsTXAinput  BYTE(1)  !0=Source
ParseOnly   BYTE
Cfg:Do2DataEmbeds       BYTE    !Cfg:Do2DataEmbeds  !n/a save
Cfg:Do2LocalProc        BYTE    !Cfg:Do2LocalProc   !n/a save
Cfg:Do2MethodCode       BYTE    !Cfg:Do2MethodCode  !do not save as default, doubt works 
!Future Cfg:OnePerEmbed         BYTE(1) !06/22/20 Split Routines into 1 Per Embed in TXXA Write

ConfigGrp   GROUP,PRE(Cfg) 
!Do2DataEmbeds       BYTE        !Cfg:Do2DataEmbeds
!Do2LocalProc        BYTE        !Cfg:Do2LocalProc
NoMoveRoutines      BYTE        !Cfg:NoMoveRoutines   Do NOT move %Routine to %LocalProcedures
OmitDATAline        BYTE(1)     !Cfg:OmitDATAline
SELFinClass         BYTE        !Cfg:SELFinClass
WriteClass2Data     BYTE(1)     !Cfg:WriteClass2Data
WriteClassSorted    BYTE(1)     !Cfg:WriteClassSorted
TagImplicitLines    BYTE(1)     !Cfg:TagImplicitLines  on Write mark Implicitis with !#$"
!         BYTE        !Cfg:
!         BYTE        !Cfg:

            END
!NoDOO   BYTE    !A way to just analyze / XRef only , no source changes for less con            
                !  maybe do this at the end by undoing the chnages, read Change Q and chnage the CODEQ Back.
                ! could be a button 
!                CHECK('No DOO'),AT(245,42),USE(NoDOO),TIP('Do not make DOO changes, way to XRef')                

DidNotMoveRoutineEmbeds    BYTE !workaround !Normally move %ProcedureRoutines to %LocalProcedures
WriteNoChanges    BYTE        !way to test TXA write matches original
Font10pt          BYTE(10)

Window WINDOW('DO to CLASS for TXA '),AT(,,577,388),CENTER,GRAY,IMM,SYSTEM,MAX,ICON('Do2CIcon.ICO'), |
            FONT('Segoe UI',9),DROPID('~FILE'),RESIZE
        OPTION,AT(481,2,66,12),USE(IsTXAinput),SKIP
            RADIO('TXA'),AT(485,3),USE(?IsTXAinput:Radio1),TIP('Load and convert TXA file.<13,10>Mus' & |
                    't be ONE Procedure'),VALUE('1')
            RADIO('Source'),AT(513,3),USE(?IsTXAinput:Radio2),TIP('Paste in Source Code with ROUTINE' & |
                    ' and convert'),VALUE('0')
        END
        SPIN(@n2),AT(551,3,23,9),USE(Font10pt),SKIP,TIP('Font Size'),RANGE(8,14)
        SHEET,AT(1,2),FULL,USE(?Sheet1)
            TAB(' Steps  '),USE(?Tab_Instrux)
                CHECK('. Step'),AT(54,37),USE(?Step:X)
                CHECK(' Backup APP Folder. TXA Verify by Export + Import + Compare CLWs'),AT(54,37), |
                        USE(?Step:BackupAPP)
                CHECK(' Selective Export to TXA One Procedure to APP_Procedure.TXA.  Optional: Copy ' & |
                        'Procedure inside APP to have original to view.'),AT(54,60),USE(?Step:Txa1Procedure) |
                        
                CHECK(' In Do2Class TXA Name pick your "APP_Procedure.TXA" and click Load'),AT(54,82), |
                        USE(?Step:LoadTxaHere)
                BUTTON('Jump'),AT(18,101,29,10),USE(?Jump:Omit)
                CHECK(' Review OMIT tab. All Omit code is processed whether conditional or not. Chec' & |
                        'k if code contains Routines that were processed.'),AT(54,101),USE(?Step:OmitReview) |
                        
                BUTTON('Jump'),AT(18,122,29,10),USE(?Jump:Problems)
                CHECK(' Review PROBLEMS tab. This code refactoring can be handled after Import Do2Cl' & |
                        'ass TXA. Or refactor and export and import again.'),AT(54,122),USE(?Step:Problems)
                BUTTON('Jump'),AT(18,141,29,10),USE(?Jump:Implicit)
                CHECK(' Review IMPLICIT #$" tab. CANNOT be used to pass data. They are Class Method ' & |
                        'Scope versus in Routines are Procedure scope.  Best to remove implicits. '), |
                        AT(54,141),USE(?Step:Implicit)
                BUTTON('Jump'),AT(18,162,29,10),USE(?Jump:SaveTxa)
                CHECK(' Save TXA on CLASS Code tab with modifications to change Routine to Class nam' & |
                        'es "_Do2Class_". Compare to original TXA to see changes. '),AT(54,162), |
                        USE(?Step:SaveTXA)
                CHECK(' Review the Code Changes. Review the XRef. You can Right-Click on most lines ' & |
                        'and just to other lists.'),AT(54,181),USE(?Step:ReviewThings)
                CHECK(' Import Do2Class TXA in APP and Build. Review Errors and refactor code as nee' & |
                        'ded. ... Don''t like the results? Import the original TXA to undo changes.'), |
                        AT(54,202),USE(?Step:AppImportTxaDo2Class)
                CHECK(' After Import to APP Review PROBLEMS, OMITS, IMPLICIT tabs again and check in' & |
                        ' APP code. '),AT(54,227),USE(?Step:Problems2)
                CHECK(' The XRef tab may help understand the code. '),AT(54,242),USE(?Step:Problems2:2)
            END
            TAB(' &TXA '),USE(?TabTxaLoad)
                PROMPT('TXA &File:'),AT(7,20,,14),USE(?TxaLoadFile:Pmt)
                COMBO(@s255),AT(43,20,339,12),USE(TxaLoadFile),VSCROLL,DROP(15),FROM(MruQ), |
                        FORMAT('290L(2)@s255@Z(1)20L(2)@s1@Z(1)')
                BUTTON('&Pick...'),AT(387,20,26,11),USE(?PickTXAbtn)
                BUTTON('Paste'),AT(417,20,26,11),USE(?PasteTXAbtn),TIP('Paste TXA File Name')
                BUTTON('Edit'),AT(448,20,24,11),USE(?EditLoadFnBtn),SKIP,TIP('Open TXA file in an Editor')
                BUTTON('Explore'),AT(476,20,28,11),USE(?ExploreBtn),SKIP,TIP('Open Windows Explorer')
                BUTTON('&Load'),AT(6,30,30,18),USE(?LoadTxaBtn),FONT(,,,FONT:bold),DEFAULT
                CHECK('Parse'),AT(544,18),USE(ParseOnly),FONT(,7),TIP('When Loading do NOT convert s' & |
                        'o can test parsing')
                BUTTON('Process'),AT(544,28,29,10),USE(?ProcessTxaBtn),DISABLE,SKIP,FONT(,8), |
                        TIP('DOO.ProcessCodeQ2DoClass()')
                BUTTON('Build'),AT(544,40,29,10),USE(?BuildTxaBtn),DISABLE,SKIP,FONT(,8),TIP('DOO.Bu' & |
                        'ildFixdCode() ')
                BUTTON('ReRun'),AT(510,20,26,11),USE(?RunAgainBtn),SKIP,TIP('Run another Instance')
                PROMPT('Class:'),AT(43,37),USE(?DooClassName:Pmt)
                ENTRY(@s30),AT(66,37,63,10),USE(DooClassName),SKIP,TIP('Your Desired Class Name, I l' & |
                        'ike DOO')
                CHECK('SELF'),AT(145,43),USE(Cfg:SELFinClass),TIP('Use SELF.Method inside Class meth' & |
                        'od code instead of DOO.Method<13,10>This is to work like ROUTINEs so Carl p' & |
                        'refers to NOT use SELF and always type DOO.<13,10>If you plan to extract th' & |
                        'is class and inherit it then check SELF.')
                CHECK('Do NOT move %Routine to %LocalProcedures'),AT(145,33),USE(Cfg:NoMoveRoutines), |
                        TIP('Check to NOT move %ProcedureRoutines to %LocalProcedures<13,10>Good to ' & |
                        'test and see compiled source compare better because less changes.')
                CHECK('DATA Embeds'),AT(311,33),USE(Cfg:Do2DataEmbeds),TIP('Check to process EMBED %' & |
                        'DataSection.<13,10>Should not have code to change so are skipped.')
                CHECK('Method Code'),AT(370,33),USE(Cfg:Do2MethodCode),TIP('Methods can have Routine' & |
                        's. I do NOT think this will work.<13,10>Process %NewMethodCodeSection Embed' & |
                        's and Data')
                CHECK('%LocalProcedures Embeds'),AT(311,43),USE(Cfg:Do2LocalProc),TIP('Local Procedu' & |
                        'res can contain Local ROUTINEs which should NOT be processed by Do2Class.' & |
                        '<13,10,13,10>If this Embed was incorrectly used for ROUTINEs then check thi' & |
                        's box to process it.')
                PROMPT('Must be SINGLE Procedure TXA.'),AT(434,36,49,15),USE(?TxaWarn1Proc), |
                        FONT(,8,COLOR:Red)
                !--- TXA Q   
                PANEL,AT(3,53,,2),FULL,USE(?PanelH),BEVEL(0,0,0600H)
                SHEET,AT(6,54),FULL,USE(?Sheet_TXA),NOSHEET,BELOW
                    TAB('  TXA File  '),USE(?Tab_TxaFile)
                        LIST,AT(2,72),FULL,USE(?LIST:TxaQ),HVSCROLL,FONT('CONSOLAS',10),VCR, |
                                FROM(TxaQ),FORMAT('30R(2)|FM~Line#~L(2)@n_5@30L(2)Y~TXA Line Text - ' & |
                                'Right click for options~@s255@#3#'),ALRT(EnterKey)
                    END
                    TAB('  TXA [EMBED]  '),USE(?Tab_Txa_EMBED)
                        LIST,AT(1,71),FULL,USE(?LIST:EmbedQ),VSCROLL,FONT('CONSOLAS',10),FROM(EmbedQ), |
                                FORMAT('30R(2)|M~Line#~C(0)@n7@19C|M~Type~@s1@Q''D=Data R=Routine L=' & |
                                'Local Procedure <13,10>M=Method Code P=Method Data''125L(2)|M~[Embe' & |
                                'd]<13,10>Name~@s255@[30R(2)|M~BegPos~C(0)@n7@30R(2)|M~EndPos~C(0)@n' & |
                                '7@30R(2)|M~Code#~C(0)@n75@]|~[SOURCE]~31L(2)~Code<13,10>Line 1     ' & |
                                'Double Click to jump to TXA line~@s255@'),ALRT(EnterKey)
                    END
                    TAB('  Embed [SOURCE] Lines '),USE(?Tab_Txa_SOURCE)
                        STRING('SourceQ Recs'),AT(350,58),USE(?SourceQRecs)
                        LIST,AT(1,71),FULL,USE(?LIST:SourceQ),HVSCROLL,FONT('CONSOLAS',10),VCR, |
                                FROM(SourceQ),FORMAT('30R(2)|FM~Line#~C(0)@n7@24R(2)|M~Len~C(0)@n7@3' & |
                                '0R(2)|M~BegPos~C(0)@n7@#5#30R(2)|M~EndPos~C(0)@n7@21C|M~Type~@s1@Q''' & |
                                'D=Data R=Routine L=Local Procedure <13,10>M=Method Code P=Method Da' & |
                                'ta''#10#30L(2)~Source lines before change - Double click jumps to T' & |
                                'XA Line, Right click more ...~@s255@#3#'),ALRT(EnterKey)
                    END
                    TAB(' [Section] Only '),USE(?Tab_SxaQ)
                        LIST,AT(1,71),FULL,USE(?LIST:SxaQ),HVSCROLL,FONT('CONSOLAS',10),VCR, |
                                FROM(SxaQ),FORMAT('30R(2)|FM~Line#~L(2)@n_5@72L(2)|FM~[Secton]~@s255' & |
                                '@#3#30L(2)~Next Line~@s255@#5#'),ALRT(EnterKey)
                    END
                END
            END
            TAB(' &ROUTINE Code'),USE(?TabRoutineCode),TIP('Source with code using Routines to be DO' & |
                    'O converted')
                BUTTON('Paste'),AT(83,18,,14),USE(?PasteBtn),SKIP,TIP('Paste Clipboard into Window a' & |
                        'nd Process<13,10>Clipboard should contain CODE down through the end of ROUT' & |
                        'INEs<13,10>Do NOT include DATA nor existing Classes.')
                BUTTON('Process'),AT(122,18,41,14),USE(?ProcessBtn)
                CHECK('Parse Only'),AT(168,19,47),USE(ParseOnly,, ?ParseOnly:2),TIP('Do not convert ' & |
                        'so can test parsing')
                BUTTON('Cl&ear'),AT(227,18,,14),USE(?ClearBtn),SKIP
                BUTTON('Copy'),AT(263,18,,14),USE(?CopyOrigBtn),SKIP,TIP('Copy code below to clipboard')
                BUTTON('&Close'),AT(324,18,42,14),USE(?CloseBtn),SKIP,STD(STD:Close)
                PROMPT('&CODE plus ROUTINEs:'),AT(7,22),USE(?OrigCode:Prompt)
                PROMPT('Class:'),AT(389,20),USE(?DooClassName:Pmt2)
                ENTRY(@s30),AT(411,20,63,10),USE(DooClassName,, ?DooClassName:2),SKIP,TIP('Your Desi' & |
                        'red Class Name, I like DOO')
                TEXT,AT(7,36),FULL,USE(OrigCode),HVSCROLL,FONT('Consolas',10)
            END
            TAB(' &CLASS Code (Save TXA)'),USE(?TabClassCode),TIP('Final Result of this program')
                BUTTON('Copy Class'),AT(7,19,41,13),USE(?CopyBtn),SKIP,TIP('Copy CLASS Code to Clipboard')
                BUTTON('Close'),AT(53,19,27,13),USE(?Close2Btn),SKIP,STD(STD:Close)
                GROUP,AT(90,18,453,25),USE(?SaveTxaGroup),DISABLE
                    BUTTON('&Save TXA with Classes'),AT(92,19,,13),USE(?TxaSaveBtn),TIP('Save TXA wi' & |
                            'th Class changes and below TXT file<13,10>Also places file name on Clipboard')
                    CHECK('Omit ! DATA'),AT(92,33),USE(Cfg:OmitDATAline),TIP('Uncheck to retain Rout' & |
                            'ine DATA lines  as a !comment.<13,10>Allow searching for Routines that ' & |
                            'had DATA.')
                    ENTRY(@s255),AT(185,20,314,11),USE(TxaSaveFile),SKIP,COLOR(COLOR:BTNFACE),READONLY
                    CHECK('Write CLASS to %Data'),AT(185,33),USE(Cfg:WriteClass2Data),TIP('Write CLA' & |
                            'SS declaration to first Data embed')
                    CHECK('Sorted'),AT(267,33),USE(Cfg:WriteClassSorted),TIP('Sort Methods by Name, ' & |
                            'uncheck for by Line Order')
                    CHECK('Tag Implicit !#$" '),AT(303,33),USE(Cfg:TagImplicitLines),TIP('Tag Implic' & |
                            'it Lines !#$" in the save TXA to make it easy to find them and fix.')
                    CHECK('Write No Changes'),AT(375,33),USE(WriteNoChanges),TIP('Test the TXA Write' & |
                            ' code works correcty.<13><10>Without changes a compare should show the ' & |
                            'files are identical.')
                    BUTTON('Copy'),AT(503,20,26,11),USE(?CopySaveFnBtn),SKIP,TIP('Copy Save Filename')
                    BUTTON('Edit'),AT(475,33,24,11),USE(?EditSaveFnBtn),SKIP,TIP('Open Save TXA in a' & |
                            'n Editor, maybe to rename the Procedure')
                    BUTTON('Compare'),AT(503,33,35,11),USE(?CompareSaveFnBtn),SKIP,TIP('Compare Save' & |
                            'd TXA to Original with WinMerge')
                    BUTTON('Explore'),AT(543,33,30,11),USE(?ExploreSaveFnBtn),SKIP,TIP('Open Explorer')
                END
                BUTTON('ReRun'),AT(543,20,30,11),USE(?RunAgainBtn2),SKIP,TIP('Run another Instance')
                PROMPT('CLASS Code:'),AT(7,35),USE(?FixdCode:Prompt)
                TEXT,AT(7,46),FULL,USE(FixdCode),HVSCROLL,FONT('Consolas',10)
            END
            TAB(' C&hanges'),USE(?TabChangeQ),TIP('Changed lines Over-Under view')
                BUTTON('Copy'),AT(7,18,,13),USE(?CopyChangesBtn),SKIP,TIP('Copy the Change list below')
                LIST,AT(7,33),FULL,USE(?LIST:ChangeQ),VSCROLL,FONT('Consolas',10),FROM(ChangeQ), |
                        FORMAT('[66R(2)|FM~Line~C(0)@n6@/](28)|F[20L(2)|FM~Before Line            Do' & |
                        'uble Click on line to jump to SourceQ Tab~@s255@/2L(2)|_FM~After Line      ' & |
                        '   Delete any undesired changes by selecting the line and pressing DELETE~L' & |
                        '(18)@s255@]|F'),ALRT(DeleteKey), ALRT(EnterKey)
            END
            TAB(' Chg SbS'),USE(?TabChgQSbS),TIP('Changed lines Side-by-Side view')
                BUTTON('Copy'),AT(7,18,,13),USE(?CopyChgSbSBtn),SKIP,TIP('Copy the Change list below')
                LIST,AT(7,33),FULL,USE(?LIST:ChangeQ:SbS),VSCROLL,FONT('Consolas',10),FROM(ChangeQ), |
                        FORMAT('28R(2)|FM~Line~C(0)@n6@250L(2)|FM~Before Line    Double Click on lin' & |
                        'e to jump to SourceQ Tab~@s255@400L(2)F~After Line    Delete any undesired ' & |
                        'changes by selecting the line and pressing DELETE~@s255@'),ALRT(DeleteKey), |
                         ALRT(EnterKey)
            END
            TAB(' Cl&ass Q'),USE(?TabClassQ),TIP('List of Classes (Routines) found in code')
                LIST,AT(7,23),FULL,USE(?LIST:ClassQ),VSCROLL,FONT('Consolas',10),FROM(ClassQ), |
                        FORMAT('100L(2)|FM~Name~23R(2)|FM~Line~L(2)@n5b@204L(2)|FM~Code~@s255@'), |
                        ALRT(EnterKey)
            END
            TAB(' C&ode Q'),USE(?TabCodeQ),TIP('Code Side-by-Side View. Changes are highlighted.')
                STRING('Type: 10=Routine  20=RoutineDC 21=DATA 22=CODE  3=In Routine  -1=Comment'), |
                        AT(7,19,247),USE(?StringCodeQ)
                BUTTON('&Back'),AT(263,18,29,12),USE(?CodeBackRtn),SKIP,TIP('Jump to Previous Change')
                BUTTON('&Next'),AT(295,18,29,12),USE(?CodeNextRtn),SKIP,TIP('Jump to Next Change')
                BUTTON('ROUTINE'),AT(343,19,37,10),USE(?Sample:Routine),TIP('Find next ROUTINE chang' & |
                        'ed to DOO.Method()')
                BUTTON('Change'),AT(381,19,35,10),USE(?Sample:Change),TIP('Find next code change.' & |
                        '<13,10>Like EXIT becomes RETURN,<13,10>DO Routine becomes DOO.Routine()')
                BUTTON('Problem'),AT(417,19,35,10),USE(?Sample:Problem),TIP('RETURN in Routine was f' & |
                        'or PROCEDURE() <13,10>... now will Return from DOO.Method()<13,10><13,10>OM' & |
                        'ITTED() Paramater check was looking at Procedure(),<13,10>... new would be ' & |
                        'DOO.Method() parameters.')
                BUTTON('Implicit#$"'),AT(453,19,37,10),USE(?Sample:Impli),TIP('Find next Implicit us' & |
                        'age<13,10>They work differently.')
                BUTTON('OMIT'),AT(493,19,30,10),USE(?Sample:OMIT)
                LIST,AT(7,33),FULL,USE(?LIST:CodeQ),VSCROLL,FONT('Consolas',10),FROM(CodeQ), |
                        FORMAT('23R(2)|FM~Line~L(2)@n5b@23R(2)|FM~Type~L(2)@n-3b@/23R(2)|FM~Rtn Pos ' & |
                        'of Label ~L(2)@n4b@/250L(2)|MY~Code Line Original~S(200)@s255@204L(2)|M~CoL' & |
                        'ine - Code Only no Literal/Comment for Parsing~S(200)@s255@'),ALRT(EnterKey)
            END
            TAB(' Code O-U'),USE(?TabCodeOU),TIP('Code Over-Under View')
                LIST,AT(7,23),FULL,USE(?LIST:CodeOU),VSCROLL,FONT('Consolas',10),VCR,FROM(CodeQ), |
                        FORMAT('[66R(1)|FM~Line~@n5@/20L(1)|_FM~Type~@n-3b@](33)|F[20L(2)|FMY~Code L' & |
                        'ine Original~@s255@/#4#20L(2)|_FM~CoLine - Code only without String Literal' & |
                        's or Comments for Searching~L(18)@s255@]|F'),ALRT(EnterKey)
            END
            TAB(' &Implicit #$" '),USE(?TabImpliQ),TIP('Implicit #$" Variable Use. These could be a ' & |
                    'PROBLEM.')
                STRING('Implicits are generally BAD and should be avoided by professional programmer' & |
                        's! It says so in the Help.'),AT(7,19),USE(?StringImpliQ),FONT(,,,FONT:bold)
                STRING('Implicits work differently in ROUTINEs then they work in CLASS methods. In R' & |
                        'OUTINEs they were Main Procedure Local. In a CLASS they are Method Local.'), |
                        AT(49,29),USE(?StringImpliQ2)
                STRING('Implicit values persist between ROUTINE calls (DOs). Values do NOT persist b' & |
                        'etween CLASS method calls. They are Method LOCAL in Scope in a CLASS'),AT(49,38), |
                        USE(?StringImpliQ3)
                STRING('Implicits CANNOT be used to pass data between Methods like Routines ... E#=?' & |
                        'Entry ; DO CheckRtn '),AT(49,47),USE(?StringImpliQ:2)
                !STRING('Implicits are BAD! They are LOCAL in Scope in CLASS so do NOT Persist. Implicits CANNOT be used to pass data between Methods.... DIFFERENT than Routines. '),AT(49,19+40),USE(?StringImpliQ9)
                BUTTON('Copy'),AT(7,59,,12),USE(?CopyImplicitsBtn),SKIP,TIP('Copy the Implicits list' & |
                        ' below')
                CHECK('In save TXA Tag Implicit lines with !#$" to make them easy to jump to with Search'), |
                        AT(49,60),USE(Cfg:TagImplicitLines,, ?Cfg:TagImplicitLines:ImpTab),SKIP, |
                        TIP('Tag Implicit Lines !#$" on write TXA<13,10>to make it easy to find them' & |
                        ' in the Embeditor by search for !#$.')
                LIST,AT(7,73),FULL,USE(?LIST:ImpliQ),VSCROLL,FONT('Consolas',10),FROM(ImpliQ), |
                        FORMAT('23R(2)|FM~Line~L(2)@n5b@23R(2)|FM~Type~L(2)@n-3b@/23R(2)|FM~Rtn Pos ' & |
                        'of Label ~L(2)@n4b@/250L(2)|MY~Code Line Modified ~S(200)@s255@204L(2)~Code' & |
                        ' Line Original ~S(200)@s255@'),ALRT(EnterKey)
            END
            TAB(' Problems '),USE(?TabProblmQ)
                STRING('RETURN in a ROUTINE was for the Procedure(), in a Class method RETURN will e' & |
                        'xit the Method not the Procedure.'),AT(7,20),USE(?ProbReturn)
                STRING('Code changes MUST be made for this Return to work...have Method return BOOL ' & |
                        'and caller does IF ~DOO.MyCode() THEN RETURN. (read tooltip)'),AT(27,30), |
                        USE(?ProbReturnFix)
                STRING('DO ProcedureReturn in a ROUTINE was also for the Procedure(). Compiler will ' & |
                        'error: ROUTINE that can RETURN from procedure cannot be called here. Same f' & |
                        'ix as Return. (tip)'),AT(27,40),USE(?ProbReturnDPR)
                STRING('OMITTED() in a ROUTINE is for the Procedure(<<Parm>). In a Class it will be ' & |
                        'for Method(<<Parm>). Suggest to never use old syntax Omitted(#).'),AT(7,50), |
                        USE(?ProbOmitted)
                STRING('Code changes MUST be made ... An ABC limitation is a Routine is the only way' & |
                        ' to get Omitted() for Procedure so change back to a Routine.  (read tooltip)'), |
                        AT(27,60),USE(?ProbOmittedFix)
                LIST,AT(7,83),FULL,USE(?LIST:ProblmQ),VSCROLL,FONT('Consolas',10),FROM(ProblmQ), |
                        FORMAT('23R(2)|FM~Line~L(2)@n5b@23R(2)|FM~Type~L(2)@n-3b@/23R(2)|FM~Rtn Pos ' & |
                        'of Label ~L(2)@n4b@/250L(2)|MY~Code Line Modified ~S(200)@s255@204L(2)~Code' & |
                        ' Line Original ~S(200)@s255@'),ALRT(EnterKey)
            END
            TAB(' OMIT '),USE(?TabOmitQ),TIP('OMIT() lines.')
                STRING('This program processes OMIT() / COMPILE() code. Best to double check this wi' & |
                        'll not cause problems. '),AT(49,19),USE(?StringOmitQ)
                BUTTON('Copy'),AT(7,18,,12),USE(?CopyOmitBtn),SKIP,TIP('Copy the Omit list below')
                LIST,AT(7,33),FULL,USE(?LIST:OmitQ),VSCROLL,FONT('Consolas',10),FROM(OmitQ), |
                        FORMAT('23R(2)|FM~Line~L(2)@n5b@23R(2)|FM~Type~L(2)@n-3b@/23R(2)|FM~Rtn Pos ' & |
                        'of Label ~L(2)@n4b@/250L(2)|MY~Code Line Modified ~S(200)@s255@204L(2)~Code' & |
                        ' Line Original ~S(200)@s255@'),ALRT(EnterKey)
            END
            TAB('XRef'),USE(?TabXRef),TIP('Cross Reference<13,10>Callee-Caller Explosion of Who call' & |
                    's Me<13,10>Caller-Callee Implosion of Who do I Call')
                BUTTON('Expand'),AT(7,18,,12),USE(?XRefExpandBtn),SKIP
                BUTTON('Contract'),AT(49,18,,12),USE(?XRefContractBtn),SKIP
                BUTTON('Copy'),AT(101,18,29,12),USE(?XRefCopyBtn),SKIP
                BUTTON('Show Dbg Q'),AT(480,18,,12),USE(?XRefDbgBtn),SKIP
                LIST,AT(6,33,280),FULL,USE(?LIST:XRefQ),VSCROLL,FONT('Consolas',10),FROM(XRefQ), |
                        FORMAT('100L(2)|FMT~Callee / Caller Name~22R(2)|FM~Line~C(0)@n_5b@280L(2)F~C' & |
                        'ode~@s255@')
                LIST,AT(292,33,280),FULL,USE(?LIST:XRef2Q),VSCROLL,FONT('Consolas',10),FROM(XRef2Q), |
                        FORMAT('100L(2)|FMT~Caller / Callee Name~22R(2)|FM~Line~C(0)@n_5b@280L(2)F~C' & |
                        'ode~@s255@')
            END
            TAB('X dbg'),USE(?TabXRefDebug),HIDE
                LIST,AT(7,20,,180),FULL,USE(?LIST:XRefQ__),VSCROLL,FONT('Consolas',10),FROM(XRefQ), |
                        FORMAT('100L(2)|FMT~Callee / Caller Name~22R(2)|FM~Line~C(0)@n_5b@280L(2)|FM' & |
                        '~Code~@s255@50L(2)|M~NameUPR~@s255@50L(2)|M~Class~@s255@20L(2)|M~Method~@s255@')
                LIST,AT(7,206,,182),FULL,USE(?LIST:XRef2Q__),VSCROLL,FONT('Consolas',10),FROM(XRef2Q), |
                        FORMAT('100L(2)|FMT~Caller / Callee Name~22R(2)|FM~Line~C(0)@n_5b@280L(2)|FM' & |
                        '~Code~@s255@50L(2)|M~NameUPR~@s255@50L(2)|M~Class~@s255@20L(2)|M~Method~@s255@')
            END
        END
    END
