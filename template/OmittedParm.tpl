#TEMPLATE(OmittedParmeterHelper,'Omitted Parameter Helper') ,FAMILY('ABC','CW20')
#!------------------------------------------------------------------------------
#!  Template by Carl Barnes free to use under MIT License.
#!------------------------------------------------------------------------------
#EXTENSION(OmittedParmHelper,'OMITTED() <Parameter> to OMITTED_Variable BOOL')
  #RESTRICT
    #IF (~%prototype OR SUB(%prototype,1,2)='()')
      #REJECT
    #ELSE
      #ACCEPT
    #ENDIF
  #ENDRESTRICT
#DISPLAY('Defines Variables for each Omitted <Parameter> to allow code')
#DISPLAY('in Class Methods testing IF Procedure Omitted parameters.')
#DISPLAY('')
#DISPLAY('   E.g. PROCEDURE(<LONG VarParm>)'),PROP(PROP:FontName,'Consolas')
#DISPLAY('   Omitted_VarParm BOOL  !Generated Variable'),PROP(PROP:FontName,'Consolas')
#DISPLAY('   Omitted_VarParm = OMITTED(VarParm)  !After Code'),PROP(PROP:FontName,'Consolas')
#DISPLAY('   To code: IF Omitted_VarParm  THEN '),PROP(PROP:FontName,'Consolas')
#DISPLAY('   Versus:  IF Omitted(VarParm) THEN   !Fails in Class  '),PROP(PROP:FontName,'Consolas')
#DISPLAY('')
#DISPLAY('Also can generate Default value Equates:')
#DISPLAY('   E.g. PROCEDURE(LONG RptDate=-1)'),PROP(PROP:FontName,'Consolas')
#DISPLAY('   RptDate_DefaultParm EQUATE(-1) !Generated Equate'),PROP(PROP:FontName,'Consolas')
#DISPLAY('   To code: IF RptDate=RptDate_DefaultParm THEN '),PROP(PROP:FontName,'Consolas')
#DISPLAY('')
#PROMPT('Generate Omitted_Variable BOOL',CHECK),%GenOmittedVar,DEFAULT(1),AT(10)
#PROMPT('Generate Variable_Default EQUATE',CHECK),%GenDefaultVar,DEFAULT(0),AT(10)
#DISPLAY('')
#DISPLAY('ABC does NOT have an Embed after CODE. This template provides')
#DISPLAY('Omitted_Parm variables that can be used in Class code.')
#DISPLAY('')
#DISPLAY('Template written by Carl Barnes for Do2Class.')
#DISPLAY('')
#!------------------------------------------------------------------------------------------
#! New C9 symbols: https://clarionsharp.com/blog/changes-to-template-built-in-symbols-in-c9/
#! But this is wrong:  %ProcedureParameterName - multi,  dependent from %Procedure
#! The correct symbol: %ProcedureParameters      that is multi and %ProcedureParameterName is dependent
#!------------------------------------------------------------------------------------------
#! ?? Option to generate as Variable_Omitted not Omitted_Varible so when type IF Var
#AT(%DataSection),FIRST
#INSERT(%ParmsOmittedData)
#INSERT(%ParmsDefaultData)
#ENDAT
#!-----------------------
!  #EMBED(%BeforeWindowManagerRun, 'Before WindowManager Run is called'),HIDE
! UPPER(%ProcedureTemplate)='PROCESS'
#AT(%BeforeWindowManagerRun),PRIORITY(1),WHERE(%AppTemplateFamily='ABC' AND UPPER(%ProcedureTemplate)<>'SOURCE')
  #INSERT(%ParmsOmittedCode)
#END
#AT(%ProcedureInitialize),PRIORITY(1),WHERE(%AppTemplateFamily<>'ABC' AND UPPER(%ProcedureTemplate)<>'SOURCE')
  #INSERT(%ParmsOmittedCode)
#END
#AT(%ProcessedCode),PRIORITY(1),WHERE(UPPER(%ProcedureTemplate)='SOURCE')
  #INSERT(%ParmsOmittedCode)
#END
#!-----------------------
#GROUP(%ParmsOmittedData)
  #IF (%GenOmittedVar)
    #FOR(%ProcedureParameters),WHERE(%ProcedureParameterOmitted)
OMITTED_%ProcedureParameterName BOOL
    #ENDFOR
  #ENDIF
#!-----------------------
#GROUP(%ParmsDefaultData)
  #IF (%GenDefaultVar)
    #FOR(%ProcedureParameters),WHERE(%ProcedureParameterDefault)
%ProcedureParameterName_DefaultParm EQUATE(%ProcedureParameterDefault)
    #ENDFOR
  #ENDIF
#!-----------------------
#GROUP(%ParmsOmittedCode)
  #IF (%GenOmittedVar)
    #FOR(%ProcedureParameters),WHERE(%ProcedureParameterOmitted)
OMITTED_%ProcedureParameterName = OMITTED(%ProcedureParameterName)
    #ENDFOR
  #ENDIF
#!-----------------------