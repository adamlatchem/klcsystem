C+
C
C VERSION:      1.2-002
C
C FACILITY:     KLC
C
C MODULE:       HELP
C
C ABSTRACT:     This will displaya window with help text in from the help
C               library KLC$PATH:KLC.HLB
C
C ENVIRONMENT:  It requires a string and keybaord_id. The string is the
C               help key
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  21-MAR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: HELP v1.2'
        SUBROUTINE HELP( string, keyboard, paste, errorc )
        IMPLICIT INTEGER(s)
        INCLUDE '($SMGDEF)'
C
        CHARACTER*(*)   string, string1*240
        INTEGER*4       key, keyboard, localdisplay, paste, line, helpdisp
        INTEGER*4       errorc
        EXTERNAL        KLC_NORMAL
        errorc = %LOC(KLC_NORMAL)
C+
C       Firstly we create a display to hold the help text. Then we put text to
C       the display and then allow the user to scroll around the text.
C-
        s = SMG$CREATE_VIRTUAL_DISPLAY( 2, 80, helpdisp )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$DRAW_LINE( helpdisp, 1, 1, 1, 80 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( helpdisp, 
        1 'HELP                    Use arrow keys to view text, RETURN to exit'
        2 , 2, 6 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$CREATE_VIRTUAL_DISPLAY( 100, 80, localdisplay )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_HELP_TEXT( localdisplay, , string, 'KLC$PATH:KLC.HLB')
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$CREATE_VIEWPORT( localdisplay, 1, 1, 22, 80 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PASTE_VIRTUAL_DISPLAY( localdisplay, paste, 1, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PASTE_VIRTUAL_DISPLAY( helpdisp, paste, 23, 1 )
        IF (.NOT.s) CALL ERROR( s )
1       key = 0
        line = 1
        DO WHILE(key.NE.13)
                s = SMG$READ_KEYSTROKE( keyboard, key )
                IF (.NOT.s) CALL ERROR( s )
                IF (key.EQ.SMG$K_TRM_UP) THEN
                   IF (line.GT.1) THEN
                        line = line - 1
                        s = SMG$SCROLL_VIEWPORT( localdisplay, SMG$M_DOWN, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                   ELSE
                        line = 0
                        key = 13
                   END IF
                END IF
                IF (key.EQ.SMG$K_TRM_DOWN) THEN
                   IF (line.LT.30) THEN
                        line = line + 1
                        s = SMG$SCROLL_VIEWPORT( localdisplay, SMG$M_UP, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                   END IF
                END IF
        END DO
        IF (line.EQ.0) THEN
                s = SMG$BEGIN_PASTEBOARD_UPDATE( paste )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$UNPASTE_VIRTUAL_DISPLAY( localdisplay, paste )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$ERASE_DISPLAY( localdisplay )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$DELETE_VIEWPORT( localdisplay )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$PUT_HELP_TEXT( localdisplay, ,'GENERAL' , 
        1               'KLC$PATH:KLC.HLB')
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$CREATE_VIEWPORT( localdisplay, 1, 1, 22, 80 )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$PASTE_VIRTUAL_DISPLAY( localdisplay, paste, 1, 1 )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$PUT_CHARS( helpdisp, 
        1 'Please enter the topic which you wish to gain help on:             '
        2       , 2, 6 )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$END_PASTEBOARD_UPDATE( paste )
                IF (.NOT.s) CALL ERROR( s )
                string1 = '                  '
                CALL RSTRING( helpdisp, string1, 80, 60, 2, 13, 
        1               keyboard, paste, t, s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                IF (string1.NE.'             ') THEN
                        s = SMG$BEGIN_PASTEBOARD_UPDATE( paste )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$UNPASTE_VIRTUAL_DISPLAY( localdisplay, paste )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$ERASE_DISPLAY( localdisplay )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$DELETE_VIEWPORT( localdisplay )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$PUT_HELP_TEXT( localdisplay, , 
        1                       'GENERAL '//string1, 'KLC$PATH:KLC.HLB')
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$PUT_CHARS( helpdisp, 
        1 'HELP                    Use arrow keys to view text, RETURN to exit'
        1               , 2, 6 )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$CREATE_VIEWPORT( localdisplay, 1, 1, 22, 80 )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$PASTE_VIRTUAL_DISPLAY( localdisplay, paste,
        1                1, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$END_PASTEBOARD_UPDATE( paste )
                        IF (.NOT.s) CALL ERROR( s )
                        GOTO 1        
                ENDIF
        END IF
        s = SMG$BEGIN_PASTEBOARD_UPDATE( paste )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$DELETE_VIRTUAL_DISPLAY( helpdisp)
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$DELETE_VIRTUAL_DISPLAY( localdisplay )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$END_PASTEBOARD_UPDATE( paste )
        IF (.NOT.s) CALL ERROR( s )
        RETURN
	END
