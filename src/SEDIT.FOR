C+
C
C VERSION:      1.2-002
C
C FACILITY:     KLC
C
C MODULE:       SEDIT
C
C ABSTRACT:     This allows a user to edit a file created by the supplier
C               enquiry function.  After editing it may then be printed
C
C ENVIRONMENT:  The curretn enquiry number is required, a keyboard id and
C               a pasteboard id
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  7-APR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: SEDIT v1.2'
        SUBROUTINE SEDIT( enquiry, keyboard, paste, errorc )
        IMPLICIT INTEGER (a-z)
C
        CHARACTER*10    enquiry, string*240
        INTEGER*4       keyboard, paste, localdisplay, status, errorc
        EXTERNAL        KLC_NORMAL, KLC_SSELNFIL
        errorc = %LOC(KLC_NORMAL)
        DO i=1,240
                string(i:i)=' '
        END DO
C       Greetings ...
        CALL INITSCREEN( localdisplay,
        1       'KLC - Edit Supplier Enquiry', paste, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'Current Enquiry Number :'//enquiry,
        1               3, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 
        1       'Enter data or "QUIT" and press RETURN.', 4, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'Enter shortcode of supplier :', 5, 1)
        IF (.NOT.s) CALL ERROR( s )
1       CALL RSTRING( localdisplay, string, 80, 30, 5, 7, keyboard, paste,
        1       status, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        IF (status.NE.13) GOTO 1
        CALL UCASE( string(1:7), s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        IF (string(1:4).EQ.'QUIT') GOTO 2
C       If the user wants to quit then quit otherwise open the file
        OPEN( UNIT=1, FILE='KLC$PATH:'//enquiry//'SUP'//string(1:7)//'.LIS', 
        1       ERR=3, CARRIAGECONTROL='LIST', STATUS='OLD' )
        CLOSE( UNIT=1 )
        i = 1
        DO WHILE(string(i:i).NE.' ')
                i = i + 1
        END DO
        string(i:i+3)='.LIS'
        s = TPU$EDIT( '/SECTION=KLC$PATH:KLC_EDIT '//
        1  '/INIT=KLC$PATH:EDITINIT KLC$PATH:'//enquiry//'SUP'//string(1:12) )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$REPAINT_SCREEN( paste )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 
        1       'Press "P" to print or any key to continue ...', 7, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, a )
        IF (.NOT.s) CALL ERROR( s )
        IF ((a.EQ.80).OR.(a.EQ.112)) THEN
                OPEN( UNIT=1, FILE='KLC$PATH:'//enquiry//'S'//string(1:12),
        1               CARRIAGECONTROL='LIST', STATUS='OLD' )
                CLOSE( UNIT=1, DISPOSE='PRINT' )
        END IF
2       s = SMG$DELETE_VIRTUAL_DISPLAY( localdisplay )
        IF (.NOT.s) CALL ERROR( s )
        RETURN
C       If we get here then the file we want does not exist, this is a problem!
3       s = SMG$PUT_CHARS( localdisplay,
        1       'The enquiry you specified or the short code does not exist.'
        1       , 5, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay,
        1       'Try a new enquiry number or use the Produce Supplier
        1 Enquiry function.', 6, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'Press any key to continue ...'
        1       , 7, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, a )
        IF (.NOT.s) CALL ERROR( s )
        errorc = %LOC(KLC_SSELNFIL)
        GOTO 2
	END
