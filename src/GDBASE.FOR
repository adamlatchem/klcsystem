C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       GDBASE
C
C ABSTRACT:     Called by a database so you enter data.
C
C ENVIRONMENT:  needs a display-id and variables for returns.  This routine
C               will kill off most screen lines except the first 4 lines of the
C               screen.
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  27-MAR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: GDBASE v1.2'
        SUBROUTINE GDBASE( localdisplay, short, company, contact1, contact2,
        1       fax, tel, address, title1, title2, keyboard, paste, errorc )
        IMPLICIT INTEGER( i-s )
        INCLUDE '($SMGDEF)'
C
        INTEGER*4       localdisplay, keyboard, paste, status, errorc
        CHARACTER*240   address, contact1*30, contact2*30, company*40, fax*20
        CHARACTER*20    tel, short*7, string*240, title1*30, title2*30
        EXTERNAL        KLC_NORMAL
        errorc = %LOC(KLC_NORMAL)
C       'string' must be full of spaces to display a blocked field.
        DO i=1,240
                string(i:i)=' ' 
        END DO
C       Tidy the screen ready for output of the fields
        DO i=5,20
                s = SMG$ERASE_LINE ( localdisplay, i, 1 )
                IF (.NOT.s) CALL ERROR( s )
        END DO
        s = SMG$PUT_CHARS( localdisplay, '         Short code:', 5, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, '       Company name:', 6, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, '     Contact 1 Name:', 7, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, '              Title:', 8, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, '     Contact 2 Name:', 9, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, '              Title:', 10, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, '         Fax Number:', 11, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, '   Telephone Number:', 12, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, '            Address:', 13, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
1       string( 1:7 )=short( 1:7 )
        CALL RSTRING( localdisplay, string, 80, 21, 5, 7, keyboard, paste,
        1       status, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        IF ( string(1:7).EQ.'      ' ) THEN
                s = SMG$RING_BELL ( localdisplay )
                IF ( .NOT.s ) CALL ERROR( s )
                s = SMG$PUT_CHARS( localdisplay,
        1       'Field cannot be blank                      ', 4, 1
        1       ,, SMG$M_REVERSE )
                IF ( .NOT.s ) CALL ERROR( s )
                GOTO 1
        ENDIF
        CALL UCASE( string(1:7), s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        short = string(1:7)
        IF (short(1:4).EQ.'QUIT') RETURN
        IF ((status.EQ.SMG$K_TRM_UP).OR.(status.EQ.SMG$K_TRM_LEFT)) GOTO 1
2       string(1:40)=company(1:40)
        CALL RSTRING( localdisplay, string, 80, 21, 6, 40, keyboard, paste,
        1       status, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        company = string(1:40)
        IF (company(1:4).EQ.'QUIT') THEN
                short(1:4)='QUIT'
                RETURN
        ENDIF
        IF ((status.EQ.SMG$K_TRM_UP).OR.(status.EQ.SMG$K_TRM_LEFT)) GOTO 1
3       string(1:30)=contact1(1:30)
        CALL RSTRING( localdisplay, string, 80, 21, 7, 30, keyboard, paste,
        1       status, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        IF ((status.EQ.SMG$K_TRM_UP).OR.(status.EQ.SMG$K_TRM_LEFT)) GOTO 2
        contact1 = string(1:30)
        IF (contact1(1:4).EQ.'QUIT') THEN
                short(1:4)='QUIT'
                RETURN
        ENDIF
4       string(1:30)=title1(1:30)
        CALL RSTRING( localdisplay, string, 80, 21, 8, 30, keyboard, paste,
        1       status, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        title1 = string(1:30)
        IF (title1(1:4).EQ.'QUIT') THEN
                short(1:4)='QUIT'
                RETURN
        ENDIF
        IF ((status.EQ.SMG$K_TRM_UP).OR.(status.EQ.SMG$K_TRM_LEFT)) GOTO 3
5       string(1:30)=contact2(1:30)
        CALL RSTRING( localdisplay, string, 80, 21, 9, 30, keyboard, paste,
        1       status, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        contact2 = string(1:30)
        IF (contact2(1:4).EQ.'QUIT') THEN
                short(1:4)='QUIT'
                RETURN
        ENDIF
        IF ((status.EQ.SMG$K_TRM_UP).OR.(status.EQ.SMG$K_TRM_LEFT)) GOTO 4
6       string(1:30)=title2(1:30)
        CALL RSTRING( localdisplay, string, 80, 21, 10, 30, keyboard, paste,
        1       status, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        title2 = string(1:30)
        IF (title2(1:4).EQ.'QUIT') THEN
                short(1:4)='QUIT'
                RETURN
        ENDIF
        IF ((status.EQ.SMG$K_TRM_UP).OR.(status.EQ.SMG$K_TRM_LEFT)) GOTO 5
7       string(1:20)=fax(1:20)
        CALL RSTRING( localdisplay, string, 80, 21, 11, 20, keyboard, paste,
        1       status, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        fax = string(1:20)
        IF (fax(1:4).EQ.'QUIT') THEN
                short(1:7)='QUIT'
                RETURN
        ENDIF
        IF ((status.EQ.SMG$K_TRM_UP).OR.(status.EQ.SMG$K_TRM_LEFT)) GOTO 6
8       string(1:20)=tel(1:20)
        CALL RSTRING( localdisplay, string, 80, 21, 12, 20, keyboard, paste,
        1       status, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        tel = string(1:20)
        IF (tel(1:4).EQ.'QUIT') THEN
                short(1:4)='QUIT'
                RETURN
        ENDIF
        IF ((status.EQ.SMG$K_TRM_UP).OR.(status.EQ.SMG$K_TRM_LEFT)) GOTO 7
9       string(1:240)=address(1:240)
        CALL RSTRING( localdisplay, string, 60, 21, 13, 240, keyboard, paste,
        1       status, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        address = string(1:240)
        IF (address(1:4).EQ.'QUIT') THEN
                short(1:4)='QUIT'
                RETURN
        ENDIF
        IF ((status.EQ.SMG$K_TRM_UP).OR.(status.EQ.SMG$K_TRM_LEFT)) GOTO 8
        IF (status.NE.13) GOTO 9
        RETURN
	END
