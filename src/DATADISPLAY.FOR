C+
C
C VERSION:      1.2-002
C
C FACILITY:     KLC
C
C MODULE:       DATADISPLAY
C
C ABSTRACT:     This will display a form for the database routines
C
C ENVIRONMENT:  KLC system database routines - SDATABASE, CDATABASE
C               We nned a SMG display id, y position, and strings holding
C               the contact name, company name, address, fax and telephone 
C               numbers.  A box will be drawn
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  21-MAR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: DATADISPLAY v1.2'
        SUBROUTINE DATADISPLAY( localdisplay, y, short, contact1, contact2, 
        1       company, address, fax, tel, title1, title2, errorc )
        IMPLICIT INTEGER( a-z )
        INCLUDE '($SMGDEF)'
C
        INTEGER*4       y, localdisplay, errorc
        CHARACTER*20    fax, tel, address*240, company*40, contact1*30
        CHARACTER*70    pa, short*7, contact2*30, title1*30, title2*30
        EXTERNAL        KLC_NORMAL
        errorc = %LOC(KLC_NORMAL)
C
        s = SMG$BEGIN_DISPLAY_UPDATE( localdisplay )
        IF ( .NOT.s ) CALL ERROR( s )
C       The next line just makes sure there are no error messages on screen
        s = SMG$ERASE_LINE( localdisplay, 4, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$DRAW_LINE( localdisplay, y, 1, y, 79 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$DRAW_LINE( localdisplay, y, 1, y+12, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$DRAW_LINE( localdisplay, y+12, 1, y+12, 79 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$DRAW_LINE( localdisplay, y+12, 79, y, 79 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, ' Found :'//short//' ', y, 10 )
        IF ( .NOT.s ) CALL ERROR( s )  
        s = SMG$PUT_CHARS( localdisplay, ' Contact 1:'//contact1, y+1, 3 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, title1, y+1, 48 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, ' Contact 2:'//contact2, y+2, 3)
        IF ( .NOT.s ) CALL ERROR( s ) 
        s = SMG$PUT_CHARS( localdisplay, title2, y+2, 48 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, '  Company :'//company, y+3, 3 )
        IF ( .NOT.s ) CALL ERROR( s )                  
        DO i=0,5
                pa = '          '//address((i*40)+1:(i+1)*40)
                a = y+4+i
                s = SMG$PUT_CHARS( localdisplay, pa, a, 3 )
                IF ( .NOT.s ) CALL ERROR( s )
        END DO
        s = SMG$PUT_CHARS( localdisplay, '      Fax :'//fax, y+10, 3 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, '      Tel :'//tel, y+11, 3 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$END_DISPLAY_UPDATE( localdisplay )
        IF ( .NOT.s ) CALL ERROR( s )
        RETURN
	END
