C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       KLC
C
C ABSTRACT:     This is the main routine. It does menu interactions and
C               routine dispatching.  Written for KLC Internamtional.
C               The logical name KLC$PATH must point to the directory
C               holding all the data files for the system.
C
C ENVIRONMENT:  FORTRAN programming language, SMG$
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  17-MAR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM v1.2'
        PROGRAM KLC
        IMPLICIT INTEGER (s)
        INCLUDE '($SMGDEF)'
C
        INTEGER*4       localdisplay, pasteboard_id, maindisplay_id, men
        INTEGER*4       option, keyboard_id, keycode
        CHARACTER*10    enquiry /'NONE SET .'/
        EXTERNAL KLC_NORMAL
C                     
        CALL INIT( pasteboard_id, maindisplay_id, keyboard_id, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
1       CALL MENU( maindisplay_id, men, option, keyboard_id, enquiry, 
        1       pasteboard_id, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
C+
C       If men = 0 and option = 7 then user requested quit, ask if they are
C       sure, and if they are then quit
C-
        IF ((men.EQ.0).AND.(option.EQ.7)) THEN
                s = SMG$CREATE_VIRTUAL_DISPLAY( 5, 46, localdisplay,
        1               SMG$M_BORDER )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$PASTE_VIRTUAL_DISPLAY( localdisplay, pastebaord,
        1               10, 19)
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$PUT_CHARS( localdisplay,
        1               '   Are You Sure You Want To Quit Now (Y/N)?', 3, 1 )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$SET_PHYSICAL_CURSOR( pasteboard_id, 13, 42 )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$READ_KEYSTROKE( keyboard_id, keycode )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$DELETE_VIRTUAL_DISPLAY( localdisplay )
                IF ((keycode.EQ.89).OR.(keycode.EQ.121)) THEN
                        CALL EXITIT( pasteboard_id, maindisplay_id, 
        1               keyboard_id, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        GOTO 2
                ELSE
                        men = 0
                ENDIF
        ENDIF
C+
C       This next bit calls the customer/supplier database with the required
C       function number (1=add,2=edit,3=find)
C-
        IF (men.EQ.4) THEN
                CALL SDATABASE( option, keyboard_id, pasteboard_id,
        1        'CUSTOMER', s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        END IF
        IF (men.EQ.5) THEN
                CALL SDATABASE( option, keyboard_id, pasteboard_id,
        1        'SUPPLIER', s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        END IF
C       If we chose enquiry then do the enquiry stuff
        IF (men.EQ.1) THEN
                IF (option.EQ.1) THEN 
                        CALL CRENEWNUM( enquiry, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        CALL CENQUIRE( enquiry, keyboard_id, pasteboard_id, s)
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                ELSE
                IF (option.EQ.3) THEN
                        IF (enquiry.EQ.'NONE SET .') THEN
                                CALL SET( maindisplay_id, keyboard_id,
        1                       enquiry, s )
                                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        END IF
                        CALL REPORT( enquiry, keyboard_id, pasteboard_id, 1,
        1                        s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                ELSE
                        IF (enquiry.EQ.'NONE SET .') THEN
                                CALL SET( maindisplay_id, keyboard_id, enquiry,
        1                               s )
                                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        END IF
                        IF (option.EQ.4) THEN
                                CALL SEDIT( enquiry, keyboard_id,
        1                               pasteboard_id, s )
                        ELSE
                        IF (option.EQ.5) THEN
                                CALL OLIST( keyboard_id, pasteboard_id, s )
                                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        ELSE
                        IF (enquiry.EQ.'NONE SET .') THEN
                                CALL SET( maindisplay_id, keyboard_id, enquiry,
        1                               s )
                                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        END IF
                                CALL CENQUIRE( enquiry, keyboard_id, 
        1                               pasteboard_id, s )
                                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        ENDIF
                        ENDIF
                ENDIF
                ENDIF
        ENDIF
C       Now we do the costing/order menu routines
        IF (men.EQ.2) THEN
                IF (enquiry.EQ.'NONE SET .') THEN
                        CALL SET( maindisplay_id, keyboard_id, enquiry,
        1                       s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                ENDIF
                IF (option.EQ.1) THEN
                        CALL CENQUIRE( enquiry, keyboard_id, pasteboard_id, s)
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                ENDIF
                IF (option.EQ.2) THEN
                        CALL REPORT( enquiry, keyboard_id, pasteboard_id, 2,
        1                       s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                ENDIF
                IF (option.EQ.3) THEN
                        CALL REPORT( enquiry, keyboard_id, pasteboard_id, 3, s)
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                ENDIF
                IF (option.EQ.4) THEN
                        CALL REPORT( enquiry, keyboard_id, pasteboard_id, 4, s)
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                ENDIF
                IF (option.EQ.5) THEN
                        CALL REPORT( enquiry, keyboard_id, pasteboard_id, 5, s)
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                ENDIF
        END IF
C       The utility menu is handled next
        IF (men.EQ.3) THEN
                IF (option.EQ.1) THEN
                        CALL CREATE_FAX( keyboard_id, pasteboard_id
        1               ,enquiry, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                ENDIF
                IF (option.EQ.2) THEN
                        CALL COPY( keyboard_id, pasteboard_id, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                ENDIF
                IF (option.EQ.3) THEN
                        CALL COMMAND( keyboard_id, pasteboard_id, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                ENDIF
        END IF
        GOTO 1
c+
C       This bit llows us to set a new enquiry number
C-                                              
2       END
        SUBROUTINE SET( maindisplay_id, keyboard_id, enquiry, errorc )
        IMPLICIT INTEGER (a-z)
        INTEGER*4       maindisplay_id, keyboard_id, errorc
        CHARACTER*10    enquiry
        EXTERNAL        KLC_NORMAL
        erroc = %LOC(KLC_NORMAL)
        s = SMG$PUT_CHARS( maindisplay_id,
        1       '  Enter enquiry number :          ', 23, 23 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$SET_PHYSICAL_CURSOR( paste, 23, 47 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$READ_STRING( keyboard_id, enquiry,, 10 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( maindisplay_id,
        1       'Current Enquiry Number :'//enquiry, 23, 23 )
        IF (.NOT.s) CALL ERROR( s )
        RETURN
	END
