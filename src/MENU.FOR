C+
C
C VERSION:      1.2-003
C
C FACILITY:     KLC
C
C MODULE:       MENU
C
C ABSTRACT:     Returns the main menu number and option number of an option
C               Selected by the user.  This routine must have the main display
C               ID sent to it, and the virtual keyboard id.  We also need
C               the path to the system data. Enquiry is the current enquiry
C               number, it is a string.  If we enter with men<>0 then the 
C               appropriate menu is displayed.
C
C ENVIRONMENT:  Called by KLC module
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  17-MAR-1993
C
C MODIFIED BY:
C       
C-        
CDEC$   IDENT 'KLC SYSTEM: MENU v1.2'
        SUBROUTINE MENU( display, men, option, keyboard, enquiry, paste,
        1               errorc )
        IMPLICIT INTEGER (s)
        INCLUDE '($SMGDEF)' 
C
        INTEGER*4       ret, option, men, menudisplay, localdisplay, display
        INTEGER*4       keyboard, enqnum, last(6), keyp, paste, errorc
        CHARACTER*20    path / 'KLC$PATH:MAINMEN.PIC' /
        CHARACTER*(*)   enquiry
        EXTERNAL        KLC_NORMAL
C+    
C       The following charcter arrays hold all the menu choices
C       the intergers last(1)...last(6) hold the last subscript of each array
C-
        CHARACTER*30    choices(6,7), options(7)
        DATA            (choices(1,i),i=1,7) / 'Enquiry',
        1               'Costing/Produce Order', 'Utilities',
        1               'Customer Database','Supplier Database',
        1               'Set Enquiry Number', 'Quit' /
        1               ,(choices(2,i),i=1,7) / 'Create New Enquiry',
        1               'Edit Existing Enquiry', 'Produce Supplier Enquiry',
        1               'Edit individual Enquiry', 'Show enquiry states',
        1               'Set Enquiry Number', 'Return To Main Menu'/
        1               ,(choices(3,i),i=1,7) /'Enter Costing Figures',
        1               'Produce Quote For Customer', 'Produce Purchase Order',
        1               'Produce Invoice', 'Produce Packing List',
        1               'Set Enquiry Number',
        1               'Return To Main Menu'/
        1               ,(choices(4,i),i=1,5) /'Create Fax',
        1               'Copy Information', 'DCL Commands',
        1               'Set Enquiry Number', 'Return To Main Menu'/
        1               ,(choices(5,i),i=1,5) /'Add Customer', 'Edit Cutomer',
        1               'Find Customer', 'Set Enquiry Number',
        1               'Return To Main Menu'/
        1               ,(choices(6,i),i=1,5) /'Add Supplier', 'Edit Supplier',
        1               'Find Supplier', 'Set Enquiry Number',
        1               'Return To Main Menu'/
        DATA            (last(i),i=1,6) / 7,7,7,5,5,5 /
        errorc = %LOC(KLC_NORMAL)
C       Clear the screen of any rubbish
        s = SMG$ERASE_DISPLAY( display, 1, 1, 24, 132 )
        IF (.NOT.s) CALL ERROR( s )
C       Draw the initial selection screen
        s = SMG$LOAD_VIRTUAL_DISPLAY( localdisplay, path )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PASTE_VIRTUAL_DISPLAY( localdisplay, paste, 1, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$CREATE_VIRTUAL_DISPLAY( 7, 30, menudisplay )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PASTE_VIRTUAL_DISPLAY( menudisplay, paste, 12, 28 )
        IF (.NOT.s) CALL ERROR( s )
C       Before we use the menu display the enquiry number
        s = SMG$PUT_CHARS( localdisplay, 'Current Enquiry Number :'//
        1       enquiry, 23, 23 )
        IF (.NOT.s) CALL ERROR( s )
C+
C       This code produces the first menu - choices in choices array are
C       copied to the options array.  We loop until ret <> 0
C-
        ret = 0
        DO WHILE(ret.EQ.0)
                DO i=1,7
                        IF (i.LE.last(men+1)) THEN
                                options(i) = choices(men+1,i)
                        ELSE
                                options(i)=' '
                        END IF
                END DO
                s = SMG$CREATE_MENU( menudisplay, options, SMG$K_VERTICAL,,1 )
                IF (.NOT.s) CALL ERROR( s )
C       create the menu but get it to return when a key is pressed
                keyp = 0
                DO WHILE(keyp.NE.13)
                        s = SMG$SELECT_FROM_MENU( keyboard, menudisplay, 
        1                       option,, SMG$M_RETURN_IMMED,,, keyp )
                        IF (.NOT.s) CALL ERROR( s )
C       If the key was help then do help
                        IF ((keyp.EQ.SMG$K_TRM_PF2).OR.
        1                       (keyp.EQ.SMG$K_TRM_HELP)) THEN
                                CALL HELP( choices( men+1,option ), keyboard,
        1                               paste, s )
                                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        ENDIF
                END DO
                s = SMG$DELETE_MENU ( menudisplay )
                IF (.NOT.s) CALL ERROR( s )
                IF (option.EQ.last(men+1)-1) THEN 
                        CALL SET( localdisplay, keyboard, enquiry )
                ELSE
                        IF (option.EQ.last(men+1)) THEN
                                IF (men.EQ.0) THEN
                                        ret = 1
                                ELSE
                                        men = 0
                                END IF
                        ELSE
                                IF (men.EQ.0) THEN
                                        men = option
                                ELSE
                                        ret = 1
                                END IF
                        END IF
                END IF
        END DO
C       Get rid of the menu and return to the main procedure
        s = SMG$DELETE_VIRTUAL_DISPLAY( menudisplay )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$DELETE_VIRTUAL_DISPLAY( localdisplay )
        IF (.NOT.s) CALL ERROR( s )
        RETURN
	END
