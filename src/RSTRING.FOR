C+
C
C VERSION:      1.2-002
C
C FACILITY:     KLC
C
C MODULE:       RSTRING
C
C ABSTRACT:     This routine will read a string from the screen with full
C               editing and stuff like that
C               The routine checks the very last character of the string
C               and if it is a "Y" then it will move the cursor
C               along one - this is so the routine works wth the CENQUIRE
C               module.  The value in status is the key pressed that caused
C               the exit.  It is an integer
C
C ENVIRONMENT:
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  20-MAR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: RSTRING v1.2'
        SUBROUTINE RSTRING( display, string, wid, x1, y1, max, keyboard,
        1       paste, status, errorc )
        IMPLICIT INTEGER (s)
        INCLUDE '($SMGDEF)'
C
        INTEGER*4       display, keyboard, wid, x, y, max, oldx, oldy, endx
        INTEGER*4       charac, d1, d2, a, x1, y1, i, j, paste, status, errorc
        INTEGER*4       endy
        CHARACTER*240   string
        CHARACTER*1     wmode
        EXTERNAL        KLC_NORMAL
        errorc = %LOC(KLC_NORMAL)
        x = x1
        y = y1
C       Set cursor to start position of data entry
        s = SMG$SET_CURSOR_ABS ( display, y, x )
        IF (.NOT.s) CALL ERROR( s )
C       wmode is the way text is entered 'I'nsert or 'O'verstrike
        wmode = 'O'
        oldx = x
        oldy = y
C+
C       endx, endy are calculated so the routine knows where it can go to
C       charac is an index to the string being edited
C-
        charac = 1
C+
C       This is the edit string loop
C       First print out the whole string in reverse
C-
1       s = SMG$SET_CURSOR_ABS ( display, oldy, oldx )
        IF (.NOT.s) CALL ERROR( s )
        endx = oldx-1
        endy = oldy
        j = 1
        d1 = oldy
        d2 = oldx
        DO 2 i=1,max
                endx = endx+1
                IF (endx.GT.wid) THEN
                        endy = endy + 1
                        endx = oldx
                s = SMG$PUT_CHARS( display, string(j:i-1), d1, d2,,
        1               SMG$M_REVERSE )
                IF (.NOT.s) CALL ERROR( s )
                d2 = endx
                d1 = endy
                j = i
                ENDIF
2       CONTINUE
        s = SMG$PUT_CHARS( display, string(j:i-1), d1, d2,,
        1               SMG$M_REVERSE )
        IF (.NOT.s) CALL ERROR( s )
C+
C       This is where the main loop starts
C       We need to check that the x,y position is correct
C-          
        IF (string(240:240).EQ.'Y') THEN
                x = x + 1
                charac = charac + 1
                string(240:240)=' '
        END IF
3       IF (x.LT.oldx) THEN
                x = wid
                y = y - 1
        END IF
        IF (x.GT.endx) THEN
                IF (y.LT.endy) THEN
                        x = oldx
                        y = y + 1
                ELSE
                        x = endx
                END IF
        END IF
        IF (y.LT.oldy) THEN
                y = oldy
        END IF
        IF (y.GT.endy) THEN
                y = endy
        END IF
        charac = x-(oldx-1)+((y-oldy)*(wid-(oldx-1)))
C       Put the cursor on the screen and then get a key stroke in variable a
        s = SMG$PUT_CHARS( display, string(charac:charac), y, x,,
        1       SMG$M_REVERSE )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$SET_CURSOR_ABS ( display, y, x )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE ( keyboard, a )
        IF (.NOT.s) CALL ERROR( s )
C       If user wants help then help them !!!
        IF ((a.EQ.SMG$K_TRM_HELP).OR.(a.EQ.SMG$K_TRM_PF2)) THEN
                CALL HELP( 'EDITING', keyboard, paste, s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                GOTO 3
        END IF
C+
C       If we press a key on the numeric keypad then the ascii equivalent
C       must be substituted
C-
        IF (a.EQ.SMG$K_TRM_KP0) a = SMG$K_TRM_ZERO
        IF (a.EQ.SMG$K_TRM_KP1) a = SMG$K_TRM_ONE
        IF (a.EQ.SMG$K_TRM_KP2) a = SMG$K_TRM_TWO
        IF (a.EQ.SMG$K_TRM_KP3) a = SMG$K_TRM_THREE
        IF (a.EQ.SMG$K_TRM_KP4) a = SMG$K_TRM_FOUR
        IF (a.EQ.SMG$K_TRM_KP5) a = SMG$K_TRM_FIVE
        IF (a.EQ.SMG$K_TRM_KP6) a = SMG$K_TRM_SIX
        IF (a.EQ.SMG$K_TRM_KP7) a = SMG$K_TRM_SEVEN
        IF (a.EQ.SMG$K_TRM_KP8) a = SMG$K_TRM_EIGHT
        IF (a.EQ.SMG$K_TRM_KP9) a = SMG$K_TRM_NINE
        IF (a.EQ.SMG$K_TRM_MINUS) a = SMG$K_TRM_DASH
        IF (a.EQ.SMG$K_TRM_PERIOD) a = SMG$K_TRM_DOT
        IF (a.EQ.SMG$K_TRM_COMMA) a = SMG$K_TRM_COMMA_CHAR
C+
C       If user pressed left arrow then update position
C       If up/down means we leave this field then exit and return the keycode
C       in status
C-
        IF (a.EQ.SMG$K_TRM_UP) THEN
                y = y - 1
                IF (y.LT.oldy) THEN
                        s = SMG$SET_CURSOR_ABS ( display, endy, endx )
                        IF (.NOT.s) CALL ERROR( s )
                        status = SMG$K_TRM_UP
                        RETURN
                ENDIF
                GOTO 3
        ENDIF
        IF( a.EQ.SMG$K_TRM_DOWN) THEN
                y = y + 1
                IF (y.GT.endy) THEN
                        s = SMG$SET_CURSOR_ABS ( display, endy, endx )
                        IF (.NOT.s) CALL ERROR( s )
                        status = SMG$K_TRM_DOWN
                        RETURN
                ENDIF
                GOTO 3
        ENDIF
        IF (a.EQ.SMG$K_TRM_LEFT) THEN
                x = x - 1
                IF ((x.LT.oldx).AND.(y.EQ.oldy)) THEN
                        s = SMG$SET_CURSOR_ABS ( display, endy, endx )
                        IF (.NOT.s) CALL ERROR( s )
                        status = SMG$K_TRM_LEFT
                        RETURN
                ENDIF
                GOTO 3
        ENDIF
C       If user pressed right arrow key then update cursor position
        IF (a.EQ.SMG$K_TRM_RIGHT) THEN
                x = x + 1
                IF ((x.GT.endx).AND.(y.EQ.endy)) THEN
                        s = SMG$SET_CURSOR_ABS ( display, endy, endx )
                        IF (.NOT.s) CALL ERROR( s )
                        status = SMG$K_TRM_RIGHT
                        RETURN
                ENDIF
                GOTO 3
        ENDIF
C       If user pressed CTRLA for toggle insert/overstrike mode
        IF (a.EQ.SMG$K_TRM_CTRLA) THEN
                IF (wmode.EQ.'I') THEN 
                        wmode = 'O'
                ELSE 
                        wmode = 'I' 
                ENDIF
                GOTO 3
        ENDIF
C+
C       If user pressed return then exit this routine if cursor on last line
C       else send cursor to the next line at the start i.e. CR LF]
C-
        IF (a.EQ.13) THEN
                IF (y.EQ.endy) THEN
                        s = SMG$SET_CURSOR_ABS ( display, endy, endx )
                        IF (.NOT.s) CALL ERROR( s )
                        status = 13
                        RETURN
                ELSE
                        x = oldx
                        y = y + 1
                ENDIF
        ENDIF
C       This next section deals with DELETE being pressed
        IF ((a.EQ.SMG$K_TRM_DELETE).AND.(charac.GT.1)) THEN
                IF (wmode.EQ.'O') THEN
                        string(charac-1:charac-1)=' '
                        x = x - 1
                        charac = charac - 1
                        IF (x.EQ.oldx-1) THEN
                                x = wid
                                y = y - 1
                        ENDIF
                        IF ((x.LT.oldx).AND.(y.LE.oldy)) THEN
                                x = oldx
                                y = oldy
                                charac = 1
                        ENDIF
                        GOTO 3
                ELSE
                        string = string(1:charac-2)//string(charac:max)//' '
                        x = x - 1
                        charac = charac - 1
                        IF (x.EQ.oldx-1) THEN
                                x = wid 
                                y = y - 1
                        ENDIF
                        IF (((x.LT.oldx).AND.(y.LE.oldy)).OR.(y.LT.oldy)) THEN
                                x = oldx
                                y = oldy
                                charac = 1
                        ENDIF
                        GOTO 1
                ENDIF
        ENDIF
C+
C       if we get here then check to see a printable character was entered and
C       insert/overstrike it into the string
C-
        IF ((a.GT.31).AND.(a.LT.126)) THEN
                IF (wmode.EQ.'O') THEN
                        s = SMG$PUT_CHARS ( display, CHAR(a), y, x,, 
        1                       SMG$M_REVERSE )
                        IF (.NOT.s) CALL ERROR( s )
                        string(charac:charac) = CHAR(a)
                        x = x + 1
                        charac = charac + 1
                        IF (x.GT.wid) THEN
                                x = oldx
                                y = y + 1
                        ENDIF
                        IF (((x.GT.endx).AND.(y.GE.endy)).OR.(y.GT.endy)) THEN
                                x = endx
                                y = endy
                                charac = max
                        ENDIF
                        GOTO 3
                ELSE
                        string = string(1:charac-1)//CHAR(a)//
        1       string(charac:max-1)
                        x = x + 1
                        charac = charac + 1
                        IF (x.GT.wid) THEN
                                x = oldx
                                y = y + 1
                        ENDIF
                        IF (((x.GT.endx).AND.(y.GE.endy)).OR.(y.GT.endy)) THEN
                                x = endx
                                y = endy   
                                charac = max
                        ENDIF
                        GOTO 1
                ENDIF
        ELSE
                GOTO 3
        ENDIF
	END
