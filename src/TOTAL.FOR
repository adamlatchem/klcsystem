C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       TOTAL
C
C ABSTRACT:     This will recalculate and display the total cost to the
C               customer and more importantly the profit made by KLC on
C               the current enquiry
C
C ENVIRONMENT:  The two arrays holding figures for all 1000 lines are required
C               By this routine to calculate the new totals and the display id
C               is used to display the results
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  17-APR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: TOTAL v1.2'
        SUBROUTINE TOTAL( costklc, costcust, display, errorc )
        IMPLICIT INTEGER( a-z )
C
        REAL            costcust(1000), costklc(1000), tot, profit
        INTEGER*4       display, errorc
        CHARACTER*10    datastring
C
        EXTERNAL        KLC_NORMAL, KLC_TCPTOT, KLC_TCPROF
        errorc = %LOC(KLC_NORMAL)
C       First add up the totals
        tot = 0.0
        profit = 0.0
        DO i=1,1000
                tot = tot + costcust(i)
                profit = profit + (costcust(i)-costklc(i))
        END DO
C       Now convert to character data and display on the screen
1       FORMAT(BNF10.2)
        DO i=1,10
                datastring(i:i)=' '
        END DO
        ENCODE( 10, 1, datastring, ERR=3 ) tot
        s = SMG$PUT_CHARS( display, 'TOTAL COST :'//datastring, 23, 58)
        IF (.NOT.s) CALL ERROR( s )
        ENCODE( 10, 1, datastring, ERR=4 ) profit
        s = SMG$PUT_CHARS( display, 'KLC PROFIT :'//datastring, 24, 58 )
        IF (.NOT.s) CALL ERROR( s )
2       RETURN
C       Come here if there was an error converting the total.
3       errorc = %LOC(KLC_TCPTOT)
        GOTO 2
C       Come here if there was an error converting the profit value.
4       errorc = %LOC(KLC_TCPROF)
        GOTO 2
	END
