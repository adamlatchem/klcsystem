C+
C       The next routine is used to read in the last line of an open tag file.
C       This will contain information about the position of fields in the file
C       and formatting procedures.
C-
CDEC$   IDENT 'KLC SYSTEM: GETFORMAT v1.2'
        SUBROUTINE GETFORMAT( unitn, string, errorc )
        INTEGER*4       unitn, errorc
        CHARACTER*80    string
        EXTERNAL        KLC_NORMAL
        errorc = %LOC(KLC_NORMAL)
        READ( UNIT=unitn, FMT='(A80)', ERR=2 )string
1       RETURN
C       If we get here there was an error reading the file
2       errorc = %LOC(KLC_GTFFDNS)
        GOTO 1
        END
