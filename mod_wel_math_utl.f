C  ------------------------------------------------------------------- C
C  -----------------  Convert string to upper case  ------------------ C
C  ------------------------------------------------------------------- C
      SUBROUTINE UPPERCASE(STR)
      CHARACTER*(*) STR

      do I=1,LEN(STR)
        if ('a'.le.STR(I:I).and.STR(I:I).le.'z')
     |    STR(I:I) = CHAR(ICHAR(STR(I:I))-32)
      enddo
      END
      SUBROUTINE IZREAD(IZONE,NLAY,NROW,NCOL,INZN1)
C     ******************************************************************
C     ROUTINE TO INPUT 3-D ZONE MATRIX, IZONE
C       INZN1 IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C        SPECIFICATIONS:
      DIMENSION IZONE(NCOL,NROW,NLAY)
      CHARACTER*20 FMTIN
      CHARACTER*80 NAME,NAMPRV,LINE
      CHARACTER*10 LOCAT
C     ------------------------------------------------------------------
      INZN2 = 99; IOUT = 6
      NAMPRV=' '
      DO 1000 K=1,NLAY
C
C-----READ ARRAY CONTROL RECORD.
      READ(INZN1,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,0,INZN1)
      LOCAT=LINE(ISTART:ISTOP)
C
C-----USE LOCAT TO SEE WHERE ARRAY VALUES COME FROM.
      IF(LOCAT.NE.'CONSTANT') GO TO 90
C
C-----LOCAT='CONSTANT' -- SET ALL ARRAY VALUES EQUAL TO ICONST.
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICONST,R,0,INZN1)
      DO 80 I=1,NROW
      DO 80 J=1,NCOL
80    IZONE(J,I,K)=ICONST
      WRITE(IOUT,*)
      WRITE(IOUT,83) ICONST,K
83    FORMAT(13X,'Zone Array =',I4,' for layer',I4)
      IF(ICONST.LT.0) THEN
         WRITE(*,*) ' NEGATIVE ZONE NUMBER IS NOT ALLOWED'
         STOP
      END IF
      GO TO 1000
C
C-----Get FMTIN and IPRN -- there may be an unused value for ICONST
C-----in columns 11-20 if the file is an old file.
90    CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,I,R,0,INZN1)
      IF(LINE(ISTART:ISTART).NE.'(' ) THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,I,R,0,INZN1)
        IF(LINE(ISTART:ISTART).NE.'(' ) THEN
          WRITE(*,91) LINE
91        FORMAT(1X,
     1   'Format for reading zone array does not contain "(":',/1X,A)
          STOP
        END IF
      END IF
      FMTIN=LINE(ISTART:ISTOP)
C-----Blank inside parentheses indicates free format
      NC=ISTOP-ISTART-1
      IF(NC.LE.0) THEN
         FMTIN=' '
      ELSE
        IF(LINE(ISTART+1:ISTOP-1).EQ.' ') FMTIN=' '
      END IF
C
C-----LOCAT SHOULD BE EITHER 'EXTERNAL' OR 'INTERNAL'
C-----IF 'INTERNAL', READ ARRAY FROM SAME FILE
      IF(LOCAT.EQ.'INTERNAL') THEN
         INUNIT=INZN1
         WRITE(IOUT,*)
         WRITE(IOUT,92) K
92       FORMAT(1X,'Zone Array for layer',I4,
     1      ' will be read from the Zone File')
C
C-----IF 'EXTERNAL', OPEN A SEPARATE FILE
      ELSE IF(LOCAT.EQ.'EXTERNAL') THEN
         READ(INZN1,'(A)') NAME
         INUNIT=INZN2
         WRITE(IOUT,*)
         WRITE(IOUT,93) K,NAME
93       FORMAT(1X,'Zone Array for layer',I4,
     1         ' will be read from file:'/1X,A)
         IF(NAME.NE.NAMPRV) THEN
            IF(NAMPRV.NE.' ') CLOSE(UNIT=INUNIT)
            OPEN(UNIT=INUNIT,FILE=NAME,STATUS='OLD')
            WRITE(IOUT,96)
96          FORMAT(1X,'The file was opened successfully.')
            NAMPRV=NAME
         ELSE
            WRITE(IOUT,97)
97          FORMAT(1X,'This file is already open -- will continue readi
     1ng from the current location.')
         END IF
C
C-----LOCAT IS INVALID
      ELSE
         WRITE(*,*) ' INVALID LOCAT IN ARRAY CONTROL RECORD:',LOCAT
         STOP
      END IF
C
C-----LOCAT>0 -- READ RECORDS USING FREE-FORMAT OR FMTIN.
      IF(FMTIN.EQ.' ') THEN
         WRITE(IOUT,98) K
98       FORMAT(1X,'Zone Array for layer',I4,
     1       ' will be read using free format.'/1X,55('-'))
         DO 100 I=1,NROW
         READ(INUNIT,*) (IZONE(J,I,K),J=1,NCOL)
100      CONTINUE
      ELSE
         WRITE(IOUT,104) K,FMTIN
104      FORMAT(1X,'Zone Array for layer',I4,
     1       ' will be read using format: ',A/1X,71('-'))
         DO 110 I=1,NROW
         READ (INUNIT,FMTIN) (IZONE(J,I,K),J=1,NCOL)
110      CONTINUE
      END IF
C
C-----CHECK FOR NEGATIVE IZONE VALUES
320   DO 400 I=1,NROW
      DO 400 J=1,NCOL
      IF(IZONE(J,I,K).LT.0) THEN
         WRITE(*,*) ' NEGATIVE ZONE AT (LAYER,ROW,COLUMN):',K,I,J
         STOP
      END IF
400   CONTINUE

1000  CONTINUE
      IF(NAMPRV.NE.' ') CLOSE(UNIT=INZN2)
C
C-----RETURN
      RETURN
      END
      
      SUBROUTINE URWORD(LINE,ICOL,ISTART,ISTOP,NCODE,N,R,IOUT,IN)
C     ******************************************************************
C     ROUTINE TO EXTRACT A WORD FROM A LINE OF TEXT, AND OPTIONALLY
C     CONVERT THE WORD TO A NUMBER.
C        ISTART AND ISTOP WILL BE RETURNED WITH THE STARTING AND
C          ENDING CHARACTER POSITIONS OF THE WORD.
C        THE LAST CHARACTER IN THE LINE IS SET TO BLANK SO THAT IF ANY
C          PROBLEMS OCCUR WITH FINDING A WORD, ISTART AND ISTOP WILL
C          POINT TO THIS BLANK CHARACTER.  THUS, A WORD WILL ALWAYS BE
C          RETURNED UNLESS THERE IS A NUMERIC CONVERSION ERROR.  BE SURE
C          THAT THE LAST CHARACTER IN LINE IS NOT AN IMPORTANT CHARACTER
C          BECAUSE IT WILL ALWAYS BE SET TO BLANK.
C        A WORD STARTS WITH THE FIRST CHARACTER THAT IS NOT A SPACE OR
C          COMMA, AND ENDS WHEN A SUBSEQUENT CHARACTER THAT IS A SPACE
C          OR COMMA.  NOTE THAT THESE PARSING RULES DO NOT TREAT TWO
C          COMMAS SEPARATED BY ONE OR MORE SPACES AS A NULL WORD.
C        FOR A WORD THAT BEGINS WITH "'", THE WORD STARTS WITH THE
C          CHARACTER AFTER THE QUOTE AND ENDS WITH THE CHARACTER
C          PRECEDING A SUBSEQUENT QUOTE.  THUS, A QUOTED WORD CAN
C          INCLUDE SPACES AND COMMAS.  THE QUOTED WORD CANNOT CONTAIN
C          A QUOTE CHARACTER.
C        IF NCODE IS 1, THE WORD IS CONVERTED TO UPPER CASE.
C        IF NCODE IS 2, THE WORD IS CONVERTED TO AN INTEGER.
C        IF NCODE IS 3, THE WORD IS CONVERTED TO A REAL NUMBER.
C        NUMBER CONVERSION ERROR IS WRITTEN TO UNIT IOUT IF IOUT IS
C          POSITIVE; ERROR IS WRITTEN TO DEFAULT OUTPUT IF IOUT IS 0;
C          NO ERROR MESSAGE IS WRITTEN IF IOUT IS NEGATIVE.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*(*) LINE
      CHARACTER*20 STRING
      CHARACTER*30 RW
      CHARACTER*1 TAB
C     ------------------------------------------------------------------
      TAB=CHAR(9)
C
C1------Set last char in LINE to blank and set ISTART and ISTOP to point
C1------to this blank as a default situation when no word is found.  If
C1------starting location in LINE is out of bounds, do not look for a
C1------word.
      LINLEN=LEN(LINE)
      LINE(LINLEN:LINLEN)=' '
      ISTART=LINLEN
      ISTOP=LINLEN
      LINLEN=LINLEN-1
      IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
C
C2------Find start of word, which is indicated by first character that
C2------is not a blank, a comma, or a tab.
      DO 10 I=ICOL,LINLEN
      IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.','
     &    .AND. LINE(I:I).NE.TAB) GO TO 20
10    CONTINUE
      ICOL=LINLEN+1
      GO TO 100
C
C3------Found start of word.  Look for end.
C3A-----When word is quoted, only a quote can terminate it.
20    IF(LINE(I:I).EQ.'''') THEN
         I=I+1
         IF(I.LE.LINLEN) THEN
            DO 25 J=I,LINLEN
            IF(LINE(J:J).EQ.'''') GO TO 40
25          CONTINUE
         END IF
C
C3B-----When word is not quoted, space, comma, or tab will terminate.
      ELSE
         DO 30 J=I,LINLEN
         IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.','
     &    .OR. LINE(J:J).EQ.TAB) GO TO 40
30       CONTINUE
      END IF
C
C3C-----End of line without finding end of word; set end of word to
C3C-----end of line.
      J=LINLEN+1
C
C4------Found end of word; set J to point to last character in WORD and
C-------set ICOL to point to location for scanning for another word.
40    ICOL=J+1
      J=J-1
      IF(J.LT.I) GO TO 100
      ISTART=I
      ISTOP=J
C
C5------Convert word to upper case and RETURN if NCODE is 1.
      IF(NCODE.EQ.1) THEN
         IDIFF=ICHAR('a')-ICHAR('A')
         DO 50 K=ISTART,ISTOP
            IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z')
     1             LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
50       CONTINUE
         RETURN
      END IF
C
C6------Convert word to a number if requested.
100   IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
         RW=' '
         L=30-ISTOP+ISTART
         IF(L.LT.1) GO TO 200
         RW(L:30)=LINE(ISTART:ISTOP)
         IF(NCODE.EQ.2) READ(RW,'(I30)',ERR=200) N
         IF(NCODE.EQ.3) READ(RW,'(F30.0)',ERR=200) R
      END IF
      RETURN
C
C7------Number conversion error.
200   IF(NCODE.EQ.3) THEN
         STRING= 'A REAL NUMBER'
         L=13
      ELSE
         STRING= 'AN INTEGER'
         L=10
      END IF
C
C7A-----If output unit is negative, set last character of string to 'E'.
      IF(IOUT.LT.0) THEN
         N=0
         R=0.
         LINE(LINLEN+1:LINLEN+1)='E'
         RETURN
C
C7B-----If output unit is positive; write a message to output unit.
      ELSE IF(IOUT.GT.0) THEN
         IF(IN.GT.0) THEN
            WRITE(IOUT,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
         ELSE
            WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
         END IF
201      FORMAT(1X,/1X,'FILE UNIT ',I4,' : ERROR CONVERTING "',A,
     1       '" TO ',A,' IN LINE:',/1X,A)
202      FORMAT(1X,/1X,'KEYBOARD INPUT : ERROR CONVERTING "',A,
     1       '" TO ',A,' IN LINE:',/1X,A)
C
C7C-----If output unit is 0; write a message to default output.
      ELSE
         IF(IN.GT.0) THEN
            WRITE(*,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
         ELSE
            WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
         END IF
      END IF
C
C7D-----STOP after writing message.
      STOP
      END