
SUBROUTINE emgout(Rbuf,Dbuf,Lbuf,Eoe,Dict,File,Inprec)
   IMPLICIT NONE
   LOGICAL Anycon , Error , Heat
   INTEGER Dictn(3) , Elid , Eltype , Estid , Flags(3) , Icmbar , Icong , Icore , Icstm , Idit , Ihmat , Imat , Jcore , Kflags(3) , &
         & Ksystm(65) , Lcong , Lcstm , Ldict , Lhmat , Lmat , Matrix(3) , Misc(5) , Ncong , Ncore , Ncstm , Ndit , Nhmat , Nlocs , &
         & Nmat , Nok4gg , Nval(3) , Outpt , Precis , Z(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   REAL Xxxxx(3)
   COMMON /blank / Xxxxx , Nok4gg
   COMMON /emgdic/ Eltype , Ldict , Nlocs , Elid , Estid
   COMMON /emgfil/ Misc , Matrix , Dictn
   COMMON /emgprm/ Icore , Jcore , Ncore , Icstm , Ncstm , Imat , Nmat , Ihmat , Nhmat , Idit , Ndit , Icong , Ncong , Lcong ,      &
                 & Anycon , Flags , Precis , Error , Heat , Icmbar , Lcstm , Lmat , Lhmat , Kflags
   COMMON /iemgot/ Nval
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Z
   INTEGER Eoe , File , Inprec , Lbuf
   DOUBLE PRECISION Dbuf(Lbuf)
   INTEGER Dict(5)
   REAL Rbuf(Lbuf)
   DOUBLE PRECISION da
   INTEGER eor , fredms(3) , i , i1 , i2 , iadd , icrq , ifff , igrids , iloc , iprime , itab , itemp , j , k , locs(3) , maxfil ,  &
         & n2word , noeor , nwords , part(3) , qfile
   REAL fff , ra
!
!     THIS ROUTINE OF THE -EMG- MODULE WRITES THE DATA IN -BUF- TO
!     -FILE-.
!
!     BEFORE CALLING THIS ROUTINE THE CALLING ROUTINE SETS UP THE
!     FOLLOWING ARGUMENTS.....
!
!     RBUF,DBUF  =  BOTH POINT TO THE SAME MATRIX ARRAY CONTAINING THE
!                   MATRIX DATA TO BE OUTPUT.
!
!     LBUF   = NUMBER OF DATA VALUES TO BE OUTPUT ON CURRENT CALL NOT
!              CONSIDERING THE PRECISION OF THE DATA VALUES.
!
!     EOE    = SEE BELOW.
!
!     DICT   = ARRAY OF SIZE NLOCS (SEE BELOW) + 5.  WORDS 1 THROUGH 5
!              OF THIS ARRAY ARE SET BY THE CALLING ROUITNE.(SEE BELOW)
!
!     FILE   = SET TO 1 IF STIFFNESS MATRIX
!              SET TO 2 IF MASS MATRIX
!              SET TO 3 IF DAMPING MATRIX
!
!     INPREC = PRECISION OF THE DATA  RBUF AND DBUF POINT TO.
!              SET = 1 IF SINGLE AND SET TO 2 IF DOUBLE.
!
!     --- IMPORTANT--- UNDER NO CIRCUMSTANCES SHOULD THE CALLING PROGRAM
!     MODIFY DATA IN COMMON BLOCK /EMGDIC/.
!
!     NOTE.  ON EACH CALL TO THIS ROUTINE THE CALLING ROUTINE MUST SEND
!     AN AMOUNT OF DATA FOR ONE OR MORE GRID POINT-PARTITIONS OF THE
!     TOTAL ELEMENT MATRIX.  THIS IS CONSIDERING DEGREES OF FREEDOM AND
!     ANY CONDENSATION OF THE DATA IF A DIAGONAL MATRIX IS BEING SENT.)
!
!     IE. FOR A MATRIX WHERE 6 DEGREES OF FREEDOM ARE LISTED IN THE CODE
!     WORD OF THE DICTIONARY THEN THE FOLLWING WOULD BE CONSIDERED.
!
!     IF THE MATRIX WAS DIAGONAL THE VALUE OF LBUF WOULD BE SIMPLY
!     (0 TO NLOCS) TIMES 6
!
!     IF THE MATRIX WAS SQUARE THEN LBUF WOULD BE
!     (0 TO NLOCS) TIMES 6 TIMES NLOCS TIMES 6.  NOTE THE PRECISION DOES
!     NOT ENTER INTO THE CALCULATION OF LBUF BY THE CALLING ROUTINE)
!
!     IF -EOE- IS GREATER THAN 0, INDICATING END-OF-ELEMENT-DATA, THE
!     DICTIONARY WILL BE WRITTEN TO THE APPROPRIATE COMPANION FILE
!     OF -FILE-.  IF CONGRUENT LOGIC IS ACTIVE THE DICTIONARY WILL
!     ALSO BE PLACED IN CORE IF POSSIBLE.
!
!     CHECKS TO INSURE THAT THE CALLING ROUTINE SENT A REASONABLY
!     CORRECT AMOUNT OF MATRIX DATA ARE MADE BY THIS ROUTINE.
!
!     DICTIONARY FORMAT 1)ELEMENT-COUNTER-ID-POSITON-IN-EST
!     ================= 2)F = 1 IF SQUARE, = 2 IF DIAGONAL FORMAT DATA.
!     DICTIONARY        3)N = NUMBER OF CONNECTED GRID POINTS * FREEDOMS
!     CAN EXPAND        4)COMPONENT-CODE-WORD
!     THROUGH THE       5)DAMPING CONSTANT
!     MIDDLE.                    .
!           LDICT-NLOC+1)GINO-LOC. (FIRST PARTITION)
!                                .
!                                .
!                                . (NLOCS GINO-LOC VALUES)
!                                .
!                LDICT-1)GINO-LOC. (NEXT TO LAST PARTITION)
!                  LDICT)GINO-LOC. (LAST PARTITION OF THIS ELEMENT IF
!                                  ALL -NLOCS- GRID POINTS CONNECTED.)
!
!     THIS ROUTINE WILL WRITE PARTITIONS OF THE MATRIX WHERE THE NUMBER
!     OF COLUMNS IN EACH PARTITION WRITTEN EQUALS ACTIVE FREEDOMS
!     WHICH = NUMBER OF BITS ON IN THE CODE WORD ( DICT(4) ).
!
!     THE VALUES OF DICT(1), DICT(2), DICT(3), DICT(4), AND DICT(5) MUST
!     REMAIN CONSTANT BETWEEN CALLS TO THIS ROUTINE WITH RESPECT TO
!     A PARTICULAR ELEMENT ID AND FILE TYPE.
!
!     ONE OR MORE PARTITIONS WILL BE WRITTEN ON EACH CALL.
!
   EQUIVALENCE (Ksystm(2),Outpt) , (fff,ifff)
   DATA eor , noeor , maxfil/1 , 0 , 3/
!
   IF ( Error ) RETURN
   IF ( File>=1 .AND. File<=maxfil ) THEN
!
!     ON FIRST CALL TO THIS ROUTINE FOR THIS ELEMENT THE SIZE OF COLUMN
!     AND SIZE OF PARTITION BEING WRITTEN IS SET.
!     IF NVAL(FILE) .GT. 0 THEN THIS IS NOT THE FIRST CALL.
!
      IF ( Kflags(File)==0 ) RETURN
      IF ( Nval(File)>0 ) GOTO 200
      Nval(File) = Dict(3)
!
!     DETERMINE NUMBER OF ACTIVE FREEDOMS BY COUNTING BITS ON IN CODE
!     WORD.  THIS CODE ADDED AS AN TEMPORARY NECESSITY.
!
      IF ( Dict(4)==63 ) THEN
!
         itemp = 6
      ELSE
         i = Dict(4)
         itemp = 0
         DO j = 1 , 31
            IF ( mod(i,2)>0 ) itemp = itemp + 1
            i = i/2
            IF ( i<=0 ) EXIT
         ENDDO
      ENDIF
      fredms(File) = itemp
!
!     CHECK NUMBER OF ACTIVE GRID POINTS FOR THIS ELEMENT TO BE
!     LESS THAN OR EQUAL TO NLOCS.
!
      igrids = Dict(3)/fredms(File)
      IF ( igrids<=Nlocs ) THEN
!
         locs(File) = Ldict - Nlocs
!
!     ZERO ALL GINO-LOC SLOTS IN DICTIONARY.
!
         i = Ldict - Nlocs + 1
         DO j = i , Ldict
            Dict(j) = 0
         ENDDO
         IF ( Dict(2)==1 ) THEN
!
!     FULL SQUARE MATRIX WILL BE OUTPUT. (VALUES PER PARTITION TO WRITE)
!
            part(File) = Dict(3)*fredms(File)
            GOTO 200
         ELSEIF ( Dict(2)==2 ) THEN
!
!     DIAGONAL MATRIX.  (VALUES PER PARTITION TO WRITE)
!
            part(File) = fredms(File)
            GOTO 200
         ELSE
            WRITE (Outpt,99001) Sfm , Dict(2) , Elid
99001       FORMAT (A25,' 3109, EMGOUT HAS BEEN SENT AN INVALID DICTIONARY ','WORD-2 =',I10,' FROM ELEMENT ID =',I10)
         ENDIF
      ELSE
         WRITE (Outpt,99002) Sfm , igrids , Elid
99002    FORMAT (A25,' 3122,  EMGOUT HAS DETERMINED THAT THERE ARE',I10,' CONNECTING GRID POINTS FOR ELEMENT ID =',I10,/5X,         &
                &'THIS IS GREATER THAN THE MAXIMUM AS PER /GPTA1/ TABLE ','FOR THE TYPE OF THIS ELEMENT. PROBABLE ERROR IN ELEMENT',&
                &' ROUTINE PROGRAM')
      ENDIF
   ELSE
!
!     ILLEGAL FILE VALUE
!
      WRITE (Outpt,99003) Sfm , File
99003 FORMAT (A25,' 3108, EMGOUT RECEIVES ILLEGAL FILE TYPE =',I10)
   ENDIF
 100  Error = .TRUE.
   RETURN
!
!     WRITE MATRIX DATA TO FILE DESIRED.
!
 200  nwords = part(File)
   IF ( mod(Lbuf,nwords)/=0 ) THEN
      WRITE (Outpt,99004) Sfm , Elid
99004 FORMAT (A25,' 3110, EMGOUT HAS BEEN CALLED TO WRITE AN INCORRECT',' NUMBER OF WORDS FOR ELEMENT ID =',I10)
      GOTO 100
   ELSE
!
      iloc = locs(File)
      IF ( Lbuf>0 ) THEN
         qfile = Matrix(File)
         IF ( Inprec/=Precis ) THEN
!
!     INPUT PRECISION IS DIFFERENT FROM OUTPUT PRECISION
!
            k = 0
            DO i = 1 , Lbuf , nwords
               k = k + nwords
               IF ( Precis==2 ) THEN
!
!     SINGLE PRECISION INPUT AND DOUBLE PRECISION OUTPUT
!
                  DO j = i , k
                     da = Rbuf(j)
                     CALL write(qfile,da,2,noeor)
                  ENDDO
               ELSE
!
!     DOUBLE PRECISION INPUT AND SINGLE PRECISION OUTPUT
!
                  DO j = i , k
                     ra = Dbuf(j)
                     CALL write(qfile,ra,1,noeor)
                  ENDDO
               ENDIF
               iloc = iloc + 1
               CALL write(qfile,0,0,eor)
               CALL savpos(qfile,Dict(iloc))
            ENDDO
         ELSE
!
!     INPUT AND OUTPUT PRECISIONS ARE THE SAME
!
            n2word = Precis*nwords
            k = 1
            DO i = 1 , Lbuf , nwords
               iloc = iloc + 1
               CALL write(qfile,Rbuf(k),n2word,eor)
               CALL savpos(qfile,Dict(iloc))
               k = k + n2word
            ENDDO
         ENDIF
!
         locs(File) = iloc
      ENDIF
!
!     IF -EOE- .GT. 0 (IMPLYING END-OF-ELEMENT-DATA) WRITE
!     OUT THE COMPLETED DICTIONARY.
!
      IF ( Eoe>0 ) THEN
!
!     OK -EOE- IS ON.  FIRST WRITE DICTIONARY OUT.
!     INSURE ALL -LOCS- SET CONSIDERING THE NUMBER OF ACTIVE GRID POINTS
!     FOR THIS PARTICULAR ELEMENT.
!
         IF ( locs(File)==Ldict-Nlocs+Dict(3)/fredms(File) ) THEN
!
            IF ( Flags(File)<0 ) THEN
               Flags(File) = iabs(Flags(File))
               CALL write(Dictn(File),Eltype,3,noeor)
            ENDIF
            Flags(File) = Flags(File) + 1
            CALL write(Dictn(File),Dict,Ldict,noeor)
            Nval(File) = 0
!
!     EXISTENCE OF NON-ZERO DAMPING CONSTANT TURNS ON NOK4GG FLAG.
!
            IF ( Nok4gg<=0 ) THEN
               ifff = Dict(5)
               IF ( fff/=0 ) Nok4gg = 1
            ENDIF
!
!     CHECK FOR THIS ELEMENT BEING IN CONGRUENT LIST.
!
!     EMGOUT WILL NEVER BE CALLED FOR AN ELEMENT WHICH IS IN THE
!     CONGRUENT LIST AND ALREADY HAS A DICTIONARY.
!
            IF ( Anycon ) THEN
               CALL bisloc(*300,Elid,Z(Icong),2,Lcong/2,j)
!
!     OK ELEMENT IS CONGRUENT, FIND PRIMARY ID.
!
               iadd = Icong + j
               DO
                  iprime = Z(iadd)
!
!     IPRIME .GT. 0 POINTS TO PRIMARY ID
!     IPRIME .EQ. 0 IS PRIMARY ID TABLE POINTER AND NO TABLE EXISTS
!     IPRIME .LT. 0 IS TABLE POINTER NEGATED.
!
                  IF ( iprime<0 ) GOTO 500
                  IF ( iprime/=0 ) THEN
!
!     IPRIME POINTS TO PRIMARY ID
!
                     iadd = iprime + 1
!
!     IPRIME IS TABLE POINTER AND NONE EXISTS YET.
!     THUS ADD ONE TO CORE, FROM THE BOTTOM OF CORE.
!
                  ELSEIF ( Ncore-maxfil>Jcore ) THEN
!
!     ALLOCATE SMALL TABLE FOR POINTERS TO DICTIONARY FOR EACH FILE TYPE
!     POSSIBLE.
!
!     NOTE THAT THE ELEMENT-ID (IF SECONDARY) WILL HAVE A POINTER TO THE
!     PRIMARY-ID.  THE PRIMARY-ID  THEN WILL HAVE A POINTER TO A TABLE
!     OF SIZE -MAXFIL- POINTING TO -MAXFIL- DICTIONARYS.  (SOME OF WHICH
!     MAY NOT YET OR EVER BE CREATED).
!     NO CORE IS USED UNTIL A DICTIONARY IS CREATED.
!
                     i2 = Ncore
                     Ncore = Ncore - maxfil
                     i1 = Ncore + 1
                     DO i = i1 , i2
                        Z(i) = 0
                     ENDDO
!
!     STORE ZERO ADDRESS OF THIS TABLE WITH PRIMARY ID.
!
                     Z(iadd) = -Ncore
                     iprime = -Ncore
                     GOTO 500
                  ELSE
!
!     NOT ENOUGH CORE SO CONGRUENCY IS IGNORED.
!
                     icrq = Jcore - Ncore + maxfil
                     GOTO 400
                  ENDIF
               ENDDO
            ENDIF
         ELSE
            WRITE (Outpt,99005) Sfm , Elid , File
99005       FORMAT (A25,' 3111, INVALID NUMBER OF PARTITIONS WERE SENT EMGOUT',' FOR ELEMENT ID =',I10,/5X,                         &
                   &'WITH RESPECT TO DATA BLOCK ','TYPE =',I3,1H.)
            GOTO 100
         ENDIF
      ENDIF
   ENDIF
 300  RETURN
 400  CALL page2(4)
   WRITE (Outpt,99006) Uim , Elid
99006 FORMAT (A29,' 3112, ELEMENTS CONGRUENT TO ELEMENT ID =',I10,/5X,'WILL BE RE-COMPUTED AS THERE IS INSUFFICIENT CORE AT ',      &
             &'THIS MOMENT TO HOLD DICTIONARY DATA.')
   WRITE (Outpt,99007) icrq
99007 FORMAT (5X,24HADDITIONAL CORE NEEDED =,I8,7H WORDS.)
   GOTO 300
!
!     IPRIME IS NEGATIVE ZERO POINTER TO FILE-DICTIONARY-TABLE FOR THIS
!     CONGRUENCY SET.
!
 500  itab = -iprime
!
!     ALLOCATE DICTIONARY SPACE IN CORE, IF THERE IS CORE,
!     SET FILE POSITION IN TABLE TO POINT TO THIS DICTIONARY,
!     AND STORE THE DICTIONARY.
!
   IF ( Ncore-Ldict>Jcore ) THEN
!
!     ALLOCATE AND WRITE DICTIONARY
!
      Ncore = Ncore - Ldict
      j = Ncore
      DO i = 1 , Ldict
         j = j + 1
         Z(j) = Dict(i)
      ENDDO
!
!     STORE DICTIONARY ADDRESS IN TABLE(FILE), WHERE TABLE BEGINS
!     AT Z(ITAB+1).
!
      Z(itab+File) = Ncore + 1
      GOTO 300
   ELSE
!
!     INSUFFICIENT CORE THUS IGNORE CONGRUENCY, AND FOR SAFETY
!     PURGE THIS CONGRUENCY FOR ALL FILES.
!
      icrq = Jcore - Ncore + Ldict
      Z(iadd) = 0
      GOTO 400
   ENDIF
END SUBROUTINE emgout
