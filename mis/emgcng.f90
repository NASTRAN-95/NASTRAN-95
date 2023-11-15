
SUBROUTINE emgcng
   IMPLICIT NONE
!
! COMMON variable declarations
!
   LOGICAL Anycon , Error , Heat
   INTEGER Cls , Clsrew , Cstm , Dictn(3) , Dit , Est , Flags(3) , Geom2 , Icmbar , Icong , Icore , Icstm , Idit , Ihmat , Imat ,   &
         & Jcore , Ksystm(65) , Lcong , Lcstm , Lhmat , Lmat , Mats(3) , Mpt , Ncong , Ncore , Ncstm , Ndit , Nhmat , Nmat , Nout , &
         & Precis , Rd , Rdrew , Sysbuf , Wrt , Wrtrew , Z(1)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /emgfil/ Est , Cstm , Mpt , Dit , Geom2 , Mats , Dictn
   COMMON /emgprm/ Icore , Jcore , Ncore , Icstm , Ncstm , Imat , Nmat , Ihmat , Nhmat , Idit , Ndit , Icong , Ncong , Lcong ,      &
                 & Anycon , Flags , Precis , Error , Heat , Icmbar , Lcstm , Lmat , Lhmat
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER buf , cngrnt(2) , flag , i , icongz , icrq , idprim , iwords , j , k , kid , lnum , ncong1 , ncong2 , noeor , nogo ,     &
         & subr(2)
!
! End of declarations
!
!
!     THIS ROUTINE OF THE -EMG- MODULE READS -CNGRNT- CARD
!     IMAGES, IF ANY, FROM GEOM2 AND BUILDS A PAIRED LIST.
!
!     ON EACH -CNGRNT- DATA CARD THE  FIRST ID (NEED NOT BE THE SMALLEST
!     ID) BECOMES THE PRIMARY ID.  THIS ID WILL BE PAIRED WITH A ZERO
!     NOW AND A NEGATIVE DICTIONARY-TABLE  ADDRESS LATER.  AS SOME OF
!     THE ID-S APPEARING ON THE -CNGRNT- DATA CARD MAY NOT EVEN BE IN
!     THE PROBLEM, THE FIRST ID OF A CONGRUENT GROUP REFERENCED WILL
!     RESULT IN THE ELEMENT COMPUTATIONS AND THE SETTING OF A DICTIONARY
!     FILE TABLE ADDRESS WITH THE PRIMARY ID.
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   DATA subr/4HEMGC , 4HNG  / , noeor/0/ , cngrnt/5008 , 50/
!
   buf = Ncore - Sysbuf - 2
   IF ( buf<=Jcore ) CALL mesage(-8,Jcore-buf,subr)
   Anycon = .FALSE.
   Icong = Jcore
   Ncong = Jcore - 1
   Lcong = 0
!
!     LOCATE -CNGRNT- BULK DATA CARDS IF ANY.
!
   CALL preloc(*400,Z(buf),Geom2)
   CALL locate(*300,Z(buf),cngrnt,flag)
!
!     PROCESS ONE DATA CARD
!
   DO WHILE ( Ncong+2<buf )
      CALL read(*200,*200,Geom2,Z(Ncong+1),1,noeor,iwords)
      Z(Ncong+2) = 0
      idprim = Z(Ncong+1)
      Ncong = Ncong + 2
!
!     READ ANY SECONDARY IDS.
!
      DO WHILE ( Ncong+2<buf )
         CALL read(*200,*200,Geom2,Z(Ncong+1),1,noeor,iwords)
!
!     CHECK FOR THE FOLLOWING CONDITION
!
!     CONDITION 1
!     ------------
!
!     A SECONDARY ID ON THIS CARD IS THE SAME AS THE PRIMARY ID
!     ON THIS CARD.  THE SECONDARY ID IS IGNORED AND THE CONDITION
!     IS INDICATED BY A USER INFORMATION MESSAGE.
!
         IF ( Z(Ncong+1)/=idprim ) THEN
!
            IF ( Z(Ncong+1)<0 ) GOTO 100
            IF ( Z(Ncong+1)/=0 ) THEN
               Z(Ncong+2) = idprim
               Ncong = Ncong + 2
            ENDIF
         ELSE
!
!     THE ABOVE CONDITION EXISTS
!
            CALL page2(3)
            WRITE (Nout,99001) Uwm , idprim
!
!
99001       FORMAT (A25,' 3169, PRIMARY ID',I9,' ON A CNGRNT CARD ALSO USED ','AS A SECONDARY ID ON THE SAME CARD.',/5X,            &
                   &'SECONDARY ID IGNORED.')
         ENDIF
      ENDDO
      EXIT
 100  ENDDO
!
!     INSUFFICIENT CORE TO PROCESS ALL -CNGRNT- CARDS
!
   icrq = Ncong + 2 - buf
   CALL page2(2)
   WRITE (Nout,99002) Uwm , icrq
99002 FORMAT (A25,' 3182, INSUFFICIENT CORE TO PROCESS ALL CNGRNT ','CARDS.  ADDITIONAL CORE NEEDED =',I8,7H WORDS.)
!
!     NO MORE -CNGRNT- CARDS
!
 200  Lcong = Ncong - Icong + 1
   IF ( Lcong>0 ) THEN
      CALL sort(0,0,2,1,Z(Icong),Lcong)
!
!     CHECK FOR THE FOLLOWING ADDITIONAL CONDITIONS
!
!     CONDITION 2
!     -----------
!
!     A PRIMARY ID ON A CNGRNT CARD IS ALSO USED AS A SECONDARY
!     ID ON ANOTHER CNGRNT CARD.  THIS RESULTS IN A USER FATAL
!     MESSAGE.
!
!     CONDITION 3
!     -----------
!
!     A SECONDARY ID IS SPECIFIED AS CONGRUENT TO MORE THAN ONE
!     PRIMARY ID.  THIS ALSO RESULTS IN A USER FATAL MESSAGE.
!
!     CONDITION 4
!     -----------
!
!     A SECONDARY ID IS REDUNDANTLY SPECIFIED.  THE REDUNDANCIES ARE
!     IGNORED AND THE CONDITION IS INDICATED BY A USER INFORMATION
!     MESSAGE.
!
      nogo = 0
      ncong1 = Ncong - 2
      DO i = Icong , ncong1 , 2
         IF ( Z(i)==Z(i+2) ) THEN
            IF ( Z(i+1)/=Z(i+3) ) THEN
               nogo = 1
               IF ( Z(i+1)/=0 .AND. Z(i+3)/=0 ) THEN
!
!     THIS IS CONDITION 3 DESCRIBED ABOVE
!
                  WRITE (Nout,99003) Ufm , Z(i)
99003             FORMAT (A23,' 3171, SECONDARY ID',I9,' SPECIFIED AS CONGRUENT TO MORE THAN ONE PRIMARY ID.')
               ELSE
!
!     THIS IS CONDITION 2 DESCRIBED ABOVE
!
                  WRITE (Nout,99004) Ufm , Z(i)
99004             FORMAT (A23,' 3170, PRIMARY ID',I9,' ON A CNGRNT CARD ALSO USED ','AS A SECONDARY ID ON ANOTHER CNGRNT CARD.')
               ENDIF
            ENDIF
         ENDIF
!
      ENDDO
      IF ( nogo==1 ) CALL mesage(-37,0,subr)
      ncong2 = ncong1
      DO i = Icong , ncong1 , 2
         IF ( Z(i)>=0 ) THEN
            IF ( Z(i)==Z(i+2) ) THEN
               j = i + 2
               DO
                  DO k = j , ncong2 , 2
                     Z(k) = Z(k+2)
                     Z(k+1) = Z(k+3)
                  ENDDO
                  Lcong = Lcong - 2
                  Ncong = Ncong - 2
                  Z(ncong2-1) = -1
                  ncong2 = ncong2 - 2
                  IF ( Z(j)/=Z(i) ) THEN
                     IF ( Z(i+1)/=0 ) THEN
!
!     THIS IS CONDITION 4 DESCRIBED ABOVE
!
                        CALL page2(2)
                        WRITE (Nout,99005) Uwm , Z(i)
99005                   FORMAT (A25,' 3172, SECONDARY ID',I9,' REDUNDANTLY SPECIFIED ON ','CNGRNT CARDS.  REDUNDANCY IGNORED.')
                     ENDIF
                     EXIT
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
!
      ENDDO
!
!     REPLACE PRIMARY ID ASSOCIATED WITH EACH SECONDARY ID
!     WITH LOCATION OF PRIMARY ID IN TABLE.
!
      lnum = Lcong/2
      icongz = Icong - 1
      DO i = Icong , Ncong , 2
         IF ( Z(i+1)/=0 ) THEN
            kid = Z(i+1)
            CALL bisloc(*250,kid,Z(Icong),2,lnum,j)
            Z(i+1) = icongz + j
         ENDIF
 250  ENDDO
   ENDIF
!
!     TABLE IS COMPLETE
!
 300  CALL close(Geom2,Clsrew)
   IF ( Ncong>Icong ) Anycon = .TRUE.
   Jcore = Ncong + 1
 400  RETURN
!
END SUBROUTINE emgcng
