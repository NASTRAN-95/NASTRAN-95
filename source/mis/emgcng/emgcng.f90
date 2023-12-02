!*==emgcng.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE emgcng
   USE c_emgfil
   USE c_emgprm
   USE c_names
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf , flag , i , icongz , icrq , idprim , iwords , j , k , kid , lnum , ncong1 , ncong2 , nogo , nout , sysbuf
   INTEGER , DIMENSION(2) , SAVE :: cngrnt , subr
   INTEGER , SAVE :: noeor
   EXTERNAL bisloc , close , locate , mesage , page2 , preloc , read , sort
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   DATA subr/4HEMGC , 4HNG  / , noeor/0/ , cngrnt/5008 , 50/
!
   buf = ncore - sysbuf - 2
   IF ( buf<=jcore ) CALL mesage(-8,jcore-buf,subr)
   anycon = .FALSE.
   icong = jcore
   ncong = jcore - 1
   lcong = 0
!
!     LOCATE -CNGRNT- BULK DATA CARDS IF ANY.
!
   CALL preloc(*99999,z(buf),geom2)
   CALL locate(*200,z(buf),cngrnt,flag)
!
!     PROCESS ONE DATA CARD
!
   SPAG_Loop_1_1: DO WHILE ( ncong+2<buf )
      CALL read(*100,*100,geom2,z(ncong+1),1,noeor,iwords)
      z(ncong+2) = 0
      idprim = z(ncong+1)
      ncong = ncong + 2
!
!     READ ANY SECONDARY IDS.
!
      DO WHILE ( ncong+2<buf )
         CALL read(*100,*100,geom2,z(ncong+1),1,noeor,iwords)
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
         IF ( z(ncong+1)/=idprim ) THEN
!
            IF ( z(ncong+1)<0 ) CYCLE SPAG_Loop_1_1
            IF ( z(ncong+1)/=0 ) THEN
               z(ncong+2) = idprim
               ncong = ncong + 2
            ENDIF
         ELSE
!
!     THE ABOVE CONDITION EXISTS
!
            CALL page2(3)
            WRITE (nout,99001) uwm , idprim
!
!
99001       FORMAT (A25,' 3169, PRIMARY ID',I9,' ON A CNGRNT CARD ALSO USED ','AS A SECONDARY ID ON THE SAME CARD.',/5X,            &
                   &'SECONDARY ID IGNORED.')
         ENDIF
      ENDDO
      EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
!
!     INSUFFICIENT CORE TO PROCESS ALL -CNGRNT- CARDS
!
   icrq = ncong + 2 - buf
   CALL page2(2)
   WRITE (nout,99002) uwm , icrq
99002 FORMAT (A25,' 3182, INSUFFICIENT CORE TO PROCESS ALL CNGRNT ','CARDS.  ADDITIONAL CORE NEEDED =',I8,7H WORDS.)
!
!     NO MORE -CNGRNT- CARDS
!
 100  lcong = ncong - icong + 1
   IF ( lcong>0 ) THEN
      CALL sort(0,0,2,1,z(icong),lcong)
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
      ncong1 = ncong - 2
      DO i = icong , ncong1 , 2
         IF ( z(i)==z(i+2) ) THEN
            IF ( z(i+1)/=z(i+3) ) THEN
               nogo = 1
               IF ( z(i+1)/=0 .AND. z(i+3)/=0 ) THEN
!
!     THIS IS CONDITION 3 DESCRIBED ABOVE
!
                  WRITE (nout,99003) ufm , z(i)
99003             FORMAT (A23,' 3171, SECONDARY ID',I9,' SPECIFIED AS CONGRUENT TO MORE THAN ONE PRIMARY ID.')
               ELSE
!
!     THIS IS CONDITION 2 DESCRIBED ABOVE
!
                  WRITE (nout,99004) ufm , z(i)
99004             FORMAT (A23,' 3170, PRIMARY ID',I9,' ON A CNGRNT CARD ALSO USED ','AS A SECONDARY ID ON ANOTHER CNGRNT CARD.')
               ENDIF
            ENDIF
         ENDIF
!
      ENDDO
      IF ( nogo==1 ) CALL mesage(-37,0,subr)
      ncong2 = ncong1
      DO i = icong , ncong1 , 2
         IF ( z(i)>=0 ) THEN
            IF ( z(i)==z(i+2) ) THEN
               j = i + 2
               SPAG_Loop_2_2: DO
                  DO k = j , ncong2 , 2
                     z(k) = z(k+2)
                     z(k+1) = z(k+3)
                  ENDDO
                  lcong = lcong - 2
                  ncong = ncong - 2
                  z(ncong2-1) = -1
                  ncong2 = ncong2 - 2
                  IF ( z(j)/=z(i) ) THEN
                     IF ( z(i+1)/=0 ) THEN
!
!     THIS IS CONDITION 4 DESCRIBED ABOVE
!
                        CALL page2(2)
                        WRITE (nout,99005) uwm , z(i)
99005                   FORMAT (A25,' 3172, SECONDARY ID',I9,' REDUNDANTLY SPECIFIED ON ','CNGRNT CARDS.  REDUNDANCY IGNORED.')
                     ENDIF
                     EXIT SPAG_Loop_2_2
                  ENDIF
               ENDDO SPAG_Loop_2_2
            ENDIF
         ENDIF
!
      ENDDO
!
!     REPLACE PRIMARY ID ASSOCIATED WITH EACH SECONDARY ID
!     WITH LOCATION OF PRIMARY ID IN TABLE.
!
      lnum = lcong/2
      icongz = icong - 1
      DO i = icong , ncong , 2
         IF ( z(i+1)/=0 ) THEN
            kid = z(i+1)
            CALL bisloc(*150,kid,z(icong),2,lnum,j)
            z(i+1) = icongz + j
         ENDIF
 150  ENDDO
   ENDIF
!
!     TABLE IS COMPLETE
!
 200  CALL close(geom2,clsrew)
   IF ( ncong>icong ) anycon = .TRUE.
   jcore = ncong + 1
!
99999 END SUBROUTINE emgcng
