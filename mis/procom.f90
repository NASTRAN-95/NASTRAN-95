
SUBROUTINE procom(Procos,Procof,Casecc,Ncoefs,Ngrids)
   IMPLICIT NONE
   INTEGER Ibuf , Iz(1)
   REAL Z(1)
   COMMON /system/ Ibuf
   COMMON /zzzzzz/ Z
   INTEGER Casecc , Ncoefs , Ngrids , Procof , Procos
   INTEGER buf1 , buf2 , buf3 , file , i , i16 , i166 , inew , info(7) , iwords , j , k , lcc , lcore , lsym , nam(2) , ntot
   REAL coef
   INTEGER korsz
!
!     PROCOM COMBINES PROCOF CASES FOR SUBCOM-S AND REPCASES
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA i166 , i16 , nam/166 , 16 , 4HPROC , 4HOM  /
!
   lcore = korsz(Z)
   buf1 = lcore - Ibuf + 1
   buf2 = buf1 - Ibuf
   buf3 = buf2 - Ibuf
   lcore = buf3 - 1
   IF ( lcore<Ncoefs .OR. lcore<Ngrids ) GOTO 500
   CALL gopen(Procos,Z(buf1),0)
   CALL gopen(Procof,Z(buf2),1)
!
!     CHECK EACH SUBCASE FOR REPCASE OR SUBCOM-IF NONE(JUST COPY SET OF
!     5 RECORDS FROM PROCOS TO PROCOF
!
   file = Casecc
   CALL gopen(Casecc,Z(buf3),0)
 100  file = Casecc
   CALL read(*300,*200,Casecc,Z(1),lcore,0,iwords)
   GOTO 500
 200  IF ( Iz(i16)/=0 ) THEN
!
!     REPCASE OR SUBCOM
!
      IF ( Iz(i16)>0 ) THEN
!
!     SUBCOM
!
         lcc = Iz(i166)
         lsym = Iz(lcc)
         DO i = 1 , lsym
            DO j = 1 , 5
               CALL bckrec(Procos)
            ENDDO
         ENDDO
         ntot = 2*(Ncoefs+Ngrids)
         IF ( iwords+2*ntot>lcore ) GOTO 500
         inew = iwords + ntot
         DO i = 1 , ntot
            Z(inew+i) = 0.
         ENDDO
         DO i = 1 , lsym
            coef = Z(lcc+i)
            IF ( coef==0. ) THEN
               DO k = 1 , 5
                  CALL fwdrec(*400,Procos)
               ENDDO
            ELSE
               CALL fread(Procos,info,103,1)
               CALL fread(Procos,Z(iwords+1),Ncoefs,1)
               CALL fread(Procos,Z(iwords+Ncoefs+1),Ncoefs,1)
               CALL fread(Procos,Z(iwords+2*Ncoefs+1),Ngrids,1)
               CALL fread(Procos,Z(iwords+2*Ncoefs+Ngrids+1),Ngrids,1)
               DO j = 1 , ntot
                  Z(inew+j) = Z(inew+j) + coef*Z(iwords+j)
               ENDDO
            ENDIF
!
         ENDDO
!
!     WRITE TO PROCOF- 1ST BE SURE THAT ISYM IS 0 TO ACCOUNT FOR
!     POSSIBLE SYMMETRY-ANTISYMMETRY COMBINATION
!
         info(6) = 0
         CALL write(Procof,info,103,1)
         CALL write(Procof,Z(inew+1),Ncoefs,1)
         CALL write(Procof,Z(inew+Ncoefs+1),Ncoefs,1)
         CALL write(Procof,Z(inew+2*Ncoefs+1),Ngrids,1)
         CALL write(Procof,Z(inew+2*Ncoefs+Ngrids+1),Ngrids,1)
!
!     GO BACK FOR ANOTHER SUBCASE
!
         GOTO 100
      ELSE
!
!     REPCASE
!
         DO i = 1 , 5
            CALL bckrec(Procos)
         ENDDO
      ENDIF
   ENDIF
!
!     NOT A SUBCOM - MIGHT BE REPCASE
!
   file = Procos
   CALL fread(Procos,Z,103,1)
   CALL write(Procof,Z,103,1)
   CALL fread(Procos,Z,Ncoefs,1)
   CALL write(Procof,Z,Ncoefs,1)
   CALL fread(Procos,Z,Ncoefs,1)
   CALL write(Procof,Z,Ncoefs,1)
   CALL fread(Procos,Z,Ngrids,1)
   CALL write(Procof,Z,Ngrids,1)
   CALL fread(Procos,Z,Ngrids,1)
   CALL write(Procof,Z,Ngrids,1)
!
!     GO BACK FOR ANOTHER CASE CONTROL RECORD
!
   GOTO 100
!
!     DONE
!
 300  CALL close(Casecc,1)
   CALL close(Procos,1)
   CALL close(Procof,1)
   info(1) = Procos
   CALL rdtrl(info)
   info(1) = Procof
   CALL wrttrl(info)
   RETURN
!
 400  CALL mesage(-2,0,nam)
 500  CALL mesage(-8,0,nam)
END SUBROUTINE procom