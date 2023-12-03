!*==procom.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE procom(Procos,Procof,Casecc,Ncoefs,Ngrids)
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Procos
   INTEGER :: Procof
   INTEGER :: Casecc
   INTEGER :: Ncoefs
   INTEGER :: Ngrids
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , buf2 , buf3 , file , i , inew , iwords , j , k , lcc , lcore , lsym , ntot
   REAL :: coef
   INTEGER , SAVE :: i16 , i166
   INTEGER , DIMENSION(7) :: info
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL bckrec , close , fread , fwdrec , gopen , korsz , mesage , rdtrl , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     PROCOM COMBINES PROCOF CASES FOR SUBCOM-S AND REPCASES
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA i166 , i16 , nam/166 , 16 , 4HPROC , 4HOM  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         lcore = korsz(z)
         buf1 = lcore - ibuf + 1
         buf2 = buf1 - ibuf
         buf3 = buf2 - ibuf
         lcore = buf3 - 1
         IF ( lcore<Ncoefs .OR. lcore<Ngrids ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(Procos,z(buf1),0)
         CALL gopen(Procof,z(buf2),1)
!
!     CHECK EACH SUBCASE FOR REPCASE OR SUBCOM-IF NONE(JUST COPY SET OF
!     5 RECORDS FROM PROCOS TO PROCOF
!
         file = Casecc
         CALL gopen(Casecc,z(buf3),0)
         spag_nextblock_1 = 2
      CASE (2)
         file = Casecc
         CALL read(*40,*20,Casecc,z(1),lcore,0,iwords)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 20      IF ( iz(i16)/=0 ) THEN
!
!     REPCASE OR SUBCOM
!
            IF ( iz(i16)>0 ) THEN
!
!     SUBCOM
!
               lcc = iz(i166)
               lsym = iz(lcc)
               DO i = 1 , lsym
                  DO j = 1 , 5
                     CALL bckrec(Procos)
                  ENDDO
               ENDDO
               ntot = 2*(Ncoefs+Ngrids)
               IF ( iwords+2*ntot>lcore ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               inew = iwords + ntot
               DO i = 1 , ntot
                  z(inew+i) = 0.
               ENDDO
               DO i = 1 , lsym
                  coef = z(lcc+i)
                  IF ( coef==0. ) THEN
                     DO k = 1 , 5
                        CALL fwdrec(*60,Procos)
                     ENDDO
                  ELSE
                     CALL fread(Procos,info,103,1)
                     CALL fread(Procos,z(iwords+1),Ncoefs,1)
                     CALL fread(Procos,z(iwords+Ncoefs+1),Ncoefs,1)
                     CALL fread(Procos,z(iwords+2*Ncoefs+1),Ngrids,1)
                     CALL fread(Procos,z(iwords+2*Ncoefs+Ngrids+1),Ngrids,1)
                     DO j = 1 , ntot
                        z(inew+j) = z(inew+j) + coef*z(iwords+j)
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
               CALL write(Procof,z(inew+1),Ncoefs,1)
               CALL write(Procof,z(inew+Ncoefs+1),Ncoefs,1)
               CALL write(Procof,z(inew+2*Ncoefs+1),Ngrids,1)
!
!     GO BACK FOR ANOTHER SUBCASE
!
               CALL write(Procof,z(inew+2*Ncoefs+Ngrids+1),Ngrids,1)
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
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
         CALL fread(Procos,z,103,1)
         CALL write(Procof,z,103,1)
         CALL fread(Procos,z,Ncoefs,1)
         CALL write(Procof,z,Ncoefs,1)
         CALL fread(Procos,z,Ncoefs,1)
         CALL write(Procof,z,Ncoefs,1)
         CALL fread(Procos,z,Ngrids,1)
         CALL write(Procof,z,Ngrids,1)
         CALL fread(Procos,z,Ngrids,1)
!
!     GO BACK FOR ANOTHER CASE CONTROL RECORD
!
         CALL write(Procof,z,Ngrids,1)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     DONE
!
 40      CALL close(Casecc,1)
         CALL close(Procos,1)
         CALL close(Procof,1)
         info(1) = Procos
         CALL rdtrl(info)
         info(1) = Procof
         CALL wrttrl(info)
         RETURN
!
 60      CALL mesage(-2,0,nam)
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(-8,0,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE procom