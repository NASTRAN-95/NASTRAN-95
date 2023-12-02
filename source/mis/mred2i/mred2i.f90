!*==mred2i.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2i(Kode,Nuf,N2)
   USE c_blank
   USE c_condas
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Kode
   INTEGER :: Nuf
   INTEGER :: N2
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: dblkor , gs , i , ifile , imsg , j , k , kore , lamamr , locqsm , nwdsrd , qsm , qsmcol , qsmrow , qsmtyp , sglkor
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , SAVE :: idiag
   INTEGER , DIMENSION(7) :: itrlr1
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL , DIMENSION(1) :: rz
   EXTERNAL close , fwdrec , gopen , makmcb , mesage , pack , rdtrl , read , sofcls , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
!
!     THIS SUBROUTINE COMPUTES THE GS MATRIX FOR THE MRED2 MODULE.
!
   !>>>>EQUIVALENCE (Lamamr,Infile(2)) , (Qsm,Infile(12)) , (Gs,Iscr(7)) , (Rz(1),Z(1)) , (Dz(1),Z(1))
   DATA modnam/4HMRED , 4H2I  / , idiag/3/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     TEST OPERATION MODE
!
         IF ( dry==-2 ) RETURN
!
!     FORM GS MATRIX
!
!                 **     **
!                 *       *        T
!        **  **   * .   0 * **   **
!        *    *   *  .    * *     *                 2
!        * GS * =-*  1/K  * * QSM *    WHERE K = M W
!        *    *   *    .  * *     *               I I
!        **  **   * 0   . * **   **
!                 *       *
!                 **     **
!
         itrlr1(1) = qsm
         CALL rdtrl(itrlr1)
         IF ( itrlr1(1)<0 ) THEN
!
!     PROCESS MODULE FATAL ERRORS
!
            WRITE (iprntr,99001) ufm
!
99001       FORMAT (A23,' 6638, IN MODULE MREDUCE WITH USERMODE=2, THE ','CONSTRAINT FORCES MATRIX (QSM) CANNOT BE PURGED.')
            dry = -2
            RETURN
         ELSE
            qsmrow = itrlr1(2)
            qsmcol = itrlr1(3)
!
!                        2
!     FORM K = 1.0 / (M W )
!                      I I
!
            IF ( korbgn+7+qsmrow>=korlen ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ifile = lamamr
            CALL gopen(lamamr,z(gbuf1),0)
            CALL fwdrec(*60,lamamr)
            nmodes = 0
            DO
               CALL read(*40,*20,lamamr,z(korbgn),7,0,nwdsrd)
               rz(korbgn+7+nmodes) = 1.0/(forpi2*rz(korbgn+5)*(rz(korbgn+4)**2))
               nmodes = nmodes + 1
               IF ( korbgn+7+nmodes>=korlen ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
 20      CALL close(lamamr,1)
         IF ( nmodes/=qsmrow ) THEN
            WRITE (iprntr,99002) ufm , qsmrow , qsmcol , nmodes
99002       FORMAT (A23,' 6634, IN MODULE MREDUCE WITH USERMODE=2, THE ','CONSTRAINT FORCES MATRIX (',I3,3H X ,I3,1H),/30X,         &
                   &'IS INCOMPATABLE WITH THE NUMBER OF MODES (',I3,2H).)
            dry = -2
            RETURN
         ELSE
            modlen = nmodes
!
!     READ QSM INTO CORE
!
            kore = korbgn
            korbgn = korbgn + 7 + itrlr1(2)
            IF ( korbgn+qsmrow*(qsmcol+1)>=korlen ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            typinu = itrlr1(5)
            irowu = 1
            nrowu = itrlr1(3)
            incru = 1
            qsmtyp = itrlr1(5)
            dblkor = korbgn/2 + 1
            sglkor = 2*dblkor - 1
            CALL gopen(qsm,z(gbuf1),0)
            IF ( qsmtyp==2 ) THEN
               locqsm = dblkor
               DO i = 1 , qsmrow
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        CALL unpack(*22,qsm,dz(dblkor))
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
 22                     DO j = 1 , qsmcol
                           dz(dblkor+j-1) = 0.0D0
                        ENDDO
                        spag_nextblock_2 = 2
                     CASE (2)
                        dblkor = dblkor + itrlr1(3)
                        EXIT SPAG_DispatchLoop_2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
               ENDDO
               korbgn = dblkor
            ELSE
               locqsm = sglkor
               DO i = 1 , qsmrow
                  spag_nextblock_3 = 1
                  SPAG_DispatchLoop_3: DO
                     SELECT CASE (spag_nextblock_3)
                     CASE (1)
                        CALL unpack(*24,qsm,rz(sglkor))
                        spag_nextblock_3 = 2
                        CYCLE SPAG_DispatchLoop_3
 24                     DO j = 1 , qsmcol
                           rz(sglkor+j-1) = 0.0E0
                        ENDDO
                        spag_nextblock_3 = 2
                     CASE (2)
                        sglkor = sglkor + itrlr1(3)
                        EXIT SPAG_DispatchLoop_3
                     END SELECT
                  ENDDO SPAG_DispatchLoop_3
               ENDDO
               korbgn = sglkor
            ENDIF
            CALL close(qsm,1)
!
!     FORM GS MATRIX
!
            typin = itrlr1(5)
            typout = itrlr1(5)
            irow = 1
            nrow = qsmrow
            incr = 1
            CALL makmcb(itrlr1,gs,qsmrow,idiag,typin)
            dblkor = korbgn/2 + 1
            sglkor = 2*dblkor - 1
            CALL gopen(gs,z(gbuf1),1)
            DO i = 1 , qsmcol
               DO j = 1 , qsmrow
                  k = 3*(j-1)
                  IF ( qsmtyp==2 ) THEN
                     dz(dblkor+j-1) = rz(kore+7+j-1)*dz(locqsm+k)
                  ELSE
                     rz(sglkor+j-1) = rz(kore+7+j-1)*rz(locqsm+k)
                  ENDIF
               ENDDO
               CALL pack(dz(dblkor),gs,itrlr1)
            ENDDO
            korbgn = kore
            CALL close(gs,1)
            CALL wrttrl(itrlr1)
            RETURN
         ENDIF
!
!     PROCESS SYSTEM FATAL ERRORS
!
 40      imsg = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      imsg = -3
         spag_nextblock_1 = 3
      CASE (2)
         imsg = -8
         ifile = 0
         spag_nextblock_1 = 3
      CASE (3)
         CALL sofcls
         CALL mesage(imsg,ifile,modnam)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE mred2i
