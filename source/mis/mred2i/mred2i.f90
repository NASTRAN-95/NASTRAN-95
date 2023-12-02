!*==mred2i.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2i(Kode,Nuf,N2)
USE C_BLANK
USE C_CONDAS
USE C_PACKX
USE C_SYSTEM
USE C_UNPAKX
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
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
         IF ( Dry==-2 ) RETURN
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
            WRITE (Iprntr,99001) Ufm
!
99001       FORMAT (A23,' 6638, IN MODULE MREDUCE WITH USERMODE=2, THE ','CONSTRAINT FORCES MATRIX (QSM) CANNOT BE PURGED.')
            Dry = -2
            RETURN
         ELSE
            qsmrow = itrlr1(2)
            qsmcol = itrlr1(3)
!
!                        2
!     FORM K = 1.0 / (M W )
!                      I I
!
            IF ( Korbgn+7+qsmrow>=Korlen ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            ifile = lamamr
            CALL gopen(lamamr,Z(Gbuf1),0)
            CALL fwdrec(*60,lamamr)
            Nmodes = 0
            DO
               CALL read(*40,*20,lamamr,Z(Korbgn),7,0,nwdsrd)
               rz(Korbgn+7+Nmodes) = 1.0/(Forpi2*rz(Korbgn+5)*(rz(Korbgn+4)**2))
               Nmodes = Nmodes + 1
               IF ( Korbgn+7+Nmodes>=Korlen ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
 20      CALL close(lamamr,1)
         IF ( Nmodes/=qsmrow ) THEN
            WRITE (Iprntr,99002) Ufm , qsmrow , qsmcol , Nmodes
99002       FORMAT (A23,' 6634, IN MODULE MREDUCE WITH USERMODE=2, THE ','CONSTRAINT FORCES MATRIX (',I3,3H X ,I3,1H),/30X,         &
                   &'IS INCOMPATABLE WITH THE NUMBER OF MODES (',I3,2H).)
            Dry = -2
            RETURN
         ELSE
            Modlen = Nmodes
!
!     READ QSM INTO CORE
!
            kore = Korbgn
            Korbgn = Korbgn + 7 + itrlr1(2)
            IF ( Korbgn+qsmrow*(qsmcol+1)>=Korlen ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Typinu = itrlr1(5)
            Irowu = 1
            Nrowu = itrlr1(3)
            Incru = 1
            qsmtyp = itrlr1(5)
            dblkor = Korbgn/2 + 1
            sglkor = 2*dblkor - 1
            CALL gopen(qsm,Z(Gbuf1),0)
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
               Korbgn = dblkor
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
               Korbgn = sglkor
            ENDIF
            CALL close(qsm,1)
!
!     FORM GS MATRIX
!
            Typin = itrlr1(5)
            Typout = itrlr1(5)
            Irow = 1
            Nrow = qsmrow
            Incr = 1
            CALL makmcb(itrlr1,gs,qsmrow,idiag,Typin)
            dblkor = Korbgn/2 + 1
            sglkor = 2*dblkor - 1
            CALL gopen(gs,Z(Gbuf1),1)
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
            Korbgn = kore
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
         CYCLE SPAG_DispatchLoop_1
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
