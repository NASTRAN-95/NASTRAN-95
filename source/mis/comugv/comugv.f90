!*==comugv.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE comugv
   IMPLICIT NONE
   USE C_BLANK
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , buf2 , buf3 , i , j , jj , jnrow , k , kk , lcore , ll , ncol , nd , nd1 , nm1 , nrow , nskip
   INTEGER , DIMENSION(2) :: indb , oudb
   INTEGER , DIMENSION(7) :: mcb , mcb1 , mcb2
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER , SAVE :: ugv , ugvadc , ugvadd , ugvnrl , ugvsqc , ugvsqr
   EXTERNAL close , fwdrec , gopen , korsz , mesage , pack , rdtrl , rewind , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
! FOR DDAM/EARTHQUAKE ANALYSES, COMBUGV COMBINES DISPLACEMENT
! COMPONENTS BY (1)ADDING THE COMPONENTS IN ABS VALUE AND (2)TAKING THE
! SQUARE ROOT OF THE SUMS OF THE SQUARES. AFTER THIS MODEULE, THE
! TWO OUTPUT DATA BLOCKS ARE N X NMODES, WHEREAS UGV IS N X (NMODES)(L)
! MODULE NRLSUM COMBINES STRESSES ACROSS MODES FOR EACH DIRECTION
! INDIVIDUALLY. THE OUTPUTS OF THIS MODULE HAVE THE DIRECTIONS
! COMBINED. BUT NRLSUM CAN WORK ON THEM (AFTER CASEGEN AND SDR2) BY
! SPECIFYING NDIR=1 IN THE DMAP STATEMENT FOR THOSE MODULES.
! THIS MODULE WILL ALSO COMBINE THE MAXIMUM RESPONSES ACROSS THE MODES
! BY USING SQRSS TO COME UP WITH ONE RESPONSE VECTOR. THEREFORE THIS
! MODULE COMBINES COMPONENTS TO GET MAXIMUM RESPONSES BY ADDING (UGVADD)
! AND BY SQRSS (UGVSQR). THEN IT TAKES EACH OF THESE AND TAKES SQRSS
! ACROSS THE MODES TO GET UGVADC AND UGVSQC, RESPECTIVELY.
! FINALLY, THE MODULE COMPUTES THE NRL SUMS FOR THE L DIRECTIONS
! TO USE CASEGEN,SDR2,ETC. ON UGVADC AND UGVSQC, IN CASEGEN,USE
! LMODES=NDIR=1 IN DMAP STATEMENT. FOR UGVNRL, JUST USE LMODES=1.
!
! COMBUGV UGV/UGVADD,UGVSQR,UGVADC,UGVSQC,UGVNRL/V,N,NMODES/V,N,NDIR $
!
   DATA ugv , ugvadd , ugvsqr , ugvadc , ugvsqc/101 , 201 , 202 , 203 , 204/
   DATA ugvnrl/205/
   DATA nam/4HCOMB , 4HUGV /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! OPEN CORE AND BUFFERS
!
         lcore = korsz(Z)
         buf1 = lcore - Ibuf + 1
         buf2 = buf1 - Ibuf
         buf3 = buf2 - Ibuf
         lcore = buf3 - 1
         IF ( lcore<=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         mcb(1) = ugv
         CALL rdtrl(mcb)
         ncol = mcb(2)
         nrow = mcb(3)
         IF ( ncol/=Nmodes*Ndir ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( lcore<4*nrow ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         mcb1(1) = ugvadd
         mcb1(2) = 0
         mcb1(3) = nrow
         mcb1(4) = 2
         mcb1(5) = 1
         mcb1(6) = 0
         mcb1(7) = 0
         mcb2(1) = ugvsqr
         mcb2(2) = 0
         mcb2(3) = nrow
         mcb2(4) = 2
         mcb2(5) = 1
         mcb2(6) = 0
         mcb2(7) = 0
!
         Jout = 1
         Iii = 1
         Nnn = nrow
         Jncr = 1
         Iin = 1
         Iout = 1
         Ii = 1
         Nn = nrow
         Incr = 1
!
         CALL gopen(ugv,Z(buf1),0)
         CALL gopen(ugvadd,Z(buf2),1)
         CALL gopen(ugvsqr,Z(buf3),1)
!
! UNPACK NDIR COLUMNS OF UGV WHICH CORRESPOND TO A SINGLE MODE
!
         nm1 = Nmodes - 1
         nd1 = Ndir - 1
         DO i = 1 , Nmodes
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
!
! POINTER TO PROPER MODE IN 1ST DIRECTION
!
                  nskip = i - 1
                  IF ( nskip/=0 ) THEN
                     DO ll = 1 , nskip
                        CALL fwdrec(*40,ugv)
                     ENDDO
                  ENDIF
!
! UNPACK VECTOR
!
                  CALL unpack(*2,ugv,Z(1))
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
!
 2                DO j = 1 , nrow
                     Z(j) = 0.
                  ENDDO
                  spag_nextblock_2 = 2
               CASE (2)
!
! SKIP TO NEW DIRECTION, UNPACK, SKIP AND UNAPCK
!
                  IF ( nd1==0 ) THEN
!
! JUST ONE DIRECTION ON UGV- COPY TO DATA BLOCKS
!
                     CALL pack(Z(1),ugvadd,mcb1)
                     CALL pack(Z(1),ugvsqr,mcb2)
                  ELSE
                     DO j = 1 , nd1
                        IF ( nm1/=0 ) THEN
                           DO jj = 1 , nm1
                              CALL fwdrec(*40,ugv)
                           ENDDO
                        ENDIF
!
                        jnrow = j*nrow
                        CALL unpack(*4,ugv,Z(jnrow+1))
                        CYCLE
 4                      DO jj = 1 , nrow
                           Z(j*nrow+jj) = 0.
                        ENDDO
!
                     ENDDO
!
! NOW PERFORM EACH OPERATION AND STORE INTO Z(3*NROW+1)
!
                     DO kk = 1 , nrow
                        Z(3*nrow+kk) = abs(Z(kk)) + abs(Z(nrow+kk)) + abs(Z(2*nrow+kk))
                     ENDDO
                     CALL pack(Z(3*nrow+1),ugvadd,mcb1)
!
                     DO kk = 1 , nrow
                        Z(3*nrow+kk) = sqrt(Z(kk)**2+Z(nrow+kk)**2+Z(2*nrow+kk)**2)
                     ENDDO
                     CALL pack(Z(3*nrow+1),ugvsqr,mcb2)
                  ENDIF
!
! DONE FOR THIS MODE - GET ANOTHER
!
                  CALL rewind(ugv)
                  CALL fwdrec(*40,ugv)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO
!
         CALL close(ugvadd,1)
         CALL close(ugvsqr,1)
         CALL wrttrl(mcb1)
         CALL wrttrl(mcb2)
!
! NOW COMPUTE NRL SUMS FOR THE L DIRECTIONS
!
         mcb1(1) = ugvnrl
         mcb1(2) = 0
         mcb1(3) = nrow
         mcb1(4) = 2
         mcb1(5) = 1
         mcb1(6) = 0
         mcb1(7) = 0
         CALL rewind(ugv)
         CALL fwdrec(*40,ugv)
         CALL gopen(ugvnrl,Z(buf2),1)
!
         DO nd = 1 , Ndir
!
! SET UP VECTOR OF MAXIMUM DISPLACEMENT COMPONENTS AND VECTOR OF SUMS
!
            DO i = 1 , nrow
               Z(i) = 0.
               Z(2*nrow+i) = 0.
            ENDDO
!
            DO i = 1 , Nmodes
!
               CALL unpack(*10,ugv,Z(nrow+1))
!
! COMPARE TO MAXIMUM COMPONENTS
!
               DO j = 1 , nrow
                  IF ( abs(Z(nrow+j))>Z(j) ) Z(j) = abs(Z(nrow+j))
                  Z(2*nrow+j) = Z(2*nrow+j) + Z(nrow+j)**2
               ENDDO
!
! GET ANOTHER DISPLACEMENT VECTOR CORRESPONDING TO ANOTHER MODE
!
 10         ENDDO
!
! SUBTRACT THE MAXIMA FROM THE SUMS
!
            DO j = 1 , nrow
               Z(2*nrow+j) = Z(2*nrow+j) - Z(j)**2
!
! TAKE SQUARE ROOT AND ADD IN THE MAXIMA
!
               Z(2*nrow+j) = sqrt(Z(2*nrow+j)) + Z(j)
            ENDDO
!
! PACK RESULTS ANG GET ANOTHER DIRECTION
!
            CALL pack(Z(2*nrow+1),ugvnrl,mcb1)
         ENDDO
!
         CALL close(ugv,1)
         CALL close(ugvnrl,1)
         CALL wrttrl(mcb1)
!
! NOW LETS COMBINE RESPONSES OVER THE MODES USING SQRSS. DO FOR BOTH
! UGVADD AND UGVSQR. THE RESULT WILL BE ONE DISLPACEMENT VECTOR.
! (BOTH UGVADD AND UGVSQR ARE N X M ( M= NO. OF MODES)
!
         indb(1) = ugvadd
         indb(2) = ugvsqr
         oudb(1) = ugvadc
         oudb(2) = ugvsqc
!
         DO i = 1 , 2
!
            mcb(1) = indb(i)
            CALL rdtrl(mcb)
            ncol = mcb(2)
            nrow = mcb(3)
            mcb1(1) = oudb(i)
            mcb1(2) = 0
            mcb1(3) = nrow
            mcb1(4) = 2
            mcb1(5) = 1
            mcb1(6) = 0
            mcb1(7) = 0
            IF ( ncol/=Nmodes ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
            CALL gopen(indb(i),Z(buf1),0)
            CALL gopen(oudb(i),Z(buf2),1)
!
            DO j = 1 , nrow
               Z(j) = 0.
            ENDDO
!
! UNPACK THE COLUMNS OF INDB AND ACCUMULATE SUMS OF SQUARES
!
            DO j = 1 , Nmodes
               CALL unpack(*20,indb(i),Z(nrow+1))
!
               DO k = 1 , nrow
                  Z(k) = Z(k) + Z(nrow+k)**2
               ENDDO
!
 20         ENDDO
!
            DO k = 1 , nrow
               Z(k) = sqrt(Z(k))
            ENDDO
!
            CALL pack(Z(1),oudb(i),mcb1)
!
            CALL close(indb(i),1)
            CALL close(oudb(i),1)
            CALL wrttrl(mcb1)
!
         ENDDO
!
         RETURN
!
 40      CALL mesage(-2,ugv,nam)
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(-7,0,nam)
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(-8,0,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE comugv
