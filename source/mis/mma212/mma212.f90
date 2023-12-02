!*==mma212.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
SUBROUTINE mma212(Zi,Zd)
   USE i_mmacom
   USE c_mpyadx
   USE c_names
   USE c_packx
   USE c_system
   USE c_type
   USE c_unpakx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Zi
   REAL*8 , DIMENSION(2) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ii , indx , indxa , indxb , indxd , irow1 , irowa1 , irowan , irowb1 , irowbn , irown , irows , k , nac , nadens ,&
            & naform , nanzwd , nar , natype , nbc , nbdens , nbform , nbnzwd , nbr , nbtype , ncc , ncdens , ncform , ncnzwd ,     &
            & ncr , nctype , ndc , nddens , ndform , ndnzwd , ndr , ndtype , nout
   REAL :: sysbuf
!
! End of declarations rewritten by SPAG
!
!
!     MMA212 PERFORMS THE MATRIX OPERATION USING METHOD 21 IN
!       REAL DOUBLE PRECISION
!
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA212 USES METHOD 21 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  UNPACK AS MANY COLUMNS OF "B" INTO MEMORY AS POSSIBLE
!           LEAVING SPACE FOR A COLUMN OF "D" FOR EVERY COLUMN "B" READ.
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  USE GETSTR TO READ MATRIX "A", (SEE SUBROUTINES MMARC1,2,3,4)
!       5.  USE UNPACK TO READ MATRIX "C".
!
!     MEMORY FOR EACH COLUMN OF "B" IS AS FOLLOWS:
!         Z(1)   = FIRST NON-ZERO ROW NUMBER FOR COLUMN
!         Z(2)   = LAST NON-ZERO ROW NUMBER FOR COLUMN
!         Z(3-N) = VALUES OF NON-ZERO ROWS
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
!
!   OPEN CORE ALLOCATION AS FOLLOWS:
!     Z( 1        ) = ARRAY FOR ONE COLUMN OF "A" MATRIX
!     Z( IDX      ) = ARRAY FOR MULTIPLE COLUMNS OF "D" MATRIX
!     Z( IBX      ) = ARRAY FOR MULTIPLE COLUMNS OF "B" MATRIX
!        THROUGH
!     Z( LASMEM   )
!     Z( IBUF4    ) = BUFFER FOR "D" FILE
!     Z( IBUF3    ) = BUFFER FOR "C" FILE
!     Z( IBUF2    ) = BUFFER FOR "B" FILE
!     Z( IBUF1    ) = BUFFER FOR "A" FILE
!     Z( NZ       ) = END OF OPEN CORE THAT IS AVAILABLE
!
!
! PROCESS ALL OF THE COLUMNS OF "A"
!
   DO ii = 1 , nac
!
! READ A COLUMN FROM "A" MATRIX
!
      CALL mmarc2(Zi,Zd)
!
! CHECK FOR NULL COLUMN ON "A"
!
      IF ( Zi(1)/=0 ) THEN
         IF ( t/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
! DOUBLE PRECISION
            DO i = 1 , ncolpp
               indxb = ibx + 2*i + (i-1)*nwddnbr
               irowb1 = Zi(indxb-2)
               IF ( irowb1==0 ) CYCLE
               irowbn = Zi(indxb-1)
               indx = 1
               irowa1 = Zi(indx)
               indxb = ((indxb+1)/2) - irowb1
               indxd = idx + (i-1)*nwddndr
               indxd = ((indxd+1)/2) - 1 + ii
               SPAG_Loop_3_1: DO
                  irows = Zi(indx+1)
                  irowan = irowa1 + irows - 1
                  irow1 = max0(irowa1,irowb1)
                  irown = min0(irowan,irowbn)
                  IF ( irown>=irow1 ) THEN
                     indxa = ((indx+1)/2) + 1 - irowa1
                     DO k = irow1 , irown
                        Zd(indxd) = Zd(indxd) + Zd(indxa+k)*Zd(indxb+k)
                     ENDDO
                  ENDIF
                  indx = indx + 2 + irows*nwdd
                  IF ( indx>=lasind ) EXIT SPAG_Loop_3_1
                  irowa1 = Zi(indx)
               ENDDO SPAG_Loop_3_1
            ENDDO
         ELSE
!
! "A" NON-TRANSPOSE CASE    ( A*B + C )
!
! DOUBLE PRECISION
            DO i = 1 , ncolpp
               indx = 1
               irowa1 = Zi(indx)
               indxb = ibx + 2*i + (i-1)*nwddnbr
               irowb1 = Zi(indxb-2)
               irowbn = Zi(indxb-1)
               IF ( ii>=irowb1 .AND. ii<=irowbn ) THEN
                  indxb = ((indxb+1)/2) + ii - irowb1
                  IF ( Zd(indxb)/=0.0D0 ) THEN
                     indxd = idx + (i-1)*nwddndr
                     indxd = ((indxd+1)/2) - 1
                     SPAG_Loop_3_2: DO
                        irows = Zi(indx+1)
                        irowan = irowa1 + irows - 1
                        indxa = ((indx+1)/2) + 1 - irowa1
                        DO k = irowa1 , irowan
                           Zd(indxd+k) = Zd(indxd+k) + Zd(indxa+k)*Zd(indxb)
                        ENDDO
                        indx = indx + 2 + irows*nwdd
                        IF ( indx>=lasind ) EXIT SPAG_Loop_3_2
                        irowa1 = Zi(indx)
                     ENDDO SPAG_Loop_3_2
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF
! END OF PROCESSING THIS COLUMN OF "A" FOR THIS PASS
   ENDDO
!  NOW SAVE COLUMNS COMPLETED
   DO k = 1 , ncolpp
      indx = idx2 + (k-1)*ndr
      CALL pack(Zd(indx+1),filed,filed)
   ENDDO
END SUBROUTINE mma212
