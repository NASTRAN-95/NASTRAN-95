!*==mma401.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE mma401(Zi,Zr)
   IMPLICIT NONE
   USE I_MMACOM
   USE C_MPYADX
   USE C_NAMES
   USE C_PACKX
   USE C_SYSTEM
   USE C_TYPE
   USE C_UNPAKX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Zi
   REAL , DIMENSION(2) :: Zr
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , icolb , ii , indx , indxa , indxb , indxbl , indxbv , indxd , irow1 , irowa1 , irowan , irowb1 , irowbn , irown , &
            & irows , iwr , k , nac , nadens , naform , nanzwd , nar , natype , nbc , nbdens , nbform , nbnzwd , nbr , nbtype ,     &
            & ncc , ncdens , ncform , ncnzwd , ncr , nctype , ndc , nddens , ndform , ndnzwd , ndr , ndtype
   REAL :: sysbuf
!
! End of declarations rewritten by SPAG
!
!
!     MMA401 PERFORMS THE MATRIX OPERATION USING METHOD 40
!       IN REAL SINGLE PRECISION
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA401 USES METHOD 40 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  READ AS MANY COLUMNS OF "B" INTO MEMORY AS POSSIBLE
!           INTO MEMORY IN COMPACT FORM LEAVING SPACE FOR A FULL
!           COLUMN OF "D" FOR EVERY COLUMN "B" READ.  SEE SUBROUTINES
!           MMARM1,2,3,4 FOR FORMAT OF COMPACT FORM.
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  CALL UNPACK TO READ MATRICES "A" AND "C".
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Iwr)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
!
!   OPEN CORE ALLOCATION AS FOLLOWS:
!     Z( 1        ) = ARRAY FOR ONE COLUMN OF "A" MATRIX
!     Z( IBX      ) = ARRAY FOR MULTIPLE COLUMNS OF "B" MATRIX
!                     (STORED IN COMPACT FORM)
!     Z( IDX      ) = ARRAY FOR MULTIPLE COLUMNS OF "D" MATRIX
!                     (FULL COLUMN SPACE ALLOCATION)
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
!      print *,' processing column of a, ii=',ii
      Iurow1 = -1
      Typeu = ndtype
      CALL unpack(*100,Filea,Zr(1))
      irowa1 = Iurow1
      irowan = Iurown
      indxa = 1 - irowa1
      IF ( T/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
! SINGLE PRECISION
         indxb = ibx
         DO i = 1 , ncolpp
            icolb = ibrow + i
            IF ( icolb/=iabs(Zi(indxb)) ) GOTO 200
            indxbl = Zi(indxb+1) + ibx - 1
            indxb = indxb + 2
            indxd = (idx+(i-1)*ndr) + ii - 1
            DO WHILE ( indxb<indxbl )
               irowb1 = Zi(indxb)
               irows = Zi(indxb+1)
               irowbn = irowb1 + irows - 1
               irow1 = max0(irowa1,irowb1)
               irown = min0(irowan,irowbn)
               IF ( irown>=irow1 ) THEN
                  indxbv = indxb + 2 - irowb1
                  DO k = irow1 , irown
                     Zr(indxd) = Zr(indxd) + Zr(indxa+k)*Zr(indxbv+k)
                  ENDDO
               ENDIF
               indxb = indxb + 2 + irows
            ENDDO
            indxb = indxbl
         ENDDO
      ELSE
!
! "A" NON-TRANSPOSE CASE    ( A*B + C )
!
! SINGLE PRECISION
         indxb = ibx
         DO i = 1 , ncolpp
            icolb = ibrow + i
            IF ( icolb/=iabs(Zi(indxb)) ) GOTO 200
            indxbl = Zi(indxb+1) + ibx - 1
            indxb = indxb + 2
            indxd = (idx+(i-1)*ndr) - 1
            DO WHILE ( indxb<indxbl )
               irowb1 = Zi(indxb)
               irows = Zi(indxb+1)
               irowbn = irowb1 + irows - 1
               IF ( ii>irowbn ) THEN
                  indxb = indxb + 2 + irows
               ELSE
                  IF ( ii>=irowb1 ) THEN
                     indxbv = indxb + 2 + ii - irowb1
                     IF ( Zr(indxbv)/=0.0 ) THEN
                        DO k = irowa1 , irowan
                           Zr(indxd+k) = Zr(indxd+k) + Zr(indxa+k)*Zr(indxbv)
                        ENDDO
                     ENDIF
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
            indxb = indxbl
         ENDDO
      ENDIF
! END OF PROCESSING THIS COLUMN OF "A" FOR THIS PASS
 100  ENDDO
!  NOW SAVE COLUMNS COMPLETED
   DO k = 1 , ncolpp
      indx = idx + (k-1)*ndr
      CALL pack(Zr(indx),Filed,Filed)
   ENDDO
   GOTO 99999
 200  WRITE (iwr,99001) icolb , Zi(indxb) , ibx , indxb
99001 FORMAT (' UNEXPECTED COLUMN FOUND IN PROCESSING MATRIX B',/,' COLUMN EXPECTED:',I6,/,' COLUMN FOUND   :',I6,/,' IBX =',I7,    &
             &'  INDXB =',I7)
   CALL mesage(-61,0,0)
99999 END SUBROUTINE mma401
