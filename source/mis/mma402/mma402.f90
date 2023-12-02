!*==mma402.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
SUBROUTINE mma402(Zi,Zd)
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
   INTEGER :: i , icolb , ii , indx , indxa , indxb , indxbl , indxbv , indxdv , irow1 , irowa1 , irowan , irowb1 , irowbn , irown ,&
            & irows , iwr , k , nac , nadens , naform , nanzwd , nar , natype , nbc , nbdens , nbform , nbnzwd , nbr , nbtype ,     &
            & ncc , ncdens , ncform , ncnzwd , ncr , nctype , ndc , nddens , ndform , ndnzwd , ndr , ndtype
   REAL :: sysbuf
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
! End of declarations rewritten by SPAG
!
!
!     MMA402 PERFORMS THE MATRIX OPERATION USING METHOD 40
!       IN REAL DOUBLE PRECISION
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA402 USES METHOD 40 WHICH IS AS FOLLOWS:
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
            iurow1 = -1
            typeu = ndtype
            CALL unpack(*20,filea,Zd(1))
            irowa1 = iurow1
            irowan = iurown
            indxa = 1 - irowa1
            IF ( t/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
! DOUBLE PRECISION
               indxb = ibx
               DO i = 1 , ncolpp
                  icolb = ibrow + i
                  IF ( icolb/=iabs(Zi(indxb)) ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  indxbl = Zi(indxb+1) + ibx - 1
                  indxb = indxb + 2
                  indxdv = idx2 + (i-1)*ndr + ii
                  DO WHILE ( indxb<indxbl )
                     irowb1 = Zi(indxb)
                     irows = Zi(indxb+1)
                     irowbn = irowb1 + irows - 1
                     irow1 = max0(irowa1,irowb1)
                     irown = min0(irowan,irowbn)
                     IF ( irown>=irow1 ) THEN
                        indxbv = ((indxb+3)/2) - irowb1
                        DO k = irow1 , irown
                           Zd(indxdv) = Zd(indxdv) + Zd(indxa+k)*Zd(indxbv+k)
                        ENDDO
                     ENDIF
                     indxb = indxb + 2 + irows*nwdd
                  ENDDO
                  indxb = indxbl
               ENDDO
            ELSE
!
! "A" NON-TRANSPOSE CASE    ( A*B + C )
!
! DOUBLE PRECISION
               indxb = ibx
               DO i = 1 , ncolpp
                  icolb = ibrow + i
                  IF ( icolb/=iabs(Zi(indxb)) ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  indxbl = Zi(indxb+1) + ibx - 1
                  indxb = indxb + 2
                  indxdv = idx2 + (i-1)*ndr
                  SPAG_Loop_3_1: DO WHILE ( indxb<indxbl )
                     irowb1 = Zi(indxb)
                     irows = Zi(indxb+1)
                     irowbn = irowb1 + irows - 1
                     IF ( ii>irowbn ) THEN
                        indxb = indxb + 2 + irows*nwdd
                     ELSE
                        IF ( ii>=irowb1 ) THEN
                           indxbv = ((indxb+3)/2) + ii - irowb1
                           IF ( Zd(indxbv)/=0.0D0 ) THEN
                              DO k = irowa1 , irowan
                                 Zd(indxdv+k) = Zd(indxdv+k) + Zd(indxa+k)*Zd(indxbv)
                              ENDDO
                           ENDIF
                        ENDIF
                        EXIT SPAG_Loop_3_1
                     ENDIF
                  ENDDO SPAG_Loop_3_1
                  indxb = indxbl
               ENDDO
            ENDIF
! END OF PROCESSING THIS COLUMN OF "A" FOR THIS PASS
 20      ENDDO
!  NOW SAVE COLUMNS COMPLETED
         DO k = 1 , ncolpp
            indx = idx2 + (k-1)*ndr + 1
            CALL pack(Zd(indx),filed,filed)
         ENDDO
         RETURN
      CASE (2)
         WRITE (iwr,99001) icolb , Zi(indxb) , ibx , indxb
99001    FORMAT (' UNEXPECTED COLUMN FOUND IN PROCESSING MATRIX B',/,' COLUMN EXPECTED:',I6,/,' COLUMN FOUND   :',I6,/,' IBX =',I7, &
                &'  INDXB =',I7)
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mma402
