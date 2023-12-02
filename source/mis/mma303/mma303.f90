!*==mma303.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
 
SUBROUTINE mma303(Zi,Zc)
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
   COMPLEX , DIMENSION(2) :: Zc
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ibrowi , icola , idxx , ii , indxa , indxal , indxav , indxb , irow1 , irowa1 , irowan , irowb1 , irowbn , irown ,&
            & iwr , j , k , nac , nadens , naform , nanzwd , nar , natype , nbc , nbdens , nbform , nbnzwd , nbr , nbtype , ncc ,   &
            & ncdens , ncform , ncnzwd , ncr , nctype , ndc , nddens , ndform , ndnzwd , ndr , ndtype , ntms
   REAL :: sysbuf
!
! End of declarations rewritten by SPAG
!
!
!     MMA303 PERFORMS THE MATRIX OPERATION USING METHOD 30 AND
!       COMPLEX SINGLE PRECISION
!
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA303 USES METHOD 30 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  CALL 'MMARM1' TO PACK AS MANY COLUMNS OF "A" INTO MEMORY
!           AS POSSIBLE LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  USE UNPACK TO READ MATRICES "B" AND "C".
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Iwr)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
!
!   OPEN CORE ALLOCATION AS FOLLOWS:
!     Z( 1        ) = ARRAY FOR ONE COLUMN OF "B" MATRIX
!     Z( IDX      ) = ARRAY FOR ONE COLUMN OF "D" MATRIX
!     Z( IAX      ) = ARRAY FOR MULTIPLE COLUMNS OF "A" MATRIX
!        THROUGH
!     Z( LASMEM   )
!     Z( IBUF4    ) = BUFFER FOR "D" FILE
!     Z( IBUF3    ) = BUFFER FOR "C" FILE
!     Z( IBUF2    ) = BUFFER FOR "B" FILE
!     Z( IBUF1    ) = BUFFER FOR "A" FILE
!     Z( NZ       ) = END OF OPEN CORE THAT IS AVAILABLE
!
! PROCESS ALL OF THE COLUMNS OF "B";  ADD "C" DATA ON FIRST PASS
   DO ii = 1 , nbc
      Iurow1 = -1
      Typeu = ndtype
      CALL unpack(*50,Fileb,Zc(1))
      irowb1 = Iurow1
      irowbn = Iurown
      GOTO 100
 50   irowb1 = 0
      irowbn = 0
 100  IF ( ifile/=0 ) THEN
         Iurow1 = 1
         Iurown = ndr
         Typeu = ndtype
         IF ( ipass==1 ) Typeu = ndtype*Signc
         CALL unpack(*150,ifile,Zc(idx2+1))
         GOTO 200
      ENDIF
 150  DO j = 1 , ndr
         Zc(idx2+j) = (0.0,0.0)
      ENDDO
!
! CHECK IF COLUMN OF "B" IS NULL
!
 200  indxa = iax
      IF ( irowb1/=0 ) THEN
         IF ( T/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
! COMPLEX SINGLE PRECISION
            indxb = 1 - irowb1
            idxx = idx2 + ibrow
            DO i = 1 , ncolpp
               icola = ibrow + i
               IF ( icola/=iabs(Zi(indxa)) ) GOTO 300
               indxal = Zi(indxa+1) + iax - 1
               indxa = indxa + 2
               DO WHILE ( indxa<indxal )
                  irowa1 = Zi(indxa)
                  ntms = Zi(indxa+1)
                  irowan = irowa1 + ntms - 1
                  irow1 = max0(irowa1,irowb1)
                  irown = min0(irowan,irowbn)
                  IF ( irown>=irow1 ) THEN
                     indxav = ((indxa+3)/2) - irowa1
!
!         D = C + A*B
!
                     DO k = irow1 , irown
                        Zc(idxx+i) = Zc(idxx+i) + Zc(indxav+k)*Zc(indxb+k)
                     ENDDO
                  ENDIF
                  indxa = indxa + 2 + ntms*2
               ENDDO
               indxa = indxal
            ENDDO
         ELSE
!
! "A" NON-TRANSPOSE CASE    ( A * B  +  C )
!
! COMPLEX SINGLE PRECISION
            DO i = 1 , ncolpp
               indxal = Zi(indxa+1) + iax - 1
               icola = ibrow + i
               IF ( icola>=irowb1 .AND. icola<=irowbn ) THEN
                  ibrowi = icola - irowb1 + 1
                  IF ( Zc(ibrowi)/=0. ) THEN
                     IF ( icola/=iabs(Zi(indxa)) ) GOTO 300
                     indxa = indxa + 2
                     DO WHILE ( indxa<indxal )
                        irowa1 = Zi(indxa)
                        ntms = Zi(indxa+1)
                        irowan = irowa1 + ntms - 1
                        indxav = ((indxa+3)/2) - irowa1
                        DO k = irowa1 , irowan
!
!         D = C + A*B
!
                           Zc(idx2+k) = Zc(idx2+k) + Zc(indxav+k)*Zc(ibrowi)
                        ENDDO
                        indxa = indxa + 2 + ntms*2
                     ENDDO
                  ENDIF
               ENDIF
               indxa = indxal
            ENDDO
         ENDIF
      ENDIF
! END OF PROCESSING THIS COLUMN FOR THIS PASS
!  NOW SAVE COLUMN
      CALL pack(Zc(idx2+1),ofile,Filed)
   ENDDO
   GOTO 99999
 300  WRITE (iwr,99001) icola , Zi(indxa) , iax , indxa
99001 FORMAT (' UNEXPECTED COLUMN FOUND IN PROCESSING MATRIX A',/,' COLUMN EXPECTED:',I6,/,' COLUMN FOUND   :',I6,/,' IAX =',I7,    &
             &'  INDXA=',I7)
   CALL mesage(-61,0,0)
99999 END SUBROUTINE mma303
