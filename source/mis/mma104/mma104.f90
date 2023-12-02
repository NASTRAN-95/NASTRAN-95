!*==mma104.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
 
SUBROUTINE mma104(Zi,Zd,Zdc)
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
   REAL*8 , DIMENSION(2) :: Zd
   COMPLEX*16 , DIMENSION(2) :: Zdc
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ibrow2 , ibrowi , idrow , idx4x , ii , indxa , indxb , irow1 , irowa1 , irowan , irowb1 , irowbn , irown , j , k ,&
            & kcnt , nac , nadens , naform , nanzwd , nar , natype , nbc , nbdens , nbform , nbnzwd , nbr , nbtype , ncc , ncdens , &
            & ncform , ncnzwd , ncr , nctype , ndc , nddens , ndform , ndnzwd , ndr , ndtype , nout , nwddnar
   REAL :: sysbuf
!
! End of declarations rewritten by SPAG
!
!
!     MMA10 PERFORMS THE MATRIX OPERATION USING METHOD 10 AND
!       COMPLEX DOUBLE PRECISION
!
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA104 USES METHOD 10 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  UNPACK AS MANY COLUMNS OF "A" INTO MEMORY AS POSSIBLE
!           LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  USE UNPACK TO READ MATRICES "B" AND "C".
!
!     MEMORY FOR EACH COLUMN OF "A" IS AS FOLLOWS:
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
! PROCESS ALL OF THE COLUMNS OF "B", ADD "C" DATA ON FIRST PASS
   DO ii = 1 , nbc
      Iurow1 = -1
      Typeu = ndtype*Signab
      CALL unpack(*50,Fileb,Zdc(1))
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
         CALL unpack(*150,ifile,Zdc(idx4+1))
         GOTO 200
      ENDIF
 150  DO j = 1 , ndr
         Zdc(idx4+j) = (0.0,0.0)
      ENDDO
 200  nwddnar = nwdd*nar
!
! CHECK IF COLUMN OF "B" IS NULL
!
      IF ( irowb1/=0 ) THEN
         IF ( T/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
            idrow = ibrow
! COMPLEX DOUBLE PRECISION
            DO i = 1 , ncolpp
               indxa = iax + 2*i + (i-1)*nwddnar
               irowa1 = Zi(indxa-2)
               IF ( irowa1/=0 ) THEN
                  irowan = Zi(indxa-1)
                  irow1 = max0(irowa1,irowb1)
                  irown = min0(irowan,irowbn)
                  IF ( irown>=irow1 ) THEN
                     indxa = ((indxa+1)/2) + 2*(irow1-irowa1) - 1
                     idx4x = idx4 + idrow
                     indxb = 2*(irow1-irowb1)
                     kcnt = (irown-irow1)*2 + 1
                     DO k = 1 , kcnt , 2
                        Zdc(idx4x+i) = Zdc(idx4x+i) + dcmplx(Zd(indxa+k),Zd(indxa+k+1))*dcmplx(Zd(indxb+k),Zd(indxb+k+1))
                     ENDDO
                  ENDIF
               ENDIF
            ENDDO
         ELSE
!
! "A" NON-TRANSPOSE CASE    ( A * B  +  C )
!
! COMLEX DOUBLE PRECISION
            DO i = 1 , ncolpp
               ibrowi = ibrow + i
               IF ( ibrowi>=irowb1 .AND. ibrowi<=irowbn ) THEN
                  ibrow2 = 2*(ibrow+i-irowb1) + 1
                  IF ( Zd(ibrow2)/=0.D0 .OR. Zd(ibrow2+1)/=0.D0 ) THEN
                     indxa = iax + 2*i + (i-1)*nwddnar
                     irowa1 = Zi(indxa-2)
                     IF ( irowa1/=0 ) THEN
                        irowan = Zi(indxa-1)
                        indxa = ((indxa+1)/2) - 2
                        DO k = irowa1 , irowan
                           indxa = indxa + 2
                           Zdc(idx4+k) = Zdc(idx4+k) + dcmplx(Zd(indxa),Zd(indxa+1))*dcmplx(Zd(ibrow2),Zd(ibrow2+1))
                        ENDDO
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF
! END OF PROCESSING THIS COLUMN FOR THIS PASS
!  NOW SAVE COLUMN
      CALL pack(Zdc(idx4+1),ofile,Filed)
   ENDDO
END SUBROUTINE mma104
