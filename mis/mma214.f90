
 
SUBROUTINE mma214(Zi,Zd,Zdc)
   IMPLICIT NONE
   INCLUDE 'MMACOM.COM'
!
! COMMON variable declarations
!
   INTEGER Cls , Clsrew , Filea(7) , Fileb(7) , Filec(7) , Filed(7) , Incrp , Incru , Iprc(2) , Iprow1 , Iprown , Irc(4) , Iurow1 , &
         & Iurown , Ksystm(152) , Nac , Nadens , Naform , Nanzwd , Nar , Natype , Nbc , Nbdens , Nbform , Nbnzwd , Nbr , Nbtype ,   &
         & Ncc , Ncdens , Ncform , Ncnzwd , Ncr , Nctype , Ndc , Nddens , Ndform , Ndnzwd , Ndr , Ndtype , Nout , Nwords(4) , Nz ,  &
         & Rd , Rdrew , Signab , Signc , T , Typei , Typep , Typeu , Wrt , Wrtrew
   REAL Prec1 , Scrtch , Sysbuf , Time
   COMMON /mpyadx/ Filea , Fileb , Filec , Filed , Nz , T , Signab , Signc , Prec1 , Scrtch , Time
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /packx / Typei , Typep , Iprow1 , Iprown , Incrp
   COMMON /system/ Ksystm
   COMMON /type  / Iprc , Nwords , Irc
   COMMON /unpakx/ Typeu , Iurow1 , Iurown , Incru
!
! Dummy argument declarations
!
   DOUBLE PRECISION Zd(2)
   DOUBLE COMPLEX Zdc(2)
   INTEGER Zi(2)
!
! Local variable declarations
!
   INTEGER i , ii , indx , indxa , indxb , indxbv , indxd , irow1 , irowa1 , irowan , irowb1 , irowbn , irown , irows , k , kcnt
!
! End of declarations
!
!
!     MMA214 PERFORMS THE MATRIX OPERATION USING METHOD 21
!       IN COMPLEX DOUBLE PRECISION
!
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA214 USES METHOD 21 WHICH IS AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  UNPACK AS MANY COLUMNS OF "B" INTO MEMORY AS POSSIBLE
!           LEAVING SPACE FOR A COLUMN OF "D" FOR EVERY COLUMN "B" READ.
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  USE UNPACK TO READ MATRICES "B" AND "C".
!
!     MEMORY FOR EACH COLUMN OF "B" IS AS FOLLOWS:
!         Z(1)   = FIRST NON-ZERO ROW NUMBER FOR COLUMN
!         Z(2)   = LAST NON-ZERO ROW NUMBER FOR COLUMN
!         Z(3-N) = VALUES OF NON-ZERO ROWS
!
   EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
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
   DO ii = 1 , Nac
!
! READ A COLUMN FROM THE "A" MATRIX
!
      CALL mmarc4(Zi,Zd)
!
! CHECK IF COLUMN FROM "A" IS NULL
!
      IF ( Zi(1)/=0 ) THEN
         IF ( T/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
! COMLEX DOUBLE PRECISION
            DO i = 1 , Ncolpp
               indxb = Ibx + 2*i + (i-1)*Nwddnbr
               irowb1 = Zi(indxb-2)
               IF ( irowb1==0 ) CYCLE
               irowbn = Zi(indxb-1)
               indx = 1
               irowa1 = Zi(indx)
               indxd = Idx4 + (i-1)*Ndr + ii
               DO
                  irows = Zi(indx+1)
                  irowan = irowa1 + irows - 1
                  irow1 = max0(irowa1,irowb1)
                  irown = min0(irowan,irowbn)
                  IF ( irown>=irow1 ) THEN
                     indxbv = (((indxb+1)/2)+2*(irow1-irowb1)) - 1
                     indxa = (((indx+3)/2)+2*(irow1-irowa1)) - 1
                     kcnt = (irown-irow1)*2 + 1
                     DO k = 1 , kcnt , 2
                        Zdc(indxd) = Zdc(indxd) + dcmplx(Zd(indxa+k),Zd(indxa+k+1))*dcmplx(Zd(indxbv+k),Zd(indxbv+k+1))
                     ENDDO
                  ENDIF
                  indx = indx + 2 + irows*Nwdd
                  IF ( indx>=Lasind ) EXIT
                  irowa1 = Zi(indx)
               ENDDO
            ENDDO
         ELSE
!
! "A" NON-TRANSPOSE CASE    ( A*B + C )
!
! COMLEX DOUBLE PRECISION
            DO i = 1 , Ncolpp
               indx = 1
               irowa1 = Zi(indx)
               indxb = Ibx + 2*i + (i-1)*Nwddnbr
               irowb1 = Zi(indxb-2)
               irowbn = Zi(indxb-1)
               IF ( ii>=irowb1 .AND. ii<=irowbn ) THEN
                  indxb = ((indxb+1)/2) + 2*(ii-irowb1)
                  IF ( Zd(indxb)/=0.0D0 .OR. Zd(indxb+1)/=0.0D0 ) THEN
                     indxd = Idx4 + (i-1)*Ndr
                     DO
                        irows = Zi(indx+1)
                        irowan = irowa1 + irows - 1
                        indxa = (indx+3)/2
                        DO k = irowa1 , irowan
                           Zdc(indxd+k) = Zdc(indxd+k) + dcmplx(Zd(indxa),Zd(indxa+1))*dcmplx(Zd(indxb),Zd(indxb+1))
                           indxa = indxa + 2
                        ENDDO
                        indx = indx + 2 + irows*Nwdd
                        IF ( indx>=Lasind ) EXIT
                        irowa1 = Zi(indx)
                     ENDDO
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF
! END OF PROCESSING THIS COLUMN OF "A" FOR THIS PASS
   ENDDO
!  NOW SAVE COLUMNS COMPLETED
   DO k = 1 , Ncolpp
      indx = Idx4 + (k-1)*Ndr
      CALL pack(Zdc(indx+1),Filed,Filed)
   ENDDO
END SUBROUTINE mma214
