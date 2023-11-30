
 
SUBROUTINE mma202(Zi,Zd)
   IMPLICIT NONE
   INCLUDE 'MMACOM.COM'
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
   DOUBLE PRECISION Zd(2)
   INTEGER Zi(2)
   INTEGER i , ii , indx , indxa , indxb , indxd , irow1 , irowa1 , irowan , irowb1 , irowbn , irown , k
!
!     MMA202 PERFORMS THE MATRIX OPERATION USING METHOD 20 IN
!       REAL DOUBLE PRECISION
!
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA202 USES METHOD 20 WHICH IS AS FOLLOWS:
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
   DO ii = 1 , Nac
      Iurow1 = -1
      Typeu = Ndtype
      CALL unpack(*100,Filea,Zd(1))
      irowa1 = Iurow1
      irowan = Iurown
      indxa = 1 - irowa1
      IF ( T/=0 ) THEN
!
!  TRANSPOSE CASE ( A(T) * B + C )
!
! DOUBLE PRECISION
         DO i = 1 , Ncolpp
            indxb = Ibx + 2*i + (i-1)*Nwddnbr
            irowb1 = Zi(indxb-2)
            IF ( irowb1/=0 ) THEN
               irowbn = Zi(indxb-1)
               irow1 = max0(irowa1,irowb1)
               irown = min0(irowan,irowbn)
               IF ( irown>=irow1 ) THEN
                  indxb = ((indxb+1)/2) - irowb1
                  indxd = Idx + (i-1)*Nwddndr
                  indxd = ((indxd+1)/2) - 1 + ii
                  DO k = irow1 , irown
                     Zd(indxd) = Zd(indxd) + Zd(indxa+k)*Zd(indxb+k)
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
      ELSE
!
! "A" NON-TRANSPOSE CASE    ( A*B + C )
!
! DOUBLE PRECISION
         DO i = 1 , Ncolpp
            indxb = Ibx + 2*i + (i-1)*Nwddnbr
            irowb1 = Zi(indxb-2)
            irowbn = Zi(indxb-1)
            IF ( ii>=irowb1 .AND. ii<=irowbn ) THEN
               indxb = ((indxb+1)/2) + ii - irowb1
               IF ( Zd(indxb)/=0.0D0 ) THEN
                  indxd = Idx + (i-1)*Nwddndr
                  indxd = ((indxd+1)/2) - 1
                  DO k = irowa1 , irowan
                     Zd(indxd+k) = Zd(indxd+k) + Zd(indxa+k)*Zd(indxb)
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
      ENDIF
! END OF PROCESSING THIS COLUMN OF "A" FOR THIS PASS
 100  ENDDO
!  NOW SAVE COLUMNS COMPLETED
   DO k = 1 , Ncolpp
      indx = Idx2 + (k-1)*Ndr
      CALL pack(Zd(indx+1),Filed,Filed)
   ENDDO
END SUBROUTINE mma202