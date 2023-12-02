!*==fbsi.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fbsi(Zs,Zd)
!
!     GIVEN A LOWER TRIANGULAR FACTOR WITH DIAGONAL SUPERIMPOSED, AND
!     WRITTEN WITH TRAILING STRING DEFINITION WORDS, FBS WILL PERFORM
!     THE FORWARD-BACKWARD SUBSTITUTION NECESSARY TO SOLVE A LINEAR
!     SYSTEM OF EQUATIONS.
!
!     OPEN CORE IS DEFINED AS FOLLOWS
!
!     ZS(   1         ) - FIRST RIGHT HAND VECTOR ON FILE DBB
!                         (SIZE = NCOL*NWDS)
!                         NCOL = NUMBER OF COLUMNS (ROWS) IN LOWER
!                                TRIANGULAR MATRIX
!                         NWDS = 1, IF MATRICES ARE REAL SINGLE
!                              = 2, IF MATRICES ARE REAL DOUBLE OR
!                                COMPLEX SINGLE
!                              = 4, IF MATRICES ARE COMPLEX DOUBLE
!     ZS( NCOL*NWDS+1 ) - NEXT RIGHT HAND VECTOR
!         .
!         .               ( "NRHV" RIGHT HAND VECTORS WILL BE LOADED INTO
!         .               MEMORY)
!         .
!     ZS( MTRIA       ) - MEMORY FOR STORAGE OF ALL OR PART OF THE LOWER
!                         TRIANGULAR MATRIX.  (SEE SUBROUTINE FBSRDM FOR
!                         FORMAT OF STORAGE OF MATRIX.)
!     ZS( BUF1        ) - BUFFER FOR FILE WITH RIGHT HAND VECTORS
!                         AND FOR SOLUTION VECTORS
!     ZS( BUF2        ) - BUFFER FOR FILE WITH TRIANGULAR MATRIX
!
USE C_FBSM
USE C_FBSX
USE C_LOGOUT
USE C_NAMES
USE C_PACKX
USE C_SYSTEM
USE C_TYPE
USE C_UNPAKX
USE C_XMSSG
USE C_ZBLPKX
USE C_ZNTPKX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Zs
   REAL(REAL64) , DIMENSION(1) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: begn , end
   INTEGER , DIMENSION(15) :: block
   INTEGER :: buf1 , buf2 , i , icdpsg , icspsg , j , k , l , l47 , last , last2 , ll , ln , memavl , mtria , ncol , nrhv , nrhvwd ,&
            & rcb , rcl , switch , typeb , typel , typex
   LOGICAL :: ident
   INTEGER , DIMENSION(2) :: iname
   INTEGER , DIMENSION(2) , SAVE :: subnam
   REAL , DIMENSION(4) :: xs , ys
   EXTERNAL bldpk , bldpkn , close , conmsg , fbsi1 , fbsi2 , fbsi3 , fbsi4 , fbsrdm , fname , gopen , intpk , pack , sswtch ,      &
          & unpack , zblpki , zntpki
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (Dbl(2),Ncol) , (Dbb(5),Typeb) , (Dbx(5),Typex) , (Xd(1),Xs(1)) , (Yd(1),Ys(1))
   DATA subnam/4HFBSI , 4H    /
   DATA begn/4HBEGN/
   DATA end/4HEND /
!
!     GENERAL INITIALIZATION
!
   buf2 = Lcore - Sysbuf
   buf1 = buf2 - Sysbuf
   typel = Dbl(5)
   rcb = Rlcmpx(typeb)
   rcl = Rlcmpx(typel)
   Nwds = Words(typeb)
   IF ( rcb==rcl .AND. typel>typeb ) Nwds = Words(typel)
   nrhvwd = Nwds*ncol
   Nwds = Words(typel)
   nrhv = Dbb(2)
   ident = .FALSE.
   IF ( Dbb(4)==8 ) ident = .TRUE.
   IF ( ident ) nrhv = ncol
   switch = 1
!
! SET SWITCH AS FOLLOWS:
!  =1, IF LOWER TRIANGULAR MATRIX AND RIGHT HAND VECTORS ARE SAME TYPE
!  =2, LOWER TRIANGULAR MATRIX IS REAL SINGLE AND RIGHT HAND VECTOR IS
!      COMPLEX
!  =3, LOWER TRIANGULAR MATRIX IS REAL DOUBLE AND RIGHT HAND VECTOR IS
!      COMPLEX
!  (NOTE, IF SWITCH IS .NE. 1, THEN THE REAL AND IMAGINARY PARTS OF THE
!   THE RIGHT HAND VECTOR ARE TREATED AS TWO SEPARATE VECTORS.  I.E.,
!   THE REAL PART BECOMES ONE VECTOR AND THE IMAGINARY PART BECOMES A
!   SECOND VECTOR.)
!
   IF ( typel==Rsp .AND. rcb==2 ) switch = 2
   IF ( typel==Rdp .AND. rcb==2 ) switch = 3
   IF ( switch/=1 ) THEN
      IF ( switch==3 ) THEN
         nrhvwd = 4*ncol
      ELSE
         nrhvwd = 2*ncol
      ENDIF
   ENDIF
   mtria = nrhv*nrhvwd + 1
!
! ENSURE DOUBLE WORD BOUNDARY
!
   mtria = (mtria/2)*2 + 1
   memavl = buf1 - mtria - 2
   subnam(2) = begn
   CALL conmsg(subnam,2,0)
   CALL fbsrdm(Dbl,Zs(mtria),Zs(mtria),Zs(mtria),memavl,Zs(buf2),Lasind,Ipos)
   CALL sswtch(47,l47)
   CALL fname(Dbl,iname)
   IF ( l47/=0 ) THEN
      WRITE (Lout,99001) Dbl(1) , iname , Ipos(1) , ncol , Lcore , memavl
99001 FORMAT (4X,' FORWARD BACKWARD SUBSTITUTION OF FILE ',I3,'   NAME=',2A4,/,4X,                                                  &
             &' LAST COLUMN OF TRIANGULAR MATRIX IN MEMORY        =',I8,/,4X,' TOTAL COLUMNS IN TRIANGULAR MATRIX                =',&
            & I8,/,4X,' TOTAL OPEN CORE AVAILABLE FOR USE                 =',I8,/,4X,                                               &
             &' OPEN CORE AVAILABLE FOR TRIANGULAR MATRIX STORAGE =',I8)
      CALL fname(Dbb,iname)
      WRITE (Lout,99002) Dbb(1) , iname , Dbl , Dbb
99002 FORMAT (4X,' RIGHT HAND VECTOR FILE ',I3,'   NAME=',2A4,/,4X,' TRIANGULAR MATRIX TRAILER    =',7I6,/,4X,                      &
             &' RIGHT HAND VECTOR(S) TRAILER =',7I6)
   ENDIF
   I2 = 1
   J2 = ncol
   Incr2 = 1
   I1 = 1
   J1 = ncol
   Incr1 = 1
   Itype1 = typel
   Itype2 = typex
   Itype3 = Sign*typel
   Dbx(2) = 0
   Dbx(6) = 0
   Dbx(7) = 0
   block(1) = Dbl(1)
!
!     OPEN RIGHT HAND VECTORS FILE (DBB)
!
   last = nrhv*nrhvwd
   IF ( ident ) THEN
!
!     SPECIAL CASE - GENERATE IDENTITY MATRIX
!
      last = nrhv*nrhvwd
      DO k = 1 , last
         Zd(k) = 0.0D+0
      ENDDO
      l = 0
      IF ( typel==2 ) THEN
         DO k = 1 , nrhv
            Zd(l+k) = 1.0D+0
            l = l + nrhvwd
         ENDDO
      ELSEIF ( typel==3 ) THEN
         DO k = 1 , nrhv
            Zs(l+2*k-1) = 1.0
            l = l + nrhvwd
         ENDDO
      ELSEIF ( typel==4 ) THEN
         DO k = 1 , nrhv
            Zd(l+2*k-1) = 1.0D+0
            l = l + nrhvwd
         ENDDO
      ELSE
         DO k = 1 , nrhv
            Zs(l+k) = 1.0
            l = l + nrhvwd
         ENDDO
      ENDIF
   ELSE
      CALL gopen(Dbb,Zs(buf1),Rdrew)
      IF ( switch==2 ) THEN
!
!     SPECIAL CASE - LOWER TRIANGULAR MATRIX IS RSP AND VECTORS ARE CSP
!
         last2 = last/2
         l = 0
         DO k = 1 , last2
            Zd(k) = 0.0D+0
         ENDDO
         DO k = 1 , nrhv
            icspsg = Csp*Sign
            CALL intpk(*10,Dbb,0,icspsg,0)
            SPAG_Loop_2_1: DO
               CALL zntpki
               Zs(l+Ix) = xs(1)
               Zs(l+Ix+ncol) = xs(2)
               IF ( Eol/=0 ) EXIT SPAG_Loop_2_1
            ENDDO SPAG_Loop_2_1
 10         l = l + 2*ncol
         ENDDO
      ELSEIF ( switch==3 ) THEN
!
!     SPECIAL CASE - LOWER TRIANGULAR MATRIX IS RDP AND VECTORS ARE CDP
!
         last2 = last/2
         l = 0
         DO k = 1 , last2
            Zd(k) = 0.0D+0
         ENDDO
         DO k = 1 , nrhv
            icdpsg = Cdp*Sign
            CALL intpk(*20,Dbb,0,icdpsg,0)
            SPAG_Loop_2_2: DO
               CALL zntpki
               Zd(l+Ix) = Xd(1)
               Zd(l+Ix+ncol) = Xd(2)
               IF ( Eol/=0 ) EXIT SPAG_Loop_2_2
            ENDDO SPAG_Loop_2_2
 20         l = l + 2*ncol
         ENDDO
      ELSE
!
!     READ RIGHT HAND VECTORS INTO MEMORY
!
         DO l = 1 , last , nrhvwd
            CALL unpack(*30,Dbb,Zs(l))
            CYCLE
 30         ln = l + nrhvwd - 1
            DO ll = l , ln
               Zs(ll) = 0.
            ENDDO
         ENDDO
      ENDIF
!
!    CLOSE RIGHT HAND VECTORS FILE (DBB).
!    START FORWARD-BACKWARD SUBSTITUTION ON RIGHT HAND VECTORS
!
      CALL close(Dbb,Rew)
   ENDIF
   j = typel
   Nvec = nrhv
   Nvecsz = ncol
   IF ( switch>1 ) Nvec = Nvec*2
   IF ( j==2 ) THEN
      CALL fbsi2(block,Zs,Zs(mtria),Zs(mtria),Zs(buf2))
   ELSEIF ( j==3 ) THEN
      CALL fbsi3(block,Zs,Zs(mtria),Zs(mtria),Zs(buf2))
   ELSEIF ( j==4 ) THEN
      CALL fbsi4(block,Zs,Zs(mtria),Zs(mtria),Zs(buf2))
   ELSE
      CALL fbsi1(block,Zs,Zs(mtria),Zs(mtria),Zs(buf2))
   ENDIF
!
!     OPEN AND PACK SOLUTION VECTORS ONTO OUTPUT FILE (DBX)
!
   CALL gopen(Dbx,Zs(buf1),Wrtrew)
   IF ( switch==2 ) THEN
!
!     SPECIAL CASE - LOWER TRIANGULAR MATRIX IS RSP AND VECTORS ARE CSP
!
      l = 0
      DO k = 1 , nrhv
         CALL bldpk(Csp,typex,Dbx,0,0)
         DO i = 1 , ncol
            ys(1) = Zs(l+i)
            ys(2) = Zs(l+i+ncol)
            Iy = i
            CALL zblpki
         ENDDO
         CALL bldpkn(Dbx,0,Dbx)
         l = l + 2*ncol
      ENDDO
   ELSEIF ( switch==3 ) THEN
!
!     SPECIAL CASE - LOWER TRIANGULAR MATRIX IS RDP AND VECTORS ARE CDP
!
      l = 0
      DO k = 1 , nrhv
         CALL bldpk(Cdp,typex,Dbx,0,0)
         DO i = 1 , ncol
            Yd(1) = Zd(l+i)
            Yd(2) = Zd(l+i+ncol)
            Iy = i
            CALL zblpki
         ENDDO
         CALL bldpkn(Dbx,0,Dbx)
         l = l + 2*ncol
      ENDDO
   ELSE
!
!     NORMAL CASE - CALL PACK
!
      DO l = 1 , last , nrhvwd
         CALL pack(Zs(l),Dbx,Dbx)
      ENDDO
   ENDIF
!
!     JOB DONE. CLOSE TRIANGULAR MATRIX AND SOLUTION FILE.
!
   CALL close(Dbx,Rew)
   subnam(2) = end
   CALL conmsg(subnam,2,0)
END SUBROUTINE fbsi
