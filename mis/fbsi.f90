
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
   IMPLICIT NONE
   INTEGER Cdp , Csp , Dbb(7) , Dbl(7) , Dbu(7) , Dbx(7) , Eofnrw , Eol , I1 , I2 , Incr1 , Incr2 , Ipos(7) , Itype1 , Itype2 ,     &
         & Itype3 , Ix , Iy , J1 , J2 , Ksys94 , Lasind , Lcore , Lout , Ncol , Norew , Nout , Nvec , Nvecsz , Nwds , Prc(2) ,      &
         & Prec , Rd , Rdp , Rdrew , Rew , Rlcmpx(4) , Rsp , Scrx , Sign , Skip(91) , Sysbuf , Typeb , Typex , Words(4) , Wrt ,     &
         & Wrtrew
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   DOUBLE PRECISION Xd(2) , Yd(2)
   REAL Xs(4) , Ys(4)
   COMMON /fbsm  / Nvec , Nvecsz , Nwds , Lasind , Ipos
   COMMON /fbsx  / Dbl , Dbu , Dbb , Dbx , Lcore , Prec , Sign , Scrx
   COMMON /logout/ Lout
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp
   COMMON /packx / Itype1 , Itype2 , I1 , J1 , Incr1
   COMMON /system/ Sysbuf , Nout , Skip , Ksys94
   COMMON /type  / Prc , Words , Rlcmpx
   COMMON /unpakx/ Itype3 , I2 , J2 , Incr2
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zblpkx/ Yd , Iy
   COMMON /zntpkx/ Xd , Ix , Eol
   DOUBLE PRECISION Zd(1)
   REAL Zs(1)
   INTEGER begn , block(15) , buf1 , buf2 , end , i , icdpsg , icspsg , iname(2) , j , k , l , l47 , last , last2 , ll , ln ,       &
         & memavl , mtria , nrhv , nrhvwd , rcb , rcl , subnam(2) , switch , typel
   LOGICAL ident
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
   rcb = Rlcmpx(Typeb)
   rcl = Rlcmpx(typel)
   Nwds = Words(Typeb)
   IF ( rcb==rcl .AND. typel>Typeb ) Nwds = Words(typel)
   nrhvwd = Nwds*Ncol
   Nwds = Words(typel)
   nrhv = Dbb(2)
   ident = .FALSE.
   IF ( Dbb(4)==8 ) ident = .TRUE.
   IF ( ident ) nrhv = Ncol
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
         nrhvwd = 4*Ncol
      ELSE
         nrhvwd = 2*Ncol
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
      WRITE (Lout,99001) Dbl(1) , iname , Ipos(1) , Ncol , Lcore , memavl
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
   J2 = Ncol
   Incr2 = 1
   I1 = 1
   J1 = Ncol
   Incr1 = 1
   Itype1 = typel
   Itype2 = Typex
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
            DO
               CALL zntpki
               Zs(l+Ix) = Xs(1)
               Zs(l+Ix+Ncol) = Xs(2)
               IF ( Eol/=0 ) EXIT
            ENDDO
 10         l = l + 2*Ncol
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
            DO
               CALL zntpki
               Zd(l+Ix) = Xd(1)
               Zd(l+Ix+Ncol) = Xd(2)
               IF ( Eol/=0 ) EXIT
            ENDDO
 20         l = l + 2*Ncol
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
   Nvecsz = Ncol
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
         CALL bldpk(Csp,Typex,Dbx,0,0)
         DO i = 1 , Ncol
            Ys(1) = Zs(l+i)
            Ys(2) = Zs(l+i+Ncol)
            Iy = i
            CALL zblpki
         ENDDO
         CALL bldpkn(Dbx,0,Dbx)
         l = l + 2*Ncol
      ENDDO
   ELSEIF ( switch==3 ) THEN
!
!     SPECIAL CASE - LOWER TRIANGULAR MATRIX IS RDP AND VECTORS ARE CDP
!
      l = 0
      DO k = 1 , nrhv
         CALL bldpk(Cdp,Typex,Dbx,0,0)
         DO i = 1 , Ncol
            Yd(1) = Zd(l+i)
            Yd(2) = Zd(l+i+Ncol)
            Iy = i
            CALL zblpki
         ENDDO
         CALL bldpkn(Dbx,0,Dbx)
         l = l + 2*Ncol
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