
SUBROUTINE solve
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION Cdet(2) , Cmndia , Det , Detc , Dett , Mindia , Minds
   INTEGER Cdp , Ia(7) , Ib , Ibbar , Ichol , Identy , Ifila(7) , Ifilb(7) , Ifilc(7) , Ifill(7) , Ifill1(7) , Ifillt(7) , Ifilx(7) &
         & , Il(7) , Ipow , Ipower , Ipr , Iprec , Iprec1 , Iscr , Iscr11 , Iscr22 , Iscr33 , Isgn , Isign1 , Isr1 , Isr2 , Isr3 ,  &
         & Isym , Itype , Iu(7) , Ja(7) , Jb(7) , Jbb , Jbbar , Jl(7) , Jpow , Jscr1 , Jscr2 , Jscr3 , Ju(7) , Jx(7) , Kl(7) ,      &
         & Kprec , Ksign , Ksystm(65) , Ku(7) , Lower , Norew , Nx , Nz , Nzz , Nzzz , Nzzzz , Outpt , Rdp , Rect , Sqr , Sym
   REAL Csp , Diag , Eofnrw , Rd , Rdrew , Rew , Row , Rsp , Upper , Wrt , Wrtrew , Z(1) , Zz(1) , Zzz(1) , Zzzz(1) , Zzzzz(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Isym , Ksign , Iprec , Itype
   COMMON /cdcmpx/ Ja , Kl , Ku , Jscr1 , Jscr2 , Jscr3 , Cdet , Jpow , Nzzzz , Cmndia , Jbb , Jbbar
   COMMON /dcompx/ Ia , Il , Iu , Isr1 , Isr2 , Isr3 , Dett , Ipow , Nzz , Mindia , Ib , Ibbar
   COMMON /fbsx  / Ifill , Ifillt , Ifilb , Ifilx , Nx , Iprec1 , Isign1 , Iscr
   COMMON /gfbsx / Jl , Ju , Jb , Jx , Nzzz , Ipr , Isgn
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr , Rect , Diag , Lower , Upper ,   &
                 & Sym , Row , Identy
   COMMON /sfact / Ifila , Ifill1 , Ifilc , Iscr11 , Iscr22 , Nz , Det , Detc , Ipower , Iscr33 , Minds , Ichol
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER a , b , dosi(3) , i , ia5 , ib5 , iform , index , iscr1 , iscr2 , iscr3 , iscr4 , iscr5 , jj , ltype , n , name(2) , no ,&
         & refus(3) , x
   INTEGER korsz
!
! End of declarations
!
!
!     SOLVE IS A DMAP DRIVER TO SOLVE THE MATRIX EQUATION AX=B
!
!     SOLVE   A,B/X/SYM/SIGN/PREC/TYPE $
!
!     SYM     =  1 - USE SYMETRIC DECOMPOSITION
!                0 - CHOOSE WHICH DECOMPOSITION BASED ON INPUT MATRIX
!               -1 - USE UNSYMETRIC DECOMPOSITION
!     ISIGN   =  1   SOLVE AX = B
!               -1   SOLVE AX =-B
!     IPREC   =  PRECISION USED IN THE FBS PASS
!     ITYPE   =  DESIRED TYPE OF THE OUTPUT MATRIX X
!
!
   EQUIVALENCE (Zz(1),Z(1))
   EQUIVALENCE (Zzz(1),Z(1))
   EQUIVALENCE (Zzzz(1),Z(1))
   EQUIVALENCE (Zzzzz(1),Z(1))
   EQUIVALENCE (Ksystm(55),Kprec) , (Ksystm(2),Outpt)
   DATA a , b , x/101 , 102 , 201/ , name/4HSOLV , 4HE   /
   DATA iscr1 , iscr2 , iscr3 , iscr4 , iscr5/301 , 302 , 303 , 304 , 305/
   DATA dosi/4HSING , 4HDOUB , 4HMLTP/ , refus/2*3H    , 3HREF/
!
!
   Ja(1) = a
   CALL rdtrl(Ja)
!
   iform = Ja(4)
   IF ( Isym<0 ) THEN
      IF ( iform==Sym ) WRITE (Outpt,99001) Uwm , name
99001 FORMAT (A25,' 2340, MODULE ',2A4,' HAS BEEN REQUESTED TO DO ','UNSYMETRIC DECOMPOSITION OF A SYMETRIC MATRIX.')
      iform = Rect
      IF ( Ja(2)==Ja(3) ) iform = Sqr
   ELSEIF ( Isym/=0 ) THEN
      IF ( Ja(2)==Ja(3) .AND. iform/=Sym ) WRITE (Outpt,99002) Swm , name
99002 FORMAT (A27,' 2341, MODULE ',2A4,' HAS BEEN FURNISHED A SQUARE ','MATRIX MARKED UNSYMETRIC FOR SYMETRIC DECOMPOSITION.')
      iform = Sym
   ENDIF
   Isym = -1
   IF ( iform==Sym ) Isym = 1
   Ja(4) = iform
   IF ( Isym<0 ) GOTO 300
!
!     SET UP CALL TO SDCOMP AND FBS
!
   index = 1
   Ichol = 0
   DO i = 1 , 7
      Ifila(i) = Ja(i)
   ENDDO
   n = Ifila(2)
   Ifill1(1) = iscr1
   Ifilc(1) = iscr2
   Iscr11 = iscr3
   Iscr22 = iscr4
   Iscr33 = iscr5
   Nz = korsz(Z)
   Ifill1(5) = Ifila(5)
   CALL sdcomp(*200,Z,Z,Z)
   Ifill1(3) = Ifill1(2)
   Ifill1(4) = Lower
   CALL wrttrl(Ifill1)
   Ifill(1) = iscr1
   CALL rdtrl(Ifill)
   Ifilb(1) = b
   CALL rdtrl(Ifilb)
!
!     IF THE B MATRIX IS PURGED, ASSUME AN IDENTITY MATRIX IN ITS PLACE
!
   IF ( Ifilb(1)<=0 ) CALL makmcb(Ifilb,b,n,Identy,Ja(5))
   Isign1 = Ksign
   ia5 = Ifila(5)
   ib5 = Ifilb(5)
!
!     DETERMINE THE PRECISION FOR THE CALCULATIONS
!     AND THE TYPE OF THE OUTPUT MATRIX
!
 100  Iprec1 = Kprec
   IF ( (ia5>0 .AND. ia5<=4) .OR. (ib5>0 .AND. ib5<=4) ) Iprec1 = 1
   IF ( ia5==2 .OR. ia5==4 .OR. ib5==2 .OR. ib5==4 ) Iprec1 = 2
   IF ( Iprec/=Iprec1 .AND. Iprec/=0 ) THEN
      IF ( Iprec<1 .OR. Iprec>2 ) Iprec = 3
      WRITE (Outpt,99003) Swm , dosi(Iprec) , refus(Iprec) , name , dosi(Iprec1)
99003 FORMAT (A27,' 2163, REQUESTED ',A4,'LE PRECISION ',A3,'USED BY ',2A4,2H. ,A4,'LE PRECISION IS LOGICAL CHOICE')
      IF ( Iprec/=3 ) Iprec1 = Iprec
   ENDIF
   Iprec = Iprec1
   ltype = Iprec1
   IF ( ia5==3 .OR. ia5==4 .OR. ib5==3 .OR. ib5==4 ) ltype = Iprec1 + 2
   IF ( Itype/=0 .AND. Itype/=ltype ) THEN
      jj = 1
      IF ( Itype<1 .OR. Itype>4 ) jj = 3
      WRITE (Outpt,99004) Sfm , Itype , refus(jj) , name , ltype
99004 FORMAT (A27,' 2164, REQUESTED TYPE ',I4,2H, ,A3,'USED BY ',2A4,'. TYPE ',I4,' IS LOGICAL CHOICE.')
      IF ( jj/=3 ) ltype = Itype
   ENDIF
   Itype = ltype
   IF ( index==2 ) THEN
      Ipr = Iprec
!
!     DEFINE THE MATRIX CONTROL BLOCK FOR THE OUTPUT MATRIX
!
      CALL makmcb(Jx,x,n,Rect,Itype)
      Nzzz = korsz(Zzzz)
      IF ( Jb(4)==Identy ) Jb(5) = Iprec
      CALL gfbs(Zzzz,Zzzz)
      IF ( Jx(2)==n ) Jx(4) = Sqr
      CALL wrttrl(Jx)
      RETURN
   ELSE
!
!     DEFINE THE MATRIX CONTROL BLOCK FOR THE OUTPUT MATRIX
!
      CALL makmcb(Ifilx,x,n,Rect,Itype)
      Nx = korsz(Zz)
      IF ( Ifilb(4)==Identy ) Ifilb(5) = Iprec
      Iscr = iscr1
      CALL fbs(Zz,Zz)
      IF ( Ifilx(2)==n ) Ifilx(4) = Sqr
      CALL wrttrl(Ifilx)
      RETURN
   ENDIF
!
 200  no = isign(5,Isym)
   Isym = -1
   CALL mesage(no,a,name)
!
!     SET UP THE CALL TO DECOMP AND GFBS
!
 300  index = 2
   IF ( Ja(5)>2 ) THEN
!
!     SET UP CALL TO CDCOMP AND GFBS
!
      Kl(1) = iscr1
      Ku(1) = iscr2
      Jscr1 = iscr3
      Jscr2 = iscr4
      Jscr3 = iscr5
      Nzzzz = korsz(Zzzzz)
      Ja(4) = Sqr
      n = Ja(2)
      Kl(5) = Ja(5)
      Jbb = 0
      Jbbar = 0
      CALL cdcomp(*200,Zzzzz,Zzzzz,Zzzzz)
      DO i = 1 , 7
         Jl(i) = Kl(i)
         Ju(i) = Ku(i)
      ENDDO
   ELSE
      Ia(1) = a
      Il(1) = iscr1
      Iu(1) = iscr2
      Isr1 = iscr3
      Isr3 = iscr5
      Isr2 = iscr4
      Nzz = korsz(Zzz)
      CALL rdtrl(Ia)
      Ia(4) = Sqr
      n = Ia(2)
      Il(5) = Ja(5)
      Ib = 0
      Ibbar = 0
      CALL decomp(*200,Zzz,Zzz,Zzz)
      DO i = 1 , 7
         Jl(i) = Il(i)
         Ju(i) = Iu(i)
      ENDDO
   ENDIF
   Jb(1) = b
   CALL rdtrl(Jb)
!
!     IF THE B MATRIX IS PURGED, ASSUME AN IDENTITY MATRIX IN ITS PLACE
!
   IF ( Jb(1)<=0 ) CALL makmcb(Jb,b,n,Identy,Ja(5))
   ia5 = Ja(5)
   ib5 = Jb(5)
   Isgn = Ksign
!
!     DETERMINE THE PRECISION FOR THE CALCULATIONS
!     AND THE TYPE OF THE OUTPUT MATRIX
!
   GOTO 100
END SUBROUTINE solve
