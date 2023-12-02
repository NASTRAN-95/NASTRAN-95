!*==valvec.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE valvec
   IMPLICIT NONE
   USE C_GIVN
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i1 , i2 , i3 , i4 , i5 , i6 , id , il , io , iterm , iv , md , n , n2 , never , nv , nver , qr
   INTEGER , SAVE :: ibegn , iend
   INTEGER , DIMENSION(8) :: mcb
   REAL :: oeigs , rstrt
   INTEGER , DIMENSION(3) , SAVE :: qrx , tri , val , wil
   REAL , DIMENSION(30) :: vcom
!
! End of declarations rewritten by SPAG
!
!
!     LARGE ORDER REAL SYMMETRIC EIGENVALUE-EIGENVECTOR PROBLEM
!
!WKBR 2/94 SPR93027 COMMON /SYSTEM/ ISYS
   !>>>>EQUIVALENCE (Md,Title(3)) , (Vcom(1),Title(101)) , (N,Vcom(1)) , (Nv,Vcom(7)) , (Oeigs,Vcom(11)) , (Nver,Vcom(13)) ,             &
!>>>>    & (Never,Vcom(14)) , (Iterm,Vcom(16))
   DATA tri/4HTRID , 4HI    , 4H    /
   DATA qrx/4HQRIT , 4HER   , 4H    /
   DATA wil/4HWILV , 4HEC   , 4H    /
   DATA val/4HVALV , 4HEC   , 4H    /
   DATA ibegn , iend/4HBEGN , 4HEND /
!
!     DEFINITION OF VARIABLES AND DATA FORMATS
!
!     MD       INPUT MATRIX
!     N        SIZE OF MATRIX
!     NV       NUMBER OF EIGENVECTORS DESIRED
!     OEIGS    EIGENVALUE SUMMARY FILE
!     A        OPEN CORE
!     ID       POINTER TO DIAGONALS      -- N OF THEM (D.P.)
!     IO       POINTER TO OFF-DIAGONALS  -- N OF THEM (D.P.)
!     IV       POINTER TO EIGENVALUES    -- N OF THEM (D.P.)
!     IL       POINTER TO ORDER FOUND ARRAY N OF THEM (S.P.)
!     I1 - I6  POINTS TO SCRATCH ARRAYS  -- 2XN LONG
!     NVER     NUMBER OF VECTORS    ERRORS
!     NEVER    NUMBER OF EIGENVALUE ERRORS
!     ITERM    REASON FOR TERMINATION
!
!     INITIALIZATION FOR VALVEC IN BLOCKDATA ROUTINE READBD
!
!     DATA
!    1 MO, MD,MR1, M1, M2, M3, M4,LGAMA,OEIGS,PHIA,ORDER,RSTRT,NCOL,MAX/
!    *301,304,202,303,307,308,309,  201,  204, 305,   -2,   0 ,   0,253/
!
!
   val(3) = ibegn
   CALL conmsg(val,3,0)
   iterm = 1
   mcb(1) = md
   CALL rdtrl(mcb(1))
   n = mcb(2)
   n2 = n*Iprec
   id = 1
   io = id + n2
   iv = io + n2
   il = iv + n2
   i1 = il + n
   IF ( (i1+1)/2==i1/2 ) i1 = i1 + 1
   i2 = i1 + n2
   i3 = i2 + n2
   i4 = i3 + n2
   i5 = i4 + n2
   i6 = i5 + n2
!
!     TRIDIAGONALIZATION.
!
   IF ( n>2 ) THEN
      tri(3) = ibegn
      CALL conmsg(tri,3,0)
!WKBD 2/94 SPR93027 CALL TRIDI (A(ID),A(IO),A(IV),A(IL),A(I1),A(IL))
!WKBNB 2/94 SPR93027
      IF ( Iprec==2 ) CALL tridi(A(id),A(io),A(iv),A(il),A(i1),A(il))
!                   D      O    V     A     B
      IF ( Iprec==1 ) CALL tridi1(A(id),A(io),A(iv),A(il),A(i1),A(il))
!                   D      O    V     A     B
!WKBNE 2/94 SPR93027
      tri(3) = iend
      CALL conmsg(tri,3,0)
   ELSE
!WKBD 2/94 SPR93027 CALL SMLEIG (A(ID),A(IO),A(IV))
!WKBNB 2/94 SPR93027
      IF ( Iprec==2 ) CALL smleig(A(id),A(io),A(iv))
      IF ( Iprec==1 ) CALL smleig1(A(id),A(io),A(iv))
!WKBNE 2/94 SPR93027
 
      IF ( n/=2 ) GOTO 100
   ENDIF
!
!     EIGENVALUES
!
   qr = 0
   IF ( n<=2 ) qr = 1
   qrx(3) = ibegn
   CALL conmsg(qrx,3,0)
!WKBD 2/94 SPR93027 CALL QRITER (A(IV),A(I1),A(IL),QR)
!WKBNB 2/94 SPR93027
   IF ( Iprec==2 ) CALL qriter(A(iv),A(i1),A(il),qr)
   IF ( Iprec==1 ) CALL qriter1(A(iv),A(i1),A(il),qr)
!WKBNE 2/94 SPR93027
 
   qrx(3) = iend
   CALL conmsg(qrx,3,0)
   rstrt = 0
   wil(3) = ibegn
   CALL conmsg(wil,3,0)
!
!     EIGENVECTORS
!
!WKBDB 2/94 SPR93027
!     CALL WILVEC (A(ID),A(IO),A(IV),A(IL),A(I1),A(I2),A(I3),A(I4),
!     1             A(I5),A(I6),N,A(I6))
!WKBDE 2/94 SPR93027
!WKBNB 2/94 SPR93027
!                    D      0    C    A      B
   IF ( Iprec==1 ) CALL wilvec1(A(id),A(io),A(iv),A(il),A(i1),A(i2),A(i3),A(i4),A(i5),A(i6),n,A(i6))
!                    D      0    C    A      B
   IF ( Iprec==2 ) CALL wilvec(A(id),A(io),A(iv),A(il),A(i1),A(i2),A(i3),A(i4),A(i5),A(i6),n,A(i6))
!WKBNE 2/94 SPR93027
   wil(3) = iend
   CALL conmsg(wil,3,0)
 100  CALL gopen(oeigs,A(1),1)
   mcb(1) = 4
   mcb(2) = n
   mcb(3) = nv
   mcb(4) = never
   mcb(5) = nver
   mcb(8) = iterm
   CALL write(oeigs,mcb,8,1)
   CALL close(oeigs,1)
   val(3) = iend
   CALL conmsg(val,3,0)
END SUBROUTINE valvec
