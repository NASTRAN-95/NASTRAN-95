!*==smpyad.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE smpyad
   IMPLICIT NONE
   USE c_blank
   USE c_mpyadx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: addmat , intres , mpyads , recmat , resmat
   INTEGER , DIMENSION(3) , SAVE :: dosi , refus
   INTEGER :: i , icol , irow , itype , j , k , kprec , l , m , nm1 , nogo , outpt
   INTEGER , DIMENSION(5) , SAVE :: mat
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(7,5) :: trlr
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Ksystm(55),Kprec) , (Ksystm(2),Outpt)
   DATA mat/101 , 102 , 103 , 104 , 105/
   DATA addmat , resmat , intres , mpyads , recmat/106 , 201 , 301 , 302 , 2/
   DATA dosi/4HSING , 4HDOUB , 4HMLTP/
   DATA refus/2*3H    , 3HREF/
   DATA name/4HSMPY , 4HAD  /
!
   IF ( n>1 ) THEN
      IF ( n>5 ) n = 5
      iprec1 = 1
      itype = 0
!
!     IF ONE OF THE -N- MATRICES IN THE PRODUCT DOES NOT EXIST,
!     SKIP THE ENTIRE CALCULATION.
!
      DO i = 1 , n
         trlr(1,i) = mat(i)
         CALL rdtrl(trlr(1,i))
         IF ( trlr(1,i)<=0 .OR. trlr(2,i)<=0 .OR. trlr(3,i)<=0 ) GOTO 99999
         IF ( trlr(5,i)==2 .OR. trlr(5,i)==4 ) iprec1 = 2
         IF ( trlr(5,i)==3 .OR. trlr(5,i)==4 ) itype = 2
      ENDDO
!
!     CHECK TO SEE IF THE INPUT MATRICES ARE CONFORMABLE
!
      nm1 = n - 1
      nogo = 0
      DO i = 1 , nm1
         icol = trlr(2,i)
         IF ( tmat(i)/=0 ) icol = trlr(3,i)
         irow = trlr(3,i+1)
         IF ( i/=nm1 ) THEN
            IF ( tmat(i+1)/=0 ) irow = trlr(2,i+1)
         ENDIF
         IF ( icol/=irow ) nogo = 1
      ENDDO
      trlrc(1) = addmat
      CALL rdtrl(trlrc)
      IF ( trlrc(1)>0 ) THEN
         irow = trlr(3,1)
         IF ( tmat(1)/=0 ) irow = trlr(2,1)
         icol = trlr(2,n)
         IF ( irow/=trlrc(3) .OR. icol/=trlrc(2) ) nogo = 1
      ENDIF
      IF ( nogo==1 ) CALL mesage(-55,0,name)
!
      IF ( iprec1<1 .OR. iprec1>2 ) iprec1 = kprec
      IF ( iprec/=iprec1 .AND. iprec/=0 ) THEN
         IF ( iprec<1 .OR. iprec>2 ) iprec = 3
         WRITE (outpt,99001) swm , dosi(iprec) , refus(iprec) , name , dosi(iprec1)
99001    FORMAT (A27,' 2163, REQUESTED ',A4,'LE PRECISION ',A3,'USED BY ',2A4,2H. ,A4,'LE PRECISION IS LOGICAL CHOICE')
         IF ( iprec/=3 ) iprec1 = iprec
      ENDIF
      iprec = iprec1
      itype = itype + iprec1
!
!     SETUP THE MPYADX COMMON BLOCK.
!
      IF ( (n+1)/2==n/2 ) THEN
         trlrb(1) = resmat
         m = intres
      ELSE
         trlrb(1) = intres
         m = resmat
      ENDIF
      trlrc(1) = 0
      DO i = 1 , 7
         trlrd(i) = trlr(i,n)
      ENDDO
      trlrd(4) = recmat
      na = korsz(a)
      signab = 1
      signc = sadd
      scrtch = mpyads
!
!     DO THE N-1 MULTIPLICATIONS.
!
      DO k = 2 , n
         j = n - k + 1
         trlra(1) = trlr(1,j)
         IF ( k/=3 ) l = trlrb(1)
         IF ( k==3 ) l = m
         trlrb(1) = trlrd(1)
         trlrd(1) = l
         DO i = 2 , 7
            trlra(i) = trlr(i,j)
            trlrb(i) = trlrd(i)
         ENDDO
         IF ( k/=n ) THEN
            trlrd(5) = iprec1
            IF ( trlra(5)>2 .OR. trlrb(5)>2 ) trlrd(5) = iprec1 + 2
         ELSE
            trlrc(1) = addmat
            CALL rdtrl(trlrc)
            IF ( trlrc(1)<0 ) trlrc(1) = 0
            trlrd(5) = itype
            signab = sreslt
         ENDIF
         trnsp = tmat(j)
         trlrd(3) = trlra(3)
         IF ( trnsp/=0 ) trlrd(3) = trlra(2)
         trlrd(2) = trlrb(2)
         CALL mpyad(a,a,a)
      ENDDO
      IF ( trlrd(2)==trlrd(3) ) trlrd(4) = 1
      CALL wrttrl(trlrd)
   ENDIF
99999 END SUBROUTINE smpyad
