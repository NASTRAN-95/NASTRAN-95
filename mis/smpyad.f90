
SUBROUTINE smpyad
   IMPLICIT NONE
   REAL A(1)
   INTEGER Iprec , Iprec1 , Kprec , Ksystm(65) , N , Na , Outpt , Sadd , Scrtch , Signab , Signc , Sreslt , Tmat(4) , Trlra(7) ,    &
         & Trlrb(7) , Trlrc(7) , Trlrd(7) , Trnsp
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / N , Sreslt , Sadd , Iprec , Tmat
   COMMON /mpyadx/ Trlra , Trlrb , Trlrc , Trlrd , Na , Trnsp , Signab , Signc , Iprec1 , Scrtch
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ A
   INTEGER addmat , dosi(3) , i , icol , intres , irow , itype , j , k , l , m , mat(5) , mpyads , name(2) , nm1 , nogo , recmat ,  &
         & refus(3) , resmat , trlr(7,5)
   INTEGER korsz
!
   EQUIVALENCE (Ksystm(55),Kprec) , (Ksystm(2),Outpt)
   DATA mat/101 , 102 , 103 , 104 , 105/
   DATA addmat , resmat , intres , mpyads , recmat/106 , 201 , 301 , 302 , 2/
   DATA dosi/4HSING , 4HDOUB , 4HMLTP/
   DATA refus/2*3H    , 3HREF/
   DATA name/4HSMPY , 4HAD  /
!
   IF ( N>1 ) THEN
      IF ( N>5 ) N = 5
      Iprec1 = 1
      itype = 0
!
!     IF ONE OF THE -N- MATRICES IN THE PRODUCT DOES NOT EXIST,
!     SKIP THE ENTIRE CALCULATION.
!
      DO i = 1 , N
         trlr(1,i) = mat(i)
         CALL rdtrl(trlr(1,i))
         IF ( trlr(1,i)<=0 .OR. trlr(2,i)<=0 .OR. trlr(3,i)<=0 ) GOTO 99999
         IF ( trlr(5,i)==2 .OR. trlr(5,i)==4 ) Iprec1 = 2
         IF ( trlr(5,i)==3 .OR. trlr(5,i)==4 ) itype = 2
      ENDDO
!
!     CHECK TO SEE IF THE INPUT MATRICES ARE CONFORMABLE
!
      nm1 = N - 1
      nogo = 0
      DO i = 1 , nm1
         icol = trlr(2,i)
         IF ( Tmat(i)/=0 ) icol = trlr(3,i)
         irow = trlr(3,i+1)
         IF ( i/=nm1 ) THEN
            IF ( Tmat(i+1)/=0 ) irow = trlr(2,i+1)
         ENDIF
         IF ( icol/=irow ) nogo = 1
      ENDDO
      Trlrc(1) = addmat
      CALL rdtrl(Trlrc)
      IF ( Trlrc(1)>0 ) THEN
         irow = trlr(3,1)
         IF ( Tmat(1)/=0 ) irow = trlr(2,1)
         icol = trlr(2,N)
         IF ( irow/=Trlrc(3) .OR. icol/=Trlrc(2) ) nogo = 1
      ENDIF
      IF ( nogo==1 ) CALL mesage(-55,0,name)
!
      IF ( Iprec1<1 .OR. Iprec1>2 ) Iprec1 = Kprec
      IF ( Iprec/=Iprec1 .AND. Iprec/=0 ) THEN
         IF ( Iprec<1 .OR. Iprec>2 ) Iprec = 3
         WRITE (Outpt,99001) Swm , dosi(Iprec) , refus(Iprec) , name , dosi(Iprec1)
99001    FORMAT (A27,' 2163, REQUESTED ',A4,'LE PRECISION ',A3,'USED BY ',2A4,2H. ,A4,'LE PRECISION IS LOGICAL CHOICE')
         IF ( Iprec/=3 ) Iprec1 = Iprec
      ENDIF
      Iprec = Iprec1
      itype = itype + Iprec1
!
!     SETUP THE MPYADX COMMON BLOCK.
!
      IF ( (N+1)/2==N/2 ) THEN
         Trlrb(1) = resmat
         m = intres
      ELSE
         Trlrb(1) = intres
         m = resmat
      ENDIF
      Trlrc(1) = 0
      DO i = 1 , 7
         Trlrd(i) = trlr(i,N)
      ENDDO
      Trlrd(4) = recmat
      Na = korsz(A)
      Signab = 1
      Signc = Sadd
      Scrtch = mpyads
!
!     DO THE N-1 MULTIPLICATIONS.
!
      DO k = 2 , N
         j = N - k + 1
         Trlra(1) = trlr(1,j)
         IF ( k/=3 ) l = Trlrb(1)
         IF ( k==3 ) l = m
         Trlrb(1) = Trlrd(1)
         Trlrd(1) = l
         DO i = 2 , 7
            Trlra(i) = trlr(i,j)
            Trlrb(i) = Trlrd(i)
         ENDDO
         IF ( k/=N ) THEN
            Trlrd(5) = Iprec1
            IF ( Trlra(5)>2 .OR. Trlrb(5)>2 ) Trlrd(5) = Iprec1 + 2
         ELSE
            Trlrc(1) = addmat
            CALL rdtrl(Trlrc)
            IF ( Trlrc(1)<0 ) Trlrc(1) = 0
            Trlrd(5) = itype
            Signab = Sreslt
         ENDIF
         Trnsp = Tmat(j)
         Trlrd(3) = Trlra(3)
         IF ( Trnsp/=0 ) Trlrd(3) = Trlra(2)
         Trlrd(2) = Trlrb(2)
         CALL mpyad(A,A,A)
      ENDDO
      IF ( Trlrd(2)==Trlrd(3) ) Trlrd(4) = 1
      CALL wrttrl(Trlrd)
   ENDIF
99999 RETURN
END SUBROUTINE smpyad
