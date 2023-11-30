
SUBROUTINE combin(Pg,Ilist,Nlist)
   IMPLICIT NONE
   REAL A(4) , Ary(1) , Core(1) , Dum52(52)
   INTEGER Iary(1) , Ieol , Ieor , Ii , Incur , Iprec , Iptr , Ita , Itb , Jj , Lc , Ll , Lodc , Mass , N(13) , Nload , Nrowsp ,    &
         & Otpe , Sysbuf
   CHARACTER*23 Ufm
   COMMON /blank / Nrowsp
   COMMON /loads / Nload , Iptr
   COMMON /loadx / Lc , N , Lodc , Mass
   COMMON /packx / Ita , Itb , Ii , Jj , Incur
   COMMON /system/ Sysbuf , Otpe , Dum52 , Iprec
   COMMON /xmssg / Ufm
   COMMON /zntpkx/ A , Ll , Ieol , Ieor
   COMMON /zzzzzz/ Core
   INTEGER Nlist , Pg
   INTEGER Ilist(1)
   REAL alpha(360) , alpha1(360) , head(2)
   INTEGER hccen , hccens , hcfld , hcflds , i , ibuf1 , inull , ip1 , j , k , kk , kl , lcore , loadn(360) , loadnn(360) , lodc1(7)&
         & , mcb(7) , name(2) , nhc , nl1 , nlj , nperms , ns , remfl , remfls
!
   !>>>>EQUIVALENCE (Core(1),Iary(1),Ary(1))
!
!     ALSO COMBINE HCFLD AND REMFL IN MAGNETOSTATIC PROBLEMS
!
   DATA hcflds , hcfld/304 , 202/
   DATA remfls , remfl/305 , 203/
   DATA hccens , hccen/307 , 204/
   DATA name/4HCOMB , 4HIN  /
!
!
   Ita = 1
   Itb = Iprec
   Ii = 1
!
!     PERFORM CHECKS IN E AND M PROBLEM
!     IN E AND M PROBLEM, REMFLS AND HCFLDS MUST HAVE THE SAME NUMBER
!     OF COLUMNS AS PG
!
   mcb(1) = remfls
   CALL rdtrl(mcb)
   nperms = 0
   IF ( mcb(1)>0 ) nperms = mcb(2)
   mcb(1) = hcflds
   CALL rdtrl(mcb)
   nhc = 0
   IF ( mcb(1)>0 ) nhc = mcb(2)
   IF ( nhc/=nperms ) GOTO 400
   IF ( nhc/=0 ) THEN
      mcb(1) = Pg
      CALL rdtrl(mcb)
      IF ( nhc/=mcb(2) ) GOTO 400
   ENDIF
   DO
      mcb(1) = hccens
      CALL rdtrl(mcb)
      ns = 0
      IF ( mcb(1)>0 ) ns = mcb(2)
      IF ( ns/=nhc ) GOTO 400
      Jj = Nrowsp
      Incur = 1
      lcore = Lc
      ibuf1 = lcore
      lcore = lcore - Sysbuf
      CALL open(*300,Lodc,Core(lcore+1),1)
      CALL fname(Lodc,head)
      CALL write(Lodc,head,2,1)
      lcore = lcore - Sysbuf
      CALL open(*100,Pg,Core(lcore+1),0)
      CALL makmcb(lodc1,Lodc,Nrowsp,2,Iprec)
      nlj = Iptr
      nl1 = 0
      DO i = 1 , Nload
         DO j = 1 , Nrowsp
            Core(j) = 0.0
         ENDDO
         nlj = nlj + nl1*2 + 1
         nl1 = Iary(nlj)
         DO k = 1 , nl1
            kk = nlj + (k-1)*2 + 1
            loadn(k) = Iary(kk)
            IF ( loadn(k)<0 ) GOTO 60
            alpha(k) = Ary(kk+1)
         ENDDO
         kk = 1
         kl = 0
         DO k = 1 , Nlist
            IF ( Ilist(k)/=0 ) THEN
               kl = kl + 1
               DO j = 1 , nl1
                  IF ( loadn(j)==Ilist(k) ) GOTO 10
               ENDDO
            ENDIF
            CYCLE
 10         loadnn(kk) = kl
            alpha1(kk) = alpha(j)
            kk = kk + 1
         ENDDO
         kk = 1
         DO j = 1 , nl1
            inull = 0
            IF ( j==1 ) CALL skprec(Pg,1)
            CALL intpk(*30,Pg,0,1,0)
 20         DO WHILE ( loadnn(j)/=kk )
               IF ( inull/=1 ) THEN
                  IF ( Ieor==0 ) CALL skprec(Pg,1)
               ENDIF
               kk = kk + 1
               inull = 0
               CALL intpk(*30,Pg,0,1,0)
            ENDDO
            DO WHILE ( inull/=1 )
               IF ( Ieol/=0 ) EXIT
               CALL zntpki
               Core(Ll) = Core(Ll) + A(1)*alpha1(j)
            ENDDO
            GOTO 40
 30         inull = 1
            GOTO 20
 40         kk = kk + 1
         ENDDO
 60      CALL pack(Core,lodc1(1),lodc1)
         CALL rewind(Pg)
      ENDDO
      CALL wrttrl(lodc1(1))
      CALL close(lodc1(1),1)
      CALL close(Pg,1)
      IF ( Pg==hcflds ) THEN
!
!     DO REMFLS
!
         lodc1(1) = remfls
         CALL rdtrl(lodc1)
         IF ( lodc1(2)<=0 ) RETURN
         Pg = remfls
         Lodc = remfl
         Nrowsp = lodc1(3)
      ELSEIF ( Pg==remfls ) THEN
!
!     HCCENS
!
         lodc1(1) = hccens
         CALL rdtrl(lodc1)
         IF ( lodc1(2)<=0 ) RETURN
         Pg = hccens
         Lodc = hccen
         Nrowsp = lodc1(3)
      ELSE
         IF ( Pg==hccens ) RETURN
!
!     DO MAGNETOSTATIC FIELDS FOR USE IN EMFLD
!
         lodc1(1) = hcflds
         CALL rdtrl(lodc1)
!
!     IF HCFLD IS PURGED, SO MUST REMFLS
!
         IF ( lodc1(2)<=0 ) RETURN
         Pg = hcflds
         Lodc = hcfld
         Nrowsp = 3*Nrowsp
      ENDIF
   ENDDO
 100  ip1 = Pg
 200  CALL mesage(-1,ip1,name)
 300  IF ( Lodc==hcfld ) RETURN
   ip1 = Lodc
   GOTO 200
 400  WRITE (Otpe,99001) Ufm
99001 FORMAT (A23,', IN AN E AND M PROBLEM, SCRATCH DATA BLOCKS HCFLDS',' AND REMFLS HAVE DIFFERENT NUMBERS OF COLUMNS.',/10X,      &
             &' THIS MAY RESULT FROM SPCFLD AND REMFLU CARDS HAVING THE',' SAME LOAD SET ID')
   CALL mesage(-61,0,0)
END SUBROUTINE combin