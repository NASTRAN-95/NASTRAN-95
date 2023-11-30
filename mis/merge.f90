
SUBROUTINE merge(Irp,Icp,Core)
!
!     MERGE WILL PUT UP TO 4 MATRICES, IA11,IA21,IA12,IA22, TOGETHER
!     INTO NAMEA -- THIS ROUTINE IS THE INVERSE OF PARTN
!
!     THE ARGUMENTS ARE EXACTLY THE SAME IN MEANING AND OPTION AS FOR
!     PARTITION
!
   IMPLICIT NONE
   INTEGER Ia(2) , Ia11(7,4) , Ic11(4) , Iforma , Ii , Itypa , Lcare , Namea , Ncola , Nout , Nrowa , Rule , Sysbuf , Two1(32)
   COMMON /parmeg/ Namea , Ncola , Nrowa , Iforma , Itypa , Ia , Ia11 , Lcare , Rule
   COMMON /system/ Sysbuf , Nout
   COMMON /two   / Two1
   COMMON /zblpkx/ Ic11 , Ii
   INTEGER Core(1) , Icp(1) , Irp(1)
   INTEGER a11(4) , b11(4) , block(40) , i , iaz , ibuf , ibufcp , ibufrp , ibz , ieol , io , iotp , ipos , irew , istor , itemp ,  &
         & j , jeol , jpos , k , km , l , l1 , l2 , l3 , lcore , loop , m , mn , nam1 , nam2 , name(2) , ncola1 , nm , nmat ,       &
         & ntypa , ocpct , orpct , zcpct , zrpct
   INTEGER andf , rshift
   EXTERNAL andf , rshift
   DATA name/4HMERG , 4HE   /
!
!     CHECK FILES
!
   lcore = iabs(Lcare)
   k = Namea
   DO i = 1 , 4
      IF ( k/=0 ) THEN
         DO j = i , 4
            IF ( Ia11(1,j)==k ) GOTO 200
         ENDDO
      ENDIF
      k = Ia11(1,i)
   ENDDO
!
!     PICK UP PARAMETERS AND INITIALIZE
!
   irew = 0
   IF ( Lcare<0 ) irew = 2
   ncola1 = Ncola
   Ncola = 0
   Ia(1) = 0
   Ia(2) = 0
   istor = 0
   iotp = Itypa
   nmat = 0
   DO i = 1 , 4
      IF ( Ia11(1,i)>0 ) THEN
!WKBD 2/94 SPR93025 IF (IA11(5,I) .NE. ITYPA) IOTP = 4
         nmat = nmat + 1
         DO j = 2 , 5
            IF ( Ia11(j,i)==0 ) GOTO 300
         ENDDO
      ENDIF
   ENDDO
   ntypa = iotp
   IF ( ntypa==3 ) ntypa = 2
   ibuf = lcore - Sysbuf + 1
   ibufcp = ibuf - Nrowa
   IF ( ibufcp<=0 ) THEN
!
      mn = -8
      GOTO 400
   ELSE
      lcore = ibufcp - 1
      CALL ruler(Rule,Icp,zcpct,ocpct,Core(ibufcp),Nrowa,Core(ibuf),1)
      IF ( Irp(1)==Icp(1) .AND. Irp(1)/=0 ) THEN
         istor = 1
      ELSE
         ibufrp = ibufcp - (ncola1+31)/32
         IF ( ibufrp<=0 ) THEN
            mn = -8
            GOTO 400
         ELSE
            CALL ruler(Rule,Irp,zrpct,orpct,Core(ibufrp),ncola1,Core(ibuf),0)
            lcore = ibufrp - 1
         ENDIF
      ENDIF
!
!     OPEN INPUT FILES
!
      IF ( lcore<nmat*Sysbuf ) THEN
         mn = -8
         GOTO 400
      ELSE
         DO i = 1 , 4
            IF ( Ia11(1,i)<0 ) GOTO 10
            IF ( Ia11(1,i)/=0 ) THEN
               lcore = lcore - Sysbuf
               CALL open(*10,Ia11(1,i),Core(lcore+1),irew)
               CALL skprec(Ia11(1,i),1)
            ENDIF
            CYCLE
 10         Ia11(1,i) = 0
         ENDDO
!
!     OPEN OUTPUT FILE
!
         CALL gopen(Namea,Core(ibuf),1)
!
!     FIX POINTERS -- SORT ON ABS VALUE
!
         k = ibufcp - 1
         l = ibufcp
         DO i = 1 , Nrowa
            k = k + 1
            IF ( Core(k)<0 ) THEN
               Core(l) = i
               l = l + 1
            ENDIF
         ENDDO
         m = l - 1
         k = ibufcp
         DO i = 1 , Nrowa
            DO
               IF ( Core(k)<i ) THEN
                  IF ( k==m ) EXIT
                  k = k + 1
               ELSEIF ( Core(k)==i ) THEN
                  GOTO 20
               ELSE
                  EXIT
               ENDIF
            ENDDO
            Core(l) = i
            l = l + 1
 20      ENDDO
!
!     LOOP ON COLUMNS OF OUTPUT
!
         km = 0
         l2 = ibufcp
         l3 = ibufcp + zcpct
         DO loop = 1 , ncola1
            CALL bldpk(iotp,Itypa,Namea,0,0)
            IF ( istor/=1 ) THEN
               j = (loop-1)/32 + ibufrp
               km = km + 1
               IF ( km>32 ) km = 1
               itemp = andf(Core(j),Two1(km))
               IF ( km==1 ) itemp = rshift(andf(Core(j),Two1(km)),1)
               IF ( itemp/=0 ) GOTO 30
!
!     USE ROW STORE
!
            ELSEIF ( Core(l2)/=loop ) THEN
               IF ( Core(l3)/=loop ) GOTO 300
               GOTO 30
            ENDIF
!
!     IA11 AND IA21 BEING USED
!
            l1 = 0
            IF ( l2/=l3-1 ) l2 = l2 + 1
            GOTO 40
!
!     IA12 AND IA22 BEING USED
!
 30         l1 = 2
            l3 = l3 + 1
!
!     BEGIN ON SUBMATRICES
!
 40         io = 0
            DO j = 1 , 2
               k = l1 + j
               IF ( Ia11(1,k)/=0 ) THEN
                  m = 20*j - 19
                  CALL intpk(*50,Ia11(1,k),block(m),iotp,1)
                  io = io + j
               ENDIF
 50         ENDDO
            IF ( io==0 ) GOTO 110
!
!     PICK UP NON ZERO
!
            ieol = 0
            jeol = 0
            ipos = 9999999
            jpos = 9999999
            iaz = 1
            ibz = 1
            nam1 = Ia11(1,l1+1)
            nam2 = Ia11(1,l1+2)
            IF ( io/=2 ) THEN
               iaz = 0
            ELSE
               ibz = 0
               GOTO 70
            ENDIF
 60         IF ( ieol/=0 ) THEN
               iaz = 1
               ipos = 9999999
               IF ( iaz+ibz==2 ) GOTO 110
               GOTO 90
            ELSE
               CALL intpki(a11(1),i,nam1,block(1),ieol)
               k = ibufcp + i - 1
               ipos = Core(k)
               IF ( io==1 ) GOTO 80
               io = 1
               ibz = 0
            ENDIF
 70         IF ( jeol/=0 ) THEN
               jpos = 9999999
               ibz = 1
               IF ( iaz+ibz/=2 ) GOTO 100
               GOTO 110
            ELSE
               CALL intpki(b11(1),j,nam2,block(21),jeol)
               k = ibufcp + zcpct + j - 1
               jpos = Core(k)
            ENDIF
 80         IF ( ipos<jpos ) GOTO 100
!
!     PUT IN B11
!
 90         DO m = 1 , ntypa
               Ic11(m) = b11(m)
            ENDDO
            Ii = jpos
            CALL zblpki
            GOTO 70
 100        DO m = 1 , ntypa
               Ic11(m) = a11(m)
            ENDDO
            Ii = ipos
            CALL zblpki
            GOTO 60
!
!     OUTPUT COLUMN
!
 110        CALL bldpkn(Namea,0,Namea)
!
         ENDDO
!
!     DONE -- CLOSE OPEN MATRICES
!
         DO i = 1 , 4
            IF ( Ia11(1,i)>0 ) CALL close(Ia11(1,i),1)
         ENDDO
         CALL close(Namea,1)
         GOTO 99999
      ENDIF
   ENDIF
 200  WRITE (Nout,99001) k
99001 FORMAT ('0*** SYSTEM OR USER ERROR, DUPLICATE GINO FILES AS ','DETECTED BY MERGE ROUTINE - ',I5)
   nm = -37
   GOTO 400
 300  mn = -7
 400  CALL mesage(mn,0,name)
!
99999 RETURN
END SUBROUTINE merge