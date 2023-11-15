
SUBROUTINE ifp1s(List,Istor,Nlist)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Inx(6) , Inx1(2) , Line , Nlpp , Otpe
   REAL Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /system/ Sysbuf , Otpe , Inx , Nlpp , Inx1 , Line
   COMMON /xmssg / Ufm , Uwm
!
! Dummy argument declarations
!
   INTEGER Nlist
   INTEGER Istor(1) , List(1)
!
! Local variable declarations
!
   INTEGER i , in , in1 , iout , ipair , ising , ix , k , l , m , n1 , n2
!
! End of declarations
!
!
!     THIS ROUTINE FINDS ANY OVERLAPPING INTERVALS IN A SET LIST.
!     IT WILL ALSO CHECK SINGLES
!
!
   ipair = 0
   DO i = 1 , Nlist
      IF ( List(i)>0 ) CYCLE
 50   IF ( ipair/=0 ) THEN
!
!     PAIR FOUND - CHECK FOR OVERLAP
!
         l = 1
         in = iabs(List(i-1))
         iout = iabs(List(i))
         k = 2*l - 1
         DO WHILE ( in<Istor(k) .OR. in>iabs(Istor(k+1)) )
            IF ( iout>=Istor(k) .AND. iout<=iabs(Istor(k+1)) ) EXIT
            l = l + 1
!
!     STORE NEW PAIR IN LIST
!
            IF ( l>ipair ) GOTO 100
         ENDDO
         GOTO 200
      ENDIF
 100  l = 2*ipair + 1
      Istor(l) = List(i-1)
      Istor(l+1) = List(i)
      ipair = ipair + 1
 150  List(i) = 0
      List(i-1) = 0
      CYCLE
!
!     ERROR IN INTERVAL
!
 200  List(i-1) = min0(in,Istor(k))
      List(i) = -max0(iout,iabs(Istor(k+1)))
      IF ( List(i-1)==Istor(k) .AND. List(i)==Istor(k+1) ) GOTO 150
      ix = iabs(Istor(k+1))
      WRITE (Otpe,99001) Uwm , in , iout , Istor(k) , ix
99001 FORMAT (A25,' 621, INTERVAL',I8,' THRU',I8,' OVERLAPS INTERVAL',I8,' THRU',I8,'. THE MAXIMUM INTERVAL WILL BE USED.')
      Line = Line + 3
      IF ( Line>=Nlpp ) CALL page
!
!     REMOVE PAIR L FROM ISTOR
!
      DO WHILE ( l<ipair )
         m = 2*l + 1
         k = 2*l - 1
         Istor(k) = Istor(m)
         Istor(k+1) = Istor(m+1)
         l = l + 1
      ENDDO
      ipair = ipair - 1
      GOTO 50
   ENDDO
!
!     ALL PAIRS PROCESSED - TRY SINGLES
!
   ising = 0
   m = 2*ipair
   DO i = 1 , Nlist
      in = List(i)
      IF ( ipair/=0 ) THEN
         IF ( List(i)==0 ) CYCLE
!
!     CHECK EACH PAIR
!
         l = 1
         DO
            k = 2*l - 1
            IF ( in>=Istor(k) .AND. in<=iabs(Istor(k+1)) ) THEN
!
!     ERROR -- PAIR CONTAINS SINGLE
!
               in1 = iabs(Istor(k+1))
               WRITE (Otpe,99002) Uwm , in , Istor(k) , in1
99002          FORMAT (A25,' 619, SET MEMBER',I8,' BELONGS TO',I8,' THRU',I8)
               Line = Line + 3
               IF ( Line>=Nlpp ) CALL page
               GOTO 300
            ELSE
               l = l + 1
               IF ( l>ipair ) EXIT
            ENDIF
         ENDDO
      ENDIF
!
!     CHECK FOR DUPLICATE SINGLES
!
      IF ( ising/=0 ) THEN
         DO k = 1 , ising
            l = 2*ipair + k
            IF ( in==Istor(l) ) THEN
               WRITE (Otpe,99003) Uwm , in
99003          FORMAT (A25,' 620, SET MEMBER',I8,' IS DUPLICATED IN SET LIST.')
               Line = Line + 3
               IF ( Line>=Nlpp ) CALL page
               GOTO 300
            ENDIF
         ENDDO
      ENDIF
      m = m + 1
      ising = ising + 1
      Istor(m) = in
 300  ENDDO
!
!     COPY GOOD STUFF INTO LIST
!
   DO i = 1 , m
      List(i) = Istor(i)
   ENDDO
   Nlist = m
!
!     SORT LIST
!
   n1 = m - 1
   DO i = 1 , n1
      n2 = i + 1
      DO k = n2 , m
         IF ( iabs(List(i))>iabs(List(k)) ) THEN
!
!     SWITCH
!
            in = List(i)
            List(i) = List(k)
            List(k) = in
         ENDIF
      ENDDO
   ENDDO
!
END SUBROUTINE ifp1s
