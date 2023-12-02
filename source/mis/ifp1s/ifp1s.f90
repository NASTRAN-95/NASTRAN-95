!*==ifp1s.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifp1s(List,Istor,Nlist)
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: List
   INTEGER , DIMENSION(1) :: Istor
   INTEGER :: Nlist
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , in , in1 , iout , ipair , ising , ix , k , l , m , n1 , n2
   EXTERNAL page
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE FINDS ANY OVERLAPPING INTERVALS IN A SET LIST.
!     IT WILL ALSO CHECK SINGLES
!
!
   ipair = 0
   DO i = 1 , Nlist
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            IF ( List(i)>0 ) CYCLE
            SPAG_Loop_2_2: DO WHILE ( ipair/=0 )
!
!     PAIR FOUND - CHECK FOR OVERLAP
!
               l = 1
               in = iabs(List(i-1))
               iout = iabs(List(i))
               k = 2*l - 1
               SPAG_Loop_3_1: DO WHILE ( in<Istor(k) .OR. in>iabs(Istor(k+1)) )
                  IF ( iout>=Istor(k) .AND. iout<=iabs(Istor(k+1)) ) EXIT SPAG_Loop_3_1
                  l = l + 1
!
!     STORE NEW PAIR IN LIST
!
                  IF ( l>ipair ) EXIT SPAG_Loop_2_2
               ENDDO SPAG_Loop_3_1
!
!     ERROR IN INTERVAL
!
               List(i-1) = min0(in,Istor(k))
               List(i) = -max0(iout,iabs(Istor(k+1)))
               IF ( List(i-1)==Istor(k) .AND. List(i)==Istor(k+1) ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ix = iabs(Istor(k+1))
               WRITE (otpe,99001) uwm , in , iout , Istor(k) , ix
99001          FORMAT (A25,' 621, INTERVAL',I8,' THRU',I8,' OVERLAPS INTERVAL',I8,' THRU',I8,'. THE MAXIMUM INTERVAL WILL BE USED.')
               line = line + 3
               IF ( line>=nlpp ) CALL page
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
            ENDDO SPAG_Loop_2_2
            l = 2*ipair + 1
            Istor(l) = List(i-1)
            Istor(l+1) = List(i)
            ipair = ipair + 1
            spag_nextblock_1 = 2
         CASE (2)
            List(i) = 0
            List(i-1) = 0
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
!
!     ALL PAIRS PROCESSED - TRY SINGLES
!
   ising = 0
   m = 2*ipair
   SPAG_Loop_1_3: DO i = 1 , Nlist
      in = List(i)
      IF ( ipair/=0 ) THEN
         IF ( List(i)==0 ) CYCLE
!
!     CHECK EACH PAIR
!
         l = 1
         SPAG_Loop_2_4: DO
            k = 2*l - 1
            IF ( in>=Istor(k) .AND. in<=iabs(Istor(k+1)) ) THEN
!
!     ERROR -- PAIR CONTAINS SINGLE
!
               in1 = iabs(Istor(k+1))
               WRITE (otpe,99002) uwm , in , Istor(k) , in1
99002          FORMAT (A25,' 619, SET MEMBER',I8,' BELONGS TO',I8,' THRU',I8)
               line = line + 3
               IF ( line>=nlpp ) CALL page
               CYCLE SPAG_Loop_1_3
            ELSE
               l = l + 1
               IF ( l>ipair ) EXIT SPAG_Loop_2_4
            ENDIF
         ENDDO SPAG_Loop_2_4
      ENDIF
!
!     CHECK FOR DUPLICATE SINGLES
!
      IF ( ising/=0 ) THEN
         DO k = 1 , ising
            l = 2*ipair + k
            IF ( in==Istor(l) ) THEN
               WRITE (otpe,99003) uwm , in
99003          FORMAT (A25,' 620, SET MEMBER',I8,' IS DUPLICATED IN SET LIST.')
               line = line + 3
               IF ( line>=nlpp ) CALL page
               CYCLE SPAG_Loop_1_3
            ENDIF
         ENDDO
      ENDIF
      m = m + 1
      ising = ising + 1
      Istor(m) = in
   ENDDO SPAG_Loop_1_3
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
