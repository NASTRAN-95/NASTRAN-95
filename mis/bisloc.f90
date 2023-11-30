
SUBROUTINE bisloc(Id,Arr,Len,Kn,Jloc) !HIDESTARS (*,Id,Arr,Len,Kn,Jloc)
   IMPLICIT NONE
   INTEGER Id , Jloc , Kn , Len
   INTEGER Arr(1)
   INTEGER iswtch , j , jj , k , khi , klo
!-----
!     BINARY SEARCH - LOCATE KEY WORD 'ID' IN ARRAY 'ARR', 1ST ENTRY
!     IF FOUND, 'JLOC' IS THE MATCHED POSITION IN 'ARR'
!     IF NOT FOUND, NON-STANDARD RETURN
!                                                     I.E.
!     ID  = KEY WORD TO MATCH IN ARR.      MATCH AGAINST 1ST COL OF ARR
!     ARR = ARRAY TO SEARCH.                          ARR(ROW,COL)
!     LEN = LENGTH OF EACH ENTRY IN ARRAY.            LEN=ROW
!     KN  = NUMBER OF ENTRIES IN THE ARR.             KN =COL
!     JLOC= POINTER RETURNED - FIRST WORD OF ENTRY.   MATCHED ROW
!-----
!
   DATA iswtch/16/
!
   jj = Len - 1
   IF ( Kn<iswtch ) THEN
!
!     SEQUENTIAL SEARCH MORE EFFICIENT
!
      khi = Kn*Len - jj
      DO j = 1 , khi , Len
         IF ( Arr(j)<Id ) THEN
         ELSEIF ( Arr(j)==Id ) THEN
            GOTO 100
         ELSE
            GOTO 300
         ENDIF
      ENDDO
      Jloc = khi + Len
      GOTO 200
   ELSE
      klo = 1
      khi = Kn
      k = (klo+khi+1)/2
      DO
         j = k*Len - jj
         IF ( Id<Arr(j) ) THEN
            khi = k
         ELSEIF ( Id==Arr(j) ) THEN
            EXIT
         ELSE
            klo = k
         ENDIF
         IF ( khi-klo<1 ) THEN
            Jloc = khi*Len - jj
            j = Kn*Len - jj
            IF ( Id>Arr(j) ) Jloc = Jloc + Len
            GOTO 200
         ELSEIF ( khi-klo==1 ) THEN
            IF ( k==klo ) THEN
               k = khi
            ELSE
               k = klo
            ENDIF
            klo = khi
         ELSE
            k = (klo+khi+1)/2
         ENDIF
      ENDDO
   ENDIF
 100  Jloc = j
   RETURN
 200  RETURN 1
 300  Jloc = j
   GOTO 200
END SUBROUTINE bisloc