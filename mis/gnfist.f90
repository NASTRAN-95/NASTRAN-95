
SUBROUTINE gnfist(Filenm,Fistnm,Modno)
   IMPLICIT NONE
   INTEGER Fiat(3) , Fist(2) , Icfiat , Idpl(3) , Ipval(5) , Isval(34) , Ixval(5) , Oscar(7)
   REAL Skip(23)
   COMMON /ipurge/ Ipval
   COMMON /isosgn/ Isval
   COMMON /ixsfa / Ixval
   COMMON /oscent/ Oscar
   COMMON /system/ Skip , Icfiat
   COMMON /xdpl  / Idpl
   COMMON /xfiat / Fiat
   COMMON /xfist / Fist
   INTEGER Fistnm , Modno
   INTEGER Filenm(2)
   INTEGER andf
   INTEGER fistx , i , i1 , j , j1 , k , l , lfiat , m , mask , mask1
   EXTERNAL andf
!
   DATA mask1/65535/ , mask/32767/
!             MASK1 = O177777   MASK  = O77777
!
   DO k = 1 , 5
      Ipval(k) = 0
      Ixval(k) = 0
   ENDDO
   DO k = 5 , 34
      Isval(k) = 0
   ENDDO
!
   Isval(1) = 3
   Isval(2) = 3
   Isval(3) = 1
   Isval(4) = 2
!
   Ixval(3) = 10
!
   IF ( Filenm(1)==0 .AND. Filenm(2)==0 ) RETURN
!
!     SEARCH FIAT FOR MATCHING FILE
!
   lfiat = Fiat(3)
   k = 5
   DO j = 1 , lfiat
      IF ( Filenm(1)==Fiat(k) .AND. Filenm(2)==Fiat(k+1) ) GOTO 200
      k = k + Icfiat
   ENDDO
!
!     FILE NOT IN FIAT - IF INPUT FILE ASSUME PURGED
!
   IF ( Fistnm>100 .AND. Fistnm<200 ) GOTO 300
!
!     MUST CALL IN FILE ALLOCATOR
!
 100  CALL xsfa(Modno)
   Modno = -Modno
   RETURN
!
!     IF FILE POINTER = 77777 NO ENTRY IS MADE IN FIST
!
 200  IF ( andf(Fiat(k-1),mask)==mask ) RETURN
   IF ( Fistnm<=100 .OR. Fistnm>=300 ) GOTO 600
   IF ( Fistnm>=200 ) THEN
!
!
!     OUTPUT FILE
!     ===========
!
!     SEARCH DPL FOR FILE NAME
!
      j1 = Idpl(3)*3 + 1
      DO m = 4 , j1 , 3
         IF ( Idpl(m)==Filenm(1) .AND. Idpl(m+1)==Filenm(2) ) GOTO 500
      ENDDO
      GOTO 600
   ELSE
!
!
!     INPUT FILE
!     ==========
!
!     SEE IF IT EXISTS
!
      IF ( Fiat(k+2)/=0 .OR. Fiat(k+3)/=0 .OR. Fiat(k+4)/=0 ) GOTO 600
      IF ( Icfiat==11 .AND. (Fiat(k+7)/=0 .OR. Fiat(k+8)/=0 .OR. Fiat(k+9)/=0) ) GOTO 600
   ENDIF
!
!     INPUT FILE NOT GENERATED ACCORDING TO FIAT - CHECK DPL
!
 300  i1 = Oscar(7)*3 + 5
   j1 = Idpl(3)*3 + 1
   l = Fiat(3)*Icfiat - 2
   DO j = 4 , j1 , 3
      IF ( Idpl(j)==Filenm(1) .AND. Idpl(j+1)==Filenm(2) ) GOTO 400
   ENDDO
   RETURN
!
!     FILE IN DPL - ZERO FIAT ENTRY SO FILE ALLOCATOR WILL UNPOOL IT.
!     DO THIS FOR OTHER LIKE I/P FILES IN OSCAR ENTRY.
!
 400  DO i = 8 , i1 , 3
      IF ( Oscar(i)/=0 ) THEN
!
!     SEARCH FIAT
!
         DO k = 4 , l , Icfiat
            IF ( Oscar(i)==Fiat(k+1) .AND. Oscar(i+1)==Fiat(k+2) ) GOTO 450
!
!     FILE NOT IN FIAT - CHECK NEXT INPUT FILE
!
         ENDDO
      ENDIF
      CYCLE
!
!     FILE IN FIAT - CHECK DPL IF FIAT TRAILER IS ZERO
!
 450  IF ( Fiat(k+3)==0 .AND. Fiat(k+4)==0 .AND. Fiat(k+5)==0 .AND. andf(mask,Fiat(k))/=mask ) THEN
         IF ( .NOT.(Icfiat==11 .AND. (Fiat(k+8)/=0 .OR. Fiat(k+9)/=0 .OR. Fiat(k+10)/=0)) ) THEN
            DO j = 4 , j1 , 3
               IF ( Idpl(j)==Fiat(k+1) .AND. Idpl(j+1)==Fiat(k+2) ) GOTO 460
            ENDDO
         ENDIF
         CYCLE
!
!     FILE IS IN DPL - ZERO OUT FIAT ENTRY
!
 460     Fiat(k) = andf(mask1,Fiat(k))
         IF ( andf(mask,Fiat(k))==mask ) Fiat(k) = 0
         Fiat(k+1) = 0
         Fiat(k+2) = 0
      ENDIF
   ENDDO
!
!     CALL FILE ALLOCATOR AND UNPOOL FILES
!
   GOTO 100
!
!     FILE NAME IS IN DPL - PURGE IT AND ALL EQUIV FILE FROM DPL
!
 500  Idpl(m) = 0
   Idpl(m+1) = 0
   l = Idpl(m+2)
   DO j = 4 , j1 , 3
      IF ( j/=m .AND. l==Idpl(j+2) ) THEN
         Idpl(j) = 0
         Idpl(j+1) = 0
         Idpl(j+2) = 0
      ENDIF
   ENDDO
!
!     IF THIS IS LAST FILE ON POOL TAPE, DECREASE FILE COUNT IN DPL
!
   IF ( andf(l,mask)==Idpl(1)-1 ) THEN
      Idpl(1) = Idpl(1) - 1
      Idpl(m+2) = 0
   ENDIF
!
!     IF DELETED FILES ARE AT END OF DPL, DECREMENT ENTRY COUNT
!
   DO WHILE ( Idpl(j1)==0 .AND. Idpl(j1+1)==0 .AND. Idpl(j1+2)==0 )
      Idpl(3) = Idpl(3) - 1
      j1 = Idpl(3)*3 + 1
   ENDDO
!
!     CHECK FOR FIST TABLE OVERFLOW
!
 600  IF ( Fist(1)<=Fist(2) ) CALL mesage(-20,iabs(Modno),Filenm)
   Fist(2) = Fist(2) + 1
   fistx = Fist(2)*2 + 1
   Fist(fistx) = Fistnm
   Fist(fistx+1) = k - 2
   IF ( Fistnm<300 ) RETURN
!
!     ZERO TRAILER FOR SCRATCH FILE
!
   Fiat(k+2) = 0
   Fiat(k+3) = 0
   Fiat(k+4) = 0
   IF ( Icfiat/=8 ) THEN
      Fiat(k+7) = 0
      Fiat(k+8) = 0
      Fiat(k+9) = 0
   ENDIF
END SUBROUTINE gnfist
