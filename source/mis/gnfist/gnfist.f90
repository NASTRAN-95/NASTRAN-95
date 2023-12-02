!*==gnfist.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gnfist(Filenm,Fistnm,Modno)
   IMPLICIT NONE
   USE C_IPURGE
   USE C_ISOSGN
   USE C_IXSFA
   USE C_OSCENT
   USE C_SYSTEM
   USE C_XDPL
   USE C_XFIAT
   USE C_XFIST
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Filenm
   INTEGER :: Fistnm
   INTEGER :: Modno
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: fistx , i , i1 , j , j1 , k , l , lfiat , m
   INTEGER , SAVE :: mask , mask1
   EXTERNAL andf , mesage , xsfa
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
   DATA mask1/65535/ , mask/32767/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
            IF ( Filenm(1)==Fiat(k) .AND. Filenm(2)==Fiat(k+1) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            k = k + Icfiat
         ENDDO
!
!     FILE NOT IN FIAT - IF INPUT FILE ASSUME PURGED
!
         IF ( Fistnm>100 .AND. Fistnm<200 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     MUST CALL IN FILE ALLOCATOR
!
         CALL xsfa(Modno)
         Modno = -Modno
         RETURN
      CASE (3)
!
!     IF FILE POINTER = 77777 NO ENTRY IS MADE IN FIST
!
         IF ( andf(Fiat(k-1),mask)==mask ) RETURN
         IF ( Fistnm<=100 .OR. Fistnm>=300 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
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
               IF ( Idpl(m)==Filenm(1) .AND. Idpl(m+1)==Filenm(2) ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!
!     INPUT FILE
!     ==========
!
!     SEE IF IT EXISTS
!
            IF ( Fiat(k+2)/=0 .OR. Fiat(k+3)/=0 .OR. Fiat(k+4)/=0 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Icfiat==11 .AND. (Fiat(k+7)/=0 .OR. Fiat(k+8)/=0 .OR. Fiat(k+9)/=0) ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     INPUT FILE NOT GENERATED ACCORDING TO FIAT - CHECK DPL
!
         i1 = Oscar(7)*3 + 5
         j1 = Idpl(3)*3 + 1
         l = Fiat(3)*Icfiat - 2
         DO j = 4 , j1 , 3
            IF ( Idpl(j)==Filenm(1) .AND. Idpl(j+1)==Filenm(2) ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         RETURN
      CASE (5)
!
!     FILE IN DPL - ZERO FIAT ENTRY SO FILE ALLOCATOR WILL UNPOOL IT.
!     DO THIS FOR OTHER LIKE I/P FILES IN OSCAR ENTRY.
!
         DO i = 8 , i1 , 3
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  IF ( Oscar(i)/=0 ) THEN
!
!     SEARCH FIAT
!
                     DO k = 4 , l , Icfiat
                        IF ( Oscar(i)==Fiat(k+1) .AND. Oscar(i+1)==Fiat(k+2) ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
!
!     FILE NOT IN FIAT - CHECK NEXT INPUT FILE
!
                     ENDDO
                  ENDIF
                  CYCLE
               CASE (2)
!
!     FILE IN FIAT - CHECK DPL IF FIAT TRAILER IS ZERO
!
                  IF ( Fiat(k+3)==0 .AND. Fiat(k+4)==0 .AND. Fiat(k+5)==0 .AND. andf(mask,Fiat(k))/=mask ) THEN
                     IF ( .NOT.(Icfiat==11 .AND. (Fiat(k+8)/=0 .OR. Fiat(k+9)/=0 .OR. Fiat(k+10)/=0)) ) THEN
                        DO j = 4 , j1 , 3
                           IF ( Idpl(j)==Fiat(k+1) .AND. Idpl(j+1)==Fiat(k+2) ) GOTO 2
                        ENDDO
                     ENDIF
                     CYCLE
!
!     FILE IS IN DPL - ZERO OUT FIAT ENTRY
!
 2                   Fiat(k) = andf(mask1,Fiat(k))
                     IF ( andf(mask,Fiat(k))==mask ) Fiat(k) = 0
                     Fiat(k+1) = 0
                     Fiat(k+2) = 0
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
!     CALL FILE ALLOCATOR AND UNPOOL FILES
!
         ENDDO
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
!
!     FILE NAME IS IN DPL - PURGE IT AND ALL EQUIV FILE FROM DPL
!
         Idpl(m) = 0
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
         spag_nextblock_1 = 7
      CASE (7)
!
!     CHECK FOR FIST TABLE OVERFLOW
!
         IF ( Fist(1)<=Fist(2) ) CALL mesage(-20,iabs(Modno),Filenm)
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
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gnfist
