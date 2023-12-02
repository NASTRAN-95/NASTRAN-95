!*==gnfist.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gnfist(Filenm,Fistnm,Modno)
   USE c_ipurge
   USE c_isosgn
   USE c_ixsfa
   USE c_oscent
   USE c_system
   USE c_xdpl
   USE c_xfiat
   USE c_xfist
   IMPLICIT NONE
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
            ipval(k) = 0
            ixval(k) = 0
         ENDDO
         DO k = 5 , 34
            isval(k) = 0
         ENDDO
!
         isval(1) = 3
         isval(2) = 3
         isval(3) = 1
         isval(4) = 2
!
         ixval(3) = 10
!
         IF ( Filenm(1)==0 .AND. Filenm(2)==0 ) RETURN
!
!     SEARCH FIAT FOR MATCHING FILE
!
         lfiat = fiat(3)
         k = 5
         DO j = 1 , lfiat
            IF ( Filenm(1)==fiat(k) .AND. Filenm(2)==fiat(k+1) ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            k = k + icfiat
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
         IF ( andf(fiat(k-1),mask)==mask ) RETURN
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
            j1 = idpl(3)*3 + 1
            DO m = 4 , j1 , 3
               IF ( idpl(m)==Filenm(1) .AND. idpl(m+1)==Filenm(2) ) THEN
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
            IF ( fiat(k+2)/=0 .OR. fiat(k+3)/=0 .OR. fiat(k+4)/=0 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( icfiat==11 .AND. (fiat(k+7)/=0 .OR. fiat(k+8)/=0 .OR. fiat(k+9)/=0) ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     INPUT FILE NOT GENERATED ACCORDING TO FIAT - CHECK DPL
!
         i1 = oscar(7)*3 + 5
         j1 = idpl(3)*3 + 1
         l = fiat(3)*icfiat - 2
         DO j = 4 , j1 , 3
            IF ( idpl(j)==Filenm(1) .AND. idpl(j+1)==Filenm(2) ) THEN
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
                  IF ( oscar(i)/=0 ) THEN
!
!     SEARCH FIAT
!
                     SPAG_Loop_4_1: DO k = 4 , l , icfiat
                        IF ( oscar(i)==fiat(k+1) .AND. oscar(i+1)==fiat(k+2) ) THEN
                           spag_nextblock_2 = 2
                           EXIT SPAG_Loop_4_1
                        ENDIF
!
!     FILE NOT IN FIAT - CHECK NEXT INPUT FILE
!
                     ENDDO SPAG_Loop_4_1
                  ENDIF
               CASE (2)
!
!     FILE IN FIAT - CHECK DPL IF FIAT TRAILER IS ZERO
!
                  IF ( fiat(k+3)==0 .AND. fiat(k+4)==0 .AND. fiat(k+5)==0 .AND. andf(mask,fiat(k))/=mask ) THEN
                     IF ( .NOT.(icfiat==11 .AND. (fiat(k+8)/=0 .OR. fiat(k+9)/=0 .OR. fiat(k+10)/=0)) ) THEN
                        DO j = 4 , j1 , 3
                           IF ( idpl(j)==fiat(k+1) .AND. idpl(j+1)==fiat(k+2) ) GOTO 2
                        ENDDO
                     ENDIF
                     CYCLE
!
!     FILE IS IN DPL - ZERO OUT FIAT ENTRY
!
 2                   fiat(k) = andf(mask1,fiat(k))
                     IF ( andf(mask,fiat(k))==mask ) fiat(k) = 0
                     fiat(k+1) = 0
                     fiat(k+2) = 0
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
!     CALL FILE ALLOCATOR AND UNPOOL FILES
!
         ENDDO
         spag_nextblock_1 = 2
      CASE (6)
!
!     FILE NAME IS IN DPL - PURGE IT AND ALL EQUIV FILE FROM DPL
!
         idpl(m) = 0
         idpl(m+1) = 0
         l = idpl(m+2)
         DO j = 4 , j1 , 3
            IF ( j/=m .AND. l==idpl(j+2) ) THEN
               idpl(j) = 0
               idpl(j+1) = 0
               idpl(j+2) = 0
            ENDIF
         ENDDO
!
!     IF THIS IS LAST FILE ON POOL TAPE, DECREASE FILE COUNT IN DPL
!
         IF ( andf(l,mask)==idpl(1)-1 ) THEN
            idpl(1) = idpl(1) - 1
            idpl(m+2) = 0
         ENDIF
!
!     IF DELETED FILES ARE AT END OF DPL, DECREMENT ENTRY COUNT
!
         DO WHILE ( idpl(j1)==0 .AND. idpl(j1+1)==0 .AND. idpl(j1+2)==0 )
            idpl(3) = idpl(3) - 1
            j1 = idpl(3)*3 + 1
         ENDDO
         spag_nextblock_1 = 7
      CASE (7)
!
!     CHECK FOR FIST TABLE OVERFLOW
!
         IF ( fist(1)<=fist(2) ) CALL mesage(-20,iabs(Modno),Filenm)
         fist(2) = fist(2) + 1
         fistx = fist(2)*2 + 1
         fist(fistx) = Fistnm
         fist(fistx+1) = k - 2
         IF ( Fistnm<300 ) RETURN
!
!     ZERO TRAILER FOR SCRATCH FILE
!
         fiat(k+2) = 0
         fiat(k+3) = 0
         fiat(k+4) = 0
         IF ( icfiat/=8 ) THEN
            fiat(k+7) = 0
            fiat(k+8) = 0
            fiat(k+9) = 0
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gnfist
