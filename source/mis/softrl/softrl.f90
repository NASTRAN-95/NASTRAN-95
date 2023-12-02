!*==softrl.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE softrl(Name,Item,Mcb)
   IMPLICIT NONE
   USE C_MACHIN
   USE C_SOF
   USE C_SYS
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Name
   INTEGER :: Item
   INTEGER , DIMENSION(7) :: Mcb
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , i1 , i2 , imdi , inxt , itm , next
   INTEGER , SAVE :: ird
   INTEGER , DIMENSION(2) , SAVE :: nmsbr
   EXTERNAL andf , chkopn , fdsub , fmdi , fnxt , itcode , ittype , rshift , sofio
!
! End of declarations rewritten by SPAG
!
!
!     UTILITY SUBROUTINE TO OBTAIN THE MATRIX TRAILER FOR A MATRIX
!     STORED ON THE SOF
!     STATUS OF THE SOF ITEM IS RETURNED IN WORD ONE OF THE MATRIX
!     CONTROL BLOCK
!
!         1 NORMAL RETURN - THE TRAILER IS STORED IN WORDS 2 THRU 7
!         2 ITEM WAS PESUDO WRITTEN
!         3 ITEM DOES NOT EXIST
!         4 SUBSTRUCTURE NAME DOES NOT EXIST
!         5 ILLEGAL ITEM NAME
!
   DATA ird/1/
   DATA nmsbr/4HSOFT , 4HRL  /
!
!
!     CHECK IF ITEM IS ONE OF THE FOLLOWING ALLOWABLE NAMES.
!     KMTX,MMTX,PVEC,POVE,UPRT,HORG,UVEC,QVEC,PAPP,POAP,LMTX
!
   CALL chkopn(nmsbr(1))
   Ioitcd = itcode(Item)
   itm = ittype(Item)
   IF ( itm/=1 ) THEN
!
!
!     ERRORS
!
!     ILLEGAL ITEM
!
      Mcb(1) = 5
      RETURN
   ELSE
!
!     FIND SUBSTRUCTURE NAME AND MDI BLOCK
!
      CALL fdsub(Name,Iosind)
      IF ( Iosind<0 ) THEN
!
!     SUBSTRUCTURE DOES NOT EXIST
!
         Mcb(1) = 4
         RETURN
      ELSE
         CALL fmdi(Iosind,imdi)
!
!     GET BLOCK NUMBER OF FIRST BLOCK
!
         Iopbn = andf(Buf(imdi+Ioitcd),Jhalf)
         IF ( Iopbn==0 ) THEN
!
!     ITEM DOES NOT EXIST
!
            Mcb(1) = 3
            RETURN
         ELSEIF ( Iopbn==Jhalf ) THEN
!
!     ITEM IS PESUDO WRITTEN
!
            Mcb(1) = 2
            RETURN
         ELSE
            Iolbn = 1
         ENDIF
      ENDIF
   ENDIF
   SPAG_Loop_1_1: DO
!
!     GET NEXT BLOCK IN CHAIN
!
      CALL fnxt(Iopbn,inxt)
      IF ( mod(Iopbn,2)==1 ) THEN
         next = andf(Buf(inxt),Jhalf)
      ELSE
         next = andf(rshift(Buf(inxt),Ihalf),Jhalf)
      ENDIF
      IF ( next==0 ) EXIT SPAG_Loop_1_1
      Iopbn = next
      Iolbn = Iolbn + 1
   ENDDO SPAG_Loop_1_1
!
!     WE HAVE HIT END OF CHAIN - READ THE LAST BLOCK
!
   CALL sofio(ird,Iopbn,Buf(Io-2))
   i1 = Io - 2
   i2 = i1 + Blksiz + 4
!
!     EXTRACT TRAILER FROM BLOCK
!
   DO i = 1 , 6
      Mcb(i+1) = Buf(Io+Blksiz-6+i)
   ENDDO
   Mcb(1) = 1
   RETURN
END SUBROUTINE softrl
