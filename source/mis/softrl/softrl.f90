!*==softrl.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE softrl(Name,Item,Mcb)
   USE c_machin
   USE c_sof
   USE c_sys
   USE c_zzzzzz
   IMPLICIT NONE
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
   ioitcd = itcode(Item)
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
      CALL fdsub(Name,iosind)
      IF ( iosind<0 ) THEN
!
!     SUBSTRUCTURE DOES NOT EXIST
!
         Mcb(1) = 4
         RETURN
      ELSE
         CALL fmdi(iosind,imdi)
!
!     GET BLOCK NUMBER OF FIRST BLOCK
!
         iopbn = andf(buf(imdi+ioitcd),jhalf)
         IF ( iopbn==0 ) THEN
!
!     ITEM DOES NOT EXIST
!
            Mcb(1) = 3
            RETURN
         ELSEIF ( iopbn==jhalf ) THEN
!
!     ITEM IS PESUDO WRITTEN
!
            Mcb(1) = 2
            RETURN
         ELSE
            iolbn = 1
         ENDIF
      ENDIF
   ENDIF
   SPAG_Loop_1_1: DO
!
!     GET NEXT BLOCK IN CHAIN
!
      CALL fnxt(iopbn,inxt)
      IF ( mod(iopbn,2)==1 ) THEN
         next = andf(buf(inxt),jhalf)
      ELSE
         next = andf(rshift(buf(inxt),ihalf),jhalf)
      ENDIF
      IF ( next==0 ) EXIT SPAG_Loop_1_1
      iopbn = next
      iolbn = iolbn + 1
   ENDDO SPAG_Loop_1_1
!
!     WE HAVE HIT END OF CHAIN - READ THE LAST BLOCK
!
   CALL sofio(ird,iopbn,buf(io-2))
   i1 = io - 2
   i2 = i1 + blksiz + 4
!
!     EXTRACT TRAILER FROM BLOCK
!
   DO i = 1 , 6
      Mcb(i+1) = buf(io+blksiz-6+i)
   ENDDO
   Mcb(1) = 1
END SUBROUTINE softrl
