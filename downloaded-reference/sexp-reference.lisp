;;; Attempt to express Intel's documentation as sexp

'(
  ("AAA—ASCII Adjust After Addition"
   ;;opcodes
   (columns opcode/instruction op/en 64-bit-mode compat/leg-mode description)
   ((37) (AAA) (NP) (Invalid) (Valid) ("ASCII adjust AL after addition."))
   ;;Instruction Operand Encoding
   (columns op/en operand-1 operand-2 operand-3 operand-4)
   (NP NA NA NA NA)
   )

  ("MULPS—Multiply Packed Single-Precision Floating-Point Values"
   ;;opcodes
   (columns opcode/instruction op/en 64/32-bit-mode cpuid-feature-flag description)
   (( 0F 59 /r) (MULPS xmm1, xmm2/m128) (rm) (v/v) (sse) 
    (Multiply packed single-precision floating-point values in xmm2/mem by xmm1.))
   ((VEX.NDS.128.0F.WIG 59 /r) (VMULPS xmm1,xmm2, xmm3/m128) (RVM) (V/V) (AVX) 
    (Multiply packed single-precision floating-point values from xmm3/mem to xmm2 and stores result in xmm1.))
   ((VEX.NDS.256.0F.WIG 59 /r) (VEX.NDS.256.0F.WIG 59 /r) (RVM) (V/V ) (AVX )
    (Multiply packed single-precision floating-point values from ymm3/mem to ymm2 and stores result in ymm1.))
   ;;Instruction Operand Encoding
   (columns op/en operand-1 operand-2 operand-3 operand-4)
   ((RM) (ModRM:reg (r, w)) (ModRM:r/m (r)) (NA) (NA))
   ((RVM) (ModRM:reg (w)) (VEX.vvvv (r)) (ModRM:r/m (r)) (NA))

   )

  )
