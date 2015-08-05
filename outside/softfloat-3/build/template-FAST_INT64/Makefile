
#=============================================================================
#
# This Makefile template is part of the SoftFloat IEEE Floating-Point
# Arithmetic Package, Release 3, by John R. Hauser.
#
# Copyright 2011, 2012, 2013, 2014 The Regents of the University of California
# (Regents).  All Rights Reserved.  Redistribution and use in source and
# binary forms, with or without modification, are permitted provided that the
# following conditions are met:
#
# Redistributions of source code must retain the above copyright notice,
# this list of conditions, and the following two paragraphs of disclaimer.
# Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions, and the following two paragraphs of disclaimer in
# the documentation and/or other materials provided with the distribution.
# Neither the name of the Regents nor the names of its contributors may be
# used to endorse or promote products derived from this software without
# specific prior written permission.
#
# IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
# SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
# ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
# REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE.  THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF ANY, PROVIDED
# HEREUNDER IS PROVIDED "AS IS".  REGENTS HAS NO OBLIGATION TO PROVIDE
# MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#
#=============================================================================

# Edit lines marked with `==>'.  See "SoftFloat-source.html".

==> SOURCE_DIR = ../../source
==> SPECIALIZE_TYPE = 8086

==> SOFTFLOAT_OPTS = -DINLINE_LEVEL=5 -DSOFTFLOAT_FAST_DIV64TO32

==> DELETE = rm -f
==> C_INCLUDES = -I. -I$(SOURCE_DIR)/$(SPECIALIZE_TYPE) -I$(SOURCE_DIR)/include
==> COMPILE_C = \
==>   cc -c -DSOFTFLOAT_FAST_INT64 $(SOFTFLOAT_OPTS) $(C_INCLUDES) -O2 -o $@
==> MAKELIB = ar crs $@

==> OBJ = .o
==> LIB = .a

.PHONY: all
all: softfloat$(LIB)

OBJS_PRIMITIVES = \
  s_eq128$(OBJ) \
  s_le128$(OBJ) \
  s_lt128$(OBJ) \
  s_shortShiftLeft128$(OBJ) \
  s_shortShiftRight128$(OBJ) \
  s_shortShiftRightJam64$(OBJ) \
  s_shortShiftRightJam64Extra$(OBJ) \
  s_shortShiftRightJam128$(OBJ) \
  s_shortShiftRightJam128Extra$(OBJ) \
  s_shiftRightJam32$(OBJ) \
  s_shiftRightJam64$(OBJ) \
  s_shiftRightJam64Extra$(OBJ) \
  s_shiftRightJam128$(OBJ) \
  s_shiftRightJam128Extra$(OBJ) \
  s_shiftRightJam256M$(OBJ) \
  s_countLeadingZeros8$(OBJ) \
  s_countLeadingZeros32$(OBJ) \
  s_countLeadingZeros64$(OBJ) \
  s_add128$(OBJ) \
  s_add256M$(OBJ) \
  s_sub128$(OBJ) \
  s_sub256M$(OBJ) \
  s_mul64ByShifted32To128$(OBJ) \
  s_mul64To128$(OBJ) \
  s_mul128By32$(OBJ) \
  s_mul128To256M$(OBJ) \
  s_approxRecip32_1$(OBJ) \
  s_approxRecipSqrt32_1$(OBJ) \

OBJS_SPECIALIZE = \
  softfloat_raiseFlags$(OBJ) \
  s_f32UIToCommonNaN$(OBJ) \
  s_commonNaNToF32UI$(OBJ) \
  s_propagateNaNF32UI$(OBJ) \
  s_f64UIToCommonNaN$(OBJ) \
  s_commonNaNToF64UI$(OBJ) \
  s_propagateNaNF64UI$(OBJ) \
  extF80M_isSignalingNaN$(OBJ) \
  s_extF80UIToCommonNaN$(OBJ) \
  s_commonNaNToExtF80UI$(OBJ) \
  s_propagateNaNExtF80UI$(OBJ) \
  f128M_isSignalingNaN$(OBJ) \
  s_f128UIToCommonNaN$(OBJ) \
  s_commonNaNToF128UI$(OBJ) \
  s_propagateNaNF128UI$(OBJ) \

OBJS_OTHERS = \
  s_roundPackToUI32$(OBJ) \
  s_roundPackToUI64$(OBJ) \
  s_roundPackToI32$(OBJ) \
  s_roundPackToI64$(OBJ) \
  s_normSubnormalF32Sig$(OBJ) \
  s_roundPackToF32$(OBJ) \
  s_normRoundPackToF32$(OBJ) \
  s_addMagsF32$(OBJ) \
  s_subMagsF32$(OBJ) \
  s_mulAddF32$(OBJ) \
  s_normSubnormalF64Sig$(OBJ) \
  s_roundPackToF64$(OBJ) \
  s_normRoundPackToF64$(OBJ) \
  s_addMagsF64$(OBJ) \
  s_subMagsF64$(OBJ) \
  s_mulAddF64$(OBJ) \
  s_normSubnormalExtF80Sig$(OBJ) \
  s_roundPackToExtF80$(OBJ) \
  s_normRoundPackToExtF80$(OBJ) \
  s_addMagsExtF80$(OBJ) \
  s_subMagsExtF80$(OBJ) \
  s_normSubnormalF128Sig$(OBJ) \
  s_roundPackToF128$(OBJ) \
  s_normRoundPackToF128$(OBJ) \
  s_addMagsF128$(OBJ) \
  s_subMagsF128$(OBJ) \
  s_mulAddF128$(OBJ) \
  softfloat_state$(OBJ) \
  ui32_to_f32$(OBJ) \
  ui32_to_f64$(OBJ) \
  ui32_to_extF80$(OBJ) \
  ui32_to_extF80M$(OBJ) \
  ui32_to_f128$(OBJ) \
  ui32_to_f128M$(OBJ) \
  ui64_to_f32$(OBJ) \
  ui64_to_f64$(OBJ) \
  ui64_to_extF80$(OBJ) \
  ui64_to_extF80M$(OBJ) \
  ui64_to_f128$(OBJ) \
  ui64_to_f128M$(OBJ) \
  i32_to_f32$(OBJ) \
  i32_to_f64$(OBJ) \
  i32_to_extF80$(OBJ) \
  i32_to_extF80M$(OBJ) \
  i32_to_f128$(OBJ) \
  i32_to_f128M$(OBJ) \
  i64_to_f32$(OBJ) \
  i64_to_f64$(OBJ) \
  i64_to_extF80$(OBJ) \
  i64_to_extF80M$(OBJ) \
  i64_to_f128$(OBJ) \
  i64_to_f128M$(OBJ) \
  f32_to_ui32$(OBJ) \
  f32_to_ui64$(OBJ) \
  f32_to_i32$(OBJ) \
  f32_to_i64$(OBJ) \
  f32_to_ui32_r_minMag$(OBJ) \
  f32_to_ui64_r_minMag$(OBJ) \
  f32_to_i32_r_minMag$(OBJ) \
  f32_to_i64_r_minMag$(OBJ) \
  f32_to_f64$(OBJ) \
  f32_to_extF80$(OBJ) \
  f32_to_extF80M$(OBJ) \
  f32_to_f128$(OBJ) \
  f32_to_f128M$(OBJ) \
  f32_roundToInt$(OBJ) \
  f32_add$(OBJ) \
  f32_sub$(OBJ) \
  f32_mul$(OBJ) \
  f32_mulAdd$(OBJ) \
  f32_div$(OBJ) \
  f32_rem$(OBJ) \
  f32_sqrt$(OBJ) \
  f32_eq$(OBJ) \
  f32_le$(OBJ) \
  f32_lt$(OBJ) \
  f32_eq_signaling$(OBJ) \
  f32_le_quiet$(OBJ) \
  f32_lt_quiet$(OBJ) \
  f32_isSignalingNaN$(OBJ) \
  f64_to_ui32$(OBJ) \
  f64_to_ui64$(OBJ) \
  f64_to_i32$(OBJ) \
  f64_to_i64$(OBJ) \
  f64_to_ui32_r_minMag$(OBJ) \
  f64_to_ui64_r_minMag$(OBJ) \
  f64_to_i32_r_minMag$(OBJ) \
  f64_to_i64_r_minMag$(OBJ) \
  f64_to_f32$(OBJ) \
  f64_to_extF80$(OBJ) \
  f64_to_extF80M$(OBJ) \
  f64_to_f128$(OBJ) \
  f64_to_f128M$(OBJ) \
  f64_roundToInt$(OBJ) \
  f64_add$(OBJ) \
  f64_sub$(OBJ) \
  f64_mul$(OBJ) \
  f64_mulAdd$(OBJ) \
  f64_div$(OBJ) \
  f64_rem$(OBJ) \
  f64_sqrt$(OBJ) \
  f64_eq$(OBJ) \
  f64_le$(OBJ) \
  f64_lt$(OBJ) \
  f64_eq_signaling$(OBJ) \
  f64_le_quiet$(OBJ) \
  f64_lt_quiet$(OBJ) \
  f64_isSignalingNaN$(OBJ) \
  extF80_to_ui32$(OBJ) \
  extF80_to_ui64$(OBJ) \
  extF80_to_i32$(OBJ) \
  extF80_to_i64$(OBJ) \
  extF80_to_ui32_r_minMag$(OBJ) \
  extF80_to_ui64_r_minMag$(OBJ) \
  extF80_to_i32_r_minMag$(OBJ) \
  extF80_to_i64_r_minMag$(OBJ) \
  extF80_to_f32$(OBJ) \
  extF80_to_f64$(OBJ) \
  extF80_to_f128$(OBJ) \
  extF80_roundToInt$(OBJ) \
  extF80_add$(OBJ) \
  extF80_sub$(OBJ) \
  extF80_mul$(OBJ) \
  extF80_div$(OBJ) \
  extF80_rem$(OBJ) \
  extF80_sqrt$(OBJ) \
  extF80_eq$(OBJ) \
  extF80_le$(OBJ) \
  extF80_lt$(OBJ) \
  extF80_eq_signaling$(OBJ) \
  extF80_le_quiet$(OBJ) \
  extF80_lt_quiet$(OBJ) \
  extF80_isSignalingNaN$(OBJ) \
  extF80M_to_ui32$(OBJ) \
  extF80M_to_ui64$(OBJ) \
  extF80M_to_i32$(OBJ) \
  extF80M_to_i64$(OBJ) \
  extF80M_to_ui32_r_minMag$(OBJ) \
  extF80M_to_ui64_r_minMag$(OBJ) \
  extF80M_to_i32_r_minMag$(OBJ) \
  extF80M_to_i64_r_minMag$(OBJ) \
  extF80M_to_f32$(OBJ) \
  extF80M_to_f64$(OBJ) \
  extF80M_to_f128M$(OBJ) \
  extF80M_roundToInt$(OBJ) \
  extF80M_add$(OBJ) \
  extF80M_sub$(OBJ) \
  extF80M_mul$(OBJ) \
  extF80M_div$(OBJ) \
  extF80M_rem$(OBJ) \
  extF80M_sqrt$(OBJ) \
  extF80M_eq$(OBJ) \
  extF80M_le$(OBJ) \
  extF80M_lt$(OBJ) \
  extF80M_eq_signaling$(OBJ) \
  extF80M_le_quiet$(OBJ) \
  extF80M_lt_quiet$(OBJ) \
  f128_to_ui32$(OBJ) \
  f128_to_ui64$(OBJ) \
  f128_to_i32$(OBJ) \
  f128_to_i64$(OBJ) \
  f128_to_ui32_r_minMag$(OBJ) \
  f128_to_ui64_r_minMag$(OBJ) \
  f128_to_i32_r_minMag$(OBJ) \
  f128_to_i64_r_minMag$(OBJ) \
  f128_to_f32$(OBJ) \
  f128_to_extF80$(OBJ) \
  f128_to_f64$(OBJ) \
  f128_roundToInt$(OBJ) \
  f128_add$(OBJ) \
  f128_sub$(OBJ) \
  f128_mul$(OBJ) \
  f128_mulAdd$(OBJ) \
  f128_div$(OBJ) \
  f128_rem$(OBJ) \
  f128_sqrt$(OBJ) \
  f128_eq$(OBJ) \
  f128_le$(OBJ) \
  f128_lt$(OBJ) \
  f128_eq_signaling$(OBJ) \
  f128_le_quiet$(OBJ) \
  f128_lt_quiet$(OBJ) \
  f128_isSignalingNaN$(OBJ) \
  f128M_to_ui32$(OBJ) \
  f128M_to_ui64$(OBJ) \
  f128M_to_i32$(OBJ) \
  f128M_to_i64$(OBJ) \
  f128M_to_ui32_r_minMag$(OBJ) \
  f128M_to_ui64_r_minMag$(OBJ) \
  f128M_to_i32_r_minMag$(OBJ) \
  f128M_to_i64_r_minMag$(OBJ) \
  f128M_to_f32$(OBJ) \
  f128M_to_extF80M$(OBJ) \
  f128M_to_f64$(OBJ) \
  f128M_roundToInt$(OBJ) \
  f128M_add$(OBJ) \
  f128M_sub$(OBJ) \
  f128M_mul$(OBJ) \
  f128M_mulAdd$(OBJ) \
  f128M_div$(OBJ) \
  f128M_rem$(OBJ) \
  f128M_sqrt$(OBJ) \
  f128M_eq$(OBJ) \
  f128M_le$(OBJ) \
  f128M_lt$(OBJ) \
  f128M_eq_signaling$(OBJ) \
  f128M_le_quiet$(OBJ) \
  f128M_lt_quiet$(OBJ) \

OBJS_ALL = $(OBJS_PRIMITIVES) $(OBJS_SPECIALIZE) $(OBJS_OTHERS)

$(OBJS_ALL): \
  platform.h $(SOURCE_DIR)/include/primitiveTypes.h \
  $(SOURCE_DIR)/include/primitives.h
$(OBJS_SPECIALIZE) $(OBJS_OTHERS): \
  $(SOURCE_DIR)/include/softfloat_types.h $(SOURCE_DIR)/include/internals.h \
  $(SOURCE_DIR)/$(SPECIALIZE_TYPE)/specialize.h \
  $(SOURCE_DIR)/include/softfloat.h

$(OBJS_PRIMITIVES) $(OBJS_OTHERS): %$(OBJ): $(SOURCE_DIR)/%.c
	$(COMPILE_C) $(SOURCE_DIR)/$*.c

$(OBJS_SPECIALIZE): %$(OBJ): $(SOURCE_DIR)/$(SPECIALIZE_TYPE)/%.c
	$(COMPILE_C) $(SOURCE_DIR)/$(SPECIALIZE_TYPE)/$*.c

softfloat$(LIB): $(OBJS_ALL)
	$(DELETE) $@
	$(MAKELIB) $^

.PHONY: clean
clean:
	$(DELETE) $(OBJS_ALL) softfloat$(LIB)

