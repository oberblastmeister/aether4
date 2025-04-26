#include <stdio.h>
#include <stdint.h>

#define int int64_t

/*
(
 (kind (CompileAndRun))
 (emit (Abs_asm Asm))
)
*/

// TODO: this is wrong
signed main() {
    int total = 0;
	int r00 = 2;
	int r01 = 16;
	int r02 = 15;
	int r03 = 14;

	for(; r00 > 0; r00 = r00 - 1) {
		r02 += r03;
		r01 += r02;
		r00 += r01;

		total += r00;

		if(r03 != 0)
			r03 -= 1;
		if(r02 != 0)
			r02 -= 1;
		if(r01 != 0)
			r01 -= 1;
	}
	
	printf("%ld", total);
}