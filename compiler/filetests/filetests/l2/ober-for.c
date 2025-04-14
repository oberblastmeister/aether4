#include <stdio.h>
#include <stdint.h>

#define int int64_t

signed main() {
    int x = 27;
    int steps = 0;
    for (int n = x; n != 1;)
    {
      if (n % 2 == 0)
        n /= 2;
      else
        n = 3 * n + 1;
      steps += 1;
    }
    
    printf("%ld",  steps);
}