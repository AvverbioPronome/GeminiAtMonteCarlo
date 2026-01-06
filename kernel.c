#include <stdint.h>

// Use int64_t to handle counts up to 9 quintillion
int64_t monte_carlo_kernel(int64_t n, uint32_t seed) {
    int64_t hits = 0;
    uint32_t s = seed;
    
    #pragma omp simd reduction(+:hits)
    for (int64_t i = 0; i < n; i++) {
        s = s ^ (s << 13); // Simple inline xorshift for speed
        s = s ^ (s >> 17);
        s = s ^ (s << 5);
        
        float x = (float)s / 4294967295.0f;
        
        // Need a second state or a second jump for y
        s = s ^ (s << 13); 
        s = s ^ (s >> 17);
        s = s ^ (s << 5);
        float y = (float)s / 4294967295.0f;

        if (x*x + y*y <= 1.0f) hits++;
    }
    return hits;
}