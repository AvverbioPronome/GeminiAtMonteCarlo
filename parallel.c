#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <omp.h>

// Forward declaration of our kernel
int64_t monte_carlo_parallel(int64_t total_n);

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s <number_of_points>\n", argv[0]);
        return 1;
    }

    int64_t n = atoll(argv[1]);
    
    // Start timing using OpenMP's high-resolution timer
    double start = omp_get_wtime();
    
    int64_t hits = monte_carlo_parallel(n);
    
    double end = omp_get_wtime();
    
    double pi = 4.0 * (double)hits / (double)n;
    
    printf("Points: %ld\n", n);
    printf("Result: %.10f\n", pi);
    printf("Time:   %.4f seconds\n", end - start);
    printf("Rate:   %.2f Mpts/s\n", (n / (end - start)) / 1e6);

    return 0;
}

int64_t monte_carlo_parallel(int64_t total_n) {
    int64_t total_hits = 0;
    // We treat our 32-bit randoms as coordinates.
    // The radius R is 2^32. Therefore R^2 is 2^64.
    // Since 2^64 is exactly 1 higher than the max uint64_t,
    // any sum that doesn't overflow is "inside" the circle.
    // To make this meaningful, we use 31-bit coordinates.
    const uint64_t limit = 1ULL << 62; // (2^31)^2
    // The 'parallel' keyword tells OpenMP to spawn the thread pool here
    #pragma omp parallel reduction(+:total_hits)
    {
        // Thread-local RNG state
        uint64_t s = (uint64_t)omp_get_thread_num() + (uint64_t)time(NULL);
        
        // 'for simd' splits the loop iterations among threads and vectorizes them
        #pragma omp for simd
        for (int64_t i = 0; i < total_n; i++) {
            // Generate one 64-bit random number
            s ^= s << 13; s ^= s >> 7; s ^= s << 17;
            
            // Extract two 31-bit coordinates from the SAME 64-bit state
            uint64_t x = s >> 33;            // Top 31 bits
            uint64_t y = (s >> 2) & 0x7FFFFFFF; // Bottom 31 bits

            // AVX2 can do this multiplication extremely fast
            if ((x*x + y*y) <= limit) total_hits++;
        }
    }
    return total_hits;
}