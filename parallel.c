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

    // The 'parallel' keyword tells OpenMP to spawn the thread pool here
    #pragma omp parallel reduction(+:total_hits)
    {
        // Thread-local RNG state
        uint32_t s = (uint32_t)omp_get_thread_num() + (uint32_t)time(NULL);
        
        // 'for simd' splits the loop iterations among threads and vectorizes them
        #pragma omp for simd
        for (int64_t i = 0; i < total_n; i++) {
            s ^= s << 13; s ^= s >> 17; s ^= s << 5;
            float x = (float)s / 4294967295.0f;
            
            s ^= s << 13; s ^= s >> 17; s ^= s << 5;
            float y = (float)s / 4294967295.0f;

            if (x*x + y*y <= 1.0f) total_hits++;
        }
    }
    return total_hits;
}