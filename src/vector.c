#include "vector.h"

struct vector
{
    const size_t element_size;
    char* elements;
    int num_elements;
};

char* vector_access(const struct vector* vector, int i) {
    char* element = vector->elements + (i * vector->element_size);
    return element;
}