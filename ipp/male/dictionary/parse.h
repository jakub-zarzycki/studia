#ifndef PARSE
#define PARSE

int parse(
        char (*delete)(const char*),
        char (*equal)(const char*, const char*),
        char (*get_energy)(const char*, uint64_t*),
        char (*insert)(const char*),
        char (*set_energy)(const char*, uint64_t),
        char (*valid)(const char*));

#endif //PARSE
