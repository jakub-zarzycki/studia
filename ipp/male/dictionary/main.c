#include <stdio.h>
#include <string.h>

#include "trie.h"
#include "parse.h"

int main(int argc, char *argv[]) {

    if (!init()) return 1;

    while (!feof(stdin)) {

        parse(delete, equal, get_energy, insert, set_energy, valid);
    }

    tidy_up();
    return 0;
}
