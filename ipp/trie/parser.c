//
// Created by jakub on 06.03.19.
//

#include <memory.h>
#include <stdio.h>
#include <stdlib.h>

#include "parser.h"

char checkWith (const char *buff, const char *correct, size_t n) {

    for (int i = 0; i < n; i++)
        if (buff[i] != correct[i]) return 0;

    return 1;
}

// get arguments
char getArgs (const char *buff, char **arg1, char **arg2, size_t start, size_t n) {

    size_t i;
    *arg2 = NULL;
    for (i = start; i < n && buff[i] != ' ' && buff[i] != '\n'; i++) {

        if (buff[i] != '0' && buff[i] != '1' && buff[i] != '2' && buff[i] != '3') {

            printf("ERROR in arg1\n");
            return 0;
        }
    }

    if (!(*arg1 = malloc((i - start) * sizeof(char)))) return 0;
    strncpy(*arg1, buff + start, i - start);

    if (buff[i] == '\n') return 1;

    start = i + 1;

    for (i = start; i < n && buff[i] != '\n'; i++) {

        if (buff[i] != '0' && buff[i] != '1' && buff[i] != '2' && buff[i] != '3') {

            printf("ERROR in arg2, i = %zu\n", n);
            return 0;
        }
    }

    if (!(*arg2 = malloc((i - start) * sizeof(char)))) return 0;
    strncpy(*arg2, buff + start, i - start);

    return 1;
}

// TODO: maybe rename it to getInput
char parse(int *token, char **arg1, char **arg2) {

    char *buff;
    size_t n = 0;

    ssize_t c = getline(&buff, &n, stdin);

    printf("read %zd chars\n", c);

    // if only '\n' was read - terminate
    if ( c < 2) return 0;

    // comment
    if (buff[0] == '#') {

        *token = SKIP;

        return 1;

    //  DECLARE
    } else if (buff[0] == 'D') {

        if (!checkWith(buff, "DECLARE ", 8)) {

            *token = ERROR;
            return 1;
        }

        if (!getArgs(buff, arg1, arg2, 8, n)) {

            *token = ERROR;
            return 1;
        }

        *token = DECLARE;

        printf("parser done: DECLARE\n");
        return 1;

    // ENERGY and EQUAL
    } else if (buff[0] == 'E') {

        // ENERGY
        if (buff[1] == 'N') {

            if (!checkWith(buff, "ENERGY ", 7)) {

                *token = ERROR;
                return 1;
            }

            if (!getArgs(buff, arg1, arg2, 7, n)) {

                *token = ERROR;
                printf("getArgs error\n");
                return 1;

            }

            if (*arg2 == NULL) {

                *token = ENERGY1;
                return 1;
            }

            *token = ENERGY2;
            return 1;
        }

        // EQUAL
        if (!checkWith(buff, "EQUAL ", 6)) {

            *token = ERROR;
            return 1;
        }

        if (!getArgs(buff, arg1, arg2, 6, n)) {

            *token = ERROR;
            return 1;
        }

        *token = EQUAL;
        return 1;

    // REMOVE
    } else if (buff[0] == 'R') {

        if (!checkWith(buff, "REMOVE ", 7)) {

            *token = ERROR;
            return 1;
        }

        if (!getArgs(buff, arg1, arg2, 7, n)) {

            *token = ERROR;
            return 1;
        }

        *token = REMOVE;
        return 1;

    // VALID
    } else if (buff[0] == 'V') {

        printf("is valid?\n");
        if (!checkWith(buff, "VALID ", 6)) {

            *token = ERROR;
            return 1;
        }

        if (!getArgs(buff, arg1, arg2, 6, n)) {

            *token = ERROR;
            return 1;
        }

        *token = VALID;
        return 1;
    }

    printf("Finishing\n");
    return 0;
}