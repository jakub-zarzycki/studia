//
// Created by jakub on 06.03.19.
//

#include <memory.h>
#include <stdio.h>
#include <stdlib.h>

#include "parser.h"

char checkWith (char *buff, char *correct, size_t n) {

    for (int i = 1; i < n; i++) {

        if (buff[i] != correct[i]) {
            return 0;
        }
    }

    return 1;
}

/* ***********************************
 * deprecated
 * i learned to do this in getArgs
// get 1 argument
char getArg (char *buff, char **arg, size_t start, size_t n) {

    printf("getArg: %zu %zu\n", start, n);

    size_t i;
    for (i = start; i < n && buff[i] != '\n'; i++) {

        if (buff[i] != '0' && buff[i] != '1' && buff[i] != '2' && buff[i] != '3') {

            printf("ERROR\n");
            return 0;
        }
    }

    if (!(*arg = malloc((i - start) * sizeof(char)))) return 0;

    strncpy(*arg, buff + start, i - start);

    printf("%d\n", *arg[i-start]);
    return 1;
}
*/

// get arguments
char getArgs (char *buff, char **arg1, char **arg2, size_t start, size_t n) {

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

char parse(int *token, char **arg1, char **arg2) {

    char *buff;
    size_t n = 0;

    ssize_t c = getline(&buff, &n, stdin);
    printf("read %zd chars\n", c);
    if ( c <= 1) return 0;

    if (buff[0] == '#') {

        *token = SKIP;

        return 1;

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

    } else if (buff[0] == 'E') {

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
    }

    printf("Finishing\n");
    return 0;
}