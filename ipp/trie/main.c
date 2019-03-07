#include <stdio.h>
#include "trie.h"
#include "parser.h"

int main() {

    Node *history = (Node*)malloc(sizeof(Node));
    history->energy = malloc(sizeof(int64_t));
    *history->energy = 0;

    for (char j = 0; j < 4; j++) {
        history->children[j] = NULL;
    }

    int64_t energy;

    int token;
    char *arg1, *arg2;

    put (&history, "000", 5);

    printf("testing getline\n");
    while (parse(&token, &arg1, &arg2)) {

        printf("got token: %d\n", token);

        switch (token) {

            case DECLARE:

                printf("doing DECLARE: %c\n", arg1[0]);
                if (!put(&history, arg1, 0)) {

                    printf("cannot allocate memory\n");
                    return 1;
                }

                break;

            case ENERGY1:

                if (getEnergy(&history, arg1, &energy))
                    printf("energy is: %zu\n", energy);
                else
                    printf("path invalid\n");
                break;

            case ENERGY2:

                put(&history, arg1, atoi(arg2));
                break;

            case EQUAL:

                printf("tego jeszcze nie ma\n");
                break;

            case REMOVE:

                removeStates(&history, arg1);
                break;

            case VALID:

                printf("history validity is: %d\n", valid(&history, arg1));
                break;

            case ERROR:

                printf("ERROR\n");
                break;

            case SKIP: default:

                printf("ignoring\n");
                break;
        }
    }

    printf("finishing\n");
    for (int i = 0; i < 3; i++) removeStates(&history, "");
    return 0;
}
