//
// Created by jakub on 05.03.19.
//

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "trie.h"

char getEnergy(Node **trie, char *states, int64_t *res) {

    size_t n = strlen(states);
    Node *iterator = *trie;

    // this 3 lines could be getNode()
    // it's 3rd time i'm repeating them a lot
    for (size_t i = 0; i < n; i++) {

        if (iterator->children[states[i] - (char)'0'] == NULL) return 0;

        iterator = iterator->children[states[i] - (char)'0'];
    }

    *res = *iterator->energy;

    if ( *res == 0) return 0;
    return 1;
}

char put(Node **trie, char *states, int64_t energy) {

    Node *iterator = *trie;
    size_t  n = strlen(states);

    for (size_t i = 0; i < n; i++) {

        //add state if not existant
        if (iterator->children[states[i] - (char)'0'] == NULL) {

            Node *newNode;
            if (!(newNode = malloc(sizeof(Node)))) return 0;

            if (!(newNode->energy = malloc(sizeof(int64_t)))) return 0;
            *(newNode->energy) = 0;

            for (char j = 0; j < 4; j++) {

                newNode->children[j] = NULL;
            }

            iterator->children[states[i] - (char)'0'] = newNode;
        }

        iterator = iterator->children[states[i] - (char)'0'];

        if (!(iterator->energy = malloc(sizeof(int64_t)))) return 0;
    }

    *iterator->energy = energy;

    return 1;
}

void removeNode(Node *trie) {

    if (trie == NULL) return;

    for (char i = 0; i < 4; i++) {
        removeNode(trie->children[i]);
        free(trie->children[i]);
    }

    printf("removing node with energy: %zu", *trie->energy);
}

char removeStates(Node **trie, char *states) {

    if (trie == NULL) return 1;

    Node *iterator = *trie;
    size_t n = strlen(states);

    // find last element of state history
    for (size_t i = 0; i < n; i++) {

        if (iterator->children[states[i] - (char)'0'] == NULL) return 1;
        iterator = iterator->children[states[i] - '0'];
    }

    // remove required state and all of its kin
    removeNode(iterator);
    free(iterator);
    return 1;
}

char valid(Node **trie, char *states) {

    Node *iterator = *trie;
    size_t n = strlen(states);

    for (size_t i= 0; i < n; i++){
        if (iterator->children[states[i] - (char)'0'] == NULL ) return 0;

        iterator = iterator->children[states[i] - (char)'0'];
    }

    return 1;
}




