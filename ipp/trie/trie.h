//
// Created by jakub on 05.03.19.
//

#ifndef TRIE_TRIE_H
#define TRIE_TRIE_H

#include <stdlib.h>

typedef struct Node {

    struct Node *children[4];
    int64_t *energy;

}Node;

// null na końcu historii oznacza nielegalność
// nieokreślona legalność to energy = 0
// TODO: rozważam reprezentowanie nielegalności jako null pointer do energy
// wtdy ponowne tworzenie usuniętych węzłów będzie szybsze

char getEnergy(Node **trie, char *states, int64_t *res);
char put(Node **trie, char *states, int64_t energy);
char removeStates(Node **trie, char *states);
char valid(Node **trie, char *states);

#endif //TRIE_TRIE_H
