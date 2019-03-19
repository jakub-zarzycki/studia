//
// Created by jakub on 05.03.19.
//

#ifndef TRIE_TRIE_H
#define TRIE_TRIE_H

#include <stdint.h>
#include <stdlib.h>

// null na końcu historii oznacza nielegalność
// nieokreślona legalność to energy = 0
// TODO: rozważam reprezentowanie nielegalności jako null pointer do energy
// wtdy ponowne tworzenie usuniętych węzłów będzie szybsze

char delete(const char *states);

char equal(const char *history1, const char *history2);

char get_energy(const char *states, uint64_t *energy);

char insert(const char *states);

char set_energy(const char *states, uint64_t energy);

char valid(const char *states);

char init();

void tidy_up();

#endif //TRIE_TRIE_H
