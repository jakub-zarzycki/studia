//
// Created by jakub on 05.03.19.
//

// TODO: when removing path represent invalid paths by NULL energy pointer
//       that would require changes in getEnergy, insert, removeStates, and valid

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>

#include "trie.h"

//będziemy trzymać energie na drzewie
//levels mówi nam jak głęboko mamy szukać - 0 oznacza poprawną energię
//energy to energia z zadania
typedef struct Energy_node {

    size_t levels;
    struct Energy_node *lower;
    uint64_t energy;

} Energy_node;

//definicja trie
typedef struct Node {

    struct Node *children[4];
    Energy_node *energy;

} Node;

//global static root pointer
static Node *root = NULL;

char equal(const char *history1, const char *history2) {

    Node *node1 = NULL;
    Node *node2 = NULL;
    size_t m, n;

    m = strlen(history1);
    n = strlen(history2);

    // get node1
    node1 = root;
    for (size_t i = 0; i < m; i++){

        if (node1->children[history1[i] - (char)'0'] == NULL ) { printf("returning1\n"); return 0;}

        node1 = node1->children[history1[i] - (char)'0'];
    }

    // get node2
    node2 = root;
    for (size_t j = 0; j < n; j++){

        if (node2->children[history2[j] - (char)'0'] == NULL ) { printf("returning2\n"); return 0;}

        node2 = node2->children[history2[j] - (char)'0'];
    }

    //sprawdzamy null pointery
    if (node1->energy == NULL) {

        if (!(node1->energy = malloc(sizeof(Energy_node)))) return 0;
        node1->energy->energy = 0;
        node1->energy->lower = NULL;
        node1->energy->levels = 0;
    }

    if (node2->energy == NULL) {

        if (!(node2->energy = malloc(sizeof(Energy_node)))) return 0;
        node2->energy->energy = 0;
        node2->energy->lower = NULL;
        node2->energy->levels = 0;
    }

    //energy_iteretor1 jest głębszy
    Energy_node *energy_iterator1 = node1->energy;
    Energy_node *energy_iterator2 = node2->energy;

    while (energy_iterator1->levels > 0) {

        energy_iterator1->levels++;
        energy_iterator1 = energy_iterator1->lower;
    }

    energy_iterator1->levels++;

    while (energy_iterator2->levels > 0) {

        energy_iterator2->levels++;
        energy_iterator2 = energy_iterator2->lower;
    }

    energy_iterator2->levels++;

    uint64_t energy = 0;
    //TODO: obsługa energy == NULL
    if (energy_iterator1->energy == 0) {

        if (energy_iterator2->energy == 0){

            printf("ERROR\n");
            return 0;
        }

        energy = energy_iterator2->energy;

    } else {

        if (energy_iterator2->energy == 0) energy = energy_iterator1->energy;
        else energy = (energy_iterator1->energy + energy_iterator2->energy) / 2;
    }

    Energy_node *new_energy = NULL;
    if(!(new_energy = malloc(sizeof(Energy_node)))) return 0;
    new_energy->energy = energy;
    new_energy->lower = NULL;
    new_energy->levels = 0;

    energy_iterator1->lower = new_energy;
    energy_iterator2->lower = new_energy;

    return 1;
}

char get_energy(const char *states, uint64_t *energy) {

    size_t n = strlen(states);
    Node *iterator = root;

    // TODO: clean-up
    // this 3 lines could be getNode()
    // it's 3rd time i'm repeating them a lot
    for (size_t i = 0; i < n; i++) {

        if (iterator->children[states[i] - (char)'0'] == NULL) return 0;

        iterator = iterator->children[states[i] - (char)'0'];
    }

    Energy_node *energy_iterator = iterator->energy;

    if (energy_iterator == NULL) return 0;

    while (energy_iterator->levels > 0) energy_iterator = energy_iterator->lower;

    *energy = energy_iterator->energy;

    return 1;
}

char set_energy(const char *states, uint64_t energy) {

    size_t n = strlen(states);
    Node *iterator = root;

    // TODO: clean-up
    // this 3 lines could be getNode()
    // it's 3rd time i'm repeating them a lot
    for (size_t i = 0; i < n; i++) {

        if (iterator->children[states[i] - (char)'0'] == NULL) return 0;

        iterator = iterator->children[states[i] - (char)'0'];
    }

    //set up energy node
    Energy_node *new_energy = NULL;
    if(!(new_energy = malloc(sizeof(Energy_node)))) return 0;
    new_energy->energy = energy;
    new_energy->lower = NULL;
    new_energy->levels = 0;

    iterator->energy = new_energy;

    return 1;
}

//obsługa polecenia INSERT
//ustawia energię na NULL
char insert(const char *states) {

    Node *iterator = root;
    size_t  n = strlen(states);

    for (size_t i = 0; i < n; i++) {

        //create new node if non existant
        if (iterator->children[states[i] - (char)'0'] == NULL) {

            Node *new_node;

            if (!(new_node = malloc(sizeof(Node)))) return 0;

            new_node->energy = NULL;

            for (char j = 0; j < 4; j++)
                new_node->children[j] = NULL;

            iterator->children[states[i] - (char)'0'] = new_node;
        }

        iterator = iterator->children[states[i] - (char)'0'];
    }

    return 1;
}

void delete_node(Node **iterator) {

    if (*iterator == NULL) return;

    for (char i = 0; i < 4; i++)
        delete_node(&(*iterator)->children[i]);

    free(*iterator);
    *iterator = NULL;
}

char delete(const char *states) {

    size_t n = strlen(states);
    Node **iterator = malloc(sizeof(Node*));
    *iterator = root;

    // find last element of state history
    for (size_t i = 0; i < n; i++) {

        if (&(*iterator)->children[states[i] - (char )'0'] == NULL) return 1;

        iterator = &(*iterator)->children[states[i] - (char)'0'];
    }

    delete_node(iterator);

    return 1;
}

char valid(const char *states) {

    Node *iterator = root;
    size_t n = strlen(states);

    for (size_t i= 0; i < n; i++){

        if (iterator->children[states[i] - (char)'0'] == NULL ) return 0;

        iterator = iterator->children[states[i] - (char)'0'];
    }

    return 1;
}

char init() {

    root = malloc(sizeof(Node));
    for (size_t i = 0;i < 3; i++)
        root->children[i] = NULL;

    return 1;
}

void tidy_up() {

    delete_node(&root);
}
