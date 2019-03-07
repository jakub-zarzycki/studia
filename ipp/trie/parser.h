//
// Created by jakub on 06.03.19.
//

#ifndef TRIE_PARSER_H
#define TRIE_PARSER_H

enum {DECLARE, ENERGY1, ENERGY2, EQUAL, ERROR, REMOVE, SKIP, VALID };

char parse(int *token, char **arg1, char **arg2);

#endif //TRIE_PARSER_H
