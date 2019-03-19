#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdint.h>

//Define używany do sprawdzania poprawności danych wejściowych. W przypadku
//złego wejścia skacze do końca funkcji, gdzie wypisywane jest "ignored"
//i zwalniana pamięć
#define INPUT_ASSERT(expression) if(!(expression)) goto wrong_input;

// stałe odpowiadające różnym poleceniom
static const char *DECLARE = "DECLARE";
static const char *ENERGY = "ENERGY";
static const char *EQUAL = "EQUAL";
static const char *REMOVE = "REMOVE";
static const char *VALID = "VALID";

uint64_t atouin64(const char *s) {

    uint64_t ret = 0;
    uint64_t old_ret = 0;

    size_t n = strlen(s);

    for (size_t i = 0; i < n; i++) {

        old_ret = ret;
        ret *= 10;

        if (ret < old_ret || ret % 10) return 0;
        else ret += s[i] - '0';
    }

    return ret;
}

inline static int correct_line(const char *s) {

    char space = 0;
    size_t n = 0;
    n = strlen(s);

    for(size_t i = 0; i < n - 1; i++ ) {

        if (s[i] == ' ' ) {

            if (space) return 0;

            space = 1;
            continue;
        }

        space = 0;
        if (!isupper(s[i]) && !isdigit(s[i]) && s[i] != '\n') return 0;
    }

    if (space) return 0;
    return 1;
}

//funkcja sprawdza, czy słowo jest poprawną (w sensie treści zadania) energią
inline static int correct_energy(const char *s) {

    size_t sl = strlen(s);
    for (int i = 0; i < sl; i++)
        if (!isdigit(s[i]))
            return 0;

    return 1;
}

//funkcja sprawdza, czy słowo jest poprawną (w sensie treści zadania) historią
inline static int correct_history(const char *s) {

    size_t sl = strlen(s);

    for (size_t i = 0; i < sl; i++)
        if (s[i] < '0' || s[i] > '3')
            return 0;

    return 1;
}

//funkcja parse obsługuje jedną linię wejścia i wywołuje odpowiednią z funkcji
//odpowiedzialnych za poszczególne operacje
void parse(
        char (*delete)(const char*),
        char (*equal)(const char*, const char*),
        char (*get_energy)(const char*, uint64_t*),
        char (*insert)(const char*),
        char (*set_energy)(const char*, uint64_t),
        char (*valid)(const char*)) {

    static char splited[4][1000 * 100 + 7];
    char *line = NULL;
    size_t buf_size = 0;

    //wczytywanie jednej linii
    size_t line_length = 0;
    line_length = (size_t) getline(&line, &buf_size, stdin);

    //sprawdzam czy to komentarz
    if (line[0] == '#' || line_length < 2) {

        free(line);
        return;
    }

    //sprawdzam, czy linia zawiera jedynie dozwolone, drukowalne znaki: spacje,
    //końce linii, wielkie litery i cyfry
    for (size_t i = 0; i < line_length; i++)
        INPUT_ASSERT(correct_line(line))

    //dzielę wczytaną linię na słowa oddzielone białymi znakami
    int words_number = sscanf(line, "%s%s%s%s",
                              splited[0],
                              splited[1],
                              splited[2],
                              splited[3]);

    //odsiewam ewidentne niepoprawne wyjścia
    INPUT_ASSERT(words_number != EOF && words_number < 4 && words_number >= 1)

    //rozpatruję przypadki odpowiadające kolejnym poleceniom, makrem INPUT_ASSERT
    //sprawdzam, czy zgadza się liczba słów oraz ich typ. Nie sprawdzam logiki poleceń.
    if (strcmp(splited[0], DECLARE) == 0) {

        INPUT_ASSERT(words_number == 2)
        INPUT_ASSERT(correct_history(splited[1]))
        insert(splited[1]);

    } else if (strcmp(splited[0], ENERGY) == 0) {

        INPUT_ASSERT(words_number < 4 && words_number > 1)
        INPUT_ASSERT(correct_history(splited[1]))

        if (words_number == 2) {

            uint64_t energy;

            if (get_energy(splited[1], &energy) && energy > 0) printf("%zu\n", energy);
            else printf("ERROR\n");

        } else if (words_number == 3) {

            INPUT_ASSERT(correct_energy(splited[2]))

            if (!(set_energy(splited[1], atouin64(splited[2])))) printf("ERROR\n");

        } else {

            printf("ERROR\n");
        }

    } else if (strcmp(splited[0], REMOVE) == 0) {

        INPUT_ASSERT(words_number == 2)
        INPUT_ASSERT(correct_history(splited[1]))

        delete(splited[1]);

        //TODO: not done
    } else if (strcmp(splited[0], VALID) == 0) {

        INPUT_ASSERT(words_number == 2)

        if (valid(splited[1])) printf("YES\n");
        else printf("NO\n");

        //TODO: the hardest one
    } else if (strcmp(splited[0], EQUAL) == 0) {

        INPUT_ASSERT(words_number == 3)
        if (!(equal(splited[1], splited[2]))) printf("ERROR\n");

    } else {

        INPUT_ASSERT(0)
    }

    free(line);
    return;

//Obsługa niepoprawnego wejścia
    wrong_input:
    free(line);
    printf("ERROR\n");
}

//
#undef INPUT_ASSERT
