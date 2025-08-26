#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

int calculate_love_percentage(const char *name1, const char *name2) {
    int sum = 0;
    int len1 = strlen(name1);
    int len2 = strlen(name2);

    for (int i = 0; i < len1; i++) {
        sum += (unsigned char)name1[i];
    }
    for (int i = 0; i < len2; i++) {
        sum += (unsigned char)name2[i];
    }

    srand(sum);
    int random_factor = rand() % 50;

    int percentage = (sum + random_factor) % 101;
    
    if (percentage < 10) {
        percentage += 10;
    }

    if (percentage > 100) {
        percentage = 100;
    }
    
    return percentage;
}

const char* get_love_message(int percentage) {
    if (percentage >= 90) {
        return "Perfect Match!";
    } else if (percentage >= 75) {
        return "Great Love!";
    } else if (percentage >= 60) {
        return "Good Compatibility!";
    } else if (percentage >= 40) {
        return "Some Potential!";
    } else if (percentage >= 20) {
        return "Needs Work!";
    } else {
        return "Maybe Just Friends!";
    }
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("ERROR");
        return 1;
    }
    
    const char *name1 = argv[1];
    const char *name2 = argv[2];

    int percentage = calculate_love_percentage(name1, name2);
    const char *message = get_love_message(percentage);

    printf("%s|%s|%d|%s", name1, name2, percentage, message);
    
    return 0;
}