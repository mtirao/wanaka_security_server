#include <stdio.h>
#include <stdbool.h>

int gpio_alert(bool a) {
    printf("Alert: %s\n", a ? "true" : "false");
    return 1;
}

int gpio_info(bool a) {
    printf("Info: %s\n", a ? "true" : "false");
    return 1;
}

int gpio_warn(bool a) {
    printf("Warn: %s\n", a ? "true" : "false");
    return 1;
}



// This is a simple C function that adds two integers and returns the result.