#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <bcm2835.h>

#define GPIO_17 RPI_V2_GPIO_P1_11 
#define GPIO_22 RPI_V2_GPIO_P1_15
#define GPIO_27 RPI_V2_GPIO_P1_13

bool init_library() {
    if (!bcm2835_init()) return false;

     // Set the pin to be an output
    bcm2835_gpio_fsel(GPIO_17, BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_fsel(GPIO_27, BCM2835_GPIO_FSEL_OUTP);
    bcm2835_gpio_fsel(GPIO_22, BCM2835_GPIO_FSEL_OUTP);
    
    return true;
}


int gpio_alert(bool a) {
    bcm2835_gpio_write(GPIO_17, a ? HIGH : LOW);
    return 1;
}

int gpio_info(bool a) {
    bcm2835_gpio_write(GPIO_22, a ? HIGH : LOW);
    return 1;
}

int gpio_warn(bool a) {
    bcm2835_gpio_write(GPIO_27, a ? HIGH : LOW);
    return 1;
}

/*int main(int argc, char **argv) {
    printf("Running ... \n");

    init_library();
   
    
    // Turn it on
    gpio_warn(true);
    
    // wait a bit
    bcm2835_delay(500);
    
    // turn it off
    gpio_warn(false);
    


    return 0;
}*/


// This is a simple C function that adds two integers and returns the result.