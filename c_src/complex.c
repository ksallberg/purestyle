/* complex.c */

#include <time.h>
#include <stdio.h>
#include <string.h>

int foo(int x) {
  return x+1;
}

char* get_time_string(int mode24)
{
  static char time_str[12] = {0};   /* Stroes the time as a string */
  struct tm *now = NULL;
  int hour = 0;
  time_t time_value = 0;

  time_value = time(NULL);          /* Get time value              */
  now = localtime(&time_value);     /* Get time and date structure */
  hour = now->tm_hour;              /* Save the hour value         */
  if(!mode24 && hour>12)            /* Adjust if 12 hour format    */
    hour -= 12;

  printf("%d:",hour);
  printf("%d:",now->tm_min);
  printf("%d",now->tm_sec);


  if(!mode24)
    if(now->tm_hour>24){
       printf("PM\n");
    }else{
       printf("AM\n");
     }
  return time_str;
}


char *str(int y) {
  return "a string from C";
}
