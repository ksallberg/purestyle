/* complex.c */

#include <time.h>
#include <stdio.h>
#include <string.h>

int foo(int x) {
  return x+1;
}

int str(char buffer[]) {
  struct tm *ts;
  size_t last;
  time_t timestamp = time(NULL);
  ts = localtime(&timestamp);
  last = strftime(buffer, 32, "%A", ts);
  buffer[last] = '\0';
  return 0;
}
