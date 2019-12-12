#include <time.h>
#include <stdio.h>
#include <string.h>

int pstyle_get_date(char buffer[]) {
  struct tm *ts;
  size_t last;
  time_t timestamp = time(NULL);
  ts = localtime(&timestamp);
  last = strftime(buffer, 32, "%A", ts);
  buffer[last] = '\0';
  return 0;
}
