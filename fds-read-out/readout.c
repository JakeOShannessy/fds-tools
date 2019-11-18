#define INMAIN
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>

// #include "options.h"
#include <sys/types.h>
#include <sys/stat.h>
// #include GLUT_H

//   dummy

// #include "string_util.h"
// #include "smokeviewvars.h"

#ifdef pp_LUA
#include "c_api.h"
#include "lua_api.h"
#endif

#define TABVALUE 4 // by default a tab counts as 4 spaces
#define MATCH 1
#define NOTMATCH 0
#define DATESTRING_BUFFERSIZE 256
#define MAX_LINE_LENGTH 4096

char *datetimeToString(char *time, struct tm tmdatetime);

char append_string[1024];
int show_version;
int show_help;
int hash_option;

typedef struct Version {
    int major;
    int minor;
    int maintenance;
} Version;

typedef struct TStepMeshProp {
    char *key;
    float *value;
    char *units;
    float *location;
} TStepMeshProp;

typedef struct TimestepMeshInfo {
    int number;
    TStepMeshProp props[];
} TimestepMeshInfo;
 //       Mesh    1
 //       Max CFL number:  0.27E-02 at ( 27, 14, 48)
 //       Max divergence:  0.13E-05 at ( 27, 32, 48)
 //       Min divergence: -0.12E-05 at ( 21, 29, 48)
 //       Max VN number:   0.18E-02 at ( 26, 33, 28)

typedef struct Timestep {
    int number;
    struct tm time;
    float stepsize;
    float simtime;
    float max_vel_err;
    float max_pres_err;
    int pressure_iterations;
    // everything else may or may not exist
} Timestep;
 // Time Step    2000   May  6, 2016  16:47:19
 //       Step Size:    0.251E-01 s, Total Time:      50.25 s
 //       Pressure Iterations:      1
 //       Maximum Velocity Error:  0.30E-01 on Mesh   7 at (  25  17  36)
 //       ---------------------------------------------------------------

typedef struct OutData {
  Version version;
} OutData;

OutData outData;
int timestepArrLength;
Timestep *timesteps;
Timestep *first_empty_timestep;

int ntimesteps;
float last_time, start_time, end_time;
int currentLine;
int currentCharacter;

int countspaces(int *cnt, char *buffer) {
  int spaces = 0;
  int i = 0;
  int cont = 1;
  while(cont) {
    if(buffer[i] == ' '){
      spaces += 1;
    } else if (buffer[i] == '\t') {
      spaces += TABVALUE;
    } else {
      cont = 0;
    }
    i++;
  }
  *cnt = spaces;
  return i-1;
}

int match(char *buffer, const char *key){
  size_t lenbuffer;
  size_t lenkey;
  lenkey=strlen(key);
  lenbuffer=strlen(buffer);
  if(lenbuffer<lenkey)return NOTMATCH; // buffer shorter than key so no match
  if(strncmp(buffer,key,lenkey) != 0)return NOTMATCH; // key doesn't match buffer so no match
  // if(lenbuffer>lenkey&&!isspace(buffer[lenkey]))return NOTMATCH;
  // there is no requirement that the buffer end in a space for this parser
  return MATCH;
}

int parsemonth(char *string) {
  if(match(string, "January"))return 0;
  if(match(string, "February"))return 1;
  if(match(string, "March"))return 2;
  if(match(string, "April"))return 3;
  if(match(string, "May"))return 4;
  if(match(string, "June"))return 5;
  if(match(string, "July"))return 6;
  if(match(string, "August"))return 7;
  if(match(string, "September"))return 8;
  if(match(string, "October"))return 9;
  if(match(string, "November"))return 10;
  if(match(string, "December"))return 11;
  fprintf(stderr, "%s\n", "date parsing error\n");
  exit(1);
}

int parseTimeStepLine(char *buffer) {
  int t;
  char month[100];
  int day, year, hour, minute, second;
  sscanf(buffer,"Time Step %d %s  %d, %d  %d:%d:%d", &t, &month, &day, &year, &hour, &minute, &second);

  (*first_empty_timestep).number = t;
  struct tm str_time;

  str_time.tm_year = year-1900;
  str_time.tm_mon = parsemonth(month);
  str_time.tm_mday = day;
  str_time.tm_hour = hour;
  str_time.tm_min = minute;
  str_time.tm_sec = second;
  str_time.tm_isdst = 0;

  (*first_empty_timestep).time = str_time;
  char datetime[100];
  datetimeToString(datetime, str_time);

  return 0;
}

int parseSecondLine(char *buffer) {
  float step_size, total_time;
  sscanf(buffer,"Step Size: %g s, Total Time: %f s", &step_size, &total_time);
  (*first_empty_timestep).simtime = total_time;
  (*first_empty_timestep).stepsize = step_size;
  last_time = total_time;
  return 0;
}

int parseOtherSecondLine(char *buffer) {
  float step_size, total_time;
  sscanf(buffer,"Time step: %g s, Total time: %f s", &step_size, &total_time);
  (*first_empty_timestep).simtime = total_time;
  (*first_empty_timestep).stepsize = step_size;
  last_time = total_time;
  return 0;
}

int parseThirdLine(char *buffer) {
  int pressure_iterations;
  sscanf(buffer,"Pressure Iterations:  %d", &pressure_iterations);
  (*first_empty_timestep).pressure_iterations = pressure_iterations;
  return 0;
}


int parseFourthLine(char *buffer) {
  float max_vel_err;
  sscanf(buffer,"Maximum Velocity Error:  %f", &max_vel_err);
  (*first_empty_timestep).max_vel_err = max_vel_err;
  return 0;
}

int parseFifthLine(char *buffer) {
  float max_pres_err;
  sscanf(buffer,"Maximum Pressure Error:  %f", &max_pres_err);
  (*first_empty_timestep).max_pres_err = max_pres_err;
  return 0;
}


int parseRunTimeDiagnostics(int indentationLevel, FILE *stream) {
  // the indentation level will not change.
  int currentSpaces;
  while(!feof(stream)){
    char buffer[MAX_LINE_LENGTH];
    currentLine++;
    if(fgets(buffer, MAX_LINE_LENGTH, stream) == NULL)break;
    buffer[strcspn(buffer, "\n")] = 0;
    int l = strlen(buffer);
    int read = countspaces(&currentSpaces, buffer);
    if(l == 0 || currentSpaces == l)continue;
    if ((currentSpaces > indentationLevel)) {
      // TODO: parse other information too
      if(match(&buffer[read], "Time Step ")){
        parseTimeStepLine(&buffer[read]);

        currentLine++;
        if(fgets(buffer, MAX_LINE_LENGTH, stream) == NULL)break;
        buffer[strcspn(buffer, "\n")] = 0;
        int l = strlen(buffer);
        int read = countspaces(&currentSpaces, buffer);

        if(match(&buffer[read], "Time step:")){
          parseOtherSecondLine(&buffer[read]);
        } else if(match(&buffer[read], "Step Size")){
          parseSecondLine(&buffer[read]);
        } else {
          fprintf(stderr,"failed to pass second line of timestep, line: %d\n", currentLine);
          fprintf(stderr,"%s\n", buffer);
          exit(1);
        }

        currentLine++;
        if(fgets(buffer, MAX_LINE_LENGTH, stream) == NULL)break;
        buffer[strcspn(buffer, "\n")] = 0;
        l = strlen(buffer);
        read = countspaces(&currentSpaces, buffer);
        parseThirdLine(&buffer[read]);

        currentLine++;
        if(fgets(buffer, MAX_LINE_LENGTH, stream) == NULL)break;
        buffer[strcspn(buffer, "\n")] = 0;
        l = strlen(buffer);
        read = countspaces(&currentSpaces, buffer);
        parseFourthLine(&buffer[read]);

        currentLine++;
        if(fgets(buffer, MAX_LINE_LENGTH, stream) == NULL)break;
        buffer[strcspn(buffer, "\n")] = 0;
        l = strlen(buffer);
        read = countspaces(&currentSpaces, buffer);
        parseFifthLine(&buffer[read]);

        first_empty_timestep += 1;
        ntimesteps++;
        if(ntimesteps>=timestepArrLength) {
          timestepArrLength = 2*timestepArrLength;
          timesteps = realloc(timesteps,timestepArrLength*sizeof(struct Timestep));
          if(timesteps == NULL) {
            fprintf(stderr,"memory allocation failed");
            exit(1);
          } else {
            first_empty_timestep = &timesteps[ntimesteps];
          }
        }
        continue;
      }
      continue;
    } else {
      // if the number of preceding spaces return to that of the title
      // or even less, then we have finished parsing this section.
      return 0;
    }

  }
  return 0;
}

int parseMiscParameters(int indentationLevel, FILE *stream) {
  // the indentation level will not change.
  int currentSpaces;
  while(!feof(stream)){
    char buffer[MAX_LINE_LENGTH];

    if(fgets(buffer, MAX_LINE_LENGTH, stream) == NULL)break;
    buffer[strcspn(buffer, "\n")] = 0;
    int l = strlen(buffer);
    int read = countspaces(&currentSpaces, buffer);
    if(l == 0 || currentSpaces == l)continue;
    if ((currentSpaces > indentationLevel)) {
      if(match(&buffer[read], "Simulation Start Time")){
        int matched = sscanf(&buffer[read],"Simulation Start Time (s) %f",
                             &start_time);
        if(matched==1){
          continue;
        } else {
          exit(1);
        }
      }
      if(match(&buffer[read], "Simulation End Time")){
        int matched = sscanf(&buffer[read],"Simulation End Time (s) %f",
                             &end_time);
        if(matched==1){
          continue;
        } else {
          exit(1);
        }
      }
      continue;
    } else {
      // if the number of preceding spaces return to that of the title
      // or even less, then we have finished parsing this section.
      return 0;
    }

  }
  return 0;
}

// return 1 if we understand the block and wish to contunue
int parseBlockTitle(char *buffer, int indentationLevel, FILE *stream) {
  char version[255];
  int v1, v2, v3;
  if(match(buffer, "Version")){
    sscanf(buffer,"Version : FDS %d.%d.%d", &outData.version.major,
      &outData.version.minor, &outData.version.maintenance);
    return 1;
  }
  if(match(buffer, "Run Time Diagnostics")) {
    parseRunTimeDiagnostics(indentationLevel, stream);
    return 1;
  }
  if(match(buffer, "Miscellaneous Parameters")) {
    parseMiscParameters(indentationLevel, stream);
    return 1;
  }
  return 0;
}


/* ------------------ readout ------------------------ */

int readout(char *file){
  // int indentationLevel; the current indentation level we are parsing in the
  // number of spaces (TODO: govern TAB behaviour). This starts at 0 (i.e. 0
  // spaces preceding the first character of text on a line).
  int indentationLevel = 0;
  // int skipping; boolean; are we attempting to parse the current block or
  // are we just ignoring it. This starts as false as we don't
  // want to ignore the single block at indentation level 0,
  // as this is the whole file
  int skipping = 0;
  FILE *stream;
  int initialSize = 120;
  timestepArrLength = initialSize;
  timesteps = malloc(initialSize*sizeof(struct Timestep));
  first_empty_timestep = timesteps;
// realloc(buf, size)
  // open the file
  if((stream = fopen(file, "r")) == NULL)return 1;

  // int currentSpaces; the current number of counted spaces
  int currentSpaces;
  // this parsing loop deals with top level blocks
  while(!feof(stream)){
    // TODO: read lines of arbitrary length
    char buffer[MAX_LINE_LENGTH];

    // read a line into the buffer
    currentLine++;
    if(fgets(buffer, MAX_LINE_LENGTH, stream) == NULL)break;
    // trim off the end of line
    buffer[strcspn(buffer, "\n")] = 0;
    // take the length of the resulting string
    int l = strlen(buffer);
    // count the number of preceding spaces on this line
    // and place it into the currentSpaces variable
    // read is the number of characters read
    int read = countspaces(&currentSpaces, buffer);
    // if the line is empty or full of only space, slip to the next line
    if(l == 0 || currentSpaces == l)continue;
    // if the current spaces is greater than the indentation level (i.e. we've moved into a subblock)
    // and we are supposed to be skipping this block, then skip to the next line.
    if ((currentSpaces > indentationLevel) && skipping == 1) {
      continue;
    } else {
      // otherwise attempt to parse the title of this block
      skipping = 1 - parseBlockTitle(&buffer[read], indentationLevel, stream);
      indentationLevel = currentSpaces;
    }

  }
  fclose(stream);
  return 0;

}
char *datetimeToString(char *time, struct tm tmdatetime) {
  time_t time_of_day;
  strftime(time, DATESTRING_BUFFERSIZE-1, "%Y-%m-%dT%H:%M:%SZ", &tmdatetime);
  time_of_day = mktime(&tmdatetime);
  if(strlen(time)==0) {
    fprintf(stderr, "empty timestring\n");
  }
  return time;
}

int main(int argc, char *argv[]) {
  int i;
  if (argv[2]==NULL){
    fprintf(stderr, "file not specified\n");
    return 1;
  }
  if (readout(argv[2])){
    fprintf(stderr, "reading of file %s failed.\n", argv[2]);
    return 1;
  }
  if(!strcmp(argv[1], "rundata")) {
    printf("{\"NameX\":\"Simulation Time\", \"UnitsX\":\"s\", \"NameY\":\"Wall Time\", \"UnitsY\":\"-\", \"Values\": [\n");
    for (i=0; &timesteps[i]!=first_empty_timestep; i++) {
      char time[DATESTRING_BUFFERSIZE];
      datetimeToString(time, timesteps[i].time);
      printf("{\"x\":\"%s\",\"y\":%f},\n", time, timesteps[i].simtime);
    }
    printf("]}\n");
  } else if(!strcmp(argv[1], "pressure-error")) {
    printf("{\"NameX\":\"Simulation Time\", \"UnitsX\":\"s\", \"NameY\":\"Pressure Error\", \"UnitsY\":\"1/s^2\", \"Values\": [\n");
    for (i=0; &timesteps[i]!=first_empty_timestep; i++) {
      printf("{\"x\":%f,\"y\":%f},\n", timesteps[i].simtime, timesteps[i].max_pres_err);
    }
    printf("]}\n");
  } else if(!strcmp(argv[1], "velocity-error")) {
    printf("{\"NameX\":\"Simulation Time\", \"UnitsX\":\"s\", \"NameY\":\"Velocity Error\", \"UnitsY\":\"m/s\", \"Values\": [\n");
    for (i=0; &timesteps[i]!=first_empty_timestep; i++) {
      printf("{\"x\":%f,\"y\":%f},\n", timesteps[i].simtime, timesteps[i].max_vel_err);
    }
    printf("]}\n");
  } else if(!strcmp(argv[1], "pressure-iterations")) {
    printf("{\"NameX\":\"Simulation Time\", \"UnitsX\":\"s\", \"NameY\":\"Pressure Iterations\", \"UnitsY\":\"-\", \"Values\": [\n");
    for (i=0; &timesteps[i]!=first_empty_timestep; i++) {
      printf("{\"x\":%f,\"y\":%d},\n", timesteps[i].simtime, timesteps[i].pressure_iterations);
    }
    printf("]}\n");
  } else if (!strcmp(argv[1], "progress")) {

    // time_t time_of_day;
    char start_wall[DATESTRING_BUFFERSIZE];
    strftime(start_wall, DATESTRING_BUFFERSIZE-1, "%Y-%m-%dT%H:%M:%SZ", &timesteps[0].time);
    // time_of_day = mktime(&timesteps[i].time);

    // time_t time_of_day;
    char last_wall[DATESTRING_BUFFERSIZE];
    strftime(last_wall, DATESTRING_BUFFERSIZE-1, "%Y-%m-%dT%H:%M:%SZ", &timesteps[ntimesteps-1].time);
    // time_of_day = mktime(&timesteps[i].time);

    printf("{\"Sim\":{\"StartTime\":%f, \"EndTime\":%f, \"LastTime\":%f}, \"Wall\":{\"StartTime\":\"%s\",\"LastTime\":\"%s\"}, \"Format\":\"0.0.3\"}\n",
            start_time, end_time, last_time, start_wall, last_wall);
  }
  return 0;
}
