#include <stdlib.h>
#include <gps.h>

struct gps_data_t *libgps_open(const char *host, const char *port)
{
     struct gps_data_t *gps_data = malloc(sizeof *gps_data);
     if(gps_open(host, port, gps_data) != -1) {

          return gps_data;
     }
     
     free(gps_data);
     return 0;
}

int libgps_close(struct gps_data_t *data)
{
     int ret = data ? gps_close(data) : 0;
     free(data);
     return ret;
}


void libgps_read(struct gps_data_t *data, double results[6])
{
   results[0] = (time_t)data->fix.time;
   results[1] = data->fix.latitude;
   results[2] = data->fix.longitude;
   results[3] = data->fix.altitude;
   results[4] = data->fix.track;
   results[5] = data->fix.speed;
}
