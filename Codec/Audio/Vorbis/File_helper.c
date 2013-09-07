#include <vorbis/vorbisfile.h>
#include <stdlib.h>


OggVorbis_File* new_OggVorbis_File(
      size_t (*read_func)  (void *ptr, size_t size, size_t nmemb, void *datasource),
      int    (*seek_func)  (void *datasource, ogg_int64_t offset, int whence),
      int    (*close_func) (void *datasource),
      long   (*tell_func)  (void *datasource),
      int    *error
    )
{
    OggVorbis_File* f = malloc(sizeof(OggVorbis_File));
    ov_callbacks cbs;
    cbs.read_func = read_func;
    cbs.seek_func = seek_func;
    cbs.close_func = close_func;
    cbs.tell_func = tell_func;
    *error = ov_open_callbacks(f, f, NULL, 0, cbs);
    if (*error != 0) {
        free(f);
        f = NULL;
    }
    return f;
}

void free_OggVorbis_File(OggVorbis_File* f)
{
    ov_clear(f);
    free(f);
}

int info_OggVorbis_File(OggVorbis_File* f, int* version, int* channels, long* rate)
{
    vorbis_info *vi=ov_info(f,-1);
    if (vi == NULL)
        return -1;
    else {
        *version = vi->version;
        *channels = vi->channels;
        *rate = vi->rate;
        return 0;
    }
}

