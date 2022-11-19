#include "vorbis/vorbisfile.h"

extern void ov_callbacks_default_ptr
              ( ov_callbacks* callbacks
              );

extern void ov_callbacks_noclose_ptr
              ( ov_callbacks* callbacks
              );

extern void ov_callbacks_streamonly_ptr
              ( ov_callbacks* callbacks
              );

extern void ov_callbacks_streamonly_noclose_ptr
              ( ov_callbacks* callbacks
              );

extern int ov_open_callbacks_plus
             ( void *f
             , OggVorbis_File *vf
             , const char *initial
             , long ibytes
             , ov_callbacks *callbacks
             );

extern int ov_test_callbacks_plus
             ( void *f
             , OggVorbis_File *vf
             , const char *initial
             , long ibytes
             , ov_callbacks *callbacks
             );
