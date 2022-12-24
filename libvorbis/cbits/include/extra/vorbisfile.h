#include "vorbis/vorbisfile.h"

void ov_callbacks_default_ptr
       ( ov_callbacks* callbacks
       );

void ov_callbacks_noclose_ptr
       ( ov_callbacks* callbacks
       );

void ov_callbacks_streamonly_ptr
       ( ov_callbacks* callbacks
       );

void ov_callbacks_streamonly_noclose_ptr
       ( ov_callbacks* callbacks
       );

int ov_open_callbacks_ptr
      ( void *f
      , OggVorbis_File *vf
      , const char *initial
      , long ibytes
      , ov_callbacks *callbacks
      );

int ov_test_callbacks_ptr
      ( void *f
      , OggVorbis_File *vf
      , const char *initial
      , long ibytes
      , ov_callbacks *callbacks
      );
